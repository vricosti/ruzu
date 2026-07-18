// SPDX-License-Identifier: GPL-3.0-or-later
//
// In-process game boot — the launcher counterpart of upstream yuzu's
// `GMainWindow::BootGame` (`main.cpp`) + the emulation-thread setup. It drives
// the same `ruzu_core::core::System` lifecycle that `ruzu_cmd` drives, but with
// the GTK-embedded `CAMetalLayer` as the render surface instead of an SDL
// window, and without SDL's event loop (GTK owns the main loop).
//
// Boot runs on a dedicated background thread so `System::load` (heavy: ROM
// parse + shader/pipeline cache build) never blocks the GTK main thread — the
// same split yuzu uses (its emulation/GPU thread does the heavy work and posts
// progress to the GUI thread via `Qt::QueuedConnection`). Presentation runs on
// the GPU thread inside `video_core`, reading `shown_state` / `framebuffer_layout`
// directly, so frames land in the window's Metal layer with no per-frame work
// here.
//
// This mirrors `ruzu_cmd/src/main.rs`'s Vulkan boot path (macOS is Vulkan/
// MoltenVK only, so the OpenGL/Null branches are omitted). It is intentionally
// launcher-local rather than shared with `ruzu_cmd`, to avoid editing the
// concurrently-modified `ruzu_cmd`/`frontend_common` trees; unifying the two is
// deferred tech debt.

use std::sync::atomic::AtomicBool;
use std::sync::mpsc::{Receiver, Sender};
use std::sync::{Arc, OnceLock, RwLock};
use std::thread::JoinHandle;

use ruzu_core::frontend::emu_window::WindowSystemInfo;
use ruzu_core::frontend::framebuffer_layout::FramebufferLayout;

use crate::loading_screen::LoadStage;

/// Progress callback marshaled to the loading screen on the GTK main thread.
pub type ProgressFn = Box<dyn Fn(LoadStage, usize, usize) + Send + 'static>;

/// A running emulation session. Dropping (or calling [`Self::stop`]) shuts the
/// system down: `System::pause` + `System::shutdown_main_process`, mirroring the
/// tail of `ruzu_cmd`'s `main`.
pub struct EmulationSession {
    stop_tx: Option<Sender<()>>,
    join: Option<JoinHandle<()>>,
}

impl EmulationSession {
    /// Signal the boot thread to shut the system down and join it.
    pub fn stop(&mut self) {
        if let Some(tx) = self.stop_tx.take() {
            let _ = tx.send(());
        }
        if let Some(join) = self.join.take() {
            let _ = join.join();
        }
    }
}

impl Drop for EmulationSession {
    fn drop(&mut self) {
        self.stop();
    }
}

/// Boot `filepath` into a new emulation session using the given render surface.
///
/// `window_info` carries the `CAMetalLayer` (Cocoa) the Vulkan renderer presents
/// into; `shown_state` / `framebuffer_layout` are the shared handles the
/// renderer reads each present (updated by the window on visibility/resize).
pub fn boot_game(
    window_info: WindowSystemInfo,
    drawable_size: (u32, u32),
    shown_state: Arc<AtomicBool>,
    framebuffer_layout: Arc<RwLock<FramebufferLayout>>,
    filepath: String,
    progress: ProgressFn,
) -> EmulationSession {
    let (stop_tx, stop_rx) = std::sync::mpsc::channel::<()>();

    let join = std::thread::Builder::new()
        .name("ruzu-boot".into())
        .spawn(move || {
            run_boot(
                window_info,
                drawable_size,
                shown_state,
                framebuffer_layout,
                filepath,
                progress,
                stop_rx,
            );
        })
        .expect("spawn boot thread");

    EmulationSession {
        stop_tx: Some(stop_tx),
        join: Some(join),
    }
}

/// The boot body, run on the boot thread. Faithful to `ruzu_cmd`'s Vulkan path.
fn run_boot(
    window_info: WindowSystemInfo,
    drawable_size: (u32, u32),
    shown_state: Arc<AtomicBool>,
    framebuffer_layout: Arc<RwLock<FramebufferLayout>>,
    filepath: String,
    progress: ProgressFn,
    stop_rx: Receiver<()>,
) {
    use ruzu_core::core::{System, SystemRef, SystemResultStatus};

    progress(LoadStage::Prepare, 0, 0);

    // Log configuration (upstream logs settings during EmuWindow construction).
    common::settings::log_settings(&common::settings::values());

    // System init — upstream `Core::System system{}; system.Initialize();`.
    let mut system = System::new();
    system.initialize();

    // Content provider / filesystem / factories (upstream core.cpp:367-370).
    {
        use ruzu_core::file_sys::registered_cache::ContentProviderUnion;
        system.set_content_provider(Arc::new(std::sync::Mutex::new(ContentProviderUnion::new())));
        if system.get_filesystem().is_none() {
            system.set_filesystem(ruzu_core::file_sys::vfs::vfs_real::RealVfsFilesystem::new());
        }
        system
            .get_filesystem_controller()
            .lock()
            .unwrap()
            .create_factories(system.get_filesystem().unwrap().clone(), false);
        system.clear_user_channel();
    }

    // Subsystem factory (upstream SetupForApplicationProcess): Host1x + GPU +
    // Vulkan renderer + AudioCore. Called during `system.load()`.
    system.set_subsystem_factory(Box::new(move |system| {
        use std::sync::Arc;

        // Host1x (upstream core.cpp:277).
        let host1x =
            video_core::host1x::host1x::Host1x::new_with_system(SystemRef::from_ref(system));
        let syncpoints = host1x.syncpoint_manager().clone();
        let device_memory = host1x.memory_manager().clone();
        system.set_host1x_core(Box::new(host1x));

        // GPU (upstream core.cpp:278).
        let system_ref = SystemRef::from_ref(&system);
        let use_async_gpu = *common::settings::values()
            .use_asynchronous_gpu_emulation
            .get_value()
            && std::env::var_os("RUZU_DISABLE_ASYNC_GPU").is_none();
        let use_nvdec = *common::settings::values().nvdec_emulation.get_value()
            != common::settings_enums::NvdecEmulation::Off;
        let gpu = Box::new(video_core::gpu::Gpu::new(use_async_gpu, use_nvdec));
        gpu.set_system_ref(system_ref);
        let gpu_ptr = gpu.as_ref() as *const video_core::gpu::Gpu as usize;

        // Mutex-free raw pointer to the process `Memory`, shared by the GPU-side
        // memory callbacks. They run on the GPU thread while holding rasterizer
        // cache locks; taking the `Memory` mutex there deadlocks against guest
        // writes that re-enter the rasterizer. Every `Memory` method used below
        // takes `&self`; the pointee lives in the `Arc<Mutex<..>>` the system
        // keeps alive for the whole session. (Faithful to `ruzu_cmd`.)
        let memory_raw: Arc<OnceLock<usize>> = Arc::new(OnceLock::new());
        fn memory_raw_of(
            cell: &OnceLock<usize>,
            memory: &Arc<std::sync::Mutex<ruzu_core::memory::memory::Memory>>,
        ) -> *const ruzu_core::memory::memory::Memory {
            *cell.get_or_init(|| {
                let guard = memory.lock().unwrap();
                &*guard as *const ruzu_core::memory::memory::Memory as usize
            }) as *const ruzu_core::memory::memory::Memory
        }

        // Vulkan renderer (macOS: MoltenVK).
        let Some(host1x_core) = system.host1x_core() else {
            log::error!("Vulkan renderer selected before Host1x initialization");
            return;
        };
        let Some(host1x) = host1x_core
            .as_any()
            .downcast_ref::<video_core::host1x::host1x::Host1x>()
        else {
            log::error!("Vulkan renderer could not resolve Host1x memory manager");
            return;
        };
        let renderer_device_memory = Arc::clone(host1x.memory_manager());
        let frame_displayed_notify = Arc::new(|| {});
        let frame_end_notify = Arc::new(move || unsafe {
            let gpu_ref = &*(gpu_ptr as *const video_core::gpu::Gpu);
            gpu_ref.renderer_frame_end_notify();
        });
        let renderer: Box<dyn video_core::renderer_base::RendererBase> = Box::new(
            video_core::renderer_vulkan::renderer_vulkan::RendererVulkan::new(
                system.telemetry_session_mut(),
                &window_info,
                drawable_size,
                Arc::clone(&shown_state),
                Arc::clone(&framebuffer_layout),
                frame_displayed_notify,
                frame_end_notify,
                syncpoints.clone(),
                renderer_device_memory,
            )
            .unwrap_or_else(|e| {
                log::error!("Failed to create Vulkan renderer: {e}");
                std::process::exit(1);
            }),
        );
        gpu.bind_renderer(renderer);

        // GPU-side guest memory reader (SMMU → page table → DRAM-direct).
        let system_ref = SystemRef::from_ref(&system);
        let memory_raw_reader = memory_raw.clone();
        gpu.set_guest_memory_reader(Arc::new(move |addr, output: &mut [u8]| {
            let sys = system_ref.get();
            if let Some(host1x) = sys.host1x_core() {
                let host_ptr = host1x.smmu_lookup(addr);
                if host_ptr != 0 {
                    unsafe {
                        std::ptr::copy_nonoverlapping(
                            host_ptr as *const u8,
                            output.as_mut_ptr(),
                            output.len(),
                        );
                    }
                    return true;
                }
            }
            if let Some(memory) = sys.memory_shared() {
                let m = unsafe { &*memory_raw_of(&memory_raw_reader, &memory) };
                if m.read_block(addr, output) {
                    return true;
                }
                let dm = sys.device_memory();
                let base = ruzu_core::device_memory::dram_memory_map::BASE;
                if addr >= base {
                    let offset = (addr - base) as usize;
                    let backing = dm.buffer.backing_base_pointer();
                    unsafe {
                        std::ptr::copy_nonoverlapping(
                            backing.add(offset),
                            output.as_mut_ptr(),
                            output.len(),
                        );
                    }
                    return true;
                }
            }
            false
        }));

        // GPU-side guest memory writer (same resolution order).
        let system_ref = SystemRef::from_ref(&system);
        let memory_raw_writer = memory_raw.clone();
        gpu.set_guest_memory_writer(Arc::new(move |addr, data: &[u8]| {
            let sys = system_ref.get();
            if let Some(host1x) = sys.host1x_core() {
                let host_ptr = host1x.smmu_lookup(addr);
                if host_ptr != 0 {
                    unsafe {
                        std::ptr::copy_nonoverlapping(
                            data.as_ptr(),
                            host_ptr as *mut u8,
                            data.len(),
                        );
                    }
                    return;
                }
            }
            if let Some(memory) = sys.memory_shared() {
                let m = unsafe { &*memory_raw_of(&memory_raw_writer, &memory) };
                if m.write_block(addr, data) {
                    return;
                }
            }
            let dm = sys.device_memory();
            let base = ruzu_core::device_memory::dram_memory_map::BASE;
            if addr >= base {
                let offset = (addr - base) as usize;
                let backing = dm.buffer.backing_base_pointer();
                unsafe {
                    std::ptr::copy_nonoverlapping(
                        data.as_ptr(),
                        backing.add(offset) as *mut u8,
                        data.len(),
                    );
                }
            }
        }));

        // GPU VA → CPU VA translator for rasterizer-side query writes.
        let gpu_ptr_for_translator = gpu.as_ref() as *const video_core::gpu::Gpu;
        unsafe { gpu.install_gpu_to_cpu_translator(gpu_ptr_for_translator) };

        system.set_gpu_core(gpu);

        // AudioCore (upstream core.cpp:283).
        let shared_system: audio_core::SharedSystem =
            Arc::new(parking_lot::Mutex::new(System::new()));
        let ac = audio_core::AudioCore::new(shared_system);
        system.set_audio_core(Box::new(ac));
    }));

    // Load the ROM (upstream `system.Load(...)`). Triggers the factory above.
    let load_result = system.load(&filepath);
    if load_result != SystemResultStatus::Success {
        log::error!("Failed to load ROM '{filepath}': {load_result:?}");
        progress(LoadStage::Complete, 1, 1);
        return;
    }

    // Build the disk pipeline cache before starting execution (upstream order).
    if *common::settings::values().use_disk_shader_cache.get_value() {
        if let Some(gpu_any) = system.gpu_core() {
            if let Some(gpu) = gpu_any.as_any().downcast_ref::<video_core::gpu::Gpu>() {
                let mut renderer_guard = gpu.renderer();
                if let Some(renderer) = renderer_guard.as_mut() {
                    let rasterizer = renderer.read_rasterizer();
                    unsafe {
                        if let Some(rasterizer) = rasterizer.as_mut() {
                            rasterizer.load_disk_resources(system.runtime_program_id());
                        }
                    }
                }
            }
        }
    }

    // GPU start (upstream `system.GPU().Start()`).
    if let Some(gpu_any) = system.gpu_core() {
        if let Some(gpu) = gpu_any.as_any().downcast_ref::<video_core::gpu::Gpu>() {
            gpu.start();
        }
    }

    system.get_cpu_manager().on_gpu_ready();
    system.register_exit_callback(Box::new(|| {
        std::process::exit(0);
    }));

    // Loading done; the render page can take over.
    progress(LoadStage::Complete, 1, 1);

    // Run the guest (upstream `system.Run()`): starts CPU threads in background.
    system.run();

    // GTK owns the main event loop; here we simply wait for a stop signal
    // instead of `ruzu_cmd`'s `while (emu_window->IsOpen()) WaitEvent()`.
    let _ = stop_rx.recv();

    log::info!("Emulation stopping: pause + shutdown");
    system.pause();
    system.shutdown_main_process();

    // Force-exit like `ruzu_cmd`: some background threads (CPU idle loops, audio
    // ADSP, CoreTiming timer) block on condvars that aren't cleanly woken during
    // shutdown, so Rust's implicit static-drop / thread-join at process teardown
    // would hang or crash (the "send report to Apple" dialog). Upstream C++ exits
    // via `main()` return → `exit()` without joining detached threads.
    std::process::exit(0);
}
