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

use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU64, Ordering};
use std::sync::mpsc::{Receiver, RecvTimeoutError, Sender, SyncSender};
use std::sync::{Arc, OnceLock, RwLock};
use std::thread::JoinHandle;
use std::time::Duration;

use ruzu_core::frontend::emu_window::WindowSystemInfo;
use ruzu_core::frontend::framebuffer_layout::FramebufferLayout;
use ruzu_core::perf_stats::PerfStatsResults;

use crate::loading_screen::LoadStage;

#[derive(Debug, Clone, Copy)]
pub struct BootParameters {
    pub applet: ruzu_core::hle::service::am::applet_manager::FrontendAppletParameters,
    pub cabinet_mode: Option<ruzu_core::hle::service::am::frontend::applet_cabinet::CabinetMode>,
}

impl Default for BootParameters {
    fn default() -> Self {
        use ruzu_core::hle::service::am::am_types::{AppletId, AppletType};
        use ruzu_core::hle::service::am::applet_manager::{FrontendAppletParameters, LaunchType};

        Self {
            applet: FrontendAppletParameters {
                applet_id: AppletId::Application,
                applet_type: AppletType::Application,
                launch_type: LaunchType::FrontendInitiated,
                program_index: 0,
                previous_program_index: -1,
                ..FrontendAppletParameters::default()
            },
            cabinet_mode: None,
        }
    }
}

enum EmulationCommand {
    Stop,
    Pause(SyncSender<()>),
    Resume(SyncSender<()>),
    CaptureScreenshot {
        path: std::path::PathBuf,
        layout: FramebufferLayout,
    },
}

/// Assets read from the current application loader for the loading screen.
#[derive(Debug, Default)]
pub struct LoadingScreenAssets {
    pub logo: Option<Vec<u8>>,
    pub banner: Option<Vec<u8>>,
}

/// Cross-thread events consumed by the GTK loading screen.
#[derive(Debug)]
pub enum LoadingEvent {
    Assets(LoadingScreenAssets),
    Progress {
        stage: LoadStage,
        value: usize,
        total: usize,
    },
    FirstFrame,
    Failed {
        message: String,
        detail: String,
    },
    Stopped {
        before_first_frame: bool,
    },
    StopComplete,
}

/// Loading event callback marshaled to the GTK main thread.
pub type LoadingEventFn = Arc<dyn Fn(LoadingEvent) + Send + Sync + 'static>;

/// A running emulation session. Dropping (or calling [`Self::stop`]) shuts the
/// system down: `System::pause` + `System::shutdown_main_process`, mirroring the
/// tail of `ruzu_cmd`'s `main`.
pub struct EmulationSession {
    command_tx: Option<Sender<EmulationCommand>>,
    join: Option<JoinHandle<()>>,
    perf_results: Arc<RwLock<PerfStatsResults>>,
    shaders_building: Arc<AtomicI32>,
    running: Arc<AtomicBool>,
    paused: Arc<AtomicBool>,
    program_id: Arc<AtomicU64>,
    exit_locked: Arc<AtomicBool>,
    frontend_stop_requested: Arc<AtomicBool>,
}

impl EmulationSession {
    /// Return the most recent 500 ms performance sample while the guest runs.
    ///
    /// Upstream's GUI calls `System::GetAndResetPerfStats()` from its status
    /// timer. The Rust `System` remains owned by the boot thread, so that thread
    /// performs the same reset and publishes this copy for GTK.
    pub fn perf_stats(&self) -> Option<PerfStatsResults> {
        if !self.running.load(Ordering::Acquire) {
            return None;
        }
        Some(
            *self
                .perf_results
                .read()
                .unwrap_or_else(|poisoned| poisoned.into_inner()),
        )
    }

    /// Return the latest `GPU::ShaderNotify().ShadersBuilding()` sample.
    pub fn shaders_building(&self) -> Option<i32> {
        self.running
            .load(Ordering::Acquire)
            .then(|| self.shaders_building.load(Ordering::Acquire))
    }

    pub fn program_id(&self) -> Option<u64> {
        self.running
            .load(Ordering::Acquire)
            .then(|| self.program_id.load(Ordering::Acquire))
    }

    /// Whether the running application requested that frontend exits be
    /// confirmed. Mirrors `Core::System::GetExitLocked()`.
    pub fn exit_locked(&self) -> bool {
        self.exit_locked.load(Ordering::Acquire)
    }

    /// Begin the upstream graceful shutdown path without blocking the GTK main
    /// loop. Completion is reported through [`LoadingEvent::StopComplete`].
    pub fn request_stop(&mut self) -> bool {
        if let Some(tx) = self.command_tx.take() {
            self.frontend_stop_requested.store(true, Ordering::Release);
            let _ = tx.send(EmulationCommand::Stop);
            true
        } else {
            false
        }
    }

    pub fn capture_screenshot(&self, path: std::path::PathBuf, layout: FramebufferLayout) -> bool {
        self.command_tx.as_ref().is_some_and(|tx| {
            tx.send(EmulationCommand::CaptureScreenshot { path, layout })
                .is_ok()
        })
    }

    pub fn is_paused(&self) -> bool {
        self.paused.load(Ordering::Acquire)
    }

    pub fn pause(&self) -> bool {
        if self.is_paused() {
            return true;
        }
        let Some(tx) = self.command_tx.as_ref() else {
            return false;
        };
        let (completed_tx, completed_rx) = std::sync::mpsc::sync_channel(0);
        let completed =
            tx.send(EmulationCommand::Pause(completed_tx)).is_ok() && completed_rx.recv().is_ok();
        if completed {
            self.paused.store(true, Ordering::Release);
        }
        completed
    }

    pub fn resume(&self) -> bool {
        if !self.is_paused() {
            return true;
        }
        let Some(tx) = self.command_tx.as_ref() else {
            return false;
        };
        let (completed_tx, completed_rx) = std::sync::mpsc::sync_channel(0);
        let completed =
            tx.send(EmulationCommand::Resume(completed_tx)).is_ok() && completed_rx.recv().is_ok();
        if completed {
            self.paused.store(false, Ordering::Release);
        }
        completed
    }

    /// Signal the boot thread to shut the system down and join it.
    pub fn stop(&mut self) {
        let _ = self.request_stop();
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
    hid_core: Arc<parking_lot::Mutex<hid_core::hid_core::HIDCore>>,
    tas: Option<Arc<parking_lot::Mutex<input_common::drivers::tas_input::Tas>>>,
    filepath: String,
    parameters: BootParameters,
    loading_event: LoadingEventFn,
) -> EmulationSession {
    let (command_tx, command_rx) = std::sync::mpsc::channel::<EmulationCommand>();
    let perf_results = Arc::new(RwLock::new(PerfStatsResults::default()));
    let shaders_building = Arc::new(AtomicI32::new(0));
    let running = Arc::new(AtomicBool::new(false));
    let paused = Arc::new(AtomicBool::new(false));
    let program_id = Arc::new(AtomicU64::new(0));
    let frontend_stop_requested = Arc::new(AtomicBool::new(false));
    let (exit_locked_tx, exit_locked_rx) = std::sync::mpsc::sync_channel(1);
    let boot_perf_results = Arc::clone(&perf_results);
    let boot_shaders_building = Arc::clone(&shaders_building);
    let boot_running = Arc::clone(&running);
    let boot_program_id = Arc::clone(&program_id);
    let boot_frontend_stop_requested = Arc::clone(&frontend_stop_requested);

    let join = std::thread::Builder::new()
        .name("ruzu-boot".into())
        .spawn(move || {
            run_boot(
                window_info,
                drawable_size,
                shown_state,
                framebuffer_layout,
                hid_core,
                tas,
                filepath,
                parameters,
                loading_event,
                command_rx,
                boot_perf_results,
                boot_shaders_building,
                boot_running,
                boot_program_id,
                exit_locked_tx,
                boot_frontend_stop_requested,
            );
        })
        .expect("spawn boot thread");

    let exit_locked = exit_locked_rx
        .recv()
        .expect("boot thread publishes System exit-lock state");

    EmulationSession {
        command_tx: Some(command_tx),
        join: Some(join),
        perf_results,
        shaders_building,
        running,
        paused,
        program_id,
        exit_locked,
        frontend_stop_requested,
    }
}

/// The boot body, run on the boot thread. Faithful to `ruzu_cmd`'s Vulkan path.
fn run_boot(
    window_info: WindowSystemInfo,
    drawable_size: (u32, u32),
    shown_state: Arc<AtomicBool>,
    framebuffer_layout: Arc<RwLock<FramebufferLayout>>,
    hid_core: Arc<parking_lot::Mutex<hid_core::hid_core::HIDCore>>,
    tas: Option<Arc<parking_lot::Mutex<input_common::drivers::tas_input::Tas>>>,
    filepath: String,
    parameters: BootParameters,
    loading_event: LoadingEventFn,
    command_rx: Receiver<EmulationCommand>,
    perf_results: Arc<RwLock<PerfStatsResults>>,
    shaders_building: Arc<AtomicI32>,
    running: Arc<AtomicBool>,
    program_id: Arc<AtomicU64>,
    exit_locked_tx: SyncSender<Arc<AtomicBool>>,
    frontend_stop_requested: Arc<AtomicBool>,
) {
    use ruzu_core::core::{System, SystemRef, SystemResultStatus};

    let first_frame_displayed = Arc::new(AtomicBool::new(false));
    let guest_exit_requested = Arc::new(AtomicBool::new(false));

    loading_event(LoadingEvent::Progress {
        stage: LoadStage::Prepare,
        value: 0,
        total: 0,
    });

    // Log configuration (upstream logs settings during EmuWindow construction).
    common::settings::log_settings(&common::settings::values());

    // System init — upstream `Core::System system{}; system.Initialize();`.
    let tas_hid_core = Arc::clone(&hid_core);
    let mut system = System::new_with_hid_core(hid_core);
    let _ = exit_locked_tx.send(system.exit_locked_state());
    system.initialize();
    system
        .frontend_applet_holder_mut()
        .set_current_applet_id(parameters.applet.applet_id);
    if let Some(mode) = parameters.cabinet_mode {
        system.frontend_applet_holder_mut().set_cabinet_mode(mode);
    }

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
    let frame_loading_event = Arc::clone(&loading_event);
    let frame_displayed = Arc::clone(&first_frame_displayed);
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
        let frame_displayed_notify = Arc::new(move || {
            if let Some(tas) = tas.as_ref() {
                use hid_core::hid_types::NpadIdType;
                use input_common::drivers::tas_input::TasAnalog;

                let controller = tas_hid_core
                    .lock()
                    .get_emulated_controller(NpadIdType::Player1);
                let controller = controller.lock();
                let buttons = controller
                    .get_buttons_values()
                    .iter()
                    .enumerate()
                    .fold(0u64, |buttons, (index, value)| {
                        buttons | (u64::from(value.value) << index)
                    });
                let sticks = controller.get_sticks();
                drop(controller);
                let mut tas = tas.lock();
                tas.record_input(
                    buttons,
                    TasAnalog {
                        x: sticks.left.x as f32 / 32767.0,
                        y: sticks.left.y as f32 / 32767.0,
                    },
                    TasAnalog {
                        x: sticks.right.x as f32 / 32767.0,
                        y: sticks.right.y as f32 / 32767.0,
                    },
                );
                tas.update_thread();
            }
            if !frame_displayed.swap(true, Ordering::AcqRel) {
                frame_loading_event(LoadingEvent::FirstFrame);
            }
        });
        let frame_end_notify = Arc::new(move || unsafe {
            let gpu_ref = &*(gpu_ptr as *const video_core::gpu::Gpu);
            gpu_ref.renderer_frame_end_notify();
        });
        let renderer: Box<dyn video_core::renderer_base::RendererBase> = Box::new(
            video_core::renderer_vulkan::renderer_vulkan::RendererVulkan::new(
                system.telemetry_session_mut(),
                // SAFETY: this renderer is immediately bound to `gpu` below;
                // `Gpu` drops the renderer before its shader notifier.
                unsafe { gpu.shader_notify_handle() },
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
        let ac = audio_core::AudioCore::new(SystemRef::from_ref(system));
        system.set_audio_core(Box::new(ac));
    }));

    // Load the ROM (upstream `system.Load(...)`). Triggers the factory above.
    let load_result = system.load_with_parameters(&filepath, parameters.applet);
    if load_result != SystemResultStatus::Success {
        log::error!("Failed to load ROM '{filepath}': {load_result:?}");
        loading_event(LoadingEvent::Failed {
            message: "Unable to start the game".to_owned(),
            detail: load_error_detail(load_result).to_owned(),
        });
        return;
    }
    program_id.store(system.runtime_program_id(), Ordering::Release);

    let loader = system.get_app_loader();
    let mut assets = LoadingScreenAssets::default();
    let mut buffer = Vec::new();
    if loader.read_banner(&mut buffer) == ruzu_core::loader::loader::ResultStatus::Success {
        assets.banner = Some(std::mem::take(&mut buffer));
    }
    if loader.read_logo(&mut buffer) == ruzu_core::loader::loader::ResultStatus::Success {
        assets.logo = Some(buffer);
    }
    loading_event(LoadingEvent::Assets(assets));

    // Build the disk pipeline cache before starting execution (upstream order).
    if *common::settings::values().use_disk_shader_cache.get_value() {
        if let Some(gpu_any) = system.gpu_core() {
            if let Some(gpu) = gpu_any.as_any().downcast_ref::<video_core::gpu::Gpu>() {
                let mut renderer_guard = gpu.renderer();
                if let Some(renderer) = renderer_guard.as_mut() {
                    let rasterizer = renderer.read_rasterizer();
                    unsafe {
                        if let Some(rasterizer) = rasterizer.as_mut() {
                            let loading_event = Arc::clone(&loading_event);
                            let callback: video_core::rasterizer_interface::DiskResourceLoadCallback =
                                Arc::new(move |stage, value, total| {
                                    let stage = match stage {
                                        video_core::rasterizer_interface::LoadCallbackStage::Prepare => {
                                            LoadStage::Prepare
                                        }
                                        video_core::rasterizer_interface::LoadCallbackStage::Build => {
                                            LoadStage::Build
                                        }
                                        video_core::rasterizer_interface::LoadCallbackStage::Complete => {
                                            LoadStage::Complete
                                        }
                                    };
                                    loading_event(LoadingEvent::Progress {
                                        stage,
                                        value,
                                        total,
                                    });
                                });
                            rasterizer.load_disk_resources(system.runtime_program_id(), callback);
                        }
                    }
                }
            }
        }
    }

    // Upstream emits Complete immediately after disk resources are loaded and
    // before releasing the graphics context / starting the GPU.
    loading_event(LoadingEvent::Progress {
        stage: LoadStage::Complete,
        value: 0,
        total: 0,
    });

    // GPU start (upstream `system.GPU().Start()`).
    if let Some(gpu_any) = system.gpu_core() {
        if let Some(gpu) = gpu_any.as_any().downcast_ref::<video_core::gpu::Gpu>() {
            gpu.start();
        }
    }

    system.get_cpu_manager().on_gpu_ready();
    let exit_requested = Arc::clone(&guest_exit_requested);
    system.register_exit_callback(Box::new(move || {
        exit_requested.store(true, Ordering::Release);
    }));

    // Run the guest (upstream `system.Run()`): starts CPU threads in background.
    system.run();

    // GTK owns the main event loop. The boot thread retains ownership of
    // `System`, samples the same counters as upstream's 500 ms GUI timer, and
    // waits for a stop request between samples.
    let stopped_by_frontend = loop {
        if guest_exit_requested.load(Ordering::Acquire) {
            break false;
        }
        match command_rx.recv_timeout(Duration::from_millis(500)) {
            Ok(EmulationCommand::Stop) | Err(RecvTimeoutError::Disconnected) => {
                frontend_stop_requested.store(true, Ordering::Release);
                break true;
            }
            Ok(EmulationCommand::CaptureScreenshot { path, layout }) => {
                request_screenshot(&system, path, layout);
            }
            Ok(EmulationCommand::Pause(completed)) => {
                system.pause();
                let _ = completed.send(());
            }
            Ok(EmulationCommand::Resume(completed)) => {
                system.run();
                let _ = completed.send(());
            }
            Err(RecvTimeoutError::Timeout) => {
                let sample = system.get_and_reset_perf_stats();
                *perf_results
                    .write()
                    .unwrap_or_else(|poisoned| poisoned.into_inner()) = sample;
                let building = system
                    .gpu_core()
                    .and_then(|gpu| gpu.as_any().downcast_ref::<video_core::gpu::Gpu>())
                    .map(|gpu| gpu.shader_notify().shaders_building())
                    .unwrap_or(0);
                shaders_building.store(building, Ordering::Release);
                running.store(true, Ordering::Release);
            }
        }
    };
    running.store(false, Ordering::Release);
    shaders_building.store(0, Ordering::Release);

    if stopped_by_frontend {
        // Upstream `RequestGameExit`: let the application process its normal
        // exit request before `OnEmulationStopTimeExpired` forces shutdown.
        system.set_shutting_down(true);
        system.set_exit_requested(true);
        system.get_applet_manager().request_exit();
        let timeout = if system.debugger_enabled() {
            Duration::ZERO
        } else if system.get_exit_locked() {
            Duration::from_secs(5)
        } else {
            Duration::from_secs(1)
        };
        let deadline = std::time::Instant::now() + timeout;
        while !guest_exit_requested.load(Ordering::Acquire) && std::time::Instant::now() < deadline
        {
            std::thread::sleep(Duration::from_millis(10));
        }
    }

    log::info!("Emulation stopping: pause + shutdown");
    system.pause();
    system.shutdown_main_process();
    loading_event(terminal_event_after_shutdown(
        frontend_stop_requested.load(Ordering::Acquire),
        first_frame_displayed.load(Ordering::Acquire),
    ));
}

fn request_screenshot(
    system: &ruzu_core::core::System,
    path: std::path::PathBuf,
    layout: FramebufferLayout,
) {
    let Some(gpu) = system
        .gpu_core()
        .and_then(|gpu| gpu.as_any().downcast_ref::<video_core::gpu::Gpu>())
    else {
        log::error!("Cannot capture screenshot: GPU renderer is unavailable");
        return;
    };
    let mut renderer = gpu.renderer();
    let Some(renderer) = renderer.as_mut() else {
        log::error!("Cannot capture screenshot: renderer is unavailable");
        return;
    };
    if renderer.is_screenshot_pending() {
        log::warn!("A screenshot is already requested or in progress");
        return;
    }

    let width = layout.width;
    let height = layout.height;
    let mut pixels = vec![0u8; width as usize * height as usize * 4];
    let pixels_ptr = pixels.as_mut_ptr().cast();
    renderer.request_screenshot(
        pixels_ptr,
        Box::new(move |invert_y| {
            if invert_y {
                let stride = width as usize * 4;
                for y in 0..height as usize / 2 {
                    let opposite = height as usize - y - 1;
                    let (top, bottom) = pixels.split_at_mut(opposite * stride);
                    top[y * stride..(y + 1) * stride].swap_with_slice(&mut bottom[..stride]);
                }
            }
            let result = (|| -> Result<(), Box<dyn std::error::Error>> {
                if let Some(parent) = path.parent() {
                    std::fs::create_dir_all(parent)?;
                }
                let stride = cairo::Format::Rgb24.stride_for_width(width)?;
                let surface = cairo::ImageSurface::create_for_data(
                    pixels,
                    cairo::Format::Rgb24,
                    width as i32,
                    height as i32,
                    stride,
                )?;
                let mut file = std::fs::File::create(&path)?;
                surface.write_to_png(&mut file)?;
                Ok(())
            })();
            match result {
                Ok(()) => log::info!("Screenshot saved to {:?}", path),
                Err(error) => log::error!("Failed to save screenshot to {:?}: {error}", path),
            }
        }),
        layout,
    );
}

/// Select the terminal frontend event after the emulation thread has completed
/// `ShutdownMainProcess`. Upstream emits `QThread::finished` only after that
/// teardown; publishing earlier makes GTK join the still-running boot thread.
fn terminal_event_after_shutdown(
    frontend_stop_requested: bool,
    first_frame_displayed: bool,
) -> LoadingEvent {
    if frontend_stop_requested {
        LoadingEvent::StopComplete
    } else {
        LoadingEvent::Stopped {
            before_first_frame: !first_frame_displayed,
        }
    }
}

fn load_error_detail(status: ruzu_core::core::SystemResultStatus) -> &'static str {
    use ruzu_core::core::SystemResultStatus;

    match status {
        SystemResultStatus::ErrorGetLoader => "The ROM format is not supported.",
        SystemResultStatus::ErrorVideoCore => {
            "The video renderer could not be initialized. Check the log for details."
        }
        SystemResultStatus::ErrorLoader => {
            "The game data could not be loaded. Check the log for details."
        }
        _ => "An unknown error occurred while loading the game. Check the log for details.",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ruzu_core::core::SystemResultStatus;

    #[test]
    fn default_boot_parameters_launch_an_application() {
        use ruzu_core::hle::service::am::am_types::{AppletId, AppletType};
        use ruzu_core::hle::service::am::applet_manager::LaunchType;

        let parameters = BootParameters::default();
        assert_eq!(parameters.applet.applet_id, AppletId::Application);
        assert_eq!(parameters.applet.applet_type, AppletType::Application);
        assert_eq!(parameters.applet.launch_type, LaunchType::FrontendInitiated);
        assert_eq!(parameters.applet.previous_program_index, -1);
        assert!(parameters.cabinet_mode.is_none());
    }

    #[test]
    fn requesting_stop_is_idempotent_and_marks_frontend_shutdown() {
        let (command_tx, command_rx) = std::sync::mpsc::channel();
        let frontend_stop_requested = Arc::new(AtomicBool::new(false));
        let mut session = EmulationSession {
            command_tx: Some(command_tx),
            join: None,
            perf_results: Arc::new(RwLock::new(PerfStatsResults::default())),
            shaders_building: Arc::new(AtomicI32::new(0)),
            running: Arc::new(AtomicBool::new(false)),
            paused: Arc::new(AtomicBool::new(false)),
            program_id: Arc::new(AtomicU64::new(0)),
            exit_locked: Arc::new(AtomicBool::new(false)),
            frontend_stop_requested: Arc::clone(&frontend_stop_requested),
        };

        assert!(session.request_stop());
        assert!(frontend_stop_requested.load(Ordering::Acquire));
        assert!(matches!(command_rx.recv(), Ok(EmulationCommand::Stop)));
        assert!(!session.request_stop());
    }

    #[test]
    fn pause_and_resume_round_trip_updates_session_state() {
        let (command_tx, command_rx) = std::sync::mpsc::channel();
        let mut session = EmulationSession {
            command_tx: Some(command_tx),
            join: None,
            perf_results: Arc::new(RwLock::new(PerfStatsResults::default())),
            shaders_building: Arc::new(AtomicI32::new(0)),
            running: Arc::new(AtomicBool::new(true)),
            paused: Arc::new(AtomicBool::new(false)),
            program_id: Arc::new(AtomicU64::new(0)),
            exit_locked: Arc::new(AtomicBool::new(false)),
            frontend_stop_requested: Arc::new(AtomicBool::new(false)),
        };
        let worker = std::thread::spawn(move || {
            let Ok(EmulationCommand::Pause(completed)) = command_rx.recv() else {
                panic!("expected pause command");
            };
            completed.send(()).unwrap();
            let Ok(EmulationCommand::Resume(completed)) = command_rx.recv() else {
                panic!("expected resume command");
            };
            completed.send(()).unwrap();
            assert!(matches!(command_rx.recv(), Ok(EmulationCommand::Stop)));
        });

        assert!(!session.is_paused());
        assert!(session.pause());
        assert!(session.is_paused());
        assert!(session.resume());
        assert!(!session.is_paused());
        session.stop();
        worker.join().unwrap();
    }

    #[test]
    fn terminal_event_is_selected_only_after_shutdown_completes() {
        assert!(matches!(
            terminal_event_after_shutdown(false, true),
            LoadingEvent::Stopped {
                before_first_frame: false
            }
        ));
        assert!(matches!(
            terminal_event_after_shutdown(false, false),
            LoadingEvent::Stopped {
                before_first_frame: true
            }
        ));
        assert!(matches!(
            terminal_event_after_shutdown(true, true),
            LoadingEvent::StopComplete
        ));
    }

    #[test]
    fn load_errors_have_frontend_facing_details() {
        assert_eq!(
            load_error_detail(SystemResultStatus::ErrorGetLoader),
            "The ROM format is not supported."
        );
        assert!(load_error_detail(SystemResultStatus::ErrorVideoCore).contains("renderer"));
        assert!(load_error_detail(SystemResultStatus::ErrorUnknown).contains("unknown"));
    }
}
