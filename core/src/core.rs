//! Port of zuyu/src/core/core.h and zuyu/src/core/core.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-05
//!
//! The main System class. This is the top-level orchestrator that owns all
//! subsystems (CPU manager, timing, kernel, services, GPU, etc.).
//! The C++ uses a Pimpl pattern (System::Impl); here we flatten it into
//! a single struct.

use crate::core_timing::CoreTiming;
use crate::cpu_manager::CpuManager;
use crate::device_memory::DeviceMemory;
use crate::file_sys::fs_filesystem::OpenMode;
use crate::file_sys::vfs::vfs_real::RealVfsFilesystem;
use crate::hardware_properties;
use crate::hle::kernel::kernel::KernelCore;
use crate::hle::service::sm::sm::ServiceManager;
use crate::perf_stats::{PerfStats, PerfStatsResults, SpeedLimiter};

use parking_lot::Mutex;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex as StdMutex};

/// Enumeration representing the return values of the System Initialize and Load process.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SystemResultStatus {
    Success = 0,
    /// Error trying to use core prior to initialization
    ErrorNotInitialized,
    /// Error finding the correct application loader
    ErrorGetLoader,
    /// Error in finding system files
    ErrorSystemFiles,
    /// Error in finding shared font
    ErrorSharedFont,
    /// Error in the video core
    ErrorVideoCore,
    /// Any other error
    ErrorUnknown,
    /// The base for loader errors (too many to repeat)
    ErrorLoader,
}

/// Type used for the frontend to designate a callback for System to re-launch
/// the application using a specified program index.
pub type ExecuteProgramCallback = Box<dyn Fn(usize) + Send>;

/// Type used for the frontend to designate a callback for System to exit the application.
pub type ExitCallback = Box<dyn Fn() + Send>;

/// The main System struct. Top-level orchestrator that owns all subsystems.
///
/// In C++, this uses a Pimpl pattern (System::Impl). In Rust, we flatten
/// everything into this struct directly.
pub struct System {
    // ── Core subsystems ──
    /// The core timing system (event scheduling).
    pub core_timing: Arc<StdMutex<CoreTiming>>,

    /// The CPU manager (thread dispatch).
    pub cpu_manager: CpuManager,

    /// The kernel core (schedulers, physical cores, kernel objects).
    kernel: Option<KernelCore>,

    /// Shared HLE service registry.
    service_manager: Option<Arc<std::sync::Mutex<ServiceManager>>>,

    /// Device memory (emulated Switch DRAM).
    device_memory: Option<Box<DeviceMemory>>,

    // ── State flags ──
    /// Guard for suspend/resume operations.
    suspend_guard: Mutex<()>,
    /// Whether the system is currently paused.
    is_paused: AtomicBool,
    /// Whether the system is shutting down.
    is_shutting_down: AtomicBool,
    /// Whether the system is powered on (all subsystems initialized).
    is_powered_on: AtomicBool,

    /// Whether exit is locked (prevents accidental exit).
    exit_locked: bool,
    /// Whether exit has been requested.
    exit_requested: bool,

    /// Whether NVDEC is active.
    nvdec_active: bool,

    // ── Configuration ──
    /// Whether multicore mode is enabled.
    is_multicore: bool,
    /// Whether async GPU mode is enabled.
    is_async_gpu: bool,
    /// Whether extended memory layout is enabled.
    #[allow(dead_code)]
    extended_memory_layout: bool,

    // ── Performance tracking ──
    /// Performance statistics tracker.
    perf_stats: Option<PerfStats>,
    /// Speed limiter for single-core mode.
    pub speed_limiter: SpeedLimiter,

    // ── Status ──
    /// Current system status.
    status: SystemResultStatus,
    /// Detailed status information.
    status_details: String,

    // ── Application data ──
    /// Build ID for the currently running application process.
    build_id: [u8; 0x20],

    /// User channel for inter-process data transfer.
    user_channel: VecDeque<Vec<u8>>,

    // ── Callbacks ──
    /// Callback to re-launch the application with a specific program index.
    execute_program_callback: Option<ExecuteProgramCallback>,
    /// Callback to exit the application.
    exit_callback: Option<ExitCallback>,

    // ── Loader / VFS ──
    /// The application loader used to load the game.
    app_loader: Option<Box<dyn crate::loader::loader::AppLoader>>,
    /// The virtual filesystem backed by the real filesystem.
    virtual_filesystem: Option<Arc<RealVfsFilesystem>>,

    // ── Process ──
    /// The current application process.
    /// Stored here after loading so CPU execution can access the process memory.
    current_process: Option<crate::hle::kernel::k_process::KProcess>,
    /// Load parameters from the loader (thread priority, stack size).
    load_parameters: Option<crate::loader::loader::LoadParameters>,

    // ── Per-core profiling data ──
    /// Dynarmic tick counters per CPU core (for profiling).
    _dynarmic_ticks: [u64; hardware_properties::NUM_CPU_CORES as usize],
}

impl System {
    /// Creates a new System instance.
    pub fn new() -> Self {
        Self {
            core_timing: Arc::new(StdMutex::new(CoreTiming::new())),
            cpu_manager: CpuManager::new(),
            kernel: None,
            service_manager: None,
            device_memory: None,
            suspend_guard: Mutex::new(()),
            is_paused: AtomicBool::new(false),
            is_shutting_down: AtomicBool::new(false),
            is_powered_on: AtomicBool::new(false),
            exit_locked: false,
            exit_requested: false,
            nvdec_active: false,
            is_multicore: false,
            is_async_gpu: false,
            extended_memory_layout: false,
            perf_stats: None,
            speed_limiter: SpeedLimiter::new(),
            status: SystemResultStatus::Success,
            status_details: String::new(),
            build_id: [0u8; 0x20],
            user_channel: VecDeque::new(),
            execute_program_callback: None,
            exit_callback: None,
            app_loader: None,
            virtual_filesystem: None,
            current_process: None,
            load_parameters: None,
            _dynarmic_ticks: [0u64; hardware_properties::NUM_CPU_CORES as usize],
        }
    }

    /// Initializes the system.
    /// This function will initialize core functionality used for system emulation.
    ///
    /// Corresponds to C++ System::Impl::Initialize().
    pub fn initialize(&mut self) {
        // Allocate device memory
        self.device_memory = Some(Box::new(DeviceMemory::new()));

        // Read configuration from settings
        // In C++: is_multicore = Settings::values.use_multi_core.GetValue()
        // For now, default to false (single-core). The caller can override.
        // self.is_multicore = Settings::values.use_multi_core...;
        // self.extended_memory_layout = ...;

        self.core_timing.lock().unwrap().set_multicore(self.is_multicore);
        // In C++: core_timing.Initialize([&system]() { system.RegisterHostThread(); });
        self.core_timing.lock().unwrap().initialize(|| {
            // Host thread registration placeholder.
            // In full implementation, this registers the timer thread with the kernel.
        });

        self.cpu_manager.set_multicore(self.is_multicore);
        self.cpu_manager.set_async_gpu(self.is_async_gpu);

        let mut kernel = KernelCore::new();
        kernel.set_multicore(self.is_multicore);
        kernel.initialize();
        self.kernel = Some(kernel);
        self.service_manager = Some(crate::hle::service::sm::sm::create_service_manager());

        log::info!("System: initialized (multicore={}, async_gpu={})", self.is_multicore, self.is_async_gpu);
    }

    /// Load the game at the given filepath.
    ///
    /// Corresponds to C++ System::Load().
    /// Creates a RealVfsFilesystem, opens the game file, identifies the loader,
    /// and calls AppLoader::load().
    pub fn load(&mut self, filepath: &str) -> SystemResultStatus {
        // Create a RealVfsFilesystem if not already set.
        if self.virtual_filesystem.is_none() {
            self.virtual_filesystem = Some(RealVfsFilesystem::new());
        }

        let vfs = self.virtual_filesystem.as_ref().unwrap();

        // Open the game file.
        let file = match vfs.arc_open_file(filepath, OpenMode::READ) {
            Some(f) => f,
            None => {
                log::error!("Failed to open ROM file: {}", filepath);
                return SystemResultStatus::ErrorGetLoader;
            }
        };

        // Get the appropriate loader for this file type.
        let mut loader_system = crate::loader::loader::System;
        let loader = crate::loader::loader::get_loader(&mut loader_system, file, 0, 0);

        let mut loader = match loader {
            Some(l) => l,
            None => {
                log::error!("Failed to obtain loader for ROM file: {}", filepath);
                return SystemResultStatus::ErrorGetLoader;
            }
        };

        // Create a KProcess and call the loader.
        let mut process = crate::loader::loader::KProcess::new();
        let (result_status, load_parameters) = loader.load(&mut process, &mut loader_system);

        if result_status != crate::loader::loader::ResultStatus::Success {
            log::error!(
                "Failed to load ROM: {} ({})",
                crate::loader::loader::get_result_status_string(result_status),
                filepath,
            );
            self.status = SystemResultStatus::ErrorLoader;
            return SystemResultStatus::ErrorLoader;
        }

        // Store the loader and process.
        self.app_loader = Some(loader);
        self.load_parameters = load_parameters;
        self.current_process = Some(process);
        self.is_powered_on.store(true, Ordering::Relaxed);
        self.status = SystemResultStatus::Success;

        log::info!("Successfully loaded ROM: {}", filepath);
        SystemResultStatus::Success
    }

    /// Run the OS and Application.
    /// This function will start emulation and run the relevant devices.
    pub fn run(&mut self) {
        let _lk = self.suspend_guard.lock();

        // In C++: kernel.SuspendEmulation(false);
        self.core_timing.lock().unwrap().sync_pause(false);
        self.is_paused.store(false, Ordering::Relaxed);

        log::info!("System: running");
    }

    /// Pause the OS and Application.
    /// This function will pause emulation and stop the relevant devices.
    pub fn pause(&mut self) {
        let _lk = self.suspend_guard.lock();

        self.core_timing.lock().unwrap().sync_pause(true);
        // In C++: kernel.SuspendEmulation(true);
        self.is_paused.store(true, Ordering::Relaxed);

        log::info!("System: paused");
    }

    /// Check if the core is currently paused.
    pub fn is_paused(&self) -> bool {
        self.is_paused.load(Ordering::Relaxed)
    }

    /// Shutdown the main emulated process.
    pub fn shutdown_main_process(&mut self) {
        self.set_shutting_down(true);

        // Log last frame performance stats if game was loaded
        if let Some(ref perf_stats) = self.perf_stats {
            let perf_results = perf_stats.get_and_reset_stats(
                self.core_timing.lock().unwrap().get_global_time_us()
            );
            log::info!(
                "Shutdown stats: speed={:.1}%, fps={:.1}, frametime={:.3}ms",
                perf_results.emulation_speed * 100.0,
                perf_results.average_game_fps,
                perf_results.frametime * 1000.0
            );
        }

        self.is_powered_on.store(false, Ordering::Relaxed);
        self.exit_locked = false;
        self.exit_requested = false;

        // In C++: gpu_core->NotifyShutdown();
        // stop_event.request_stop();

        self.core_timing.lock().unwrap().sync_pause(false);

        // In C++:
        // Network::CancelPendingSocketOperations();
        // kernel.SuspendEmulation(true);
        // kernel.CloseServices();
        // kernel.ShutdownCores();
        // services.reset();
        // service_manager.reset();
        // fs_controller.Reset();
        // cheat_engine.reset();
        // telemetry_session.reset();

        self.core_timing.lock().unwrap().clear_pending_events();

        // In C++:
        // app_loader.reset();
        // audio_core.reset();
        // gpu_core.reset();
        // host1x_core.reset();

        self.perf_stats = None;
        self.cpu_manager.shutdown();

        // In C++:
        // debugger.reset();
        // kernel.Shutdown();
        // stop_event = {};
        // Network::RestartSocketOperations();

        log::info!("System: shutdown complete");
    }

    /// Check if the core is shutting down.
    pub fn is_shutting_down(&self) -> bool {
        self.is_shutting_down.load(Ordering::Relaxed)
    }

    /// Set the shutting down state.
    pub fn set_shutting_down(&self, shutting_down: bool) {
        self.is_shutting_down.store(shutting_down, Ordering::Relaxed);
    }

    /// Stall the application (pause with lock held).
    /// Returns the lock guard which must be dropped to unstall.
    pub fn stall_application(&mut self) -> parking_lot::MutexGuard<'_, ()> {
        let lk = self.suspend_guard.lock();
        // In C++: kernel.SuspendEmulation(true);
        self.core_timing.lock().unwrap().sync_pause(true);
        lk
    }

    /// Unstall the application (resume if not paused).
    pub fn unstall_application(&mut self) {
        if !self.is_paused() {
            self.core_timing.lock().unwrap().sync_pause(false);
            // In C++: kernel.SuspendEmulation(false);
        }
    }

    /// Check if the system is powered on (all subsystems initialized and able to run).
    pub fn is_powered_on(&self) -> bool {
        self.is_powered_on.load(Ordering::Relaxed)
    }

    /// Set NVDEC active state.
    pub fn set_nvdec_active(&mut self, is_nvdec_active: bool) {
        self.nvdec_active = is_nvdec_active;
    }

    /// Get NVDEC active state.
    pub fn get_nvdec_active(&self) -> bool {
        self.nvdec_active
    }

    /// Gets a reference to the core timing instance.
    pub fn core_timing(&self) -> Arc<StdMutex<CoreTiming>> {
        self.core_timing.clone()
    }

    /// Gets a shared reference to the core timing instance.
    pub fn core_timing_shared(&self) -> Arc<StdMutex<CoreTiming>> {
        self.core_timing.clone()
    }

    /// Gets a reference to the CPU manager.
    pub fn get_cpu_manager(&self) -> &CpuManager {
        &self.cpu_manager
    }

    /// Gets a mutable reference to the CPU manager.
    pub fn get_cpu_manager_mut(&mut self) -> &mut CpuManager {
        &mut self.cpu_manager
    }

    /// Gets a reference to the device memory.
    pub fn device_memory(&self) -> &DeviceMemory {
        self.device_memory
            .as_ref()
            .expect("DeviceMemory not initialized; call System::initialize() first")
    }

    /// Gets a mutable reference to the device memory.
    pub fn device_memory_mut(&mut self) -> &mut DeviceMemory {
        self.device_memory
            .as_mut()
            .expect("DeviceMemory not initialized; call System::initialize() first")
    }

    /// Gets a reference to the performance stats, if available.
    pub fn get_perf_stats(&self) -> Option<&PerfStats> {
        self.perf_stats.as_ref()
    }

    /// Gets a reference to the speed limiter.
    pub fn speed_limiter(&self) -> &SpeedLimiter {
        &self.speed_limiter
    }

    /// Gets a mutable reference to the speed limiter.
    pub fn speed_limiter_mut(&mut self) -> &mut SpeedLimiter {
        &mut self.speed_limiter
    }

    /// Gets and resets performance statistics.
    pub fn get_and_reset_perf_stats(&self) -> PerfStatsResults {
        match &self.perf_stats {
            Some(stats) => stats.get_and_reset_stats(
                self.core_timing.lock().unwrap().get_global_time_us()
            ),
            None => PerfStatsResults::default(),
        }
    }

    /// Sets the system status and optional detail message.
    pub fn set_status(&mut self, new_status: SystemResultStatus, details: Option<&str>) {
        self.status = new_status;
        if let Some(d) = details {
            self.status_details = d.to_string();
        }
    }

    /// Returns the current system status.
    pub fn status(&self) -> SystemResultStatus {
        self.status
    }

    /// Returns the status details string.
    pub fn status_details(&self) -> &str {
        &self.status_details
    }

    /// Returns whether multicore mode is enabled.
    pub fn is_multicore(&self) -> bool {
        self.is_multicore
    }

    /// Sets multicore mode. Should be called before initialize().
    pub fn set_multicore(&mut self, multicore: bool) {
        self.is_multicore = multicore;
    }

    /// Sets async GPU mode. Should be called before initialize().
    pub fn set_async_gpu(&mut self, async_gpu: bool) {
        self.is_async_gpu = async_gpu;
    }

    /// Set whether exit is locked.
    pub fn set_exit_locked(&mut self, locked: bool) {
        self.exit_locked = locked;
    }

    /// Get whether exit is locked.
    pub fn get_exit_locked(&self) -> bool {
        self.exit_locked
    }

    /// Set whether exit has been requested.
    pub fn set_exit_requested(&mut self, requested: bool) {
        self.exit_requested = requested;
    }

    /// Get whether exit has been requested.
    pub fn get_exit_requested(&self) -> bool {
        self.exit_requested
    }

    /// Set the application process build ID.
    pub fn set_application_process_build_id(&mut self, id: [u8; 0x20]) {
        self.build_id = id;
    }

    /// Get the application process build ID.
    pub fn get_application_process_build_id(&self) -> &[u8; 0x20] {
        &self.build_id
    }

    /// Gets a mutable reference to the user channel.
    /// Used to transfer data between programs.
    pub fn get_user_channel(&mut self) -> &mut VecDeque<Vec<u8>> {
        &mut self.user_channel
    }

    /// Registers a callback from the frontend for System to re-launch the application.
    pub fn register_execute_program_callback(&mut self, callback: ExecuteProgramCallback) {
        self.execute_program_callback = Some(callback);
    }

    /// Instructs the frontend to re-launch the application using the specified program_index.
    pub fn execute_program(&self, program_index: usize) {
        if let Some(ref callback) = self.execute_program_callback {
            callback(program_index);
        } else {
            log::error!("execute_program_callback must be initialized by the frontend");
        }
    }

    /// Registers a callback from the frontend for System to exit the application.
    pub fn register_exit_callback(&mut self, callback: ExitCallback) {
        self.exit_callback = Some(callback);
    }

    /// Instructs the frontend to exit the application.
    pub fn exit(&self) {
        if let Some(ref callback) = self.exit_callback {
            callback();
        } else {
            log::error!("exit_callback must be initialized by the frontend");
        }
    }

    /// Register a host thread as an emulated CPU Core.
    /// Placeholder - full implementation requires the kernel module.
    pub fn register_core_thread(&mut self, _id: usize) {
        // In C++: impl->kernel.RegisterCoreThread(id);
    }

    /// Register a host thread as an auxiliary thread.
    /// Placeholder - full implementation requires the kernel module.
    pub fn register_host_thread(&mut self) {
        // In C++: impl->kernel.RegisterHostThread();
    }

    /// Get a reference to the kernel core.
    pub fn kernel(&self) -> Option<&KernelCore> {
        self.kernel.as_ref()
    }

    /// Get a mutable reference to the kernel core.
    pub fn kernel_mut(&mut self) -> Option<&mut KernelCore> {
        self.kernel.as_mut()
    }

    /// Get the shared HLE service registry.
    pub fn service_manager(&self) -> Option<Arc<std::sync::Mutex<ServiceManager>>> {
        self.service_manager.clone()
    }

    /// Get a reference to the current application process.
    pub fn current_process(&self) -> Option<&crate::hle::kernel::k_process::KProcess> {
        self.current_process.as_ref()
    }

    /// Get a mutable reference to the current application process.
    pub fn current_process_mut(&mut self) -> Option<&mut crate::hle::kernel::k_process::KProcess> {
        self.current_process.as_mut()
    }

    /// Get the load parameters from the last successful load.
    pub fn load_parameters(&self) -> Option<&crate::loader::loader::LoadParameters> {
        self.load_parameters.as_ref()
    }

    /// Initialize performance stats for a specific title.
    pub fn init_perf_stats(&mut self, title_id: u64) {
        self.perf_stats = Some(PerfStats::new(title_id));
        // Reset counters and set time origin to current frame
        self.get_and_reset_perf_stats();
        if let Some(ref stats) = self.perf_stats {
            stats.begin_system_frame();
        }
    }
}

impl Default for System {
    fn default() -> Self {
        Self::new()
    }
}
