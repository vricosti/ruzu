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
use crate::file_sys::romfs_factory::StorageId;
use crate::file_sys::vfs::vfs_real::RealVfsFilesystem;
use crate::gpu_dirty_memory_manager::GpuDirtyMemoryManager;
use crate::hardware_properties;
use crate::hle::kernel::k_process::SharedProcessMemory;
use crate::hle::kernel::k_scheduler::KScheduler;
use crate::hle::kernel::k_thread::KThread;
use crate::hle::kernel::kernel::KernelCore;
use crate::hle::service::am::am_types::{AppletId, AppletType};
use crate::hle::service::am::applet_manager::{
    AppletManager, FrontendAppletParameters, LaunchType,
};
use crate::hle::service::apm::apm_controller::Controller as ApmController;
use crate::hle::service::glue::glue_manager::{ARPManager, ApplicationLaunchProperty};
use crate::hle::service::server_manager::ServerManager;
use crate::hle::service::sm::sm::ServiceManager;
use crate::memory::memory::Memory;
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

/// A non-owning reference to System, mirroring upstream's `Core::System&`.
///
/// System is stack-allocated in main() and outlives all services. This is the
/// same lifetime guarantee upstream relies on. Services store `SystemRef` the
/// same way upstream stores `System& m_system`.
///
/// # Safety
/// The System pointed to must outlive all holders of SystemRef. This is
/// guaranteed by construction: System is created in main() before services,
/// and services are destroyed (via System::shutdown/drop) before System.
#[derive(Clone, Copy)]
pub struct SystemRef(*const System);

unsafe impl Send for SystemRef {}
unsafe impl Sync for SystemRef {}

impl SystemRef {
    /// Create a SystemRef from a reference to System.
    pub fn from_ref(system: &System) -> Self {
        Self(system as *const System)
    }

    /// Create a null SystemRef (for default initialization).
    pub fn null() -> Self {
        Self(std::ptr::null())
    }

    /// Check if this is a null reference.
    pub fn is_null(&self) -> bool {
        self.0.is_null()
    }

    /// Access the System reference.
    ///
    /// # Safety
    /// Caller must ensure the System is still alive. This is guaranteed
    /// by the construction contract (System outlives all services).
    pub fn get(&self) -> &System {
        assert!(!self.0.is_null(), "SystemRef is null");
        unsafe { &*self.0 }
    }

    /// Get a reference to the Reporter.
    /// Upstream: `system.GetReporter()`.
    pub fn get_reporter(&self) -> &Arc<crate::reporter::Reporter> {
        &self.get().reporter
    }
}

/// Type used for the frontend to designate a callback for System to exit the application.
pub type ExitCallback = Box<dyn Fn() + Send>;

/// Minimal bridge from `core` to a concrete `audio_core::renderer::Renderer`
/// session owned by AudioCore.
///
/// Upstream `IAudioRenderer` owns `AudioCore::Renderer::Renderer` directly.
/// Rust cannot name that concrete type from `core`, so exercised service calls
/// go through this owner-preserving trait object.
pub trait AudioRendererSessionInterface: Send {
    fn get_sample_rate(&self) -> u32;
    fn get_sample_count(&self) -> u32;
    fn get_mix_buffer_count(&self) -> u32;
    fn get_state(&self) -> u32;
    fn request_update(
        &self,
        input: &[u8],
        performance: &mut [u8],
        output: &mut [u8],
    ) -> crate::hle::result::ResultCode;
    fn start(&self);
    fn stop(&self);
    fn supports_system_event(&self) -> bool;
    fn set_process(&self, process: *mut crate::hle::kernel::k_process::KProcess);
    fn set_rendering_time_limit(&self, rendering_time_limit: u32);
    fn get_rendering_time_limit(&self) -> u32;
    fn set_voice_drop_parameter(&self, voice_drop_parameter: f32);
    fn get_voice_drop_parameter(&self) -> f32;
    /// Pass the readable event Arc so the audio thread can signal it directly.
    fn set_rendered_readable_event(
        &self,
        _event: std::sync::Arc<
            std::sync::Mutex<crate::hle::kernel::k_readable_event::KReadableEvent>,
        >,
    ) {
    }
    /// Pass the process Arc for thread-safe waiter notification.
    fn set_process_arc(
        &self,
        _process: std::sync::Arc<std::sync::Mutex<crate::hle::kernel::k_process::KProcess>>,
    ) {
    }
}

/// Minimal bridge from `core` to the concrete `audio_core` crate.
///
/// Upstream `Core::System` owns `AudioCore::AudioCore` directly. Rust cannot do
/// that without a crate cycle, so `System` stores a trait object that preserves
/// the same owner boundary for exercised service calls.
pub trait AudioCoreInterface: Send {
    /// Mirror `AudioCore::Renderer::Manager::GetWorkBufferSize`.
    fn get_audio_renderer_work_buffer_size(&self, params: &[u8; 0x34]) -> Option<u64>;

    /// Mirror `AudioCore::Renderer::AudioDevice::ListAudioDeviceName`.
    fn list_audio_device_name(
        &self,
        applet_resource_user_id: u64,
        revision: u32,
        out_names: &mut [[u8; 0x100]],
    ) -> u32;

    /// Mirror `AudioCore::Renderer::AudioDevice::ListAudioOutputDeviceName`.
    fn list_audio_output_device_name(
        &self,
        applet_resource_user_id: u64,
        revision: u32,
        out_names: &mut [[u8; 0x100]],
    ) -> u32;

    /// Mirror `AudioCore::Renderer::AudioDevice::SetDeviceVolumes`.
    fn set_audio_device_volume(&self, applet_resource_user_id: u64, revision: u32, volume: f32);

    /// Mirror `AudioCore::Renderer::AudioDevice::GetDeviceVolume`.
    fn get_audio_device_volume(
        &self,
        applet_resource_user_id: u64,
        revision: u32,
        name: &str,
    ) -> f32;

    /// Mirror `AudioCore::Sink::Sink::GetSystemChannels`.
    fn get_audio_output_system_channels(&self) -> u32;

    /// Mirror `IAudioRendererManager::OpenAudioRenderer` by creating an owned
    /// audio renderer session backed by the real audio_core renderer owners.
    fn open_audio_renderer(
        &self,
        params: &[u8; 0x34],
        transfer_memory: *mut crate::hle::kernel::k_transfer_memory::KTransferMemory,
        transfer_memory_size: u64,
        process: *mut crate::hle::kernel::k_process::KProcess,
        applet_resource_user_id: u64,
        rendered_event: std::sync::Arc<std::sync::Mutex<crate::hle::kernel::k_event::KEvent>>,
    ) -> std::result::Result<Box<dyn AudioRendererSessionInterface>, crate::hle::result::ResultCode>;
}

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

    /// Applet manager used to register frontend-launched applets with AM.
    applet_manager: AppletManager,

    /// Shared APM controller state.
    /// Upstream: `System::GetAPMController()`.
    apm_controller: Arc<StdMutex<ApmController>>,

    /// Shared ARP manager for application launch/control properties.
    /// Upstream: `System::GetARPManager()`.
    arp_manager: Arc<StdMutex<ARPManager>>,

    /// Filesystem controller (process registrations, factory management).
    /// Upstream: `FileSystemController& GetFileSystemController()`.
    filesystem_controller:
        Arc<StdMutex<crate::hle::service::filesystem::filesystem::FileSystemController>>,

    /// Content provider union for NCA/content lookups.
    /// Upstream: `std::unique_ptr<FileSys::ContentProviderUnion> content_provider`.
    content_provider:
        Option<Arc<StdMutex<crate::file_sys::registered_cache::ContentProviderUnion>>>,

    /// Telemetry session for collecting and submitting usage data.
    /// Upstream: `std::unique_ptr<Core::TelemetrySession> telemetry_session`.
    telemetry_session: Option<crate::telemetry_session::TelemetrySession>,

    /// Host1x subsystem (syncpoint manager, device memory, GMMU, CDMA devices).
    /// Upstream: `std::unique_ptr<Tegra::Host1x::Host1x> host1x_core`.
    /// Stored behind an opaque bridge so `core` keeps upstream ownership
    /// without naming `video_core` concrete types.
    host1x_core: Option<Box<dyn crate::host1x_core::Host1xCoreInterface>>,

    /// GPU core (channels, engines, rendering, host synchronization).
    /// Upstream: `std::unique_ptr<Tegra::GPU> gpu_core`.
    /// Type-erased because video_core crate does not depend on core.
    /// The frontend creates the concrete GPU and sets it via set_gpu_core().
    gpu_core: Option<Box<dyn crate::gpu_core::GpuCoreInterface>>,

    /// AudioCore subsystem (audio sinks, ADSP, audio manager).
    /// Upstream: `std::unique_ptr<AudioCore::AudioCore> audio_core`.
    /// The frontend creates the concrete AudioCore and sets it via set_audio_core().
    audio_core: Option<Box<dyn AudioCoreInterface>>,

    /// Device memory (emulated Switch DRAM).
    device_memory: Option<Box<DeviceMemory>>,
    /// Core::Memory::Memory bridge (maps virtual→physical→host).
    /// Upstream: `std::unique_ptr<Core::Memory::Memory> m_memory`.
    memory: Option<Arc<StdMutex<Memory>>>,
    /// Upstream owner: `std::array<GPUDirtyMemoryManager, NUM_CPU_CORES>`.
    gpu_dirty_memory_managers: Vec<Arc<StdMutex<GpuDirtyMemoryManager>>>,

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
    /// Factory callback for creating video/audio subsystems.
    /// Called by setup_for_application_process() to create Host1x, GPU, AudioCore.
    /// Upstream creates these directly in SetupForApplicationProcess (core.cpp:277-283)
    /// but Rust can't due to circular crate dependencies (video_core/audio_core depend on core).
    /// The frontend registers this callback before calling load().
    subsystem_factory: Option<Box<dyn FnOnce(&mut System) + Send>>,

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

    // ── Runtime SVC state ──
    // These fields are set during the bootstrap phase (after load, before JIT
    // dispatch) and match the role of upstream `Core::System&` as passed to SVC
    // handlers. Upstream SVC handlers receive `Core::System&` and derive
    // process, scheduler, memory, etc. from it.
    /// The current application process, wrapped for shared access.
    /// Set by the frontend after `System::load()` returns.
    /// Upstream: `system.CurrentProcess()`.
    pub(crate) current_process_arc: Option<Arc<StdMutex<crate::hle::kernel::k_process::KProcess>>>,

    /// The cooperative scheduler for the main core.
    /// Upstream has per-core schedulers in the kernel; here we use one.
    pub(crate) scheduler_arc: Option<Arc<StdMutex<KScheduler>>>,

    /// Cached shared process memory (the RwLock-wrapped ProcessMemoryData).
    /// Used as a fallback memory accessor when the Memory bridge is not set
    /// (tests and legacy paths). Upstream equivalent: process.GetMemory().
    pub(crate) shared_process_memory: Option<SharedProcessMemory>,

    /// Reporter for saving telemetry/crash/error reports.
    /// Upstream: `system.GetReporter()`.
    pub reporter: Arc<crate::reporter::Reporter>,

    /// The title/program ID for the running application.
    /// Upstream: `system.GetApplicationProcessProgramID()`.
    pub(crate) runtime_program_id: u64,

    /// Whether the loaded process is AArch64.
    /// Upstream: `process.Is64Bit()`.
    pub(crate) runtime_is_64bit: bool,
}

impl System {
    /// Creates a new System instance.
    pub fn new() -> Self {
        Self {
            core_timing: Arc::new(StdMutex::new(CoreTiming::new())),
            cpu_manager: CpuManager::new(),
            kernel: None,
            telemetry_session: None,
            host1x_core: None,
            gpu_core: None,
            audio_core: None,
            service_manager: None,
            applet_manager: AppletManager::new(),
            apm_controller: Arc::new(StdMutex::new(ApmController::new())),
            arp_manager: Arc::new(StdMutex::new(ARPManager::new())),
            filesystem_controller: Arc::new(StdMutex::new(
                crate::hle::service::filesystem::filesystem::FileSystemController::new(),
            )),
            content_provider: None,
            device_memory: None,
            memory: None,
            gpu_dirty_memory_managers: Vec::new(),
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
            subsystem_factory: None,
            app_loader: None,
            virtual_filesystem: None,
            current_process: None,
            load_parameters: None,
            _dynarmic_ticks: [0u64; hardware_properties::NUM_CPU_CORES as usize],
            current_process_arc: None,
            scheduler_arc: None,
            shared_process_memory: None,
            reporter: Arc::new(crate::reporter::Reporter::new()),
            runtime_program_id: 0,
            runtime_is_64bit: false,
        }
    }

    /// Initializes the system.
    /// This function will initialize core functionality used for system emulation.
    ///
    /// Corresponds to C++ System::Impl::Initialize() (core.cpp:118-144).
    /// Only performs lightweight configuration. Kernel initialization and service
    /// creation happen later in load() via initialize_kernel() and
    /// setup_for_application_process(), matching upstream's phased approach.
    pub fn initialize(&mut self) {
        // Allocate device memory
        self.device_memory = Some(Box::new(DeviceMemory::new()));

        // Create the Memory bridge (maps virtual→physical→host via PageTable).
        // Upstream: m_memory = make_unique<Core::Memory::Memory>(system)
        let dm_ptr = self.device_memory.as_ref().unwrap().as_ref() as *const DeviceMemory;
        let buffer_ptr = unsafe { &(*dm_ptr).buffer as *const common::host_memory::HostMemory };
        let mut memory = unsafe { Memory::new(SystemRef::from_ref(self), dm_ptr, buffer_ptr) };
        self.gpu_dirty_memory_managers = (0..hardware_properties::NUM_CPU_CORES as usize)
            .map(|_| Arc::new(StdMutex::new(GpuDirtyMemoryManager::new())))
            .collect();
        memory.set_gpu_dirty_managers(self.gpu_dirty_memory_managers.clone());
        self.memory = Some(Arc::new(StdMutex::new(memory)));

        // Read configuration from settings.
        // In C++: is_multicore = Settings::values.use_multi_core.GetValue()
        self.is_multicore = *common::settings::values().use_multi_core.get_value();

        self.core_timing
            .lock()
            .unwrap()
            .set_multicore(self.is_multicore);
        // In C++: core_timing.Initialize([&system]() { system.RegisterHostThread(); });
        let system_ref = SystemRef::from_ref(self);
        self.core_timing.lock().unwrap().initialize(move || {
            // Upstream: core_timing.Initialize([&system]() { system.RegisterHostThread(); });
            system_ref.get().kernel().unwrap().register_host_thread();
        });

        // Ensure VFS exists. Upstream does this in Initialize().
        if self.virtual_filesystem.is_none() {
            self.virtual_filesystem = Some(RealVfsFilesystem::new());
        }

        // Provide the VFS to the filesystem controller so it can create SaveDataFactory instances.
        if let Some(ref vfs) = self.virtual_filesystem {
            self.filesystem_controller
                .lock()
                .unwrap()
                .set_filesystem(vfs.clone());
        }

        self.cpu_manager.set_multicore(self.is_multicore);
        self.cpu_manager.set_async_gpu(self.is_async_gpu);

        // Create kernel but do NOT initialize it yet.
        // Upstream creates kernel in Impl::Impl() (constructor), and only calls
        // kernel.Initialize() later in InitializeKernel() (called from Load()).
        let mut kernel = KernelCore::new();
        kernel.set_multicore(self.is_multicore);
        self.kernel = Some(kernel);

        log::info!(
            "System: initialized (multicore={}, async_gpu={})",
            self.is_multicore,
            self.is_async_gpu
        );
    }

    /// Initialize the kernel and CPU manager.
    ///
    /// Corresponds to C++ InitializeKernel(system) (core.cpp:264-272).
    /// Called at the start of load(), before ROM loading.
    fn initialize_kernel(&mut self) {
        // Upstream: KernelCore holds System& from construction.
        // Set it here since Rust can't pass &self during construction.
        let system_ref = SystemRef::from_ref(self);

        let kernel = self
            .kernel
            .as_mut()
            .expect("kernel must be created in initialize()");

        // Upstream: ReinitializeIfNecessary() checks if multicore/memory layout
        // changed and re-runs Initialize() if so. We just call initialize() directly.
        kernel.initialize();
        kernel.set_system_ref(system_ref);

        // Initialize the kernel physical memory manager with a Secure pool.
        // Upstream traverses the memory layout tree; we reserve a region at
        // the top of DeviceMemory DRAM to avoid collision with guest
        // identity-mapped regions (which use low addresses).
        {
            use crate::device_memory::dram_memory_map;
            use crate::hle::kernel::k_memory_manager::Pool;
            const SECURE_POOL_OFFSET: u64 = 0xF000_0000; // 3.75 GiB into DRAM
            const SECURE_POOL_SIZE: usize = 256 * 1024 * 1024; // 256 MiB
            let secure_pool_base = dram_memory_map::BASE + SECURE_POOL_OFFSET;
            kernel.memory_manager_mut().initialize_pool(
                Pool::SECURE,
                secure_pool_base,
                SECURE_POOL_SIZE,
            );
        }

        // Provide CoreTiming to the kernel so guest thread functions can access it.
        self.kernel
            .as_mut()
            .unwrap()
            .set_core_timing(self.core_timing.clone());
        self.kernel
            .as_ref()
            .unwrap()
            .wire_hardware_timer(self.core_timing.clone());

        // Schedule preemption event (10ms interval) and start timer thread.
        // Upstream: InitializePreemption in kernel.cpp schedules a looping event.
        self.kernel
            .as_ref()
            .unwrap()
            .schedule_preemption_event(&self.core_timing);
        CoreTiming::start_timer_thread(self.core_timing.clone());

        // Upstream: cpu_manager.Initialize() — creates barrier and spawns per-core
        // host threads that wait on the GPU barrier before yielding to guest fibers.
        // Safety: kernel outlives the threads (shutdown joins them first).
        let kernel_ptr = self.kernel.as_ref().unwrap() as *const KernelCore;
        unsafe {
            self.cpu_manager.initialize(kernel_ptr);
        }

        // Wire up the system reference in AppletManager so it can access kernel,
        // core_timing, and user_channel during SetWindowSystem.
        // Upstream: AppletManager constructor takes Core::System& m_system.
        let system_ref = SystemRef::from_ref(self);
        self.applet_manager.set_system(system_ref);

        log::info!("System: kernel initialized");
    }

    /// Set up subsystems that depend on the application process being loaded.
    ///
    /// Corresponds to C++ SetupForApplicationProcess(system, emu_window)
    /// (core.cpp:274-305). Called from load() after ROM loading and process
    /// creation, but before applet manager registration.
    fn setup_for_application_process(&mut self) {
        // Upstream order (core.cpp:274-305):

        // 1. TelemetrySession
        // Upstream: telemetry_session = std::make_unique<Core::TelemetrySession>();
        self.telemetry_session = Some(crate::telemetry_session::TelemetrySession::new());

        // 2-4. Host1x, GPU, AudioCore
        // Upstream creates these directly (core.cpp:277-283):
        //   host1x_core = make_unique<Host1x>(system);
        //   gpu_core = VideoCore::CreateGPU(emu_window, system);
        //   audio_core = make_unique<AudioCore>(system);
        // In Rust, video_core/audio_core crates depend on core (circular dep),
        // so the frontend provides a factory callback that creates them.
        if let Some(factory) = self.subsystem_factory.take() {
            factory(self);
        }

        // 5. Create the ServiceManager.
        // Upstream: service_manager = std::make_shared<SM::ServiceManager>(kernel);
        let service_manager = Arc::new(StdMutex::new(ServiceManager::new()));

        // Store service_manager on System BEFORE launching services,
        // so that ServerManager::register_named_service can find it via
        // system.service_manager() during registration.
        self.service_manager = Some(service_manager.clone());

        // 6. Launch all system services.
        // Upstream: services = std::make_unique<Services>(service_manager, system, stop_token);
        let dm_ptr = self.device_memory.as_ref().unwrap().as_ref() as *const DeviceMemory;
        let mm_ptr = self.kernel.as_mut().unwrap().memory_manager_mut() as *mut _;
        let system_ref = SystemRef::from_ref(self);
        let _services = crate::hle::service::services::Services::new(
            &service_manager,
            system_ref,
            dm_ptr,
            mm_ptr,
            self.filesystem_controller.clone(),
        );

        // Upstream: is_powered_on = true, exit_locked = false, exit_requested = false
        self.is_powered_on.store(true, Ordering::Relaxed);
        self.exit_locked = false;
        self.exit_requested = false;

        log::info!("System: application process setup complete (services created)");
    }

    /// Load the game at the given filepath.
    ///
    /// Corresponds to C++ System::Impl::Load() (core.cpp:307-399).
    /// Phases:
    ///   1. InitializeKernel — kernel + cpu_manager init
    ///   2. CreateApplicationProcess — loader, ROM loading, process creation
    ///   3. SetupForApplicationProcess — ServiceManager, Services, powered on
    pub fn load(&mut self, filepath: &str) -> SystemResultStatus {
        // Phase 1: Initialize kernel (upstream: InitializeKernel(system))
        self.initialize_kernel();

        let vfs = self
            .virtual_filesystem
            .as_ref()
            .expect("VFS must be created in initialize()");

        // Open the game file.
        let file = match vfs.arc_open_file(filepath, OpenMode::READ) {
            Some(f) => f,
            None => {
                log::error!("Failed to open ROM file: {}", filepath);
                return SystemResultStatus::ErrorGetLoader;
            }
        };

        // Get the appropriate loader for this file type.
        let mut loader_system = crate::loader::loader::System {
            content_provider: self.content_provider.clone(),
            filesystem_controller: Some(self.filesystem_controller.clone()),
        };
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

        // Create per-process Memory (matching upstream `Core::Memory::Memory m_memory`
        // owned by KProcess). Each process gets its own Memory instance with its
        // own current_page_table, sharing the DeviceMemory backing with System.
        process.create_memory(self);

        let (result_status, load_parameters) = loader.load(&mut process, &mut loader_system);

        // Note: set_current_page_table is called inside initialize_for_user
        // (matching upstream k_process.cpp:423), so no need to call it here.

        if result_status != crate::loader::loader::ResultStatus::Success {
            log::error!(
                "Failed to load ROM: {} ({})",
                crate::loader::loader::get_result_status_string(result_status),
                filepath,
            );
            self.status = SystemResultStatus::ErrorLoader;
            return SystemResultStatus::ErrorLoader;
        }

        if let Some(kernel) = self.kernel_mut() {
            process.process_id = kernel.create_new_user_process_id();
            // Upstream: kernel.MakeApplicationProcess(process->GetHandle())
            // Registers this process as THE application process.
        }

        // Store the loader and process before setup_for_application_process,
        // since services may query System state.
        self.app_loader = Some(loader);
        self.load_parameters = load_parameters;
        self.current_process = Some(process);

        if let Some(ref process) = self.current_process {
            let program_id = process.get_program_id();
            let launch = ApplicationLaunchProperty {
                title_id: program_id,
                version: 0,
                base_game_storage_id: StorageId::Host as u8,
                update_storage_id: StorageId::None as u8,
                program_index: 0,
                reserved: 0,
            };
            let control =
                vec![0u8; std::mem::size_of::<crate::file_sys::control_metadata::RawNACP>()];
            let result = self
                .arp_manager
                .lock()
                .unwrap()
                .register(program_id, launch, control);
            if result != crate::hle::result::RESULT_SUCCESS {
                log::warn!(
                    "System::load: failed to register ARP launch property for {:016X}: {:?}",
                    program_id,
                    result
                );
            }
        }

        // Phase 3: Set up GPU, audio, services (upstream: SetupForApplicationProcess)
        self.setup_for_application_process();

        // Register the process with the filesystem controller.
        // Upstream: this happens inside each loader's Load() (e.g. nca.cpp:77-80),
        // but in the Rust port the process_id is assigned after Load() returns
        // (see kernel.create_new_user_process_id() above), so registration must
        // happen here instead.
        if let Some(ref process) = self.current_process {
            let romfs_factory = self
                .app_loader
                .as_ref()
                .zip(self.content_provider.as_ref())
                .map(|(app_loader, content_provider)| {
                    Arc::new(crate::file_sys::romfs_factory::RomFSFactory::new(
                        app_loader.as_ref(),
                        content_provider.clone(),
                        self.filesystem_controller.clone(),
                    ))
                });
            self.filesystem_controller.lock().unwrap().register_process(
                process.process_id,
                process.get_program_id(),
                romfs_factory,
            );
        }

        // Upstream: applet_manager.CreateAndInsertByFrontendAppletParameters(process, params)
        // followed by applet->process->Run() at the end of SetWindowSystem.
        if let Some(process) = self.current_process.take() {
            let lp = self
                .load_parameters
                .as_ref()
                .expect("loader must provide process launch parameters");
            let is_64bit = process.is_64bit();
            let priority = lp.main_thread_priority;
            let stack_size = lp.main_thread_stack_size as usize;

            // Wrap in Arc<Mutex<>> — run() needs self_reference for thread creation.
            let process_arc = Arc::new(StdMutex::new(process));
            process_arc
                .lock()
                .unwrap()
                .bind_self_reference(&process_arc);

            // Wire the global scheduler context and per-core scheduler so the
            // process can update priority queues when threads become runnable.
            if let Some(ref kernel) = self.kernel {
                if let Some(gsc) = kernel.global_scheduler_context() {
                    process_arc
                        .lock()
                        .unwrap()
                        .set_global_scheduler_context(gsc.clone());
                }
                // Attach core-0's scheduler to the process.
                if let Some(scheduler) = kernel.scheduler(0) {
                    process_arc.lock().unwrap().attach_scheduler(scheduler);
                }
            }

            // Initialize ARM JIT interfaces before process.run() so that when
            // the guest thread starts it can immediately call get_arm_interface_mut().
            // Must happen before create_and_insert_by_frontend_applet_parameters so
            // it is done before set_window_system calls process.run().
            {
                let shared_memory = process_arc.lock().unwrap().get_shared_memory();
                let core_timing = self.core_timing.clone();
                process_arc
                    .lock()
                    .unwrap()
                    .initialize_interfaces(shared_memory, core_timing);
                log::info!("ARM JIT interfaces initialized for application process");
            }

            // Dump the memory block layout for diagnostics.
            process_arc.lock().unwrap().page_table.dump_memory_blocks();

            // Build the guest-thread entry closure.
            // Upstream: KThread::InitializeUserThread passes
            // system.GetCpuManager().GetGuestThreadFunc() as the fiber entry.
            // We transmit the kernel pointer as usize because the crate is named
            // `core` which shadows `std::core`, preventing `unsafe impl Send`
            // for raw pointer wrappers.
            let guest_thread_func: Option<Box<dyn FnOnce() + Send>> = {
                let kp = self.kernel.as_ref().unwrap() as *const KernelCore as usize;
                Some(Box::new(move || {
                    // Safety: kernel_ptr is valid for the lifetime of the thread
                    // (System owns KernelCore and joins all threads before drop).
                    let kernel = unsafe { &*(kp as *const KernelCore) };
                    let is_multicore = kernel.is_multicore();
                    if is_multicore {
                        crate::cpu_manager::CpuManager::multi_core_run_guest_thread(kernel);
                    } else {
                        crate::cpu_manager::CpuManager::single_core_run_guest_thread_entry(kernel);
                    }
                }))
            };

            // Use kernel's ID allocators to avoid collisions with per-core threads.
            let (app_thread_id, app_object_id) = if let Some(ref kernel) = self.kernel {
                (
                    kernel.create_new_thread_id(),
                    kernel.create_new_object_id() as u64,
                )
            } else {
                (1, 1)
            };

            // Extract program_id in a separate binding so the MutexGuard<KProcess>
            // is dropped before calling create_and_insert_by_frontend_applet_parameters.
            let app_program_id = process_arc.lock().unwrap().get_program_id();

            // Upstream: CreateAndInsertByFrontendAppletParameters stores process +
            // params and notifies the CV. SetWindowSystem (running on the
            // am:SetWindowSystem thread) will wake, build the applet, track it,
            // and then call process->Run() with the run params we pass here.
            self.applet_manager
                .create_and_insert_by_frontend_applet_parameters(
                    process_arc.clone(),
                    FrontendAppletParameters {
                        program_id: app_program_id,
                        applet_id: AppletId::Application,
                        applet_type: AppletType::Application,
                        launch_type: LaunchType::FrontendInitiated,
                        program_index: 0,
                        previous_program_index: -1,
                    },
                    crate::hle::service::am::applet_manager::PendingRunParameters {
                        priority,
                        stack_size,
                        main_thread_id: app_thread_id,
                        main_object_id: app_object_id,
                        is_64bit,
                        init_func: guest_thread_func,
                    },
                );

            self.current_process_arc = Some(process_arc);
        }

        self.status = SystemResultStatus::Success;

        log::info!("Successfully loaded ROM: {}", filepath);
        SystemResultStatus::Success
    }

    /// Run the OS and Application.
    /// This function will start emulation and run the relevant devices.
    pub fn run(&mut self) {
        let _lk = self.suspend_guard.lock();

        if let Some(ref kernel) = self.kernel {
            kernel.suspend_emulation(false);
        }
        self.core_timing.lock().unwrap().sync_pause(false);
        self.is_paused.store(false, Ordering::Relaxed);

        log::info!("System: running");
    }

    /// Pause the OS and Application.
    /// This function will pause emulation and stop the relevant devices.
    pub fn pause(&mut self) {
        let _lk = self.suspend_guard.lock();

        self.core_timing.lock().unwrap().sync_pause(true);
        if let Some(ref kernel) = self.kernel {
            kernel.suspend_emulation(true);
        }
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
            let perf_results = perf_stats
                .get_and_reset_stats(self.core_timing.lock().unwrap().get_global_time_us());
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

        self.core_timing.lock().unwrap().sync_pause(false);
        if let Some(ref kernel) = self.kernel {
            kernel.suspend_emulation(true);
            kernel.close_services();
            kernel.shutdown_cores();
        }
        self.service_manager = None;

        // Upstream: telemetry_session.reset();
        self.telemetry_session = None;

        self.core_timing.lock().unwrap().clear_pending_events();

        // Upstream: app_loader.reset();
        self.app_loader = None;
        // Upstream: audio_core.reset();
        self.audio_core = None;
        // Upstream: gpu_core.reset();
        self.gpu_core = None;
        // Upstream: host1x_core.reset();
        self.host1x_core = None;

        self.perf_stats = None;
        self.cpu_manager.shutdown();
        self.current_process_arc = None;
        self.current_process = None;
        if let Some(ref mut kernel) = self.kernel {
            kernel.shutdown();
        }

        log::info!("System: shutdown complete");
    }

    /// Check if the core is shutting down.
    pub fn is_shutting_down(&self) -> bool {
        self.is_shutting_down.load(Ordering::Relaxed)
    }

    /// Set the shutting down state.
    pub fn set_shutting_down(&self, shutting_down: bool) {
        self.is_shutting_down
            .store(shutting_down, Ordering::Relaxed);
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

    /// Get the telemetry session.
    /// Upstream: `System::TelemetrySession()`.
    pub fn telemetry_session(&self) -> Option<&crate::telemetry_session::TelemetrySession> {
        self.telemetry_session.as_ref()
    }

    /// Get the telemetry session mutably.
    pub fn telemetry_session_mut(
        &mut self,
    ) -> Option<&mut crate::telemetry_session::TelemetrySession> {
        self.telemetry_session.as_mut()
    }

    /// Set the Host1x core subsystem.
    /// Called by the frontend after System::load() since video_core does not
    /// depend on core. Upstream: created in SetupForApplicationProcess() (core.cpp:277).
    pub fn set_host1x_core(&mut self, host1x: Box<dyn crate::host1x_core::Host1xCoreInterface>) {
        self.host1x_core = Some(host1x);
    }

    /// Get the Host1x core.
    /// Upstream: `System::Host1x()` returns `Tegra::Host1x::Host1x&`.
    pub fn host1x_core(&self) -> Option<&dyn crate::host1x_core::Host1xCoreInterface> {
        self.host1x_core.as_deref()
    }

    pub fn host1x_core_mut(&mut self) -> Option<&mut dyn crate::host1x_core::Host1xCoreInterface> {
        self.host1x_core.as_deref_mut()
    }

    /// Set the GPU core subsystem.
    /// Called by the frontend after System::load() since video_core does not
    /// depend on core. Upstream: created in SetupForApplicationProcess() (core.cpp:278).
    pub fn set_gpu_core(&mut self, gpu: Box<dyn crate::gpu_core::GpuCoreInterface>) {
        self.gpu_core = Some(gpu);
    }

    /// Get the GPU core (type-erased).
    /// Callers must downcast to the concrete GPU type.
    /// Upstream: `System::GPU()` returns `Tegra::GPU&`.
    pub fn gpu_core(&self) -> Option<&dyn crate::gpu_core::GpuCoreInterface> {
        self.gpu_core.as_deref()
    }

    /// Get the GPU core mutably (type-erased).
    pub fn gpu_core_mut(&mut self) -> Option<&mut dyn crate::gpu_core::GpuCoreInterface> {
        self.gpu_core.as_deref_mut()
    }

    /// Set the content provider.
    /// Upstream: `system.SetContentProvider(make_unique<ContentProviderUnion>())`.
    pub fn set_content_provider(
        &mut self,
        provider: Arc<StdMutex<crate::file_sys::registered_cache::ContentProviderUnion>>,
    ) {
        self.content_provider = Some(provider);
    }

    /// Get the content provider.
    /// Upstream: `system.GetContentProvider()`.
    pub fn get_content_provider(
        &self,
    ) -> Option<&Arc<StdMutex<crate::file_sys::registered_cache::ContentProviderUnion>>> {
        self.content_provider.as_ref()
    }

    /// Get the shared ARP manager.
    /// Upstream: `System::GetARPManager()`.
    pub fn arp_manager(&self) -> Arc<StdMutex<ARPManager>> {
        self.arp_manager.clone()
    }

    /// Register the subsystem factory callback.
    /// The frontend calls this before load() to provide Host1x/GPU/AudioCore creation.
    /// setup_for_application_process() will call this during loading.
    pub fn set_subsystem_factory(&mut self, factory: Box<dyn FnOnce(&mut System) + Send>) {
        self.subsystem_factory = Some(factory);
    }

    /// Get the virtual filesystem.
    /// Upstream: `system.GetFilesystem()`.
    pub fn get_filesystem(&self) -> Option<&Arc<RealVfsFilesystem>> {
        self.virtual_filesystem.as_ref()
    }

    /// Set the virtual filesystem.
    /// Upstream: `system.SetFilesystem(make_shared<RealVfsFilesystem>())`.
    pub fn set_filesystem(&mut self, vfs: Arc<RealVfsFilesystem>) {
        self.virtual_filesystem = Some(vfs);
    }

    /// Clear the user channel.
    /// Upstream: `system.GetUserChannel().clear()`.
    pub fn clear_user_channel(&mut self) {
        self.user_channel.clear();
    }

    /// Set the AudioCore subsystem.
    /// Called by the frontend after System::load() since audio_core crate depends
    /// on core (circular dependency prevents core from creating AudioCore directly).
    /// Upstream: created inside SetupForApplicationProcess() (core.cpp:283).
    pub fn set_audio_core(&mut self, audio_core: Box<dyn AudioCoreInterface>) {
        self.audio_core = Some(audio_core);
    }

    /// Get the AudioCore subsystem bridge.
    /// Upstream: `System::AudioCore()` returns `AudioCore::AudioCore&`.
    pub fn audio_core(&self) -> Option<&dyn AudioCoreInterface> {
        self.audio_core.as_deref()
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

    /// Gets a shared reference to the Memory bridge, if initialized.
    /// Upstream: `System::Memory()`.
    /// Get the application process's Memory bridge.
    /// Upstream: `System::ApplicationMemory()` → `ApplicationProcess()->GetMemory()`.
    /// Falls back to the legacy `self.memory` if no application process is set.
    pub fn memory_shared(&self) -> Option<Arc<StdMutex<Memory>>> {
        // Try per-process Memory first (the correct upstream path)
        if let Some(ref process) = self.current_process {
            if let Some(mem) = process.get_memory() {
                return Some(mem);
            }
        }
        // Try via Arc<Mutex<KProcess>> current_process_arc
        if let Some(ref process_arc) = self.current_process_arc {
            if let Ok(process) = process_arc.lock() {
                if let Some(mem) = process.get_memory() {
                    return Some(mem);
                }
            }
        }
        // Fallback to legacy global Memory
        self.memory.clone()
    }

    /// Upstream: `System::GetGPUDirtyMemoryManager()`.
    pub fn gpu_dirty_memory_managers(&self) -> Vec<Arc<StdMutex<GpuDirtyMemoryManager>>> {
        self.gpu_dirty_memory_managers.clone()
    }

    /// Upstream: `System::GatherGPUDirtyMemory(std::function<void(PAddr, size_t)>&)`.
    pub fn gather_gpu_dirty_memory(&self, callback: &mut dyn FnMut(u64, usize)) {
        for manager in &self.gpu_dirty_memory_managers {
            manager.lock().unwrap().gather(callback);
        }
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

    /// Raw pointers to DeviceMemory and its HostMemory buffer, for creating
    /// per-process Memory instances.  Both pointers remain valid for the
    /// lifetime of System (they live in `self.device_memory`).
    pub fn device_memory_ptrs(
        &self,
    ) -> (*const DeviceMemory, *const common::host_memory::HostMemory) {
        let dm = self.device_memory();
        let dm_ptr = dm as *const DeviceMemory;
        let buffer_ptr = &dm.buffer as *const common::host_memory::HostMemory;
        (dm_ptr, buffer_ptr)
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
            Some(stats) => {
                stats.get_and_reset_stats(self.core_timing.lock().unwrap().get_global_time_us())
            }
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

    /// Returns a snapshot (clone) of the user channel for reading from a
    /// shared reference (e.g. from AppletManager::set_window_system).
    /// Upstream: `m_system.GetUserChannel()` used in SetWindowSystem for swap.
    pub fn get_user_channel_snapshot(&self) -> std::collections::VecDeque<Vec<u8>> {
        self.user_channel.clone()
    }

    /// Returns the current CoreTiming tick count.
    /// Upstream: `system.CoreTiming().GetClockTicks()`.
    pub fn get_core_timing_ticks(&self) -> u64 {
        self.core_timing.lock().unwrap().get_clock_ticks()
    }

    /// Registers the application main thread with the kernel and global scheduler.
    ///
    /// Called from AppletManager::set_window_system after process::Run() returns,
    /// matching the upstream flow where SetWindowSystem calls applet->process->Run()
    /// and the created thread is immediately visible to the scheduler.
    ///
    /// # Safety
    /// Must only be called during process setup, before CPU threads are released
    /// past the GPU barrier. This ensures exclusive access to kernel state.
    pub fn register_application_thread(
        &self,
        main_thread: Arc<StdMutex<crate::hle::kernel::k_thread::KThread>>,
    ) {
        let thread_id = main_thread.lock().unwrap().get_thread_id();
        log::info!(
            "register_application_thread: called, thread_id={}",
            thread_id
        );
        // Upstream: KProcess::Run() owns all scheduler registration for the
        // application thread (AddThread, PushBack, highest_priority_thread_id).
        // By the time we reach here the thread is already in the GSC thread list
        // and the priority queue (done in k_process::run() before thread.run()).
        // All that remains is:
        //   1. Record the application thread in the kernel for later use.
        //   2. Interrupt core 0 to wake it from PhysicalCore::idle() so that
        //      reschedule_current_core_raw can fiber-switch to the app thread.
        let sys_ptr = self as *const Self as *mut Self;
        if let Some(ref mut kernel) = unsafe { (*sys_ptr).kernel.as_mut() } {
            kernel.set_application_thread(main_thread);
            if let Some(core0) = kernel.physical_core(0) {
                log::info!("register_application_thread: interrupting core 0 to wake idle loop");
                core0.interrupt();
            }
        }
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
    pub fn register_core_thread(&mut self, id: usize) {
        if let Some(kernel) = self.kernel.as_ref() {
            kernel.register_core_thread(id);
        }
    }

    /// Register a host thread as an auxiliary thread.
    pub fn register_host_thread(&mut self) {
        if let Some(kernel) = self.kernel.as_ref() {
            kernel.register_host_thread();
        }
    }

    /// Get a reference to the kernel core.
    pub fn kernel(&self) -> Option<&KernelCore> {
        self.kernel.as_ref()
    }

    /// Runs a service server until shutdown.
    ///
    /// Port of upstream `System::RunServer`.
    pub fn run_server(&self, server_manager: ServerManager) {
        let Some(kernel) = self.kernel() else {
            log::warn!("System::run_server called before kernel initialization");
            return;
        };

        kernel.run_server(server_manager);
    }

    /// Get a mutable reference to the kernel core.
    pub fn kernel_mut(&mut self) -> Option<&mut KernelCore> {
        self.kernel.as_mut()
    }

    /// Get the shared HLE service registry.
    pub fn service_manager(&self) -> Option<Arc<std::sync::Mutex<ServiceManager>>> {
        self.service_manager.clone()
    }

    /// Get the shared APM controller.
    /// Upstream: `System::GetAPMController()`.
    pub fn apm_controller(&self) -> Arc<StdMutex<ApmController>> {
        self.apm_controller.clone()
    }

    pub fn get_applet_manager(&self) -> &AppletManager {
        &self.applet_manager
    }

    /// Get the filesystem controller.
    /// Upstream: `System::GetFileSystemController()`.
    pub fn get_filesystem_controller(
        &self,
    ) -> Arc<StdMutex<crate::hle::service::filesystem::filesystem::FileSystemController>> {
        self.filesystem_controller.clone()
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

    // ── Runtime SVC state setters ──

    /// Set the Arc-wrapped current process for SVC dispatch.
    pub fn set_current_process_arc(
        &mut self,
        p: Arc<StdMutex<crate::hle::kernel::k_process::KProcess>>,
    ) {
        self.current_process_arc = Some(p);
    }

    /// Set the cooperative scheduler for SVC dispatch.
    pub fn set_scheduler_arc(&mut self, s: Arc<StdMutex<KScheduler>>) {
        self.scheduler_arc = Some(s);
    }

    /// Set the shared process memory for SVC fallback paths.
    pub fn set_shared_process_memory(&mut self, m: SharedProcessMemory) {
        self.shared_process_memory = Some(m);
    }

    /// Set the runtime program/title ID.
    pub fn set_runtime_program_id(&mut self, id: u64) {
        self.runtime_program_id = id;
    }

    /// Set whether the loaded process is 64-bit.
    pub fn set_runtime_64bit(&mut self, is_64bit: bool) {
        self.runtime_is_64bit = is_64bit;
    }

    // ── SVC accessor methods ──
    //
    // These match the interface that upstream SVC handlers use when given
    // `Core::System&`. Each SVC handler calls system.Kernel(),
    // system.CurrentProcess(), etc.

    /// Get the current process Arc. Panics if not set.
    /// Upstream: `Kernel::GetCurrentProcess(kernel)`.
    pub fn current_process_arc(&self) -> &Arc<StdMutex<crate::hle::kernel::k_process::KProcess>> {
        self.current_process_arc
            .as_ref()
            .expect("current_process_arc not set; call set_current_process_arc() first")
    }

    /// Get the scheduler Arc for the current core.
    /// Upstream: `kernel.CurrentScheduler()`.
    pub fn scheduler_arc(&self) -> Arc<StdMutex<KScheduler>> {
        // Try per-core scheduler from kernel first (proper path).
        if let Some(ref kernel) = self.kernel {
            if let Some(sched) = kernel.current_scheduler() {
                return sched.clone();
            }
        }
        // Fallback to the System-level field (legacy path).
        self.scheduler_arc
            .as_ref()
            .expect("scheduler_arc not set and no kernel scheduler available")
            .clone()
    }

    /// Get the shared process memory. Panics if not set.
    pub fn shared_process_memory(&self) -> &SharedProcessMemory {
        self.shared_process_memory
            .as_ref()
            .expect("shared_process_memory not set")
    }

    /// Get the runtime program ID.
    pub fn runtime_program_id(&self) -> u64 {
        self.runtime_program_id
    }

    /// Whether the current process is 64-bit.
    pub fn runtime_is_64bit(&self) -> bool {
        self.runtime_is_64bit
    }

    /// Get the current thread's ID via the scheduler.
    /// Matches upstream `GetCurrentThread(kernel).GetId()`.
    pub fn current_thread_id(&self) -> Option<u64> {
        crate::hle::kernel::kernel::get_current_thread_pointer()
            .map(|t| t.lock().unwrap().get_thread_id())
    }

    /// Get the current thread Arc.
    /// Matches upstream `GetCurrentThread(kernel)`.
    pub fn current_thread(&self) -> Option<Arc<StdMutex<KThread>>> {
        crate::hle::kernel::kernel::get_current_thread_pointer()
    }

    /// Get the Memory bridge for guest memory access.
    /// Matches upstream `GetCurrentMemory(kernel)` which returns
    /// `GetCurrentProcess(kernel).GetMemory()`.
    /// Returns None when Memory is not wired (tests).
    pub fn get_svc_memory(&self) -> Option<Arc<StdMutex<Memory>>> {
        self.current_process_arc
            .as_ref()?
            .lock()
            .unwrap()
            .page_table
            .get_base()
            .m_memory
            .clone()
    }

    /// Create a minimal System for SVC handler unit tests.
    ///
    /// Sets up just enough state (kernel with ID counters, empty service
    /// manager) for SVC handlers to function. Callers must still set
    /// `current_process_arc`, `scheduler_arc`, etc.
    pub fn new_for_test() -> Self {
        let mut system = Self::new();
        // Kernel (provides object/thread ID allocation). Don't call
        // initialize() — that creates timers, physical cores, etc.
        let kernel = KernelCore::new();
        // Pre-advance ID counters past values that test setups manually assign
        // to pre-existing threads (typically object_id 1..2, thread_id 1..2).
        // This prevents collision when SVC handlers allocate new IDs.
        for _ in 0..10 {
            kernel.create_new_object_id();
            kernel.create_new_thread_id();
        }
        system.kernel = Some(kernel);
        let service_manager = Arc::new(StdMutex::new(ServiceManager::new()));
        // Register "sm:" port so connect_to_named_port tests work.
        let sys_ref = SystemRef::from_ref(&system);
        crate::hle::service::sm::sm::loop_process(&service_manager, sys_ref);
        system.service_manager = Some(service_manager);
        system
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

    /// Run the main JIT/SVC execution loop.
    ///
    /// Encapsulates JIT creation, thread bootstrap, SVC dispatch, and halt
    /// handling. Upstream, this logic lives inside CpuManager/PhysicalCore.
    ///
    /// Returns `(total_iterations, total_svc_count)`.
    pub fn run_main_loop(&mut self) -> (u32, u32) {
        use crate::arm::arm_interface::{
            ArmInterface, HaltReason, KProcess as OpaqueKProcess, KThread as OpaqueKThread,
            ThreadContext,
        };
        use crate::hle::kernel::physical_core::PhysicalCoreExecutionControl;

        let process = match self.current_process.take() {
            Some(p) => p,
            None => {
                log::warn!("No process loaded, skipping CPU execution");
                return (0, 0);
            }
        };

        let shared_memory = process.get_shared_memory();
        let is_64bit = process.is_64bit();
        let program_id = process.get_program_id();

        let load_parameters = self
            .load_parameters()
            .cloned()
            .expect("loader must provide process launch parameters");

        let process = Arc::new(StdMutex::new(process));
        process.lock().unwrap().bind_self_reference(&process);
        let scheduler = Arc::new(StdMutex::new(
            crate::hle::kernel::k_scheduler::KScheduler::new(0),
        ));
        process.lock().unwrap().attach_scheduler(&scheduler);
        let (main_thread, _main_thread_handle, _stack_base, _stack_top) = process
            .lock()
            .unwrap()
            .run(
                load_parameters.main_thread_priority,
                load_parameters.main_thread_stack_size as usize,
                1,
                1,
                is_64bit,
                None, // init_func — test/bootstrap path, no fiber needed
            )
            .expect("process runtime bootstrap must succeed");
        let tls_base = main_thread.lock().unwrap().get_tls_address().get();

        // Initialize ARM interfaces on the process (JIT + exclusive monitor).
        // Upstream: KProcess::InitializeInterfaces() (k_process.cpp:1263-1291).
        // Creates the exclusive monitor and one ARM JIT per core, owned by KProcess.
        {
            let core_timing = self.core_timing_shared();
            process
                .lock()
                .unwrap()
                .initialize_interfaces(shared_memory.clone(), core_timing);
        }

        // Get core 0's ARM interface from the process for the main execution loop.
        // Upstream: PhysicalCore gets this via process->GetArmInterface(core_index).
        let mut jit_holder = process.lock().unwrap().arm_interfaces[0]
            .take()
            .expect("ARM interface must exist after initialize_interfaces");
        let jit: &mut dyn ArmInterface = jit_holder.as_mut();

        // Wire the runtime SVC state on System so that SVC handlers
        // can access process, scheduler, memory, etc. via &System.
        self.set_current_process_arc(process.clone());
        self.set_scheduler_arc(scheduler.clone());
        self.set_shared_process_memory(shared_memory.clone());
        self.set_runtime_program_id(program_id);
        self.set_runtime_64bit(is_64bit);

        // Set initial context.
        // Maps to upstream ResetThreadContext32/64 in k_thread.cpp.
        let mut ctx = ThreadContext::default();
        let physical_core: *const crate::hle::kernel::physical_core::PhysicalCore = self
            .kernel()
            .and_then(|kernel| kernel.physical_core(0))
            .map(|core| core as *const _)
            .expect("kernel physical core 0 must exist after System::initialize");
        unsafe { &*physical_core }.initialize_guest_runtime(
            main_thread.clone(),
            &mut *jit,
            &mut ctx,
        );

        // Upstream: physical_core.cpp:158 — SetTpidrroEl0(GetInteger(thread->GetTlsAddress()))
        jit.set_tpidrro_el0(tls_base);

        // SVC dispatch loop.
        let dummy_thread = unsafe { &mut *(&mut 0u32 as *mut u32 as *mut OpaqueKThread) };

        let mut total_iteration = 0u32;
        let mut total_svc_count = 0u32;
        loop {
            let physical_core_ref = unsafe { &*physical_core };
            let (iteration, svc_count, exec_control) = physical_core_ref.run_loop(
                &mut *jit,
                dummy_thread,
                &mut ctx,
                &scheduler,
                &process,
                is_64bit,
                self,
                |_svc_num, _svc_args, _ctx, _svc_count, _iteration| {
                    PhysicalCoreExecutionControl::Continue
                },
                // Halt handling — maps to upstream PhysicalCore::RunThread()
                // halt-reason analysis (physical_core.cpp:55-100).
                |halt_reason, _exception_address, ctx, _svc_count, _iteration| {
                    if halt_reason.contains(HaltReason::STEP_THREAD) {
                        // Upstream: checked by debugger step state. No debugger
                        // yet, so just continue execution.
                        PhysicalCoreExecutionControl::Continue
                    } else if halt_reason.contains(HaltReason::PREFETCH_ABORT) {
                        log::error!(
                            "PREFETCH_ABORT at PC={:#x}, SP={:#x}, LR={:#x}",
                            ctx.pc, ctx.sp, ctx.lr
                        );
                        log::error!(
                            "  R0={:#x} R1={:#x} R2={:#x} R3={:#x} R4={:#x} R5={:#x} R6={:#x} R7={:#x} R8={:#x} R9={:#x} R10={:#x} R11={:#x} R12={:#x}",
                            ctx.r[0], ctx.r[1], ctx.r[2], ctx.r[3],
                            ctx.r[4], ctx.r[5], ctx.r[6], ctx.r[7],
                            ctx.r[8], ctx.r[9], ctx.r[10], ctx.r[11], ctx.r[12]
                        );
                        PhysicalCoreExecutionControl::Break
                    } else if halt_reason.contains(HaltReason::DATA_ABORT) {
                        // Upstream: notifies the debugger and suspends the thread.
                        log::error!(
                            "DATA_ABORT at PC={:#x}, SP={:#x}, LR={:#x}",
                            ctx.pc, ctx.sp, ctx.lr
                        );
                        PhysicalCoreExecutionControl::Break
                    } else if halt_reason.contains(HaltReason::BREAK_LOOP) {
                        // Upstream: return from RunThread, causes Fiber yield
                        // back to host thread → single-core preemption.
                        PhysicalCoreExecutionControl::Yield
                    } else {
                        log::warn!(
                            "JIT halted: {:?} at PC={:#x}, SP={:#x}",
                            halt_reason, ctx.pc, ctx.sp
                        );
                        PhysicalCoreExecutionControl::Break
                    }
                },
            );
            total_iteration += iteration;
            total_svc_count += svc_count;

            match exec_control {
                PhysicalCoreExecutionControl::Continue => continue,
                PhysicalCoreExecutionControl::Yield => {
                    // Upstream SingleCoreRunGuestThread: advance simulated
                    // time, then preempt to the next core.
                    let core_timing = self.core_timing_shared();
                    core_timing.lock().unwrap().advance();
                    self.get_cpu_manager_mut()
                        .preempt_single_core(&core_timing, true);
                    continue;
                }
                PhysicalCoreExecutionControl::Break => break,
            }
        }

        (total_iteration, total_svc_count)
    }
}

impl Default for System {
    fn default() -> Self {
        Self::new()
    }
}
