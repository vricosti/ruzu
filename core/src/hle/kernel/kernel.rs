//! Port of zuyu/src/core/hle/kernel/kernel.h/.cpp
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! KernelCore: the main kernel class, managing all kernel subsystems
//! including schedulers, physical cores, memory layout, slab heaps,
//! shared memory objects, and the global scheduler context.
//!
//! Full implementation requires KProcess, KThread, KScheduler,
//! KMemoryManager, KMemoryLayout, KHardwareTimer, KHandleTable,
//! KResourceLimit, KWorkerTaskManager, and many other subsystems.

use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, Weak};

use super::k_memory_manager::KMemoryManager;
use super::k_process::KProcess;
use super::k_scheduler::KScheduler;
use super::k_thread::KThread;
use super::super::service::server_manager::ServerManager;

use super::global_scheduler_context::GlobalSchedulerContext;
use super::init::init_slab_setup::KSlabResourceCounts;
use super::k_auto_object_container::KAutoObjectWithListContainer;
use super::k_hardware_timer::KHardwareTimer;
use super::k_object_name::KObjectNameGlobalData;
use super::k_thread::SuspendType;
use super::k_worker_task_manager::KWorkerTaskManager;
use super::physical_core::PhysicalCore;
use crate::core_timing::CoreTiming;
use crate::hardware_properties;

// Thread-local host thread ID.
// Upstream: `static inline thread_local u8 host_thread_id = UINT8_MAX` in KernelCore::Impl.
// Core threads get IDs 0..NUM_CPU_CORES-1. Other host threads get IDs >= NUM_CPU_CORES.
// UINT8_MAX (255) means "not yet registered".
std::thread_local! {
    static HOST_THREAD_ID: std::cell::Cell<u32> = const { std::cell::Cell::new(u32::MAX) };
}

// Global kernel pointer for scheduler callbacks.
// Set during kernel initialization, cleared on shutdown.
// The callbacks (fn pointers) need access to GSC + schedulers but can't capture state.
static KERNEL_PTR: std::sync::atomic::AtomicPtr<KernelCore> =
    std::sync::atomic::AtomicPtr::new(std::ptr::null_mut());

/// Real scheduler callbacks that access the kernel via KERNEL_PTR.
/// Wired to the scheduler lock during kernel initialization.
static SCHEDULER_CALLBACKS: super::k_scheduler_lock::SchedulerCallbacks =
    super::k_scheduler_lock::SchedulerCallbacks {
        disable_scheduling: real_disable_scheduling,
        enable_scheduling: real_enable_scheduling,
        update_highest_priority_threads: real_update_highest_priority_threads,
    };

fn real_disable_scheduling() {
    if let Some(thread) = get_current_thread_pointer() {
        let mut t = thread.lock().unwrap();
        debug_assert!(t.get_disable_dispatch_count() >= 0);
        t.disable_dispatch();
    }
}

fn real_enable_scheduling(cores_needing_scheduling: u64) {
    if let Some(thread) = get_current_thread_pointer() {
        let mut t = thread.lock().unwrap();
        debug_assert!(t.get_disable_dispatch_count() >= 1);
        if t.get_disable_dispatch_count() > 1 {
            t.enable_dispatch();
        } else {
            t.enable_dispatch();
            // Upstream: RescheduleCurrentCore if cores_needing_scheduling has our core.
            // For now, trigger schedule on all cores that need it.
            if cores_needing_scheduling != 0 {
                drop(t); // release thread lock before touching schedulers
                let kernel_ptr = KERNEL_PTR.load(Ordering::Acquire);
                if !kernel_ptr.is_null() {
                    let kernel = unsafe { &*kernel_ptr };
                    for core_id in 0..hardware_properties::NUM_CPU_CORES {
                        if cores_needing_scheduling & (1u64 << core_id) != 0 {
                            if let Some(sched) = kernel.scheduler(core_id as usize) {
                                let s = sched.lock().unwrap();
                                s.state.needs_scheduling.store(true, Ordering::Relaxed);
                            }
                        }
                    }
                }
            }
        }
    }
}

fn real_update_highest_priority_threads() -> u64 {
    use super::k_scheduler::KScheduler;

    let kernel_ptr = KERNEL_PTR.load(Ordering::Acquire);
    if kernel_ptr.is_null() {
        return 0;
    }
    let kernel = unsafe { &*kernel_ptr };

    let gsc_arc = match kernel.global_scheduler_context() {
        Some(gsc) => gsc.clone(),
        None => return 0,
    };

    // Collect scheduler arcs before locking GSC (lock order: GSC before schedulers).
    let sched_arcs: Vec<_> = (0..hardware_properties::NUM_CPU_CORES as usize)
        .filter_map(|i| kernel.scheduler(i).cloned())
        .collect();

    let migrations;
    let cores_needing_scheduling;
    {
        let mut gsc = gsc_arc.lock().unwrap();

        if !gsc.m_scheduler_update_needed.load(Ordering::Relaxed) {
            return 0;
        }

        let mut sched_guards: Vec<_> = sched_arcs.iter().map(|s| s.lock().unwrap()).collect();

        // Delegate to full implementation with idle core migration.
        let result = KScheduler::update_highest_priority_threads_impl(
            &mut sched_guards, &mut gsc,
        );
        cores_needing_scheduling = result.0;
        migrations = result.1;
        // GSC lock released here.
    }

    // Apply deferred migration core_id updates (outside GSC lock to avoid deadlock).
    // Upstream: SetActiveCore is called inside UpdateHighestPriorityThreadsImpl under
    // the scheduler lock. We defer it because our lock ordering is thread→GSC.
    if !migrations.is_empty() {
        let gsc = gsc_arc.lock().unwrap();
        for (thread_id, new_core) in migrations {
            if let Some(thread) = gsc.get_thread_by_thread_id(thread_id) {
                thread.lock().unwrap().core_id = new_core;
            }
        }
    }

    cores_needing_scheduling
}

// Thread-local current thread pointer.
// Upstream: `static inline thread_local KThread* current_thread{nullptr}` in KernelCore::Impl.
// Each physical core host thread (and any other host thread) stores its own current KThread.
std::thread_local! {
    static CURRENT_THREAD: RefCell<Option<Weak<Mutex<KThread>>>> = RefCell::new(None);
}

/// Get the current emulation thread for the calling host thread.
/// Upstream: `KernelCore::Impl::GetCurrentEmuThread()`.
pub fn get_current_emu_thread() -> Option<Arc<Mutex<KThread>>> {
    CURRENT_THREAD.with(|cell| {
        cell.borrow().as_ref().and_then(Weak::upgrade)
    })
}

/// Set the current emulation thread for the calling host thread.
/// Upstream: `KernelCore::Impl::SetCurrentEmuThread(KThread*)`.
pub fn set_current_emu_thread(thread: Option<&Arc<Mutex<KThread>>>) {
    CURRENT_THREAD.with(|cell| {
        *cell.borrow_mut() = thread.map(Arc::downgrade);
    });
}

/// Get the current thread pointer for the calling host thread.
/// Upstream: `GetCurrentThreadPointer(kernel)`.
/// Returns None if no thread is set.
pub fn get_current_thread_pointer() -> Option<Arc<Mutex<KThread>>> {
    get_current_emu_thread()
}

/// Get the current hardware timer tick for the active kernel instance.
/// Returns `None` when no kernel or hardware timer is initialized.
pub fn get_current_hardware_tick() -> Option<i64> {
    let kernel_ptr = KERNEL_PTR.load(Ordering::Acquire);
    if kernel_ptr.is_null() {
        return None;
    }

    let kernel = unsafe { &*kernel_ptr };
    kernel
        .hardware_timer()
        .map(|timer| timer.lock().unwrap().get_tick())
}

/// Constants from the upstream KernelCore::Impl.
const APPLICATION_MEMORY_BLOCK_SLAB_HEAP_SIZE: usize = 20000;
const SYSTEM_MEMORY_BLOCK_SLAB_HEAP_SIZE: usize = 10000;
const BLOCK_INFO_SLAB_HEAP_SIZE: usize = 4000;
const RESERVED_DYNAMIC_PAGE_COUNT: usize = 64;

/// Represents a single instance of the kernel.
///
/// Maps to upstream KernelCore and its inner Impl struct.
pub struct KernelCore {
    // -- Initialization state --
    is_multicore: bool,
    is_shutting_down: AtomicBool,
    is_phantom_mode_for_singlecore: AtomicBool,
    exception_exited: bool,

    // -- ID counters --
    next_object_id: AtomicU32,
    next_kernel_process_id: AtomicU64,
    next_user_process_id: AtomicU64,
    next_thread_id: AtomicU64,

    // -- Subsystems --
    hardware_timer: Option<Arc<Mutex<KHardwareTimer>>>,
    global_object_list_container: Option<KAutoObjectWithListContainer>,
    global_scheduler_context: Option<Arc<Mutex<GlobalSchedulerContext>>>,
    object_name_global_data: Option<KObjectNameGlobalData>,

    // -- Physical cores and schedulers --
    /// Per-core KScheduler instances.
    /// Upstream: `std::array<std::unique_ptr<Kernel::KScheduler>, NUM_CPU_CORES> schedulers`.
    schedulers: Vec<Arc<Mutex<KScheduler>>>,
    /// Per-core PhysicalCore instances.
    /// Upstream: `std::array<std::unique_ptr<Kernel::PhysicalCore>, NUM_CPU_CORES> cores`.
    cores: Vec<PhysicalCore>,

    /// Per-core main threads.
    /// Upstream: created in `InitializePhysicalCores()` via `KThread::InitializeMainThread`.
    main_threads: Vec<Arc<Mutex<KThread>>>,
    /// Per-core idle threads.
    /// Upstream: created in `InitializePhysicalCores()` via `KThread::InitializeIdleThread`.
    idle_threads: Vec<Arc<Mutex<KThread>>>,

    /// The application's main thread (created by KProcess::run).
    /// Used to set as the current thread when entering guest dispatch.
    application_thread: Option<Arc<Mutex<KThread>>>,

    // -- Slab resource counts --
    slab_resource_counts: KSlabResourceCounts,

    // -- Process tracking --
    // application_process: Option<*mut KProcess>,
    // process_list: Vec<*mut KProcess>,
    process_list_lock: Mutex<()>,

    // -- Registered objects for leak tracking --
    registered_objects: Mutex<Vec<u64>>,
    registered_in_use_objects: Mutex<Vec<u64>>,

    // -- Host thread management --
    next_host_thread_id: AtomicU32,
    /// In single-core mode, the host thread ID of the single core thread.
    /// Upstream: `u32 single_core_thread_id{}` in Impl.
    single_core_thread_id: AtomicU32,

    // -- Memory management --
    /// Physical memory manager. Upstream: `Impl::memory_manager`.
    memory_manager: KMemoryManager,

    // -- Core timing --
    /// Reference to the system's CoreTiming.
    /// Upstream: accessed via `system.CoreTiming()` through `System& system` reference.
    /// Stored here so fiber closures (guest_activate, idle thread) can access it
    /// without needing a System reference.
    core_timing: Option<Arc<Mutex<CoreTiming>>>,

    /// Reference to the owning System.
    /// Upstream: `Core::System& system` stored in KernelCore::Impl.
    /// Used by SVC dispatch (`Svc::Call(system, svc_number)`) and other
    /// kernel operations that need access to System-level state.
    system_ref: crate::core::SystemRef,

    /// Preemption timer event (10ms interval).
    /// Upstream: `std::shared_ptr<Core::Timing::EventType> preemption_event`.
    preemption_event: Option<Arc<parking_lot::Mutex<crate::core_timing::EventType>>>,

    /// Active service server managers.
    /// Upstream: `Impl::server_managers`.
    server_managers: Mutex<Vec<Arc<Mutex<ServerManager>>>>,

    /// Guest service processes created by `RunOnGuestCoreProcess`.
    /// Upstream keeps them alive after `KProcess::Register(*this, process)`.
    service_processes: Mutex<Vec<Arc<Mutex<KProcess>>>>,
}

// KProcess initial ID constants (matching upstream).
const INITIAL_PROCESS_ID_MIN: u64 = 1;
const PROCESS_ID_MIN: u64 = 81;

impl KernelCore {
    /// Construct a new kernel instance.
    pub fn new() -> Self {
        Self {
            is_multicore: true,
            is_shutting_down: AtomicBool::new(false),
            is_phantom_mode_for_singlecore: AtomicBool::new(false),
            exception_exited: false,

            next_object_id: AtomicU32::new(0),
            next_kernel_process_id: AtomicU64::new(INITIAL_PROCESS_ID_MIN),
            next_user_process_id: AtomicU64::new(PROCESS_ID_MIN),
            next_thread_id: AtomicU64::new(1),

            hardware_timer: None,
            global_object_list_container: None,
            global_scheduler_context: None,
            object_name_global_data: None,

            schedulers: Vec::new(),
            cores: Vec::new(),

            main_threads: Vec::new(),
            idle_threads: Vec::new(),
            application_thread: None,

            slab_resource_counts: KSlabResourceCounts::create_default(),

            process_list_lock: Mutex::new(()),
            registered_objects: Mutex::new(Vec::new()),
            registered_in_use_objects: Mutex::new(Vec::new()),

            memory_manager: KMemoryManager::new(),
            next_host_thread_id: AtomicU32::new(hardware_properties::NUM_CPU_CORES),
            single_core_thread_id: AtomicU32::new(0),
            core_timing: None,
            system_ref: crate::core::SystemRef::null(),
            preemption_event: None,
            server_managers: Mutex::new(Vec::new()),
            service_processes: Mutex::new(Vec::new()),
        }
    }

    /// Set whether emulation is multicore or single core.
    /// Must be called before Initialize.
    pub fn set_multicore(&mut self, is_multicore: bool) {
        self.is_multicore = is_multicore;
    }

    /// Initialize the kernel.
    pub fn initialize(&mut self) {
        self.hardware_timer = Some(Arc::new(Mutex::new(KHardwareTimer::new())));

        self.global_object_list_container = Some(KAutoObjectWithListContainer::new());
        self.global_scheduler_context = Some(Arc::new(Mutex::new(GlobalSchedulerContext::new())));

        self.is_phantom_mode_for_singlecore.store(false, Ordering::Relaxed);

        // Initialize slab resource counts.
        super::init::init_slab_setup::initialize_slab_resource_counts(
            &mut self.slab_resource_counts,
        );

        // Initialize physical cores.
        self.initialize_physical_cores();

        // Initialize global data.
        self.object_name_global_data = Some(KObjectNameGlobalData::new());

        // Wire up scheduler lock callbacks.
        // The callbacks need kernel access but are plain fn pointers.
        // Store a raw pointer to self in a static for the callbacks to use.
        KERNEL_PTR.store(self as *mut KernelCore, Ordering::Release);
        if let Some(ref gsc) = self.global_scheduler_context {
            gsc.lock().unwrap().m_scheduler_lock.set_callbacks(&SCHEDULER_CALLBACKS);
        }

        // Initialize preemption event.
        // Upstream: InitializePreemption schedules a looping CoreTiming event every 10ms
        // that calls PreemptThreads + Interrupt on each core, forcing the JIT out of
        // tight loops so the scheduler can reschedule threads.
        // Upstream: InitializePreemption creates a looping CoreTiming event.
        // The callback takes KScopedSchedulerLock then calls PreemptThreads.
        // Additionally, we interrupt all cores to force the JIT to exit
        // linked block loops and check is_interrupted in the outer loop.
        self.preemption_event = Some(crate::core_timing::create_event(
            "PreemptionCallback".to_string(),
            Box::new(move |_time, _ns| {
                let kernel_ptr = KERNEL_PTR.load(Ordering::Acquire);
                if kernel_ptr.is_null() {
                    return None;
                }
                let kernel = unsafe { &*kernel_ptr };

                // PreemptThreads (rotate priority queue).
                if let Some(gsc_arc) = kernel.global_scheduler_context() {
                    let mut gsc = gsc_arc.lock().unwrap();
                    gsc.preempt_threads();
                    drop(gsc);
                }

                // Interrupt all cores.
                for core_id in 0..hardware_properties::NUM_CPU_CORES as usize {
                    if let Some(core) = kernel.physical_core(core_id) {
                        core.interrupt();
                    }
                }

                None
            }),
        ));
    }

    /// Run a service function on a guest core as a KThread with fiber context.
    ///
    /// Port of upstream `KernelCore::RunOnGuestCoreProcess` (kernel.cpp:1105-1139).
    /// Creates a new KProcess and KThread, initializes the thread as a service
    /// thread (HighPriority, core 3, priority 16), and makes it schedulable.
    /// The scheduler will pick it up and run the fiber on guest core 3.
    pub fn run_on_guest_core_process(&self, name: &str, func: Box<dyn FnOnce() + Send>) {
        const SERVICE_THREAD_PRIORITY: i32 = 16;
        const SERVICE_THREAD_CORE: i32 = 3;

        // Create a service process for tracking.
        let process = Arc::new(Mutex::new(super::k_process::KProcess::new()));
        // Minimal init — service processes don't need page tables or user memory.
        self.service_processes
            .lock()
            .unwrap()
            .push(Arc::clone(&process));

        // Create the service thread.
        let thread = Arc::new(Mutex::new(super::k_thread::KThread::new()));
        let (thread_id, object_id) = {
            static NEXT_SERVICE_THREAD_ID: std::sync::atomic::AtomicU64 =
                std::sync::atomic::AtomicU64::new(0x8000_0000);
            let id = NEXT_SERVICE_THREAD_ID.fetch_add(1, Ordering::Relaxed);
            (id, id)
        };

        // Give the process a scheduler reference for the target core.
        // Upstream: service threads run on core 3, so use core 3's scheduler.
        if let Some(scheduler) = self.scheduler(SERVICE_THREAD_CORE as usize) {
            process.lock().unwrap().scheduler = Some(Arc::downgrade(scheduler));
        }
        // Wire GSC so the thread's notify_state_transition can update PQ directly.
        if let Some(gsc) = self.global_scheduler_context() {
            process.lock().unwrap().global_scheduler_context = Some(gsc.clone());
        }

        {
            let mut t = thread.lock().unwrap();
            t.initialize_service_thread(
                self.system_ref,
                &thread,
                func,
                SERVICE_THREAD_PRIORITY,
                SERVICE_THREAD_CORE,
                &process,
                thread_id,
                object_id,
            );
        }

        // Upstream registers the thread before making it runnable.
        process.lock().unwrap().register_thread_object(Arc::clone(&thread));
        self.register_kernel_object(thread.lock().unwrap().get_object_id());

        // Make the thread runnable. t.run() → set_state(RUNNABLE) →
        // notify_state_transition pushes to PQ via the GSC reference
        // (wired during initialize_service_thread from the process).
        // Must be called OUTSIDE GSC lock scope to avoid deadlock.
        thread.lock().unwrap().run();

        // Request reschedule so the scheduler picks up the new thread.
        // Upstream: SetSchedulerUpdateNeeded + KScopedSchedulerLock release triggers reschedule.
        // Request reschedule on ALL cores (upstream does this via SetSchedulerUpdateNeeded
        // which marks a global flag checked by all cores).
        for core_id in 0..crate::hardware_properties::NUM_CPU_CORES as usize {
            if let Some(scheduler) = self.scheduler(core_id) {
                scheduler.lock().unwrap().request_schedule();
            }
        }
        // Verify the thread is in the priority queue.
        if let Some(gsc) = self.global_scheduler_context() {
            let gsc = gsc.lock().unwrap();
            let front = gsc.m_priority_queue.get_scheduled_front(SERVICE_THREAD_CORE);
            log::info!(
                "KernelCore::run_on_guest_core_process: '{}' thread_id={} core={} priority={} pq_front={:?}",
                name, thread_id, SERVICE_THREAD_CORE, SERVICE_THREAD_PRIORITY, front
            );
        }
    }

    /// Run a service function on a host thread with a dummy KThread for tracking.
    ///
    /// Port of upstream `KernelCore::RunOnHostCoreProcess` (kernel.cpp:1077-1094).
    /// Spawns an OS thread that runs the function. Used for CPU-intensive services
    /// (audio, filesystem, etc.) that benefit from running on host hardware.
    pub fn run_on_host_core_process(&self, name: &str, func: Box<dyn FnOnce() + Send>) {
        let thread_name = format!("HLE:{}", name);
        std::thread::Builder::new()
            .name(thread_name.clone())
            .spawn(move || {
                log::info!("Host service thread '{}' started", thread_name);
                func();
                log::info!("Host service thread '{}' exited", thread_name);
            })
            .expect("Failed to spawn host service thread");
    }

    /// Wire the hardware timer to CoreTiming.
    /// Must be called after initialize() when System has CoreTiming available.
    pub fn wire_hardware_timer(&self, core_timing: Arc<std::sync::Mutex<CoreTiming>>) {
        if let Some(ref timer) = self.hardware_timer {
            KHardwareTimer::wire_callback(timer, core_timing);
        }
    }

    /// Store a reference to CoreTiming so that fiber closures (guest_activate,
    /// idle thread) can access it without needing a System reference.
    /// Must be called after System creates CoreTiming and before CPU threads start.
    pub fn set_core_timing(&mut self, core_timing: Arc<Mutex<CoreTiming>>) {
        self.core_timing = Some(core_timing);
    }

    /// Get the CoreTiming reference.
    /// Upstream: accessed via `system.CoreTiming()`.
    pub fn core_timing(&self) -> Option<&Arc<Mutex<CoreTiming>>> {
        self.core_timing.as_ref()
    }

    /// Schedule the preemption timer event (10ms interval).
    /// Matches upstream `InitializePreemption(kernel)` in kernel.cpp.
    /// Must be called after set_core_timing().
    pub fn schedule_preemption_event(&self, core_timing: &Arc<std::sync::Mutex<CoreTiming>>) {
        if let Some(ref event) = self.preemption_event {
            let interval = std::time::Duration::from_millis(10);
            core_timing.lock().unwrap().schedule_looping_event(
                interval, interval, event, false,
            );
            log::info!("KernelCore: preemption event scheduled (10ms interval)");
        }
    }

    /// Set the System reference.
    /// Upstream: `KernelCore(System& system)` stores it at construction.
    pub fn set_system_ref(&mut self, system_ref: crate::core::SystemRef) {
        self.system_ref = system_ref;
    }

    /// Get the System reference.
    /// Upstream: `KernelCore::System()`.
    pub fn system(&self) -> crate::core::SystemRef {
        self.system_ref
    }

    /// Set the application's main thread.
    pub fn set_application_thread(&mut self, thread: Arc<Mutex<KThread>>) {
        self.application_thread = Some(thread);
    }

    /// Get the application's main thread.
    pub fn get_application_thread(&self) -> Option<Arc<Mutex<KThread>>> {
        self.application_thread.clone()
    }

    /// Shutdown the kernel.
    pub fn shutdown(&mut self) {
        self.is_shutting_down.store(true, Ordering::Relaxed);

        self.close_services();

        self.next_object_id.store(0, Ordering::Relaxed);
        self.next_kernel_process_id
            .store(INITIAL_PROCESS_ID_MIN, Ordering::Relaxed);
        self.next_user_process_id
            .store(PROCESS_ID_MIN, Ordering::Relaxed);
        self.next_thread_id.store(1, Ordering::Relaxed);

        // Clean up registered objects.
        {
            let mut in_use = self.registered_in_use_objects.lock().unwrap();
            in_use.clear();
        }
        {
            let mut registered = self.registered_objects.lock().unwrap();
            if !registered.is_empty() {
                log::debug!(
                    "{} kernel objects were dangling on shutdown!",
                    registered.len()
                );
                registered.clear();
            }
        }

        self.object_name_global_data = None;

        // Upstream: schedulers[core_id].reset() per core.
        self.schedulers.clear();
        self.main_threads.clear();
        self.idle_threads.clear();
        self.service_processes.lock().unwrap().clear();

        if let Some(ref container) = self.global_object_list_container {
            container.finalize();
        }
        self.global_object_list_container = None;

        if let Some(ref timer) = self.hardware_timer {
            timer.lock().unwrap().finalize();
        }
        self.hardware_timer = None;

        self.is_shutting_down.store(false, Ordering::Relaxed);
    }

    /// Close all active services.
    /// Upstream iterates server_managers and closes each.
    pub fn close_services(&self) {
        let server_managers = {
            let mut managers = self.server_managers.lock().unwrap();
            std::mem::take(&mut *managers)
        };

        for manager in server_managers {
            manager.lock().unwrap().request_stop();
        }

    }

    /// Port of upstream `KernelCore::RunServer`.
    pub fn run_server(&self, server_manager: ServerManager) {
        let manager = Arc::new(Mutex::new(server_manager));

        {
            let mut managers = self.server_managers.lock().unwrap();
            if self.is_shutting_down.load(Ordering::Relaxed) {
                return;
            }
            managers.push(Arc::clone(&manager));
        }

        manager.lock().unwrap().loop_process();
    }

    #[cfg(test)]
    fn track_server_manager_for_test(&self, server_manager: Arc<Mutex<ServerManager>>) {
        self.server_managers.lock().unwrap().push(server_manager);
    }

    /// Suspend or resume emulation threads for the current application process.
    ///
    /// Upstream: `KernelCore::SuspendEmulation(bool)`.
    /// This port currently tracks only the frontend-loaded application process.
    pub fn suspend_emulation(&self, suspended: bool) {
        let should_suspend = self.exception_exited || suspended;
        let Some(process) = self.system_ref.get().current_process_arc.as_ref().cloned() else {
            return;
        };

        let threads: Vec<Arc<Mutex<KThread>>> = {
            let process_guard = process.lock().unwrap();
            process_guard.thread_objects.values().cloned().collect()
        };

        for thread in threads {
            let mut thread_guard = thread.lock().unwrap();
            if should_suspend {
                thread_guard.request_suspend(SuspendType::System);
            } else {
                thread_guard.resume(SuspendType::System);
            }
        }

        if should_suspend {
            self.interrupt_all_cores();
        }
    }

    /// Begin kernel-side shutdown for the current application process.
    ///
    /// Upstream: `KernelCore::ShutdownCores()`.
    /// The current Rust port uses process termination plus per-core interrupts
    /// to drive CpuManager guest fibers into their shutdown yield path.
    pub fn shutdown_cores(&self) {
        if let Some(process) = self.system_ref.get().current_process_arc.as_ref().cloned() {
            let _ = process.lock().unwrap().terminate();
            KWorkerTaskManager::wait_for_global_idle();
        }

        self.interrupt_all_cores();
    }

    /// Get the global scheduler context (Arc reference).
    pub fn global_scheduler_context(&self) -> Option<&Arc<Mutex<GlobalSchedulerContext>>> {
        self.global_scheduler_context.as_ref()
    }

    /// Get a physical core by index.
    /// Upstream: `KernelCore::PhysicalCore(id)`.
    pub fn physical_core(&self, id: usize) -> Option<&PhysicalCore> {
        self.cores.get(id)
    }

    /// Get a physical core mutably by index.
    pub fn physical_core_mut(&mut self, id: usize) -> Option<&mut PhysicalCore> {
        self.cores.get_mut(id)
    }

    /// Get a per-core scheduler by index.
    /// Upstream: `KernelCore::Scheduler(id)` (kernel.cpp:924).
    pub fn scheduler(&self, id: usize) -> Option<&Arc<Mutex<KScheduler>>> {
        self.schedulers.get(id)
    }

    fn interrupt_all_cores(&self) {
        for core_id in 0..self.cores.len() {
            if let Some(core) = self.physical_core(core_id) {
                core.interrupt();
            }
        }
    }

    /// Get the scheduler for the calling host thread's core.
    /// Upstream: `KernelCore::CurrentScheduler()` (kernel.cpp:956-963).
    /// Returns None if called from a non-core thread.
    pub fn current_scheduler(&self) -> Option<&Arc<Mutex<KScheduler>>> {
        let core_id = self.get_current_host_thread_id();
        if core_id >= hardware_properties::NUM_CPU_CORES {
            return None;
        }
        self.schedulers.get(core_id as usize)
    }

    /// Get the physical core index for the calling host thread.
    /// Upstream: `KernelCore::CurrentPhysicalCoreIndex()` (kernel.cpp:940-946).
    pub fn current_physical_core_index(&self) -> usize {
        let core_id = self.get_current_host_thread_id();
        if core_id >= hardware_properties::NUM_CPU_CORES {
            return (hardware_properties::NUM_CPU_CORES - 1) as usize;
        }
        core_id as usize
    }

    /// Get the physical core for the calling host thread.
    /// Upstream: `KernelCore::CurrentPhysicalCore()` (kernel.cpp:948).
    pub fn current_physical_core(&self) -> &PhysicalCore {
        &self.cores[self.current_physical_core_index()]
    }

    /// Get the physical core for the calling host thread (mutable).
    pub fn current_physical_core_mut(&mut self) -> &mut PhysicalCore {
        let idx = self.current_physical_core_index();
        &mut self.cores[idx]
    }

    /// Register a CPU core thread by setting the thread-local host thread ID.
    /// Upstream: `KernelCore::RegisterCoreThread(core_id)` (kernel.cpp:1032).
    /// Must be called from the host thread that will run this core.
    pub fn register_core_thread(&self, core_id: usize) {
        assert!(core_id < hardware_properties::NUM_CPU_CORES as usize);
        let this_id = core_id as u32;
        HOST_THREAD_ID.with(|id| {
            assert_eq!(id.get(), u32::MAX, "host thread already registered");
            id.set(this_id);
        });
        if !self.is_multicore {
            self.single_core_thread_id.store(this_id, Ordering::Relaxed);
        }
    }

    /// Register a host thread (non-core) by allocating the next host thread ID.
    /// Upstream: `KernelCore::RegisterHostThread(existing_thread)` (kernel.cpp:1036).
    pub fn register_host_thread(&self) {
        HOST_THREAD_ID.with(|id| {
            if id.get() == u32::MAX {
                let new_id = self.next_host_thread_id.fetch_add(1, Ordering::Relaxed);
                id.set(new_id);
            }
        });
    }

    /// Get the host thread ID for the calling thread.
    /// Upstream: `Impl::GetCurrentHostThreadID()` (kernel.cpp:403-409).
    /// In single-core mode, if the calling thread is the single core thread,
    /// returns the current core index from CpuManager instead of the raw ID.
    fn get_current_host_thread_id(&self) -> u32 {
        HOST_THREAD_ID.with(|id| {
            let this_id = id.get();
            if !self.is_multicore
                && this_id == self.single_core_thread_id.load(Ordering::Relaxed)
            {
                // In single-core mode, the single core thread ID maps to the
                // current core index. Upstream reads system.GetCpuManager().CurrentCore().
                // We return 0 as default; CpuManager.current_core rotates this.
                // Upstream: system.GetCpuManager().CurrentCore(). Defaults to 0 until wired.
                return 0;
            }
            this_id
        })
    }

    /// Get the hardware timer (Arc reference).
    pub fn hardware_timer(&self) -> Option<&Arc<Mutex<KHardwareTimer>>> {
        self.hardware_timer.as_ref()
    }

    /// Get the object list container.
    pub fn object_list_container(&self) -> Option<&KAutoObjectWithListContainer> {
        self.global_object_list_container.as_ref()
    }

    /// Get the object name global data.
    pub fn object_name_global_data(&self) -> Option<&KObjectNameGlobalData> {
        self.object_name_global_data.as_ref()
    }

    /// Get the current emulation thread for the calling host thread.
    /// Matches upstream `KernelCore::GetCurrentEmuThread()`.
    /// Delegates to the thread-local `get_current_emu_thread()` free function.
    pub fn get_current_emu_thread(&self) -> Option<Arc<Mutex<KThread>>> {
        get_current_emu_thread()
    }

    /// Set the current emulation thread for the calling host thread.
    /// Matches upstream `KernelCore::SetCurrentEmuThread(KThread*)`.
    /// Delegates to the thread-local `set_current_emu_thread()` free function.
    pub fn set_current_emu_thread(&self, thread: Option<&Arc<Mutex<KThread>>>) {
        set_current_emu_thread(thread);
    }

    /// Register a kernel object for leak tracking.
    pub fn register_kernel_object(&self, object_id: u64) {
        self.registered_objects.lock().unwrap().push(object_id);
    }

    /// Unregister a kernel object from leak tracking.
    pub fn unregister_kernel_object(&self, object_id: u64) {
        self.registered_objects
            .lock()
            .unwrap()
            .retain(|&id| id != object_id);
    }

    /// Register a kernel object as in-use.
    pub fn register_in_use_object(&self, object_id: u64) {
        self.registered_in_use_objects
            .lock()
            .unwrap()
            .push(object_id);
    }

    /// Unregister an in-use kernel object.
    pub fn unregister_in_use_object(&self, object_id: u64) {
        self.registered_in_use_objects
            .lock()
            .unwrap()
            .retain(|&id| id != object_id);
    }

    /// Whether the kernel is in multicore mode.
    pub fn is_multicore(&self) -> bool {
        self.is_multicore
    }

    /// Whether the kernel is shutting down.
    pub fn is_shutting_down(&self) -> bool {
        self.is_shutting_down.load(Ordering::Relaxed)
    }

    /// Workaround for single-core mode phantom mode.
    pub fn is_phantom_mode_for_single_core(&self) -> bool {
        self.is_phantom_mode_for_singlecore.load(Ordering::Relaxed)
    }

    /// Set the phantom mode for single core.
    pub fn set_is_phantom_mode_for_single_core(&self, value: bool) {
        self.is_phantom_mode_for_singlecore
            .store(value, Ordering::Relaxed);
    }

    /// Get the slab resource counts.
    pub fn slab_resource_counts(&self) -> &KSlabResourceCounts {
        &self.slab_resource_counts
    }

    /// Get the slab resource counts (mutable).
    pub fn slab_resource_counts_mut(&mut self) -> &mut KSlabResourceCounts {
        &mut self.slab_resource_counts
    }

    // -- Private methods --

    /// Create a new object ID.
    pub fn create_new_object_id(&self) -> u32 {
        self.next_object_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Create a new kernel process ID.
    pub fn create_new_kernel_process_id(&self) -> u64 {
        self.next_kernel_process_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Create a new user process ID.
    pub fn create_new_user_process_id(&self) -> u64 {
        self.next_user_process_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Create a new thread ID.
    pub fn create_new_thread_id(&self) -> u64 {
        self.next_thread_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Get the memory manager.
    /// Upstream: `KernelCore::MemoryManager()`.
    pub fn memory_manager(&self) -> &KMemoryManager {
        &self.memory_manager
    }

    /// Get the memory manager (mutable).
    pub fn memory_manager_mut(&mut self) -> &mut KMemoryManager {
        &mut self.memory_manager
    }

    /// Initialize physical cores and per-core schedulers.
    ///
    /// Upstream: `Impl::InitializePhysicalCores()` (kernel.cpp:192-211).
    /// Creates KScheduler + PhysicalCore for each core, then initializes
    /// each scheduler with main/idle threads.
    /// Initialize physical cores and per-core schedulers.
    ///
    /// Upstream: `Impl::InitializePhysicalCores()` (kernel.cpp:192-211).
    /// For each core, creates a KScheduler and PhysicalCore, then creates
    /// main and idle threads (ownerless, `ThreadType::Main`) and initializes
    /// the scheduler with them so that `get_scheduler_current_thread()` returns
    /// the main thread with a valid host fiber context.
    fn initialize_physical_cores(&mut self) {
        self.schedulers.clear();
        self.cores.clear();
        self.main_threads.clear();
        self.idle_threads.clear();

        // Capture a raw pointer to self for use in fiber closures.
        // Safety: KernelCore outlives all fibers — fibers are destroyed during
        // kernel shutdown which happens before KernelCore is dropped.
        let kernel_ptr = self as *const KernelCore as usize;

        for i in 0..hardware_properties::NUM_CPU_CORES as usize {
            let core_id = i as i32;

            // Create scheduler and physical core.
            let scheduler = Arc::new(Mutex::new(KScheduler::new(core_id)));
            // Wire the global scheduler context so the scheduler can find threads.
            if let Some(ref gsc) = self.global_scheduler_context {
                scheduler.lock().unwrap().global_scheduler_context = Some(gsc.clone());
            }
            // Initialize the switch fiber for this scheduler.
            // Upstream: done implicitly during ScheduleImplFiber's first yield.
            KScheduler::init_switch_fiber(&scheduler);
            self.schedulers.push(scheduler.clone());
            self.cores.push(PhysicalCore::new(i, self.is_multicore));

            // Create main thread.
            // Upstream: auto* main_thread = KThread::Create(kernel);
            //           main_thread->SetCurrentCore(core);
            //           KThread::InitializeMainThread(system, main_thread, core);
            //           KThread::Register(kernel, main_thread);
            //
            // Upstream passes system.GetCpuManager().GetGuestActivateFunc() as the
            // fiber entry point. GuestActivate calls scheduler->Activate().
            let main_thread = Arc::new(Mutex::new(KThread::new()));
            {
                let thread_id = self.create_new_thread_id();
                let object_id = self.create_new_object_id() as u64;
                let mut t = main_thread.lock().unwrap();
                t.set_current_core(core_id);

                // Upstream: InitializeMainThread passes GetGuestActivateFunc().
                // GuestActivate() calls kernel.CurrentScheduler()->Activate().
                let kp = kernel_ptr;
                let guest_activate_func: Box<dyn FnOnce() + Send> = Box::new(move || {
                    // Safety: kernel_ptr is valid for the lifetime of this fiber.
                    let kernel = unsafe { &*(kp as *const KernelCore) };
                    crate::cpu_manager::CpuManager::guest_activate(kernel);
                });

                t.initialize_kernel_main_thread(core_id, thread_id, object_id, Some(guest_activate_func));
                t.bind_self_reference(&main_thread);
            }
            // Upstream: KThread::Register(kernel, main_thread) → adds to object list container.
            self.register_kernel_object(main_thread.lock().unwrap().get_object_id());

            // Create idle thread.
            // Upstream: auto* idle_thread = KThread::Create(kernel);
            //           idle_thread->SetCurrentCore(core);
            //           KThread::InitializeIdleThread(system, idle_thread, core);
            //           KThread::Register(kernel, idle_thread);
            //
            // Upstream passes system.GetCpuManager().GetIdleThreadStartFunc() as the
            // fiber entry point. IdleThreadFunction dispatches to MultiCoreRunIdleThread
            // or SingleCoreRunIdleThread.
            let idle_thread = Arc::new(Mutex::new(KThread::new()));
            {
                let thread_id = self.create_new_thread_id();
                let object_id = self.create_new_object_id() as u64;
                let mut t = idle_thread.lock().unwrap();
                t.set_current_core(core_id);

                // Upstream: InitializeIdleThread passes GetIdleThreadStartFunc().
                // IdleThreadFunction calls MultiCoreRunIdleThread or SingleCoreRunIdleThread.
                let kp = kernel_ptr;
                let is_mc = self.is_multicore;
                let idle_func: Box<dyn FnOnce() + Send> = Box::new(move || {
                    // Safety: kernel_ptr is valid for the lifetime of this fiber.
                    let kernel = unsafe { &*(kp as *const KernelCore) };
                    if is_mc {
                        crate::cpu_manager::CpuManager::multi_core_run_idle_thread_entry(kernel);
                    } else {
                        // Single-core idle requires CoreTiming and additional state
                        // that are not available from the fiber context yet.
                        // For now, run the multicore idle path which is functionally
                        // equivalent (idle + handle interrupt loop).
                        crate::cpu_manager::CpuManager::multi_core_run_idle_thread_entry(kernel);
                    }
                });

                t.initialize_kernel_idle_thread(core_id, thread_id, object_id, Some(idle_func));
                t.bind_self_reference(&idle_thread);
            }
            self.register_kernel_object(idle_thread.lock().unwrap().get_object_id());

            // Initialize the scheduler with the main and idle threads.
            // Upstream: schedulers[i]->Initialize(main_thread, idle_thread, core);
            // This sets m_current_thread = main_thread so get_scheduler_current_thread()
            // returns a valid thread.
            scheduler
                .lock()
                .unwrap()
                .initialize_with_threads(&main_thread, &idle_thread, core_id);

            self.main_threads.push(main_thread);
            self.idle_threads.push(idle_thread);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::SystemRef;

    #[test]
    fn close_services_requests_stop_on_tracked_server_managers() {
        let kernel = KernelCore::new();
        let manager = Arc::new(Mutex::new(ServerManager::new(SystemRef::null())));

        kernel.track_server_manager_for_test(Arc::clone(&manager));
        kernel.close_services();

        assert!(manager.lock().unwrap().stop_requested_for_test());
    }

    #[test]
    fn run_on_guest_core_process_retains_service_process_owner() {
        let mut kernel = KernelCore::new();
        kernel.initialize();

        kernel.run_on_guest_core_process("svc-test", Box::new(|| {}));

        assert_eq!(kernel.service_processes.lock().unwrap().len(), 1);

        let service_process = kernel.service_processes.lock().unwrap()[0].clone();
        let thread = {
            let process = service_process.lock().unwrap();
            process.thread_objects.values().next().cloned()
        }
        .expect("service process should keep its thread object");

        assert!(thread.lock().unwrap().parent.as_ref().and_then(Weak::upgrade).is_some());
    }
}

impl Default for KernelCore {
    fn default() -> Self {
        Self::new()
    }
}
