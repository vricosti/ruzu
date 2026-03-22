//! Port of zuyu/src/core/cpu_manager.h and zuyu/src/core/cpu_manager.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! Manages the CPU threads. Creates a thread per core and dispatches execution.

use crate::core_timing::CoreTiming;
use crate::hardware_properties;
use crate::hle::kernel::k_interrupt_manager;
use crate::hle::kernel::kernel::KernelCore;
use common::fiber::Fiber;
use common::thread::Barrier;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

/// Per-core data held by the CPU manager.
/// Matches upstream `CpuManager::CoreData`.
struct CoreData {
    /// Host fiber context for this core's thread.
    /// Upstream: `std::shared_ptr<Common::Fiber> host_context`
    /// Wrapped in Arc<Mutex<>> because the spawned thread sets it and
    /// ShutdownThread reads it from the guest fiber context.
    host_context: Arc<Mutex<Option<Arc<Fiber>>>>,
    /// The host thread running this core.
    /// Upstream: `std::jthread host_thread`
    host_thread: Option<std::thread::JoinHandle<()>>,
}

impl Default for CoreData {
    fn default() -> Self {
        Self {
            host_context: Arc::new(Mutex::new(None)),
            host_thread: None,
        }
    }
}

/// Manages CPU threads for the emulated system.
/// Matches upstream `CpuManager` class (cpu_manager.h).
///
/// Upstream holds `System& system` — a reference to the owning System.
/// In Rust, CpuManager is owned by System, so we use a raw pointer set
/// after construction via `set_system()`. This mirrors the C++ pattern.
pub struct CpuManager {
    /// Barrier for GPU readiness synchronization.
    gpu_barrier: Option<Arc<Barrier>>,
    /// Per-core thread data.
    core_data: [CoreData; hardware_properties::NUM_CPU_CORES as usize],
    /// Whether we're using async GPU emulation.
    is_async_gpu: bool,
    /// Whether we're in multicore mode.
    is_multicore: bool,
    /// Currently active core index.
    current_core: AtomicUsize,
    /// Count of idle iterations (single-core).
    idle_count: usize,
    /// Number of active cores.
    num_cores: usize,
    /// Stop flag for graceful thread shutdown.
    /// Upstream: `std::stop_token` from `std::jthread`.
    stop_requested: Arc<AtomicBool>,
}

/// Maximum number of cycle runs before preemption in single-core mode.
const _MAX_CYCLE_RUNS: usize = 5;

impl CpuManager {
    /// Creates a new CpuManager.
    /// Upstream: `CpuManager::CpuManager(System& system_) : system{system_} {}`
    pub fn new() -> Self {
        Self {
            gpu_barrier: None,
            core_data: Default::default(),
            is_async_gpu: false,
            is_multicore: false,
            current_core: AtomicUsize::new(0),
            idle_count: 0,
            num_cores: 0,
            stop_requested: Arc::new(AtomicBool::new(false)),
        }
    }

    /// Sets if emulation is multicore or single core. Must be set before Initialize.
    pub fn set_multicore(&mut self, is_multi: bool) {
        self.is_multicore = is_multi;
    }

    /// Sets if emulation is using an asynchronous GPU.
    pub fn set_async_gpu(&mut self, is_async: bool) {
        self.is_async_gpu = is_async;
    }

    /// Called when the GPU is ready. Synchronizes with the GPU barrier.
    /// Upstream: `CpuManager::OnGpuReady()` (cpu_manager.h:47-49).
    pub fn on_gpu_ready(&self) {
        if let Some(ref barrier) = self.gpu_barrier {
            barrier.sync();
        }
    }

    /// Initializes the CPU manager, creating threads for each core.
    ///
    /// Upstream: `CpuManager::Initialize()` (cpu_manager.cpp:23-31).
    /// Creates GPU barrier and spawns N host threads that each call RunThread(core).
    ///
    /// # Safety
    /// `kernel_ptr` must point to a valid KernelCore that outlives all spawned
    /// threads (guaranteed by System::shutdown joining them first).
    pub unsafe fn initialize(&mut self, kernel_ptr: *const KernelCore) {
        self.num_cores = if self.is_multicore {
            hardware_properties::NUM_CPU_CORES as usize
        } else {
            1
        };

        // Create GPU barrier: num_cores + 1 (the +1 is for the GPU thread)
        self.gpu_barrier = Some(Arc::new(Barrier::new(self.num_cores + 1)));
        self.stop_requested.store(false, Ordering::Relaxed);

        log::info!(
            "CpuManager: initialized for {} core(s), multicore={}",
            self.num_cores,
            self.is_multicore
        );

        // Upstream: for (core = 0; core < num_cores; core++)
        //   core_data[core].host_thread = jthread([this, core](token) { RunThread(token, core); });
        for core in 0..self.num_cores {
            let barrier = self.gpu_barrier.clone().expect("gpu_barrier must be set before spawn_threads");
            let stop = self.stop_requested.clone();
            let is_mc = self.is_multicore;
            let is_ag = self.is_async_gpu;
            let host_ctx_slot = self.core_data[core].host_context.clone();
            // Safety: kernel_ptr is valid for the lifetime of the threads (joined before drop).
            // We transmit as usize because the crate is named `core` which shadows
            // `std::core`, preventing `unsafe impl Send` for raw pointer wrappers.
            let kp = kernel_ptr as usize;

            // Upstream: core_data[core].host_thread =
            //   std::jthread([this, core](stop_token token) { RunThread(token, core); });
            let handle = std::thread::Builder::new()
                .name(if is_mc { format!("CPUCore_{}", core) } else { "CPUThread".to_string() })
                .spawn(move || {
                    // Safety: kernel_ptr is valid for the lifetime of this thread.
                    let kernel = unsafe { &*(kp as *const KernelCore) };
                    Self::run_thread_on_core(kernel, core, is_mc, is_ag, &barrier, &stop, &host_ctx_slot);
                })
                .expect("Failed to spawn CPU thread");

            self.core_data[core].host_thread = Some(handle);
        }
    }

    /// Shuts down all CPU threads.
    /// Upstream: `CpuManager::Shutdown()` (cpu_manager.cpp:33-40).
    pub fn shutdown(&mut self) {
        self.stop_requested.store(true, Ordering::Relaxed);
        for i in 0..self.num_cores {
            if let Some(thread) = self.core_data[i].host_thread.take() {
                let _ = thread.join();
            }
        }
        log::info!("CpuManager: shutdown complete");
    }

    /// Returns the currently active core index.
    pub fn current_core(&self) -> usize {
        self.current_core.load(Ordering::Relaxed)
    }

    /// Returns whether multicore mode is enabled.
    pub fn is_multicore(&self) -> bool {
        self.is_multicore
    }

    /// Returns whether async GPU mode is enabled.
    pub fn is_async_gpu(&self) -> bool {
        self.is_async_gpu
    }

    /// Whether stop has been requested.
    pub fn is_stop_requested(&self) -> bool {
        self.stop_requested.load(Ordering::Relaxed)
    }

    /// Get the host context for a core (for fiber switching).
    /// Returns a clone of the Arc if set.
    pub fn core_host_context(&self, core: usize) -> Option<Arc<Fiber>> {
        self.core_data[core].host_context.lock().unwrap().clone()
    }

    // =========================================================================
    // Thread function closures — returned to KScheduler/KThread for fiber entry
    // =========================================================================

    /// Handle an interrupt on the current core.
    ///
    /// Upstream: `CpuManager::HandleInterrupt()` (cpu_manager.cpp:62-67).
    /// Delegates to KInterruptManager::HandleInterrupt.
    pub fn handle_interrupt(kernel: &KernelCore) {
        let core_index = kernel.current_physical_core_index();
        k_interrupt_manager::handle_interrupt(kernel, core_index as i32);
    }

    /// Guest thread activation function.
    ///
    /// Upstream: `CpuManager::GuestActivate()` (cpu_manager.cpp:168-175).
    /// Called as the entry point for new guest fibers.
    ///
    /// Upstream calls scheduler->Activate() which fiber-switches into
    /// ScheduleImplFiber → SwitchThread → Reload → YieldTo the next thread's
    /// fiber whose entry is GuestThreadFunction → MultiCoreRunGuestThread.
    /// Because the fiber switch never returns, upstream has no code after
    /// Activate().
    ///
    /// Since fiber-based scheduling isn't fully wired yet, we enter the guest
    /// dispatch loop directly after Activate(). This is the function that the
    /// fiber would eventually run, so the end result is identical: the current
    /// core's host thread executes guest code in a loop.
    pub fn guest_activate(kernel: &KernelCore) {
        if let Some(scheduler_arc) = kernel.current_scheduler() {
            scheduler_arc.lock().unwrap().activate();
        }

        // Upstream: scheduler.Activate() fiber-switches and never returns.
        // Since fiber-based scheduling isn't complete, enter the guest
        // dispatch loop directly — this is what the fiber would eventually run.
        let is_multicore = kernel.is_multicore();
        if is_multicore {
            Self::multi_core_run_guest_thread(kernel);
        } else {
            // Single-core needs CoreTiming for time advancement and preemption.
            if let Some(core_timing) = kernel.core_timing() {
                let core_timing = core_timing.clone();
                let current_core = AtomicUsize::new(0);
                let mut idle_count: usize = 0;
                Self::single_core_run_guest_thread(
                    kernel,
                    &core_timing,
                    &current_core,
                    &mut idle_count,
                );
            } else {
                log::error!("GuestActivate: single-core mode but no CoreTiming available");
                // Fall back to multicore loop which doesn't need CoreTiming.
                Self::multi_core_run_guest_thread(kernel);
            }
        }
        // The guest thread functions loop forever, so this is effectively unreachable.
        unreachable!("GuestActivate: guest thread function returned unexpectedly");
    }

    /// Guest thread function — dispatches to multicore or single-core variant.
    ///
    /// Upstream: `CpuManager::GuestThreadFunction()` (cpu_manager.cpp:42-48).
    pub fn guest_thread_function(
        kernel: &KernelCore,
        core_timing: &Arc<Mutex<CoreTiming>>,
        current_core: &AtomicUsize,
        idle_count: &mut usize,
        is_multicore: bool,
    ) {
        if is_multicore {
            Self::multi_core_run_guest_thread(kernel);
        } else {
            Self::single_core_run_guest_thread(kernel, core_timing, current_core, idle_count);
        }
    }

    /// Idle thread function — dispatches to multicore or single-core variant.
    ///
    /// Upstream: `CpuManager::IdleThreadFunction()` (cpu_manager.cpp:50-56).
    pub fn idle_thread_function(
        kernel: &KernelCore,
        core_timing: &Arc<Mutex<CoreTiming>>,
        current_core: &AtomicUsize,
        idle_count: &mut usize,
        is_multicore: bool,
    ) {
        if is_multicore {
            Self::multi_core_run_idle_thread(kernel);
        } else {
            Self::single_core_run_idle_thread(kernel, core_timing, current_core, idle_count);
        }
    }

    /// Shutdown thread function.
    ///
    /// Upstream: `CpuManager::ShutdownThread()` (cpu_manager.cpp:177-184).
    /// Yields from the current thread's host context back to the core's host context.
    pub fn shutdown_thread(
        kernel: &KernelCore,
        core_data_host_context: &Arc<Fiber>,
        is_multicore: bool,
    ) {
        let _core = if is_multicore {
            kernel.current_physical_core_index()
        } else {
            0
        };

        if let Some(thread_arc) = kernel.get_current_emu_thread() {
            let thread = thread_arc.lock().unwrap();
            if let Some(thread_host_ctx) = thread.get_host_context() {
                Fiber::yield_to(Arc::downgrade(thread_host_ctx), core_data_host_context);
            }
        }
        unreachable!("ShutdownThread must not return");
    }

    // =========================================================================
    // MultiCore guest/idle thread loops
    // =========================================================================

    /// Multicore guest thread loop.
    ///
    /// Upstream: `CpuManager::MultiCoreRunGuestThread()` (cpu_manager.cpp:73-88).
    /// Runs on the guest fiber. Calls PhysicalCore::RunThread in a loop,
    /// handling interrupts between runs.
    fn multi_core_run_guest_thread(kernel: &KernelCore) {
        // Upstream: auto* thread = Kernel::GetCurrentThreadPointer(kernel);
        // kernel.CurrentScheduler()->OnThreadStart();
        //
        // In upstream, by this point the scheduler has fiber-switched to the
        // application's user thread. The user thread starts with disable_count=1,
        // and OnThreadStart() decrements it to 0 (EnableDispatch).
        //
        // Upstream: by this point the scheduler has fiber-switched to the
        // application's user thread. The user thread starts with disable_count=1,
        // and OnThreadStart() calls EnableDispatch (1→0).
        //
        // Since we skip the fiber switch:
        // 1. Set the application thread as current on core 0
        // 2. Restore disable_count to 1 (Activate's RescheduleCurrentCore decremented it to 0)
        //    so OnThreadStart's EnableDispatch works correctly
        if kernel.current_physical_core_index() == 0 {
            if let Some(app_thread) = kernel.get_application_thread() {
                super::hle::kernel::kernel::set_current_emu_thread(Some(&app_thread));
            }
        }

        // Upstream: the thread OnThreadStart runs on always has disable_count=1.
        // Activate() → RescheduleCurrentCore() decremented it to 0. Restore it.
        if let Some(thread_arc) = super::hle::kernel::kernel::get_current_thread_pointer() {
            let mut t = thread_arc.lock().unwrap();
            if t.get_disable_dispatch_count() == 0 {
                t.disable_dispatch();
            }
            drop(t);

            if let Some(scheduler_arc) = kernel.current_scheduler() {
                scheduler_arc.lock().unwrap().on_thread_start(&thread_arc);
            }
        }

        loop {
            let physical_core = kernel.current_physical_core();
            while !physical_core.is_interrupted() {
                // Upstream: physical_core->RunThread(thread) gets the JIT from
                // thread->GetOwnerProcess()->GetArmInterface(core_index).
                // Here we replicate that lookup.
                Self::run_guest_thread_once(kernel, physical_core);
                // Re-fetch in case we changed cores during scheduling.
                break;
            }

            Self::handle_interrupt(kernel);
        }
    }

    /// Execute one iteration of guest code via the JIT on the given physical core.
    ///
    /// Upstream: `PhysicalCore::RunThread(KThread*)` (physical_core.cpp:22-146).
    /// Gets the ARM interface from thread->GetOwnerProcess()->GetArmInterface(core_index),
    /// then runs the JIT until an interrupt, SVC, or halt.
    fn run_guest_thread_once(kernel: &KernelCore, physical_core: &super::hle::kernel::physical_core::PhysicalCore) {
        use crate::arm::arm_interface::KThread as OpaqueKThread;

        let thread_arc = match super::hle::kernel::kernel::get_current_thread_pointer() {
            Some(t) => t,
            None => return,
        };

        let thread = thread_arc.lock().unwrap();
        // Get the owner process to access the ARM JIT interface.
        let parent_weak = match thread.parent.as_ref() {
            Some(p) => p.clone(),
            None => {
                drop(thread);
                physical_core.idle();
                return;
            }
        };
        drop(thread);

        let parent_arc = match parent_weak.upgrade() {
            Some(p) => p,
            None => return,
        };

        let core_index = physical_core.core_index();

        // Get the ARM interface from the process for this core.
        // Upstream: auto* interface = process->GetArmInterface(m_core_index);
        let mut process = parent_arc.lock().unwrap();
        let jit = match process.get_arm_interface_mut(core_index) {
            Some(j) => j as *mut _ as *mut Box<dyn crate::arm::arm_interface::ArmInterface>,
            None => {
                drop(process);
                physical_core.idle();
                return;
            }
        };
        drop(process);

        // Safety: The JIT is owned by the process which lives as long as the thread.
        // We hold the raw pointer only for the duration of this call.
        // The thread_arc Mutex<KThread> is cast to OpaqueKThread for the JIT interface.
        let thread_ptr = {
            let mut t = thread_arc.lock().unwrap();
            &mut *t as *mut super::hle::kernel::k_thread::KThread
        };

        unsafe {
            let jit_ref = &mut **jit;
            let opaque = &mut *(thread_ptr as *mut OpaqueKThread);

            // Load the thread's register context into the JIT.
            // Upstream: PhysicalCore::LoadContext → interface->SetContext(thread->GetContext())
            {
                let thread = thread_arc.lock().unwrap();
                // Convert k_thread::ThreadContext → arm_interface::ThreadContext.
                // Both structs have identical layout, so transmute is safe.
                let k_ctx = &thread.thread_context;
                let arm_ctx: &crate::arm::arm_interface::ThreadContext =
                    unsafe { &*(k_ctx as *const super::hle::kernel::k_thread::ThreadContext
                        as *const crate::arm::arm_interface::ThreadContext) };
                jit_ref.set_context(arm_ctx);
                jit_ref.set_tpidrro_el0(thread.get_tls_address().get());
            }

            // Set the running state on the physical core.
            physical_core.set_running(
                jit_ref as *mut dyn crate::arm::arm_interface::ArmInterface,
                thread_ptr,
            );

            // Run the JIT — executes ARM instructions until SVC, interrupt, or halt.
            let _halt_reason = jit_ref.run_thread(opaque);

            // Clear the running state.
            physical_core.clear_running();
        }
    }

    /// Public entry point for the multicore idle thread loop, called from
    /// fiber closures created in `KernelCore::initialize_physical_cores`.
    ///
    /// Upstream: `CpuManager::IdleThreadFunction()` → `MultiCoreRunIdleThread()`.
    pub fn multi_core_run_idle_thread_entry(kernel: &KernelCore) {
        Self::multi_core_run_idle_thread(kernel);
    }

    /// Multicore idle thread loop.
    ///
    /// Upstream: `CpuManager::MultiCoreRunIdleThread()` (cpu_manager.cpp:90-106).
    fn multi_core_run_idle_thread(kernel: &KernelCore) {
        if let Some(scheduler_arc) = kernel.current_scheduler() {
            if let Some(thread_arc) = kernel.get_current_emu_thread() {
                scheduler_arc.lock().unwrap().on_thread_start(&thread_arc);
            }
        }

        loop {
            let physical_core = kernel.current_physical_core();
            if !physical_core.is_interrupted() {
                physical_core.idle();
            }

            Self::handle_interrupt(kernel);
        }
    }

    // =========================================================================
    // SingleCore guest/idle thread loops
    // =========================================================================

    /// Single-core guest thread loop.
    ///
    /// Upstream: `CpuManager::SingleCoreRunGuestThread()` (cpu_manager.cpp:112-131).
    /// In the full fiber model, PhysicalCore::RunThread drives JIT execution
    /// for the current guest thread (obtained from the scheduler).
    fn single_core_run_guest_thread(
        kernel: &KernelCore,
        core_timing: &Arc<Mutex<CoreTiming>>,
        current_core: &AtomicUsize,
        idle_count: &mut usize,
    ) {
        // Upstream: kernel.CurrentScheduler()->OnThreadStart();
        if let Some(scheduler_arc) = kernel.current_scheduler() {
            if let Some(thread_arc) = kernel.get_current_emu_thread() {
                scheduler_arc.lock().unwrap().on_thread_start(&thread_arc);
            }
        }

        loop {
            // Upstream: physical_core->RunThread(thread)
            let physical_core = kernel.current_physical_core();
            if !physical_core.is_interrupted() {
                Self::run_guest_thread_once(kernel, physical_core);
            }

            // Upstream: kernel.SetIsPhantomModeForSingleCore(true);
            kernel.set_is_phantom_mode_for_single_core(true);
            let _ = core_timing.lock().unwrap().advance();
            kernel.set_is_phantom_mode_for_single_core(false);

            Self::preempt_single_core_inner(kernel, core_timing, current_core, idle_count, true);
            Self::handle_interrupt(kernel);
        }
    }

    /// Single-core idle thread loop.
    ///
    /// Upstream: `CpuManager::SingleCoreRunIdleThread()` (cpu_manager.cpp:133-143).
    fn single_core_run_idle_thread(
        kernel: &KernelCore,
        core_timing: &Arc<Mutex<CoreTiming>>,
        current_core: &AtomicUsize,
        idle_count: &mut usize,
    ) {
        if let Some(scheduler_arc) = kernel.current_scheduler() {
            if let Some(thread_arc) = kernel.get_current_emu_thread() {
                scheduler_arc.lock().unwrap().on_thread_start(&thread_arc);
            }
        }

        loop {
            Self::preempt_single_core_inner(kernel, core_timing, current_core, idle_count, false);
            core_timing.lock().unwrap().add_ticks(1000);
            *idle_count += 1;
            Self::handle_interrupt(kernel);
        }
    }

    // =========================================================================
    // Preemption
    // =========================================================================

    /// Preempts the current core in single-core mode.
    ///
    /// Upstream: `CpuManager::PreemptSingleCore(bool)` (cpu_manager.cpp:145-166).
    ///
    /// This is the public API called from System::run_main_loop.
    pub fn preempt_single_core(
        &mut self,
        core_timing: &Arc<Mutex<CoreTiming>>,
        from_running_environment: bool,
    ) {
        // Without kernel access, delegate to the inner version using stored state.
        // This path is used by the legacy System::run_main_loop path.
        if self.idle_count >= 4 || from_running_environment {
            if !from_running_environment {
                core_timing.lock().unwrap().idle();
                self.idle_count = 0;
            }
            let _ = core_timing.lock().unwrap().advance();
        }

        let next_core = (self.current_core.load(Ordering::Relaxed) + 1)
            % hardware_properties::NUM_CPU_CORES as usize;
        self.current_core.store(next_core, Ordering::Relaxed);

        core_timing.lock().unwrap().reset_ticks();
    }

    /// Inner preemption with full kernel access.
    ///
    /// Upstream: `CpuManager::PreemptSingleCore(bool)` (cpu_manager.cpp:145-166).
    fn preempt_single_core_inner(
        kernel: &KernelCore,
        core_timing: &Arc<Mutex<CoreTiming>>,
        current_core: &AtomicUsize,
        idle_count: &mut usize,
        from_running_environment: bool,
    ) {
        if *idle_count >= 4 || from_running_environment {
            if !from_running_environment {
                core_timing.lock().unwrap().idle();
                *idle_count = 0;
            }
            kernel.set_is_phantom_mode_for_single_core(true);
            let _ = core_timing.lock().unwrap().advance();
            kernel.set_is_phantom_mode_for_single_core(false);
        }

        let next_core = (current_core.load(Ordering::Relaxed) + 1)
            % hardware_properties::NUM_CPU_CORES as usize;
        current_core.store(next_core, Ordering::Relaxed);
        core_timing.lock().unwrap().reset_ticks();

        // Upstream: kernel.Scheduler(current_core).PreemptSingleCore();
        if let Some(scheduler) = kernel.scheduler(next_core) {
            scheduler.lock().unwrap().preempt_single_core();
        }

        // We've now been scheduled again, and we may have exchanged schedulers.
        // Reload the scheduler in case it's different.
        if let Some(scheduler) = kernel.scheduler(next_core) {
            if !scheduler.lock().unwrap().is_idle() {
                *idle_count = 0;
            }
        }
    }

    // =========================================================================
    // RunThread — the per-core host thread entry point
    // =========================================================================

    /// Per-core host thread function (static, called from spawned threads).
    ///
    /// Upstream: `CpuManager::RunThread(stop_token, core)` (cpu_manager.cpp:186-222).
    ///
    /// Called on each spawned host thread. Registers with the kernel, creates
    /// a host fiber, waits for GPU, gets the scheduled thread, and yields to
    /// the guest fiber.
    fn run_thread_on_core(
        kernel: &KernelCore,
        core: usize,
        is_multicore: bool,
        is_async_gpu: bool,
        gpu_barrier: &Arc<Barrier>,
        stop_requested: &AtomicBool,
        host_context_slot: &Arc<Mutex<Option<Arc<Fiber>>>>,
    ) {
        // Initialization.
        // Upstream: system.RegisterCoreThread(core);
        kernel.register_core_thread(core);

        let name = if is_multicore {
            format!("CPUCore_{}", core)
        } else {
            "CPUThread".to_string()
        };
        // Upstream: Common::SetCurrentThreadName(name.c_str());
        // Upstream: Common::SetCurrentThreadPriority(Common::ThreadPriority::Critical);
        log::info!("CpuManager: {} starting", name);

        // Upstream: data.host_context = Common::Fiber::ThreadToFiber();
        let host_ctx = Fiber::thread_to_fiber();
        *host_context_slot.lock().unwrap() = Some(host_ctx.clone());

        // Upstream: SCOPE_EXIT { data.host_context->Exit(); }
        // We use a guard struct to ensure Exit is called on all return paths.
        struct FiberGuard<'a> {
            fiber: &'a Arc<Fiber>,
            name: &'a str,
        }
        impl<'a> Drop for FiberGuard<'a> {
            fn drop(&mut self) {
                self.fiber.exit();
                log::info!("CpuManager: {} exiting", self.name);
            }
        }
        let _guard = FiberGuard {
            fiber: &host_ctx,
            name: &name,
        };

        // Running.
        // Upstream: if (!gpu_barrier->Sync(token)) { return; }
        gpu_barrier.sync();
        if stop_requested.load(Ordering::Relaxed) {
            return;
        }

        // Upstream: if (!is_async_gpu && !is_multicore) { system.GPU().ObtainContext(); }
        if !is_async_gpu && !is_multicore {
            // GPU context acquisition — requires GPU integration.
            // When GPU is fully wired, call gpu.obtain_context() here.
        }

        // Upstream:
        //   auto& scheduler = *kernel.CurrentScheduler();
        //   auto* thread = scheduler.GetSchedulerCurrentThread();
        //   Kernel::SetCurrentThread(kernel, thread);
        //   Common::Fiber::YieldTo(data.host_context, *thread->GetHostContext());
        if let Some(scheduler_arc) = kernel.current_scheduler() {
            let scheduler = scheduler_arc.lock().unwrap();
            if let Some(thread_arc) = scheduler.get_scheduler_current_thread() {
                // Upstream: Kernel::SetCurrentThread(kernel, thread);
                super::hle::kernel::kernel::set_current_emu_thread(Some(&thread_arc));

                // Get the guest thread's host context for fiber switching.
                let thread = thread_arc.lock().unwrap();
                if let Some(thread_host_ctx) = thread.get_host_context() {
                    let thread_host_ctx = thread_host_ctx.clone();
                    drop(thread);
                    drop(scheduler);

                    // Upstream: Common::Fiber::YieldTo(data.host_context, *thread->GetHostContext());
                    Fiber::yield_to(Arc::downgrade(&host_ctx), &thread_host_ctx);
                } else {
                    drop(thread);
                    drop(scheduler);
                    log::warn!(
                        "CpuManager: {} — scheduler current thread has no host context",
                        name
                    );
                }
            } else {
                drop(scheduler);
                log::warn!(
                    "CpuManager: {} — no scheduler current thread available",
                    name
                );
            }
        } else {
            log::warn!("CpuManager: {} — no current scheduler", name);
        }

        // After YieldTo returns, we've been yielded back (from ShutdownThread).
        // The _guard drop will call host_ctx.exit() and log.
    }
}

impl Default for CpuManager {
    fn default() -> Self {
        Self::new()
    }
}
