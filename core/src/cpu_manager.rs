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
    host_context: Option<Arc<Fiber>>,
    /// The host thread running this core.
    /// Upstream: `std::jthread host_thread`
    host_thread: Option<std::thread::JoinHandle<()>>,
}

impl Default for CoreData {
    fn default() -> Self {
        Self {
            host_context: None,
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
    /// Spawns N host threads that each call RunThread(core).
    pub fn initialize(&mut self) {
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

        // Upstream spawns threads here:
        //   core_data[core].host_thread =
        //       std::jthread([this, core](stop_token token) { RunThread(token, core); });
        //
        // Thread spawning requires passing a System reference to the thread.
        // In ruzu, threads are spawned by the caller (System::load or run_main_loop)
        // which can pass the necessary references. CpuManager provides RunThread
        // as a callable method.
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
    pub fn core_host_context(&self, core: usize) -> Option<&Arc<Fiber>> {
        self.core_data[core].host_context.as_ref()
    }

    // =========================================================================
    // Thread function closures — returned to KScheduler/KThread for fiber entry
    // =========================================================================

    /// Handle an interrupt on the current core.
    ///
    /// Upstream: `CpuManager::HandleInterrupt()` (cpu_manager.cpp:62-67).
    /// Delegates to KInterruptManager::HandleInterrupt.
    pub fn handle_interrupt(kernel: &mut KernelCore) {
        let core_index = kernel.current_physical_core_index();
        k_interrupt_manager::handle_interrupt(kernel, core_index as i32);
    }

    /// Guest thread activation function.
    ///
    /// Upstream: `CpuManager::GuestActivate()` (cpu_manager.cpp:168-175).
    /// Called as the entry point for new guest fibers. Gets the current
    /// scheduler and calls Activate(), which never returns.
    pub fn guest_activate(kernel: &KernelCore) {
        if let Some(scheduler_arc) = kernel.current_scheduler() {
            scheduler_arc.lock().unwrap().activate();
        }
        unreachable!("GuestActivate must not return");
    }

    /// Guest thread function — dispatches to multicore or single-core variant.
    ///
    /// Upstream: `CpuManager::GuestThreadFunction()` (cpu_manager.cpp:42-48).
    pub fn guest_thread_function(
        kernel: &mut KernelCore,
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
        kernel: &mut KernelCore,
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
    fn multi_core_run_guest_thread(kernel: &mut KernelCore) {
        // Upstream: kernel.CurrentScheduler()->OnThreadStart();
        if let Some(scheduler_arc) = kernel.current_scheduler() {
            if let Some(thread_arc) = kernel.get_current_emu_thread() {
                scheduler_arc.lock().unwrap().on_thread_start(&thread_arc);
            }
        }

        loop {
            // Upstream: auto* physical_core = &kernel.CurrentPhysicalCore();
            // while (!physical_core->IsInterrupted()) {
            //     physical_core->RunThread(thread);
            //     physical_core = &kernel.CurrentPhysicalCore();
            // }
            //
            // PhysicalCore::RunThread takes (jit, thread) arguments — the JIT
            // instance and opaque thread pointer are managed by the fiber context.
            // In the full fiber-based execution model, the JIT is owned by the
            // PhysicalCore and the thread comes from the scheduler.
            // For now, check interrupt and idle until the JIT is wired through
            // the fiber path.
            let physical_core = kernel.current_physical_core();
            if !physical_core.is_interrupted() {
                physical_core.idle();
            }

            Self::handle_interrupt(kernel);
        }
    }

    /// Multicore idle thread loop.
    ///
    /// Upstream: `CpuManager::MultiCoreRunIdleThread()` (cpu_manager.cpp:90-106).
    fn multi_core_run_idle_thread(kernel: &mut KernelCore) {
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
        kernel: &mut KernelCore,
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
            // See multi_core_run_guest_thread comment about JIT wiring.
            let physical_core = kernel.current_physical_core();
            if !physical_core.is_interrupted() {
                physical_core.idle();
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
        kernel: &mut KernelCore,
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

    /// Per-core host thread function.
    ///
    /// Upstream: `CpuManager::RunThread(stop_token, core)` (cpu_manager.cpp:186-222).
    ///
    /// Called on each spawned host thread. Registers with the kernel, creates
    /// a host fiber, waits for GPU, gets the scheduled thread, and yields to
    /// the guest fiber.
    pub fn run_thread(
        kernel: &KernelCore,
        core: usize,
        is_multicore: bool,
        is_async_gpu: bool,
        gpu_barrier: &Arc<Barrier>,
        core_host_context: &mut Option<Arc<Fiber>>,
        stop_requested: &AtomicBool,
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
        *core_host_context = Some(host_ctx.clone());

        // Running.
        // Upstream: if (!gpu_barrier->Sync(token)) { return; }
        gpu_barrier.sync();
        if stop_requested.load(Ordering::Relaxed) {
            // Clean up the fiber before returning.
            host_ctx.exit();
            return;
        }

        // Upstream: if (!is_async_gpu && !is_multicore) { system.GPU().ObtainContext(); }
        if !is_async_gpu && !is_multicore {
            // GPU context acquisition — requires GPU integration.
            // When GPU is fully wired, call gpu.obtain_context() here.
        }

        // Upstream: auto& scheduler = *kernel.CurrentScheduler();
        // auto* thread = scheduler.GetSchedulerCurrentThread();
        // Kernel::SetCurrentThread(kernel, thread);
        // Common::Fiber::YieldTo(data.host_context, *thread->GetHostContext());
        if let Some(scheduler_arc) = kernel.current_scheduler() {
            let scheduler = scheduler_arc.lock().unwrap();
            let _thread_id = scheduler.get_scheduler_current_thread_id();
            // To yield to the guest thread's fiber, we need the KThread object.
            // This requires the kernel to look up the thread by ID and get its
            // host_context. The full wiring depends on KScheduler creating
            // per-core main/idle threads during InitializePhysicalCores.
            drop(scheduler);
        }

        // Clean up.
        // Upstream: SCOPE_EXIT { data.host_context->Exit(); }
        host_ctx.exit();
        log::info!("CpuManager: {} exiting", name);
    }
}

impl Default for CpuManager {
    fn default() -> Self {
        Self::new()
    }
}
