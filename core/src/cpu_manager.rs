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

    /// Perform a scheduling fiber switch without holding the per-core scheduler Mutex.
    ///
    /// Upstream: after `HandleInterrupt`, `RequestScheduleOnInterrupt` directly
    /// calls `ScheduleOnInterrupt` → `Schedule` → `ScheduleImpl` (fiber yield)
    /// without holding any lock.  In Rust, holding the scheduler Mutex across a
    /// fiber yield causes deadlock (the next fiber cannot re-lock the same Mutex).
    /// This helper acquires the Mutex briefly to get a raw pointer, releases it,
    /// then calls `KScheduler::schedule_raw_if_needed` via the raw pointer so the
    /// actual fiber switch occurs without the Mutex held.
    ///
    /// Called after every `handle_interrupt()` in the guest and idle thread loops.
    fn reschedule_current_core_raw(kernel: &KernelCore) {
        if let Some(scheduler_arc) = kernel.current_scheduler() {
            let sched_ptr = {
                let guard = scheduler_arc.lock().unwrap();
                &*guard as *const super::hle::kernel::k_scheduler::KScheduler
                    as *mut super::hle::kernel::k_scheduler::KScheduler
            }; // Mutex guard dropped here
            // Safety: core OS thread; scheduler Arc keeps pointer valid; cooperative fibers.
            unsafe {
                super::hle::kernel::k_scheduler::KScheduler::schedule_raw_if_needed(sched_ptr);
            }
        }
    }

    /// Guest thread activation function.
    ///
    /// Upstream: `CpuManager::GuestActivate()` (cpu_manager.cpp:168-175).
    /// Called as the entry point for main thread fibers (InitializeMainThread).
    ///
    /// Upstream calls scheduler->Activate() which fiber-switches to the
    /// scheduled thread's fiber whose entry is GuestThreadFunction.
    /// The fiber switch never returns.
    pub fn guest_activate(kernel: &KernelCore) {
        if let Some(scheduler_arc) = kernel.current_scheduler() {
            // Acquire the lock briefly to get a raw pointer, then release it
            // BEFORE calling activate_and_schedule_raw.  This prevents holding
            // the per-core scheduler Mutex across any fiber yield inside
            // schedule_impl_fiber (which would deadlock the next fiber that
            // tries to lock the same Mutex).
            let sched_ptr = {
                let guard = scheduler_arc.lock().unwrap();
                &*guard as *const super::hle::kernel::k_scheduler::KScheduler
                    as *mut super::hle::kernel::k_scheduler::KScheduler
            }; // Mutex guard dropped here
            // Safety: we're on the core OS thread; the scheduler Arc keeps the
            // pointer valid; fibers are cooperative so no other fiber on this
            // thread runs concurrently.
            unsafe {
                super::hle::kernel::k_scheduler::KScheduler::activate_and_schedule_raw(sched_ptr);
            }
        }
        // Upstream: scheduler.Activate() fiber-switches and never returns.
        // If it returns, this core has no user thread — run idle loop.
        // This happens for cores 1-3 when only core 0 has the application thread.
        log::info!("GuestActivate: no user thread on this core, entering idle loop");
        Self::multi_core_run_idle_thread(kernel);
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

    fn shutdown_if_requested(kernel: &KernelCore) {
        let system_ref = kernel.system();
        if system_ref.is_null() || !system_ref.get().is_shutting_down() {
            return;
        }

        let system = system_ref.get();
        let core = if system.cpu_manager.is_multicore() {
            kernel.current_physical_core_index()
        } else {
            0
        };
        let host_context = system
            .cpu_manager
            .core_host_context(core)
            .expect("core host context must exist during shutdown");

        Self::shutdown_thread(kernel, &host_context, system.cpu_manager.is_multicore());
    }

    // =========================================================================
    // MultiCore guest/idle thread loops
    // =========================================================================

    /// Multicore guest thread loop.
    ///
    /// Upstream: `CpuManager::MultiCoreRunGuestThread()` (cpu_manager.cpp:73-88).
    /// Runs on the guest fiber. Calls PhysicalCore::RunThread in a loop,
    /// handling interrupts between runs.
    pub fn multi_core_run_guest_thread(kernel: &KernelCore) {
        log::info!("multi_core_run_guest_thread: ENTERED on core {}", kernel.current_physical_core_index());

        let cur_thread = super::hle::kernel::kernel::get_current_thread_pointer();
        log::info!(
            "multi_core_run_guest_thread: current_thread={}, has_parent={}",
            cur_thread.as_ref().map(|t| t.lock().unwrap().get_thread_id()).unwrap_or(u64::MAX),
            cur_thread.as_ref().map(|t| t.lock().unwrap().parent.is_some()).unwrap_or(false),
        );

        // Upstream: auto* thread = Kernel::GetCurrentThreadPointer(kernel);
        // kernel.CurrentScheduler()->OnThreadStart();
        // Upstream: kernel.CurrentScheduler()->OnThreadStart();
        // OnThreadStart just calls thread.EnableDispatch().
        // We can't lock the scheduler here because it's still locked by
        // guest_activate → scheduler.activate() → schedule_impl_fiber
        // (the lock is on the suspended kernel main thread's stack).
        // Call EnableDispatch directly matching what OnThreadStart does.
        if let Some(thread_arc) = super::hle::kernel::kernel::get_current_thread_pointer() {
            let mut t = thread_arc.lock().unwrap();
            if t.get_disable_dispatch_count() > 0 {
                t.enable_dispatch();
            }
        }

        loop {
            Self::shutdown_if_requested(kernel);
            let mut physical_core = kernel.current_physical_core();
            while !physical_core.is_interrupted() {
                Self::run_guest_thread_once(kernel, physical_core);
                // Upstream: physical_core = &kernel.CurrentPhysicalCore();
                physical_core = kernel.current_physical_core();
            }

            Self::handle_interrupt(kernel);
            Self::shutdown_if_requested(kernel);
            // Upstream: RequestScheduleOnInterrupt → ScheduleOnInterrupt → fiber yield.
            // Done outside any Mutex lock to avoid deadlock (see reschedule_current_core_raw).
            Self::reschedule_current_core_raw(kernel);
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

            // Upstream PhysicalCore::RunThread loop (physical_core.cpp:65-145):
            // NOTE: upstream does NOT call SetContext here — context is loaded
            // once by the scheduler's Reload during context switches. The JIT
            // maintains its own register state across run_thread() calls.
            // Runs the JIT, handles the halt reason, loops until interrupt/return.
            loop {
                // Check termination.
                {
                    let thread = thread_arc.lock().unwrap();
                    if thread.has_dpc() && thread.is_termination_requested() {
                        break;
                    }
                }

                // Enter context — mark running on physical core.
                if physical_core.is_interrupted() {
                    break;
                }
                physical_core.set_running(
                    jit_ref as *mut dyn crate::arm::arm_interface::ArmInterface,
                    thread_ptr,
                );

                // Run the JIT.
                let hr = jit_ref.run_thread(opaque);

                // Exit context — clear running.
                physical_core.clear_running();

                // Step-trace after SVC N (RUZU_STEP_AFTER_SVC=N).
                // Enabled at compile time via cfg. Steps all instructions from that
                // point, dispatching SVCs inline with full args + TLS logging, and
                // exiting on svcBreak. Disabled by default to avoid overhead.
                //
                // To enable: change `cfg!(feature = "step_tracer")` to `true` below,
                // or use: cargo build --features step_tracer
                #[allow(unreachable_code)]
                if cfg!(feature = "step_tracer") {
                    use std::sync::atomic::{AtomicU32, AtomicBool, Ordering};
                    static SVC_N: AtomicU32 = AtomicU32::new(0);
                    static ACTIVE: AtomicBool = AtomicBool::new(false);
                    static DONE: AtomicBool = AtomicBool::new(false);
                    if !DONE.load(Ordering::Relaxed) {
                        let threshold: Option<u32> = {
                            use std::sync::OnceLock;
                            static V: OnceLock<Option<u32>> = OnceLock::new();
                            *V.get_or_init(|| std::env::var("RUZU_STEP_AFTER_SVC").ok().and_then(|s| s.trim().parse().ok()))
                        };
                        if let Some(t) = threshold {
                            if hr.contains(crate::arm::arm_interface::HaltReason::SUPERVISOR_CALL) {
                                if SVC_N.fetch_add(1, Ordering::Relaxed) == t {
                                    ACTIVE.store(true, Ordering::Relaxed);
                                    eprintln!("=== STEP TRACE activated after SVC#{} ===", t);
                                }
                            }
                            if ACTIVE.load(Ordering::Relaxed) {
                                for step in 0..500_000u32 {
                                    let op = unsafe { &mut *(thread_ptr as *mut crate::arm::arm_interface::KThread) };
                                    let h = jit_ref.step_thread(op);
                                    if step % 10_000 == 0 {
                                        let mut c = crate::arm::arm_interface::ThreadContext::default();
                                        jit_ref.get_context(&mut c);
                                        eprintln!("STEP {:6} PC={:#010x} SP={:#010x} LR={:#010x} R0={:#010x}",
                                            step, c.pc, c.sp, c.lr, c.r[0]);
                                    }
                                    if h.contains(crate::arm::arm_interface::HaltReason::SUPERVISOR_CALL) {
                                        let svc = jit_ref.get_svc_number();
                                        let mut args = [0u64; 8];
                                        jit_ref.get_svc_arguments(&mut args);
                                        let mut c = crate::arm::arm_interface::ThreadContext::default();
                                        jit_ref.get_context(&mut c);
                                        eprintln!("SVC_IN  imm=0x{:02x} step={} core=0 tid=0 args=[0x{:x},0x{:x},0x{:x},0x{:x},0x{:x},0x{:x},0x{:x},0x{:x}]",
                                            svc, step, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
                                        // For SendSyncRequest, dump TLS before
                                        if svc == 0x21 {
                                            let sr = kernel.system();
                                            if !sr.is_null() {
                                                let sys = sr.get();
                                                if let Some(memory) = sys.get_svc_memory() {
                                                    let m = memory.lock().unwrap();
                                                    let tls = c.r[1] as u64; // R1 often has TLS in args but let's use actual TLS
                                                    if let Some(thread) = sys.current_thread() {
                                                        let tls_addr = thread.lock().unwrap().get_tls_address().get();
                                                        let mut w = [0u32; 16];
                                                        for i in 0..16 { w[i] = m.read_32(tls_addr + i as u64 * 4); }
                                                        eprintln!("  TLS_REQ [{:#x}]: {:08x} {:08x} {:08x} {:08x}  {:08x} {:08x} {:08x} {:08x}  {:08x} {:08x} {:08x} {:08x}  {:08x} {:08x} {:08x} {:08x}",
                                                            tls_addr, w[0],w[1],w[2],w[3],w[4],w[5],w[6],w[7],w[8],w[9],w[10],w[11],w[12],w[13],w[14],w[15]);
                                                    }
                                                }
                                            }
                                        }
                                        // Dispatch
                                        let sr = kernel.system();
                                        if !sr.is_null() {
                                            let sys = sr.get();
                                            crate::hle::kernel::svc_dispatch::call(sys, svc, false, &mut args);
                                            jit_ref.set_svc_arguments(&args);
                                        }
                                        eprintln!("SVC_OUT imm=0x{:02x} args=[0x{:x},0x{:x},0x{:x},0x{:x},0x{:x},0x{:x},0x{:x},0x{:x}]",
                                            svc, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
                                        // For SendSyncRequest, dump TLS after
                                        if svc == 0x21 {
                                            let sr2 = kernel.system();
                                            if !sr2.is_null() {
                                                let sys2 = sr2.get();
                                                if let Some(memory) = sys2.get_svc_memory() {
                                                    let m = memory.lock().unwrap();
                                                    if let Some(thread) = sys2.current_thread() {
                                                        let tls_addr = thread.lock().unwrap().get_tls_address().get();
                                                        let mut w = [0u32; 16];
                                                        for i in 0..16 { w[i] = m.read_32(tls_addr + i as u64 * 4); }
                                                        eprintln!("  TLS_RSP [{:#x}]: {:08x} {:08x} {:08x} {:08x}  {:08x} {:08x} {:08x} {:08x}  {:08x} {:08x} {:08x} {:08x}  {:08x} {:08x} {:08x} {:08x}",
                                                            tls_addr, w[0],w[1],w[2],w[3],w[4],w[5],w[6],w[7],w[8],w[9],w[10],w[11],w[12],w[13],w[14],w[15]);
                                                    }
                                                }
                                            }
                                        }
                                        if svc == 0x26 {
                                            eprintln!("=== svcBreak — exiting ===");
                                            DONE.store(true, Ordering::Relaxed);
                                            ACTIVE.store(false, Ordering::Relaxed);
                                            std::process::exit(1);
                                        }
                                    }
                                }
                                eprintln!("=== step limit reached (500K) ===");
                                DONE.store(true, Ordering::Relaxed);
                                ACTIVE.store(false, Ordering::Relaxed);
                            }
                        }
                    }
                } // cfg!(feature = "step_tracer")

                // Handle halt reason (upstream physical_core.cpp:100-144).
                use crate::arm::arm_interface::HaltReason;
                let supervisor_call = hr.contains(HaltReason::SUPERVISOR_CALL);
                let interrupt = hr.contains(HaltReason::BREAK_LOOP);

                if supervisor_call {
                    // Upstream: Svc::Call(system, interface->GetSvcNumber())
                    // (svc.cpp:4425-4441)
                    //
                    // 1. SaveSvcArguments: read R0-R7 from JIT
                    // 2. Call32/Call64: dispatch SVC, modifies args in-place
                    // 3. LoadSvcArguments: write modified args back to JIT
                    let svc_number = jit_ref.get_svc_number();
                    log::debug!("SVC #{:#x} dispatching", svc_number);

                    let system_ref = kernel.system();
                    if !system_ref.is_null() {
                        let system = system_ref.get();
                        let is_64bit = {
                            let thread = thread_arc.lock().unwrap();
                            thread.parent.as_ref()
                                .and_then(|p| p.upgrade())
                                .map(|p| p.lock().unwrap().is_64bit())
                                .unwrap_or(false)
                        };

                        // Upstream: kernel.CurrentPhysicalCore().SaveSvcArguments(process, args)
                        let mut svc_args = [0u64; 8];
                        jit_ref.get_svc_arguments(&mut svc_args);

                        // Dispatch the SVC.
                        crate::hle::kernel::svc_dispatch::call(
                            system, svc_number, is_64bit, &mut svc_args,
                        );

                        // Upstream: kernel.CurrentPhysicalCore().LoadSvcArguments(process, args)
                        jit_ref.set_svc_arguments(&svc_args);
                    } else {
                        log::warn!("SVC #{:#x}: no System reference available", svc_number);
                    }

                    // SVC handled, return to MultiCoreRunGuestThread loop.
                    break;
                }

                if interrupt {
                    break;
                }

                // Any other non-empty halt reason — break.
                if !hr.is_empty() {
                    break;
                }

                // hr.is_empty() → quantum expired naturally (get_ticks_remaining() hit 0).
                // Upstream multicore: the JIT runs until a hardware timer fires Interrupt(),
                // which sets BreakLoop. Since ruzu has no hardware preemption timer yet,
                // we advance CoreTiming here (fires pending kernel timer events) and reset
                // the quantum so the JIT can continue executing the next slice.
                if let Some(ct) = kernel.core_timing() {
                    let mut ct_lock = ct.lock().unwrap();
                    let _ = ct_lock.advance();
                    ct_lock.reset_ticks();
                }
                // Continue the inner JIT loop — quantum is fresh, let interrupt check decide.
            }
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
        // Upstream: kernel.CurrentScheduler()->OnThreadStart();
        // OnThreadStart calls EnableDispatch — ensure dispatch_count is 1 first.
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
            Self::shutdown_if_requested(kernel);
            let physical_core = kernel.current_physical_core();
            if !physical_core.is_interrupted() {
                physical_core.idle();
            }

            Self::handle_interrupt(kernel);
            Self::shutdown_if_requested(kernel);
            // Upstream: RequestScheduleOnInterrupt → ScheduleOnInterrupt → Schedule →
            // ScheduleImpl (fiber yield).  We do the fiber switch here, outside of any
            // Mutex lock, to avoid the scheduler Mutex being held across the yield.
            Self::reschedule_current_core_raw(kernel);
        }
    }

    // =========================================================================
    // SingleCore guest/idle thread loops
    // =========================================================================

    /// Public entry point for single-core guest thread, called from the
    /// guest thread fiber closure created in `core.rs`.
    ///
    /// Upstream: `CpuManager::GuestThreadFunction()` → `SingleCoreRunGuestThread()`.
    /// In upstream, CpuManager is a class with member fields (core_timing,
    /// current_core, idle_count). Here we access core_timing via kernel and
    /// use local state for current_core/idle_count.
    pub fn single_core_run_guest_thread_entry(kernel: &KernelCore) {
        if let Some(core_timing) = kernel.core_timing() {
            let core_timing = core_timing.clone();
            let current_core = AtomicUsize::new(0);
            let mut idle_count: usize = 0;
            Self::single_core_run_guest_thread(kernel, &core_timing, &current_core, &mut idle_count);
        } else {
            log::error!("SingleCoreRunGuestThread: no CoreTiming available");
        }
    }

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
            Self::shutdown_if_requested(kernel);
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
            Self::shutdown_if_requested(kernel);
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
            Self::shutdown_if_requested(kernel);
            Self::preempt_single_core_inner(kernel, core_timing, current_core, idle_count, false);
            core_timing.lock().unwrap().add_ticks(1000);
            *idle_count += 1;
            Self::handle_interrupt(kernel);
            Self::shutdown_if_requested(kernel);
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
                    log::info!("RunThread core {}: yielding from host fiber to main thread fiber", core);
                    Fiber::yield_to(Arc::downgrade(&host_ctx), &thread_host_ctx);
                    log::info!("RunThread core {}: returned from main thread fiber", core);
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
