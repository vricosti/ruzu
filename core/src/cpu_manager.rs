//! Port of zuyu/src/core/cpu_manager.h and zuyu/src/core/cpu_manager.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! Manages the CPU threads. Creates a thread per core and dispatches execution.

use crate::core_timing::CoreTiming;
use crate::hardware_properties;
use crate::hle::kernel::k_interrupt_manager;
use crate::hle::kernel::k_process::ProcessLock;
use crate::hle::kernel::k_thread::KThreadLock;
use crate::hle::kernel::kernel::KernelCore;
use common::fiber::Fiber;
use common::thread::Barrier;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

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
static TID17_HALT_SAMPLE_COUNT: AtomicU32 = AtomicU32::new(0);
static THREAD_PROBE_HALT_COUNT: AtomicU32 = AtomicU32::new(0);
static PC_PROBE_HALT_COUNT: AtomicU32 = AtomicU32::new(0);
static RUNNING_GUEST_THREADS: OnceLock<Mutex<HashMap<u64, usize>>> = OnceLock::new();
static SERIALIZED_JIT_RUN: OnceLock<Mutex<()>> = OnceLock::new();
static THREAD_SCHED_MISMATCH_COUNT: AtomicU64 = AtomicU64::new(0);
static JIT_RUN_TRACE_COUNT: AtomicU64 = AtomicU64::new(0);

struct RunningGuestThreadGuard {
    enabled: bool,
    thread_id: u64,
    core_index: usize,
}

impl RunningGuestThreadGuard {
    fn new(enabled: bool, thread_id: u64, core_index: usize, thread_ptr: usize) -> Self {
        if enabled {
            let mut running = RUNNING_GUEST_THREADS
                .get_or_init(|| Mutex::new(HashMap::new()))
                .lock()
                .unwrap();
            if let Some(owner_core) = running.insert(thread_id, core_index) {
                eprintln!(
                    "[THREAD_CONCURRENCY] tid={} ptr=0x{:X} already_running_core={} entering_core={}",
                    thread_id, thread_ptr, owner_core, core_index
                );
            }
        }
        Self {
            enabled,
            thread_id,
            core_index,
        }
    }
}

impl Drop for RunningGuestThreadGuard {
    fn drop(&mut self) {
        if !self.enabled {
            return;
        }
        if let Some(map) = RUNNING_GUEST_THREADS.get() {
            let mut running = map.lock().unwrap();
            if running.get(&self.thread_id) == Some(&self.core_index) {
                running.remove(&self.thread_id);
            }
        }
    }
}

fn parse_hex_env_u64(name: &str) -> Option<u64> {
    let value = std::env::var(name).ok()?;
    let trimmed = value.trim();
    let hex = trimmed
        .strip_prefix("0x")
        .or_else(|| trimmed.strip_prefix("0X"))
        .unwrap_or(trimmed);
    u64::from_str_radix(hex, 16).ok()
}

/// Returns true when `RUZU_TRACE_CORE_DISPATCH` is set AND the given core
/// index is in the (optional) comma-separated allow list. `RUZU_TRACE_CORE_DISPATCH=1`
/// (or any non-numeric value) enables all cores. `RUZU_TRACE_CORE_DISPATCH=1,2`
/// enables cores 1 and 2 only.
fn should_trace_core_dispatch(core_index: usize) -> bool {
    let Some(raw) = std::env::var_os("RUZU_TRACE_CORE_DISPATCH") else {
        return false;
    };
    let raw = raw.to_string_lossy();
    let raw = raw.trim();
    // "1" enables all cores (common shorthand). Empty also enables all.
    if raw.is_empty() || raw == "1" || raw.eq_ignore_ascii_case("all") {
        return true;
    }
    // Otherwise treat as comma-separated allow list of core indices.
    raw.split(',')
        .filter_map(|s| s.trim().parse::<usize>().ok())
        .any(|c| c == core_index)
}

/// Emit one `[CORE_DISPATCH]` line tagged with the dispatch-loop phase and the
/// guest thread the loop is currently running. Lightweight (single
/// `eprintln!`); gated by `RUZU_TRACE_CORE_DISPATCH`. Used to diagnose the
/// MK8D wedge where tid=99's wakes take ~1 second despite end_wait firing
/// promptly — we want to see, per core, when the JIT inner-loop exits and
/// when reschedule actually swaps threads.
fn trace_core_dispatch(
    core_index: usize,
    phase: &str,
    thread: &Arc<crate::hle::kernel::k_thread::KThreadLock>,
) {
    if !should_trace_core_dispatch(core_index) {
        return;
    }
    let tid = thread.lock().ok().map(|t| t.get_thread_id()).unwrap_or(0);
    let t = crate::hle::kernel::trace_format::elapsed_secs();
    eprintln!(
        "[{:>10.6}] [CORE_DISPATCH] core={} phase={} fiber_tid={}",
        t, core_index, phase, tid
    );
}

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
        if self.is_multicore {
            if let Ok(raw_core_count) = std::env::var("RUZU_CORE_COUNT") {
                if let Ok(core_count) = raw_core_count.parse::<usize>() {
                    if (1..=hardware_properties::NUM_CPU_CORES as usize).contains(&core_count) {
                        self.num_cores = core_count;
                        log::warn!("RUZU_CORE_COUNT={} -> limiting CPU host cores", core_count);
                    }
                }
            }
        }

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
            let barrier = self
                .gpu_barrier
                .clone()
                .expect("gpu_barrier must be set before spawn_threads");
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
                .name(if is_mc {
                    format!("CPUCore_{}", core)
                } else {
                    "CPUThread".to_string()
                })
                .spawn(move || {
                    // Safety: kernel_ptr is valid for the lifetime of this thread.
                    let kernel = unsafe { &*(kp as *const KernelCore) };
                    Self::run_thread_on_core(
                        kernel,
                        core,
                        is_mc,
                        is_ag,
                        &barrier,
                        &stop,
                        &host_ctx_slot,
                    );
                })
                .expect("Failed to spawn CPU thread");

            self.core_data[core].host_thread = Some(handle);
        }

        // RUZU_FORCE_SAMPLE_MS=N — spawn a debug sampler thread that calls
        // PhysicalCore::interrupt() on every core every N ms. Forces the JIT
        // to halt periodically (returning HaltReason::EXTERNAL_HALT), which
        // surfaces a SPIN trace sample via `cpu_manager.rs:686-718`. Used to
        // identify the spin location during a CPU-bound JIT run that makes
        // 0 SVCs (block-cache misses, ticks check, and SVCs all fail to fire).
        if let Ok(s) = std::env::var("RUZU_FORCE_SAMPLE_MS") {
            if let Ok(period_ms) = s.parse::<u64>() {
                if period_ms >= 1 {
                    let stop = self.stop_requested.clone();
                    let kp = kernel_ptr as usize;
                    let num = self.num_cores;
                    let _ = std::thread::Builder::new()
                        .name("RUZU_FORCE_SAMPLE".into())
                        .spawn(move || {
                            let kernel = unsafe { &*(kp as *const KernelCore) };
                            log::warn!(
                                "RUZU_FORCE_SAMPLE: sampler thread started, period={}ms cores={}",
                                period_ms,
                                num
                            );
                            while !stop.load(Ordering::Relaxed) {
                                std::thread::sleep(std::time::Duration::from_millis(period_ms));
                                for c in 0..num {
                                    if let Some(pc) = kernel.physical_core(c) {
                                        pc.interrupt();
                                    }
                                }
                            }
                        });
                }
            }
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
        core_timing: &Arc<CoreTiming>,
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
        core_timing: &Arc<CoreTiming>,
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

    /// Shutdown thread function entry point.
    ///
    /// Upstream: `CpuManager::ShutdownThreadFunction()` (cpu_manager.cpp:58-60).
    /// Calls `ShutdownThread()`. Used as the fiber entry for shutdown threads
    /// created by `InitializeShutdownThreads()`.
    pub fn shutdown_thread_function(kernel: &KernelCore) {
        let system_ref = kernel.system();
        if system_ref.is_null() {
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

    // (trace_core_dispatch is a free function defined below at module scope)

    /// Multicore guest thread loop.
    ///
    /// Upstream: `CpuManager::MultiCoreRunGuestThread()` (cpu_manager.cpp:73-88).
    /// Runs on the guest fiber. Calls PhysicalCore::RunThread in a loop,
    /// handling interrupts between runs.
    pub fn multi_core_run_guest_thread(kernel: &KernelCore) {
        // Upstream: auto* thread = Kernel::GetCurrentThreadPointer(kernel);
        let thread_arc = match super::hle::kernel::kernel::get_current_thread_pointer() {
            Some(thread) => thread,
            None => return,
        };
        // kernel.CurrentScheduler()->OnThreadStart();
        // Upstream: kernel.CurrentScheduler()->OnThreadStart();
        // OnThreadStart just calls thread.EnableDispatch().
        // We can't lock the scheduler here because it's still locked by
        // guest_activate → scheduler.activate() → schedule_impl_fiber
        // (the lock is on the suspended kernel main thread's stack).
        // Call EnableDispatch directly matching what OnThreadStart does.
        {
            let mut t = thread_arc.lock().unwrap();
            if t.get_disable_dispatch_count() > 0 {
                t.enable_dispatch();
            }
        }

        // Cache the owner process and per-core JIT pointers once, outside the
        // inner loop. Upstream reads these via raw pointers (no locking);
        // previously ruzu locked the process Mutex every inner-loop iteration,
        // serializing all 4 CPU cores on a single Mutex and collapsing guest
        // emulation onto one host thread.
        //
        // Safety: the Arc<ProcessLock> keeps the process alive as long as
        // this guest thread exists. `arm_interfaces` entries are populated once
        // during process init and never change afterwards, so holding raw
        // pointers into them is sound for the thread's lifetime.
        let (parent_arc, is_64bit, cached_jits): (
            _,
            bool,
            [Option<*mut Box<dyn crate::arm::arm_interface::ArmInterface>>;
                hardware_properties::NUM_CPU_CORES as usize],
        ) = {
            let parent_weak = {
                let thread = thread_arc.lock().unwrap();
                match thread.parent.as_ref() {
                    Some(p) => p.clone(),
                    None => return,
                }
            };
            let parent_arc = match parent_weak.upgrade() {
                Some(p) => p,
                None => return,
            };
            let mut process = parent_arc.lock().unwrap();
            let is_64bit = process.is_64bit();
            let mut jits: [Option<*mut Box<dyn crate::arm::arm_interface::ArmInterface>>;
                hardware_properties::NUM_CPU_CORES as usize] =
                [const { None }; hardware_properties::NUM_CPU_CORES as usize];
            for i in 0..hardware_properties::NUM_CPU_CORES as usize {
                if let Some(jit) = process.get_arm_interface_mut(i) {
                    jits[i] = Some(
                        jit as *mut _ as *mut Box<dyn crate::arm::arm_interface::ArmInterface>,
                    );
                }
            }
            drop(process);
            (parent_arc, is_64bit, jits)
        };
        loop {
            Self::shutdown_if_requested(kernel);
            let mut physical_core = kernel.current_physical_core();
            // Clear any stale interrupt before entering the JIT loop.
            // The preemption timer may have fired while we were in the scheduler.
            if physical_core.is_interrupted() {
                trace_core_dispatch(
                    physical_core.core_index(),
                    "pre_inner stale_interrupt",
                    &thread_arc,
                );
                Self::handle_interrupt(kernel);
            }
            trace_core_dispatch(physical_core.core_index(), "enter_inner_loop", &thread_arc);
            while !physical_core.is_interrupted() {
                Self::run_guest_thread_once(
                    kernel,
                    physical_core,
                    &parent_arc,
                    &thread_arc,
                    is_64bit,
                    &cached_jits,
                );
                // Upstream: physical_core = &kernel.CurrentPhysicalCore();
                physical_core = kernel.current_physical_core();
            }
            trace_core_dispatch(physical_core.core_index(), "exit_inner_loop", &thread_arc);

            Self::handle_interrupt(kernel);
            trace_core_dispatch(
                physical_core.core_index(),
                "after_handle_interrupt",
                &thread_arc,
            );
            Self::shutdown_if_requested(kernel);
            // Upstream: RequestScheduleOnInterrupt → ScheduleOnInterrupt → fiber yield.
            // Done outside any Mutex lock to avoid deadlock (see reschedule_current_core_raw).
            Self::reschedule_current_core_raw(kernel);
            trace_core_dispatch(physical_core.core_index(), "after_reschedule", &thread_arc);
        }
    }

    /// Execute one iteration of guest code via the JIT on the given physical core.
    ///
    /// Upstream: `PhysicalCore::RunThread(KThread*)` (physical_core.cpp:22-146).
    /// Gets the ARM interface from thread->GetOwnerProcess()->GetArmInterface(core_index),
    /// then runs the JIT until an interrupt, SVC, or halt.
    ///
    /// `cached_jits` is populated once by the caller (multi_core_run_guest_thread)
    /// to avoid locking the process Mutex on every inner-loop iteration, which
    /// was serializing all 4 guest cores onto one host thread.
    fn run_guest_thread_once(
        kernel: &KernelCore,
        physical_core: &super::hle::kernel::physical_core::PhysicalCore,
        process: &Arc<ProcessLock>,
        thread_arc: &Arc<KThreadLock>,
        is_64bit: bool,
        cached_jits: &[Option<*mut Box<dyn crate::arm::arm_interface::ArmInterface>>;
             hardware_properties::NUM_CPU_CORES as usize],
    ) {
        use crate::arm::arm_interface::KThread as OpaqueKThread;

        let core_index = physical_core.core_index();
        let jit = match cached_jits.get(core_index).copied().flatten() {
            Some(j) => j,
            None => {
                physical_core.idle();
                return;
            }
        };
        let scheduler = match kernel.current_scheduler() {
            Some(scheduler) => scheduler.clone(),
            None => return,
        };

        // Safety: The JIT is owned by the process which lives as long as the thread.
        // We hold the raw pointer only for the duration of this call.
        // The thread_arc KThreadLock is cast to OpaqueKThread for the JIT interface.
        let thread_ptr = {
            let mut t = thread_arc.lock().unwrap();
            &mut *t as *mut super::hle::kernel::k_thread::KThread
        };
        let trace_thread_concurrency = std::env::var_os("RUZU_TRACE_THREAD_CONCURRENCY").is_some();
        let (thread_id_for_guard, thread_current_core, thread_active_core) =
            if trace_thread_concurrency {
                let thread = thread_arc.lock().unwrap();
                (
                    thread.get_thread_id(),
                    thread.get_current_core(),
                    thread.get_active_core(),
                )
            } else {
                (0, 0, 0)
            };
        if trace_thread_concurrency {
            let scheduler_current = scheduler.lock().unwrap().get_scheduler_current_thread_id();
            let mismatch = scheduler_current != Some(thread_id_for_guard)
                || thread_current_core != core_index as i32
                || (thread_active_core >= 0 && thread_active_core != core_index as i32);
            if mismatch {
                let n = THREAD_SCHED_MISMATCH_COUNT.fetch_add(1, Ordering::Relaxed);
                if n < 200 || n % 1000 == 0 {
                    eprintln!(
                        "[THREAD_SCHED_MISMATCH] n={} tid={} ptr=0x{:X} entering_core={} scheduler_current={:?} thread_current_core={} thread_active_core={}",
                        n,
                        thread_id_for_guard,
                        thread_ptr as usize,
                        core_index,
                        scheduler_current,
                        thread_current_core,
                        thread_active_core
                    );
                }
            }
        } else {
        }
        let _running_thread_guard = RunningGuestThreadGuard::new(
            trace_thread_concurrency,
            thread_id_for_guard,
            core_index,
            thread_ptr as usize,
        );

        unsafe {
            let jit_ref = &mut **jit;
            let opaque = &mut *(thread_ptr as *mut OpaqueKThread);
            use crate::arm::arm_interface::HaltReason;
            let mut thread_context = crate::arm::arm_interface::ThreadContext::default();

            {
                let thread = thread_arc.lock().unwrap();
                if thread.has_dpc() && thread.is_termination_requested() {
                    return;
                }
            }

            if physical_core.is_interrupted() {
                return;
            }

            let jit_ptr = jit_ref as *mut dyn crate::arm::arm_interface::ArmInterface;
            if !physical_core.enter_running(jit_ptr, thread_ptr) {
                return;
            }
            if std::env::var_os("RUZU_TRACE_JIT_RUNS").is_some() {
                let n = JIT_RUN_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
                if n < 512 || n.is_multiple_of(1000) {
                    jit_ref.get_context(&mut thread_context);
                    let thread = thread_arc.lock().unwrap();
                    let scheduler_current =
                        scheduler.lock().unwrap().get_scheduler_current_thread_id();
                    eprintln!(
                        "[JIT_RUN #{}] core={} tid={} scheduler_current={:?} thread_current_core={} thread_active_core={} pc=0x{:016X} lr=0x{:016X} sp=0x{:016X}",
                        n,
                        core_index,
                        thread.get_thread_id(),
                        scheduler_current,
                        thread.get_current_core(),
                        thread.get_active_core(),
                        thread_context.pc,
                        thread_context.lr,
                        thread_context.sp,
                    );
                }
            }
            let _serialized_jit_guard = if std::env::var_os("RUZU_SERIALIZE_JIT").is_some() {
                Some(
                    SERIALIZED_JIT_RUN
                        .get_or_init(|| Mutex::new(()))
                        .lock()
                        .unwrap(),
                )
            } else {
                None
            };
            let event = physical_core.run_thread(jit_ref, opaque);
            drop(_serialized_jit_guard);
            physical_core.exit_running(jit_ptr, thread_ptr);

            match event {
                crate::hle::kernel::physical_core::PhysicalCoreExecutionEvent::SupervisorCall {
                    svc_num,
                    mut svc_args,
                } => {
                    let (current_thread_id, current_thread_priority) = {
                        let thread = thread_arc.lock().unwrap();
                        (thread.get_thread_id(), thread.get_priority())
                    };
                    if current_thread_id == 17 || current_thread_id >= 18 {
                        let mut tc = crate::arm::arm_interface::ThreadContext::default();
                        jit_ref.get_context(&mut tc);
                        log::trace!(
                            "multi_core_run_guest_thread: host={} tid={} core={} svc=0x{:x} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X} arg0=0x{:X} arg1=0x{:X} arg2=0x{:X}",
                            std::thread::current().name().unwrap_or("?"),
                            current_thread_id,
                            core_index,
                            svc_num,
                            tc.pc,
                            tc.lr,
                            tc.sp,
                            svc_args[0],
                            svc_args[1],
                            svc_args[2],
                        );
                        if svc_num == 0x1c {
                            log::trace!(
                                "multi_core_run_guest_thread: tid={} WaitProcessWideKeyAtomic args addr=0x{:X} key=0x{:X} tag=0x{:08X} timeout_lo=0x{:08X} timeout_hi=0x{:08X}",
                                current_thread_id,
                                svc_args[0] as u32,
                                svc_args[1] as u32,
                                svc_args[2] as u32,
                                svc_args[3] as u32,
                                svc_args[4] as u32,
                            );
                        } else if svc_num == 0x1d {
                            log::trace!(
                                "multi_core_run_guest_thread: tid={} SignalProcessWideKey args key=0x{:X} count={}",
                                current_thread_id,
                                svc_args[0] as u32,
                                svc_args[1] as i32,
                            );
                        }
                    }
                    // Record guest PC + LR + SP at SVC entry for SIGUSR1
                    // dumper. LR points back into the caller (usually the
                    // game's code that invoked the nnSdk SVC stub), which
                    // is more diagnostic than PC when a spin loop calls the
                    // same SVC millions of times. SP lets the dumper walk
                    // stack frames above the stub to find the game-level
                    // loop caller.
                    {
                        let mut tc_pc = crate::arm::arm_interface::ThreadContext::default();
                        jit_ref.get_context(&mut tc_pc);
                        crate::hle::kernel::kernel::record_guest_full(
                            core_index, tc_pc.pc, tc_pc.lr, tc_pc.sp, &tc_pc.r,
                        );
                        crate::hle::kernel::svc_dispatch::record_svc_ring(
                            current_thread_id,
                            current_thread_priority,
                            core_index as i32,
                            svc_num,
                            tc_pc.pc,
                            tc_pc.lr,
                            tc_pc.sp,
                            &svc_args,
                        );
                    }
                    let system_ref = kernel.system();
                    if !system_ref.is_null() {
                        let system = system_ref.get();
                        let continue_thread = physical_core.dispatch_supervisor_call(
                            jit_ref,
                            &mut thread_context,
                            &scheduler,
                            process,
                            thread_arc,
                            svc_num,
                            0,
                            is_64bit,
                            &mut svc_args,
                            system,
                        );
                        if !continue_thread {
                            Self::reschedule_current_core_raw(kernel);
                            return;
                        }
                        let (switched_scheduler_thread, needs_scheduling) = {
                            let scheduler = scheduler.lock().unwrap();
                            (
                                scheduler.get_scheduler_current_thread_id()
                                    != Some(current_thread_id),
                                scheduler.needs_scheduling(),
                            )
                        };
                        let current_thread_blocked = {
                            let thread = thread_arc.lock().unwrap();
                            thread.get_raw_state()
                                != crate::hle::kernel::k_thread::ThreadState::RUNNABLE
                        };
                        if current_thread_blocked {
                            log::trace!(
                                "multi_core_run_guest_thread: tid={} core={} svc=0x{:x} reschedule after SVC switched={} blocked={} needs_sched={}",
                                current_thread_id,
                                core_index,
                                svc_num,
                                switched_scheduler_thread,
                                current_thread_blocked,
                                needs_scheduling,
                            );
                            Self::reschedule_current_core_raw(kernel);
                            return;
                        }
                        if switched_scheduler_thread || needs_scheduling {
                            log::trace!(
                                "multi_core_run_guest_thread: tid={} core={} svc=0x{:x} reschedule after SVC switched={} blocked={} needs_sched={}",
                                current_thread_id,
                                core_index,
                                svc_num,
                                switched_scheduler_thread,
                                current_thread_blocked,
                                needs_scheduling,
                            );
                            if std::env::var_os("RUZU_DISABLE_POST_SVC_RESCHEDULE").is_none() {
                                Self::reschedule_current_core_raw(kernel);
                                return;
                            }
                        }
                        log::trace!(
                            "multi_core_run_guest_thread: tid={} core={} svc=0x{:x} returned from svc_dispatch r0=0x{:X} r1=0x{:X}",
                            current_thread_id,
                            core_index,
                            svc_num,
                            svc_args[0],
                            svc_args[1],
                        );
                        return;
                    }
                }
                crate::hle::kernel::physical_core::PhysicalCoreExecutionEvent::Halted(
                    halt_reason,
                ) => {
                    // Refresh the SIGUSR1-dumper PC+LR+SP snapshot. Halts
                    // fire on preemption interrupts (~10ms), so even a
                    // guest-code spin loop with no SVCs gets its PC/LR/SP
                    // surfaced via `kernel::{GUEST_PC,GUEST_LR,GUEST_SP}
                    // [core_index]` after each preempt.
                    let _spin_pc = {
                        let mut tc_pc = crate::arm::arm_interface::ThreadContext::default();
                        jit_ref.get_context(&mut tc_pc);
                        crate::hle::kernel::kernel::record_guest_full(
                            core_index, tc_pc.pc, tc_pc.lr, tc_pc.sp, &tc_pc.r,
                        );
                        tc_pc
                    };
                    // RUZU_SPIN_TRACE=1 — log live PC/LR/key registers on
                    // every halt for the configured tid (default 73).
                    // Bypasses the preemption-thread SIGUSR1 dump path which
                    // gets starved when the JIT thread saturates a host core.
                    if std::env::var_os("RUZU_SPIN_TRACE").is_some() {
                        let target_tid: u64 = std::env::var("RUZU_SPIN_TRACE_TID")
                            .ok()
                            .and_then(|s| s.parse().ok())
                            .unwrap_or(73);
                        let cur_tid = thread_arc.lock().unwrap().get_thread_id() as u64;
                        if cur_tid == target_tid {
                            static SPIN_COUNT: AtomicU64 = AtomicU64::new(0);
                            let n = SPIN_COUNT.fetch_add(1, Ordering::Relaxed);
                            // Throttle: log first 200, then every 1000th.
                            if n < 200 || n % 1000 == 0 {
                                eprintln!(
                                    "[SPIN] n={} tid={} pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} x21=0x{:X} x22=0x{:X} x7=0x{:X} x18=0x{:X} x20=0x{:X} halt={:?}",
                                    n, cur_tid,
                                    _spin_pc.pc, _spin_pc.lr, _spin_pc.sp,
                                    _spin_pc.r[21], _spin_pc.r[22], _spin_pc.r[7],
                                    _spin_pc.r[18], _spin_pc.r[20],
                                    halt_reason
                                );
                            }
                        }
                    }
                    let current_thread_id = thread_arc.lock().unwrap().get_thread_id();
                    let interrupt = halt_reason.contains(HaltReason::BREAK_LOOP);
                    let data_abort = halt_reason.contains(HaltReason::DATA_ABORT);
                    let prefetch_abort = halt_reason.contains(HaltReason::PREFETCH_ABORT);
                    let breakpoint = halt_reason.contains(HaltReason::INSTRUCTION_BREAKPOINT);
                    if current_thread_id == 17
                        && TID17_HALT_SAMPLE_COUNT.fetch_add(1, Ordering::Relaxed) < 500
                    {
                        let mut tc = crate::arm::arm_interface::ThreadContext::default();
                        jit_ref.get_context(&mut tc);
                        log::trace!(
                            "multi_core_run_guest_thread: tid=17 core={} halted={:?} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X} r4=0x{:X} r7=0x{:X} r0=0x{:X} r1=0x{:X}",
                            core_index,
                            halt_reason,
                            tc.pc,
                            tc.lr,
                            tc.sp,
                            tc.r[4],
                            tc.r[7],
                            tc.r[0],
                            tc.r[1],
                        );
                    }
                    if std::env::var_os("RUZU_LOG_THREAD_PROBE").is_some()
                        && THREAD_PROBE_HALT_COUNT.fetch_add(1, Ordering::Relaxed) < 400
                    {
                        let mut tc = crate::arm::arm_interface::ThreadContext::default();
                        jit_ref.get_context(&mut tc);
                        log::info!(
                            "thread_probe: tid={} core={} halted={:?} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X} interrupt={} data_abort={} prefetch_abort={} breakpoint={}",
                            current_thread_id,
                            core_index,
                            halt_reason,
                            tc.pc,
                            tc.lr,
                            tc.sp,
                            interrupt,
                            data_abort,
                            prefetch_abort,
                            breakpoint,
                        );
                    }
                    let zero_pc_break_loop = interrupt && _spin_pc.pc == 0;

                    // Upstream: if (breakpoint || prefetch_abort) {
                    //     thread->RequestSuspend(SuspendType::Debug); return; }
                    // Upstream then handles data_abort the same way. The Rust
                    // Dynarmic bridge can currently surface a null-PC fetch as
                    // BreakLoop instead of PrefetchAbort after an interrupt;
                    // treat that as the same non-continuable state so the
                    // guest thread does not spin forever at PC=0.
                    // Without this, the thread re-executes the faulting PC forever.
                    if breakpoint || prefetch_abort || data_abort || zero_pc_break_loop {
                        if breakpoint {
                            jit_ref.rewind_breakpoint_instruction();
                        }
                        {
                            let mut tc = crate::arm::arm_interface::ThreadContext::default();
                            jit_ref.get_context(&mut tc);
                            let reason = if zero_pc_break_loop {
                                "BreakLoopNullPc"
                            } else if data_abort {
                                "DataAbort"
                            } else if prefetch_abort {
                                "PrefetchAbort"
                            } else {
                                "Breakpoint"
                            };
                            if zero_pc_break_loop
                                && std::env::var_os("RUZU_DUMP_NULL_PC_CONTEXT").is_some()
                            {
                                let system_ref = kernel.system();
                                if !system_ref.is_null() {
                                    if let Some(memory) = system_ref.get().memory_shared() {
                                        use std::fmt::Write;

                                        let mem = memory.lock().unwrap();
                                        let mut regs = String::new();
                                        for i in 0..13 {
                                            let _ =
                                                write!(regs, " r{}=0x{:08X}", i, tc.r[i] as u32);
                                        }
                                        log::error!(
                                            "[NULL_PC_CONTEXT] tid={} core={} pstate=0x{:08X} lr=0x{:08X} sp=0x{:08X}{}",
                                            current_thread_id,
                                            core_index,
                                            tc.pstate,
                                            tc.lr as u32,
                                            tc.sp as u32,
                                            regs
                                        );

                                        let mut stack = String::new();
                                        for i in 0..32u64 {
                                            if i % 8 == 0 {
                                                let _ = write!(
                                                    stack,
                                                    "\n  0x{:08X}:",
                                                    tc.sp.wrapping_add(i * 4) as u32
                                                );
                                            }
                                            let word = mem.read_32(tc.sp.wrapping_add(i * 4));
                                            let _ = write!(stack, " {:08X}", word);
                                        }
                                        log::error!("[NULL_PC_STACK]{}", stack);

                                        for &reg_index in
                                            &[1usize, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
                                        {
                                            let ptr = tc.r[reg_index];
                                            if ptr < 0x10000 {
                                                continue;
                                            }
                                            let mut words = String::new();
                                            for i in 0..8u64 {
                                                let word = mem.read_32(ptr.wrapping_add(i * 4));
                                                let _ = write!(words, " {:08X}", word);
                                            }
                                            log::error!(
                                                "[NULL_PC_PTR] r{}=0x{:08X}:{}",
                                                reg_index,
                                                ptr as u32,
                                                words
                                            );
                                        }

                                        if (tc.lr as u32) == 0x00A325B8 {
                                            let trampoline_ptr_slot = mem.read_32(0x00ED6DB0);
                                            let trampoline_target_slot =
                                                mem.read_32(trampoline_ptr_slot as u64);
                                            let mut slot_words = String::new();
                                            for i in 0..8u64 {
                                                let word = mem.read_32(
                                                    (trampoline_ptr_slot as u64)
                                                        .wrapping_add(i * 4),
                                                );
                                                let _ = write!(slot_words, " {:08X}", word);
                                            }
                                            log::error!(
                                                "[NULL_PC_A325B8] ed6db0=0x{:08X} [ed6db0]=0x{:08X} slot_words:{}",
                                                trampoline_ptr_slot,
                                                trampoline_target_slot,
                                                slot_words
                                            );

                                            let mut target_words = String::new();
                                            for i in 0..8u64 {
                                                let word = mem.read_32(
                                                    (trampoline_target_slot as u64)
                                                        .wrapping_add(i * 4),
                                                );
                                                let _ = write!(target_words, " {:08X}", word);
                                            }
                                            log::error!(
                                                "[NULL_PC_A325B8_TARGET] target=0x{:08X} words:{}",
                                                trampoline_target_slot,
                                                target_words
                                            );

                                            let wrapper_arg = tc.r[4] as u32;
                                            let first = mem.read_32(wrapper_arg as u64);
                                            let second = mem.read_32(first as u64);
                                            let mut first_words = String::new();
                                            for i in 0..8u64 {
                                                let word =
                                                    mem.read_32((first as u64).wrapping_add(i * 4));
                                                let _ = write!(first_words, " {:08X}", word);
                                            }
                                            log::error!(
                                                "[NULL_PC_A325B8_ARG] r4=0x{:08X} [r4]=0x{:08X} [[r4]]=0x{:08X} first_words:{}",
                                                wrapper_arg,
                                                first,
                                                second,
                                                first_words
                                            );
                                        }
                                    }
                                }
                            }
                            log::error!(
                                "multi_core_run_guest_thread: tid={} core={} {} at pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} r0=0x{:016X} r28=0x{:016X} — suspending thread",
                                current_thread_id,
                                core_index,
                                reason,
                                tc.pc,
                                tc.lr,
                                tc.sp,
                                tc.r[0],
                                tc.r[28],
                            );
                        }
                        thread_arc
                            .lock()
                            .unwrap()
                            .request_suspend(crate::hle::kernel::k_thread::SuspendType::Debug);
                        Self::reschedule_current_core_raw(kernel);
                        return;
                    }
                    if let Some(probe_pc) = parse_hex_env_u64("RUZU_PC_PROBE") {
                        let mut tc = crate::arm::arm_interface::ThreadContext::default();
                        jit_ref.get_context(&mut tc);
                        if tc.pc == probe_pc
                            && PC_PROBE_HALT_COUNT.fetch_add(1, Ordering::Relaxed) < 120
                        {
                            let (op_m2, op_m1, op_0, op_p1, op_p2, stack_words) = {
                                let system_ref = kernel.system();
                                if system_ref.is_null() {
                                    (0, 0, 0, 0, 0, [0; 8])
                                } else if let Some(memory) = system_ref.get().memory_shared() {
                                    let mem = memory.lock().unwrap();
                                    (
                                        mem.read_32(tc.pc.wrapping_sub(8)),
                                        mem.read_32(tc.pc.wrapping_sub(4)),
                                        mem.read_32(tc.pc),
                                        mem.read_32(tc.pc + 4),
                                        mem.read_32(tc.pc + 8),
                                        [
                                            mem.read_32(tc.sp),
                                            mem.read_32(tc.sp.wrapping_add(4)),
                                            mem.read_32(tc.sp.wrapping_add(8)),
                                            mem.read_32(tc.sp.wrapping_add(12)),
                                            mem.read_32(tc.sp.wrapping_add(16)),
                                            mem.read_32(tc.sp.wrapping_add(20)),
                                            mem.read_32(tc.sp.wrapping_add(24)),
                                            mem.read_32(tc.sp.wrapping_add(28)),
                                        ],
                                    )
                                } else {
                                    (0, 0, 0, 0, 0, [0; 8])
                                }
                            };
                            log::info!(
                                "pc_probe: tid={} core={} halted={:?} pc=0x{:08X} pstate=0x{:08X} op_m2=0x{:08X} op_m1=0x{:08X} op_0=0x{:08X} op_p1=0x{:08X} op_p2=0x{:08X} lr=0x{:08X} sp=0x{:08X} r0=0x{:08X} r1=0x{:08X} r2=0x{:08X} r3=0x{:08X} r4=0x{:08X} r5=0x{:08X} r6=0x{:08X} r7=0x{:08X} r8=0x{:08X} r9=0x{:08X} r10=0x{:08X} r11=0x{:08X} r12=0x{:08X}",
                                current_thread_id,
                                core_index,
                                halt_reason,
                                tc.pc as u32,
                                tc.pstate,
                                op_m2,
                                op_m1,
                                op_0,
                                op_p1,
                                op_p2,
                                tc.lr as u32,
                                tc.sp as u32,
                                tc.r[0] as u32,
                                tc.r[1] as u32,
                                tc.r[2] as u32,
                                tc.r[3] as u32,
                                tc.r[4] as u32,
                                tc.r[5] as u32,
                                tc.r[6] as u32,
                                tc.r[7] as u32,
                                tc.r[8] as u32,
                                tc.r[9] as u32,
                                tc.r[10] as u32,
                                tc.r[11] as u32,
                                tc.r[12] as u32,
                            );
                            log::info!(
                                "pc_probe_stack: [sp+00]=0x{:08X} [sp+04]=0x{:08X} [sp+08]=0x{:08X} [sp+0C]=0x{:08X} [sp+10]=0x{:08X} [sp+14]=0x{:08X} [sp+18]=0x{:08X} [sp+1C]=0x{:08X}",
                                stack_words[0],
                                stack_words[1],
                                stack_words[2],
                                stack_words[3],
                                stack_words[4],
                                stack_words[5],
                                stack_words[6],
                                stack_words[7],
                            );
                        }
                    }
                }
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
            Self::single_core_run_guest_thread(
                kernel,
                &core_timing,
                &current_core,
                &mut idle_count,
            );
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
        core_timing: &Arc<CoreTiming>,
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
                let thread_arc = match kernel.get_current_emu_thread() {
                    Some(thread) => thread,
                    None => return,
                };
                // Single-core path: one host thread, no contention. Fetch the
                // JIT per-iteration (matches upstream's RunThread) — the
                // multi-core caching optimization above is unnecessary here.
                let (parent_arc, is_64bit, cached_jits) = {
                    let parent_weak = {
                        let thread = thread_arc.lock().unwrap();
                        match thread.parent.as_ref() {
                            Some(p) => p.clone(),
                            None => return,
                        }
                    };
                    let parent_arc = match parent_weak.upgrade() {
                        Some(p) => p,
                        None => return,
                    };
                    let mut process = parent_arc.lock().unwrap();
                    let is_64bit = process.is_64bit();
                    let mut jits: [Option<*mut Box<dyn crate::arm::arm_interface::ArmInterface>>;
                        hardware_properties::NUM_CPU_CORES as usize] =
                        [const { None }; hardware_properties::NUM_CPU_CORES as usize];
                    for i in 0..hardware_properties::NUM_CPU_CORES as usize {
                        if let Some(jit) = process.get_arm_interface_mut(i) {
                            jits[i] = Some(
                                jit as *mut _
                                    as *mut Box<dyn crate::arm::arm_interface::ArmInterface>,
                            );
                        }
                    }
                    drop(process);
                    (parent_arc, is_64bit, jits)
                };
                Self::run_guest_thread_once(
                    kernel,
                    physical_core,
                    &parent_arc,
                    &thread_arc,
                    is_64bit,
                    &cached_jits,
                );
            }

            // Upstream: kernel.SetIsPhantomModeForSingleCore(true);
            kernel.set_is_phantom_mode_for_single_core(true);
            let _ = core_timing.advance();
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
        core_timing: &Arc<CoreTiming>,
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
            core_timing.add_ticks(1000);
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
        core_timing: &Arc<CoreTiming>,
        from_running_environment: bool,
    ) {
        // Without kernel access, delegate to the inner version using stored state.
        // This path is used by the legacy System::run_main_loop path.
        if self.idle_count >= 4 || from_running_environment {
            if !from_running_environment {
                core_timing.idle();
                self.idle_count = 0;
            }
            let _ = core_timing.advance();
        }

        let next_core = (self.current_core.load(Ordering::Relaxed) + 1)
            % hardware_properties::NUM_CPU_CORES as usize;
        self.current_core.store(next_core, Ordering::Relaxed);

        core_timing.reset_ticks();
    }

    /// Inner preemption with full kernel access.
    ///
    /// Upstream: `CpuManager::PreemptSingleCore(bool)` (cpu_manager.cpp:145-166).
    fn preempt_single_core_inner(
        kernel: &KernelCore,
        core_timing: &Arc<CoreTiming>,
        current_core: &AtomicUsize,
        idle_count: &mut usize,
        from_running_environment: bool,
    ) {
        if *idle_count >= 4 || from_running_environment {
            if !from_running_environment {
                core_timing.idle();
                *idle_count = 0;
            }
            kernel.set_is_phantom_mode_for_single_core(true);
            let _ = core_timing.advance();
            kernel.set_is_phantom_mode_for_single_core(false);
        }

        let next_core = (current_core.load(Ordering::Relaxed) + 1)
            % hardware_properties::NUM_CPU_CORES as usize;
        current_core.store(next_core, Ordering::Relaxed);
        core_timing.reset_ticks();

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

        // Register a per-thread alternate signal stack for the rdynarmic SIGSEGV
        // (fastmem) handler. The handler is installed with SA_ONSTACK, but
        // sigaltstack is per-thread and rdynarmic only sets it once on the
        // registering thread — so without this, a fastmem fault on a guest
        // fiber runs the handler (FastmemPatchTable HashMap/SipHash lookup) on
        // the small fiber stack and can overflow it, producing a secondary
        // SIGSEGV that kills the process (silent exit 139 at the MK8D
        // scene-transition texture burst). Each CPU-core OS thread needs its
        // own altstack.
        rdynarmic::backend::x64::exception_handler::register_thread_signal_stack();

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
                let thread_host_ctx = thread_arc.lock().unwrap().get_host_context().cloned();
                if let Some(thread_host_ctx) = thread_host_ctx {
                    drop(scheduler);

                    // Upstream: Common::Fiber::YieldTo(data.host_context, *thread->GetHostContext());
                    log::info!(
                        "RunThread core {} host={}: yielding from host fiber to main thread fiber",
                        core,
                        std::thread::current().name().unwrap_or("?"),
                    );
                    Fiber::yield_to(Arc::downgrade(&host_ctx), &thread_host_ctx);
                    log::info!(
                        "RunThread core {} host={}: returned from main thread fiber",
                        core,
                        std::thread::current().name().unwrap_or("?"),
                    );
                } else {
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
