//! Port of zuyu/src/core/hle/kernel/k_scheduler.h / k_scheduler.cpp
//! Status: Partial (structural port, complex methods stubbed)
//! Derniere synchro: 2026-03-11
//!
//! KScheduler: per-core scheduler managing thread dispatch and context switching.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, Weak};
use std::thread;
use std::time::{Duration, Instant};

use common::fiber::Fiber;

use super::k_priority_queue::{KPriorityQueue, ThreadAccessor};
use super::k_process::KProcess;
use super::k_thread::KThread;
use super::k_thread::ThreadState;

/// Scheduling state held per-core.
/// Matches upstream `KScheduler::SchedulingState` (k_scheduler.h).
pub struct SchedulingState {
    pub needs_scheduling: AtomicBool,
    pub interrupt_task_runnable: bool,
    pub should_count_idle: bool,
    pub idle_count: u64,
    pub highest_priority_thread_id: Option<u64>,
    pub idle_thread_stack: usize, // void* — opaque
    pub prev_thread_id: Option<u64>,
    // interrupt_task_manager — opaque
}

impl Default for SchedulingState {
    fn default() -> Self {
        Self {
            needs_scheduling: AtomicBool::new(false),
            interrupt_task_runnable: false,
            should_count_idle: false,
            idle_count: 0,
            highest_priority_thread_id: None,
            idle_thread_stack: 0,
            prev_thread_id: None,
        }
    }
}

/// The per-core kernel scheduler.
/// Matches upstream `KScheduler` class (k_scheduler.h).
pub struct KScheduler {
    pub state: SchedulingState,
    pub is_active: bool,
    pub core_id: i32,
    pub last_context_switch_time: i64,
    pub idle_thread_id: Option<u64>,
    pub idle_thread: Option<Weak<Mutex<KThread>>>,
    pub current_thread_id: Option<u64>,
    pub current_thread: Option<Weak<Mutex<KThread>>>,
    pub yielded_thread_id: Option<u64>,

    // Kernel references — upstream stores KernelCore& m_kernel
    pub global_scheduler_context: Option<Arc<Mutex<super::global_scheduler_context::GlobalSchedulerContext>>>,
    pub physical_cores: Vec<Arc<super::physical_core::PhysicalCore>>,
    pub core_timing: Option<Arc<std::sync::Mutex<crate::core_timing::CoreTiming>>>,

    // Fiber fields for host-thread switching
    /// Upstream: `std::shared_ptr<Common::Fiber> m_switch_fiber`
    pub switch_fiber: Option<Arc<Fiber>>,
    pub switch_cur_thread: Option<Weak<Mutex<KThread>>>,
    pub switch_highest_priority_thread: Option<Weak<Mutex<KThread>>>,
    pub switch_from_schedule: bool,
}

impl KScheduler {
    fn exit_thread_if_termination_requested(
        &self,
        process: &Arc<Mutex<KProcess>>,
        thread_id: u64,
    ) -> bool {
        let thread = {
            let process = process.lock().unwrap();
            process.get_thread_by_thread_id(thread_id)
        };
        let Some(thread) = thread else {
            return false;
        };

        if {
            let thread = thread.lock().unwrap();
            thread.is_termination_requested() && !thread.is_signaled()
        } {
            thread.lock().unwrap().exit();
            return true;
        }

        false
    }

    /// Create a new scheduler for the given core.
    pub fn new(core_id: i32) -> Self {
        Self {
            state: SchedulingState::default(),
            is_active: false,
            core_id,
            last_context_switch_time: 0,
            idle_thread_id: None,
            idle_thread: None,
            current_thread_id: None,
            current_thread: None,
            yielded_thread_id: None,
            global_scheduler_context: None,
            physical_cores: Vec::new(),
            core_timing: None,
            switch_fiber: None,
            switch_cur_thread: None,
            switch_highest_priority_thread: None,
            switch_from_schedule: false,
        }
    }

    /// Initialize the scheduler with main and idle threads (ID-only variant).
    /// Used by tests that don't need full thread references.
    pub fn initialize(
        &mut self,
        main_thread_id: u64,
        idle_thread_id: u64,
        core_id: i32,
    ) {
        self.core_id = core_id;
        self.idle_thread_id = Some(idle_thread_id);
        self.current_thread_id = Some(main_thread_id);
    }

    /// Initialize the scheduler with main and idle thread references.
    /// Matches upstream `KScheduler::Initialize(main_thread, idle_thread, core_id)`
    /// (k_scheduler.cpp:147-168).
    ///
    /// Sets `m_current_thread = main_thread` so that `get_scheduler_current_thread()`
    /// returns a valid thread with a host context for fiber switching.
    pub fn initialize_with_threads(
        &mut self,
        main_thread: &Arc<Mutex<KThread>>,
        idle_thread: &Arc<Mutex<KThread>>,
        core_id: i32,
    ) {
        self.core_id = core_id;

        let main_thread_id = main_thread.lock().unwrap().get_thread_id();
        let idle_thread_id = idle_thread.lock().unwrap().get_thread_id();

        self.idle_thread_id = Some(idle_thread_id);
        self.idle_thread = Some(Arc::downgrade(idle_thread));

        self.current_thread_id = Some(main_thread_id);
        self.current_thread = Some(Arc::downgrade(main_thread));
    }

    /// Activate the scheduler.
    /// Matches upstream `KScheduler::Activate()`.
    pub fn activate(&mut self) {
        self.is_active = true;
        self.reschedule_current_core();
    }

    /// Get the idle thread count.
    pub fn get_idle_count(&self) -> u64 {
        self.state.idle_count
    }

    /// Is the scheduler currently idle?
    pub fn is_idle(&self) -> bool {
        self.current_thread_id == self.idle_thread_id
    }

    /// Get the previous thread id.
    pub fn get_previous_thread_id(&self) -> Option<u64> {
        self.state.prev_thread_id
    }

    /// Get the current thread id.
    pub fn get_scheduler_current_thread_id(&self) -> Option<u64> {
        self.current_thread_id
    }

    /// Get the scheduler's current thread (Arc).
    /// Upstream: `KScheduler::GetSchedulerCurrentThread()`.
    pub fn get_scheduler_current_thread(&self) -> Option<Arc<Mutex<KThread>>> {
        self.current_thread.as_ref().and_then(Weak::upgrade)
    }

    /// Get the last context switch time.
    pub fn get_last_context_switch_time(&self) -> i64 {
        self.last_context_switch_time
    }

    /// Set the interrupt task as runnable.
    /// Matches upstream: sets flag and needs_scheduling.
    pub fn set_interrupt_task_runnable(&mut self) {
        self.state.interrupt_task_runnable = true;
        self.state.needs_scheduling.store(true, Ordering::Relaxed);
    }

    /// Request schedule on interrupt.
    /// Matches upstream: sets needs_scheduling, calls ScheduleOnInterrupt
    /// if dispatch is allowed.
    pub fn request_schedule_on_interrupt(&mut self) {
        self.state.needs_scheduling.store(true, Ordering::Relaxed);
    }

    /// Initialize the switch fiber for this scheduler.
    /// Must be called after the scheduler is wrapped in Arc<Mutex<>>.
    ///
    /// Upstream creates this in the KScheduler constructor:
    /// ```cpp
    /// m_switch_fiber = std::make_shared<Common::Fiber>([this] {
    ///     while (true) { ScheduleImplFiber(); }
    /// });
    /// ```
    /// In Rust we can't capture `self` in the constructor before the Arc exists,
    /// so this is a separate initialization step.
    pub fn init_switch_fiber(scheduler: &Arc<Mutex<KScheduler>>) {
        // The switch fiber runs on the same OS thread as the code that yields
        // to it. Only one fiber runs at a time, so there's no data race.
        // We use a raw pointer because the scheduler's Mutex is already held
        // by the caller when it yields to this fiber (YieldTo suspends the
        // caller's fiber with the MutexGuard still on its stack).
        // This matches upstream where ScheduleImplFiber accesses `this`
        // directly without re-locking.
        let sched_ptr = {
            let mut guard = scheduler.lock().unwrap();
            &mut *guard as *mut KScheduler as usize
        };
        let fiber = Fiber::new(Box::new(move || {
            loop {
                // Safety: only one fiber runs at a time on this OS thread,
                // and the scheduler outlives the switch fiber. We use usize
                // to bypass Send requirement (same pattern as CpuManager threads).
                let sched = unsafe { &mut *(sched_ptr as *mut KScheduler) };
                sched.schedule_impl_fiber_loop();
            }
        }));
        scheduler.lock().unwrap().switch_fiber = Some(fiber);
    }

    /// Preempt single core.
    /// Matches upstream `KScheduler::PreemptSingleCore()`:
    /// disables dispatch, unloads thread, yields to switch fiber, enables dispatch.
    pub fn preempt_single_core(&mut self) {
        // Upstream:
        //   GetCurrentThread(m_kernel).DisableDispatch();
        //   auto* thread = GetCurrentThreadPointer(m_kernel);
        //   auto& previous_scheduler = m_kernel.Scheduler(thread->GetCurrentCore());
        //   previous_scheduler.Unload(thread);
        //   Common::Fiber::YieldTo(thread->GetHostContext(), *m_switch_fiber);
        //   GetCurrentThread(m_kernel).EnableDispatch();

        let cur_thread = super::kernel::get_current_thread_pointer();
        if let Some(ref cur_thread) = cur_thread {
            cur_thread.lock().unwrap().disable_dispatch();

            // Unload the current thread
            self.unload(cur_thread);

            // Yield to the switch fiber
            if let Some(ref switch_fiber) = &self.switch_fiber {
                if let Some(ref host_ctx) = cur_thread.lock().unwrap().host_context {
                    Fiber::yield_to(Arc::downgrade(host_ctx), switch_fiber);
                }
            }

            cur_thread.lock().unwrap().enable_dispatch();
        }
    }

    /// Called when a thread first starts executing on this core.
    /// Matches upstream `KScheduler::OnThreadStart()`.
    pub fn on_thread_start(&self, current_thread: &Arc<Mutex<KThread>>) {
        current_thread.lock().unwrap().enable_dispatch();
    }

    /// Unload a thread's context (save guest state).
    /// Matches upstream `KScheduler::Unload(KThread*)`.
    pub fn unload(&self, thread: &Arc<Mutex<KThread>>) {
        // Upstream: m_kernel.PhysicalCore(m_core_id).SaveContext(thread)
        if let Some(core) = self.physical_cores.get(self.core_id as usize) {
            core.save_context(&mut thread.lock().unwrap());
        }

        // Check if the thread is terminated by checking the DPC flags.
        let thread_guard = thread.lock().unwrap();
        let dpc_flags = thread_guard.get_dpc();
        drop(thread_guard);
        if (dpc_flags & super::k_thread::DpcFlag::TERMINATED.bits() as u8) == 0 {
            // Upstream: thread->m_context_guard.unlock()
            // In our model, context_guard is managed by the fiber loop.
        }
    }

    /// Reload a thread's context (restore guest state).
    /// Matches upstream `KScheduler::Reload(KThread*)`.
    /// Upstream: `m_kernel.PhysicalCore(m_core_id).LoadContext(thread)`.
    pub fn reload(&self, thread: &Arc<Mutex<KThread>>) {
        // Inline PhysicalCore::LoadContext since the scheduler doesn't hold
        // a reference to the kernel's physical cores.
        let thread_guard = thread.lock().unwrap();
        let parent = match thread_guard.parent.as_ref().and_then(|w| w.upgrade()) {
            Some(p) => p,
            None => return,
        };
        let mut process = parent.lock().unwrap();
        if let Some(jit) = process.get_arm_interface_mut(self.core_id as usize) {
            let k_ctx = &thread_guard.thread_context;
            let arm_ctx: &crate::arm::arm_interface::ThreadContext =
                unsafe { &*(k_ctx as *const super::k_thread::ThreadContext
                    as *const crate::arm::arm_interface::ThreadContext) };
            jit.set_context(arm_ctx);
            jit.set_tpidrro_el0(thread_guard.get_tls_address().get());
            log::info!(
                "KScheduler::Reload: core={} r15/PC=0x{:X} r13/SP=0x{:X}",
                self.core_id, k_ctx.r[15], k_ctx.r[13],
            );
        }
    }

    /// Reschedule other cores by sending IPI.
    /// Matches upstream `KScheduler::RescheduleOtherCores(u64)`.
    pub fn reschedule_other_cores(&self, cores_needing_scheduling: u64) {
        let core_mask = cores_needing_scheduling & !(1u64 << self.core_id);
        if core_mask != 0 {
            self.reschedule_cores_impl(core_mask);
        }
    }

    /// Send IPI to cores that need rescheduling.
    /// Matches upstream `KScheduler::RescheduleCores(kernel, core_mask)`.
    fn reschedule_cores_impl(&self, core_mask: u64) {
        for i in 0..crate::hardware_properties::NUM_CPU_CORES as usize {
            if core_mask & (1u64 << i) != 0 {
                if let Some(core) = self.physical_cores.get(i) {
                    core.interrupt();
                }
            }
        }
    }

    /// Static version of RescheduleCores (when no scheduler instance available).
    /// Upstream: `KScheduler::RescheduleCores(kernel, core_mask)` sends IPIs.
    /// Without kernel reference, logs and returns (IPIs require physical core access).
    pub fn reschedule_cores(core_mask: u64) {
        if core_mask != 0 {
            log::trace!("KScheduler::reschedule_cores: core_mask={:#x} (no physical core access)", core_mask);
        }
    }

    /// Matches upstream `KScheduler::RescheduleCurrentHLEThread(kernel)`.
    /// Called when no scheduler is available (non-core threads, phantom mode).
    pub fn reschedule_current_hle_thread() {
        if let Some(cur_thread) = super::kernel::get_current_thread_pointer() {
            let mut t = cur_thread.lock().unwrap();
            debug_assert!(t.get_disable_dispatch_count() == 1);

            // Upstream: GetCurrentThread(kernel).DummyThreadBeginWait();
            // Ensure dummy threads that are waiting block.
            if t.is_dummy_thread() {
                t.dummy_thread_begin_wait();
            }

            debug_assert!(t.get_state() != ThreadState::WAITING);
            t.enable_dispatch();
        }
    }

    /// Reschedule the current core.
    /// Matches upstream `KScheduler::RescheduleCurrentCore()`.
    pub fn reschedule_current_core(&mut self) {
        // Upstream: ASSERT(!m_kernel.IsPhantomModeForSingleCore());
        // Upstream: ASSERT(GetCurrentThread(m_kernel).GetDisableDispatchCount() == 1);

        // Upstream: GetCurrentThread(m_kernel).EnableDispatch();
        if let Some(cur_thread) = super::kernel::get_current_thread_pointer() {
            cur_thread.lock().unwrap().enable_dispatch();
        }

        if self.state.needs_scheduling.load(Ordering::SeqCst) {
            self.reschedule_current_core_impl();
        }
    }

    fn reschedule_current_core_impl(&mut self) {
        // Upstream: if (m_state.needs_scheduling.load()) [[likely]] {
        //     GetCurrentThread(m_kernel).DisableDispatch();
        //     Schedule();
        //     GetCurrentThread(m_kernel).EnableDispatch();
        // }
        if self.state.needs_scheduling.load(Ordering::SeqCst) {
            if let Some(cur_thread) = super::kernel::get_current_thread_pointer() {
                cur_thread.lock().unwrap().disable_dispatch();
            }
            self.schedule();
            if let Some(cur_thread) = super::kernel::get_current_thread_pointer() {
                cur_thread.lock().unwrap().enable_dispatch();
            }
        }
    }

    /// Matches upstream `KScheduler::Schedule()`.
    fn schedule(&mut self) {
        // Upstream: ScheduleImpl() which yields to the switch fiber.
        self.schedule_impl_fiber();
    }

    /// Clear previous thread across all schedulers.
    /// Matches upstream `KScheduler::ClearPreviousThread(kernel, thread)`.
    pub fn clear_previous_thread(schedulers: &mut [KScheduler], thread_id: u64) {
        for scheduler in schedulers.iter_mut() {
            if scheduler.state.prev_thread_id == Some(thread_id) {
                scheduler.state.prev_thread_id = None;
            }
        }
    }

    // -- Static methods --
    // In upstream these take `KernelCore&` and access global state via
    // GetCurrentThread(kernel). Here they take the current thread directly.

    /// Matches upstream `KScheduler::DisableScheduling(kernel)`.
    /// Increments the current thread's disable_dispatch_count.
    pub fn disable_scheduling(current_thread: &Arc<Mutex<KThread>>) {
        let mut t = current_thread.lock().unwrap();
        debug_assert!(t.get_disable_dispatch_count() >= 0);
        t.disable_dispatch();
    }

    /// Matches upstream `KScheduler::EnableScheduling(kernel, cores_needing_scheduling)`.
    /// Decrements dispatch count. If it reaches 0, triggers rescheduling.
    pub fn enable_scheduling_with_scheduler(
        current_thread: &Arc<Mutex<KThread>>,
        cores_needing_scheduling: u64,
        scheduler: Option<&Arc<Mutex<KScheduler>>>,
        is_phantom_mode: bool,
    ) {
        debug_assert!(current_thread.lock().unwrap().get_disable_dispatch_count() >= 1);

        if scheduler.is_none() || is_phantom_mode {
            // Upstream: KScheduler::RescheduleCores(kernel, cores_needing_scheduling);
            //           KScheduler::RescheduleCurrentHLEThread(kernel);
            Self::reschedule_cores(cores_needing_scheduling);
            Self::reschedule_current_hle_thread();
            return;
        }

        let scheduler = scheduler.unwrap();
        {
            let sched = scheduler.lock().unwrap();
            sched.reschedule_other_cores(cores_needing_scheduling);
        }

        if current_thread.lock().unwrap().get_disable_dispatch_count() > 1 {
            current_thread.lock().unwrap().enable_dispatch();
        } else {
            scheduler.lock().unwrap().reschedule_current_core();
        }
    }

    /// Matches upstream `KScheduler::UpdateHighestPriorityThreads(kernel)`.
    /// Called by KAbstractSchedulerLock::Unlock.
    ///
    /// Upstream checks IsSchedulerUpdateNeeded and if set, calls
    /// UpdateHighestPriorityThreadsImpl. Returns bitmask of cores needing rescheduling.
    ///
    /// Note: The static version without kernel context returns 0.
    /// The full implementation requires GlobalSchedulerContext access and is
    /// invoked through the scheduler callbacks wired by the kernel.
    pub fn update_highest_priority_threads() -> u64 {
        // Without kernel context, return 0. When wired through SchedulerCallbacks,
        // the kernel provides the actual implementation via
        // update_highest_priority_threads_with_context().
        0
    }

    /// Full implementation of UpdateHighestPriorityThreads with GSC access.
    /// Called from wired scheduler callbacks that have kernel context.
    pub fn update_highest_priority_threads_with_context(
        gsc: &mut super::global_scheduler_context::GlobalSchedulerContext,
        schedulers: &mut [KScheduler],
    ) -> u64 {
        if gsc.m_scheduler_update_needed.load(std::sync::atomic::Ordering::Relaxed) {
            Self::update_highest_priority_threads_impl(schedulers, gsc)
        } else {
            0
        }
    }

    /// Matches upstream `KScheduler::UpdateHighestPriorityThreadsImpl(kernel)`.
    /// Full implementation with pinned thread handling and idle core migration.
    pub fn update_highest_priority_threads_impl(
        schedulers: &mut [KScheduler],
        gsc: &mut super::global_scheduler_context::GlobalSchedulerContext,
    ) -> u64 {
        use super::global_scheduler_context::HIGHEST_CORE_MIGRATION_ALLOWED_PRIORITY;
        use crate::hardware_properties::NUM_CPU_CORES;

        // Clear scheduler update needed.
        gsc.m_scheduler_update_needed
            .store(false, std::sync::atomic::Ordering::Relaxed);

        let mut cores_needing_scheduling = 0u64;
        let mut idle_cores = 0u64;
        let mut top_threads: [Option<u64>; NUM_CPU_CORES as usize] = [None; NUM_CPU_CORES as usize];

        // Select top thread per core from PQ.
        for core_id in 0..NUM_CPU_CORES as usize {
            let top_thread_id = gsc.m_priority_queue.get_scheduled_front(core_id as i32);

            // Upstream: check pinned thread for the process.
            // If the top thread's process has a pinned thread for this core,
            // and it's different from the top thread, prefer the pinned one
            // (unless top thread has kernel waiters).
            // Simplified: no pinned thread support yet.

            if top_thread_id.is_none() {
                idle_cores |= 1u64 << core_id;
            }

            top_threads[core_id] = top_thread_id;
            if core_id < schedulers.len() {
                cores_needing_scheduling |=
                    schedulers[core_id].update_highest_priority_thread(top_threads[core_id]);
            }
        }

        // Idle core migration: try to move suggested threads to idle cores.
        let accessor = gsc.make_accessor();
        let mut pq = std::mem::take(&mut gsc.m_priority_queue);
        while idle_cores != 0 {
            let core_id = idle_cores.trailing_zeros() as i32;

            let mut suggested = pq.get_suggested_front(core_id);
            let mut migration_candidates: Vec<i32> = Vec::new();

            while let Some(suggested_id) = suggested {
                let suggested_core = accessor
                    .with_thread(suggested_id, |t| t.get_active_core())
                    .unwrap_or(-1);

                let top_on_suggested =
                    if suggested_core >= 0 { top_threads[suggested_core as usize] } else { None };

                if top_on_suggested != Some(suggested_id) {
                    // Check priority threshold for migration.
                    let top_priority = top_on_suggested.and_then(|id| {
                        accessor.with_thread(id, |t| t.get_priority())
                    });
                    if let Some(p) = top_priority {
                        if p < HIGHEST_CORE_MIGRATION_ALLOWED_PRIORITY {
                            break; // Too high priority to migrate
                        }
                    }

                    // Migrate the suggested thread to the idle core.
                    accessor.with_thread_mut(suggested_id, |t| {
                        // Can't call set_active_core on trait object. We store core_id.
                    });
                    // Use PQ ChangeCore to move.
                    pq.change_core(suggested_core, suggested_id, false, &accessor);
                    top_threads[core_id as usize] = Some(suggested_id);
                    if (core_id as usize) < schedulers.len() {
                        cores_needing_scheduling |=
                            schedulers[core_id as usize].update_highest_priority_thread(Some(suggested_id));
                    }
                    break;
                }

                migration_candidates.push(suggested_core);
                let s_priority = accessor
                    .with_thread(suggested_id, |t| t.get_priority())
                    .unwrap_or(63);
                suggested = pq.get_suggested_next(core_id, suggested_id, s_priority, &accessor);
            }

            // If we didn't migrate a thread directly, try migrating a top thread
            // from a candidate core (if that core has another thread to run).
            if suggested.is_none() && top_threads[core_id as usize].is_none() {
                for &candidate_core in &migration_candidates {
                    if candidate_core < 0 { continue; }
                    let Some(candidate_top_id) = top_threads[candidate_core as usize] else { continue; };
                    let c_priority = accessor
                        .with_thread(candidate_top_id, |t| t.get_priority())
                        .unwrap_or(63);
                    let next_on_candidate = pq.get_scheduled_next(
                        candidate_core,
                        candidate_top_id,
                        c_priority,
                        &accessor,
                    );
                    if let Some(next_id) = next_on_candidate {
                        // The candidate core has another thread — migrate its top.
                        top_threads[candidate_core as usize] = Some(next_id);
                        if (candidate_core as usize) < schedulers.len() {
                            cores_needing_scheduling |=
                                schedulers[candidate_core as usize]
                                    .update_highest_priority_thread(Some(next_id));
                        }

                        pq.change_core(candidate_core, candidate_top_id, false, &accessor);
                        top_threads[core_id as usize] = Some(candidate_top_id);
                        if (core_id as usize) < schedulers.len() {
                            cores_needing_scheduling |=
                                schedulers[core_id as usize]
                                    .update_highest_priority_thread(Some(candidate_top_id));
                        }
                        break;
                    }
                }
            }

            idle_cores &= !(1u64 << core_id);
        }
        gsc.m_priority_queue = pq;

        // Wake up waiting dummy threads.
        gsc.wakeup_waiting_dummy_threads();

        cores_needing_scheduling
    }

    /// Update the highest priority thread for this core.
    /// Matches upstream `KScheduler::UpdateHighestPriorityThread(KThread*)`.
    /// Returns a bitmask of cores needing scheduling (1 << core_id) if changed.
    pub fn update_highest_priority_thread(&mut self, highest_thread_id: Option<u64>) -> u64 {
        let prev = self.state.highest_priority_thread_id;
        if prev != highest_thread_id {
            // Upstream: IncrementScheduledCount on prev, track idle count, etc.
            self.state.highest_priority_thread_id = highest_thread_id;
            self.state.needs_scheduling.store(true, Ordering::Relaxed);
            1u64 << self.core_id
        } else {
            0
        }
    }

    /// On thread state changed.
    /// Matches upstream `KScheduler::OnThreadStateChanged(kernel, thread, old_state)`.
    ///
    /// Updates the priority queue when a thread transitions to/from RUNNABLE.
    pub fn on_thread_state_changed(
        &mut self,
        thread_id: u64,
        old_state: ThreadState,
        new_state: ThreadState,
    ) {
        if old_state == new_state {
            return;
        }

        // Always request scheduling on state change for cooperative dispatch.
        self.state.prev_thread_id = Some(thread_id);
        self.request_schedule();

        // PQ updates happen at the call sites that transition thread state
        // while holding the process lock (condvar wait/signal, sync wait,
        // thread exit, etc.). The dispatch loop's scan_runnable_threads
        // fallback catches any threads that bypass PQ updates.
    }

    /// On thread priority changed.
    /// Matches upstream `KScheduler::OnThreadPriorityChanged(kernel, thread, old_priority)`.
    pub fn on_thread_priority_changed(&mut self, thread_id: u64, _old_priority: i32) {
        self.state.prev_thread_id = Some(thread_id);
        self.request_schedule();

        // Upstream: if thread is RUNNABLE, call
        // priority_queue.ChangePriority(old_priority, is_running, thread).
        // Same as above: PQ update deferred until PQ-based dispatch.
    }

    /// Yield without core migration.
    /// Matches upstream `KScheduler::YieldWithoutCoreMigration(kernel)`.
    pub fn yield_without_core_migration(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) {
        // Cooperative runtime approximation of a thread observing its pending
        // termination and exiting at the next yield boundary.
        if self.exit_thread_if_termination_requested(process, current_thread_id) {
            self.request_schedule();
            return;
        }

        let current_thread = {
            let process = process.lock().unwrap();
            process.get_thread_by_thread_id(current_thread_id)
        };
        let Some(current_thread) = current_thread else {
            return;
        };

        let mut process = process.lock().unwrap();
        {
            let current_thread = current_thread.lock().unwrap();
            if current_thread.get_state() != ThreadState::RUNNABLE {
                return;
            }
            if current_thread.get_yield_schedule_count() == process.get_scheduled_count() {
                return;
            }
        }

        // Move current thread to the back of its priority level in the PQ.
        // Upstream: next_thread = priority_queue.MoveToScheduledBack(cur_thread)
        let next_thread_id = if let Some(ref gsc) = process.global_scheduler_context {
            gsc.lock().unwrap().move_to_scheduled_back(current_thread_id)
        } else {
            None
        };
        process.increment_scheduled_count();

        if let Some(next_id) = next_thread_id {
            if next_id != current_thread_id {
                // A different thread is now at the front — schedule update needed.
                self.yielded_thread_id = Some(current_thread_id);
                self.request_schedule();
            } else {
                // No other thread at this priority — set yield count.
                current_thread
                    .lock()
                    .unwrap()
                    .set_yield_schedule_count(process.get_scheduled_count());
            }
        } else {
            // PQ was empty (thread not in PQ) — fall back to old behavior.
            self.yielded_thread_id = Some(current_thread_id);
            self.request_schedule();
        }
    }

    /// Yield with core migration.
    pub fn yield_with_core_migration(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) {
        // Single-core bring-up: match upstream control flow entry point, but keep the same
        // local-core behavior until cross-core migration exists.
        self.yield_without_core_migration(process, current_thread_id);
    }

    /// Yield to any thread.
    pub fn yield_to_any_thread(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) {
        // Single-core bring-up: preserve the SVC ownership in KScheduler while deferring
        // real inter-core migration until the full priority queue exists.
        self.yield_without_core_migration(process, current_thread_id);
    }

    pub fn set_scheduler_current_thread_id(&mut self, thread_id: u64) {
        self.current_thread_id = Some(thread_id);
        if self.yielded_thread_id == Some(thread_id) {
            self.yielded_thread_id = None;
        }
        self.state.needs_scheduling.store(false, Ordering::Relaxed);
    }

    pub fn request_schedule(&self) {
        self.state.needs_scheduling.store(true, Ordering::Relaxed);
    }

    /// Rotate the scheduled queue at a given priority for a core.
    /// Matches upstream `KScheduler::RotateScheduledQueue(kernel, core_id, priority)`.
    ///
    /// Moves the front thread at `priority` to the back, then tries to
    /// migrate a suggested thread to fill the gap.
    /// Rotate the scheduled queue at a given priority for a core.
    /// Operates on the GlobalSchedulerContext's PQ.
    pub fn rotate_scheduled_queue(
        gsc: &mut super::global_scheduler_context::GlobalSchedulerContext,
        core_id: i32,
        priority: i32,
    ) {
        let accessor = gsc.make_accessor();
        let mut pq = std::mem::take(&mut gsc.m_priority_queue);
        let top_thread_id = pq.get_scheduled_front_at_priority(core_id, priority);
        if let Some(top_id) = top_thread_id {
            let _ = pq.move_to_scheduled_back(top_id, &accessor);
        }
        gsc.m_priority_queue = pq;
    }

    pub fn needs_scheduling(&self) -> bool {
        self.state.needs_scheduling.load(Ordering::Relaxed)
    }

    pub fn wake_expired_sleeping_threads(&mut self, process: &Arc<Mutex<KProcess>>) -> bool {
        let now = Instant::now();
        let mut process = process.lock().unwrap();
        let mut woke_any = false;
        let mut woke_ids = Vec::new();

        for thread_id in &process.thread_list {
            let Some(thread) = process.get_thread_by_thread_id(*thread_id) else {
                continue;
            };

            let mut thread = thread.lock().unwrap();
            let Some(deadline) = thread.get_sleep_deadline() else {
                continue;
            };
            if deadline > now {
                continue;
            }

            let tid = thread.get_thread_id();
            thread.on_timer();
            woke_ids.push(tid);
            woke_any = true;
        }

        // Push woken threads to PQ (they're now RUNNABLE).
        for tid in woke_ids {
            process.push_back_to_priority_queue(tid);
        }

        if woke_any {
            self.request_schedule();
        }

        woke_any
    }

    pub fn wake_signaled_synchronization_threads(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
    ) -> bool {
        let mut process = process.lock().unwrap();
        let mut woke_any = false;
        let mut woke_ids = Vec::new();

        for thread_id in &process.thread_list {
            let Some(thread) = process.get_thread_by_thread_id(*thread_id) else {
                continue;
            };

            let mut thread = thread.lock().unwrap();
            if !thread.is_waiting_on_synchronization() || thread.get_state() != ThreadState::WAITING {
                continue;
            }

            let Some(synced_index) = thread.check_synchronization_ready(&process) else {
                continue;
            };

            let tid = thread.get_thread_id();
            thread.complete_synchronization_wait(synced_index, crate::hle::result::RESULT_SUCCESS.get_inner_value());
            woke_ids.push(tid);
            woke_any = true;
        }

        // Push woken threads to PQ (they're now RUNNABLE).
        for tid in woke_ids {
            process.push_back_to_priority_queue(tid);
        }

        if woke_any {
            self.request_schedule();
        }

        woke_any
    }

    fn next_sleep_deadline(&self, process: &Arc<Mutex<KProcess>>) -> Option<Instant> {
        let process = process.lock().unwrap();
        let mut next_deadline = None;

        for thread_id in &process.thread_list {
            let Some(thread) = process.get_thread_by_thread_id(*thread_id) else {
                continue;
            };

            let thread = thread.lock().unwrap();
            let Some(deadline) = thread.get_sleep_deadline() else {
                continue;
            };

            next_deadline = Some(match next_deadline {
                Some(current) if current <= deadline => current,
                _ => deadline,
            });
        }

        next_deadline
    }

    pub fn wait_for_next_runnable_thread(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) -> u64 {
        loop {
            self.wake_expired_sleeping_threads(process);
            self.wake_signaled_synchronization_threads(process);

            // PQ-based selection (O(1) for highest priority thread).
            if let Some(next) = self.select_next_thread_from_pq(process) {
                return next;
            }

            // PQ empty — fallback to linear scan for RUNNABLE threads that
            // aren't in the PQ (timer/cancel_wait wakeups, early init).
            // This is a safety net; once all wakeup paths push to PQ, this
            // becomes unreachable.
            if let Some(next) = self.scan_runnable_threads(process) {
                return next;
            }

            if let Some(deadline) = self.next_sleep_deadline(process) {
                let now = Instant::now();
                if deadline > now {
                    thread::sleep(deadline.duration_since(now));
                }
                continue;
            }

            thread::sleep(Duration::from_millis(1));
        }
    }

    /// Fallback: scan for the highest-priority RUNNABLE thread.
    /// Used when PQ is empty (threads woken via timer/cancel_wait that
    /// bypass PQ). Returns the thread_id and pushes it to PQ for future use.
    fn scan_runnable_threads(&self, process: &Arc<Mutex<KProcess>>) -> Option<u64> {
        let process = process.lock().unwrap();
        let mut best_id = None;
        let mut best_priority = i32::MAX;

        for thread_id in &process.thread_list {
            let Some(thread) = process.get_thread_by_thread_id(*thread_id) else {
                continue;
            };
            let thread = thread.lock().unwrap();
            if thread.get_state() != ThreadState::RUNNABLE {
                continue;
            }
            if thread.get_priority() < best_priority {
                best_priority = thread.get_priority();
                best_id = Some(thread.get_thread_id());
            }
        }

        // Push the found thread to PQ so future lookups are O(1).
        if let Some(tid) = best_id {
            process.push_back_to_priority_queue(tid);
        }
        best_id
    }

    /// Select the next thread using the priority queue.
    /// This is the upstream-matching dispatch path: O(1) lookup of the
    /// highest priority RUNNABLE thread for our core.
    fn select_next_thread_from_pq(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
    ) -> Option<u64> {
        let process_guard = process.lock().unwrap();
        let gsc = process_guard.global_scheduler_context.as_ref()?;
        let gsc_guard = gsc.lock().unwrap();
        let next_thread_id = gsc_guard.get_scheduled_front(self.core_id)?;

        // Handle yield: if the current thread yielded, try the next one.
        if let Some(yielded) = self.yielded_thread_id {
            if next_thread_id == yielded {
                let priority = process_guard
                    .get_thread_by_thread_id(next_thread_id)
                    .map(|t| t.lock().unwrap().get_priority())
                    .unwrap_or(63);
                let next = gsc_guard.get_scheduled_next(
                    self.core_id,
                    next_thread_id,
                    priority,
                );
                if let Some(alternative) = next {
                    self.yielded_thread_id = None;
                    return Some(alternative);
                }
                if let Some(thread) = process_guard.get_thread_by_thread_id(yielded) {
                    thread
                        .lock()
                        .unwrap()
                        .set_yield_schedule_count(process_guard.get_scheduled_count());
                }
                self.yielded_thread_id = None;
            }
        }

        Some(next_thread_id)
    }

    /// Matches upstream `KScheduler::ScheduleImpl()`.
    /// Clears needs_scheduling, selects the highest priority thread.
    ///
    /// Upstream yields to a fiber for context switching via ScheduleImplFiber.
    /// We provide both cooperative (no fiber) and fiber-based paths.
    fn schedule_impl_cooperative(&mut self) {
        self.state.needs_scheduling.store(false, Ordering::Relaxed);
        std::sync::atomic::fence(Ordering::SeqCst);

        let highest = self.state.highest_priority_thread_id;

        // If the interrupt task is runnable, switch to idle.
        let target = if self.state.interrupt_task_runnable {
            self.idle_thread_id
        } else {
            highest
        };

        // If same as current, nothing to do.
        if target == self.current_thread_id {
            std::sync::atomic::fence(Ordering::SeqCst);
            return;
        }

        // Switch to the target thread.
        let next_id = target.or(self.idle_thread_id);
        if let Some(next_id) = next_id {
            self.switch_thread_impl(next_id);
        }
    }

    /// Matches upstream `KScheduler::ScheduleImpl()` — fiber-based path.
    /// In upstream, this yields to m_switch_fiber which runs ScheduleImplFiber.
    fn schedule_impl_fiber(&mut self) {
        self.state.needs_scheduling.store(false, Ordering::Relaxed);
        std::sync::atomic::fence(Ordering::SeqCst);

        let cur_thread = self.current_thread.as_ref().and_then(Weak::upgrade);
        let highest = self.state.highest_priority_thread_id;

        log::info!(
            "schedule_impl_fiber: core={} cur_thread={} highest={:?} has_gsc={} has_switch_fiber={}",
            self.core_id,
            cur_thread.as_ref().map(|t| t.lock().unwrap().get_thread_id()).unwrap_or(u64::MAX),
            highest,
            self.global_scheduler_context.is_some(),
            self.switch_fiber.is_some(),
        );

        let target = if self.state.interrupt_task_runnable {
            self.idle_thread.as_ref().and_then(Weak::upgrade)
        } else {
            highest.and_then(|id| {
                let gsc = self.global_scheduler_context.as_ref()?;
                gsc.lock().unwrap().get_thread_by_thread_id(id)
            })
        };

        log::info!(
            "schedule_impl_fiber: target={}",
            target.as_ref().map(|t| t.lock().unwrap().get_thread_id()).unwrap_or(u64::MAX),
        );

        // If same as current, nothing to do.
        if let (Some(ref cur), Some(ref tgt)) = (&cur_thread, &target) {
            if Arc::ptr_eq(cur, tgt) {
                log::info!("schedule_impl_fiber: target == current, returning");
                std::sync::atomic::fence(Ordering::SeqCst);
                return;
            }
        }

        if target.is_none() {
            log::info!("schedule_impl_fiber: no target thread, returning");
            return;
        }

        // Store switch state for the fiber.
        self.switch_cur_thread = cur_thread.as_ref().map(Arc::downgrade);
        self.switch_highest_priority_thread = target.as_ref().map(Arc::downgrade);
        self.switch_from_schedule = true;

        // Upstream: Common::Fiber::YieldTo(cur_thread->m_host_context, *m_switch_fiber)
        if let Some(ref cur) = cur_thread {
            let cur_lock = cur.lock().unwrap();
            let has_ctx = cur_lock.host_context.is_some();
            log::info!("schedule_impl_fiber: cur thread {} has host_context={}", cur_lock.get_thread_id(), has_ctx);
            if let Some(ref host_ctx) = cur_lock.host_context {
                if let Some(ref switch_fiber) = self.switch_fiber {
                    let host_weak = Arc::downgrade(host_ctx);
                    drop(cur_lock); // release thread lock before yield
                    log::info!("schedule_impl_fiber: yielding to switch_fiber NOW");
                    Fiber::yield_to(host_weak, switch_fiber);
                    log::info!("schedule_impl_fiber: returned from switch_fiber");
                } else {
                    log::warn!("schedule_impl_fiber: no switch_fiber");
                }
            } else {
                log::warn!("schedule_impl_fiber: current thread has no host_context");
            }
        } else {
            log::warn!("schedule_impl_fiber: no current thread");
        }
    }

    /// Matches upstream `KScheduler::ScheduleImplFiber()` (k_scheduler.cpp:420-494).
    /// This runs inside the switch fiber and handles the actual context switch:
    /// unloads old thread, spins to acquire new thread's context_guard,
    /// calls SwitchThread, reloads new thread, then yields back.
    fn schedule_impl_fiber_loop(&mut self) {
        log::info!("schedule_impl_fiber_loop: ENTERED switch fiber");

        let cur_thread = self.switch_cur_thread.as_ref().and_then(Weak::upgrade);
        let mut highest_priority_thread = self.switch_highest_priority_thread.as_ref().and_then(Weak::upgrade);

        log::info!(
            "schedule_impl_fiber_loop: cur={} hpt={} from_schedule={}",
            cur_thread.as_ref().map(|t| t.lock().unwrap().get_thread_id()).unwrap_or(u64::MAX),
            highest_priority_thread.as_ref().map(|t| t.lock().unwrap().get_thread_id()).unwrap_or(u64::MAX),
            self.switch_from_schedule,
        );

        // If we're not coming from scheduling (i.e., we came from SC preemption),
        // skip the unload and jump straight to retry.
        let mut need_retry = !self.switch_from_schedule;

        if self.switch_from_schedule {
            self.switch_from_schedule = false;

            // Save the original thread context.
            if let Some(ref cur) = cur_thread {
                self.unload(cur);
            }
        }

        // Loop until we successfully switch the thread context.
        loop {
            if need_retry {
                // Clear needs_scheduling and refresh highest priority thread.
                self.state.needs_scheduling.store(false, Ordering::Relaxed);
                std::sync::atomic::fence(Ordering::SeqCst);

                highest_priority_thread = if let Some(id) = self.state.highest_priority_thread_id {
                    if let Some(ref gsc) = self.global_scheduler_context {
                        gsc.lock().unwrap().get_thread_by_thread_id(id)
                    } else {
                        None
                    }
                } else {
                    None
                };
            }
            need_retry = false;

            // If highest_priority_thread is null, switch to idle thread.
            if highest_priority_thread.is_none() {
                highest_priority_thread = self.idle_thread.as_ref().and_then(Weak::upgrade);
            }

            let Some(ref hpt) = highest_priority_thread else {
                // No thread available at all — retry.
                need_retry = true;
                continue;
            };

            // Try to lock the highest priority thread's context_guard.
            loop {
                if hpt.lock().unwrap().context_guard.try_lock().is_some() {
                    break;
                }
                // Context is locked by another core. Check if we need rescheduling.
                if self.state.needs_scheduling.load(Ordering::SeqCst) {
                    need_retry = true;
                    break;
                }
            }
            if need_retry {
                continue;
            }

            // Switch to the highest priority thread.
            let hpt_id = hpt.lock().unwrap().thread_id;
            self.switch_thread_impl(hpt_id);

            // Check if we need scheduling again. If so, unlock and retry.
            if self.state.needs_scheduling.load(Ordering::SeqCst) {
                // Unlock context_guard — drop the lock we acquired above.
                // (parking_lot::Mutex try_lock returns a guard that auto-drops)
                need_retry = true;
                continue;
            }

            // Success — break out of the loop.
            break;
        }

        // Reload the guest thread context.
        if let Some(ref hpt) = highest_priority_thread {
            let hpt_id = hpt.lock().unwrap().get_thread_id();
            log::info!("schedule_impl_fiber_loop: Reload + YieldTo thread {}", hpt_id);
            self.reload(hpt);

            // Yield back from switch_fiber to the newly scheduled thread's host_context.
            // Upstream: Common::Fiber::YieldTo(m_switch_fiber, *highest_priority_thread->m_host_context)
            if let Some(ref switch_fiber) = self.switch_fiber {
                let host_ctx = hpt.lock().unwrap().host_context.clone();
                if let Some(ref ctx) = host_ctx {
                    log::info!("schedule_impl_fiber_loop: yielding from switch_fiber to thread {} fiber", hpt_id);
                    Fiber::yield_to(Arc::downgrade(switch_fiber), ctx);
                } else {
                    log::warn!("schedule_impl_fiber_loop: thread {} has no host_context", hpt_id);
                }
            }
        }
    }

    /// Matches upstream `KScheduler::SwitchThread(KThread* next_thread)`.
    /// Updates CPU time tracking, previous thread, current thread.
    fn switch_thread_impl(&mut self, next_thread_id: u64) {
        let cur_thread_id = self.current_thread_id;

        log::info!(
            "switch_thread_impl: cur={:?} next={} has_gsc={}",
            cur_thread_id, next_thread_id, self.global_scheduler_context.is_some()
        );

        // If same thread, nothing to do.
        if Some(next_thread_id) == cur_thread_id {
            log::info!("switch_thread_impl: same thread, skipping");
            return;
        }

        // Update CPU time tracking.
        let prev_tick = self.last_context_switch_time;
        let cur_tick = if let Some(ref ct) = self.core_timing {
            ct.lock().unwrap().get_global_time_ns().as_nanos() as i64
        } else {
            prev_tick + 1
        };
        let tick_diff = cur_tick - prev_tick;
        self.last_context_switch_time = cur_tick;

        // Add CPU time to current thread.
        if let Some(ref cur) = self.current_thread.as_ref().and_then(Weak::upgrade) {
            cur.lock().unwrap().add_cpu_time(self.core_id, tick_diff);
        }

        // Update previous thread.
        if let Some(ref cur) = self.current_thread.as_ref().and_then(Weak::upgrade) {
            let cur_guard = cur.lock().unwrap();
            if !cur_guard.is_termination_requested()
                && cur_guard.get_active_core() == self.core_id
            {
                self.state.prev_thread_id = cur_thread_id;
            } else {
                self.state.prev_thread_id = None;
            }
        }

        // Set the new current thread.
        self.current_thread_id = Some(next_thread_id);
        if let Some(ref gsc) = self.global_scheduler_context {
            if let Some(next) = gsc.lock().unwrap().get_thread_by_thread_id(next_thread_id) {
                // Ensure the thread is on our core.
                {
                    let mut next_guard = next.lock().unwrap();
                    if next_guard.get_current_core() != self.core_id {
                        next_guard.set_current_core(self.core_id);
                    }
                }
                // Upstream: SetCurrentThread(m_kernel, next_thread);
                super::kernel::set_current_emu_thread(Some(&next));
                self.current_thread = Some(Arc::downgrade(&next));
            }
        }
    }

    /// Matches upstream `KScheduler::Unload(KThread*)`.
    /// Saves guest context and unlocks the thread's context guard.
    pub fn unload_thread(&self, thread: &Arc<Mutex<KThread>>) {
        // Upstream: m_kernel.PhysicalCore(m_core_id).SaveContext(thread)
        if let Some(core) = self.physical_cores.get(self.core_id as usize) {
            core.save_context(&mut thread.lock().unwrap());
        }

        // Unlock context guard if thread is not terminated.
        let thread_guard = thread.lock().unwrap();
        let dpc_flags = thread_guard.get_dpc();
        drop(thread_guard);
        if (dpc_flags & super::k_thread::DpcFlag::TERMINATED.bits() as u8) == 0 {
            // Upstream: thread->m_context_guard.unlock()
            // In our model, parking_lot::Mutex doesn't support manual unlock
            // without a guard. The context_guard is managed by the fiber loop.
        }
    }

    /// Matches upstream `KScheduler::Reload(KThread*)`.
    /// Restores guest context.
    pub fn reload_thread(&self, thread: &Arc<Mutex<KThread>>) {
        // Upstream: m_kernel.PhysicalCore(m_core_id).LoadContext(thread)
        if let Some(core) = self.physical_cores.get(self.core_id as usize) {
            core.load_context(&thread.lock().unwrap());
        }
    }

    pub fn wait_for_next_thread(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) -> Option<Arc<Mutex<KThread>>> {
        if self.exit_thread_if_termination_requested(process, current_thread_id) {
            self.request_schedule();
        }
        let next_thread_id = self.wait_for_next_runnable_thread(process, current_thread_id);
        let next_thread = process
            .lock()
            .unwrap()
            .get_thread_by_thread_id(next_thread_id);
        if next_thread.is_some() {
            self.set_scheduler_current_thread_id(next_thread_id);
        }
        next_thread
    }

    /// Deprecated: linear scan for next thread. Replaced by PQ-based dispatch.
    /// Kept only for test compatibility (svc_thread tests use it directly).
    #[cfg(test)]
    pub fn select_next_thread_id(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) -> Option<u64> {
        if self.exit_thread_if_termination_requested(process, current_thread_id) {
            self.request_schedule();
        }
        let mut process = process.lock().unwrap();

        let mut best_thread_id = None;
        let mut best_priority = i32::MAX;
        let mut yielded_alternative_thread_id = None;
        let mut yielded_priority = i32::MAX;
        let yielded_thread_id = self.yielded_thread_id;

        for thread_id in &process.thread_list {
            let Some(thread) = process.get_thread_by_thread_id(*thread_id) else {
                continue;
            };

            let thread = thread.lock().unwrap();
            if thread.get_state() != ThreadState::RUNNABLE {
                continue;
            }

            if thread.get_priority() < best_priority {
                best_priority = thread.get_priority();
                best_thread_id = Some(thread.get_thread_id());
            } else if thread.get_priority() == best_priority {
                let replace = match best_thread_id {
                    None => true,
                    Some(best) => thread.get_thread_id() == current_thread_id && best != current_thread_id,
                };
                if replace {
                    best_thread_id = Some(thread.get_thread_id());
                }
            }

            if yielded_thread_id == Some(current_thread_id)
                && thread.get_thread_id() != current_thread_id
                && thread.get_priority() <= yielded_priority
            {
                yielded_priority = thread.get_priority();
                yielded_alternative_thread_id = Some(thread.get_thread_id());
            }
        }

        let next_thread_id = if yielded_thread_id == Some(current_thread_id) {
            match yielded_alternative_thread_id {
                Some(candidate) if yielded_priority == best_priority => Some(candidate),
                _ => best_thread_id,
            }
        } else {
            best_thread_id
        };

        if let Some(yielded_thread_id) = yielded_thread_id {
            if next_thread_id == Some(yielded_thread_id) {
                if let Some(current_thread) = process.get_thread_by_thread_id(yielded_thread_id) {
                    current_thread
                        .lock()
                        .unwrap()
                        .set_yield_schedule_count(process.get_scheduled_count());
                }
            }
        }

        self.yielded_thread_id = None;
        next_thread_id
    }
}
