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
    pub current_thread_id: Option<u64>,
    pub yielded_thread_id: Option<u64>,
    // Fiber fields for host-thread switching
    /// Upstream: `std::shared_ptr<Common::Fiber> m_switch_fiber`
    pub switch_fiber: Option<Arc<Fiber>>,
    pub switch_cur_thread_id: Option<u64>,
    pub switch_highest_priority_thread_id: Option<u64>,
    pub switch_from_schedule: bool,
}

impl KScheduler {
    /// Create a new scheduler for the given core.
    pub fn new(core_id: i32) -> Self {
        Self {
            state: SchedulingState::default(),
            is_active: false,
            core_id,
            last_context_switch_time: 0,
            idle_thread_id: None,
            current_thread_id: None,
            yielded_thread_id: None,
            switch_fiber: None,
            switch_cur_thread_id: None,
            switch_highest_priority_thread_id: None,
            switch_from_schedule: false,
        }
    }

    /// Initialize the scheduler with main and idle threads.
    /// Matches upstream `KScheduler::Initialize(main_thread, idle_thread, core_id)`.
    pub fn initialize(
        &mut self,
        main_thread_id: u64,
        idle_thread_id: u64,
        core_id: i32,
    ) {
        self.core_id = core_id;
        self.idle_thread_id = Some(idle_thread_id);
        self.current_thread_id = Some(main_thread_id);
        // Upstream also: sets idle_thread_stack, interrupt_task_manager,
        // inserts main_thread into PQ, sets scheduler update needed.
        // PQ insertion is done by the caller (KProcess::create_main_thread).
    }

    /// Activate the scheduler.
    /// Matches upstream `KScheduler::Activate()`.
    pub fn activate(&mut self) {
        self.is_active = true;
        // Upstream: calls RescheduleCurrentCore().
        // In cooperative model, scheduling happens at SVC boundaries.
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
    /// if dispatch is allowed. In cooperative model, just set the flag.
    pub fn request_schedule_on_interrupt(&mut self) {
        self.state.needs_scheduling.store(true, Ordering::Relaxed);
    }

    /// Preempt single core.
    /// Matches upstream: disables dispatch, unloads thread, yields to switch fiber.
    pub fn preempt_single_core(&mut self) {
        self.request_schedule();
        // Upstream: DisableDispatch, Unload, Fiber::YieldTo(switch_fiber), EnableDispatch
    }

    /// Called when a thread first starts executing on this core.
    /// Matches upstream `KScheduler::OnThreadStart()`.
    pub fn on_thread_start(&self, current_thread: &Arc<Mutex<KThread>>) {
        current_thread.lock().unwrap().enable_dispatch();
    }

    /// Unload a thread's context (save guest state).
    /// Matches upstream `KScheduler::Unload(KThread*)`.
    /// In cooperative model: context save is handled by the dispatch loop
    /// via PhysicalCore::SaveContext.
    pub fn unload(&self, _thread: &Arc<Mutex<KThread>>) {
        // Upstream: m_kernel.PhysicalCore(m_core_id).SaveContext(thread)
        // Then unlock context_guard if not terminated.
        // In cooperative model: save happens in physical_core dispatch.
    }

    /// Reload a thread's context (restore guest state).
    /// Matches upstream `KScheduler::Reload(KThread*)`.
    pub fn reload(&self, _thread: &Arc<Mutex<KThread>>) {
        // Upstream: m_kernel.PhysicalCore(m_core_id).LoadContext(thread)
        // In cooperative model: load happens in physical_core dispatch.
    }

    /// Reschedule other cores by sending IPI.
    /// Matches upstream `KScheduler::RescheduleOtherCores(u64)`.
    pub fn reschedule_other_cores(&self, cores_needing_scheduling: u64) {
        let core_mask = cores_needing_scheduling & !(1u64 << self.core_id);
        if core_mask != 0 {
            Self::reschedule_cores(core_mask);
        }
    }

    /// Send IPI to cores that need rescheduling.
    /// Matches upstream `KScheduler::RescheduleCores(kernel, core_mask)`.
    pub fn reschedule_cores(core_mask: u64) {
        for i in 0..crate::hardware_properties::NUM_CPU_CORES as u64 {
            if core_mask & (1u64 << i) != 0 {
                // Upstream: kernel.PhysicalCore(i).Interrupt()
                // In cooperative model: interrupt other host threads.
                // For single-core, this is a no-op.
                log::trace!("RescheduleCores: would interrupt core {}", i);
            }
        }
    }

    /// Reschedule the current core.
    /// Matches upstream `KScheduler::RescheduleCurrentCore()`.
    pub fn reschedule_current_core(&mut self) {
        if self.state.needs_scheduling.load(Ordering::Relaxed) {
            self.reschedule_current_core_impl();
        }
    }

    fn reschedule_current_core_impl(&mut self) {
        if self.state.needs_scheduling.load(Ordering::Relaxed) {
            // Upstream: DisableDispatch, Schedule(), EnableDispatch
            self.state.needs_scheduling.store(false, Ordering::Relaxed);
        }
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
        current_thread.lock().unwrap().disable_dispatch();
    }

    /// Matches upstream `KScheduler::EnableScheduling(kernel, cores_needing_scheduling)`.
    /// Decrements dispatch count. If it reaches 0, reschedules.
    /// In cooperative model: scheduling happens at SVC boundaries via the
    /// dispatch loop, so we just decrement. Upstream would call
    /// RescheduleOtherCores + RescheduleCurrentCore here.
    pub fn enable_scheduling(current_thread: &Arc<Mutex<KThread>>, _cores_needing_scheduling: u64) {
        current_thread.lock().unwrap().enable_dispatch();
    }

    /// Matches upstream `KScheduler::UpdateHighestPriorityThreads(kernel)`.
    /// Called by KAbstractSchedulerLock::Unlock.
    pub fn update_highest_priority_threads() -> u64 {
        // In cooperative model, the dispatch loop's select_next_thread_from_pq
        // handles thread selection at SVC boundaries. This static method
        // would need GlobalSchedulerContext access (not available here).
        // Returns 0 = no cores need IPI-based rescheduling.
        0
    }

    /// Matches upstream `KScheduler::UpdateHighestPriorityThreadsImpl(kernel)`.
    /// Selects the highest-priority thread per core from the PQ.
    /// Returns bitmask of cores needing rescheduling.
    ///
    /// Full upstream version handles pinned threads and idle core migration.
    pub fn update_highest_priority_threads_impl(
        schedulers: &mut [KScheduler],
        gsc: &super::global_scheduler_context::GlobalSchedulerContext,
    ) -> u64 {
        use crate::hardware_properties::NUM_CPU_CORES;

        let mut cores_needing_scheduling = 0u64;
        let mut idle_cores = 0u64;
        let mut top_threads: [Option<u64>; NUM_CPU_CORES as usize] = [None; NUM_CPU_CORES as usize];

        // Select top thread per core from PQ.
        for core_id in 0..NUM_CPU_CORES as usize {
            let top_thread_id = gsc.m_priority_queue.get_scheduled_front(core_id as i32);

            // Upstream: check pinned threads here. Simplified for now.
            if top_thread_id.is_none() {
                idle_cores |= 1u64 << core_id;
            }

            top_threads[core_id] = top_thread_id;
            if core_id < schedulers.len() {
                cores_needing_scheduling |=
                    schedulers[core_id].update_highest_priority_thread(top_threads[core_id]);
            }
        }

        // Upstream: migrate suggested threads to idle cores.
        // For single-core with one active core, this is rarely needed.
        // TODO: implement full idle core migration for multi-core.

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
        let mut process = process.lock().unwrap();
        let Some(current_thread) = process.get_thread_by_thread_id(current_thread_id) else {
            return;
        };

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
    /// In upstream, this yields to a fiber for context switching.
    /// In our cooperative model, it updates current_thread_id and returns
    /// the next thread for the dispatch loop to switch to.
    pub fn schedule_impl(&mut self, process: &Arc<Mutex<KProcess>>) -> Option<u64> {
        self.state.needs_scheduling.store(false, Ordering::Relaxed);
        std::sync::atomic::fence(Ordering::SeqCst);

        let highest_priority_thread_id = self.state.highest_priority_thread_id;

        // If the interrupt task is runnable, switch to idle.
        let target = if self.state.interrupt_task_runnable {
            self.idle_thread_id
        } else {
            highest_priority_thread_id
        };

        // If same as current, nothing to do.
        if target == self.current_thread_id {
            std::sync::atomic::fence(Ordering::SeqCst);
            return None;
        }

        // Switch thread.
        if let Some(next_id) = target {
            self.switch_thread(process, next_id);
            Some(next_id)
        } else {
            // No thread — switch to idle.
            if let Some(idle_id) = self.idle_thread_id {
                self.switch_thread(process, idle_id);
                Some(idle_id)
            } else {
                None
            }
        }
    }

    /// Matches upstream `KScheduler::SwitchThread(KThread* next_thread)`.
    /// Updates CPU time tracking, previous thread, current thread.
    ///
    /// Upstream also handles process switching and TLS region switching.
    /// In cooperative model: just update tracking state.
    pub fn switch_thread(&mut self, process: &Arc<Mutex<KProcess>>, next_thread_id: u64) {
        let cur_thread_id = self.current_thread_id;

        // Update CPU time tracking.
        // Upstream: gets clock ticks from CoreTiming, adds delta to thread and process.
        // Simplified: just update last_context_switch_time.
        let prev_tick = self.last_context_switch_time;
        // TODO: get actual tick from CoreTiming
        let cur_tick = prev_tick + 1;
        self.last_context_switch_time = cur_tick;

        // Update previous thread.
        self.state.prev_thread_id = cur_thread_id;

        // Set the new thread.
        self.current_thread_id = Some(next_thread_id);
    }

    pub fn wait_for_next_thread(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) -> Option<Arc<Mutex<KThread>>> {
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
