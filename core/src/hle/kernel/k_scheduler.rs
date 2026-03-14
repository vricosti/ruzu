//! Port of zuyu/src/core/hle/kernel/k_scheduler.h / k_scheduler.cpp
//! Status: Partial (structural port, complex methods stubbed)
//! Derniere synchro: 2026-03-11
//!
//! KScheduler: per-core scheduler managing thread dispatch and context switching.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Instant;

use super::k_process::KProcess;
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
            switch_cur_thread_id: None,
            switch_highest_priority_thread_id: None,
            switch_from_schedule: false,
        }
    }

    /// Initialize the scheduler with main and idle threads.
    /// TODO: Port from k_scheduler.cpp.
    pub fn initialize(
        &mut self,
        main_thread_id: u64,
        idle_thread_id: u64,
        core_id: i32,
    ) {
        self.idle_thread_id = Some(idle_thread_id);
        self.current_thread_id = Some(main_thread_id);
        self.core_id = core_id;
    }

    /// Activate the scheduler.
    /// TODO: Port from k_scheduler.cpp.
    pub fn activate(&mut self) {
        self.is_active = true;
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
    /// TODO: Port from k_scheduler.cpp.
    pub fn set_interrupt_task_runnable(&mut self) {
        self.state.interrupt_task_runnable = true;
    }

    /// Request schedule on interrupt.
    /// TODO: Port from k_scheduler.cpp.
    pub fn request_schedule_on_interrupt(&mut self) {
        self.state.needs_scheduling.store(true, Ordering::Relaxed);
    }

    /// Preempt single core.
    /// TODO: Port from k_scheduler.cpp.
    pub fn preempt_single_core(&mut self) {
        // TODO: Full implementation
    }

    // -- Static methods (take kernel state references) --

    /// Disable scheduling for the current thread.
    /// TODO: Port from k_scheduler.cpp.
    pub fn disable_scheduling() {
        // TODO: Full implementation
    }

    /// Enable scheduling, rescheduling cores as needed.
    /// TODO: Port from k_scheduler.cpp.
    pub fn enable_scheduling(_cores_needing_scheduling: u64) {
        // TODO: Full implementation
    }

    /// Update highest priority threads across all cores.
    /// TODO: Port from k_scheduler.cpp.
    pub fn update_highest_priority_threads() -> u64 {
        // TODO: Full implementation
        0
    }

    /// On thread state changed.
    /// TODO: Port from k_scheduler.cpp.
    pub fn on_thread_state_changed(
        &mut self,
        thread_id: u64,
        old_state: super::k_thread::ThreadState,
        new_state: super::k_thread::ThreadState,
    ) {
        if old_state == new_state {
            return;
        }

        if matches!(old_state, ThreadState::RUNNABLE | ThreadState::WAITING)
            || matches!(new_state, ThreadState::RUNNABLE | ThreadState::WAITING | ThreadState::TERMINATED)
        {
            self.state.prev_thread_id = Some(thread_id);
            self.request_schedule();
        }
    }

    /// On thread priority changed.
    /// TODO: Port from k_scheduler.cpp.
    pub fn on_thread_priority_changed(&mut self, thread_id: u64, _old_priority: i32) {
        self.state.prev_thread_id = Some(thread_id);
        self.request_schedule();
    }

    /// Yield without core migration.
    pub fn yield_without_core_migration(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) {
        let mut process = process.lock().unwrap();
        let Some(current_thread) = process.get_thread_by_thread_id(current_thread_id) else {
            return;
        };

        let mut current_thread = current_thread.lock().unwrap();
        if current_thread.get_state() != ThreadState::RUNNABLE {
            return;
        }

        if current_thread.get_yield_schedule_count() == process.get_scheduled_count() {
            return;
        }

        process.increment_scheduled_count();
        self.yielded_thread_id = Some(current_thread_id);
        self.state.prev_thread_id = Some(current_thread_id);
        self.request_schedule();
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

    pub fn needs_scheduling(&self) -> bool {
        self.state.needs_scheduling.load(Ordering::Relaxed)
    }

    pub fn wake_expired_sleeping_threads(&mut self, process: &Arc<Mutex<KProcess>>) -> bool {
        let now = Instant::now();
        let mut process = process.lock().unwrap();
        let mut woke_any = false;

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

            thread.on_timer();
            woke_any = true;
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
        let process = process.lock().unwrap();
        let mut woke_any = false;

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

            thread.complete_synchronization_wait(synced_index, crate::hle::result::RESULT_SUCCESS.get_inner_value());
            woke_any = true;
        }

        if woke_any {
            self.request_schedule();
        }

        woke_any
    }

    pub fn next_sleep_deadline(&self, process: &Arc<Mutex<KProcess>>) -> Option<Instant> {
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
