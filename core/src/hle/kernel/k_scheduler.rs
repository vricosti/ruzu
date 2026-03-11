//! Port of zuyu/src/core/hle/kernel/k_scheduler.h / k_scheduler.cpp
//! Status: Partial (structural port, complex methods stubbed)
//! Derniere synchro: 2026-03-11
//!
//! KScheduler: per-core scheduler managing thread dispatch and context switching.

use std::sync::atomic::{AtomicBool, Ordering};

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
    pub fn on_thread_state_changed(_thread_id: u64, _old_state: super::k_thread::ThreadState) {
        // TODO: Full implementation
    }

    /// On thread priority changed.
    /// TODO: Port from k_scheduler.cpp.
    pub fn on_thread_priority_changed(_thread_id: u64, _old_priority: i32) {
        // TODO: Full implementation
    }

    /// Yield without core migration.
    /// TODO: Port from k_scheduler.cpp.
    pub fn yield_without_core_migration() {
        // TODO: Full implementation
    }

    /// Yield with core migration.
    /// TODO: Port from k_scheduler.cpp.
    pub fn yield_with_core_migration() {
        // TODO: Full implementation
    }

    /// Yield to any thread.
    /// TODO: Port from k_scheduler.cpp.
    pub fn yield_to_any_thread() {
        // TODO: Full implementation
    }
}
