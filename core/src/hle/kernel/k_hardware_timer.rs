//! Port of zuyu/src/core/hle/kernel/k_hardware_timer.h/.cpp
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! KHardwareTimer: the kernel hardware timer, responsible for scheduling
//! timer callbacks via CoreTiming. Inherits from KHardwareTimerBase.
//!
//! Full implementation requires CoreTiming integration.

use super::k_hardware_timer_base::{KHardwareTimerBase, TimerTaskId};

/// The kernel hardware timer.
///
/// Upstream inherits from KInterruptTask and KHardwareTimerBase.
/// It registers a CoreTiming callback to fire DoTask() at the appropriate
/// absolute time.
pub struct KHardwareTimer {
    base: KHardwareTimerBase,
    /// Absolute time in nanoseconds for the next wakeup.
    m_wakeup_time: i64,
    // m_event_type: shared_ptr<EventType> — requires CoreTiming
}

impl KHardwareTimer {
    pub fn new() -> Self {
        Self {
            base: KHardwareTimerBase::new(),
            m_wakeup_time: i64::MAX,
        }
    }

    /// Create the CoreTiming callback.
    pub fn initialize(&mut self) {
        // TODO: Register CoreTiming event "KHardwareTimer::Callback".
    }

    /// Unschedule the event and clean up.
    pub fn finalize(&mut self) {
        // TODO: Unschedule CoreTiming event.
        self.m_wakeup_time = i64::MAX;
    }

    /// Get the current tick (global time in nanoseconds).
    pub fn get_tick(&self) -> i64 {
        // TODO: Return system.CoreTiming().GetGlobalTimeNs().
        0
    }

    /// Register an absolute timer task. If the new task is earlier than
    /// the current wakeup time, re-arm the interrupt.
    pub fn register_absolute_task(&mut self, task_id: TimerTaskId, task_time: i64) {
        // Upstream: KScopedDisableDispatch + KScopedSpinLock
        if self.base.register_absolute_task_impl(task_id, task_time) {
            if task_time <= self.m_wakeup_time {
                self.enable_interrupt(task_time);
            }
        }
    }

    /// Cancel a task.
    pub fn cancel_task(&mut self, task_id: TimerTaskId, task_time: i64) {
        self.base.cancel_task(task_id, task_time);
    }

    fn enable_interrupt(&mut self, wakeup_time: i64) {
        self.disable_interrupt();
        self.m_wakeup_time = wakeup_time;
        // TODO: Schedule CoreTiming event at m_wakeup_time.
    }

    fn disable_interrupt(&mut self) {
        // TODO: Unschedule CoreTiming event.
        self.m_wakeup_time = i64::MAX;
    }

    fn get_interrupt_enabled(&self) -> bool {
        self.m_wakeup_time != i64::MAX
    }

    /// Called by the CoreTiming callback.
    fn do_task(&mut self) {
        // Upstream: KScopedSchedulerLock + KScopedSpinLock
        if !self.get_interrupt_enabled() {
            return;
        }

        self.m_wakeup_time = i64::MAX;

        let cur_tick = self.get_tick();
        let next_time = self.base.do_interrupt_task_impl(cur_tick, |_task_id| {
            // task.OnTimer() — requires KTimerTask integration
        });

        if next_time > 0 && next_time <= self.m_wakeup_time {
            self.enable_interrupt(next_time);
        }
    }
}

impl Default for KHardwareTimer {
    fn default() -> Self {
        Self::new()
    }
}
