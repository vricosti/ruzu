//! Port of zuyu/src/core/hle/kernel/k_scoped_scheduler_lock_and_sleep.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-21
//!
//! KScopedSchedulerLockAndSleep — RAII guard that locks the scheduler and
//! optionally registers a timer task on drop.

use std::sync::{Arc, Mutex};

use super::k_hardware_timer::KHardwareTimer;
use super::k_scheduler_lock::KAbstractSchedulerLock;
use super::k_thread::KThread;

/// KScopedSchedulerLockAndSleep — acquires the scheduler lock on construction,
/// and on drop registers an absolute timer task if the timeout is positive,
/// then releases the scheduler lock.
///
/// Matches upstream `Kernel::KScopedSchedulerLockAndSleep`.
pub struct KScopedSchedulerLockAndSleep<'a> {
    m_scheduler_lock: &'a KAbstractSchedulerLock,
    m_timeout_tick: i64,
    m_thread: Arc<Mutex<KThread>>,
    m_timer: Option<Arc<Mutex<KHardwareTimer>>>,
}

impl<'a> KScopedSchedulerLockAndSleep<'a> {
    /// Create a new scoped scheduler lock and sleep.
    ///
    /// Matches upstream constructor:
    /// - Locks the scheduler lock
    /// - Sets the hardware timer reference if timeout > 0
    /// - Returns the timer reference via `out_timer` for caller use
    pub fn new(
        scheduler_lock: &'a KAbstractSchedulerLock,
        hardware_timer: Option<&Arc<Mutex<KHardwareTimer>>>,
        thread: Arc<Mutex<KThread>>,
        timeout_tick: i64,
    ) -> (Self, Option<Arc<Mutex<KHardwareTimer>>>) {
        // Lock the scheduler.
        scheduler_lock.lock();

        // Set our timer only if the time is positive.
        let timer = if timeout_tick > 0 {
            hardware_timer.cloned()
        } else {
            None
        };

        let out_timer = timer.clone();
        (
            Self {
                m_scheduler_lock: scheduler_lock,
                m_timeout_tick: timeout_tick,
                m_thread: thread,
                m_timer: timer,
            },
            out_timer,
        )
    }

    /// Cancel the sleep — prevents the timer registration on drop.
    /// Matches upstream `CancelSleep()`.
    pub fn cancel_sleep(&mut self) {
        self.m_timeout_tick = 0;
    }

    /// Check if a timer will be registered on drop.
    pub fn has_timer(&self) -> bool {
        self.m_timeout_tick > 0
    }
}

impl Drop for KScopedSchedulerLockAndSleep<'_> {
    fn drop(&mut self) {
        // Register the sleep timer if the timeout is still positive.
        if self.m_timeout_tick > 0 {
            if let Some(ref timer) = self.m_timer {
                timer
                    .lock()
                    .unwrap()
                    .register_absolute_task(&self.m_thread, self.m_timeout_tick);
            }
        }

        // Unlock the scheduler.
        self.m_scheduler_lock.unlock();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cancel_sleep() {
        let lock = KAbstractSchedulerLock::new();
        let thread = Arc::new(Mutex::new(KThread::new()));
        let (mut slp, _timer) =
            KScopedSchedulerLockAndSleep::new(&lock, None, thread, 100);
        assert!(slp.has_timer());
        slp.cancel_sleep();
        assert!(!slp.has_timer());
    }
}
