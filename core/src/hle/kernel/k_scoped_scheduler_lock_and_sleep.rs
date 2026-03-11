//! Port of zuyu/src/core/hle/kernel/k_scoped_scheduler_lock_and_sleep.h
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! KScopedSchedulerLockAndSleep — RAII guard that locks the scheduler and
//! optionally registers a timer task on drop.
//!
//! Stubbed until KHardwareTimer, GlobalSchedulerContext, and KThread are ported.

/// KScopedSchedulerLockAndSleep — acquires the scheduler lock on construction,
/// and on drop registers an absolute timer task if the timeout is positive,
/// then releases the scheduler lock.
///
/// Mirrors upstream `Kernel::KScopedSchedulerLockAndSleep`.
pub struct KScopedSchedulerLockAndSleep {
    m_kernel: usize,
    m_timeout_tick: i64,
    /// Opaque thread handle until KThread is ported.
    // TODO: Replace with *mut KThread.
    m_thread: usize,
    /// Whether we have a timer registered.
    // TODO: Replace with Option<&KHardwareTimer> when ported.
    m_has_timer: bool,
}

impl KScopedSchedulerLockAndSleep {
    /// Create a new scoped scheduler lock and sleep.
    ///
    /// Mirrors the C++ constructor:
    /// ```cpp
    /// KScopedSchedulerLockAndSleep(KernelCore& kernel, KHardwareTimer** out_timer,
    ///                               KThread* thread, s64 timeout_tick)
    /// ```
    ///
    /// TODO: Acquire the actual scheduler lock from GlobalSchedulerContext.
    /// TODO: Set out_timer to &kernel.HardwareTimer() if timeout_tick > 0.
    pub fn new(
        kernel: usize,
        timeout_tick: i64,
        thread: usize,
    ) -> Self {
        // TODO: kernel.GlobalSchedulerContext().m_scheduler_lock.Lock();
        let has_timer = timeout_tick > 0;
        // TODO: *out_timer = if has_timer { &kernel.HardwareTimer() } else { null };

        Self {
            m_kernel: kernel,
            m_timeout_tick: timeout_tick,
            m_thread: thread,
            m_has_timer: has_timer,
        }
    }

    /// Cancel the sleep — prevents the timer registration on drop.
    /// Mirrors upstream `CancelSleep()`.
    pub fn cancel_sleep(&mut self) {
        self.m_timeout_tick = 0;
    }

    /// Get the kernel handle.
    pub fn kernel(&self) -> usize {
        self.m_kernel
    }

    /// Check if a timer will be registered on drop.
    pub fn has_timer(&self) -> bool {
        self.m_timeout_tick > 0
    }
}

impl Drop for KScopedSchedulerLockAndSleep {
    fn drop(&mut self) {
        // Register the sleep timer if the timeout is still positive.
        if self.m_timeout_tick > 0 {
            // TODO: m_timer.RegisterAbsoluteTask(m_thread, m_timeout_tick);
        }

        // Unlock the scheduler.
        // TODO: m_kernel.GlobalSchedulerContext().m_scheduler_lock.Unlock();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cancel_sleep() {
        let mut slp = KScopedSchedulerLockAndSleep::new(0, 100, 0);
        assert!(slp.has_timer());
        slp.cancel_sleep();
        assert!(!slp.has_timer());
    }
}
