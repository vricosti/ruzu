//! Port of zuyu/src/core/hle/kernel/k_light_condition_variable.h and
//! k_light_condition_variable.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-21
//!
//! KLightConditionVariable — condition variable used with KLightLock.
//! Upstream uses KThread waiter lists and scheduler locks; here we use
//! a host Condvar which gives correct blocking behavior for HLE emulation.

use std::sync::{Condvar, Mutex};

use super::k_light_lock::KLightLock;

/// KLightConditionVariable — condition variable for use with KLightLock.
///
/// Mirrors upstream `Kernel::KLightConditionVariable`.
/// Uses a host `Condvar` for thread parking instead of the kernel's
/// KScopedSchedulerLockAndSleep + KThreadQueue mechanism.
pub struct KLightConditionVariable {
    m_kernel: usize,
    /// Number of threads currently waiting.
    wait_count: Mutex<u32>,
    /// Condvar used to park/unpark waiting host threads.
    cv: Condvar,
}

impl KLightConditionVariable {
    pub fn new(kernel: usize) -> Self {
        Self {
            m_kernel: kernel,
            wait_count: Mutex::new(0),
            cv: Condvar::new(),
        }
    }

    /// Wait on this condition variable, releasing the given lock.
    ///
    /// Port of upstream `KLightConditionVariable::Wait`.
    /// Releases the KLightLock, blocks on the condvar, then re-acquires the lock.
    pub fn wait(&self, lock: &KLightLock, timeout: i64, _allow_terminating_thread: bool) {
        // Increment wait count.
        {
            let mut count = self.wait_count.lock().unwrap();
            *count += 1;
        }

        // Release the KLightLock before blocking.
        lock.unlock();

        // Block on the condvar.
        {
            let count = self.wait_count.lock().unwrap();
            if timeout > 0 {
                let timeout_dur = std::time::Duration::from_nanos(timeout as u64);
                let _result = self.cv.wait_timeout(count, timeout_dur).unwrap();
            } else if timeout < 0 {
                // Infinite wait (upstream: -1 means no timeout)
                let _result = self.cv.wait(count).unwrap();
            }
            // timeout == 0: don't wait, just check
        }

        // Re-acquire the KLightLock.
        lock.lock();
    }

    /// Wake all waiting threads.
    ///
    /// Port of upstream `KLightConditionVariable::Broadcast`.
    /// Upstream iterates the wait list and calls EndWait on each thread.
    pub fn broadcast(&self) {
        let mut count = self.wait_count.lock().unwrap();
        if *count > 0 {
            *count = 0;
            self.cv.notify_all();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_light_condition_variable_creation() {
        let cv = KLightConditionVariable::new(0);
        assert_eq!(*cv.wait_count.lock().unwrap(), 0);
    }
}
