//! Port of zuyu/src/core/hle/kernel/k_light_condition_variable.h and
//! k_light_condition_variable.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! KLightConditionVariable — condition variable used with KLightLock.
//! The Wait and Broadcast methods interact with the scheduler and thread queues.
//!
//! Stubbed until KThread, KScheduler, KScopedSchedulerLockAndSleep, and
//! KThreadQueue are ported.

use super::k_light_lock::KLightLock;

/// KLightConditionVariable — condition variable for use with KLightLock.
///
/// Mirrors upstream `Kernel::KLightConditionVariable`.
/// The wait list uses KThread::WaiterList (intrusive list) in C++;
/// here we use a Vec as a placeholder.
pub struct KLightConditionVariable {
    /// Opaque kernel handle until KernelCore is ported.
    // TODO: Replace with reference to KernelCore.
    m_kernel: usize,
    /// Wait list of threads. Upstream uses KThread::WaiterList (intrusive linked list).
    // TODO: Replace with proper intrusive list of KThread when KThread is ported.
    m_wait_list: Vec<usize>,
}

impl KLightConditionVariable {
    pub fn new(kernel: usize) -> Self {
        Self {
            m_kernel: kernel,
            m_wait_list: Vec::new(),
        }
    }

    /// Wait on this condition variable, releasing the given lock.
    ///
    /// Mirrors upstream `KLightConditionVariable::Wait(KLightLock* lock, s64 timeout, bool allow_terminating_thread)`.
    ///
    /// TODO: Requires KThread, KScopedSchedulerLockAndSleep, KThreadQueue.
    pub fn wait(&mut self, lock: &KLightLock, _timeout: i64, _allow_terminating_thread: bool) {
        // TODO: Full implementation requires:
        // 1. KScopedSchedulerLockAndSleep to acquire scheduler lock
        // 2. Add current thread to m_wait_list
        // 3. Thread begins waiting via wait_queue
        // 4. On wake, re-acquire the lock

        // Temporary: just release and re-acquire the lock.
        lock.unlock();
        // In a real implementation, the thread would be put to sleep here.
        lock.lock();
    }

    /// Wake all waiting threads.
    ///
    /// Mirrors upstream `KLightConditionVariable::Broadcast()`.
    ///
    /// TODO: Requires KScopedSchedulerLock and KThread.
    pub fn broadcast(&mut self) {
        // TODO: KScopedSchedulerLock lk(m_kernel);
        // Signal all threads and clear the wait list.
        // for thread in m_wait_list.drain(..) {
        //     thread.end_wait(ResultSuccess);
        // }
        self.m_wait_list.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_light_condition_variable_creation() {
        let cv = KLightConditionVariable::new(0);
        assert!(cv.m_wait_list.is_empty());
    }
}
