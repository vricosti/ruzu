//! Port of zuyu/src/core/hle/kernel/k_thread_queue.h / k_thread_queue.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KThreadQueue and KThreadQueueWithoutEndWait: thread wait queue abstractions.

/// The base thread queue.
/// Matches upstream `KThreadQueue` class (k_thread_queue.h).
///
/// In upstream, methods are virtual. We use a trait to model dispatch.
pub trait ThreadQueue {
    /// Notify that a synchronization object became available.
    /// TODO: Port full implementation from k_thread_queue.cpp.
    fn notify_available(&mut self, _waiting_thread_id: u64, _wait_result: u32) {
        // Default: no-op (upstream calls EndWait)
    }

    /// End the wait for a thread.
    /// TODO: Port full implementation from k_thread_queue.cpp.
    fn end_wait(&mut self, _waiting_thread_id: u64, _wait_result: u32) {
        // Default implementation
    }

    /// Cancel the wait for a thread.
    /// TODO: Port full implementation from k_thread_queue.cpp.
    fn cancel_wait(&mut self, _waiting_thread_id: u64, _wait_result: u32, _cancel_timer_task: bool) {
        // Default implementation
    }
}

/// Base KThreadQueue holding a reference to the kernel and an optional hardware timer.
/// Matches upstream `KThreadQueue` (k_thread_queue.h).
pub struct KThreadQueue {
    // In upstream: KernelCore& m_kernel; KHardwareTimer* m_hardware_timer;
    // We use opaque ids / stubs here.
    pub hardware_timer_set: bool,
}

impl KThreadQueue {
    pub fn new() -> Self {
        Self {
            hardware_timer_set: false,
        }
    }

    pub fn set_hardware_timer(&mut self) {
        self.hardware_timer_set = true;
    }
}

impl Default for KThreadQueue {
    fn default() -> Self {
        Self::new()
    }
}

impl ThreadQueue for KThreadQueue {}

/// KThreadQueueWithoutEndWait: a queue that panics if EndWait is called.
/// Matches upstream `KThreadQueueWithoutEndWait` (k_thread_queue.h).
pub struct KThreadQueueWithoutEndWait {
    pub base: KThreadQueue,
}

impl KThreadQueueWithoutEndWait {
    pub fn new() -> Self {
        Self {
            base: KThreadQueue::new(),
        }
    }
}

impl Default for KThreadQueueWithoutEndWait {
    fn default() -> Self {
        Self::new()
    }
}

impl ThreadQueue for KThreadQueueWithoutEndWait {
    fn end_wait(&mut self, _waiting_thread_id: u64, _wait_result: u32) {
        // Upstream: ASSERT(false) — should never be called.
        panic!("KThreadQueueWithoutEndWait::end_wait should never be called");
    }
}
