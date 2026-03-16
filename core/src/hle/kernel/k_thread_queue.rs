//! Port of zuyu/src/core/hle/kernel/k_thread_queue.h / k_thread_queue.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KThreadQueue and KThreadQueueWithoutEndWait: thread wait queue abstractions.

use super::k_thread::KThread;
use super::k_process::KProcess;

/// Base KThreadQueue holding a reference to the kernel and an optional hardware timer.
/// Matches upstream `KThreadQueue` (k_thread_queue.h).
#[derive(Clone, Copy)]
pub struct KThreadQueue {
    // In upstream: KernelCore& m_kernel; KHardwareTimer* m_hardware_timer;
    // We use opaque ids / stubs here.
    pub hardware_timer_set: bool,
    pub end_wait_allowed: bool,
    pub notify_available_impl:
        Option<fn(&KThreadQueue, &mut KThread, &mut KProcess, u64, u32) -> bool>,
    pub cancel_wait_impl: Option<fn(&mut KThread)>,
}

impl KThreadQueue {
    pub fn new() -> Self {
        Self {
            hardware_timer_set: false,
            end_wait_allowed: true,
            notify_available_impl: None,
            cancel_wait_impl: None,
        }
    }

    pub const fn with_callbacks(
        notify_available_impl: Option<fn(&KThreadQueue, &mut KThread, &mut KProcess, u64, u32) -> bool>,
        cancel_wait_impl: Option<fn(&mut KThread)>,
    ) -> Self {
        Self {
            hardware_timer_set: false,
            end_wait_allowed: true,
            notify_available_impl,
            cancel_wait_impl,
        }
    }

    pub const fn without_end_wait(
        notify_available_impl: Option<fn(&KThreadQueue, &mut KThread, &mut KProcess, u64, u32) -> bool>,
        cancel_wait_impl: Option<fn(&mut KThread)>,
    ) -> Self {
        Self {
            hardware_timer_set: false,
            end_wait_allowed: false,
            notify_available_impl,
            cancel_wait_impl,
        }
    }

    pub fn set_hardware_timer(&mut self) {
        self.hardware_timer_set = true;
    }

    /// Upstream: virtual NotifyAvailable is UNREACHABLE in base KThreadQueue.
    /// Derived queues override it. We use function pointers instead.
    pub fn notify_available(
        &self,
        thread: &mut KThread,
        process: &mut KProcess,
        signaled_object_id: u64,
        wait_result: u32,
    ) -> bool {
        if thread.get_state() != super::k_thread::ThreadState::WAITING {
            return false;
        }

        if let Some(notify_impl) = self.notify_available_impl {
            notify_impl(self, thread, process, signaled_object_id, wait_result)
        } else {
            // Base KThreadQueue::NotifyAvailable is UNREACHABLE in upstream.
            // If we reach here, a queue was used without a notify_available impl.
            unreachable!("KThreadQueue::NotifyAvailable called on base queue without override");
        }
    }

    pub fn base_end_wait(&self, thread: &mut KThread, wait_result: u32) {
        // TODO: upstream cancels the hardware timer task here:
        // m_hardware_timer->CancelTask(thread);
        // This is needed once KHardwareTimer is fully ported.
        if self.hardware_timer_set {
            thread.sleep_deadline = None;
        }

        thread.wait_result = wait_result;
        thread.set_state(super::k_thread::ThreadState::RUNNABLE);
        thread.clear_wait_queue();
    }

    pub fn end_wait(&self, thread: &mut KThread, wait_result: u32) {
        assert!(self.end_wait_allowed, "KThreadQueueWithoutEndWait::end_wait should never be called");
        self.base_end_wait(thread, wait_result);
    }

    pub fn cancel_wait(&self, thread: &mut KThread, wait_result: u32, cancel_timer_task: bool) {
        if let Some(cancel_impl) = self.cancel_wait_impl {
            cancel_impl(thread);
        }

        // TODO: upstream cancels the hardware timer task here:
        // if (cancel_timer_task) { m_hardware_timer->CancelTask(thread); }
        // This is needed once KHardwareTimer is fully ported.
        if cancel_timer_task && self.hardware_timer_set {
            thread.sleep_deadline = None;
        }

        thread.wait_result = wait_result;
        thread.set_state(super::k_thread::ThreadState::RUNNABLE);
        thread.clear_wait_queue();
    }
}

/// KThreadQueueWithoutEndWait: a queue that panics if EndWait is called.
/// Matches upstream `KThreadQueueWithoutEndWait` (k_thread_queue.h).
pub struct KThreadQueueWithoutEndWait {
    pub base: KThreadQueue,
}

impl KThreadQueueWithoutEndWait {
    pub fn new() -> Self {
        Self {
            base: KThreadQueue {
                hardware_timer_set: false,
                end_wait_allowed: false,
                notify_available_impl: None,
                cancel_wait_impl: None,
            },
        }
    }

    pub const fn with_callbacks(
        notify_available_impl: Option<fn(&KThreadQueue, &mut KThread, &mut KProcess, u64, u32) -> bool>,
        cancel_wait_impl: Option<fn(&mut KThread)>,
    ) -> Self {
        Self {
            base: KThreadQueue::without_end_wait(notify_available_impl, cancel_wait_impl),
        }
    }
}

impl Default for KThreadQueue {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for KThreadQueueWithoutEndWait {
    fn default() -> Self {
        Self::new()
    }
}

impl KThreadQueueWithoutEndWait {
    pub fn end_wait(&self, _waiting_thread: &mut KThread, _wait_result: u32) {
        // Upstream: ASSERT(false) — should never be called.
        panic!("KThreadQueueWithoutEndWait::end_wait should never be called");
    }
}
