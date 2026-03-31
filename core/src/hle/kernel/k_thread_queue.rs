//! Port of zuyu/src/core/hle/kernel/k_thread_queue.h / k_thread_queue.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KThreadQueue and KThreadQueueWithoutEndWait: thread wait queue abstractions.

use std::sync::{Arc, Mutex};

use super::k_hardware_timer::KHardwareTimer;
use super::k_process::KProcess;
use super::k_thread::KThread;

/// Base KThreadQueue holding a reference to the kernel and an optional hardware timer.
/// Matches upstream `KThreadQueue` (k_thread_queue.h).
#[derive(Clone)]
pub struct KThreadQueue {
    // In upstream: KernelCore& m_kernel; KHardwareTimer* m_hardware_timer;
    pub hardware_timer: Option<Arc<Mutex<KHardwareTimer>>>,
    pub end_wait_allowed: bool,
    pub notify_available_impl:
        Option<fn(&KThreadQueue, &mut KThread, &mut KProcess, u64, u32) -> bool>,
    pub cancel_wait_impl: Option<fn(&mut KThread)>,
}

impl KThreadQueue {
    pub fn new() -> Self {
        Self {
            hardware_timer: None,
            end_wait_allowed: true,
            notify_available_impl: None,
            cancel_wait_impl: None,
        }
    }

    pub const fn with_callbacks(
        notify_available_impl: Option<
            fn(&KThreadQueue, &mut KThread, &mut KProcess, u64, u32) -> bool,
        >,
        cancel_wait_impl: Option<fn(&mut KThread)>,
    ) -> Self {
        Self {
            hardware_timer: None,
            end_wait_allowed: true,
            notify_available_impl,
            cancel_wait_impl,
        }
    }

    pub const fn without_end_wait(
        notify_available_impl: Option<
            fn(&KThreadQueue, &mut KThread, &mut KProcess, u64, u32) -> bool,
        >,
        cancel_wait_impl: Option<fn(&mut KThread)>,
    ) -> Self {
        Self {
            hardware_timer: None,
            end_wait_allowed: false,
            notify_available_impl,
            cancel_wait_impl,
        }
    }

    pub fn set_hardware_timer(&mut self, hardware_timer: Arc<Mutex<KHardwareTimer>>) {
        self.hardware_timer = Some(hardware_timer);
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
        thread.wait_result = wait_result;
        thread.set_state(super::k_thread::ThreadState::RUNNABLE);
        thread.clear_wait_queue();

        if let (Some(hardware_timer), Some(thread_arc)) = (
            self.hardware_timer.as_ref(),
            thread
                .self_reference
                .as_ref()
                .and_then(std::sync::Weak::upgrade),
        ) {
            let thread_id = thread.get_thread_id();
            let task_time = thread.get_timer_task_time();
            thread.set_timer_task_time(0);
            drop(thread_arc);
            hardware_timer
                .lock()
                .unwrap()
                .cancel_task_by_id(thread_id, task_time);
        }

        // Unpark the host thread that is blocked in begin_wait.
        thread.unpark_wait();
    }

    pub fn end_wait(&self, thread: &mut KThread, wait_result: u32) {
        assert!(
            self.end_wait_allowed,
            "KThreadQueueWithoutEndWait::end_wait should never be called"
        );
        self.base_end_wait(thread, wait_result);
    }

    pub fn cancel_wait(&self, thread: &mut KThread, wait_result: u32, cancel_timer_task: bool) {
        if let Some(cancel_impl) = self.cancel_wait_impl {
            cancel_impl(thread);
        }

        thread.wait_result = wait_result;
        thread.set_state(super::k_thread::ThreadState::RUNNABLE);
        thread.clear_wait_queue();

        if cancel_timer_task {
            if let (Some(hardware_timer), Some(thread_arc)) = (
                self.hardware_timer.as_ref(),
                thread
                    .self_reference
                    .as_ref()
                    .and_then(std::sync::Weak::upgrade),
            ) {
                let thread_id = thread.get_thread_id();
                let task_time = thread.get_timer_task_time();
                thread.set_timer_task_time(0);
                drop(thread_arc);
                hardware_timer
                    .lock()
                    .unwrap()
                    .cancel_task_by_id(thread_id, task_time);
            }
        }

        // Unpark the host thread that is blocked in begin_wait.
        thread.unpark_wait();
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
                hardware_timer: None,
                end_wait_allowed: false,
                notify_available_impl: None,
                cancel_wait_impl: None,
            },
        }
    }

    pub const fn with_callbacks(
        notify_available_impl: Option<
            fn(&KThreadQueue, &mut KThread, &mut KProcess, u64, u32) -> bool,
        >,
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
