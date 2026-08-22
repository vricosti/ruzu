//! Port of Eden's core/hle/kernel/k_light_condition_variable.h and
//! k_light_condition_variable.cpp.
//!
//! KLightConditionVariable is the kernel-internal condition variable paired
//! with KLightLock. Its wait list is scheduler-owned; it must not park a host
//! thread with `std::sync::Condvar`.

use std::sync::{Arc, Mutex, Weak};

use super::k_light_lock::KLightLock;
use super::k_scheduler_lock::KScopedSchedulerLock;
use super::k_scoped_scheduler_lock_and_sleep::KScopedSchedulerLockAndSleep;
use super::k_thread::{KThread, KThreadLock};
use super::k_thread_queue::{CancelWaitCallback, KThreadQueue};
use super::svc::svc_results::RESULT_TERMINATION_REQUESTED;
use crate::hle::result::RESULT_SUCCESS;

#[derive(Clone)]
struct Waiter {
    thread_id: u64,
    thread: Weak<KThreadLock>,
}

type WaiterList = Arc<Mutex<Vec<Waiter>>>;

/// Condition variable used with `KLightLock` by kernel objects.
///
/// Eden retains `KernelCore&` and an intrusive `KThread::WaiterList`. Ruzu's
/// kernel singleton supplies the scheduler/timer owners, while weak stable
/// thread owners provide the intrusive-list lifetime without retaining a
/// waiting thread beyond its kernel owner.
pub struct KLightConditionVariable {
    m_wait_list: WaiterList,
}

impl KLightConditionVariable {
    pub fn new() -> Self {
        Self {
            m_wait_list: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Sleep the current guest thread after releasing `lock`.
    ///
    /// Matches `KLightConditionVariable::Wait`: scheduler locking, termination
    /// check, lock release, waiter insertion, timer registration and lock
    /// reacquisition retain Eden's order.
    pub fn wait(&self, lock: &KLightLock, timeout: i64, allow_terminating_thread: bool) {
        let current = super::kernel::get_current_thread_pointer()
            .expect("KLightConditionVariable::wait requires a current guest thread");
        let scheduler_lock = super::kernel::scheduler_lock()
            .expect("KLightConditionVariable::wait requires an initialized scheduler lock");
        let hardware_timer = super::kernel::get_hardware_timer_arc();
        let (thread_id, thread_ptr) = {
            let thread = current.lock().unwrap();
            (thread.get_thread_id(), current.as_ref().as_ptr() as usize)
        };

        let (mut scheduler_sleep, timer) = KScopedSchedulerLockAndSleep::new(
            scheduler_lock,
            hardware_timer.as_ref(),
            thread_id,
            thread_ptr,
            timeout,
        );

        if !allow_terminating_thread && current.lock().unwrap().is_termination_requested() {
            scheduler_sleep.cancel_sleep();
            return;
        }

        lock.unlock();

        self.m_wait_list.lock().unwrap().push(Waiter {
            thread_id,
            thread: Arc::downgrade(&current),
        });

        let mut wait_queue = light_condition_variable_wait_queue(
            Arc::clone(&self.m_wait_list),
            allow_terminating_thread,
        );
        if let Some(timer) = timer {
            wait_queue.set_hardware_timer(timer);
        }
        current.lock().unwrap().begin_wait_with_queue(wait_queue);

        // Dropping the scheduler guard registers an absolute timer when
        // requested and switches away from the waiting guest fiber.
        drop(scheduler_sleep);

        lock.lock();
    }

    /// Wake every waiter in insertion order.
    pub fn broadcast(&self) {
        let scheduler_lock = super::kernel::scheduler_lock()
            .expect("KLightConditionVariable::broadcast requires an initialized scheduler lock");
        let _scheduler_guard = KScopedSchedulerLock::new(scheduler_lock);
        broadcast_waiters(&self.m_wait_list);
    }
}

impl Default for KLightConditionVariable {
    fn default() -> Self {
        Self::new()
    }
}

/// Rust equivalent of Eden's anonymous
/// `ThreadQueueImplForKLightConditionVariable`.
fn light_condition_variable_wait_queue(
    waiters: WaiterList,
    allow_terminating_thread: bool,
) -> KThreadQueue {
    let cancel_wait: CancelWaitCallback = Arc::new(
        move |waiting_thread: &mut KThread, wait_result: u32, _cancel_timer_task: bool| {
            if wait_result == RESULT_TERMINATION_REQUESTED.get_inner_value()
                && allow_terminating_thread
            {
                return false;
            }

            let waiting_thread_id = waiting_thread.get_thread_id();
            let mut waiters = waiters.lock().unwrap();
            let index = waiters
                .iter()
                .position(|waiter| waiter.thread_id == waiting_thread_id)
                .expect("cancelled light-condition-variable thread is not in its waiter list");
            waiters.remove(index);
            true
        },
    );
    KThreadQueue::with_cancel_wait_callback(None, Some(cancel_wait))
}

fn broadcast_waiters(waiters: &WaiterList) {
    loop {
        let Some(waiter) = waiters.lock().unwrap().first().cloned() else {
            break;
        };
        if let Some(thread) = waiter.thread.upgrade() {
            thread
                .lock()
                .unwrap()
                .end_wait(RESULT_SUCCESS.get_inner_value());
        }
        let removed = waiters.lock().unwrap().remove(0);
        debug_assert_eq!(removed.thread_id, waiter.thread_id);
    }
}

#[cfg(test)]
mod tests {
    use super::super::k_thread::ThreadState;
    use super::*;

    fn waiting_thread(thread_id: u64, queue: KThreadQueue) -> Arc<KThreadLock> {
        let thread = Arc::new(KThreadLock::new(KThread::new()));
        {
            let mut guard = thread.lock().unwrap();
            guard.thread_id = thread_id;
            guard.bind_self_reference(&thread);
            guard.begin_wait_with_queue(queue);
        }
        thread
    }

    #[test]
    fn cancellation_removes_waiter_before_base_transition() {
        let waiters = Arc::new(Mutex::new(Vec::new()));
        let queue = light_condition_variable_wait_queue(Arc::clone(&waiters), false);
        let thread = waiting_thread(7, queue.clone());
        waiters.lock().unwrap().push(Waiter {
            thread_id: 7,
            thread: Arc::downgrade(&thread),
        });

        queue.cancel_wait(&mut thread.lock().unwrap(), 0xCAFE, true);

        assert!(waiters.lock().unwrap().is_empty());
        assert_eq!(thread.lock().unwrap().get_state(), ThreadState::RUNNABLE);
        assert_eq!(thread.lock().unwrap().get_wait_result(), 0xCAFE);
    }

    #[test]
    fn allowed_termination_leaves_wait_owned_by_condition_variable() {
        let waiters = Arc::new(Mutex::new(Vec::new()));
        let queue = light_condition_variable_wait_queue(Arc::clone(&waiters), true);
        let thread = waiting_thread(8, queue.clone());
        waiters.lock().unwrap().push(Waiter {
            thread_id: 8,
            thread: Arc::downgrade(&thread),
        });

        queue.cancel_wait(
            &mut thread.lock().unwrap(),
            RESULT_TERMINATION_REQUESTED.get_inner_value(),
            true,
        );

        assert_eq!(waiters.lock().unwrap().len(), 1);
        assert_eq!(thread.lock().unwrap().get_state(), ThreadState::WAITING);
    }

    #[test]
    fn broadcast_wakes_all_waiters_in_place() {
        let waiters = Arc::new(Mutex::new(Vec::new()));
        let first = waiting_thread(1, KThreadQueue::new());
        let second = waiting_thread(2, KThreadQueue::new());
        waiters.lock().unwrap().extend([
            Waiter {
                thread_id: 1,
                thread: Arc::downgrade(&first),
            },
            Waiter {
                thread_id: 2,
                thread: Arc::downgrade(&second),
            },
        ]);

        broadcast_waiters(&waiters);

        assert!(waiters.lock().unwrap().is_empty());
        assert_eq!(first.lock().unwrap().get_state(), ThreadState::RUNNABLE);
        assert_eq!(second.lock().unwrap().get_state(), ThreadState::RUNNABLE);
        assert_eq!(
            first.lock().unwrap().get_wait_result(),
            RESULT_SUCCESS.get_inner_value()
        );
        assert_eq!(
            second.lock().unwrap().get_wait_result(),
            RESULT_SUCCESS.get_inner_value()
        );
    }
}
