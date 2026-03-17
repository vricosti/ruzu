//! Port of zuyu/src/core/hle/kernel/k_condition_variable.h/.cpp
//! Status: Partial (core wait/signal logic ported, scheduler/runtime wiring simplified)
//! Derniere synchro: 2026-03-17
//!
//! KConditionVariable: implements condition-variable-style synchronization
//! for userspace mutexes and condition variables.

use std::collections::{BTreeSet, HashMap};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_thread_queue::KThreadQueue;
use crate::hle::kernel::k_thread::{
    ConditionVariableThreadKey, KThread, ThreadWaitReasonForDebugging,
};
use crate::hle::kernel::k_typed_address::KProcessAddress;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::kernel::svc_common::{HANDLE_WAIT_MASK, INVALID_HANDLE};
use crate::hle::kernel::svc::svc_results::{
    RESULT_INVALID_CURRENT_MEMORY, RESULT_INVALID_HANDLE, RESULT_INVALID_STATE,
    RESULT_TERMINATION_REQUESTED,
};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Condition variable for thread synchronization.
///
/// Upstream stores a ThreadTree (condition-variable variant of
/// KThread::ConditionVariableThreadTreeType), a reference to System,
/// and a reference to KernelCore.
pub struct KConditionVariable {
    waiting_threads: ConditionVariableThreadTree,
}

#[derive(Default)]
struct ConditionVariableThreadTree {
    ordered: BTreeSet<ConditionVariableThreadKey>,
    by_thread_id: HashMap<u64, ConditionVariableThreadKey>,
}

struct ThreadQueueImplForKConditionVariableWaitForAddress;

impl ThreadQueueImplForKConditionVariableWaitForAddress {
    fn queue() -> KThreadQueue {
        KThreadQueue::with_callbacks(None, Some(Self::cancel_wait))
    }

    /// Upstream CancelWait: removes waiter from owner, then delegates to
    /// base KThreadQueue::CancelWait (which sets RUNNABLE + clears queue).
    /// Upstream does NOT call ClearAddressArbiter() here — the arbiter state
    /// is cleared by the base queue's state transition.
    fn cancel_wait(waiting_thread: &mut KThread) {
        if let Some(owner_thread) = waiting_thread.get_lock_owner() {
            owner_thread
                .lock()
                .unwrap()
                .remove_waiter_by_thread_id(waiting_thread.thread_id);
        }
        // Note: upstream does not clear address arbiter here; the base
        // KThreadQueue::CancelWait handles the state transition.
    }
}

struct ThreadQueueImplForKConditionVariableWaitConditionVariable;

impl ThreadQueueImplForKConditionVariableWaitConditionVariable {
    fn queue() -> KThreadQueue {
        KThreadQueue::with_callbacks(None, Some(Self::cancel_wait))
    }

    fn cancel_wait(waiting_thread: &mut KThread) {
        if let Some(owner_thread) = waiting_thread.get_lock_owner() {
            owner_thread
                .lock()
                .unwrap()
                .remove_waiter_by_thread_id(waiting_thread.thread_id);
        }

        if matches!(
            waiting_thread.get_condition_variable_tree(),
            Some(crate::hle::kernel::k_thread::ConditionVariableTreeState::ConditionVariable)
        ) {
            if let Some(parent) = waiting_thread.parent.as_ref().and_then(|parent| parent.upgrade()) {
                parent
                    .lock()
                    .unwrap()
                    .remove_condition_variable_waiter(waiting_thread.thread_id);
            }
            waiting_thread.clear_condition_variable();
        }
    }
}

impl KConditionVariable {
    pub fn new() -> Self {
        Self {
            waiting_threads: ConditionVariableThreadTree::default(),
        }
    }

    // -- Arbitration (static in upstream) --

    /// Signal to the given address, releasing the lock to the next waiter.
    ///
    /// Upstream wraps in KScopedSchedulerLock. In our single-core cooperative
    /// model, only one guest thread runs at a time (protected by process lock),
    /// so the scheduler lock is not strictly needed yet. Wire it when
    /// KConditionVariable gains a KernelCore reference.
    pub fn signal_to_address(
        process: &Arc<Mutex<KProcess>>,
        current_thread: &Arc<Mutex<KThread>>,
        addr: u64,
    ) -> ResultCode {
        let current_thread_id = current_thread.lock().unwrap().get_thread_id();

        let (next_owner_thread, result) = {
            let mut process_guard = process.lock().unwrap();
            let Some(owner_thread) = process_guard.get_thread_by_thread_id(current_thread_id) else {
                return RESULT_INVALID_HANDLE;
            };

            let mut has_waiters = false;
            let next_owner_result = owner_thread.lock().unwrap().remove_waiter_by_key(
                KProcessAddress::new(addr),
                false, // user address key
                &mut has_waiters,
            );

            // If there are remaining waiters, transfer the lock info to the next owner.
            let next_owner_thread: Option<Arc<Mutex<KThread>>> =
                if let Some((next_owner_id, _priority)) = next_owner_result {
                    if has_waiters {
                        // Take the transfer lock info and give it to the new owner.
                        if let Some(lock_info) =
                            owner_thread.lock().unwrap().take_transfer_lock_info()
                        {
                            if let Some(next_thread) =
                                process_guard.get_thread_by_thread_id(next_owner_id)
                            {
                                next_thread.lock().unwrap().add_held_lock(lock_info);
                            }
                        }
                    }
                    // Clear the next owner's waiting_lock_info.
                    if let Some(next_thread) =
                        process_guard.get_thread_by_thread_id(next_owner_id)
                    {
                        next_thread.lock().unwrap().set_waiting_lock_info(None);
                        Some(next_thread)
                    } else {
                        None
                    }
                } else {
                    None
                };

            let next_value = if let Some(thread) = next_owner_thread.as_ref() {
                let mut next_value = thread.lock().unwrap().get_address_key_value();
                if has_waiters {
                    next_value |= HANDLE_WAIT_MASK;
                }
                next_value
            } else {
                INVALID_HANDLE
            };

            let result = if process_guard.process_memory.read().unwrap().is_valid_range(addr, 4) {
                process_guard.process_memory.write().unwrap().write_32(addr, next_value);
                RESULT_SUCCESS
            } else {
                RESULT_INVALID_CURRENT_MEMORY
            };

            (next_owner_thread, result)
        };

        if let Some(ref next_owner_thread) = next_owner_thread {
            let thread_id = next_owner_thread.lock().unwrap().get_thread_id();
            next_owner_thread
                .lock()
                .unwrap()
                .end_wait(result.get_inner_value());
            // Thread became RUNNABLE — add to PQ.
            process.lock().unwrap().push_back_to_priority_queue(thread_id);
        }

        result
    }

    /// Wait for the lock at the given address.
    ///
    /// Matches upstream `KConditionVariable::WaitForAddress()`:
    /// if the tag at `addr` equals `(handle | HANDLE_WAIT_MASK)`, the calling
    /// thread is added as a waiter on the owner thread and enters WAITING state.
    /// In upstream, the thread truly blocks until ArbitrateUnlock signals it.
    ///
    /// In our cooperative model, `begin_wait()` sets the state but doesn't
    /// truly suspend — the scheduler handoff after the SVC must handle this.
    ///
    /// Upstream wraps in KScopedSchedulerLock. In our single-core cooperative
    /// model, only one guest thread runs at a time (protected by process lock),
    /// so the scheduler lock is not strictly needed yet. Wire it when
    /// KConditionVariable gains a KernelCore reference.
    pub fn wait_for_address(
        process: &Arc<Mutex<KProcess>>,
        current_thread: &Arc<Mutex<KThread>>,
        handle: Handle,
        addr: u64,
        value: u32,
    ) -> ResultCode {
        let current_thread_id = current_thread.lock().unwrap().get_thread_id();

        let mut process_guard = process.lock().unwrap();
        if current_thread.lock().unwrap().is_termination_requested() {
            return RESULT_TERMINATION_REQUESTED;
        }

        // Read the tag from userspace.
        let test_tag = {
            let mem = process_guard.process_memory.read().unwrap();
            if !mem.is_valid_range(addr, 4) {
                return RESULT_INVALID_CURRENT_MEMORY;
            }
            mem.read_32(addr)
        };

        // If the tag isn't (handle | HANDLE_WAIT_MASK), we're done.
        if test_tag != (handle | HANDLE_WAIT_MASK) {
            return RESULT_SUCCESS;
        }

        // Get the lock owner thread.
        let Some(owner_object_id) = process_guard.handle_table.get_object(handle) else {
            return RESULT_INVALID_HANDLE;
        };
        let Some(owner_thread) = process_guard.get_thread_by_object_id(owner_object_id) else {
            return RESULT_INVALID_HANDLE;
        };

        // Update the lock: set address key on current thread and add as waiter.
        // Thread leaves RUNNABLE → remove from PQ.
        process_guard.remove_from_priority_queue(current_thread_id);
        let owner_thread_id = owner_thread.lock().unwrap().get_thread_id();
        Self::begin_wait_for_address(current_thread, owner_thread_id, addr, value);
        {
            let ct = current_thread.lock().unwrap();
            let priority = ct.get_priority();
            let address_key = ct.get_address_key();
            let is_kernel = ct.get_is_kernel_address_key();
            drop(ct);
            owner_thread
                .lock()
                .unwrap()
                .add_waiter(current_thread_id, priority, address_key, is_kernel);
        }

        // Upstream calls owner_thread->Close() here to release the handle
        // reference. In Rust, Arc reference counting handles this automatically
        // when owner_thread goes out of scope.

        RESULT_SUCCESS
    }

    // -- Condition variable --

    /// Signal up to `count` threads waiting on the given condition variable key.
    ///
    /// Note: the caller must already hold the process lock. This method takes
    /// `&mut KProcess` directly to avoid re-locking.
    ///
    /// Upstream wraps in KScopedSchedulerLock. See signal_to_address comment.
    pub fn signal(
        &mut self,
        mut process_guard: &mut KProcess,
        cv_key: u64,
        count: i32,
    ) -> ResultCode {
        let mut num_waiters = 0i32;

        while count <= 0 || num_waiters < count {
            let Some(waiting_thread_key) = self.waiting_threads.nfind_key(cv_key) else {
                break;
            };
            if waiting_thread_key.cv_key != cv_key {
                break;
            }

            let waiting_thread_id = waiting_thread_key.thread_id;
            let Some(waiting_thread) = process_guard.get_thread_by_thread_id(waiting_thread_id) else {
                self.waiting_threads.erase(waiting_thread_key);
                continue;
            };

            if waiting_thread.lock().unwrap().get_condition_variable_key() != cv_key {
                self.waiting_threads.erase(waiting_thread_key);
                continue;
            }

            self.waiting_threads.erase(waiting_thread_key);
            waiting_thread.lock().unwrap().clear_condition_variable();

            let waiting_thread_result = self.signal_impl(&mut process_guard, &waiting_thread);
            if waiting_thread_result != RESULT_SUCCESS {
                return waiting_thread_result;
            }

            num_waiters += 1;
        }

        if !matches!(
            self.waiting_threads.nfind_key(cv_key),
            Some(waiting_thread_key) if waiting_thread_key.cv_key == cv_key
        ) {
            if process_guard.process_memory.read().unwrap().is_valid_range(cv_key, 4) {
                process_guard
                    .process_memory
                    .write()
                    .unwrap()
                    .write_32(cv_key, 0);
            }
        }

        RESULT_SUCCESS
    }

    /// Wait on the condition variable.
    pub fn wait(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread: &Arc<Mutex<KThread>>,
        addr: u64,
        key: u64,
        value: u32,
        timeout: i64,
    ) -> ResultCode {
        let mut process_guard = process.lock().unwrap();
        self.wait_locked(&mut process_guard, current_thread, addr, key, value, timeout)
    }

    /// Upstream wraps in KScopedSchedulerLockAndSleep for atomic lock + sleep.
    /// In our single-core cooperative model, the process lock provides
    /// equivalent protection. Wire KScopedSchedulerLockAndSleep when
    /// KConditionVariable gains a KernelCore reference.
    pub fn wait_locked(
        &mut self,
        process_guard: &mut KProcess,
        current_thread: &Arc<Mutex<KThread>>,
        addr: u64,
        key: u64,
        value: u32,
        timeout: i64,
    ) -> ResultCode {
        let current_thread_id = current_thread.lock().unwrap().get_thread_id();

        if current_thread.lock().unwrap().is_termination_requested() {
            return RESULT_TERMINATION_REQUESTED;
        }

        {
            let mut has_waiters = false;
            let next_owner_result = current_thread.lock().unwrap().remove_waiter_by_key(
                KProcessAddress::new(addr),
                false, // user address key
                &mut has_waiters,
            );

            // If there are remaining waiters, transfer the lock info to the next owner.
            if has_waiters {
                if let Some(lock_info) = current_thread.lock().unwrap().take_transfer_lock_info() {
                    if let Some((next_id, _)) = next_owner_result {
                        if let Some(next_thread) = process_guard.get_thread_by_thread_id(next_id) {
                            next_thread.lock().unwrap().add_held_lock(lock_info);
                        }
                    }
                }
            }

            let mut next_value = 0u32;
            if let Some((next_owner_thread_id, _priority)) = next_owner_result {
                let Some(next_owner_thread) = process_guard.get_thread_by_thread_id(next_owner_thread_id) else {
                    return RESULT_INVALID_STATE;
                };

                // Clear waiting lock info on the next owner.
                next_owner_thread.lock().unwrap().set_waiting_lock_info(None);

                next_value = next_owner_thread.lock().unwrap().get_address_key_value();
                if has_waiters {
                    next_value |= HANDLE_WAIT_MASK;
                }

                next_owner_thread
                    .lock()
                    .unwrap()
                    .end_wait(RESULT_SUCCESS.get_inner_value());
            }

            if !process_guard.process_memory.read().unwrap().is_valid_range(key, 4)
                || !process_guard.process_memory.read().unwrap().is_valid_range(addr, 4)
            {
                return RESULT_INVALID_CURRENT_MEMORY;
            }

            {
                let mut mem = process_guard.process_memory.write().unwrap();
                mem.write_32(key, 1);
                // Upstream: std::atomic_thread_fence(std::memory_order_seq_cst)
                // ensures the cv_key write is visible before the address write.
                // In our single-threaded guest model this is a no-op, but we
                // preserve the ordering by writing key before addr.
                std::sync::atomic::fence(std::sync::atomic::Ordering::SeqCst);
                mem.write_32(addr, next_value);
            }

            if timeout == 0 {
                return crate::hle::kernel::svc::svc_results::RESULT_TIMED_OUT;
            }
        }

        // Thread leaves RUNNABLE → remove from PQ.
        process_guard.remove_from_priority_queue(current_thread_id);

        Self::begin_wait_condition_variable(current_thread, addr, key, value, timeout);
        let thread_key = current_thread.lock().unwrap().condition_variable_tree_key();
        self.waiting_threads.insert(thread_key);
        RESULT_SUCCESS
    }

    /// Upstream: SignalImpl uses UpdateLockAtomic with exclusive monitor
    /// (ExclusiveRead32/ExclusiveWrite32) for CAS-like atomic address tag
    /// updates. In our single-core cooperative model, only one guest thread
    /// runs at a time, so plain read/write is sufficient. Implement the
    /// exclusive monitor retry loop if multi-core guest execution is added.
    fn signal_impl(
        &mut self,
        mut process_guard: &mut KProcess,
        waiting_thread: &Arc<Mutex<KThread>>,
    ) -> ResultCode {
        let (address, own_tag, previous_tag) = {
            let waiting_thread_guard = waiting_thread.lock().unwrap();
            let address = waiting_thread_guard.get_address_key();
            let own_tag = waiting_thread_guard.get_address_key_value();

            let mem = process_guard.process_memory.read().unwrap();
            if !mem.is_valid_range(address.get(), 4) {
                return RESULT_INVALID_CURRENT_MEMORY;
            }
            let previous_tag = mem.read_32(address.get());
            (address, own_tag, previous_tag)
        };

        let updated_value = if previous_tag == INVALID_HANDLE {
            own_tag
        } else {
            previous_tag | HANDLE_WAIT_MASK
        };

        process_guard
            .process_memory
            .write()
            .unwrap()
            .write_32(address.get(), updated_value);

        let waiting_thread_id = waiting_thread.lock().unwrap().get_thread_id();

        if previous_tag == INVALID_HANDLE {
            waiting_thread
                .lock()
                .unwrap()
                .end_wait(RESULT_SUCCESS.get_inner_value());
            // Thread became RUNNABLE — add to PQ.
            process_guard.push_back_to_priority_queue(waiting_thread_id);
            return RESULT_SUCCESS;
        }

        let owner_handle = previous_tag & !HANDLE_WAIT_MASK;
        let Some(owner_object_id) = process_guard.handle_table.get_object(owner_handle) else {
            waiting_thread
                .lock()
                .unwrap()
                .end_wait(RESULT_INVALID_STATE.get_inner_value());
            process_guard.push_back_to_priority_queue(waiting_thread_id);
            return RESULT_INVALID_STATE;
        };
        let Some(owner_thread) = process_guard.get_thread_by_object_id(owner_object_id) else {
            waiting_thread
                .lock()
                .unwrap()
                .end_wait(RESULT_INVALID_STATE.get_inner_value());
            process_guard.push_back_to_priority_queue(waiting_thread_id);
            return RESULT_INVALID_STATE;
        };

        {
            let wt = waiting_thread.lock().unwrap();
            let wt_id = wt.get_thread_id();
            let wt_priority = wt.get_priority();
            let wt_address_key = wt.get_address_key();
            let wt_is_kernel = wt.get_is_kernel_address_key();
            drop(wt);

            owner_thread.lock().unwrap().add_waiter(
                wt_id,
                wt_priority,
                wt_address_key,
                wt_is_kernel,
            );

            let owner_id = owner_thread.lock().unwrap().get_thread_id();
            waiting_thread
                .lock()
                .unwrap()
                .set_waiting_lock_owner_thread_id(Some(owner_id));
        }

        RESULT_SUCCESS
    }

    pub fn before_update_priority(&mut self, thread_id: u64) {
        self.waiting_threads.erase_by_thread_id(thread_id);
    }

    pub fn after_update_priority(&mut self, thread_key: ConditionVariableThreadKey) {
        if thread_key.cv_key != 0 {
            self.waiting_threads.insert(thread_key);
        }
    }

    pub fn remove_waiter(&mut self, thread_id: u64) {
        self.waiting_threads.erase_by_thread_id(thread_id);
    }

    pub fn waiting_thread_ids(&self) -> Vec<u64> {
        self.waiting_threads
            .ordered
            .iter()
            .map(|entry| entry.thread_id)
            .collect()
    }

    fn begin_wait_for_address(
        current_thread: &Arc<Mutex<KThread>>,
        owner_thread_id: u64,
        addr: u64,
        value: u32,
    ) {
        let mut current_thread = current_thread.lock().unwrap();
        current_thread.set_user_address_key(KProcessAddress::new(addr), value);
        current_thread.set_address_arbiter(addr);
        current_thread.set_waiting_lock_owner_thread_id(Some(owner_thread_id));
        current_thread.begin_wait_with_queue(
            ThreadQueueImplForKConditionVariableWaitForAddress::queue(),
        );
        current_thread.set_wait_reason_for_debugging(ThreadWaitReasonForDebugging::ConditionVar);
    }

    fn begin_wait_condition_variable(
        current_thread: &Arc<Mutex<KThread>>,
        addr: u64,
        key: u64,
        value: u32,
        timeout: i64,
    ) {
        let mut current_thread = current_thread.lock().unwrap();
        current_thread.set_condition_variable(KProcessAddress::new(addr), key, value);
        current_thread.set_waiting_lock_owner_thread_id(None);
        current_thread.begin_wait_with_queue(
            ThreadQueueImplForKConditionVariableWaitConditionVariable::queue(),
        );
        current_thread.set_wait_reason_for_debugging(ThreadWaitReasonForDebugging::ConditionVar);
        if timeout > 0 {
            current_thread.set_wait_result(RESULT_SUCCESS.get_inner_value());
            let timeout_ns = u64::try_from(timeout).unwrap_or(u64::MAX);
            current_thread.sleep_deadline = Some(
                Instant::now()
                    .checked_add(Duration::from_nanos(timeout_ns))
                    .unwrap_or_else(|| Instant::now() + Duration::from_secs(365 * 24 * 60 * 60)),
            );
        }
    }
}

impl ConditionVariableThreadTree {
    fn insert(&mut self, key: ConditionVariableThreadKey) {
        self.by_thread_id.insert(key.thread_id, key);
        self.ordered.insert(key);
    }

    fn erase(&mut self, key: ConditionVariableThreadKey) {
        self.by_thread_id.remove(&key.thread_id);
        self.ordered.remove(&key);
    }

    fn erase_by_thread_id(&mut self, thread_id: u64) {
        if let Some(existing) = self.by_thread_id.remove(&thread_id) {
            self.ordered.remove(&existing);
        }
    }

    fn nfind_key(&self, cv_key: u64) -> Option<ConditionVariableThreadKey> {
        self.ordered
            .range(
                ConditionVariableThreadKey {
                    cv_key,
                    priority: i32::MIN,
                    thread_id: 0,
                }..,
            )
            .next()
            .copied()
    }
}

impl Default for KConditionVariable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::kernel::k_scheduler::KScheduler;
    use crate::hle::kernel::k_thread::{ConditionVariableTreeState, ThreadState};

    fn setup_threads() -> (Arc<Mutex<KProcess>>, Arc<Mutex<KThread>>, Arc<Mutex<KThread>>, Handle, u64) {
        let process = Arc::new(Mutex::new(KProcess::new()));
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        scheduler.lock().unwrap().initialize(1, 0, 0);
        {
            let mut process_guard = process.lock().unwrap();
            process_guard.attach_scheduler(&scheduler);
            process_guard.bind_self_reference(&process);
            process_guard.initialize_handle_table();
            process_guard.allocate_code_memory(0x1000, 0x4000);
        }

        let owner = Arc::new(Mutex::new(KThread::new()));
        {
            let mut owner_guard = owner.lock().unwrap();
            owner_guard.object_id = 10;
            owner_guard.thread_id = 1;
            owner_guard.parent = Some(Arc::downgrade(&process));
            owner_guard.scheduler = Some(Arc::downgrade(&scheduler));
            owner_guard.set_state(ThreadState::RUNNABLE);
        }

        let waiter = Arc::new(Mutex::new(KThread::new()));
        {
            let mut waiter_guard = waiter.lock().unwrap();
            waiter_guard.object_id = 11;
            waiter_guard.thread_id = 2;
            waiter_guard.parent = Some(Arc::downgrade(&process));
            waiter_guard.scheduler = Some(Arc::downgrade(&scheduler));
            waiter_guard.set_state(ThreadState::RUNNABLE);
        }

        let owner_handle = {
            let mut process_guard = process.lock().unwrap();
            process_guard.register_thread_object(owner.clone());
            process_guard.register_thread_object(waiter.clone());
            process_guard.handle_table.add(10).unwrap()
        };

        (process, owner, waiter, owner_handle, 0x1800)
    }

    #[test]
    fn wait_for_address_enqueues_waiter_on_owner_thread() {
        let (process, owner, waiter, owner_handle, address) = setup_threads();
        process
            .lock()
            .unwrap()
            .process_memory
            .write()
            .unwrap()
            .write_32(address, owner_handle | HANDLE_WAIT_MASK);

        let result = KConditionVariable::wait_for_address(
            &process,
            &waiter,
            owner_handle,
            address,
            0x1234,
        );

        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(waiter.lock().unwrap().get_state(), ThreadState::WAITING);
        assert_eq!(
            waiter.lock().unwrap().get_wait_reason_for_debugging(),
            ThreadWaitReasonForDebugging::ConditionVar
        );
        assert_eq!(owner.lock().unwrap().waiter_thread_ids(), vec![2]);
    }

    #[test]
    fn signal_to_address_wakes_next_waiter_and_updates_tag() {
        let (process, owner, waiter, owner_handle, address) = setup_threads();
        {
            let mut waiter_guard = waiter.lock().unwrap();
            waiter_guard.set_user_address_key(KProcessAddress::new(address), 0xCAFE);
            waiter_guard.begin_wait();
            waiter_guard.set_wait_reason_for_debugging(ThreadWaitReasonForDebugging::ConditionVar);
        }
        {
            let wt = waiter.lock().unwrap();
            let addr_key = wt.get_address_key();
            let is_kernel = wt.get_is_kernel_address_key();
            let priority = wt.get_priority();
            drop(wt);
            owner.lock().unwrap().add_waiter(2, priority, addr_key, is_kernel);
            waiter.lock().unwrap().set_waiting_lock_owner_thread_id(Some(1));
        }

        let result = KConditionVariable::signal_to_address(&process, &owner, address);

        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(waiter.lock().unwrap().get_state(), ThreadState::RUNNABLE);
        assert_eq!(waiter.lock().unwrap().wait_result, RESULT_SUCCESS.get_inner_value());
        assert_eq!(
            process
                .lock()
                .unwrap()
                .process_memory
                .read()
                .unwrap()
                .read_32(address),
            0xCAFE
        );
        assert!(owner.lock().unwrap().waiter_thread_ids().is_empty(), "owner should have no waiters after signal");
        assert_ne!(owner_handle, 0);
    }

    #[test]
    fn wait_with_timeout_keeps_condvar_queue_until_timer_cancels_it() {
        let (process, _owner, waiter, _owner_handle, address) = setup_threads();
        let key = 0x1c00;
        let result = process
            .lock()
            .unwrap()
            .wait_condition_variable(&waiter, address, key, 0x1234, 1);

        assert_eq!(result, RESULT_SUCCESS.get_inner_value());
        assert!(waiter.lock().unwrap().has_wait_queue());
        assert_eq!(process.lock().unwrap().cond_var.waiting_thread_ids(), vec![2]);

        waiter.lock().unwrap().on_timer();

        let waiter_guard = waiter.lock().unwrap();
        assert_eq!(waiter_guard.get_state(), ThreadState::RUNNABLE);
        assert!(!waiter_guard.has_wait_queue());
        assert_eq!(waiter_guard.wait_result, crate::hle::kernel::svc::svc_results::RESULT_TIMED_OUT.get_inner_value());
        assert_eq!(waiter_guard.condvar_tree_state, ConditionVariableTreeState::None);
        drop(waiter_guard);
        assert!(process.lock().unwrap().cond_var.waiting_thread_ids().is_empty());
    }

    #[test]
    fn condition_variable_waiters_reorder_on_priority_change() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        scheduler.lock().unwrap().initialize(1, 0, 0);
        {
            let mut process_guard = process.lock().unwrap();
            process_guard.attach_scheduler(&scheduler);
            process_guard.bind_self_reference(&process);
            process_guard.initialize_handle_table();
            process_guard.allocate_code_memory(0x1000, 0x4000);
        }

        let waiter_a = Arc::new(Mutex::new(KThread::new()));
        {
            let mut waiter_a_guard = waiter_a.lock().unwrap();
            waiter_a_guard.object_id = 11;
            waiter_a_guard.thread_id = 2;
            waiter_a_guard.parent = Some(Arc::downgrade(&process));
            waiter_a_guard.scheduler = Some(Arc::downgrade(&scheduler));
            waiter_a_guard.set_state(ThreadState::RUNNABLE);
            waiter_a_guard.set_base_priority(20);
        }

        let waiter_b = Arc::new(Mutex::new(KThread::new()));
        {
            let mut waiter_b_guard = waiter_b.lock().unwrap();
            waiter_b_guard.object_id = 12;
            waiter_b_guard.thread_id = 3;
            waiter_b_guard.parent = Some(Arc::downgrade(&process));
            waiter_b_guard.scheduler = Some(Arc::downgrade(&scheduler));
            waiter_b_guard.set_state(ThreadState::RUNNABLE);
            waiter_b_guard.set_base_priority(30);
        }

        {
            let mut process_guard = process.lock().unwrap();
            process_guard.register_thread_object(waiter_a.clone());
            process_guard.register_thread_object(waiter_b.clone());
        }

        let key = 0x1c00;
        {
            let mut process_guard = process.lock().unwrap();
            let mut cond_var = std::mem::take(&mut process_guard.cond_var);
            cond_var.wait_locked(&mut process_guard, &waiter_a, 0x1800, key, 0x1111, -1);
            cond_var.wait_locked(&mut process_guard, &waiter_b, 0x1810, key, 0x2222, -1);
            process_guard.cond_var = cond_var;
            assert_eq!(process_guard.cond_var.waiting_thread_ids(), vec![2, 3]);
        }

        waiter_b.lock().unwrap().set_base_priority(10);

        assert_eq!(process.lock().unwrap().cond_var.waiting_thread_ids(), vec![3, 2]);
    }

}
