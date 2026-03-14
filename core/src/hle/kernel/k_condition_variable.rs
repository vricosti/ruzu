//! Port of zuyu/src/core/hle/kernel/k_condition_variable.h/.cpp
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! KConditionVariable: implements condition-variable-style synchronization
//! for userspace mutexes and condition variables.

use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_thread::{KThread, ThreadWaitReasonForDebugging};
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
    waiting_thread_ids: Vec<u64>,
}

impl KConditionVariable {
    pub fn new() -> Self {
        Self {
            waiting_thread_ids: Vec::new(),
        }
    }

    // -- Arbitration (static in upstream) --

    /// Signal to the given address, releasing the lock to the next waiter.
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

            let waiter_ids = owner_thread.lock().unwrap().waiter_thread_ids().to_vec();
            let mut next_owner_thread: Option<Arc<Mutex<KThread>>> = None;
            let mut has_waiters = false;

            for waiter_thread_id in waiter_ids {
                let Some(waiter_thread) = process_guard.get_thread_by_thread_id(waiter_thread_id) else {
                    owner_thread.lock().unwrap().remove_waiter(waiter_thread_id);
                    continue;
                };

                if waiter_thread.lock().unwrap().get_address_key().get() != addr {
                    continue;
                }

                owner_thread.lock().unwrap().remove_waiter(waiter_thread_id);
                has_waiters = owner_thread
                    .lock()
                    .unwrap()
                    .waiter_thread_ids()
                    .iter()
                    .filter_map(|candidate_id| process_guard.get_thread_by_thread_id(*candidate_id))
                    .any(|thread| thread.lock().unwrap().get_address_key().get() == addr);
                next_owner_thread = Some(waiter_thread);
                break;
            }

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

        if let Some(next_owner_thread) = next_owner_thread {
            next_owner_thread
                .lock()
                .unwrap()
                .end_wait(result.get_inner_value());
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
        {
            let mut current_thread = current_thread.lock().unwrap();
            current_thread.set_user_address_key(KProcessAddress::new(addr), value);
            current_thread.begin_wait();
            current_thread.set_wait_reason_for_debugging(ThreadWaitReasonForDebugging::ConditionVar);
        }
        owner_thread.lock().unwrap().add_waiter(current_thread_id);

        RESULT_SUCCESS
    }

    // -- Condition variable --

    /// Signal up to `count` threads waiting on the given condition variable key.
    ///
    /// Note: the caller must already hold the process lock. This method takes
    /// `&mut KProcess` directly to avoid re-locking.
    pub fn signal(
        &mut self,
        mut process_guard: &mut KProcess,
        cv_key: u64,
        count: i32,
    ) -> ResultCode {
        let mut index = 0usize;
        let mut num_waiters = 0i32;

        while index < self.waiting_thread_ids.len() && (count <= 0 || num_waiters < count) {
            let waiting_thread_id = self.waiting_thread_ids[index];
            let Some(waiting_thread) = process_guard.get_thread_by_thread_id(waiting_thread_id) else {
                self.waiting_thread_ids.remove(index);
                continue;
            };

            if waiting_thread.lock().unwrap().get_condition_variable_key() != cv_key {
                index += 1;
                continue;
            }

            self.waiting_thread_ids.remove(index);

            let waiting_thread_result = self.signal_impl(&mut process_guard, &waiting_thread);
            if waiting_thread_result != RESULT_SUCCESS {
                return waiting_thread_result;
            }

            num_waiters += 1;
        }

        if !self
            .waiting_thread_ids
            .iter()
            .filter_map(|thread_id| process_guard.get_thread_by_thread_id(*thread_id))
            .any(|thread| thread.lock().unwrap().get_condition_variable_key() == cv_key)
        {
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
        let current_thread_id = current_thread.lock().unwrap().get_thread_id();
        let mut process_guard = process.lock().unwrap();

        if current_thread.lock().unwrap().is_termination_requested() {
            return RESULT_TERMINATION_REQUESTED;
        }

        {
            let mut current_thread_guard = current_thread.lock().unwrap();

            let mut has_waiters = false;
            let next_owner_thread_id = current_thread_guard.remove_user_waiter_by_key(
                &process_guard,
                KProcessAddress::new(addr),
                &mut has_waiters,
            );

            let mut next_value = 0u32;
            if let Some(next_owner_thread_id) = next_owner_thread_id {
                let Some(next_owner_thread) = process_guard.get_thread_by_thread_id(next_owner_thread_id) else {
                    return RESULT_INVALID_STATE;
                };

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
                mem.write_32(addr, next_value);
            }

            if timeout == 0 {
                return crate::hle::kernel::svc::svc_results::RESULT_TIMED_OUT;
            }

            current_thread_guard.set_condition_variable(KProcessAddress::new(addr), key, value);
            current_thread_guard.set_wait_reason_for_debugging(
                ThreadWaitReasonForDebugging::ConditionVar,
            );
            current_thread_guard.begin_wait();
            if timeout > 0 {
                current_thread_guard.sleep(timeout);
            }
        }

        self.waiting_thread_ids.push(current_thread_id);
        RESULT_SUCCESS
    }

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

        if previous_tag == INVALID_HANDLE {
            let mut waiting_thread_guard = waiting_thread.lock().unwrap();
            waiting_thread_guard.clear_condition_variable();
            waiting_thread_guard.end_wait(RESULT_SUCCESS.get_inner_value());
            return RESULT_SUCCESS;
        }

        let owner_handle = previous_tag & !HANDLE_WAIT_MASK;
        let Some(owner_object_id) = process_guard.handle_table.get_object(owner_handle) else {
            let mut waiting_thread_guard = waiting_thread.lock().unwrap();
            waiting_thread_guard.clear_condition_variable();
            waiting_thread_guard.end_wait(RESULT_INVALID_STATE.get_inner_value());
            return RESULT_INVALID_STATE;
        };
        let Some(owner_thread) = process_guard.get_thread_by_object_id(owner_object_id) else {
            let mut waiting_thread_guard = waiting_thread.lock().unwrap();
            waiting_thread_guard.clear_condition_variable();
            waiting_thread_guard.end_wait(RESULT_INVALID_STATE.get_inner_value());
            return RESULT_INVALID_STATE;
        };

        owner_thread
            .lock()
            .unwrap()
            .add_waiter(waiting_thread.lock().unwrap().get_thread_id());

        {
            let mut waiting_thread_guard = waiting_thread.lock().unwrap();
            waiting_thread_guard.clear_condition_variable();
        }

        RESULT_SUCCESS
    }

    pub fn waiting_thread_ids(&self) -> &[u64] {
        &self.waiting_thread_ids
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
    use crate::hle::kernel::k_thread::ThreadState;

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
        assert_eq!(owner.lock().unwrap().waiter_thread_ids(), &[2]);
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
        owner.lock().unwrap().add_waiter(2);

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
        assert!(owner.lock().unwrap().waiter_thread_ids().is_empty());
        assert_ne!(owner_handle, 0);
    }

}
