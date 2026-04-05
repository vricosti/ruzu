//! Port of zuyu/src/core/hle/kernel/k_condition_variable.h/.cpp
//! Status: Ported (matches upstream structure)
//! Derniere synchro: 2026-03-21
//!
//! KConditionVariable: implements condition-variable-style synchronization
//! for userspace mutexes and condition variables.
//!
//! Upstream holds `System& m_system`, `KernelCore& m_kernel`, and
//! `ThreadTree m_tree`. In Rust, the System/KernelCore references are
//! passed by callers (process + thread Arcs) because the kernel singleton
//! pattern does not map directly to Rust ownership.

use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_scheduler::KScheduler;
use crate::hle::kernel::k_thread::{
    ConditionVariableThreadKey, KThread, ThreadWaitReasonForDebugging,
};
use crate::hle::kernel::k_thread_queue::KThreadQueue;
use crate::hle::kernel::k_typed_address::KProcessAddress;
use crate::hle::kernel::svc::svc_results::{
    RESULT_INVALID_CURRENT_MEMORY, RESULT_INVALID_HANDLE, RESULT_INVALID_STATE,
    RESULT_TERMINATION_REQUESTED,
};
use crate::hle::kernel::svc_common::Handle;
use crate::hle::kernel::svc_common::{HANDLE_WAIT_MASK, INVALID_HANDLE};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn deadline_from_timeout_tick(timeout_tick: i64, current_tick: Option<i64>) -> Option<Instant> {
    if timeout_tick <= 0 {
        return None;
    }

    let relative_ns = match current_tick {
        Some(now_tick) => timeout_tick.saturating_sub(now_tick).max(0) as u64,
        None => u64::try_from(timeout_tick).unwrap_or(u64::MAX),
    };

    Some(
        Instant::now()
            .checked_add(Duration::from_nanos(relative_ns))
            .unwrap_or_else(|| Instant::now() + Duration::from_secs(365 * 24 * 60 * 60)),
    )
}

/// Condition variable for thread synchronization.
///
/// Upstream class: `Kernel::KConditionVariable`
/// Holds a ThreadTree (BTreeSet-based condition variable thread tree),
/// matching upstream's `ThreadTree m_tree`.
pub struct KConditionVariable {
    waiting_threads: ConditionVariableThreadTree,
}

#[derive(Default)]
struct ConditionVariableThreadTree {
    ordered: BTreeMap<(u64, i32), Vec<u64>>,
    by_thread_id: HashMap<u64, ConditionVariableThreadKey>,
}

// ---------------------------------------------------------------------------
// Thread queue implementations matching upstream anonymous namespace classes
// ---------------------------------------------------------------------------

/// Matches upstream `ThreadQueueImplForKConditionVariableWaitForAddress`.
/// CancelWait removes the waiter from its lock owner, then delegates to base.
struct ThreadQueueImplForKConditionVariableWaitForAddress;

impl ThreadQueueImplForKConditionVariableWaitForAddress {
    fn queue() -> KThreadQueue {
        KThreadQueue::with_callbacks(None, Some(Self::cancel_wait))
    }

    /// Upstream CancelWait: removes waiter from owner, then delegates to
    /// base KThreadQueue::CancelWait (which sets RUNNABLE + clears queue).
    fn cancel_wait(waiting_thread: &mut KThread) {
        if let Some(owner_thread) = waiting_thread.get_lock_owner() {
            owner_thread
                .lock()
                .unwrap()
                .remove_waiter_by_thread_id(waiting_thread.thread_id);
        }
    }
}

/// Matches upstream `ThreadQueueImplForKConditionVariableWaitConditionVariable`.
/// CancelWait removes the waiter from its lock owner (if any), removes from
/// the condvar tree if waiting on a condition variable, then delegates to base.
struct ThreadQueueImplForKConditionVariableWaitConditionVariable;

impl ThreadQueueImplForKConditionVariableWaitConditionVariable {
    fn queue() -> KThreadQueue {
        KThreadQueue::with_callbacks(None, Some(Self::cancel_wait))
    }

    fn cancel_wait(waiting_thread: &mut KThread) {
        // Remove the thread as a waiter from its owner.
        if let Some(owner_thread) = waiting_thread.get_lock_owner() {
            owner_thread
                .lock()
                .unwrap()
                .remove_waiter_by_thread_id(waiting_thread.thread_id);
        }

        // If the thread is waiting on a condvar, remove it from the tree.
        if waiting_thread.is_waiting_for_condition_variable() {
            if let Some(parent) = waiting_thread
                .parent
                .as_ref()
                .and_then(|parent| parent.upgrade())
            {
                parent
                    .lock()
                    .unwrap()
                    .remove_condition_variable_waiter(waiting_thread.thread_id);
            }
            waiting_thread.clear_condition_variable();
        }
    }
}

// ---------------------------------------------------------------------------
// Helper functions matching upstream anonymous namespace
// ---------------------------------------------------------------------------

/// Read a u32 from process memory.
/// Matches upstream `ReadFromUser(KernelCore&, u32*, KProcessAddress)`.
fn read_from_user(process_guard: &KProcess, address: u64) -> Option<u32> {
    if let Some(memory) = process_guard.page_table.get_base().m_memory.as_ref() {
        Some(memory.lock().unwrap().read_32(address))
    } else {
        let mem = process_guard.process_memory.read().unwrap();
        if !mem.is_valid_range(address, 4) {
            return None;
        }
        Some(mem.read_32(address))
    }
}

/// Write a u32 to process memory.
/// Matches upstream `WriteToUser(KernelCore&, KProcessAddress, const u32*)`.
fn write_to_user(process_guard: &KProcess, address: u64, value: u32) -> bool {
    if let Some(memory) = process_guard.page_table.get_base().m_memory.as_ref() {
        memory.lock().unwrap().write_32(address, value);
        true
    } else if process_guard
        .process_memory
        .read()
        .unwrap()
        .is_valid_range(address, 4)
    {
        process_guard
            .process_memory
            .write()
            .unwrap()
            .write_32(address, value);
        true
    } else {
        false
    }
}

/// Atomic update of the lock tag at `address`.
/// Matches upstream `UpdateLockAtomic(KernelCore&, u32*, KProcessAddress, u32, u32)`.
///
/// Upstream uses ExclusiveMonitor CAS loop (ExclusiveRead32/ExclusiveWrite32).
/// In the current implementation, process memory access is serialized by the
/// process mutex, so a plain read-modify-write is sufficient. When multi-core
/// guest execution with a real exclusive monitor is implemented, this should
/// use the ExclusiveMonitor CAS loop.
fn update_lock_atomic(
    process_guard: &KProcess,
    address: u64,
    if_zero: u32,
    new_orr_mask: u32,
) -> Option<u32> {
    // Load the value from the address.
    let expected = read_from_user(process_guard, address)?;

    // Orr in the new mask.
    let mut value = expected | new_orr_mask;

    // If the value is zero, use the if_zero value, otherwise use the newly orr'd value.
    if expected == 0 {
        value = if_zero;
    }

    // Store the value.
    if !write_to_user(process_guard, address, value) {
        return None;
    }

    // Return the original value.
    Some(expected)
}

// ---------------------------------------------------------------------------
// KConditionVariable implementation
// ---------------------------------------------------------------------------

impl KConditionVariable {
    pub fn new() -> Self {
        Self {
            waiting_threads: ConditionVariableThreadTree::default(),
        }
    }

    // -- Arbitration (static in upstream) --

    /// Signal to the given address, releasing the lock to the next waiter.
    /// Matches upstream `KConditionVariable::SignalToAddress(KernelCore&, KProcessAddress)`.
    ///
    /// Upstream wraps the body in `KScopedSchedulerLock sl(kernel)`.
    /// The Rust equivalent protection is provided by the process mutex
    /// held by the caller.
    pub fn signal_to_address(
        process: &Arc<Mutex<KProcess>>,
        current_thread: &Arc<Mutex<KThread>>,
        addr: u64,
    ) -> ResultCode {
        let current_thread_id = current_thread.lock().unwrap().get_thread_id();

        let (next_owner_thread, result) = {
            let mut process_guard = process.lock().unwrap();
            let Some(owner_thread) = process_guard.get_thread_by_thread_id(current_thread_id)
            else {
                return RESULT_INVALID_HANDLE;
            };

            // Remove waiter thread.
            // Matches upstream: owner_thread->RemoveUserWaiterByKey(&has_waiters, addr)
            let mut has_waiters = false;
            let next_owner_result = owner_thread.lock().unwrap().remove_waiter_by_key(
                KProcessAddress::new(addr),
                false, // user address key (RemoveUserWaiterByKey)
                &mut has_waiters,
            );

            // If there are remaining waiters, transfer the lock info to the next owner.
            let next_owner_thread: Option<Arc<Mutex<KThread>>> =
                if let Some((next_owner_id, _priority, transfer_lock_info)) = next_owner_result {
                    if let Some(lock_info) = transfer_lock_info {
                        if let Some(next_thread) =
                            process_guard.get_thread_by_thread_id(next_owner_id)
                        {
                            next_thread.lock().unwrap().add_held_lock(lock_info);
                        }
                    }
                    if let Some(next_thread) = process_guard.get_thread_by_thread_id(next_owner_id)
                    {
                        next_thread.lock().unwrap().set_waiting_lock_info(None);
                        Some(next_thread)
                    } else {
                        None
                    }
                } else {
                    None
                };

            // Determine the next tag.
            // Matches upstream: next_value = next_owner_thread->GetAddressKeyValue();
            //                   if (has_waiters) next_value |= HandleWaitMask;
            let next_value = if let Some(thread) = next_owner_thread.as_ref() {
                let mut next_value = thread.lock().unwrap().get_address_key_value();
                if has_waiters {
                    next_value |= HANDLE_WAIT_MASK;
                }
                next_value
            } else {
                // No next owner: upstream writes 0 (next_value is default-initialized to 0).
                0
            };

            // Synchronize memory before proceeding.
            // Matches upstream: std::atomic_thread_fence(std::memory_order_seq_cst)
            std::sync::atomic::fence(std::sync::atomic::Ordering::SeqCst);

            // Write the value to userspace.
            let result = if write_to_user(&process_guard, addr, next_value) {
                RESULT_SUCCESS
            } else {
                RESULT_INVALID_CURRENT_MEMORY
            };

            log::trace!(
                "KConditionVariable::signal_to_address owner_tid={} addr=0x{:X} next_owner={:?} has_result={:#x} next_value=0x{:08X}",
                current_thread_id,
                addr,
                next_owner_thread.as_ref().map(|t| t.lock().unwrap().get_thread_id()),
                result.get_inner_value(),
                next_value
            );

            (next_owner_thread, result)
        };

        // If necessary, signal the next owner thread.
        // Matches upstream: if (next_owner_thread != nullptr) next_owner_thread->EndWait(result);
        if let Some(ref next_owner_thread) = next_owner_thread {
            next_owner_thread
                .lock()
                .unwrap()
                .end_wait(result.get_inner_value());
        }

        result
    }

    pub(crate) fn wait_for_current_thread(
        process: &Arc<Mutex<KProcess>>,
        current_thread: &Arc<Mutex<KThread>>,
    ) {
        let scheduler = super::kernel::get_kernel_ref()
            .and_then(|kernel| kernel.current_scheduler().cloned())
            .or_else(|| {
                current_thread
                    .lock()
                    .unwrap()
                    .scheduler
                    .as_ref()
                    .and_then(|scheduler| scheduler.upgrade())
            })
            .or_else(|| {
                process
                    .lock()
                    .unwrap()
                    .scheduler
                    .as_ref()
                    .and_then(|scheduler| scheduler.upgrade())
            });

        while current_thread.lock().unwrap().get_state() == super::k_thread::ThreadState::WAITING {
            if let Some(scheduler) = scheduler.as_ref() {
                scheduler.lock().unwrap().request_schedule();

                let sched_ptr = {
                    let mut scheduler_guard = scheduler.lock().unwrap();
                    &mut *scheduler_guard as *mut KScheduler
                };

                unsafe {
                    KScheduler::reschedule_current_core_raw(sched_ptr);
                }
            } else {
                std::thread::yield_now();
            }
        }
    }

    /// Wait for the lock at the given address.
    /// Matches upstream `KConditionVariable::WaitForAddress(KernelCore&, Handle, KProcessAddress, u32)`.
    ///
    /// If the tag at `addr` equals `(handle | HANDLE_WAIT_MASK)`, the calling
    /// thread is added as a waiter on the owner thread and enters WAITING state.
    ///
    /// Upstream wraps the body in `KScopedSchedulerLock sl(kernel)`.
    pub fn wait_for_address(
        process: &Arc<Mutex<KProcess>>,
        current_thread: &Arc<Mutex<KThread>>,
        handle: Handle,
        addr: u64,
        value: u32,
    ) -> ResultCode {
        let current_thread_id = current_thread.lock().unwrap().get_thread_id();

        let mut process_guard = process.lock().unwrap();

        // Check if the thread should terminate.
        if current_thread.lock().unwrap().is_termination_requested() {
            return RESULT_TERMINATION_REQUESTED;
        }

        // Read the tag from userspace.
        // Matches upstream: ReadFromUser(kernel, &test_tag, addr)
        let Some(test_tag) = read_from_user(&process_guard, addr) else {
            return RESULT_INVALID_CURRENT_MEMORY;
        };

        // If the tag isn't the handle (with wait mask), we're done.
        // Matches upstream: R_SUCCEED_IF(test_tag != (handle | HandleWaitMask))
        if test_tag != (handle | HANDLE_WAIT_MASK) {
            log::trace!(
                "KConditionVariable::wait_for_address no_wait tid={} handle=0x{:08X} addr=0x{:X} value=0x{:08X} test_tag=0x{:08X}",
                current_thread_id,
                handle,
                addr,
                value,
                test_tag
            );
            return RESULT_SUCCESS;
        }

        // Get the lock owner thread.
        // Matches upstream: GetCurrentProcess(kernel).GetHandleTable()
        //                      .GetObjectWithoutPseudoHandle<KThread>(handle)
        let Some(owner_object_id) = process_guard.handle_table.get_object(handle) else {
            return RESULT_INVALID_HANDLE;
        };
        let Some(owner_thread) = process_guard.get_thread_by_object_id(owner_object_id) else {
            return RESULT_INVALID_HANDLE;
        };

        // Update the lock: set address key on current thread and add as waiter.
        // Matches upstream: cur_thread->SetUserAddressKey(addr, value);
        //                   owner_thread->AddWaiter(cur_thread);
        process_guard.remove_from_priority_queue(current_thread_id);
        let owner_thread_id = owner_thread.lock().unwrap().get_thread_id();
        Self::begin_wait_for_address(current_thread, owner_thread_id, addr, value);
        {
            let ct = current_thread.lock().unwrap();
            let priority = ct.get_priority();
            let address_key = ct.get_address_key();
            let is_kernel = ct.get_is_kernel_address_key();
            drop(ct);
            owner_thread.lock().unwrap().add_waiter(
                current_thread_id,
                priority,
                address_key,
                is_kernel,
            );
        }

        log::trace!(
            "KConditionVariable::wait_for_address sleep tid={} owner_tid={} handle=0x{:08X} addr=0x{:X} value=0x{:08X}",
            current_thread_id,
            owner_thread_id,
            handle,
            addr,
            value
        );

        // Upstream calls owner_thread->Close() here to release the handle
        // reference. In Rust, Arc reference counting handles this automatically.
        drop(process_guard);

        Self::wait_for_current_thread(process, current_thread);
        let wait_result = current_thread.lock().unwrap().get_wait_result();
        log::trace!(
            "KConditionVariable::wait_for_address woke tid={} handle=0x{:08X} addr=0x{:X} wait_result={:#x}",
            current_thread_id,
            handle,
            addr,
            wait_result
        );
        ResultCode::new(wait_result)
    }

    // -- Condition variable --

    /// Signal up to `count` threads waiting on the given condition variable key.
    /// Matches upstream `KConditionVariable::Signal(u64 cv_key, s32 count)`.
    ///
    /// The caller must already hold the process lock (passes `&mut KProcess`).
    /// Upstream wraps the body in `KScopedSchedulerLock sl(m_kernel)`.
    pub fn signal(&mut self, process_guard: &mut KProcess, cv_key: u64, count: i32) -> ResultCode {
        let mut num_waiters = 0i32;
        log::trace!(
            "KConditionVariable::signal begin cv_key=0x{:X} count={} first_waiter={:?}",
            cv_key,
            count,
            self.waiting_threads.nfind_key(cv_key).map(|key| key.thread_id)
        );

        // Iterate the tree, finding threads with matching cv_key.
        // Matches upstream: auto it = m_tree.nfind_key({cv_key, -1});
        while count <= 0 || num_waiters < count {
            let Some(waiting_thread_key) = self.waiting_threads.nfind_key(cv_key) else {
                break;
            };
            if waiting_thread_key.cv_key != cv_key {
                break;
            }

            let waiting_thread_id = waiting_thread_key.thread_id;
            let Some(waiting_thread) = process_guard.get_thread_by_thread_id(waiting_thread_id)
            else {
                self.waiting_threads.erase(waiting_thread_key);
                continue;
            };

            if waiting_thread.lock().unwrap().get_condition_variable_key() != cv_key {
                self.waiting_threads.erase(waiting_thread_key);
                continue;
            }

            // Remove from tree and clear condvar state.
            // Matches upstream: it = m_tree.erase(it);
            //                   target_thread->ClearConditionVariable();
            self.waiting_threads.erase(waiting_thread_key);
            waiting_thread.lock().unwrap().clear_condition_variable();

            // Signal the thread.
            // Matches upstream: this->SignalImpl(target_thread);
            self.signal_impl(process_guard, &waiting_thread);
            log::trace!(
                "KConditionVariable::signal woke waiter tid={} cv_key=0x{:X}",
                waiting_thread_id,
                cv_key
            );

            num_waiters += 1;
        }

        // If we have no waiters left for this key, clear the has waiter flag.
        // Matches upstream: if (it == m_tree.end() || it->GetConditionVariableKey() != cv_key)
        //                       WriteToUser(m_kernel, cv_key, &has_waiter_flag);
        if !matches!(
            self.waiting_threads.nfind_key(cv_key),
            Some(key) if key.cv_key == cv_key
        ) {
            write_to_user(process_guard, cv_key, 0);
        }

        RESULT_SUCCESS
    }

    /// Wait on the condition variable.
    /// Matches upstream `KConditionVariable::Wait(KProcessAddress addr, u64 key, u32 value, s64 timeout)`.
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
        let scheduler_lock_ptr = current_thread.lock().unwrap().scheduler_lock_ptr;
        if scheduler_lock_ptr == 0 {
            return RESULT_INVALID_STATE;
        }
        let scheduler_lock = unsafe {
            &*(scheduler_lock_ptr as *const super::k_scheduler_lock::KAbstractSchedulerLock)
        };
        let hardware_timer = super::kernel::get_hardware_timer_arc();
        let thread_ptr = {
            let mut guard = current_thread.lock().unwrap();
            (&mut *guard) as *mut KThread as usize
        };

        let result = {
            let (mut sleep_guard, timer) =
                super::k_scoped_scheduler_lock_and_sleep::KScopedSchedulerLockAndSleep::new(
                    scheduler_lock,
                    hardware_timer.as_ref(),
                    current_thread_id,
                    thread_ptr,
                    timeout,
                );
            let mut process_guard = process.lock().unwrap();
            self.wait_locked_after_sleep_guard(
                &mut process_guard,
                current_thread,
                addr,
                key,
                value,
                timeout,
                &mut sleep_guard,
                timer,
            )
        };

        if result == RESULT_SUCCESS {
            Self::wait_for_current_thread(process, current_thread);
            ResultCode::new(current_thread.lock().unwrap().get_wait_result())
        } else {
            result
        }
    }

    /// Internal wait implementation, called when the process lock is already held.
    /// Matches upstream `KConditionVariable::Wait()` body, which wraps in
    /// `KScopedSchedulerLockAndSleep`.
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
        let scheduler_lock_ptr = current_thread.lock().unwrap().scheduler_lock_ptr;
        if scheduler_lock_ptr == 0 {
            return RESULT_INVALID_STATE;
        }
        let scheduler_lock =
            unsafe { &*(scheduler_lock_ptr as *const super::k_scheduler_lock::KAbstractSchedulerLock) };
        let hardware_timer = super::kernel::get_hardware_timer_arc();
        let mut wait_queue = ThreadQueueImplForKConditionVariableWaitConditionVariable::queue();
        let thread_ptr = {
            let mut guard = current_thread.lock().unwrap();
            (&mut *guard) as *mut KThread as usize
        };

        log::trace!(
            "KConditionVariable::wait_locked tid={} before scoped_sleep_lock addr=0x{:X} key=0x{:X}",
            current_thread_id,
            addr,
            key
        );

        let (mut sleep_guard, timer) = super::k_scoped_scheduler_lock_and_sleep::KScopedSchedulerLockAndSleep::new(
            scheduler_lock,
            hardware_timer.as_ref(),
            current_thread_id,
            thread_ptr,
            timeout,
        );
        log::trace!(
            "KConditionVariable::wait_locked tid={} after scoped_sleep_lock",
            current_thread_id
        );

        self.wait_locked_after_sleep_guard(
            process_guard,
            current_thread,
            addr,
            key,
            value,
            timeout,
            &mut sleep_guard,
            timer,
        )
    }

    pub(crate) fn wait_locked_after_sleep_guard(
        &mut self,
        process_guard: &mut KProcess,
        current_thread: &Arc<Mutex<KThread>>,
        addr: u64,
        key: u64,
        value: u32,
        timeout: i64,
        sleep_guard: &mut super::k_scoped_scheduler_lock_and_sleep::KScopedSchedulerLockAndSleep<'_>,
        timer: Option<Arc<super::k_hardware_timer::KHardwareTimer>>,
    ) -> ResultCode {
        let current_thread_id = current_thread.lock().unwrap().get_thread_id();
        let mut wait_queue = ThreadQueueImplForKConditionVariableWaitConditionVariable::queue();

        // Check that the thread isn't terminating.
        // Matches upstream: if (cur_thread->IsTerminationRequested()) { slp.CancelSleep(); ... }
        if current_thread.lock().unwrap().is_termination_requested() {
            sleep_guard.cancel_sleep();
            return RESULT_TERMINATION_REQUESTED;
        }

        // Update the value and process for the next owner.
        // Matches upstream block inside the KScopedSchedulerLockAndSleep scope.
        {
            // Remove waiter thread.
            // Matches upstream: cur_thread->RemoveUserWaiterByKey(&has_waiters, addr)
            let mut has_waiters = false;
            let next_owner_result = current_thread.lock().unwrap().remove_waiter_by_key(
                KProcessAddress::new(addr),
                false, // user address key
                &mut has_waiters,
            );
            log::trace!(
                "KConditionVariable::wait_locked tid={} after remove_waiter_by_key next_owner={:?} has_waiters={}",
                current_thread_id,
                next_owner_result.as_ref().map(|(tid, _, _)| *tid),
                has_waiters
            );

            // Update for the next owner thread.
            // Matches upstream: next_value = next_owner_thread->GetAddressKeyValue();
            let mut next_value = 0u32;
            if let Some((next_owner_thread_id, _priority, transfer_lock_info)) = next_owner_result {
                let Some(next_owner_thread) =
                    process_guard.get_thread_by_thread_id(next_owner_thread_id)
                else {
                    sleep_guard.cancel_sleep();
                    return RESULT_INVALID_STATE;
                };

                if let Some(lock_info) = transfer_lock_info {
                    next_owner_thread.lock().unwrap().add_held_lock(lock_info);
                }

                next_owner_thread
                    .lock()
                    .unwrap()
                    .set_waiting_lock_info(None);

                next_value = next_owner_thread.lock().unwrap().get_address_key_value();
                if has_waiters {
                    next_value |= HANDLE_WAIT_MASK;
                }

                // Wake up the next owner.
                // Matches upstream: next_owner_thread->EndWait(ResultSuccess);
                next_owner_thread
                    .lock()
                    .unwrap()
                    .end_wait(RESULT_SUCCESS.get_inner_value());
            }

            // Write to the cv key.
            // Matches upstream: WriteToUser(m_kernel, key, &has_waiter_flag);
            //                   std::atomic_thread_fence(std::memory_order_seq_cst);
            if !write_to_user(process_guard, key, 1) {
                sleep_guard.cancel_sleep();
                return RESULT_INVALID_CURRENT_MEMORY;
            }
            std::sync::atomic::fence(std::sync::atomic::Ordering::SeqCst);
            log::trace!(
                "KConditionVariable::wait_locked tid={} after write_to_user key",
                current_thread_id
            );

            // Write the value to userspace.
            // Matches upstream: WriteToUser(m_kernel, addr, &next_value)
            if !write_to_user(process_guard, addr, next_value) {
                sleep_guard.cancel_sleep();
                return RESULT_INVALID_CURRENT_MEMORY;
            }
            log::trace!(
                "KConditionVariable::wait_locked tid={} after write_to_user addr next_value=0x{:08X}",
                current_thread_id,
                next_value
            );
        }

        // If timeout is zero, time out.
        // Matches upstream: R_UNLESS(timeout != 0, ResultTimedOut);
        if timeout == 0 {
            sleep_guard.cancel_sleep();
            return crate::hle::kernel::svc::svc_results::RESULT_TIMED_OUT;
        }

        if let Some(timer) = timer {
            wait_queue.set_hardware_timer(timer);
        }
        log::trace!(
            "KConditionVariable::wait_locked tid={} before begin_wait_condition_variable",
            current_thread_id
        );

        // Update condition variable tracking.
        // Matches upstream: cur_thread->SetConditionVariable(&m_tree, addr, key, value);
        //                   m_tree.insert(*cur_thread);
        Self::begin_wait_condition_variable(current_thread, wait_queue, addr, key, value, timeout);
        let thread_key = current_thread.lock().unwrap().condition_variable_tree_key();
        self.waiting_threads.insert(thread_key);
        log::trace!(
            "KConditionVariable::wait_locked enqueued tid={} addr=0x{:X} key=0x{:X} tag=0x{:08X} timeout={}",
            current_thread_id,
            addr,
            key,
            value,
            timeout
        );

        RESULT_SUCCESS
    }

    /// Core signal implementation.
    /// Matches upstream `KConditionVariable::SignalImpl(KThread* thread)`.
    ///
    /// Uses UpdateLockAtomic to atomically update the lock tag, then either
    /// wakes the thread (if nobody held the lock) or adds it as a waiter on
    /// the previous lock owner.
    fn signal_impl(&mut self, process_guard: &mut KProcess, waiting_thread: &Arc<Mutex<KThread>>) {
        // Update the tag.
        // Matches upstream: KProcessAddress address = thread->GetAddressKey();
        //                   u32 own_tag = thread->GetAddressKeyValue();
        let (address, own_tag) = {
            let wt = waiting_thread.lock().unwrap();
            (wt.get_address_key(), wt.get_address_key_value())
        };

        // UpdateLockAtomic: atomically read-modify-write the lock tag.
        // Matches upstream: UpdateLockAtomic(m_kernel, &prev_tag, address, own_tag, HandleWaitMask)
        // TODO(upstream): CanAccessAtomic(..) check is also a TODO upstream.
        let can_access = true;
        let prev_tag = if can_access {
            update_lock_atomic(process_guard, address.get(), own_tag, HANDLE_WAIT_MASK)
        } else {
            None
        };

        let waiting_thread_id = waiting_thread.lock().unwrap().get_thread_id();
        log::trace!(
            "KConditionVariable::signal_impl tid={} addr=0x{:X} own_tag=0x{:08X} prev_tag={:?}",
            waiting_thread_id,
            address.get(),
            own_tag,
            prev_tag
        );

        if let Some(prev_tag) = prev_tag {
            if prev_tag == INVALID_HANDLE {
                // If nobody held the lock previously, we're all good.
                // Matches upstream: thread->EndWait(ResultSuccess);
                waiting_thread
                    .lock()
                    .unwrap()
                    .end_wait(RESULT_SUCCESS.get_inner_value());
            } else {
                // Get the previous owner.
                // Matches upstream: GetObjectWithoutPseudoHandle<KThread>(prev_tag & ~HandleWaitMask)
                let owner_handle = (prev_tag & !HANDLE_WAIT_MASK) as Handle;
                let owner_thread = process_guard
                    .handle_table
                    .get_object(owner_handle)
                    .and_then(|obj_id| process_guard.get_thread_by_object_id(obj_id));

                if let Some(owner_thread) = owner_thread {
                    // Add the thread as a waiter on the owner.
                    // Matches upstream: owner_thread->AddWaiter(thread);
                    //                   owner_thread->Close();
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
                } else {
                    // The lock was tagged with a thread that doesn't exist.
                    // Matches upstream: thread->EndWait(ResultInvalidState);
                    waiting_thread
                        .lock()
                        .unwrap()
                        .end_wait(RESULT_INVALID_STATE.get_inner_value());
                }
            }
        } else {
            // If the address wasn't accessible, note so.
            // Matches upstream: thread->EndWait(ResultInvalidCurrentMemory);
            waiting_thread
                .lock()
                .unwrap()
                .end_wait(RESULT_INVALID_CURRENT_MEMORY.get_inner_value());
        }
    }

    /// Remove a thread from the condition variable tree before updating its priority.
    /// Matches upstream free function `BeforeUpdatePriority(kernel, tree, thread)`.
    pub fn before_update_priority(&mut self, thread_id: u64) {
        self.waiting_threads.erase_by_thread_id(thread_id);
    }

    /// Re-insert a thread into the condition variable tree after updating its priority.
    /// Matches upstream free function `AfterUpdatePriority(kernel, tree, thread)`.
    pub fn after_update_priority(&mut self, thread_key: ConditionVariableThreadKey) {
        if thread_key.cv_key != 0 {
            self.waiting_threads.insert(thread_key);
        }
    }

    /// Remove a waiter from the condition variable tree.
    pub fn remove_waiter(&mut self, thread_id: u64) {
        self.waiting_threads.erase_by_thread_id(thread_id);
    }

    /// Get all waiting thread IDs in tree order.
    pub fn waiting_thread_ids(&self) -> Vec<u64> {
        self.waiting_threads.ordered_thread_ids()
    }

    /// Set up a thread to wait for an address lock.
    /// Matches upstream logic inside WaitForAddress:
    ///   cur_thread->SetUserAddressKey(addr, value);
    ///   owner_thread->AddWaiter(cur_thread);
    ///   cur_thread->BeginWait(&wait_queue);
    fn begin_wait_for_address(
        current_thread: &Arc<Mutex<KThread>>,
        owner_thread_id: u64,
        addr: u64,
        value: u32,
    ) {
        let mut current_thread = current_thread.lock().unwrap();
        current_thread.set_user_address_key(KProcessAddress::new(addr), value);
        current_thread.set_waiting_lock_owner_thread_id(Some(owner_thread_id));
        current_thread
            .begin_wait_with_queue(ThreadQueueImplForKConditionVariableWaitForAddress::queue());
        current_thread.set_wait_reason_for_debugging(ThreadWaitReasonForDebugging::ConditionVar);
    }

    /// Set up a thread to wait on a condition variable.
    /// Matches upstream logic inside Wait:
    ///   cur_thread->SetConditionVariable(&m_tree, addr, key, value);
    ///   m_tree.insert(*cur_thread);
    ///   cur_thread->BeginWait(&wait_queue);
    fn begin_wait_condition_variable(
        current_thread: &Arc<Mutex<KThread>>,
        wait_queue: KThreadQueue,
        addr: u64,
        key: u64,
        value: u32,
        timeout: i64,
    ) {
        let mut current_thread = current_thread.lock().unwrap();
        current_thread.set_condition_variable(KProcessAddress::new(addr), key, value);
        current_thread.set_waiting_lock_owner_thread_id(None);
        current_thread.begin_wait_with_queue(wait_queue);
        current_thread.set_wait_reason_for_debugging(ThreadWaitReasonForDebugging::ConditionVar);
        if timeout > 0 {
            current_thread.sleep_deadline =
                deadline_from_timeout_tick(timeout, super::kernel::get_current_hardware_tick());
        }
    }
}

// ---------------------------------------------------------------------------
// ConditionVariableThreadTree — BTreeSet-based tree matching upstream ThreadTree
// ---------------------------------------------------------------------------

impl ConditionVariableThreadTree {
    fn insert(&mut self, key: ConditionVariableThreadKey) {
        if let Some(existing) = self.by_thread_id.insert(key.thread_id, key) {
            self.remove_from_bucket(existing.cv_key, existing.priority, existing.thread_id);
        }

        self.ordered
            .entry((key.cv_key, key.priority))
            .or_default()
            .push(key.thread_id);
    }

    fn erase(&mut self, key: ConditionVariableThreadKey) {
        self.erase_by_thread_id(key.thread_id);
    }

    fn erase_by_thread_id(&mut self, thread_id: u64) {
        if let Some(existing) = self.by_thread_id.remove(&thread_id) {
            self.remove_from_bucket(existing.cv_key, existing.priority, thread_id);
        }
    }

    /// Find the first entry with cv_key >= the given key.
    /// Matches upstream `m_tree.nfind_key({cv_key, -1})`.
    fn nfind_key(&self, cv_key: u64) -> Option<ConditionVariableThreadKey> {
        self.ordered
            .range((cv_key, i32::MIN)..)
            .find_map(|(_, thread_ids)| {
                thread_ids
                    .first()
                    .and_then(|thread_id| self.by_thread_id.get(thread_id))
                    .copied()
            })
    }

    fn ordered_thread_ids(&self) -> Vec<u64> {
        self.ordered
            .values()
            .flat_map(|thread_ids| thread_ids.iter().copied())
            .collect()
    }

    fn bucket_count(&self) -> usize {
        self.ordered.len()
    }

    fn remove_from_bucket(&mut self, cv_key: u64, priority: i32, thread_id: u64) {
        let bucket_key = (cv_key, priority);
        let should_remove_bucket = if let Some(thread_ids) = self.ordered.get_mut(&bucket_key) {
            if let Some(position) = thread_ids.iter().position(|existing| *existing == thread_id) {
                thread_ids.remove(position);
            }
            thread_ids.is_empty()
        } else {
            false
        };

        if should_remove_bucket {
            self.ordered.remove(&bucket_key);
        }
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

    fn setup_threads() -> (
        Arc<Mutex<KProcess>>,
        Arc<Mutex<KThread>>,
        Arc<Mutex<KThread>>,
        Handle,
        u64,
    ) {
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

        let result =
            KConditionVariable::wait_for_address(&process, &waiter, owner_handle, address, 0x1234);

        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(waiter.lock().unwrap().get_state(), ThreadState::WAITING);
        assert_eq!(
            waiter.lock().unwrap().get_wait_reason_for_debugging(),
            ThreadWaitReasonForDebugging::ConditionVar
        );
        assert!(
            !waiter.lock().unwrap().is_waiting_for_address_arbiter(),
            "WaitForAddress should not set address-arbiter tree state on this path"
        );
        assert_eq!(owner.lock().unwrap().waiter_thread_ids(), vec![2]);
    }

    #[test]
    fn wait_for_address_returns_after_signal_result() {
        let (process, owner, waiter, owner_handle, address) = setup_threads();
        process
            .lock()
            .unwrap()
            .process_memory
            .write()
            .unwrap()
            .write_32(address, owner_handle | HANDLE_WAIT_MASK);

        let process_for_signal = Arc::clone(&process);
        let owner_for_signal = Arc::clone(&owner);
        let signal_thread = std::thread::spawn(move || {
            std::thread::sleep(Duration::from_millis(10));
            let result = KConditionVariable::signal_to_address(
                &process_for_signal,
                &owner_for_signal,
                address,
            );
            assert_eq!(result, RESULT_SUCCESS);
        });

        let result =
            KConditionVariable::wait_for_address(&process, &waiter, owner_handle, address, 0x1234);

        signal_thread.join().unwrap();

        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(waiter.lock().unwrap().get_state(), ThreadState::RUNNABLE);
    }

    #[test]
    fn thread_tree_reinsert_replaces_stale_key_for_same_thread() {
        let mut tree = ConditionVariableThreadTree::default();

        tree.insert(ConditionVariableThreadKey {
            cv_key: 0x1000,
            priority: 10,
            thread_id: 42,
        });
        tree.insert(ConditionVariableThreadKey {
            cv_key: 0x2000,
            priority: 10,
            thread_id: 42,
        });

        assert_eq!(
            tree.nfind_key(0x1000).map(|key| (key.cv_key, key.thread_id)),
            Some((0x2000, 42))
        );
        assert_eq!(
            tree.nfind_key(0x2000).map(|key| (key.cv_key, key.thread_id)),
            Some((0x2000, 42))
        );
        assert_eq!(tree.bucket_count(), 1);
        assert_eq!(tree.by_thread_id.len(), 1);
    }

    #[test]
    fn thread_tree_allows_multiple_waiters_with_same_key_and_priority() {
        let mut tree = ConditionVariableThreadTree::default();

        tree.insert(ConditionVariableThreadKey {
            cv_key: 0x3000,
            priority: 10,
            thread_id: 41,
        });
        tree.insert(ConditionVariableThreadKey {
            cv_key: 0x3000,
            priority: 10,
            thread_id: 42,
        });

        assert_eq!(tree.bucket_count(), 1);
        assert_eq!(tree.by_thread_id.len(), 2);
        assert_eq!(tree.ordered_thread_ids(), vec![41, 42]);
        assert_eq!(
            tree.nfind_key(0x3000).map(|key| (key.cv_key, key.priority, key.thread_id)),
            Some((0x3000, 10, 41))
        );

        tree.erase_by_thread_id(41);
        assert_eq!(tree.ordered_thread_ids(), vec![42]);
        assert_eq!(
            tree.nfind_key(0x3000).map(|key| (key.cv_key, key.priority, key.thread_id)),
            Some((0x3000, 10, 42))
        );
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
            owner
                .lock()
                .unwrap()
                .add_waiter(2, priority, addr_key, is_kernel);
            waiter
                .lock()
                .unwrap()
                .set_waiting_lock_owner_thread_id(Some(1));
        }

        let result = KConditionVariable::signal_to_address(&process, &owner, address);

        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(waiter.lock().unwrap().get_state(), ThreadState::RUNNABLE);
        assert_eq!(
            waiter.lock().unwrap().wait_result,
            RESULT_SUCCESS.get_inner_value()
        );
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
        assert!(
            owner.lock().unwrap().waiter_thread_ids().is_empty(),
            "owner should have no waiters after signal"
        );
        assert_ne!(owner_handle, 0);
    }

    #[test]
    fn wait_with_timeout_keeps_condvar_queue_until_timer_cancels_it() {
        let (process, _owner, waiter, _owner_handle, address) = setup_threads();
        let key = 0x1c00;
        let result = KProcess::wait_condition_variable(&process, &waiter, address, key, 0x1234, 1);

        assert_eq!(result, RESULT_SUCCESS.get_inner_value());
        assert!(waiter.lock().unwrap().has_wait_queue());
        assert_eq!(
            process.lock().unwrap().cond_var.waiting_thread_ids(),
            vec![2]
        );

        waiter.lock().unwrap().on_timer();

        let waiter_guard = waiter.lock().unwrap();
        assert_eq!(waiter_guard.get_state(), ThreadState::RUNNABLE);
        assert!(!waiter_guard.has_wait_queue());
        assert_eq!(
            waiter_guard.wait_result,
            crate::hle::kernel::svc::svc_results::RESULT_TIMED_OUT.get_inner_value()
        );
        assert_eq!(
            waiter_guard.condvar_tree_state,
            ConditionVariableTreeState::None
        );
        drop(waiter_guard);
        assert!(process
            .lock()
            .unwrap()
            .cond_var
            .waiting_thread_ids()
            .is_empty());
    }

    #[test]
    fn process_owned_condition_variable_is_visible_to_signalers_while_waiting() {
        let (process, _owner, waiter, _owner_handle, address) = setup_threads();
        let key = 0x1c40;

        let waiter_process = Arc::clone(&process);
        let waiter_thread = Arc::clone(&waiter);
        let wait_handle = std::thread::spawn(move || {
            KProcess::wait_condition_variable(
                &waiter_process,
                &waiter_thread,
                address,
                key,
                0x1234,
                5_000_000,
            )
        });

        std::thread::sleep(Duration::from_millis(10));
        {
            let process_guard = process.lock().unwrap();
            assert_eq!(process_guard.cond_var.waiting_thread_ids(), vec![2]);
        }

        process.lock().unwrap().signal_condition_variable(key, 1);

        let result = wait_handle.join().unwrap();
        assert_eq!(result, RESULT_SUCCESS.get_inner_value());
        assert_eq!(waiter.lock().unwrap().get_state(), ThreadState::RUNNABLE);
        assert!(process
            .lock()
            .unwrap()
            .cond_var
            .waiting_thread_ids()
            .is_empty());
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

        assert_eq!(
            process.lock().unwrap().cond_var.waiting_thread_ids(),
            vec![3, 2]
        );
    }

    #[test]
    fn deadline_from_timeout_tick_uses_absolute_tick_when_available() {
        let start = Instant::now();
        let deadline = super::deadline_from_timeout_tick(250, Some(200)).unwrap();
        let remaining = deadline.saturating_duration_since(start);

        assert!(remaining <= Duration::from_micros(100));
        assert!(remaining <= Duration::from_nanos(50_000));
    }

    #[test]
    fn wait_locked_returns_only_after_signal() {
        let (process, owner, _waiter, _owner_handle, address) = setup_threads();
        let key = 0x1c00;
        let start = Instant::now();

        let process_for_signal = Arc::clone(&process);
        let signal_thread = std::thread::spawn(move || {
            std::thread::sleep(Duration::from_millis(10));
            process_for_signal
                .lock()
                .unwrap()
                .signal_condition_variable(key, 1);
        });

        let result = KProcess::wait_condition_variable(&process, &owner, address, key, 0x1111, -1);

        signal_thread.join().unwrap();

        assert_eq!(result, RESULT_SUCCESS.get_inner_value());
        assert_eq!(owner.lock().unwrap().get_state(), ThreadState::RUNNABLE);
        assert!(start.elapsed() >= Duration::from_millis(8));
    }

    #[test]
    fn wait_locked_guest_thread_path_loops_until_signal() {
        let (process, owner, _waiter, _owner_handle, address) = setup_threads();
        let key = 0x1c40;
        let start = Instant::now();

        crate::hle::kernel::kernel::set_current_emu_thread(Some(&owner));

        let process_for_signal = Arc::clone(&process);
        let signal_thread = std::thread::spawn(move || {
            std::thread::sleep(Duration::from_millis(10));
            process_for_signal
                .lock()
                .unwrap()
                .signal_condition_variable(key, 1);
        });

        let result = KProcess::wait_condition_variable(&process, &owner, address, key, 0x2222, -1);

        crate::hle::kernel::kernel::set_current_emu_thread(None);
        signal_thread.join().unwrap();

        assert_eq!(result, RESULT_SUCCESS.get_inner_value());
        assert_eq!(owner.lock().unwrap().get_state(), ThreadState::RUNNABLE);
        assert!(start.elapsed() >= Duration::from_millis(8));
    }
}
