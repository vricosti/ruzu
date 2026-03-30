//! Port of zuyu/src/core/hle/kernel/svc/svc_synchronization.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for synchronization operations (CloseHandle, ResetSignal,
//! WaitSynchronization, CancelSynchronization, SynchronizePreemptionState).

use crate::core::System;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::{Handle, ARGUMENT_HANDLE_COUNT_MAX};
use crate::hle::kernel::k_synchronization_object;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn synchronization_timeout_tick_from_ns(current_tick: i64, timeout_ns: i64) -> i64 {
    debug_assert!(timeout_ns > 0);

    let timeout = current_tick
        .saturating_add(timeout_ns)
        .saturating_add(2);
    if timeout <= 0 { i64::MAX } else { timeout }
}

/// Closes a handle.
pub fn close_handle(system: &System, handle: Handle) -> ResultCode {
    log::trace!("svc::CloseHandle closing handle 0x{:08X}", handle);

    if system.current_process_arc().lock().unwrap().handle_table.remove(handle) {
        RESULT_SUCCESS
    } else {
        RESULT_INVALID_HANDLE
    }
}

/// Clears the signaled state of an event or process.
pub fn reset_signal(system: &System, handle: Handle) -> ResultCode {
    log::debug!("svc::ResetSignal called handle 0x{:08X}", handle);

    let process_arc = system.current_process_arc();
    let mut process = process_arc.lock().unwrap();
    let Some(object_id) = process.handle_table.get_object(handle) else {
        return RESULT_INVALID_HANDLE;
    };

    if let Some(readable_event) = process.get_readable_event_by_object_id(object_id) {
        return ResultCode::new(readable_event.lock().unwrap().reset());
    }
    if process.process_id == object_id {
        return ResultCode::new(process.reset());
    }

    RESULT_INVALID_HANDLE
}

/// Wait for the given handles to synchronize, timeout after the specified nanoseconds.
pub fn wait_synchronization(
    system: &System,
    out_index: &mut i32,
    user_handles: u64,
    num_handles: i32,
    timeout_ns: i64,
) -> ResultCode {
    log::trace!(
        "svc::WaitSynchronization called user_handles=0x{:x}, num_handles={}, timeout_ns={}",
        user_handles, num_handles, timeout_ns
    );

    // Ensure number of handles is valid.
    if !(0..=ARGUMENT_HANDLE_COUNT_MAX).contains(&num_handles) {
        return RESULT_OUT_OF_RANGE;
    }

    let process_arc = system.current_process_arc();
    let mut process = process_arc.lock().unwrap();

    // Read handle array from guest memory.
    // Upstream: R_UNLESS(GetCurrentMemory(kernel).ReadBlock(user_handles, handles.data(),
    //                    sizeof(Handle) * num_handles), ResultInvalidPointer)
    let mut handles = Vec::with_capacity(num_handles as usize);
    if num_handles > 0 {
        let handle_bytes = num_handles as usize * std::mem::size_of::<Handle>();
        if let Some(memory) = process.page_table.get_base().m_memory.as_ref() {
            let m = memory.lock().unwrap();
            if !m.is_valid_virtual_address_range(user_handles, handle_bytes as u64) {
                return RESULT_INVALID_POINTER;
            }
            for i in 0..num_handles as usize {
                handles.push(m.read_32(user_handles + (i * 4) as u64));
            }
        } else {
            let mem = process.process_memory.read().unwrap();
            if !mem.is_valid_range(user_handles, handle_bytes) {
                return RESULT_INVALID_POINTER;
            }
            for i in 0..num_handles as usize {
                handles.push(mem.read_32(user_handles + (i * 4) as u64));
            }
            drop(mem);
        }
    }

    let Some(current_thread_id) = system.current_thread_id() else {
        return RESULT_INVALID_HANDLE;
    };
    let Some(current_thread) = process.get_thread_by_thread_id(current_thread_id) else {
        return RESULT_INVALID_HANDLE;
    };

    let mut object_ids = Vec::with_capacity(num_handles as usize);
    let mut object_kinds = Vec::with_capacity(num_handles as usize);
    for handle in &handles {
        let Some(object_id) = process.handle_table.get_object(*handle) else {
            return RESULT_INVALID_HANDLE;
        };
        let kind = if process.get_readable_event_by_object_id(object_id).is_some() {
            "readable_event"
        } else if process.get_thread_by_object_id(object_id).is_some() {
            "thread"
        } else if process.process_id == object_id {
            "process"
        } else if process.get_event_by_object_id(object_id).is_some() {
            "event"
        } else {
            "unknown"
        };
        object_ids.push(object_id);
        object_kinds.push(kind);
    }
    log::trace!(
        "  WaitSynchronization handles={:?} object_ids={:?} kinds={:?} timeout_ns={}",
        handles,
        object_ids,
        object_kinds,
        timeout_ns
    );
    let timeout = if timeout_ns > 0 {
        let current_tick = system
            .kernel()
            .and_then(|kernel| kernel.hardware_timer())
            .map(|timer| timer.lock().unwrap().get_tick())
            .unwrap_or(i64::MAX);
        synchronization_timeout_tick_from_ns(current_tick, timeout_ns)
    } else {
        timeout_ns
    };
    let scheduler = system.scheduler_arc().clone();
    drop(process);
    let result = k_synchronization_object::wait(
        &process_arc,
        &current_thread,
        &scheduler,
        out_index,
        object_ids,
        timeout,
    );
    log::trace!(
        "  WaitSynchronization result={:?} out_index={}",
        result,
        *out_index
    );
    result
}

/// Resumes a thread waiting on WaitSynchronization.
pub fn cancel_synchronization(system: &System, handle: Handle) -> ResultCode {
    log::trace!("svc::CancelSynchronization called handle=0x{:X}", handle);

    let process = system.current_process_arc().lock().unwrap();
    let Some(object_id) = process.handle_table.get_object(handle) else {
        return RESULT_INVALID_HANDLE;
    };
    let Some(thread) = process.get_thread_by_object_id(object_id) else {
        return RESULT_INVALID_HANDLE;
    };
    drop(process);

    thread.lock().unwrap().wait_cancel();
    system.scheduler_arc().lock().unwrap().request_schedule();
    RESULT_SUCCESS
}

/// Synchronizes preemption state.
///
/// Upstream: Lock scheduler, check if current thread is pinned on the current core,
/// clear interrupt flag, unpin.
pub fn synchronize_preemption_state(system: &System) {
    // Lock the scheduler.
    let binding = system.scheduler_arc(); let scheduler = binding.lock().unwrap();

    // Get the current core ID and current thread.
    let core_id = match system.kernel() {
        Some(k) => k.current_physical_core_index() as i32,
        None => return,
    };

    let current_thread_id = match scheduler.get_scheduler_current_thread_id() {
        Some(id) => id,
        None => return,
    };
    drop(scheduler);

    let mut process = system.current_process_arc().lock().unwrap();

    // If the current thread is pinned, unpin it.
    if let Some(pinned_id) = process.get_pinned_thread(core_id) {
        if pinned_id == current_thread_id {
            // Clear the current thread's interrupt flag.
            if let Some(thread) = process.get_thread_by_thread_id(current_thread_id) {
                thread.lock().unwrap().clear_interrupt_flag();
            }

            // Unpin the current thread.
            process.unpin_current_thread(core_id, current_thread_id);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::System;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_thread::{KThread, ThreadState};
    use crate::hle::kernel::k_worker_task_manager::KWorkerTaskManager;
    use crate::hle::kernel::svc::svc_event;
    use crate::hle::kernel::svc::svc_thread;
    use std::sync::atomic::Ordering;
    use std::sync::{Arc, Mutex};

    fn test_system() -> System {
        let mut system = System::new_for_test();

        let mut process = KProcess::new();
        process.process_id = 100;
        process.capabilities.core_mask = 0xF;
        process.capabilities.priority_mask = u64::MAX;
        process.flags = 0;
        process.initialize_handle_table();
        process.allocate_code_memory(0x200000, 0x1000);
        process.initialize_thread_local_region_base(0x240000);

        let process = Arc::new(Mutex::new(process));
        let current_thread = Arc::new(Mutex::new(KThread::new()));
        {
            let mut thread = current_thread.lock().unwrap();
            thread.thread_id = 1;
            thread.object_id = 1;
            thread
                .thread_state
                .store(ThreadState::RUNNABLE.bits(), Ordering::Relaxed);
        }
        process.lock().unwrap().register_thread_object(current_thread);

        let scheduler = Arc::new(Mutex::new(crate::hle::kernel::k_scheduler::KScheduler::new(0)));
        scheduler.lock().unwrap().initialize(1, 0, 0);
        let shared_memory = process.lock().unwrap().get_shared_memory();

        system.set_current_process_arc(process);
        system.set_scheduler_arc(scheduler);
        system.set_shared_process_memory(shared_memory);
        system.set_runtime_program_id(1);
        system.set_runtime_64bit(false);
        system
    }

    #[test]
    fn reset_signal_resets_readable_event() {
        let system = test_system();
        let mut write_handle = 0;
        let mut read_handle = 0;
        assert_eq!(
            svc_event::create_event(&system, &mut write_handle, &mut read_handle),
            RESULT_SUCCESS
        );
        assert_eq!(svc_event::signal_event(&system, write_handle), RESULT_SUCCESS);
        assert_eq!(reset_signal(&system, read_handle), RESULT_SUCCESS);
    }

    #[test]
    fn wait_synchronization_returns_signaled_index() {
        let system = test_system();
        let mut write_handle = 0;
        let mut read_handle = 0;
        assert_eq!(
            svc_event::create_event(&system, &mut write_handle, &mut read_handle),
            RESULT_SUCCESS
        );
        assert_eq!(svc_event::signal_event(&system, write_handle), RESULT_SUCCESS);

        {
            let process = system.current_process_arc().lock().unwrap();
            let mut mem = process.process_memory.write().unwrap();
            mem.write_32(0x200100, read_handle);
        }

        let mut out_index = -1;
        assert_eq!(
            wait_synchronization(&system, &mut out_index, 0x200100, 1, 0),
            RESULT_SUCCESS
        );
        assert_eq!(out_index, 0);
    }

    #[test]
    fn wait_synchronization_timeout_zero_returns_timed_out() {
        let system = test_system();
        let mut write_handle = 0;
        let mut read_handle = 0;
        assert_eq!(
            svc_event::create_event(&system, &mut write_handle, &mut read_handle),
            RESULT_SUCCESS
        );

        {
            let process = system.current_process_arc().lock().unwrap();
            let mut mem = process.process_memory.write().unwrap();
            mem.write_32(0x200100, read_handle);
        }

        let mut out_index = -1;
        assert_eq!(
            wait_synchronization(&system, &mut out_index, 0x200100, 1, 0),
            RESULT_TIMED_OUT
        );
        assert_eq!(out_index, -1);
    }

    #[test]
    fn synchronization_timeout_tick_from_ns_uses_absolute_tick() {
        assert_eq!(synchronization_timeout_tick_from_ns(100, 25), 127);
    }

    #[test]
    fn wait_synchronization_blocks_then_wakes_on_signal() {
        let system = test_system();
        let mut write_handle = 0;
        let mut read_handle = 0;
        assert_eq!(
            svc_event::create_event(&system, &mut write_handle, &mut read_handle),
            RESULT_SUCCESS
        );

        {
            let process = system.current_process_arc().lock().unwrap();
            let mut mem = process.process_memory.write().unwrap();
            mem.write_32(0x200100, read_handle);
        }

        let system_ref = crate::core::SystemRef::from_ref(&system);
        let waiter = std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(5));
            assert_eq!(svc_event::signal_event(system_ref.get(), write_handle), RESULT_SUCCESS);
        });

        let mut out_index = -1;
        assert_eq!(
            wait_synchronization(&system, &mut out_index, 0x200100, 1, -1),
            RESULT_SUCCESS
        );
        assert_eq!(out_index, 0);

        waiter.join().unwrap();

        let current_thread = {
            system.current_process_arc()
                .lock()
                .unwrap()
                .get_thread_by_thread_id(1)
                .unwrap()
        };

        let thread = current_thread.lock().unwrap();
        assert_eq!(thread.get_state(), crate::hle::kernel::k_thread::ThreadState::RUNNABLE);
        assert_eq!(thread.get_synced_index(), 0);
        assert_eq!(thread.get_wait_result(), RESULT_SUCCESS.get_inner_value());
        assert_eq!(thread.thread_context.r[0], RESULT_SUCCESS.get_inner_value() as u64);
        assert_eq!(thread.thread_context.r[1], 0);
    }

    #[test]
    fn wait_synchronization_returns_cancelled_when_wait_cancelled_is_set() {
        let system = test_system();
        let mut write_handle = 0;
        let mut read_handle = 0;
        assert_eq!(
            svc_event::create_event(&system, &mut write_handle, &mut read_handle),
            RESULT_SUCCESS
        );

        {
            let process = system.current_process_arc().lock().unwrap();
            let mut mem = process.process_memory.write().unwrap();
            mem.write_32(0x200100, read_handle);
        }

        let current_thread = {
            system.current_process_arc()
                .lock()
                .unwrap()
                .get_thread_by_thread_id(1)
                .unwrap()
        };
        current_thread.lock().unwrap().wait_cancel();

        let mut out_index = 123;
        assert_eq!(
            wait_synchronization(&system, &mut out_index, 0x200100, 1, -1),
            RESULT_CANCELLED
        );
        assert_eq!(out_index, -1);
        assert!(!current_thread.lock().unwrap().is_wait_cancelled());
        assert_eq!(
            current_thread.lock().unwrap().get_state(),
            crate::hle::kernel::k_thread::ThreadState::RUNNABLE
        );
    }

    #[test]
    fn wait_synchronization_blocks_then_wakes_on_thread_exit() {
        let system = test_system();

        let mut thread_handle = 0;
        assert_eq!(
            svc_thread::create_thread(&system, &mut thread_handle, 0x201000, 0x1234, 0x260000, 44, 0),
            RESULT_SUCCESS
        );

        {
            let process = system.current_process_arc().lock().unwrap();
            let mut mem = process.process_memory.write().unwrap();
            mem.write_32(0x200100, thread_handle);
        }

        let object_id = system
            .current_process_arc()
            .lock()
            .unwrap()
            .handle_table
            .get_object(thread_handle)
            .unwrap();
        let target_thread = system
            .current_process_arc()
            .lock()
            .unwrap()
            .get_thread_by_object_id(object_id)
            .unwrap();

        let target_thread_for_exit = target_thread.clone();
        let exiter = std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(5));
            target_thread_for_exit.lock().unwrap().exit();
            KWorkerTaskManager::wait_for_global_idle();
        });

        let mut out_index = -1;
        assert_eq!(
            wait_synchronization(&system, &mut out_index, 0x200100, 1, -1),
            RESULT_SUCCESS
        );
        assert_eq!(out_index, 0);

        exiter.join().unwrap();

        let current_thread = {
            system.current_process_arc()
                .lock()
                .unwrap()
                .get_thread_by_thread_id(1)
                .unwrap()
        };

        let thread = current_thread.lock().unwrap();
        assert_eq!(
            thread.get_state(),
            crate::hle::kernel::k_thread::ThreadState::RUNNABLE
        );
        assert_eq!(thread.get_synced_index(), 0);
        assert_eq!(thread.get_wait_result(), RESULT_SUCCESS.get_inner_value());
        assert_eq!(thread.thread_context.r[0], RESULT_SUCCESS.get_inner_value() as u64);
        assert_eq!(thread.thread_context.r[1], 0);
    }

    #[test]
    fn wait_synchronization_blocks_then_wakes_on_process_signal() {
        let system = test_system();

        let process_handle = {
            let mut process = system.current_process_arc().lock().unwrap();
            let process_id = process.process_id;
            process.state = crate::hle::kernel::k_process::ProcessState::RunningAttached;
            process.is_signaled = false;
            process.handle_table.add(process_id).unwrap()
        };

        {
            let process = system.current_process_arc().lock().unwrap();
            let mut mem = process.process_memory.write().unwrap();
            mem.write_32(0x200100, process_handle);
        }

        let mut out_index = -1;
        assert_eq!(
            wait_synchronization(&system, &mut out_index, 0x200100, 1, -1),
            RESULT_SUCCESS
        );
        assert_eq!(out_index, -1);

        let current_thread = {
            system.current_process_arc()
                .lock()
                .unwrap()
                .get_thread_by_thread_id(1)
                .unwrap()
        };
        assert_eq!(
            current_thread.lock().unwrap().get_state(),
            crate::hle::kernel::k_thread::ThreadState::WAITING
        );

        system.current_process_arc().lock().unwrap().set_debug_break();

        let thread = current_thread.lock().unwrap();
        assert_eq!(
            thread.get_state(),
            crate::hle::kernel::k_thread::ThreadState::RUNNABLE
        );
        assert_eq!(thread.get_synced_index(), 0);
        assert_eq!(thread.get_wait_result(), RESULT_SUCCESS.get_inner_value());
    }
}
