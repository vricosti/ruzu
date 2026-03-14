//! Port of zuyu/src/core/hle/kernel/svc/svc_synchronization.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for synchronization operations (CloseHandle, ResetSignal,
//! WaitSynchronization, CancelSynchronization, SynchronizePreemptionState).

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::{Handle, ARGUMENT_HANDLE_COUNT_MAX};
use crate::hle::kernel::svc_dispatch::SvcContext;
use crate::hle::kernel::k_synchronization_object;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Closes a handle.
pub fn close_handle(ctx: &SvcContext, handle: Handle) -> ResultCode {
    log::trace!("svc::CloseHandle closing handle 0x{:08X}", handle);

    if ctx.current_process.lock().unwrap().handle_table.remove(handle) {
        RESULT_SUCCESS
    } else {
        RESULT_INVALID_HANDLE
    }
}

/// Clears the signaled state of an event or process.
pub fn reset_signal(ctx: &SvcContext, handle: Handle) -> ResultCode {
    log::debug!("svc::ResetSignal called handle 0x{:08X}", handle);

    let mut process = ctx.current_process.lock().unwrap();
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
    ctx: &SvcContext,
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

    let mut process = ctx.current_process.lock().unwrap();
    let mem = process.process_memory.read().unwrap();
    let handle_bytes = num_handles as usize * std::mem::size_of::<Handle>();
    if num_handles > 0 && !mem.is_valid_range(user_handles, handle_bytes) {
        return RESULT_INVALID_POINTER;
    }

    let mut handles = Vec::with_capacity(num_handles as usize);
    for i in 0..num_handles as usize {
        handles.push(mem.read_32(user_handles + (i * 4) as u64));
    }
    drop(mem);

    let Some(current_thread_id) = ctx.current_thread_id() else {
        return RESULT_INVALID_HANDLE;
    };
    let Some(current_thread) = process.get_thread_by_thread_id(current_thread_id) else {
        return RESULT_INVALID_HANDLE;
    };

    let mut object_ids = Vec::with_capacity(num_handles as usize);
    for handle in handles {
        let Some(object_id) = process.handle_table.get_object(handle) else {
            return RESULT_INVALID_HANDLE;
        };
        object_ids.push(object_id);
    }
    k_synchronization_object::wait(
        &mut process,
        &current_thread,
        &ctx.scheduler,
        out_index,
        object_ids,
        timeout_ns,
    )
}

/// Resumes a thread waiting on WaitSynchronization.
pub fn cancel_synchronization(ctx: &SvcContext, handle: Handle) -> ResultCode {
    log::trace!("svc::CancelSynchronization called handle=0x{:X}", handle);

    let process = ctx.current_process.lock().unwrap();
    let Some(object_id) = process.handle_table.get_object(handle) else {
        return RESULT_INVALID_HANDLE;
    };
    let Some(thread) = process.get_thread_by_object_id(object_id) else {
        return RESULT_INVALID_HANDLE;
    };
    drop(process);

    thread.lock().unwrap().wait_cancel();
    ctx.scheduler.lock().unwrap().request_schedule();
    RESULT_SUCCESS
}

/// Synchronizes preemption state.
pub fn synchronize_preemption_state() {
    // TODO: Lock scheduler, check if current thread is pinned, clear interrupt flag, unpin.
    log::warn!("svc::SynchronizePreemptionState: kernel object access not yet implemented");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_thread::{KThread, ThreadState};
    use crate::hle::kernel::svc::svc_event;
    use crate::hle::kernel::svc::svc_thread;
    use std::sync::atomic::Ordering;
    use std::sync::atomic::{AtomicU32, AtomicU64};
    use std::sync::{Arc, Mutex};

    fn test_context() -> SvcContext {
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

        SvcContext {
            shared_memory,
            code_base: 0x200000,
            code_size: 0x1000,
            stack_base: 0,
            stack_size: 0,
            program_id: 1,
            tls_base: 0,
            current_process: process,
            scheduler,
            next_thread_id: Arc::new(AtomicU64::new(2)),
            next_object_id: Arc::new(AtomicU32::new(2)),
            is_64bit: false,
        }
    }

    #[test]
    fn reset_signal_resets_readable_event() {
        let ctx = test_context();
        let mut write_handle = 0;
        let mut read_handle = 0;
        assert_eq!(
            svc_event::create_event(&ctx, &mut write_handle, &mut read_handle),
            RESULT_SUCCESS
        );
        assert_eq!(svc_event::signal_event(&ctx, write_handle), RESULT_SUCCESS);
        assert_eq!(reset_signal(&ctx, read_handle), RESULT_SUCCESS);
    }

    #[test]
    fn wait_synchronization_returns_signaled_index() {
        let ctx = test_context();
        let mut write_handle = 0;
        let mut read_handle = 0;
        assert_eq!(
            svc_event::create_event(&ctx, &mut write_handle, &mut read_handle),
            RESULT_SUCCESS
        );
        assert_eq!(svc_event::signal_event(&ctx, write_handle), RESULT_SUCCESS);

        {
            let process = ctx.current_process.lock().unwrap();
            let mut mem = process.process_memory.write().unwrap();
            mem.write_32(0x200100, read_handle);
        }

        let mut out_index = -1;
        assert_eq!(
            wait_synchronization(&ctx, &mut out_index, 0x200100, 1, 0),
            RESULT_SUCCESS
        );
        assert_eq!(out_index, 0);
    }

    #[test]
    fn wait_synchronization_timeout_zero_returns_timed_out() {
        let ctx = test_context();
        let mut write_handle = 0;
        let mut read_handle = 0;
        assert_eq!(
            svc_event::create_event(&ctx, &mut write_handle, &mut read_handle),
            RESULT_SUCCESS
        );

        {
            let process = ctx.current_process.lock().unwrap();
            let mut mem = process.process_memory.write().unwrap();
            mem.write_32(0x200100, read_handle);
        }

        let mut out_index = -1;
        assert_eq!(
            wait_synchronization(&ctx, &mut out_index, 0x200100, 1, 0),
            RESULT_TIMED_OUT
        );
        assert_eq!(out_index, -1);
    }

    #[test]
    fn wait_synchronization_blocks_then_wakes_on_signal() {
        let ctx = test_context();
        let mut write_handle = 0;
        let mut read_handle = 0;
        assert_eq!(
            svc_event::create_event(&ctx, &mut write_handle, &mut read_handle),
            RESULT_SUCCESS
        );

        {
            let process = ctx.current_process.lock().unwrap();
            let mut mem = process.process_memory.write().unwrap();
            mem.write_32(0x200100, read_handle);
        }

        let mut out_index = -1;
        assert_eq!(
            wait_synchronization(&ctx, &mut out_index, 0x200100, 1, -1),
            RESULT_SUCCESS
        );
        assert_eq!(out_index, -1);

        let current_thread = {
            ctx.current_process
                .lock()
                .unwrap()
                .get_thread_by_thread_id(1)
                .unwrap()
        };
        assert_eq!(current_thread.lock().unwrap().get_state(), crate::hle::kernel::k_thread::ThreadState::WAITING);

        assert_eq!(svc_event::signal_event(&ctx, write_handle), RESULT_SUCCESS);

        let thread = current_thread.lock().unwrap();
        assert_eq!(thread.get_state(), crate::hle::kernel::k_thread::ThreadState::RUNNABLE);
        assert_eq!(thread.get_synced_index(), 0);
        assert_eq!(thread.get_wait_result(), RESULT_SUCCESS.get_inner_value());
        assert_eq!(thread.thread_context.r[0], RESULT_SUCCESS.get_inner_value() as u64);
        assert_eq!(thread.thread_context.r[1], 0);
    }

    #[test]
    fn wait_synchronization_returns_cancelled_when_wait_cancelled_is_set() {
        let ctx = test_context();
        let mut write_handle = 0;
        let mut read_handle = 0;
        assert_eq!(
            svc_event::create_event(&ctx, &mut write_handle, &mut read_handle),
            RESULT_SUCCESS
        );

        {
            let process = ctx.current_process.lock().unwrap();
            let mut mem = process.process_memory.write().unwrap();
            mem.write_32(0x200100, read_handle);
        }

        let current_thread = {
            ctx.current_process
                .lock()
                .unwrap()
                .get_thread_by_thread_id(1)
                .unwrap()
        };
        current_thread.lock().unwrap().wait_cancel();

        let mut out_index = 123;
        assert_eq!(
            wait_synchronization(&ctx, &mut out_index, 0x200100, 1, -1),
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
        let ctx = test_context();

        let mut thread_handle = 0;
        assert_eq!(
            svc_thread::create_thread(&ctx, &mut thread_handle, 0x201000, 0x1234, 0x260000, 44, 0),
            RESULT_SUCCESS
        );

        {
            let process = ctx.current_process.lock().unwrap();
            let mut mem = process.process_memory.write().unwrap();
            mem.write_32(0x200100, thread_handle);
        }

        let object_id = ctx
            .current_process
            .lock()
            .unwrap()
            .handle_table
            .get_object(thread_handle)
            .unwrap();
        let target_thread = ctx
            .current_process
            .lock()
            .unwrap()
            .get_thread_by_object_id(object_id)
            .unwrap();

        let mut out_index = -1;
        assert_eq!(
            wait_synchronization(&ctx, &mut out_index, 0x200100, 1, -1),
            RESULT_SUCCESS
        );
        assert_eq!(out_index, -1);

        let current_thread = {
            ctx.current_process
                .lock()
                .unwrap()
                .get_thread_by_thread_id(1)
                .unwrap()
        };
        assert_eq!(
            current_thread.lock().unwrap().get_state(),
            crate::hle::kernel::k_thread::ThreadState::WAITING
        );

        target_thread.lock().unwrap().exit();

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
        let ctx = test_context();

        let process_handle = {
            let mut process = ctx.current_process.lock().unwrap();
            let process_id = process.process_id;
            process.state = crate::hle::kernel::k_process::ProcessState::RunningAttached;
            process.is_signaled = false;
            process.handle_table.add(process_id).unwrap()
        };

        {
            let process = ctx.current_process.lock().unwrap();
            let mut mem = process.process_memory.write().unwrap();
            mem.write_32(0x200100, process_handle);
        }

        let mut out_index = -1;
        assert_eq!(
            wait_synchronization(&ctx, &mut out_index, 0x200100, 1, -1),
            RESULT_SUCCESS
        );
        assert_eq!(out_index, -1);

        let current_thread = {
            ctx.current_process
                .lock()
                .unwrap()
                .get_thread_by_thread_id(1)
                .unwrap()
        };
        assert_eq!(
            current_thread.lock().unwrap().get_state(),
            crate::hle::kernel::k_thread::ThreadState::WAITING
        );

        ctx.current_process.lock().unwrap().set_debug_break();

        let thread = current_thread.lock().unwrap();
        assert_eq!(
            thread.get_state(),
            crate::hle::kernel::k_thread::ThreadState::RUNNABLE
        );
        assert_eq!(thread.get_synced_index(), 0);
        assert_eq!(thread.get_wait_result(), RESULT_SUCCESS.get_inner_value());
    }
}
