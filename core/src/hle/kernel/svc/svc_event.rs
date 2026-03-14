//! Port of zuyu/src/core/hle/kernel/svc/svc_event.cpp
//! Status: Partial (kernel object access implemented)
//! Derniere synchro: 2026-03-14
//!
//! SVC handlers for event operations (SignalEvent, ClearEvent, CreateEvent).

use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_event::KEvent;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::kernel::svc_dispatch::SvcContext;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Signals an event.
pub fn signal_event(ctx: &SvcContext, event_handle: Handle) -> ResultCode {
    log::debug!("svc::SignalEvent called, event_handle=0x{:08X}", event_handle);

    let mut process = ctx.current_process.lock().unwrap();
    let Some(object_id) = process.handle_table.get_object(event_handle) else {
        return RESULT_INVALID_HANDLE;
    };
    let Some(event) = process.get_event_by_object_id(object_id) else {
        return RESULT_INVALID_HANDLE;
    };

    let result = event.lock().unwrap().signal(&mut process, &ctx.scheduler);
    ResultCode::new(result)
}

/// Clears an event. Tries writable event first, then readable event.
pub fn clear_event(ctx: &SvcContext, event_handle: Handle) -> ResultCode {
    log::trace!("svc::ClearEvent called, event_handle=0x{:08X}", event_handle);

    let process = ctx.current_process.lock().unwrap();
    let Some(object_id) = process.handle_table.get_object(event_handle) else {
        return RESULT_INVALID_HANDLE;
    };

    if let Some(event) = process.get_event_by_object_id(object_id) {
        return ResultCode::new(event.lock().unwrap().clear(&process));
    }
    if let Some(readable_event) = process.get_readable_event_by_object_id(object_id) {
        return ResultCode::new(readable_event.lock().unwrap().clear());
    }

    RESULT_INVALID_HANDLE
}

/// Creates an event, returning write and read handles.
pub fn create_event(ctx: &SvcContext, out_write: &mut Handle, out_read: &mut Handle) -> ResultCode {
    log::debug!("svc::CreateEvent called");

    let event_object_id = ctx.next_object_id.fetch_add(1, Ordering::Relaxed) as u64;
    let readable_event_object_id = ctx.next_object_id.fetch_add(1, Ordering::Relaxed) as u64;

    let event = Arc::new(Mutex::new(KEvent::new()));
    let readable_event = Arc::new(Mutex::new(KReadableEvent::new()));

    {
        let mut event_guard = event.lock().unwrap();
        let mut readable_event_guard = readable_event.lock().unwrap();
        let process = ctx.current_process.lock().unwrap();

        readable_event_guard.initialize(event_object_id, readable_event_object_id);
        event_guard.initialize(process.process_id, readable_event_object_id);
    }

    let mut process = ctx.current_process.lock().unwrap();
    if process.ensure_handle_table_initialized() != RESULT_SUCCESS.get_inner_value() {
        return RESULT_OUT_OF_RESOURCE;
    }

    process.register_event_object(event_object_id, event);
    process.register_readable_event_object(readable_event_object_id, readable_event);

    let write_handle = match process.handle_table.add(event_object_id) {
        Ok(handle) => handle,
        Err(_) => {
            process.unregister_event_object_by_object_id(event_object_id);
            process.unregister_readable_event_object_by_object_id(readable_event_object_id);
            return RESULT_OUT_OF_HANDLES;
        }
    };
    let read_handle = match process.handle_table.add(readable_event_object_id) {
        Ok(handle) => handle,
        Err(_) => {
            process.handle_table.remove(write_handle);
            process.unregister_event_object_by_object_id(event_object_id);
            process.unregister_readable_event_object_by_object_id(readable_event_object_id);
            return RESULT_OUT_OF_HANDLES;
        }
    };

    *out_write = write_handle;
    *out_read = read_handle;
    RESULT_SUCCESS
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_thread::KThread;
    use crate::hle::kernel::svc_dispatch::SvcContext;
    use std::sync::atomic::{AtomicU32, AtomicU64};

    fn test_context() -> SvcContext {
        let process = Arc::new(Mutex::new(KProcess::new()));
        {
            let mut process_guard = process.lock().unwrap();
            process_guard.process_id = 1;
            process_guard.initialize_handle_table();
        }

        let current_thread = Arc::new(Mutex::new(KThread::new()));
        {
            let mut thread = current_thread.lock().unwrap();
            thread.thread_id = 1;
            thread.object_id = 1;
        }
        process.lock().unwrap().register_thread_object(current_thread);

        let scheduler = Arc::new(Mutex::new(crate::hle::kernel::k_scheduler::KScheduler::new(0)));
        scheduler.lock().unwrap().initialize(1, 0, 0);
        let shared_memory = process.lock().unwrap().get_shared_memory();

        SvcContext {
            shared_memory,
            code_base: 0,
            code_size: 0,
            stack_base: 0,
            stack_size: 0,
            program_id: 1,
            tls_base: 0,
            current_process: process,
            current_thread_id: Arc::new(Mutex::new(1)),
            scheduler,
            next_thread_id: Arc::new(AtomicU64::new(2)),
            next_object_id: Arc::new(AtomicU32::new(2)),
            is_64bit: false,
        }
    }

    #[test]
    fn create_signal_and_clear_event_round_trip() {
        let ctx = test_context();
        let mut write_handle = 0;
        let mut read_handle = 0;

        assert_eq!(
            create_event(&ctx, &mut write_handle, &mut read_handle),
            RESULT_SUCCESS
        );
        assert_ne!(write_handle, 0);
        assert_ne!(read_handle, 0);

        assert_eq!(signal_event(&ctx, write_handle), RESULT_SUCCESS);

        let process = ctx.current_process.lock().unwrap();
        let readable_object_id = process.handle_table.get_object(read_handle).unwrap();
        let readable = process
            .get_readable_event_by_object_id(readable_object_id)
            .unwrap();
        assert!(readable.lock().unwrap().is_signaled());
        drop(process);

        assert_eq!(clear_event(&ctx, read_handle), RESULT_SUCCESS);
        let process = ctx.current_process.lock().unwrap();
        let readable_object_id = process.handle_table.get_object(read_handle).unwrap();
        let readable = process
            .get_readable_event_by_object_id(readable_object_id)
            .unwrap();
        assert!(!readable.lock().unwrap().is_signaled());
    }
}
