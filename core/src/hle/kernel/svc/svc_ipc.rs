//! Port of zuyu/src/core/hle/kernel/svc/svc_ipc.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for IPC (Inter-Process Communication) operations.

use crate::hle::kernel::svc_dispatch::SvcContext;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{complete_sync_request, HLERequestContext};

/// Makes a blocking IPC call to a service.
///
/// Matches upstream `SendSyncRequest` → `SendSyncRequestImpl`:
/// 1. Get current thread and TLS address
/// 2. Resolve client session from handle
/// 3. Create HLERequestContext with thread/memory references
/// 4. Read command buffer from TLS, dispatch to handler
/// 5. Write response back to TLS (inside write_to_outgoing_command_buffer)
pub fn send_sync_request(ctx: &SvcContext, session_handle: Handle) -> ResultCode {
    let current_thread = match ctx.current_thread() {
        Some(thread) => thread,
        None => return RESULT_INVALID_HANDLE,
    };
    let tls_address = current_thread.lock().unwrap().get_tls_address().get();

    let (client_session, shared_memory) = {
        let process = ctx.current_process.lock().unwrap();
        let Some(object_id) = process.handle_table.get_object(session_handle) else {
            log::error!("  SendSyncRequest: handle {:#x} not in handle table", session_handle);
            return RESULT_INVALID_HANDLE;
        };
        let Some(client_session) = process.get_client_session_by_object_id(object_id) else {
            log::error!("  SendSyncRequest: object_id {} not a client session", object_id);
            return RESULT_INVALID_HANDLE;
        };
        process.num_ipc_messages.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        (client_session, process.get_shared_memory())
    };

    let request_manager = match client_session.lock().unwrap().request_manager() {
        Some(manager) => manager,
        None => return RESULT_INVALID_HANDLE,
    };

    // Create context with thread/memory references (matches upstream constructor).
    let debug_memory = shared_memory.clone();
    let mut context = HLERequestContext::new_with_thread(
        current_thread,
        shared_memory,
        tls_address,
    );
    context.set_session_request_manager(request_manager.clone());
    context.set_service_manager(ctx.service_manager.clone());

    // Read command buffer from TLS and parse (done inside populate).
    context.populate_from_incoming_command_buffer(&[]);

    log::info!(
        "  SendSyncRequest: handle={:#x} tls={:#x} cmd_type={:?} is_domain={} parsed_cmd={}",
        session_handle, tls_address,
        context.get_command_type(),
        request_manager.lock().unwrap().is_domain(),
        context.get_command(),
    );

    // Debug: if handle is 0, the game didn't receive the previous response correctly
    if session_handle == 0 {
        let mem = debug_memory.read().unwrap();
        log::error!(
            "  SendSyncRequest with handle=0! TLS at send time: [{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
            mem.read_32(tls_address), mem.read_32(tls_address + 4),
            mem.read_32(tls_address + 8), mem.read_32(tls_address + 12),
            mem.read_32(tls_address + 16), mem.read_32(tls_address + 20),
            mem.read_32(tls_address + 24), mem.read_32(tls_address + 28),
        );
    }

    // Dispatch to service handler.
    let result = complete_sync_request(&request_manager, &mut context);

    // write_to_outgoing_command_buffer writes back to TLS (upstream path).
    // It's already called inside handle_sync_request_impl via ServiceFramework.

    // Debug: dump TLS after response
    {
        let mem = debug_memory.read().unwrap();
        log::info!(
            "  TLS after response: [{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
            mem.read_32(tls_address), mem.read_32(tls_address + 4),
            mem.read_32(tls_address + 8), mem.read_32(tls_address + 12),
            mem.read_32(tls_address + 16), mem.read_32(tls_address + 20),
            mem.read_32(tls_address + 24), mem.read_32(tls_address + 28),
            mem.read_32(tls_address + 32), mem.read_32(tls_address + 36),
        );
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::ipc;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_thread::{KThread, ThreadState};
    use crate::hle::kernel::k_typed_address::KProcessAddress;
    use crate::hle::kernel::svc::svc_port;
    use crate::hle::kernel::svc_dispatch::SvcContext;
    use crate::hle::result::RESULT_SUCCESS;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::sm::sm::create_service_manager;
    use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
    use std::sync::{Arc, Mutex};

    fn test_context() -> SvcContext {
        let mut process = KProcess::new();
        process.process_id = 100;
        process.capabilities.core_mask = 0xF;
        process.capabilities.priority_mask = u64::MAX;
        process.flags = 0;
        process.initialize_handle_table();
        process.allocate_code_memory(0x200000, 0x60000);

        let process = Arc::new(Mutex::new(process));
        let current_thread = Arc::new(Mutex::new(KThread::new()));
        {
            let mut thread = current_thread.lock().unwrap();
            thread.thread_id = 1;
            thread.object_id = 1;
            thread.parent = Some(Arc::downgrade(&process));
            thread.tls_address = KProcessAddress::new(0x2395000);
            thread
                .thread_state
                .store(ThreadState::RUNNABLE.bits(), Ordering::Relaxed);
        }
        {
            let mut process_guard = process.lock().unwrap();
            process_guard.register_thread_object(current_thread);
            let mut mem = process_guard.process_memory.write().unwrap();
            mem.allocate(0x200000, 0x600000);
            mem.allocate(0x2395000, 0x4000);
        }

        let scheduler = Arc::new(Mutex::new(crate::hle::kernel::k_scheduler::KScheduler::new(0)));
        scheduler.lock().unwrap().initialize(1, 0, 0);
        let shared_memory = process.lock().unwrap().get_shared_memory();

        SvcContext {
            shared_memory,
            code_base: 0x200000,
            code_size: 0x60000,
            stack_base: 0,
            stack_size: 0,
            program_id: 1,
            tls_base: 0x2395000,
            current_process: process,
            service_manager: create_service_manager(),
            scheduler,
            next_thread_id: Arc::new(AtomicU64::new(2)),
            next_object_id: Arc::new(AtomicU32::new(2)),
            is_64bit: false,
        }
    }

    fn write_named_port(ctx: &SvcContext, address: u64, name: &str) {
        let mut mem = ctx.shared_memory.write().unwrap();
        for (index, byte) in name.as_bytes().iter().copied().enumerate() {
            mem.write_8(address + index as u64, byte);
        }
        mem.write_8(address + name.len() as u64, 0);
    }

    fn write_sm_initialize_request(ctx: &SvcContext) {
        let request_type = ipc::CommandType::Request as u32;
        let sfci_magic = u32::from_le_bytes([b'S', b'F', b'C', b'I']);
        let mut mem = ctx.shared_memory.write().unwrap();
        mem.write_32(ctx.tls_base, request_type);
        mem.write_32(ctx.tls_base + 4, 0);
        mem.write_32(ctx.tls_base + 0x10, sfci_magic);
        mem.write_32(ctx.tls_base + 0x14, 0);
        mem.write_32(ctx.tls_base + 0x18, 0);
        mem.write_32(ctx.tls_base + 0x1C, 0);
    }

    fn write_sm_get_service_request(ctx: &SvcContext, name: &str) {
        let request_type = ipc::CommandType::Request as u32;
        let sfci_magic = u32::from_le_bytes([b'S', b'F', b'C', b'I']);
        let mut name_buf = [0u8; 8];
        let copy_len = name.len().min(name_buf.len());
        name_buf[..copy_len].copy_from_slice(&name.as_bytes()[..copy_len]);

        let mut mem = ctx.shared_memory.write().unwrap();
        mem.write_32(ctx.tls_base, request_type);
        mem.write_32(ctx.tls_base + 4, 0);
        mem.write_32(ctx.tls_base + 0x10, sfci_magic);
        mem.write_32(ctx.tls_base + 0x14, 0);
        mem.write_32(ctx.tls_base + 0x18, 1);
        mem.write_32(ctx.tls_base + 0x1C, 0);
        mem.write_64(ctx.tls_base + 0x20, u64::from_le_bytes(name_buf));
    }

    fn write_control_query_pointer_buffer_size_request(ctx: &SvcContext) {
        let control_type = ipc::CommandType::Control as u32;
        let sfci_magic = u32::from_le_bytes([b'S', b'F', b'C', b'I']);
        let mut mem = ctx.shared_memory.write().unwrap();
        mem.write_32(ctx.tls_base, control_type);
        mem.write_32(ctx.tls_base + 4, 0);
        mem.write_32(ctx.tls_base + 0x10, sfci_magic);
        mem.write_32(ctx.tls_base + 0x14, 0);
        mem.write_32(ctx.tls_base + 0x18, 3);
        mem.write_32(ctx.tls_base + 0x1C, 0);
    }

    #[test]
    fn send_sync_request_dispatches_sm_initialize_over_tls() {
        let ctx = test_context();
        let port_name = 0x2395800;
        write_named_port(&ctx, port_name, "sm:");

        let mut session_handle = 0;
        assert_eq!(
            svc_port::connect_to_named_port(&ctx, &mut session_handle, port_name),
            RESULT_SUCCESS
        );

        write_sm_initialize_request(&ctx);
        assert_eq!(send_sync_request(&ctx, session_handle), RESULT_SUCCESS);

        let mem = ctx.shared_memory.read().unwrap();
        assert_eq!(mem.read_32(ctx.tls_base + 0x18), RESULT_SUCCESS.get_inner_value());
        assert_eq!(mem.read_32(ctx.tls_base + 0x1C), 0);
    }

    #[test]
    fn send_sync_request_dispatches_control_query_pointer_buffer_size_for_service_session() {
        let ctx = test_context();
        let current_thread = ctx
            .current_process
            .lock()
            .unwrap()
            .get_thread_by_thread_id(1)
            .unwrap();
        let mut request_context = HLERequestContext::new_with_thread(
            current_thread,
            ctx.shared_memory.clone(),
            ctx.tls_base,
        );
        request_context.set_service_manager(ctx.service_manager.clone());
        let lm_handler: SessionRequestHandlerPtr =
            Arc::new(crate::hle::service::lm::lm::LM::new());
        let lm_handle = request_context.create_session_for_service(lm_handler).unwrap();

        write_control_query_pointer_buffer_size_request(&ctx);
        assert_eq!(send_sync_request(&ctx, lm_handle), RESULT_SUCCESS);

        let mem = ctx.shared_memory.read().unwrap();
        assert_eq!(mem.read_32(ctx.tls_base + 0x18), RESULT_SUCCESS.get_inner_value());
        assert_eq!(mem.read_32(ctx.tls_base + 0x1C), 0);
        assert_eq!(mem.read_32(ctx.tls_base + 0x20), 0x8000);
    }
}

/// Sends a sync request with a user-provided message buffer.
pub fn send_sync_request_with_user_buffer(
    message: u64,
    buffer_size: u64,
    session_handle: Handle,
) -> ResultCode {
    // Validate that the message buffer is page aligned and does not overflow.
    if message % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if buffer_size == 0 {
        return RESULT_INVALID_SIZE;
    }
    if buffer_size % PAGE_SIZE != 0 {
        return RESULT_INVALID_SIZE;
    }
    if message >= message.wrapping_add(buffer_size) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // TODO: Lock message buffer, send sync request, unlock
    log::warn!("svc::SendSyncRequestWithUserBuffer: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Sends an async request with a user buffer.
pub fn send_async_request_with_user_buffer(
    _out_event_handle: &mut Handle,
    _message: u64,
    _buffer_size: u64,
    _session_handle: Handle,
) -> ResultCode {
    // TODO: Full implementation with event creation and async send
    log::warn!("svc::SendAsyncRequestWithUserBuffer: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Replies and receives IPC messages.
pub fn reply_and_receive(
    _out_index: &mut i32,
    _handles: u64,
    _num_handles: i32,
    _reply_target: Handle,
    _timeout_ns: i64,
) -> ResultCode {
    // TODO: Full implementation with handle resolution and wait loop
    log::warn!("svc::ReplyAndReceive: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Replies and receives with a user-provided message buffer.
pub fn reply_and_receive_with_user_buffer(
    _out_index: &mut i32,
    message: u64,
    buffer_size: u64,
    _handles: u64,
    _num_handles: i32,
    _reply_target: Handle,
    _timeout_ns: i64,
) -> ResultCode {
    // Validate that the message buffer is page aligned and does not overflow.
    if message % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if buffer_size == 0 {
        return RESULT_INVALID_SIZE;
    }
    if buffer_size % PAGE_SIZE != 0 {
        return RESULT_INVALID_SIZE;
    }
    if message >= message.wrapping_add(buffer_size) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // TODO: Lock buffer, reply/receive, unlock
    log::warn!("svc::ReplyAndReceiveWithUserBuffer: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}
