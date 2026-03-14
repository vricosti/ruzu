//! Port of zuyu/src/core/hle/kernel/svc/svc_ipc.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for IPC (Inter-Process Communication) operations.

use crate::hle::ipc;
use crate::hle::kernel::svc_dispatch::SvcContext;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{complete_sync_request, HLERequestContext};

fn read_tls_command_buffer(ctx: &SvcContext, tls_address: u64) -> [u32; ipc::COMMAND_BUFFER_LENGTH] {
    let mem = ctx.shared_memory.read().unwrap();
    let mut cmd_buf = [0u32; ipc::COMMAND_BUFFER_LENGTH];
    for (index, word) in cmd_buf.iter_mut().enumerate() {
        *word = mem.read_32(tls_address + (index as u64 * core::mem::size_of::<u32>() as u64));
    }
    cmd_buf
}

fn write_tls_command_buffer(
    ctx: &SvcContext,
    tls_address: u64,
    cmd_buf: &[u32; ipc::COMMAND_BUFFER_LENGTH],
) {
    let mut mem = ctx.shared_memory.write().unwrap();
    for (index, word) in cmd_buf.iter().copied().enumerate() {
        mem.write_32(tls_address + (index as u64 * core::mem::size_of::<u32>() as u64), word);
    }
}

/// Makes a blocking IPC call to a service.
pub fn send_sync_request(ctx: &SvcContext, session_handle: Handle) -> ResultCode {
    let current_thread = match ctx.current_thread() {
        Some(thread) => thread,
        None => return RESULT_INVALID_HANDLE,
    };
    let tls_address = current_thread.lock().unwrap().get_tls_address().get();

    let (client_session, process) = {
        let process = ctx.current_process.lock().unwrap();
        let Some(object_id) = process.handle_table.get_object(session_handle) else {
            return RESULT_INVALID_HANDLE;
        };
        let Some(client_session) = process.get_client_session_by_object_id(object_id) else {
            return RESULT_INVALID_HANDLE;
        };
        (client_session, process)
    };

    process.num_ipc_messages.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    drop(process);

    let request_manager = match client_session.lock().unwrap().request_manager() {
        Some(manager) => manager,
        None => return RESULT_INVALID_HANDLE,
    };

    let mut context = HLERequestContext::new();
    context.set_session_request_manager(request_manager.clone());
    let incoming = read_tls_command_buffer(ctx, tls_address);
    context.populate_from_incoming_command_buffer(&incoming);

    log::info!(
        "  SendSyncRequest: handle={:#x} tls={:#x} cmd_type={} cmd_id={}",
        session_handle, tls_address,
        incoming[0] & 0xFFFF, // command type from header
        incoming[8],          // command ID (after SFCI magic at word[4])
    );

    let result = complete_sync_request(&request_manager, &mut context);

    let outgoing = context.command_buffer();
    log::info!(
        "  SendSyncRequest result: {:#x} words[0..12]=[{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
        result.get_inner_value(),
        outgoing[0], outgoing[1], outgoing[2], outgoing[3],
        outgoing[4], outgoing[5], outgoing[6], outgoing[7],
        outgoing[8], outgoing[9], outgoing[10], outgoing[11],
    );

    write_tls_command_buffer(ctx, tls_address, outgoing);
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_thread::{KThread, ThreadState};
    use crate::hle::kernel::k_typed_address::KProcessAddress;
    use crate::hle::kernel::svc::svc_port;
    use crate::hle::kernel::svc_dispatch::SvcContext;
    use crate::hle::result::RESULT_SUCCESS;
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
