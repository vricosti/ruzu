//! Port of zuyu/src/core/hle/kernel/svc/svc_ipc.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for IPC (Inter-Process Communication) operations.

use std::sync::{Arc, Mutex};

use crate::core::System;
use crate::hle::ipc;
use crate::hle::kernel::k_event::KEvent;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::k_resource_limit::LimitableResource;
use crate::hle::kernel::k_scoped_resource_reservation::KScopedResourceReservation;
use crate::hle::kernel::k_synchronization_object;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::{Handle, INVALID_HANDLE};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{complete_sync_request, HLERequestContext};

fn ipc_timeout_tick_from_ns(current_tick: i64, timeout_ns: i64) -> i64 {
    debug_assert!(timeout_ns > 0);

    let timeout = current_tick.saturating_add(timeout_ns).saturating_add(2);
    if timeout <= 0 {
        i64::MAX
    } else {
        timeout
    }
}

fn send_sync_request_impl(
    system: &System,
    session_handle: Handle,
    message_address: u64,
) -> ResultCode {
    let (client_session, session_object_id) = {
        let process = system.current_process_arc().lock().unwrap();
        let Some(object_id) = process.handle_table.get_object(session_handle) else {
            log::error!(
                "  SendSyncRequest: handle {:#x} not in handle table",
                session_handle
            );
            return RESULT_INVALID_HANDLE;
        };
        let Some(client_session) = process.get_client_session_by_object_id(object_id) else {
            log::error!(
                "  SendSyncRequest: object_id {} not a client session",
                object_id
            );
            return RESULT_INVALID_HANDLE;
        };
        process
            .num_ipc_messages
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        (client_session, object_id)
    };

    let (request_manager, mut context, request_message_address) = {
        let mut process = system.current_process_arc().lock().unwrap();
        let send_result = client_session
            .lock()
            .unwrap()
            .send_sync_request_with_process(&mut process, message_address as usize, 0);
        if send_result != 0 {
            return ResultCode::new(send_result);
        }

        let parent_id = match client_session.lock().unwrap().get_parent_id() {
            Some(parent_id) => parent_id,
            None => return RESULT_INVALID_HANDLE,
        };
        let Some(parent_session) = process.get_session_by_object_id(parent_id) else {
            return RESULT_INVALID_HANDLE;
        };
        let server_session = parent_session.lock().unwrap().get_server_session().clone();
        let manager = match server_session.lock().unwrap().get_manager().cloned() {
            Some(manager) => manager,
            None => return RESULT_INVALID_HANDLE,
        };
        let receive_result = {
            let mut server_session = server_session.lock().unwrap();
            server_session.receive_request_hle(Arc::clone(&manager))
        };
        match receive_result {
            Ok((context, manager, request_message_address)) => {
                (manager, context, request_message_address)
            }
            Err(_) => return RESULT_INVALID_HANDLE,
        }
    };

    let service_manager = system.service_manager().unwrap();
    context.set_service_manager(service_manager);

    let (is_domain, session_handler_name) = {
        let manager = request_manager.lock().unwrap();
        let handler_name = manager
            .session_handler()
            .map(|handler| handler.service_name().to_string())
            .unwrap_or_else(|| "<none>".to_string());
        (manager.is_domain(), handler_name)
    };

    log::trace!(
        "  SendSyncRequest: handle={:#x} message={:#x} service={} cmd_type={:?} is_domain={} parsed_cmd={}",
        session_handle,
        request_message_address,
        session_handler_name,
        context.get_command_type(),
        is_domain,
        context.get_command(),
    );

    let result = complete_sync_request(&request_manager, &mut context);
    context.write_to_outgoing_command_buffer();

    if let Some(parent_session) = system
        .current_process_arc()
        .lock()
        .unwrap()
        .get_session_by_object_id(
            client_session
                .lock()
                .unwrap()
                .get_parent_id()
                .unwrap_or(session_object_id),
        )
    {
        let server_session = parent_session.lock().unwrap().get_server_session().clone();
        let _ = server_session.lock().unwrap().send_reply();
    }

    if result == crate::hle::service::ipc_helpers::RESULT_SESSION_CLOSED {
        return RESULT_SUCCESS;
    }

    result
}

/// Makes a blocking IPC call to a service.
///
/// Matches upstream `SendSyncRequest` → `SendSyncRequestImpl`:
/// 1. Get current thread and TLS address
/// 2. Resolve client session from handle
/// 3. Create HLERequestContext with thread/memory references
/// 4. Read command buffer from TLS, dispatch to handler
/// 5. Write response back to TLS (inside write_to_outgoing_command_buffer)
pub fn send_sync_request(system: &System, session_handle: Handle) -> ResultCode {
    let tls_address = match system.current_thread() {
        Some(thread) => thread.lock().unwrap().get_tls_address().get(),
        None => return RESULT_INVALID_HANDLE,
    };
    send_sync_request_impl(system, session_handle, tls_address)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::System;
    use crate::hle::ipc;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_thread::{KThread, ThreadState};
    use crate::hle::kernel::k_typed_address::KProcessAddress;
    use crate::hle::kernel::svc::svc_port;
    use crate::hle::result::RESULT_SUCCESS;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::sm::sm::ServiceManager;
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

        let scheduler = Arc::new(Mutex::new(
            crate::hle::kernel::k_scheduler::KScheduler::new(0),
        ));
        scheduler.lock().unwrap().initialize(1, 0, 0);
        let shared_memory = process.lock().unwrap().get_shared_memory();

        system.set_current_process_arc(process);
        system.set_scheduler_arc(scheduler);
        system.set_shared_process_memory(shared_memory);
        system.set_runtime_program_id(1);
        system.set_runtime_64bit(false);
        system
    }

    fn get_tls_base(system: &System) -> u64 {
        system
            .current_thread()
            .unwrap()
            .lock()
            .unwrap()
            .get_tls_address()
            .get()
    }

    fn write_named_port(system: &System, address: u64, name: &str) {
        let mut mem = system.shared_process_memory().write().unwrap();
        for (index, byte) in name.as_bytes().iter().copied().enumerate() {
            mem.write_8(address + index as u64, byte);
        }
        mem.write_8(address + name.len() as u64, 0);
    }

    fn write_sm_initialize_request(system: &System) {
        let tls_base = get_tls_base(system);
        let request_type = ipc::CommandType::Request as u32;
        let sfci_magic = u32::from_le_bytes([b'S', b'F', b'C', b'I']);
        let mut mem = system.shared_process_memory().write().unwrap();
        mem.write_32(tls_base, request_type);
        mem.write_32(tls_base + 4, 0);
        mem.write_32(tls_base + 0x10, sfci_magic);
        mem.write_32(tls_base + 0x14, 0);
        mem.write_32(tls_base + 0x18, 0);
        mem.write_32(tls_base + 0x1C, 0);
    }

    fn write_sm_get_service_request(system: &System, name: &str) {
        let tls_base = get_tls_base(system);
        let request_type = ipc::CommandType::Request as u32;
        let sfci_magic = u32::from_le_bytes([b'S', b'F', b'C', b'I']);
        let mut name_buf = [0u8; 8];
        let copy_len = name.len().min(name_buf.len());
        name_buf[..copy_len].copy_from_slice(&name.as_bytes()[..copy_len]);

        let mut mem = system.shared_process_memory().write().unwrap();
        mem.write_32(tls_base, request_type);
        mem.write_32(tls_base + 4, 0);
        mem.write_32(tls_base + 0x10, sfci_magic);
        mem.write_32(tls_base + 0x14, 0);
        mem.write_32(tls_base + 0x18, 1);
        mem.write_32(tls_base + 0x1C, 0);
        mem.write_64(tls_base + 0x20, u64::from_le_bytes(name_buf));
    }

    fn write_control_query_pointer_buffer_size_request(system: &System) {
        let tls_base = get_tls_base(system);
        write_control_query_pointer_buffer_size_request_at(system, tls_base);
    }

    fn write_control_query_pointer_buffer_size_request_at(system: &System, base: u64) {
        let control_type = ipc::CommandType::Control as u32;
        let sfci_magic = u32::from_le_bytes([b'S', b'F', b'C', b'I']);
        let mut mem = system.shared_process_memory().write().unwrap();
        mem.write_32(base, control_type);
        mem.write_32(base + 4, 0);
        mem.write_32(base + 0x10, sfci_magic);
        mem.write_32(base + 0x14, 0);
        mem.write_32(base + 0x18, 3);
        mem.write_32(base + 0x1C, 0);
    }

    #[test]
    fn send_sync_request_dispatches_sm_initialize_over_tls() {
        let system = test_system();
        let tls_base = get_tls_base(&system);
        let port_name = 0x2395800;
        write_named_port(&system, port_name, "sm:");

        let mut session_handle = 0;
        assert_eq!(
            svc_port::connect_to_named_port(&system, &mut session_handle, port_name),
            RESULT_SUCCESS
        );

        write_sm_initialize_request(&system);
        assert_eq!(send_sync_request(&system, session_handle), RESULT_SUCCESS);

        let mem = system.shared_process_memory().read().unwrap();
        assert_eq!(
            mem.read_32(tls_base + 0x18),
            RESULT_SUCCESS.get_inner_value()
        );
        assert_eq!(mem.read_32(tls_base + 0x1C), 0);
    }

    #[test]
    fn send_sync_request_dispatches_control_query_pointer_buffer_size_for_service_session() {
        let system = test_system();
        let tls_base = get_tls_base(&system);
        let current_thread = system
            .current_process_arc()
            .lock()
            .unwrap()
            .get_thread_by_thread_id(1)
            .unwrap();
        let mut request_context = HLERequestContext::new_with_thread(current_thread, tls_base);
        let service_manager = system.service_manager().unwrap();
        request_context.set_service_manager(service_manager);
        let lm_handler: SessionRequestHandlerPtr = Arc::new(crate::hle::service::lm::lm::LM::new());
        let lm_handle = request_context
            .create_session_for_service(lm_handler)
            .unwrap();

        write_control_query_pointer_buffer_size_request(&system);
        assert_eq!(send_sync_request(&system, lm_handle), RESULT_SUCCESS);

        let mem = system.shared_process_memory().read().unwrap();
        assert_eq!(
            mem.read_32(tls_base + 0x18),
            RESULT_SUCCESS.get_inner_value()
        );
        assert_eq!(mem.read_32(tls_base + 0x1C), 0);
        assert_eq!(mem.read_32(tls_base + 0x20), 0x8000);
    }

    #[test]
    fn send_sync_request_uses_server_session_manager_not_client_session_field() {
        let system = test_system();
        let tls_base = get_tls_base(&system);
        let current_thread = system
            .current_process_arc()
            .lock()
            .unwrap()
            .get_thread_by_thread_id(1)
            .unwrap();
        let mut request_context = HLERequestContext::new_with_thread(current_thread, tls_base);
        let service_manager = system.service_manager().unwrap();
        request_context.set_service_manager(service_manager);
        let lm_handler: SessionRequestHandlerPtr = Arc::new(crate::hle::service::lm::lm::LM::new());
        let lm_handle = request_context
            .create_session_for_service(lm_handler)
            .unwrap();

        {
            let process = system.current_process_arc();
            let process = process.lock().unwrap();
            let client_session_object_id = process.handle_table.get_object(lm_handle).unwrap();
            let client_session = process
                .get_client_session_by_object_id(client_session_object_id)
                .unwrap();
            client_session.lock().unwrap().request_manager = None;
        }

        write_control_query_pointer_buffer_size_request(&system);
        assert_eq!(send_sync_request(&system, lm_handle), RESULT_SUCCESS);

        let mem = system.shared_process_memory().read().unwrap();
        assert_eq!(
            mem.read_32(tls_base + 0x18),
            RESULT_SUCCESS.get_inner_value()
        );
        assert_eq!(mem.read_32(tls_base + 0x20), 0x8000);
    }

    #[test]
    fn send_sync_request_with_user_buffer_dispatches_on_message_buffer() {
        let system = test_system();
        let message = 0x23A0000u64;
        {
            let process = system.current_process_arc();
            let process_guard = process.lock().unwrap();
            let mut mem = process_guard.process_memory.write().unwrap();
            mem.allocate(message, PAGE_SIZE as usize);
        }

        let current_thread = system
            .current_process_arc()
            .lock()
            .unwrap()
            .get_thread_by_thread_id(1)
            .unwrap();
        let mut request_context = HLERequestContext::new_with_thread(current_thread, message);
        let service_manager = system.service_manager().unwrap();
        request_context.set_service_manager(service_manager);
        let lm_handler: SessionRequestHandlerPtr = Arc::new(crate::hle::service::lm::lm::LM::new());
        let lm_handle = request_context
            .create_session_for_service(lm_handler)
            .unwrap();

        write_control_query_pointer_buffer_size_request_at(&system, message);
        assert_eq!(
            send_sync_request_with_user_buffer(&system, message, PAGE_SIZE, lm_handle),
            RESULT_SUCCESS
        );

        let mem = system.shared_process_memory().read().unwrap();
        assert_eq!(
            mem.read_32(message + 0x18),
            RESULT_SUCCESS.get_inner_value()
        );
        assert_eq!(mem.read_32(message + 0x1C), 0);
        assert_eq!(mem.read_32(message + 0x20), 0x8000);
    }

    #[test]
    fn send_async_request_with_user_buffer_returns_real_readable_event_handle() {
        let system = test_system();
        let tls_base = get_tls_base(&system);
        let current_thread = system
            .current_process_arc()
            .lock()
            .unwrap()
            .get_thread_by_thread_id(1)
            .unwrap();
        let mut request_context = HLERequestContext::new_with_thread(current_thread, tls_base);
        let service_manager = system.service_manager().unwrap();
        request_context.set_service_manager(service_manager);
        let lm_handler: SessionRequestHandlerPtr = Arc::new(crate::hle::service::lm::lm::LM::new());
        let lm_handle = request_context
            .create_session_for_service(lm_handler)
            .unwrap();

        let mut out_event_handle = 0;
        assert_eq!(
            send_async_request_with_user_buffer(
                &system,
                &mut out_event_handle,
                0x2395000,
                0x80,
                lm_handle
            ),
            RESULT_SUCCESS
        );
        assert_ne!(out_event_handle, 0);

        let process = system.current_process_arc();
        let process = process.lock().unwrap();
        let readable_object_id = process.handle_table.get_object(out_event_handle).unwrap();
        assert!(process
            .get_readable_event_by_object_id(readable_object_id)
            .is_some());

        let client_session_object_id = process.handle_table.get_object(lm_handle).unwrap();
        let client_session = process
            .get_client_session_by_object_id(client_session_object_id)
            .unwrap();
        let parent_id = client_session.lock().unwrap().get_parent_id().unwrap();
        let parent_session = process.get_session_by_object_id(parent_id).unwrap();
        let server_session = parent_session.lock().unwrap().get_server_session().clone();
        drop(process);

        assert_eq!(server_session.lock().unwrap().receive_request(), 0);
        let current_request = server_session
            .lock()
            .unwrap()
            .get_current_request()
            .expect("async request");
        let current_request = current_request.lock().unwrap();
        assert!(current_request.get_event_id().is_some());
        assert_eq!(current_request.get_address(), 0x2395000);
        assert_eq!(current_request.get_size(), 0x80);
    }
}

/// Sends a sync request with a user-provided message buffer.
///
/// Upstream: Validates message buffer alignment, locks the page table for IPC,
/// sends the sync request using the locked buffer, then unlocks.
pub fn send_sync_request_with_user_buffer(
    system: &System,
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

    // Lock the message buffer in the process page table.
    {
        let mut process = system.current_process_arc().lock().unwrap();
        let msg_addr = crate::hle::kernel::k_typed_address::KProcessAddress::new(message);
        let mut paddr: u64 = 0;
        let lock_result =
            process
                .page_table
                .lock_for_ipc_user_buffer(&mut paddr, msg_addr, buffer_size as usize);
        if lock_result != 0 {
            return ResultCode::new(lock_result);
        }
    }

    // Upstream SendSyncRequestWithUserBuffer dispatches on the user-provided message
    // buffer, not the caller TLS command buffer.
    let result = send_sync_request_impl(system, session_handle, message);

    // Unlock the message buffer.
    {
        let mut process = system.current_process_arc().lock().unwrap();
        let msg_addr = crate::hle::kernel::k_typed_address::KProcessAddress::new(message);
        let unlock_result = process
            .page_table
            .unlock_for_ipc_user_buffer(msg_addr, buffer_size as usize);
        if result == RESULT_SUCCESS && unlock_result != 0 {
            return ResultCode::new(unlock_result);
        }
    }

    result
}

/// Sends an async request with a user buffer.
///
/// Upstream: Creates an event, gets client session, sends async request.
/// The readable event is returned to the caller via out_event_handle.
///
/// Needs: KEvent::Create, KEvent::Initialize, KEvent::Register, KClientSession::SendAsyncRequest.
/// These kernel objects are not yet fully ported, so this returns the event handle
/// but the async send is deferred.
pub fn send_async_request_with_user_buffer(
    system: &System,
    out_event_handle: &mut Handle,
    message: u64,
    buffer_size: u64,
    session_handle: Handle,
) -> ResultCode {
    let mut process = system.current_process_arc().lock().unwrap();
    let mut event_reservation = KScopedResourceReservation::new(
        process.resource_limit.clone(),
        LimitableResource::EventCountMax,
        1,
    );

    if !event_reservation.succeeded() {
        return RESULT_LIMIT_REACHED;
    }

    let Some(session_object_id) = process.handle_table.get_object(session_handle) else {
        return RESULT_INVALID_HANDLE;
    };
    let Some(client_session) = process.get_client_session_by_object_id(session_object_id) else {
        return RESULT_INVALID_HANDLE;
    };
    if client_session.lock().unwrap().get_parent_id().is_none() {
        return RESULT_INVALID_HANDLE;
    }

    let event_object_id = system.kernel().unwrap().create_new_object_id() as u64;
    let readable_event_object_id = system.kernel().unwrap().create_new_object_id() as u64;
    let event = Arc::new(Mutex::new(KEvent::new()));
    let readable_event = Arc::new(Mutex::new(KReadableEvent::new()));

    readable_event
        .lock()
        .unwrap()
        .initialize(event_object_id, readable_event_object_id);
    event
        .lock()
        .unwrap()
        .initialize(process.process_id, readable_event_object_id);

    process.register_event_object(event_object_id, Arc::clone(&event));
    process.register_readable_event_object(readable_event_object_id, Arc::clone(&readable_event));

    let readable_handle = match process.handle_table.add(readable_event_object_id) {
        Ok(handle) => handle,
        Err(_) => {
            process.unregister_event_object_by_object_id(event_object_id);
            process.unregister_readable_event_object_by_object_id(readable_event_object_id);
            return RESULT_OUT_OF_HANDLES;
        }
    };

    let send_result = client_session
        .lock()
        .unwrap()
        .send_async_request_with_process(
            &mut process,
            event_object_id,
            message as usize,
            buffer_size as usize,
        );
    if send_result != 0 {
        process.handle_table.remove(readable_handle);
        process.unregister_event_object_by_object_id(event_object_id);
        process.unregister_readable_event_object_by_object_id(readable_event_object_id);
        return ResultCode::new(send_result);
    }

    event_reservation.commit();
    *out_event_handle = readable_handle;
    RESULT_SUCCESS
}

/// Replies and receives IPC messages.
///
/// Upstream: Validates handle count, copies handles from user memory, resolves
/// synchronization objects, optionally sends a reply to reply_target, then
/// waits for a message on any of the provided server sessions.
pub fn reply_and_receive(
    system: &System,
    out_index: &mut i32,
    handles: u64,
    num_handles: i32,
    reply_target: Handle,
    timeout_ns: i64,
) -> ResultCode {
    let timeout = if timeout_ns > 0 {
        let current_tick = system
            .kernel()
            .and_then(|_| crate::hle::kernel::kernel::get_current_hardware_tick())
            .unwrap_or(i64::MAX);
        ipc_timeout_tick_from_ns(current_tick, timeout_ns)
    } else {
        timeout_ns
    };

    reply_and_receive_impl(
        system,
        out_index,
        0,
        0,
        handles,
        num_handles,
        reply_target,
        timeout,
    )
}

/// Replies and receives with a user-provided message buffer.
///
/// Upstream: Same as ReplyAndReceive but with a locked user buffer for the
/// message instead of using the TLS.
pub fn reply_and_receive_with_user_buffer(
    system: &System,
    out_index: &mut i32,
    message: u64,
    buffer_size: u64,
    handles: u64,
    num_handles: i32,
    reply_target: Handle,
    timeout_ns: i64,
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

    // Lock the message buffer.
    {
        let mut process = system.current_process_arc().lock().unwrap();
        let msg_addr = crate::hle::kernel::k_typed_address::KProcessAddress::new(message);
        let mut paddr: u64 = 0;
        let lock_result =
            process
                .page_table
                .lock_for_ipc_user_buffer(&mut paddr, msg_addr, buffer_size as usize);
        if lock_result != 0 {
            return ResultCode::new(lock_result);
        }
    }

    let timeout = if timeout_ns > 0 {
        let current_tick = system
            .kernel()
            .and_then(|_| crate::hle::kernel::kernel::get_current_hardware_tick())
            .unwrap_or(i64::MAX);
        ipc_timeout_tick_from_ns(current_tick, timeout_ns)
    } else {
        timeout_ns
    };

    // Perform the reply/receive.
    let result = reply_and_receive_impl(
        system,
        out_index,
        message,
        buffer_size,
        handles,
        num_handles,
        reply_target,
        timeout,
    );

    // Unlock the message buffer.
    {
        let mut process = system.current_process_arc().lock().unwrap();
        let msg_addr = crate::hle::kernel::k_typed_address::KProcessAddress::new(message);
        let unlock_result = process
            .page_table
            .unlock_for_ipc_user_buffer(msg_addr, buffer_size as usize);
        if result == RESULT_SUCCESS && unlock_result != 0 {
            return ResultCode::new(unlock_result);
        }
    }

    result
}

/// Internal implementation of ReplyAndReceive.
///
/// Upstream: Validates handle count, reads handles from user memory, resolves
/// synchronization objects, sends reply if reply_target is valid, then
/// enters a wait loop for incoming messages.
fn reply_and_receive_impl(
    system: &System,
    out_index: &mut i32,
    _message: u64,
    _buffer_size: u64,
    handles: u64,
    num_handles: i32,
    reply_target: Handle,
    timeout_ns: i64,
) -> ResultCode {
    use crate::hle::kernel::svc_common::ARGUMENT_HANDLE_COUNT_MAX;

    // Ensure number of handles is valid.
    if num_handles < 0 || num_handles > ARGUMENT_HANDLE_COUNT_MAX {
        return RESULT_OUT_OF_RANGE;
    }

    let process_arc = system.current_process_arc();
    let current_thread = match system.current_thread() {
        Some(thread) => thread,
        None => return RESULT_INVALID_HANDLE,
    };
    let scheduler = system.scheduler_arc();

    let mut process = process_arc.lock().unwrap();

    let mut handle_values = Vec::with_capacity(num_handles as usize);
    if num_handles > 0 {
        let handle_bytes = num_handles as usize * std::mem::size_of::<Handle>();
        if let Some(memory) = process.page_table.get_base().m_memory.as_ref() {
            let memory = memory.lock().unwrap();
            if !memory.is_valid_virtual_address_range(handles, handle_bytes as u64) {
                return RESULT_INVALID_POINTER;
            }
            for i in 0..num_handles as usize {
                handle_values.push(memory.read_32(handles + (i * 4) as u64));
            }
        } else {
            let memory = process.process_memory.read().unwrap();
            if !memory.is_valid_range(handles, handle_bytes) {
                return RESULT_INVALID_POINTER;
            }
            for i in 0..num_handles as usize {
                handle_values.push(memory.read_32(handles + (i * 4) as u64));
            }
        }
    }

    let mut object_ids = Vec::with_capacity(num_handles as usize);
    for handle in &handle_values {
        let Some(object_id) = process.handle_table.get_object(*handle) else {
            return RESULT_INVALID_HANDLE;
        };
        object_ids.push(object_id);
    }

    if reply_target != INVALID_HANDLE {
        let Some(reply_object_id) = process.handle_table.get_object(reply_target) else {
            return RESULT_INVALID_HANDLE;
        };
        let Some(server_session) = process.get_server_session_by_object_id(reply_object_id) else {
            return RESULT_INVALID_HANDLE;
        };
        let reply_result = server_session.lock().unwrap().send_reply_with_message(
            _message as usize,
            _buffer_size as usize,
            0,
            false,
        );
        if reply_result != 0 {
            *out_index = -1;
            return ResultCode::new(reply_result);
        }
    }

    drop(process);

    loop {
        let wait_result = k_synchronization_object::wait(
            &process_arc,
            &current_thread,
            &scheduler,
            out_index,
            object_ids.clone(),
            timeout_ns,
        );
        if wait_result != RESULT_SUCCESS {
            return wait_result;
        }

        if *out_index < 0 || (*out_index as usize) >= object_ids.len() {
            return RESULT_INVALID_HANDLE;
        }

        let object_id = object_ids[*out_index as usize];
        let process = process_arc.lock().unwrap();
        let Some(server_session) = process.get_server_session_by_object_id(object_id) else {
            return RESULT_SUCCESS;
        };
        drop(process);

        let receive_result = server_session.lock().unwrap().receive_request_with_message(
            _message as usize,
            _buffer_size as usize,
            0,
        );
        if receive_result == RESULT_NOT_FOUND.get_inner_value() {
            continue;
        }
        if receive_result != 0 {
            return ResultCode::new(receive_result);
        }
        return RESULT_SUCCESS;
    }
}

#[cfg(test)]
mod reply_receive_tests {
    use super::ipc_timeout_tick_from_ns;

    #[test]
    fn ipc_timeout_tick_from_ns_matches_upstream_saturation_rule() {
        assert_eq!(ipc_timeout_tick_from_ns(100, 5), 107);
        assert_eq!(ipc_timeout_tick_from_ns(i64::MAX - 1, 10), i64::MAX);
    }
}
