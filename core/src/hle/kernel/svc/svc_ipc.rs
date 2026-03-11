//! Port of zuyu/src/core/hle/kernel/svc/svc_ipc.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for IPC (Inter-Process Communication) operations.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Makes a blocking IPC call to a service.
pub fn send_sync_request(session_handle: Handle) -> ResultCode {
    // TODO: SendSyncRequestImpl(kernel, 0, 0, session_handle)
    log::warn!("svc::SendSyncRequest: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
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
