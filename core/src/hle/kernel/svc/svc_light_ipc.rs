//! Port of zuyu/src/core/hle/kernel/svc/svc_light_ipc.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for light IPC operations.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Sends a light sync request.
pub fn send_sync_request_light(session_handle: Handle, _args: &mut [u32; 7]) -> ResultCode {
    // TODO: Get light client session from handle, call SendSyncRequest(args)
    log::warn!("svc::SendSyncRequestLight: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Replies and receives on a light session.
pub fn reply_and_receive_light(session_handle: Handle, _args: &mut [u32; 7]) -> ResultCode {
    // TODO: Get light server session from handle, call ReplyAndReceive(args)
    log::warn!("svc::ReplyAndReceiveLight: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Custom ABI wrapper for light IPC send.
pub fn svc_wrap_send_sync_request_light(args: &mut [u64; 8]) {
    let mut ipc_args = [0u32; 7];
    let session_handle = args[0] as Handle;
    for i in 0..7 {
        ipc_args[i] = args[i + 1] as u32;
    }

    let ret = send_sync_request_light(session_handle, &mut ipc_args);

    args[0] = ret.0 as u64;
    for i in 0..7 {
        args[i + 1] = ipc_args[i] as u64;
    }
}

/// Custom ABI wrapper for light IPC reply and receive.
pub fn svc_wrap_reply_and_receive_light(args: &mut [u64; 8]) {
    let mut ipc_args = [0u32; 7];
    let session_handle = args[0] as Handle;
    for i in 0..7 {
        ipc_args[i] = args[i + 1] as u32;
    }

    let ret = reply_and_receive_light(session_handle, &mut ipc_args);

    args[0] = ret.0 as u64;
    for i in 0..7 {
        args[i + 1] = ipc_args[i] as u64;
    }
}
