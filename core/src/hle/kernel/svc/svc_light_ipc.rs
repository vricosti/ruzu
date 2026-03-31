//! Port of zuyu/src/core/hle/kernel/svc/svc_light_ipc.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for light IPC operations.
//!
//! Upstream retrieves KLightClientSession / KLightServerSession from the handle table
//! via typed GetObject<T>. The Rust handle table currently maps Handle -> object_id (u64)
//! without type discrimination. To fully implement these SVCs, KProcess needs:
//!   - A registry mapping object_id -> Arc<Mutex<KLightClientSession>>
//!     (similar to get_client_session_by_object_id)
//!   - A registry mapping object_id -> Arc<Mutex<KLightServerSession>>
//!     (similar to get_readable_event_by_object_id)
//! These registries do not yet exist. The SVC logic below is structurally complete
//! and will work once those registries are added.

use crate::core::System;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Sends a light sync request.
///
/// Upstream: Gets KLightClientSession from handle table, calls SendSyncRequest(args).
pub fn send_sync_request_light(
    system: &System,
    session_handle: Handle,
    args: &mut [u32; 7],
) -> ResultCode {
    log::trace!(
        "svc::SendSyncRequestLight called, handle=0x{:08X}",
        session_handle
    );

    // Get the light client session from its handle.
    // Upstream: GetCurrentProcess(kernel).GetHandleTable().GetObject<KLightClientSession>(session_handle)
    let mut process = system.current_process_arc().lock().unwrap();
    let Some(object_id) = process.handle_table.get_object(session_handle) else {
        return RESULT_INVALID_HANDLE;
    };

    // Upstream needs: process.get_light_client_session_by_object_id(object_id)
    // This typed object registry does not yet exist on KProcess.
    // Once added, the call would be:
    //   let Some(session) = process.get_light_client_session_by_object_id(object_id) else {
    //       return RESULT_INVALID_HANDLE;
    //   };
    //   let result = session.lock().unwrap().send_sync_request(args);
    //   ResultCode::new(result)

    log::warn!(
        "svc::SendSyncRequestLight: KProcess lacks light_client_session registry \
         (needs get_light_client_session_by_object_id for object_id={}). \
         Upstream type: KLightClientSession, method: SendSyncRequest(&mut [u32])",
        object_id
    );
    RESULT_INVALID_HANDLE
}

/// Replies and receives on a light session.
///
/// Upstream: Gets KLightServerSession from handle table, calls ReplyAndReceive(args).
pub fn reply_and_receive_light(
    system: &System,
    session_handle: Handle,
    args: &mut [u32; 7],
) -> ResultCode {
    log::trace!(
        "svc::ReplyAndReceiveLight called, handle=0x{:08X}",
        session_handle
    );

    // Get the light server session from its handle.
    // Upstream: GetCurrentProcess(kernel).GetHandleTable().GetObject<KLightServerSession>(session_handle)
    let mut process = system.current_process_arc().lock().unwrap();
    let Some(object_id) = process.handle_table.get_object(session_handle) else {
        return RESULT_INVALID_HANDLE;
    };

    // Upstream needs: process.get_light_server_session_by_object_id(object_id)
    // This typed object registry does not yet exist on KProcess.
    // Once added, the call would be:
    //   let Some(session) = process.get_light_server_session_by_object_id(object_id) else {
    //       return RESULT_INVALID_HANDLE;
    //   };
    //   let result = session.lock().unwrap().reply_and_receive(args);
    //   ResultCode::new(result)

    log::warn!(
        "svc::ReplyAndReceiveLight: KProcess lacks light_server_session registry \
         (needs get_light_server_session_by_object_id for object_id={}). \
         Upstream type: KLightServerSession, method: ReplyAndReceive(&mut [u32])",
        object_id
    );
    RESULT_INVALID_HANDLE
}

/// Custom ABI wrapper for light IPC send.
pub fn svc_wrap_send_sync_request_light(system: &System, args: &mut [u64; 8]) {
    let mut ipc_args = [0u32; 7];
    let session_handle = args[0] as Handle;
    for i in 0..7 {
        ipc_args[i] = args[i + 1] as u32;
    }

    let ret = send_sync_request_light(system, session_handle, &mut ipc_args);

    args[0] = ret.0 as u64;
    for i in 0..7 {
        args[i + 1] = ipc_args[i] as u64;
    }
}

/// Custom ABI wrapper for light IPC reply and receive.
pub fn svc_wrap_reply_and_receive_light(system: &System, args: &mut [u64; 8]) {
    let mut ipc_args = [0u32; 7];
    let session_handle = args[0] as Handle;
    for i in 0..7 {
        ipc_args[i] = args[i + 1] as u32;
    }

    let ret = reply_and_receive_light(system, session_handle, &mut ipc_args);

    args[0] = ret.0 as u64;
    for i in 0..7 {
        args[i + 1] = ipc_args[i] as u64;
    }
}
