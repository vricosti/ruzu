//! Port of zuyu/src/core/hle/kernel/svc/svc_session.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for session operations (CreateSession, AcceptSession).

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Creates a session (light or normal).
pub fn create_session(
    _out_server: &mut Handle,
    _out_client: &mut Handle,
    _is_light: bool,
    _name: u64,
) -> ResultCode {
    // TODO: Reserve session from resource limit.
    // TODO: If is_light, create KLightSession; else create KSession.
    // TODO: Initialize, register, add server and client to handle table.
    log::warn!("svc::CreateSession: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Accepts a session from a server port.
pub fn accept_session(_out: &mut Handle, _port_handle: Handle) -> ResultCode {
    // TODO: Get server port from handle.
    // TODO: Reserve handle entry.
    // TODO: Accept session (light or normal).
    // TODO: Register in handle table.
    log::warn!("svc::AcceptSession: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}
