//! Port of zuyu/src/core/hle/kernel/svc/svc_port.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for port operations (ConnectToNamedPort, CreatePort, ConnectToPort, ManageNamedPort).

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::{Handle, INVALID_HANDLE};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Connects to a named port.
pub fn connect_to_named_port(_out: &mut Handle, _user_name: u64) -> ResultCode {
    // TODO: Read port name from user memory
    // TODO: KObjectName::Find<KClientPort>, reserve handle, create session
    log::warn!("svc::ConnectToNamedPort: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Creates a port.
pub fn create_port(
    _out_server: &mut Handle,
    _out_client: &mut Handle,
    max_sessions: i32,
    _is_light: bool,
    _name: u64,
) -> ResultCode {
    if max_sessions <= 0 {
        return RESULT_OUT_OF_RANGE;
    }

    // TODO: KPort::Create, Initialize, Register, add to handle table
    log::warn!("svc::CreatePort: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Connects to a port via its handle.
pub fn connect_to_port(_out: &mut Handle, _port: Handle) -> ResultCode {
    // TODO: Get client port from handle, reserve handle, create session
    log::warn!("svc::ConnectToPort: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Manages a named port (create or destroy).
pub fn manage_named_port(
    out_server_handle: &mut Handle,
    _user_name: u64,
    max_sessions: i32,
) -> ResultCode {
    if max_sessions < 0 {
        return RESULT_OUT_OF_RANGE;
    }

    if max_sessions > 0 {
        // TODO: Create port, register, add server to handle table,
        // create named object entry
        log::warn!("svc::ManageNamedPort(create): kernel object access not yet implemented");
        RESULT_NOT_IMPLEMENTED
    } else {
        // max_sessions == 0: delete the named port.
        *out_server_handle = INVALID_HANDLE;
        // TODO: KObjectName::Delete<KClientPort>(kernel, name)
        log::warn!("svc::ManageNamedPort(delete): kernel object access not yet implemented");
        RESULT_NOT_IMPLEMENTED
    }
}
