//! Port of zuyu/src/core/hle/kernel/svc/svc_port.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for port operations (ConnectToNamedPort, CreatePort, ConnectToPort, ManageNamedPort).

use std::sync::{Arc, Mutex};
use std::sync::atomic::Ordering;

use crate::hle::kernel::k_client_session::KClientSession;
use crate::hle::kernel::k_session::KSession;
use crate::hle::kernel::svc_dispatch::SvcContext;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::{Handle, INVALID_HANDLE};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::SessionRequestManager;

const PORT_NAME_MAX_LENGTH: usize = 12;

fn read_port_name(ctx: &SvcContext, user_name: u64) -> String {
    let mut bytes = Vec::new();
    if let Some(memory) = ctx.get_memory() {
        let m = memory.lock().unwrap();
        for i in 0..PORT_NAME_MAX_LENGTH as u64 {
            let byte = m.read_8(user_name + i);
            if byte == 0 {
                break;
            }
            bytes.push(byte);
        }
    } else {
        let mem = ctx.shared_memory.read().unwrap();
        for i in 0..PORT_NAME_MAX_LENGTH as u64 {
            if !mem.is_valid_range(user_name + i, 1) {
                break;
            }
            let byte = mem.read_8(user_name + i);
            if byte == 0 {
                break;
            }
            bytes.push(byte);
        }
    }
    String::from_utf8_lossy(&bytes).to_string()
}

/// Connects to a named port.
pub fn connect_to_named_port(ctx: &SvcContext, out: &mut Handle, user_name: u64) -> ResultCode {
    let name = read_port_name(ctx, user_name);
    log::info!("  ConnectToNamedPort(\"{}\")", name);

    let Some(session_handler) = ctx.service_manager.lock().unwrap().get_service(&name) else {
        return RESULT_NOT_FOUND;
    };

    let session_object_id = ctx.next_object_id.fetch_add(1, Ordering::Relaxed) as u64;
    let client_session_object_id = ctx.next_object_id.fetch_add(1, Ordering::Relaxed) as u64;

    let request_manager = Arc::new(Mutex::new(SessionRequestManager::new()));
    request_manager
        .lock()
        .unwrap()
        .set_session_handler(session_handler);

    let session = Arc::new(Mutex::new(KSession::new()));
    session.lock().unwrap().initialize(None, 0);

    let client_session = Arc::new(Mutex::new(KClientSession::new()));
    client_session
        .lock()
        .unwrap()
        .initialize_with_manager(session_object_id, request_manager);

    let mut process = ctx.current_process.lock().unwrap();
    process.register_session_object(session_object_id, session);
    process.register_client_session_object(client_session_object_id, client_session);

    match process.handle_table.add(client_session_object_id) {
        Ok(handle) => {
            *out = handle;
            RESULT_SUCCESS
        }
        Err(_) => RESULT_OUT_OF_HANDLES,
    }
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
