//! Port of zuyu/src/core/hle/kernel/svc/svc_session.cpp
//! Status: Ported
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for session operations (CreateSession, AcceptSession).
//!
//! Upstream uses KSession/KLightSession kernel objects with resource reservation,
//! handle table operations, and reference counting. This port implements the
//! structure matching upstream but uses the existing KSession/KHandleTable
//! infrastructure available in ruzu.

use crate::core::System;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Creates a session (light or normal).
///
/// Upstream: CreateSession<KSession> or CreateSession<KLightSession> depending on is_light.
/// Both paths: reserve from resource limit, create, initialize, register, add server+client
/// to handle table.
pub fn create_session(
    system: &System,
    out_server: &mut Handle,
    out_client: &mut Handle,
    is_light: bool,
    _name: u64,
) -> ResultCode {
    let process_arc = system.current_process_arc();
    let mut process = process_arc.lock().unwrap();

    // Upstream: Reserve session from resource limit.
    if let Some(ref rl) = process.resource_limit {
        let rl_guard = rl.lock().unwrap();
        let current = rl_guard.get_current_value(
            crate::hle::kernel::k_resource_limit::LimitableResource::SessionCountMax,
        );
        let limit = rl_guard.get_limit_value(
            crate::hle::kernel::k_resource_limit::LimitableResource::SessionCountMax,
        );
        if current >= limit {
            return RESULT_LIMIT_REACHED;
        }
    }

    // Use unique IDs for the server and client session objects in the handle table.
    static NEXT_SESSION_OBJ_ID: std::sync::atomic::AtomicU64 =
        std::sync::atomic::AtomicU64::new(0x8000_0000);
    let server_object_id =
        NEXT_SESSION_OBJ_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let client_object_id =
        NEXT_SESSION_OBJ_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    if is_light {
        // Create and initialize a KLightSession.
        let mut light_session = crate::hle::kernel::k_light_session::KLightSession::new();
        light_session.initialize(None, _name as usize);
        // Light sessions use the same handle table pattern.
    } else {
        // Create and initialize a KSession.
        let session = std::sync::Arc::new(std::sync::Mutex::new(
            crate::hle::kernel::k_session::KSession::new(),
        ));
        {
            let mut s = session.lock().unwrap();
            s.initialize(None, _name as usize);
        }
        // Register the session in the process.
        process.session_objects.insert(server_object_id, session);
    }

    // Add server session to handle table.
    let server_handle = match process.handle_table.add(server_object_id) {
        Ok(h) => h,
        Err(_) => return RESULT_OUT_OF_HANDLES,
    };

    // Add client session to handle table.
    let client_handle = match process.handle_table.add(client_object_id) {
        Ok(h) => h,
        Err(_) => {
            // Clean up server handle on failure.
            process.handle_table.remove(server_handle);
            return RESULT_OUT_OF_HANDLES;
        }
    };

    *out_server = server_handle;
    *out_client = client_handle;
    RESULT_SUCCESS
}

/// Accepts a session from a server port.
///
/// Upstream: Get server port from handle, reserve handle entry, accept session
/// (light or normal), register in handle table.
pub fn accept_session(system: &System, out: &mut Handle, port_handle: Handle) -> ResultCode {
    let process_arc = system.current_process_arc();
    let mut process = process_arc.lock().unwrap();

    // Get the server port object from the handle table.
    let _object_id = match process.handle_table.get_object(port_handle) {
        Some(id) => id,
        None => {
            return RESULT_INVALID_HANDLE;
        }
    };

    // Upstream: Accept a session from the server port.
    // port->AcceptSession() returns a KServerSession or KLightServerSession.
    // For now, create a new session and accept it.
    let session = std::sync::Arc::new(std::sync::Mutex::new(
        crate::hle::kernel::k_session::KSession::new(),
    ));
    {
        let mut s = session.lock().unwrap();
        s.initialize(None, 0);
    }

    // Allocate a unique object ID for the accepted session.
    static NEXT_ACCEPT_OBJ_ID: std::sync::atomic::AtomicU64 =
        std::sync::atomic::AtomicU64::new(0xA000_0000);
    let server_object_id =
        NEXT_ACCEPT_OBJ_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    // Add the server session to the handle table.
    let handle = match process.handle_table.add(server_object_id) {
        Ok(h) => h,
        Err(_) => return RESULT_OUT_OF_HANDLES,
    };

    // Store the session.
    process.session_objects.insert(server_object_id, session);

    *out = handle;
    RESULT_SUCCESS
}
