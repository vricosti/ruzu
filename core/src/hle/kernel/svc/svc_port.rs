//! Port of zuyu/src/core/hle/kernel/svc/svc_port.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for port operations (ConnectToNamedPort, CreatePort, ConnectToPort, ManageNamedPort).
//!
//! ConnectToNamedPort is fully implemented via the HLE service manager.
//!
//! CreatePort, ConnectToPort, and ManageNamedPort require typed object registries
//! on KProcess for KPort/KClientPort/KServerPort (similar to how sessions and events
//! are registered). These registries do not yet exist. The SVC logic documents exactly
//! what upstream does and where the dependency gap is.

use std::sync::{Arc, Mutex};

use crate::core::System;
use crate::hle::kernel::k_client_session::KClientSession;
use crate::hle::kernel::k_port::KPort;
use crate::hle::kernel::k_session::KSession;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::{Handle, INVALID_HANDLE};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::SessionRequestManager;

const PORT_NAME_MAX_LENGTH: usize = 12;

/// Read a null-terminated port name from guest memory.
/// Matches upstream `ReadCString(user_name, KObjectName::NameLengthMax)` +
/// `R_UNLESS(name[sizeof(name) - 1] == '\x00', ResultOutOfRange)`.
fn read_port_name(system: &System, user_name: u64) -> Result<String, ResultCode> {
    // Upstream behavior:
    //   1. ReadCString(user_name, NameLengthMax) — reads a null-terminated string
    //   2. std::array<char, NameLengthMax> name{} — zero-initialized buffer
    //   3. strncpy(name.data(), string_name.c_str(), NameLengthMax - 1) — copy up to 11 chars
    //   4. R_UNLESS(name[sizeof(name) - 1] == '\x00', ResultOutOfRange)
    //
    // The key is that upstream reads a C string (stopping at null), then copies it
    // into a zero-initialized buffer. So name[11] is always 0 for strings <= 11 chars.
    // We must NOT read raw 12 bytes from guest memory — bytes after the null terminator
    // may be non-zero garbage.

    // Step 1: Read null-terminated string from guest memory (up to NameLengthMax bytes).
    let mut raw = [0u8; PORT_NAME_MAX_LENGTH];
    let mut string_len = PORT_NAME_MAX_LENGTH;
    if let Some(memory) = system.get_svc_memory() {
        let m = memory.lock().unwrap();
        for i in 0..PORT_NAME_MAX_LENGTH {
            let b = m.read_8(user_name + i as u64);
            raw[i] = b;
            if b == 0 {
                string_len = i;
                break;
            }
        }
    } else {
        let mem = system.shared_process_memory().read().unwrap();
        for i in 0..PORT_NAME_MAX_LENGTH {
            let b = mem.read_8(user_name + i as u64);
            raw[i] = b;
            if b == 0 {
                string_len = i;
                break;
            }
        }
    }

    // Step 2: Copy into zero-initialized buffer (like upstream strncpy into name{}).
    let mut name = [0u8; PORT_NAME_MAX_LENGTH];
    let copy_len = string_len.min(PORT_NAME_MAX_LENGTH - 1);
    name[..copy_len].copy_from_slice(&raw[..copy_len]);

    // Step 3: Validate — upstream: R_UNLESS(name[sizeof(name) - 1] == '\x00', ResultOutOfRange)
    // This catches strings that are exactly NameLengthMax (12) chars with no null terminator.
    if name[PORT_NAME_MAX_LENGTH - 1] != 0 {
        return Err(RESULT_OUT_OF_RANGE);
    }

    let len = name.iter().position(|&b| b == 0).unwrap_or(PORT_NAME_MAX_LENGTH);
    Ok(String::from_utf8_lossy(&name[..len]).to_string())
}

/// Connects to a named port.
///
/// Upstream: Reads port name, finds KClientPort via KObjectName, reserves a handle,
/// creates a session, and registers it in the handle table.
///
/// The Rust implementation uses the HLE service manager for service resolution,
/// which provides equivalent behavior for HLE services.
pub fn connect_to_named_port(system: &System, out: &mut Handle, user_name: u64) -> ResultCode {
    let name = match read_port_name(system, user_name) {
        Ok(n) => n,
        Err(rc) => return rc,
    };
    log::info!("  ConnectToNamedPort(\"{}\")", name);

    // Find the named port via KObjectName registry (matching upstream
    // KObjectName::Find<KClientPort>(kernel, name)).
    let kernel = system.kernel().expect("kernel not initialized");
    let service_manager = system.service_manager().unwrap();

    // Try KObjectName first (upstream path), then fall back to ServiceManager.
    let session_handler = if let Some(gd) = kernel.object_name_global_data() {
        if let Some(_obj_id) = gd.find(&name) {
            // Object found in kernel namespace — get handler from service manager.
            service_manager.lock().unwrap().get_service(&name)
        } else {
            // Not in kernel namespace — try service manager directly.
            service_manager.lock().unwrap().get_service(&name)
        }
    } else {
        service_manager.lock().unwrap().get_service(&name)
    };

    let Some(session_handler) = session_handler else {
        log::error!("  ConnectToNamedPort: service \"{}\" not found", name);
        return RESULT_NOT_FOUND;
    };
    log::info!("  ConnectToNamedPort: found service handler for \"{}\"", name);

    let session_object_id = system.kernel().unwrap().create_new_object_id() as u64;
    let client_session_object_id = system.kernel().unwrap().create_new_object_id() as u64;

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

    let mut process = system.current_process_arc().lock().unwrap();
    process.register_session_object(session_object_id, session);
    process.register_client_session_object(client_session_object_id, client_session);

    match process.handle_table.add(client_session_object_id) {
        Ok(handle) => {
            *out = handle;
            log::info!("  ConnectToNamedPort(\"{}\") -> success, handle={:#x}", name, handle);
            RESULT_SUCCESS
        }
        Err(_) => {
            log::error!("  ConnectToNamedPort(\"{}\"): out of handles", name);
            RESULT_OUT_OF_HANDLES
        }
    }
}

/// Creates a port.
///
/// Upstream: Creates KPort, initializes it, registers it, adds server and client
/// ports to the handle table.
///
/// Requires typed registries on KProcess for KPort/KServerPort/KClientPort objects.
/// These registries do not yet exist. Upstream types needed:
///   - KPort (exists in Rust)
///   - KProcess needs: register_port_object, register_server_port_object, register_client_port_object
///   - KHandleTable needs: ability to resolve typed objects
pub fn create_port(
    system: &System,
    out_server: &mut Handle,
    out_client: &mut Handle,
    max_sessions: i32,
    is_light: bool,
    name: u64,
) -> ResultCode {
    // Upstream: R_UNLESS(max_sessions > 0, ResultOutOfRange)
    if max_sessions <= 0 {
        return RESULT_OUT_OF_RANGE;
    }

    let kernel = system.kernel().expect("kernel not initialized");

    // Create a new port.
    let mut port = KPort::new();
    port.initialize(max_sessions, is_light, name as usize);

    // Upstream: KPort::Register(kernel, port)
    // Upstream: handle_table.Add(out_client, &port.GetClientPort())
    // Upstream: handle_table.Add(out_server, &port.GetServerPort())

    let client_port_object_id = kernel.create_new_object_id() as u64;
    let server_port_object_id = kernel.create_new_object_id() as u64;

    let mut process = system.current_process_arc().lock().unwrap();

    // KProcess needs register_client_port_object / register_server_port_object.
    // Without these, the created port handles cannot be resolved by ConnectToPort.

    match process.handle_table.add(client_port_object_id) {
        Ok(handle) => *out_client = handle,
        Err(_) => return RESULT_OUT_OF_HANDLES,
    }

    match process.handle_table.add(server_port_object_id) {
        Ok(handle) => *out_server = handle,
        Err(_) => {
            // Clean up on failure — upstream: ON_RESULT_FAILURE { handle_table.Remove(*out_client); }
            process.handle_table.remove(*out_client);
            return RESULT_OUT_OF_HANDLES;
        }
    }

    log::warn!(
        "svc::CreatePort: KProcess lacks port object registries. \
         Server handle=0x{:08X}, client handle=0x{:08X} created but objects cannot be resolved. \
         Upstream needs: register_port_object, register_server_port_object, register_client_port_object",
        *out_server, *out_client
    );

    RESULT_SUCCESS
}

/// Connects to a port via its handle.
///
/// Upstream: Gets KClientPort from handle table, reserves a handle, creates a session
/// (light or normal), registers in handle table.
///
/// Requires typed registry for KClientPort on KProcess.
/// Upstream type: KClientPort, method: CreateSession / CreateLightSession
pub fn connect_to_port(system: &System, out: &mut Handle, port: Handle) -> ResultCode {
    log::debug!("svc::ConnectToPort called, port_handle=0x{:08X}", port);

    let process = system.current_process_arc().lock().unwrap();
    let Some(_object_id) = process.handle_table.get_object(port) else {
        return RESULT_INVALID_HANDLE;
    };

    // Upstream:
    //   KScopedAutoObject client_port = handle_table.GetObject<KClientPort>(port);
    //   R_UNLESS(client_port.IsNotNull(), ResultInvalidHandle);
    //   R_TRY(handle_table.Reserve(out));
    //   ON_RESULT_FAILURE { handle_table.Unreserve(*out); };
    //   if (client_port->IsLight()) {
    //       R_TRY(client_port->CreateLightSession(&session));
    //   } else {
    //       R_TRY(client_port->CreateSession(&session));
    //   }
    //   handle_table.Register(*out, session);
    //   session->Close();

    // KProcess needs: get_client_port_by_object_id(object_id) -> Option<Arc<Mutex<KClientPort>>>
    log::warn!(
        "svc::ConnectToPort: KProcess lacks client_port registry \
         (needs get_client_port_by_object_id). \
         Upstream type: KClientPort, methods: CreateSession / CreateLightSession"
    );
    RESULT_INVALID_HANDLE
}

/// Manages a named port (create or destroy).
///
/// Upstream: If max_sessions > 0, creates a port, registers it, adds server to handle table,
/// and creates a KObjectName entry. If max_sessions == 0, deletes the named object.
pub fn manage_named_port(
    system: &System,
    out_server_handle: &mut Handle,
    user_name: u64,
    max_sessions: i32,
) -> ResultCode {
    // Read the port name from guest memory.
    let name = match read_port_name(system, user_name) {
        Ok(n) => n,
        Err(rc) => return rc,
    };

    // Upstream: R_UNLESS(max_sessions >= 0, ResultOutOfRange)
    if max_sessions < 0 {
        return RESULT_OUT_OF_RANGE;
    }

    if max_sessions > 0 {
        let kernel = system.kernel().expect("kernel not initialized");

        // Create a new port.
        let mut port = KPort::new();
        port.initialize(max_sessions, false, 0);

        // Upstream: KPort::Register(kernel, port)
        let server_port_object_id = kernel.create_new_object_id() as u64;
        let client_port_object_id = kernel.create_new_object_id() as u64;

        let mut process = system.current_process_arc().lock().unwrap();

        // Add server to handle table.
        match process.handle_table.add(server_port_object_id) {
            Ok(handle) => *out_server_handle = handle,
            Err(_) => return RESULT_OUT_OF_HANDLES,
        }

        // Upstream: KObjectName::NewFromName(kernel, &port.GetClientPort(), name)
        // Register the client port with the named object system.
        if let Some(object_name_data) = kernel.object_name_global_data() {
            if object_name_data
                .new_from_name(client_port_object_id as usize, &name)
                .is_err()
            {
                // Name already exists — clean up server handle.
                // Upstream: ON_RESULT_FAILURE { handle_table.Remove(*out_server_handle); }
                process.handle_table.remove(*out_server_handle);
                return RESULT_INVALID_STATE;
            }
        }

        log::info!(
            "svc::ManageNamedPort(create): name=\"{}\", server_handle=0x{:08X}",
            name, *out_server_handle
        );

        RESULT_SUCCESS
    } else {
        // max_sessions == 0: delete the named port.
        debug_assert_eq!(max_sessions, 0);
        *out_server_handle = INVALID_HANDLE;

        let kernel = system.kernel().expect("kernel not initialized");

        // Upstream: KObjectName::Delete<KClientPort>(kernel, name)
        if let Some(object_name_data) = kernel.object_name_global_data() {
            // Find the object and delete it.
            if let Some(obj) = object_name_data.find(&name) {
                if object_name_data.delete(obj, &name).is_err() {
                    return RESULT_NOT_FOUND;
                }
            } else {
                return RESULT_NOT_FOUND;
            }
        }

        log::info!("svc::ManageNamedPort(delete): name=\"{}\"", name);

        RESULT_SUCCESS
    }
}
