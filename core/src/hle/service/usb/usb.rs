// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/usb/usb.cpp
//!
//! USB services. Most commands are unimplemented stubs.

/// IPC command IDs for IDsRootSession ("usb:ds")
pub mod ds_root_commands {
    pub const OPEN_DS_SERVICE: u32 = 0;
}

/// IPC command IDs for IClientRootSession ("usb:hs")
pub mod hs_root_commands {
    pub const BIND_CLIENT_PROCESS: u32 = 0;
    pub const QUERY_ALL_INTERFACES: u32 = 1;
    pub const QUERY_AVAILABLE_INTERFACES: u32 = 2;
    pub const QUERY_ACQUIRED_INTERFACES: u32 = 3;
    pub const CREATE_INTERFACE_AVAILABLE_EVENT: u32 = 4;
    pub const DESTROY_INTERFACE_AVAILABLE_EVENT: u32 = 5;
    pub const GET_INTERFACE_STATE_CHANGE_EVENT: u32 = 6;
    pub const ACQUIRE_USB_IF: u32 = 7;
    pub const SET_TEST_MODE: u32 = 8;
}

/// IPC command IDs for IPdManager ("usb:pd")
pub mod pd_commands {
    pub const OPEN_SESSION: u32 = 0;
}

/// IPC command IDs for IPdCradleManager ("usb:pd:c")
pub mod pd_cradle_commands {
    pub const OPEN_CRADLE_SESSION: u32 = 0;
}

pub struct IDsInterface;
impl IDsInterface {
    pub fn new() -> Self { Self }
}

pub struct IDsRootSession;
impl IDsRootSession {
    pub fn new() -> Self { Self }
}

pub struct IClientEpSession;
impl IClientEpSession {
    pub fn new() -> Self { Self }
}

pub struct IClientIfSession;
impl IClientIfSession {
    pub fn new() -> Self { Self }
}

pub struct IClientRootSession;
impl IClientRootSession {
    pub fn new() -> Self { Self }
}

pub struct IPdSession;
impl IPdSession {
    pub fn new() -> Self { Self }
}

/// IPdManager ("usb:pd").
pub struct IPdManager;
impl IPdManager {
    pub fn new() -> Self { Self }

    pub fn open_session(&self) -> IPdSession {
        log::debug!("IPdManager::open_session called");
        IPdSession::new()
    }
}

pub struct IPdCradleSession;
impl IPdCradleSession {
    pub fn new() -> Self { Self }
}

/// IPdCradleManager ("usb:pd:c").
pub struct IPdCradleManager;
impl IPdCradleManager {
    pub fn new() -> Self { Self }

    pub fn open_cradle_session(&self) -> IPdCradleSession {
        log::debug!("IPdCradleManager::open_cradle_session called");
        IPdCradleSession::new()
    }
}

/// IPmMainService ("usb:pm"). All stubs.
pub struct IPmMainService;
impl IPmMainService {
    pub fn new() -> Self { Self }
}

/// Registers "usb:ds", "usb:hs", "usb:pd", "usb:pd:c", "usb:pm" services.
///
/// Corresponds to `LoopProcess` in upstream `usb.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;

    let mut server_manager = ServerManager::new(system);

    let stub_names = &["usb:ds", "usb:hs", "usb:pd", "usb:pd:c", "usb:pm"];
    for &name in stub_names {
        let svc_name = name.to_string();
        server_manager.register_named_service(
            name,
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(crate::hle::service::services::GenericStubService::new(&svc_name))
            }),
            16,
        );
    }

    ServerManager::run_server(server_manager);
}
