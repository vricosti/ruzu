// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfp/nfp.h
//! Port of zuyu/src/core/hle/service/nfp/nfp.cpp
//!
//! NFP service — "nfp:user", "nfp:sys", "nfp:dbg" service registration.
//!
//! Upstream nfp.cpp defines IUser, ISystem, IDebug classes (each deriving from Interface),
//! plus IUserManager, ISystemManager, IDebugManager service entry points.

/// IPC command table for IUserManager.
pub mod user_manager_commands {
    pub const CREATE_USER_INTERFACE: u32 = 0;
}

/// IPC command table for ISystemManager.
pub mod system_manager_commands {
    pub const CREATE_SYSTEM_INTERFACE: u32 = 0;
}

/// IPC command table for IDebugManager.
pub mod debug_manager_commands {
    pub const CREATE_DEBUG_INTERFACE: u32 = 0;
}

/// IPC command table for IUser (from nfp.cpp).
///
/// Corresponds to the function table in `IUser` in upstream nfp.cpp.
pub mod iuser_commands {
    pub const INITIALIZE: u32 = 0;
    pub const FINALIZE: u32 = 1;
    pub const LIST_DEVICES: u32 = 2;
    pub const START_DETECTION: u32 = 3;
    pub const STOP_DETECTION: u32 = 4;
    pub const MOUNT: u32 = 5;
    pub const UNMOUNT: u32 = 6;
    pub const OPEN_APPLICATION_AREA: u32 = 7;
    pub const GET_APPLICATION_AREA: u32 = 8;
    pub const SET_APPLICATION_AREA: u32 = 9;
    pub const FLUSH: u32 = 10;
    pub const RESTORE: u32 = 11;
    pub const CREATE_APPLICATION_AREA: u32 = 12;
    pub const GET_TAG_INFO: u32 = 13;
    pub const GET_REGISTER_INFO: u32 = 14;
    pub const GET_COMMON_INFO: u32 = 15;
    pub const GET_MODEL_INFO: u32 = 16;
    pub const ATTACH_ACTIVATE_EVENT: u32 = 17;
    pub const ATTACH_DEACTIVATE_EVENT: u32 = 18;
    pub const GET_STATE: u32 = 19;
    pub const GET_DEVICE_STATE: u32 = 20;
    pub const GET_NPAD_ID: u32 = 21;
    pub const GET_APPLICATION_AREA_SIZE: u32 = 22;
    pub const ATTACH_AVAILABILITY_CHANGE_EVENT: u32 = 23;
    pub const RECREATE_APPLICATION_AREA: u32 = 24;
}

/// IPC command table for ISystem (from nfp.cpp).
///
/// Corresponds to the function table in `ISystem` in upstream nfp.cpp.
pub mod isystem_commands {
    pub const INITIALIZE_SYSTEM: u32 = 0;
    pub const FINALIZE_SYSTEM: u32 = 1;
    pub const LIST_DEVICES: u32 = 2;
    pub const START_DETECTION: u32 = 3;
    pub const STOP_DETECTION: u32 = 4;
    pub const MOUNT: u32 = 5;
    pub const UNMOUNT: u32 = 6;
    pub const FLUSH: u32 = 10;
    pub const RESTORE: u32 = 11;
    pub const CREATE_APPLICATION_AREA: u32 = 12;
    pub const GET_TAG_INFO: u32 = 13;
    pub const GET_REGISTER_INFO: u32 = 14;
    pub const GET_COMMON_INFO: u32 = 15;
    pub const GET_MODEL_INFO: u32 = 16;
    pub const ATTACH_ACTIVATE_EVENT: u32 = 17;
    pub const ATTACH_DEACTIVATE_EVENT: u32 = 18;
    pub const GET_STATE: u32 = 19;
    pub const GET_DEVICE_STATE: u32 = 20;
    pub const GET_NPAD_ID: u32 = 21;
    pub const ATTACH_AVAILABILITY_CHANGE_EVENT: u32 = 23;
    pub const FORMAT: u32 = 100;
    pub const GET_ADMIN_INFO: u32 = 101;
    pub const GET_REGISTER_INFO_PRIVATE: u32 = 102;
    pub const SET_REGISTER_INFO_PRIVATE: u32 = 103;
    pub const DELETE_REGISTER_INFO: u32 = 104;
    pub const DELETE_APPLICATION_AREA: u32 = 105;
    pub const EXISTS_APPLICATION_AREA: u32 = 106;
}

/// IPC command table for IDebug (from nfp.cpp).
///
/// Corresponds to the function table in `IDebug` in upstream nfp.cpp.
pub mod idebug_commands {
    pub const INITIALIZE_DEBUG: u32 = 0;
    pub const FINALIZE_DEBUG: u32 = 1;
    pub const LIST_DEVICES: u32 = 2;
    pub const START_DETECTION: u32 = 3;
    pub const STOP_DETECTION: u32 = 4;
    pub const MOUNT: u32 = 5;
    pub const UNMOUNT: u32 = 6;
    pub const OPEN_APPLICATION_AREA: u32 = 7;
    pub const GET_APPLICATION_AREA: u32 = 8;
    pub const SET_APPLICATION_AREA: u32 = 9;
    pub const FLUSH: u32 = 10;
    pub const RESTORE: u32 = 11;
    pub const CREATE_APPLICATION_AREA: u32 = 12;
    pub const GET_TAG_INFO: u32 = 13;
    pub const GET_REGISTER_INFO: u32 = 14;
    pub const GET_COMMON_INFO: u32 = 15;
    pub const GET_MODEL_INFO: u32 = 16;
    pub const ATTACH_ACTIVATE_EVENT: u32 = 17;
    pub const ATTACH_DEACTIVATE_EVENT: u32 = 18;
    pub const GET_STATE: u32 = 19;
    pub const GET_DEVICE_STATE: u32 = 20;
    pub const GET_NPAD_ID: u32 = 21;
    pub const GET_APPLICATION_AREA_SIZE: u32 = 22;
    pub const ATTACH_AVAILABILITY_CHANGE_EVENT: u32 = 23;
    pub const RECREATE_APPLICATION_AREA: u32 = 24;
    pub const FORMAT: u32 = 100;
    pub const GET_ADMIN_INFO: u32 = 101;
    pub const GET_REGISTER_INFO_PRIVATE: u32 = 102;
    pub const SET_REGISTER_INFO_PRIVATE: u32 = 103;
    pub const DELETE_REGISTER_INFO: u32 = 104;
    pub const DELETE_APPLICATION_AREA: u32 = 105;
    pub const EXISTS_APPLICATION_AREA: u32 = 106;
    pub const GET_ALL: u32 = 200;
    pub const SET_ALL: u32 = 201;
    pub const FLUSH_DEBUG: u32 = 202;
    pub const BREAK_TAG: u32 = 203;
    pub const READ_BACKUP_DATA: u32 = 204;
    pub const WRITE_BACKUP_DATA: u32 = 205;
    pub const WRITE_NTF: u32 = 206;
}

/// IUserManager — entry point for "nfp:user".
///
/// Corresponds to `IUserManager` in upstream nfp.cpp.
pub struct IUserManager;

impl IUserManager {
    pub fn new() -> Self {
        Self
    }

    /// CreateUserInterface (cmd 0).
    pub fn create_user_interface(&self) -> super::nfp_interface::Interface {
        log::debug!("IUserManager::create_user_interface called");
        super::nfp_interface::Interface::new(crate::core::SystemRef::null(), "IUser")
    }
}

/// ISystemManager — entry point for "nfp:sys".
///
/// Corresponds to `ISystemManager` in upstream nfp.cpp.
pub struct ISystemManager;

impl ISystemManager {
    pub fn new() -> Self {
        Self
    }

    /// CreateSystemInterface (cmd 0).
    ///
    /// Upstream creates an ISystem (which derives from Interface with name "NFP:ISystem").
    pub fn create_system_interface(&self) -> super::nfp_interface::Interface {
        log::debug!("ISystemManager::create_system_interface called");
        super::nfp_interface::Interface::new(crate::core::SystemRef::null(), "ISystem")
    }
}

/// IDebugManager — entry point for "nfp:dbg".
///
/// Corresponds to `IDebugManager` in upstream nfp.cpp.
pub struct IDebugManager;

impl IDebugManager {
    pub fn new() -> Self {
        Self
    }

    /// CreateDebugInterface (cmd 0).
    ///
    /// Upstream creates an IDebug (which derives from Interface with name "NFP:IDebug").
    pub fn create_debug_interface(&self) -> super::nfp_interface::Interface {
        log::debug!("IDebugManager::create_debug_interface called");
        super::nfp_interface::Interface::new(crate::core::SystemRef::null(), "IDebug")
    }
}

/// LoopProcess — registers "nfp:user", "nfp:sys", "nfp:dbg" services.
///
/// Corresponds to `Service::NFP::LoopProcess` in upstream nfp.cpp.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;

    log::debug!("NFP::LoopProcess called");

    let mut server_manager = ServerManager::new(system);
    crate::hle::service::services::register_stub_services(
        &mut server_manager,
        &["nfp:user", "nfp:sys", "nfp:dbg"],
    );
    ServerManager::run_server(server_manager);
}
