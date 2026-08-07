// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfc/nfc.h
//! Port of zuyu/src/core/hle/service/nfc/nfc.cpp
//!
//! NFC service registration and inner service classes.

use std::collections::BTreeMap;
use std::sync::Arc;

use super::nfc_interface::NfcInterface;
use super::nfc_types::BackendType;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IUser (NFC::IUser)
///
/// Corresponds to `IUser` class in upstream `nfc.cpp`.
pub mod iuser_commands {
    pub const INITIALIZE_OLD: u32 = 0;
    pub const FINALIZE_OLD: u32 = 1;
    pub const GET_STATE_OLD: u32 = 2;
    pub const IS_NFC_ENABLED_OLD: u32 = 3;
    pub const INITIALIZE: u32 = 400;
    pub const FINALIZE: u32 = 401;
    pub const GET_STATE: u32 = 402;
    pub const IS_NFC_ENABLED: u32 = 403;
    pub const LIST_DEVICES: u32 = 404;
    pub const GET_DEVICE_STATE: u32 = 405;
    pub const GET_NPAD_ID: u32 = 406;
    pub const ATTACH_AVAILABILITY_CHANGE_EVENT: u32 = 407;
    pub const START_DETECTION: u32 = 408;
    pub const STOP_DETECTION: u32 = 409;
    pub const GET_TAG_INFO: u32 = 410;
    pub const ATTACH_ACTIVATE_EVENT: u32 = 411;
    pub const ATTACH_DEACTIVATE_EVENT: u32 = 412;
    pub const READ_MIFARE: u32 = 1000;
    pub const WRITE_MIFARE: u32 = 1001;
    pub const SEND_COMMAND_BY_PASS_THROUGH: u32 = 1300;
    pub const KEEP_PASS_THROUGH_SESSION: u32 = 1301;
    pub const RELEASE_PASS_THROUGH_SESSION: u32 = 1302;
}

/// IPC command table for ISystem (NFC::ISystem)
///
/// Corresponds to `ISystem` class in upstream `nfc.cpp`.
pub mod isystem_commands {
    pub const INITIALIZE_OLD: u32 = 0;
    pub const FINALIZE_OLD: u32 = 1;
    pub const GET_STATE_OLD: u32 = 2;
    pub const IS_NFC_ENABLED_OLD: u32 = 3;
    pub const SET_NFC_ENABLED_OLD: u32 = 100;
    pub const INITIALIZE: u32 = 400;
    pub const FINALIZE: u32 = 401;
    pub const GET_STATE: u32 = 402;
    pub const IS_NFC_ENABLED: u32 = 403;
    pub const LIST_DEVICES: u32 = 404;
    pub const GET_DEVICE_STATE: u32 = 405;
    pub const GET_NPAD_ID: u32 = 406;
    pub const ATTACH_AVAILABILITY_CHANGE_EVENT: u32 = 407;
    pub const START_DETECTION: u32 = 408;
    pub const STOP_DETECTION: u32 = 409;
    pub const GET_TAG_INFO: u32 = 410;
    pub const ATTACH_ACTIVATE_EVENT: u32 = 411;
    pub const ATTACH_DEACTIVATE_EVENT: u32 = 412;
    pub const SET_NFC_ENABLED: u32 = 500;
    pub const OUTPUT_TEST_WAVE: u32 = 510;
    pub const READ_MIFARE: u32 = 1000;
    pub const WRITE_MIFARE: u32 = 1001;
    pub const SEND_COMMAND_BY_PASS_THROUGH: u32 = 1300;
    pub const KEEP_PASS_THROUGH_SESSION: u32 = 1301;
    pub const RELEASE_PASS_THROUGH_SESSION: u32 = 1302;
}

/// IPC command table for MFIUser (MFInterface, Mifare backend)
///
/// Corresponds to `MFIUser` class in upstream `nfc.cpp`.
pub mod mfiuser_commands {
    pub const INITIALIZE: u32 = 0;
    pub const FINALIZE: u32 = 1;
    pub const LIST_DEVICES: u32 = 2;
    pub const START_DETECTION: u32 = 3;
    pub const STOP_DETECTION: u32 = 4;
    pub const READ: u32 = 5;
    pub const WRITE: u32 = 6;
    pub const GET_TAG_INFO: u32 = 7;
    pub const GET_ACTIVATE_EVENT_HANDLE: u32 = 8;
    pub const GET_DEACTIVATE_EVENT_HANDLE: u32 = 9;
    pub const GET_STATE: u32 = 10;
    pub const GET_DEVICE_STATE: u32 = 11;
    pub const GET_NPAD_ID: u32 = 12;
    pub const GET_AVAILABILITY_CHANGE_EVENT_HANDLE: u32 = 13;
}

/// IPC command table for IAm (NFC::IAm)
///
/// Corresponds to `IAm` class in upstream `nfc.cpp`.
pub mod iam_commands {
    pub const INITIALIZE: u32 = 0;
    pub const FINALIZE: u32 = 1;
    pub const NOTIFY_FOREGROUND_APPLET: u32 = 2;
}

/// IPC command table for NFC_AM (nfc:am)
pub mod nfc_am_commands {
    pub const CREATE_AM_NFC_INTERFACE: u32 = 0;
}

/// IPC command table for NFC_MF_U (nfc:mf:u)
pub mod nfc_mf_u_commands {
    pub const CREATE_USER_NFC_INTERFACE: u32 = 0;
}

/// IPC command table for NFC_U (nfc:user)
pub mod nfc_u_commands {
    pub const CREATE_USER_NFC_INTERFACE: u32 = 0;
}

/// IPC command table for NFC_SYS (nfc:sys)
pub mod nfc_sys_commands {
    pub const CREATE_SYSTEM_NFC_INTERFACE: u32 = 0;
}

/// IUser service (NFC::IUser) with BackendType::Nfc
pub struct IUser {
    pub interface: NfcInterface,
}

impl IUser {
    pub fn new() -> Self {
        Self {
            interface: NfcInterface::new(
                "NFC::IUser",
                BackendType::Nfc,
                build_handler_map(&[
                    (0, Some(NfcInterface::initialize_handler), "InitializeOld"),
                    (1, Some(NfcInterface::finalize_handler), "FinalizeOld"),
                    (2, Some(NfcInterface::get_state_handler), "GetStateOld"),
                    (
                        3,
                        Some(NfcInterface::is_nfc_enabled_handler),
                        "IsNfcEnabledOld",
                    ),
                    (400, Some(NfcInterface::initialize_handler), "Initialize"),
                    (401, Some(NfcInterface::finalize_handler), "Finalize"),
                    (402, Some(NfcInterface::get_state_handler), "GetState"),
                    (
                        403,
                        Some(NfcInterface::is_nfc_enabled_handler),
                        "IsNfcEnabled",
                    ),
                    (404, Some(NfcInterface::list_devices_handler), "ListDevices"),
                    (
                        405,
                        Some(NfcInterface::get_device_state_handler),
                        "GetDeviceState",
                    ),
                    (406, Some(NfcInterface::get_npad_id_handler), "GetNpadId"),
                    (
                        407,
                        Some(NfcInterface::attach_availability_change_event_handler),
                        "AttachAvailabilityChangeEvent",
                    ),
                    (
                        408,
                        Some(NfcInterface::start_detection_handler),
                        "StartDetection",
                    ),
                    (
                        409,
                        Some(NfcInterface::stop_detection_handler),
                        "StopDetection",
                    ),
                    (410, Some(NfcInterface::get_tag_info_handler), "GetTagInfo"),
                    (
                        411,
                        Some(NfcInterface::attach_activate_event_handler),
                        "AttachActivateEvent",
                    ),
                    (
                        412,
                        Some(NfcInterface::attach_deactivate_event_handler),
                        "AttachDeactivateEvent",
                    ),
                    (1000, Some(NfcInterface::read_mifare_handler), "ReadMifare"),
                    (
                        1001,
                        Some(NfcInterface::write_mifare_handler),
                        "WriteMifare",
                    ),
                    (
                        1300,
                        Some(NfcInterface::send_command_by_pass_through_handler),
                        "SendCommandByPassThrough",
                    ),
                    (1301, None, "KeepPassThroughSession"),
                    (1302, None, "ReleasePassThroughSession"),
                ]),
            ),
        }
    }
}

impl SessionRequestHandler for IUser {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        self.interface.handle_sync_request(ctx)
    }

    fn service_name(&self) -> &str {
        self.interface.service_name()
    }
}

/// ISystem service (NFC::ISystem) with BackendType::Nfc
pub struct ISystem {
    pub interface: NfcInterface,
}

impl ISystem {
    pub fn new() -> Self {
        Self {
            interface: NfcInterface::new(
                "NFC::ISystem",
                BackendType::Nfc,
                build_handler_map(&[
                    (0, Some(NfcInterface::initialize_handler), "InitializeOld"),
                    (1, Some(NfcInterface::finalize_handler), "FinalizeOld"),
                    (2, Some(NfcInterface::get_state_handler), "GetStateOld"),
                    (
                        3,
                        Some(NfcInterface::is_nfc_enabled_handler),
                        "IsNfcEnabledOld",
                    ),
                    (
                        100,
                        Some(NfcInterface::set_nfc_enabled_handler),
                        "SetNfcEnabledOld",
                    ),
                    (400, Some(NfcInterface::initialize_handler), "Initialize"),
                    (401, Some(NfcInterface::finalize_handler), "Finalize"),
                    (402, Some(NfcInterface::get_state_handler), "GetState"),
                    (
                        403,
                        Some(NfcInterface::is_nfc_enabled_handler),
                        "IsNfcEnabled",
                    ),
                    (404, Some(NfcInterface::list_devices_handler), "ListDevices"),
                    (
                        405,
                        Some(NfcInterface::get_device_state_handler),
                        "GetDeviceState",
                    ),
                    (406, Some(NfcInterface::get_npad_id_handler), "GetNpadId"),
                    (
                        407,
                        Some(NfcInterface::attach_availability_change_event_handler),
                        "AttachAvailabilityChangeEvent",
                    ),
                    (
                        408,
                        Some(NfcInterface::start_detection_handler),
                        "StartDetection",
                    ),
                    (
                        409,
                        Some(NfcInterface::stop_detection_handler),
                        "StopDetection",
                    ),
                    (410, Some(NfcInterface::get_tag_info_handler), "GetTagInfo"),
                    (
                        411,
                        Some(NfcInterface::attach_activate_event_handler),
                        "AttachActivateEvent",
                    ),
                    (
                        412,
                        Some(NfcInterface::attach_deactivate_event_handler),
                        "AttachDeactivateEvent",
                    ),
                    (
                        500,
                        Some(NfcInterface::set_nfc_enabled_handler),
                        "SetNfcEnabled",
                    ),
                    (510, None, "OutputTestWave"),
                    (1000, Some(NfcInterface::read_mifare_handler), "ReadMifare"),
                    (
                        1001,
                        Some(NfcInterface::write_mifare_handler),
                        "WriteMifare",
                    ),
                    (
                        1300,
                        Some(NfcInterface::send_command_by_pass_through_handler),
                        "SendCommandByPassThrough",
                    ),
                    (1301, None, "KeepPassThroughSession"),
                    (1302, None, "ReleasePassThroughSession"),
                ]),
            ),
        }
    }
}

impl SessionRequestHandler for ISystem {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        self.interface.handle_sync_request(ctx)
    }

    fn service_name(&self) -> &str {
        self.interface.service_name()
    }
}

/// MFIUser service (NFC::MFInterface, Mifare backend)
pub struct MFIUser {
    pub interface: NfcInterface,
}

impl MFIUser {
    pub fn new() -> Self {
        Self {
            interface: NfcInterface::new(
                "NFC::MFInterface",
                BackendType::Mifare,
                build_handler_map(&[
                    (0, Some(NfcInterface::initialize_handler), "Initialize"),
                    (1, Some(NfcInterface::finalize_handler), "Finalize"),
                    (2, Some(NfcInterface::list_devices_handler), "ListDevices"),
                    (
                        3,
                        Some(NfcInterface::start_detection_handler),
                        "StartDetection",
                    ),
                    (
                        4,
                        Some(NfcInterface::stop_detection_handler),
                        "StopDetection",
                    ),
                    (5, Some(NfcInterface::read_mifare_handler), "Read"),
                    (6, Some(NfcInterface::write_mifare_handler), "Write"),
                    (7, Some(NfcInterface::get_tag_info_handler), "GetTagInfo"),
                    (
                        8,
                        Some(NfcInterface::attach_activate_event_handler),
                        "GetActivateEventHandle",
                    ),
                    (
                        9,
                        Some(NfcInterface::attach_deactivate_event_handler),
                        "GetDeactivateEventHandle",
                    ),
                    (10, Some(NfcInterface::get_state_handler), "GetState"),
                    (
                        11,
                        Some(NfcInterface::get_device_state_handler),
                        "GetDeviceState",
                    ),
                    (12, Some(NfcInterface::get_npad_id_handler), "GetNpadId"),
                    (
                        13,
                        Some(NfcInterface::attach_availability_change_event_handler),
                        "GetAvailabilityChangeEventHandle",
                    ),
                ]),
            ),
        }
    }
}

impl SessionRequestHandler for MFIUser {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        self.interface.handle_sync_request(ctx)
    }

    fn service_name(&self) -> &str {
        self.interface.service_name()
    }
}

/// IAm service (NFC::IAm)
pub struct IAm {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IAm {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, None, "Initialize"),
                (1, None, "Finalize"),
                (2, None, "NotifyForegroundApplet"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IAm {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "NFC::IAm"
    }
}

impl ServiceFramework for IAm {
    fn get_service_name(&self) -> &str {
        "NFC::IAm"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

pub struct NfcAm {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NfcAm {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[(
                nfc_am_commands::CREATE_AM_NFC_INTERFACE,
                Some(Self::create_am_nfc_interface_handler),
                "CreateAmNfcInterface",
            )]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn create_am_nfc_interface_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = Self::as_self(this);
        let object: Arc<dyn SessionRequestHandler> = Arc::new(IAm::new());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(object);
    }
}

impl SessionRequestHandler for NfcAm {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "nfc:am"
    }
}

impl ServiceFramework for NfcAm {
    fn get_service_name(&self) -> &str {
        "nfc:am"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

pub struct NfcMfU {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NfcMfU {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[(
                nfc_mf_u_commands::CREATE_USER_NFC_INTERFACE,
                Some(Self::create_user_nfc_interface_handler),
                "CreateUserNfcInterface",
            )]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn create_user_nfc_interface_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = Self::as_self(this);
        let object: Arc<dyn SessionRequestHandler> = Arc::new(MFIUser::new());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(object);
    }
}

impl SessionRequestHandler for NfcMfU {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "nfc:mf:u"
    }
}

impl ServiceFramework for NfcMfU {
    fn get_service_name(&self) -> &str {
        "nfc:mf:u"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

pub struct NfcU {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NfcU {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[(
                nfc_u_commands::CREATE_USER_NFC_INTERFACE,
                Some(Self::create_user_nfc_interface_handler),
                "CreateUserNfcInterface",
            )]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn create_user_nfc_interface_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = Self::as_self(this);
        let object: Arc<dyn SessionRequestHandler> = Arc::new(IUser::new());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(object);
    }
}

impl SessionRequestHandler for NfcU {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "nfc:user"
    }
}

impl ServiceFramework for NfcU {
    fn get_service_name(&self) -> &str {
        "nfc:user"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

pub struct NfcSys {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NfcSys {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[(
                nfc_sys_commands::CREATE_SYSTEM_NFC_INTERFACE,
                Some(Self::create_system_nfc_interface_handler),
                "CreateSystemNfcInterface",
            )]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn create_system_nfc_interface_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let _service = Self::as_self(this);
        let object: Arc<dyn SessionRequestHandler> = Arc::new(ISystem::new());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(object);
    }
}

impl SessionRequestHandler for NfcSys {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "nfc:sys"
    }
}

impl ServiceFramework for NfcSys {
    fn get_service_name(&self) -> &str {
        "nfc:sys"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers NFC services with the server manager.
///
/// Corresponds to `LoopProcess` in upstream `nfc.cpp`.
/// Services registered: nfc:am, nfc:mf:u, nfc:user, nfc:sys
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;

    log::debug!("NFC::LoopProcess - registering nfc:am, nfc:mf:u, nfc:user, nfc:sys");

    let server_manager = ServerManager::new_shared(system);
    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "nfc:am",
            Box::new(move || -> SessionRequestHandlerPtr { Arc::new(NfcAm::new()) }),
            64,
        );
        server_manager.register_named_service(
            "nfc:mf:u",
            Box::new(move || -> SessionRequestHandlerPtr { Arc::new(NfcMfU::new()) }),
            64,
        );
        server_manager.register_named_service(
            "nfc:user",
            Box::new(move || -> SessionRequestHandlerPtr { Arc::new(NfcU::new()) }),
            64,
        );
        server_manager.register_named_service(
            "nfc:sys",
            Box::new(move || -> SessionRequestHandlerPtr { Arc::new(NfcSys::new()) }),
            64,
        );
    }
    ServerManager::run_server_shared(server_manager);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interface_command_tables_match_upstream() {
        let user = IUser::new();
        assert_eq!(user.interface.get_handlers().len(), 22);
        assert!(!user.interface.get_handlers().contains_key(&100));
        assert!(!user.interface.get_handlers().contains_key(&500));

        let system = ISystem::new();
        assert_eq!(system.interface.get_handlers().len(), 25);
        assert!(system.interface.get_handlers().contains_key(&100));
        assert!(system.interface.get_handlers().contains_key(&500));
        assert!(system.interface.get_handlers().contains_key(&510));

        let mifare = MFIUser::new();
        assert_eq!(mifare.interface.get_handlers().len(), 14);
        assert_eq!(
            mifare.interface.get_handlers().get(&3).unwrap().name,
            "StartDetection"
        );
    }

    #[test]
    fn manager_command_tables_match_upstream() {
        assert_eq!(NfcAm::new().handlers().len(), 1);
        assert_eq!(NfcMfU::new().handlers().len(), 1);
        assert_eq!(NfcU::new().handlers().len(), 1);
        assert_eq!(NfcSys::new().handlers().len(), 1);
        assert_eq!(IAm::new().handlers().len(), 3);
    }
}
