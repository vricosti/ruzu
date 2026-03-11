// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfc/nfc.h
//! Port of zuyu/src/core/hle/service/nfc/nfc.cpp
//!
//! NFC service registration and inner service classes.

use super::nfc_types::BackendType;
use super::nfc_interface::NfcInterface;

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
            interface: NfcInterface::new("NFC::IUser", BackendType::Nfc),
        }
    }
}

/// ISystem service (NFC::ISystem) with BackendType::Nfc
pub struct ISystem {
    pub interface: NfcInterface,
}

impl ISystem {
    pub fn new() -> Self {
        Self {
            interface: NfcInterface::new("NFC::ISystem", BackendType::Nfc),
        }
    }
}

/// MFIUser service (NFC::MFInterface, Mifare backend)
pub struct MFIUser {
    pub interface: NfcInterface,
}

impl MFIUser {
    pub fn new() -> Self {
        Self {
            interface: NfcInterface::new("NFC::MFInterface", BackendType::Mifare),
        }
    }
}

/// IAm service (NFC::IAm)
pub struct IAm {
    pub service_name: &'static str,
}

impl IAm {
    pub fn new() -> Self {
        Self {
            service_name: "NFC::IAm",
        }
    }
}

/// Registers NFC services with the server manager.
///
/// Corresponds to `LoopProcess` in upstream `nfc.cpp`.
/// Services registered: nfc:am, nfc:mf:u, nfc:user, nfc:sys
pub fn loop_process() {
    log::debug!("NFC::LoopProcess - registering nfc:am, nfc:mf:u, nfc:user, nfc:sys");
    // TODO: create ServerManager, register named services, run server
}
