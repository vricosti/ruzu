// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfp/nfp_interface.h
//! Port of zuyu/src/core/hle/service/nfp/nfp_interface.cpp
//!
//! IUser — NFP user interface for amiibo operations.

use super::nfp_types::DeviceState;

/// IPC command table for IUser.
pub mod commands {
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
    pub const GET_NFC_NPAD_ID: u32 = 21;
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

/// IUser — NFP user interface.
///
/// Corresponds to `IUser` in upstream nfp_interface.h / nfp_interface.cpp.
pub struct IUser {
    device_state: DeviceState,
}

impl IUser {
    pub fn new() -> Self {
        Self {
            device_state: DeviceState::Initialized,
        }
    }

    /// GetDeviceState (cmd 20).
    pub fn get_device_state(&self) -> DeviceState {
        log::debug!("IUser::get_device_state called");
        self.device_state
    }
}
