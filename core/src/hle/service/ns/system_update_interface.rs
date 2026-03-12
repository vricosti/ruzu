// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/system_update_interface.h
//! Port of zuyu/src/core/hle/service/ns/system_update_interface.cpp
//!
//! ISystemUpdateInterface — "ns:su" service.

use crate::hle::result::ResultCode;
use super::ns_types::BackgroundNetworkUpdateState;

/// IPC command table for ISystemUpdateInterface.
///
/// Corresponds to the function table in upstream system_update_interface.cpp.
pub mod commands {
    pub const GET_BACKGROUND_NETWORK_UPDATE_STATE: u32 = 0;
    pub const OPEN_SYSTEM_UPDATE_CONTROL: u32 = 1;
    pub const NOTIFY_EX_FAT_DRIVER_REQUIRED: u32 = 2;
    pub const CLEAR_EX_FAT_DRIVER_STATUS_FOR_DEBUG: u32 = 3;
    pub const REQUEST_BACKGROUND_NETWORK_UPDATE: u32 = 4;
    pub const NOTIFY_BACKGROUND_NETWORK_UPDATE: u32 = 5;
    pub const NOTIFY_EX_FAT_DRIVER_DOWNLOADED_FOR_DEBUG: u32 = 6;
    pub const GET_SYSTEM_UPDATE_NOTIFICATION_EVENT_FOR_CONTENT_DELIVERY: u32 = 9;
    pub const NOTIFY_SYSTEM_UPDATE_FOR_CONTENT_DELIVERY: u32 = 10;
    pub const PREPARE_SHUTDOWN: u32 = 11;
    pub const UNKNOWN_12: u32 = 12;
    pub const UNKNOWN_13: u32 = 13;
    pub const UNKNOWN_14: u32 = 14;
    pub const UNKNOWN_15: u32 = 15;
    pub const DESTROY_SYSTEM_UPDATE_TASK: u32 = 16;
    pub const REQUEST_SEND_SYSTEM_UPDATE: u32 = 17;
    pub const GET_SEND_SYSTEM_UPDATE_PROGRESS: u32 = 18;
}

/// ISystemUpdateInterface.
///
/// Corresponds to `ISystemUpdateInterface` in upstream.
pub struct ISystemUpdateInterface;

impl ISystemUpdateInterface {
    pub fn new() -> Self {
        Self
    }

    /// GetBackgroundNetworkUpdateState (cmd 0).
    ///
    /// Corresponds to upstream `ISystemUpdateInterface::GetBackgroundNetworkUpdateState`.
    pub fn get_background_network_update_state(
        &self,
    ) -> Result<BackgroundNetworkUpdateState, ResultCode> {
        log::warn!("(STUBBED) GetBackgroundNetworkUpdateState called");
        Ok(BackgroundNetworkUpdateState::None)
    }

    /// OpenSystemUpdateControl (cmd 1).
    ///
    /// Corresponds to upstream `ISystemUpdateInterface::OpenSystemUpdateControl`.
    pub fn open_system_update_control(&self) -> Result<(), ResultCode> {
        log::warn!("(STUBBED) OpenSystemUpdateControl called");
        // TODO: Create and return ISystemUpdateControl
        Ok(())
    }
}
