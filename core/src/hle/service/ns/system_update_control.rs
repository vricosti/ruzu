// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/system_update_control.h
//! Port of zuyu/src/core/hle/service/ns/system_update_control.cpp
//!
//! ISystemUpdateControl — system update control sub-interface.

/// IPC command table for ISystemUpdateControl.
///
/// Corresponds to the function table in upstream system_update_control.cpp.
pub mod commands {
    pub const HAS_DOWNLOADED: u32 = 0;
    pub const REQUEST_CHECK_LATEST_UPDATE: u32 = 1;
    pub const REQUEST_DOWNLOAD_LATEST_UPDATE: u32 = 2;
    pub const GET_DOWNLOAD_PROGRESS: u32 = 3;
    pub const APPLY_DOWNLOADED_UPDATE: u32 = 4;
    pub const REQUEST_PREPARE_CARD_UPDATE: u32 = 5;
    pub const GET_PREPARE_CARD_UPDATE_PROGRESS: u32 = 6;
    pub const HAS_PREPARED_CARD_UPDATE: u32 = 7;
    pub const APPLY_CARD_UPDATE: u32 = 8;
    pub const GET_DOWNLOADED_EULA_DATA_SIZE: u32 = 9;
    pub const GET_DOWNLOADED_EULA_DATA: u32 = 10;
    pub const SETUP_CARD_UPDATE: u32 = 11;
    pub const GET_PREPARED_CARD_UPDATE_EULA_DATA_SIZE: u32 = 12;
    pub const GET_PREPARED_CARD_UPDATE_EULA_DATA: u32 = 13;
    pub const SETUP_CARD_UPDATE_VIA_SYSTEM_UPDATER: u32 = 14;
    pub const HAS_RECEIVED: u32 = 15;
    pub const REQUEST_RECEIVE_SYSTEM_UPDATE: u32 = 16;
    pub const GET_RECEIVE_PROGRESS: u32 = 17;
    pub const APPLY_RECEIVED_UPDATE: u32 = 18;
    pub const GET_RECEIVED_EULA_DATA_SIZE: u32 = 19;
    pub const GET_RECEIVED_EULA_DATA: u32 = 20;
    pub const SETUP_TO_RECEIVE_SYSTEM_UPDATE: u32 = 21;
    pub const REQUEST_CHECK_LATEST_UPDATE_INCLUDES_REBOOTLESS_UPDATE: u32 = 22;
}

/// ISystemUpdateControl.
///
/// Corresponds to `ISystemUpdateControl` in upstream.
/// All commands are unimplemented (nullptr) in upstream.
pub struct ISystemUpdateControl;

impl ISystemUpdateControl {
    pub fn new() -> Self {
        Self
    }
}
