// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm_user_core.h
//! Port of zuyu/src/core/hle/service/btm/btm_user_core.cpp
//!
//! IBtmUserCore — BLE user core interface.

/// IPC command table for IBtmUserCore.
pub mod commands {
    pub const ACQUIRE_BLE_SCAN_EVENT: u32 = 0;
    pub const START_BLE_SCAN_FOR_GENERAL: u32 = 3;
    pub const STOP_BLE_SCAN_FOR_GENERAL: u32 = 4;
    pub const ACQUIRE_BLE_CONNECTION_EVENT: u32 = 17;
    pub const BLE_CONNECT: u32 = 18;
    pub const BLE_DISCONNECT: u32 = 19;
    pub const ACQUIRE_BLE_SERVICE_DISCOVERY_EVENT: u32 = 26;
    pub const ACQUIRE_BLE_MTU_CONFIG_EVENT: u32 = 33;
    pub const CONFIGURE_BLE_MTU: u32 = 34;
    pub const GET_BLE_MTU: u32 = 35;
}

/// IBtmUserCore.
pub struct IBtmUserCore {
    // TODO: service_context, scan_event, connection_event, etc.
}

impl IBtmUserCore {
    pub fn new() -> Self {
        Self {}
    }
}
