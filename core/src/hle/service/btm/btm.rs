// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm.h
//! Port of zuyu/src/core/hle/service/btm/btm.cpp
//!
//! Bluetooth management service registration and IBtm.

/// IPC command table for IBtm (all stubs).
pub mod commands {
    pub const GET_STATE: u32 = 0;
    pub const GET_HOST_DEVICE_PROPERTY: u32 = 1;
    pub const ACQUIRE_DEVICE_CONDITION_EVENT: u32 = 2;
    pub const GET_DEVICE_CONDITION: u32 = 3;
    pub const SET_BURST_MODE: u32 = 4;
    pub const SET_SLOT_MODE: u32 = 5;
    pub const SET_BLUETOOTH_MODE: u32 = 6;
    pub const SET_WLAN_MODE: u32 = 7;
    pub const ACQUIRE_DEVICE_INFO_EVENT: u32 = 8;
    pub const GET_DEVICE_INFO: u32 = 9;
    // ... many more stubs up to cmd 115 in upstream
}

/// IBtm — main Bluetooth management service.
pub struct IBtm;

impl IBtm {
    pub fn new() -> Self {
        Self
    }
}

/// LoopProcess — registers "btm", "btm:dbg", "btm:sys", "btm:u".
pub fn loop_process() {
    log::debug!("BTM::LoopProcess called");
    // TODO: Register btm services with ServerManager
}
