// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm_debug.h
//! Port of zuyu/src/core/hle/service/btm/btm_debug.cpp
//!
//! IBtmDebug — "btm:dbg".

/// IPC command table for IBtmDebug (all stubs).
pub mod commands {
    pub const ACQUIRE_DISCOVERY_EVENT: u32 = 0;
    pub const START_DISCOVERY: u32 = 1;
    pub const CANCEL_DISCOVERY: u32 = 2;
    pub const GET_DEVICE_PROPERTY: u32 = 3;
    pub const CREATE_BOND: u32 = 4;
    pub const CANCEL_BOND: u32 = 5;
    pub const SET_TSI_MODE: u32 = 6;
    pub const GENERAL_TEST: u32 = 7;
    pub const HID_CONNECT: u32 = 8;
    pub const GENERAL_GET: u32 = 9;
}

/// IBtmDebug.
pub struct IBtmDebug;

impl IBtmDebug {
    pub fn new() -> Self {
        Self
    }
}
