// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm_system_core.h
//! Port of zuyu/src/core/hle/service/btm/btm_system_core.cpp
//!
//! IBtmSystemCore — Bluetooth system core interface.

use crate::hle::result::ResultCode;

/// IPC command table for IBtmSystemCore.
pub mod commands {
    pub const START_GAMEPAD_PAIRING: u32 = 0;
    pub const CANCEL_GAMEPAD_PAIRING: u32 = 1;
    pub const CLEAR_GAMEPAD_PAIRING_DATABASE: u32 = 2;
    pub const GET_PAIRED_GAMEPAD_COUNT: u32 = 3;
    pub const ENABLE_RADIO: u32 = 4;
    pub const DISABLE_RADIO: u32 = 5;
    pub const IS_RADIO_ENABLED: u32 = 6;
    pub const ACQUIRE_RADIO_EVENT: u32 = 7;
    pub const ACQUIRE_AUDIO_DEVICE_CONNECTION_EVENT: u32 = 14;
    pub const GET_CONNECTED_AUDIO_DEVICES: u32 = 17;
    pub const GET_PAIRED_AUDIO_DEVICES: u32 = 20;
    pub const REQUEST_AUDIO_DEVICE_CONNECTION_REJECTION: u32 = 22;
    pub const CANCEL_AUDIO_DEVICE_CONNECTION_REJECTION: u32 = 23;
}

/// IBtmSystemCore.
pub struct IBtmSystemCore {
    // TODO: service_context, radio_event, audio_device_connection_event
}

impl IBtmSystemCore {
    pub fn new() -> Self {
        Self {}
    }

    /// StartGamepadPairing (cmd 0).
    pub fn start_gamepad_pairing(&self) -> ResultCode {
        log::warn!("IBtmSystemCore::start_gamepad_pairing (STUBBED) called");
        ResultCode::new(0)
    }

    /// CancelGamepadPairing (cmd 1).
    pub fn cancel_gamepad_pairing(&self) -> ResultCode {
        log::warn!("IBtmSystemCore::cancel_gamepad_pairing (STUBBED) called");
        ResultCode::new(0)
    }

    /// IsRadioEnabled (cmd 6).
    pub fn is_radio_enabled(&self) -> (ResultCode, bool) {
        log::debug!("IBtmSystemCore::is_radio_enabled (STUBBED) called");
        (ResultCode::new(0), true)
    }
}
