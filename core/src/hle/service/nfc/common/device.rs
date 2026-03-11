// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfc/common/device.h
//! Port of zuyu/src/core/hle/service/nfc/common/device.cpp
//!
//! NfcDevice: represents a single NFC device with tag detection, reading, writing.
//! Status: Stub

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::nfc::nfc_types::*;
use crate::hle::service::nfc::mifare_types::*;

/// NfcDevice corresponds to `NfcDevice` in upstream `device.h`.
pub struct NfcDevice {
    pub device_state: DeviceState,
    // TODO: many more fields from upstream
}

impl NfcDevice {
    pub fn new() -> Self {
        Self {
            device_state: DeviceState::Initialized,
        }
    }

    pub fn initialize(&mut self) -> ResultCode {
        // TODO: implement
        RESULT_SUCCESS
    }

    pub fn finalize(&mut self) -> ResultCode {
        self.device_state = DeviceState::Finalized;
        RESULT_SUCCESS
    }

    pub fn get_device_state(&self) -> DeviceState {
        self.device_state
    }
}
