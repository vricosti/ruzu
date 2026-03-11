// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfc/common/device_manager.h
//! Port of zuyu/src/core/hle/service/nfc/common/device_manager.cpp
//!
//! DeviceManager: manages multiple NfcDevice instances.
//! Status: Stub

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::nfc::nfc_types::*;
use super::device::NfcDevice;

/// DeviceManager corresponds to `DeviceManager` in upstream `device_manager.h`.
pub struct DeviceManager {
    pub devices: Vec<NfcDevice>,
}

impl DeviceManager {
    pub fn new() -> Self {
        Self {
            devices: Vec::new(),
        }
    }

    pub fn initialize(&mut self) -> ResultCode {
        // TODO: create devices for each controller
        RESULT_SUCCESS
    }

    pub fn finalize(&mut self) -> ResultCode {
        for device in &mut self.devices {
            let _ = device.finalize();
        }
        RESULT_SUCCESS
    }

    pub fn list_devices(
        &self,
        nfp_devices: &mut Vec<u64>,
        _max_allowed: usize,
        _skip_unknown: bool,
    ) -> ResultCode {
        // TODO: populate device handles
        nfp_devices.clear();
        RESULT_SUCCESS
    }

    pub fn get_device_state(&self, _device_handle: u64) -> DeviceState {
        // TODO: look up actual device
        DeviceState::Initialized
    }
}
