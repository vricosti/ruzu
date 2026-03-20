// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/vibration/n64_vibration_device.h and n64_vibration_device.cpp

use crate::hid_result::RESULT_VIBRATION_NOT_INITIALIZED;
use common::ResultCode;

/// NpadN64VibrationDevice — handles N64-style boolean vibration (on/off).
pub struct NpadN64VibrationDevice {
    pub ref_counter: i32,
    pub is_mounted: bool,
}

impl NpadN64VibrationDevice {
    pub fn new() -> Self {
        Self {
            ref_counter: 0,
            is_mounted: false,
        }
    }

    /// Port of NpadN64VibrationDevice::Activate.
    pub fn activate(&mut self) -> ResultCode {
        if self.ref_counter == 0 && self.is_mounted {
            // Upstream: check volume, xcd_handle->SetVibration(false)
        }
        self.ref_counter += 1;
        ResultCode::SUCCESS
    }

    /// Port of NpadN64VibrationDevice::Deactivate.
    pub fn deactivate(&mut self) -> ResultCode {
        if self.ref_counter == 1 && self.is_mounted {
            // Upstream: check volume, xcd_handle->SetVibration(false)
        }
        if self.ref_counter > 0 {
            self.ref_counter -= 1;
        }
        ResultCode::SUCCESS
    }

    /// Port of NpadN64VibrationDevice::Unmount.
    pub fn unmount(&mut self) -> ResultCode {
        if self.ref_counter == 0 || !self.is_mounted {
            self.is_mounted = false;
            return ResultCode::SUCCESS;
        }
        // Upstream: check volume, xcd_handle->SetVibration(false)
        self.is_mounted = false;
        ResultCode::SUCCESS
    }

    /// Port of NpadN64VibrationDevice::SendValueInBool.
    pub fn send_value_in_bool(&self, _is_vibrating: bool) -> ResultCode {
        if self.ref_counter < 1 {
            return RESULT_VIBRATION_NOT_INITIALIZED;
        }
        if self.is_mounted {
            // Upstream: check volume, xcd_handle->SetVibration(false)
        }
        ResultCode::SUCCESS
    }

    /// Port of NpadN64VibrationDevice::SendVibrationNotificationPattern.
    pub fn send_vibration_notification_pattern(&self, mut _pattern: u32) -> ResultCode {
        if !self.is_mounted {
            return ResultCode::SUCCESS;
        }
        // Upstream: check volume, if volume <= 0 then pattern = 0
        // Upstream TODO: SendVibrationNotificationPattern — not yet implemented in C++ upstream
        ResultCode::SUCCESS
    }

    pub fn is_active(&self) -> bool {
        self.ref_counter > 0
    }

    pub fn is_vibration_mounted(&self) -> bool {
        self.is_mounted
    }
}

impl Default for NpadN64VibrationDevice {
    fn default() -> Self {
        Self::new()
    }
}
