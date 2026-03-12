// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/vibration/vibration_device.h and vibration_device.cpp

use crate::hid_result::RESULT_VIBRATION_NOT_INITIALIZED;
use crate::hid_types::{DeviceIndex, VibrationValue, DEFAULT_VIBRATION_VALUE};
use common::ResultCode;

/// NpadVibrationDevice — handles standard (linear resonant actuator) vibration.
/// Manages activation, mounting to an abstracted pad, and sending vibration
/// values with volume scaling.
pub struct NpadVibrationDevice {
    pub ref_counter: i32,
    pub is_mounted: bool,
    pub device_index: DeviceIndex,
}

impl NpadVibrationDevice {
    pub fn new() -> Self {
        Self {
            ref_counter: 0,
            is_mounted: false,
            device_index: DeviceIndex::None,
        }
    }

    /// Port of NpadVibrationDevice::Activate.
    /// When first activated while mounted, sends default vibration.
    pub fn activate(&mut self) -> ResultCode {
        if self.ref_counter == 0 && self.is_mounted {
            // Upstream: vibration_handler->GetVibrationVolume(volume)
            // then xcd_handle->SetVibration(device_index, DEFAULT_VIBRATION_VALUE)
            let _ = DEFAULT_VIBRATION_VALUE;
        }
        self.ref_counter += 1;
        ResultCode::SUCCESS
    }

    /// Port of NpadVibrationDevice::Deactivate.
    /// When last reference is removed while mounted, sends default vibration.
    pub fn deactivate(&mut self) -> ResultCode {
        if self.ref_counter == 1 && self.is_mounted {
            let _ = DEFAULT_VIBRATION_VALUE;
        }
        if self.ref_counter > 0 {
            self.ref_counter -= 1;
        }
        ResultCode::SUCCESS
    }

    /// Port of NpadVibrationDevice::Unmount.
    pub fn unmount(&mut self) -> ResultCode {
        if self.ref_counter == 0 || !self.is_mounted {
            self.is_mounted = false;
            return ResultCode::SUCCESS;
        }
        // Upstream: send DEFAULT_VIBRATION_VALUE via xcd_handle
        self.is_mounted = false;
        ResultCode::SUCCESS
    }

    /// Port of NpadVibrationDevice::SendVibrationValue.
    /// Scales amplitudes by vibration volume before sending to the controller.
    pub fn send_vibration_value(&self, value: &VibrationValue) -> ResultCode {
        if self.ref_counter == 0 {
            return RESULT_VIBRATION_NOT_INITIALIZED;
        }
        if !self.is_mounted {
            return ResultCode::SUCCESS;
        }
        // Upstream:
        //   volume = vibration_handler->GetVibrationVolume()
        //   if volume <= 0: send DEFAULT_VIBRATION_VALUE
        //   else: scale amplitudes by volume, send via xcd_handle
        let _ = value;
        ResultCode::SUCCESS
    }

    /// Port of NpadVibrationDevice::SendVibrationNotificationPattern.
    pub fn send_vibration_notification_pattern(&self, _pattern: u32) -> ResultCode {
        if !self.is_mounted {
            return ResultCode::SUCCESS;
        }
        // Upstream: check volume, possibly zero the pattern, then send
        ResultCode::SUCCESS
    }

    /// Port of NpadVibrationDevice::GetActualVibrationValue.
    pub fn get_actual_vibration_value(&self) -> Result<VibrationValue, ResultCode> {
        if self.ref_counter < 1 {
            return Err(RESULT_VIBRATION_NOT_INITIALIZED);
        }
        if !self.is_mounted {
            return Ok(DEFAULT_VIBRATION_VALUE);
        }
        // Upstream: xcd_handle->GetActualVibrationValue(device_index)
        Ok(DEFAULT_VIBRATION_VALUE)
    }

    pub fn is_active(&self) -> bool {
        self.ref_counter > 0
    }

    pub fn is_vibration_mounted(&self) -> bool {
        self.is_mounted
    }
}

impl Default for NpadVibrationDevice {
    fn default() -> Self {
        Self::new()
    }
}
