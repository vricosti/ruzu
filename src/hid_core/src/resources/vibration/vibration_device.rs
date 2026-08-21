// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/vibration/vibration_device.h and vibration_device.cpp

use crate::hid_core::EmulatedControllerHandle;
use crate::hid_result::RESULT_VIBRATION_NOT_INITIALIZED;
use crate::hid_types::{DeviceIndex, VibrationValue, DEFAULT_VIBRATION_VALUE};
use crate::resources::npad::npad_vibration::NpadVibration;
use common::ResultCode;

/// NpadVibrationDevice — handles standard (linear resonant actuator) vibration.
/// Manages activation, mounting to an abstracted pad, and sending vibration
/// values with volume scaling.
pub struct NpadVibrationDevice {
    pub ref_counter: i32,
    pub is_mounted: bool,
    pub device_index: DeviceIndex,
    xcd_handle: Option<EmulatedControllerHandle>,
    vibration_handler: Option<NpadVibration>,
}

impl NpadVibrationDevice {
    pub fn new() -> Self {
        Self {
            ref_counter: 0,
            is_mounted: false,
            device_index: DeviceIndex::None,
            xcd_handle: None,
            vibration_handler: None,
        }
    }

    /// Port of NpadVibrationDevice::Mount.
    pub fn mount(
        &mut self,
        xcd_handle: EmulatedControllerHandle,
        index: DeviceIndex,
        vibration_handler: NpadVibration,
    ) -> ResultCode {
        if !xcd_handle.lock().is_connected(false) {
            return ResultCode::SUCCESS;
        }
        self.xcd_handle = Some(xcd_handle);
        self.device_index = index;
        self.vibration_handler = Some(vibration_handler);
        self.is_mounted = true;

        if self.ref_counter != 0 {
            if let (Some(handler), Some(controller)) = (&self.vibration_handler, &self.xcd_handle) {
                if handler.get_vibration_volume().is_ok() {
                    controller.lock().set_vibration_simple(false);
                }
            }
        }
        ResultCode::SUCCESS
    }

    /// Port of NpadVibrationDevice::Activate.
    /// When first activated while mounted, sends default vibration.
    pub fn activate(&mut self) -> ResultCode {
        if self.ref_counter == 0 && self.is_mounted {
            if let (Some(handler), Some(controller)) = (&self.vibration_handler, &self.xcd_handle) {
                if handler.get_vibration_volume().is_ok() {
                    controller
                        .lock()
                        .set_vibration(self.device_index, DEFAULT_VIBRATION_VALUE);
                }
            }
        }
        self.ref_counter += 1;
        ResultCode::SUCCESS
    }

    /// Port of NpadVibrationDevice::Deactivate.
    /// When last reference is removed while mounted, sends default vibration.
    pub fn deactivate(&mut self) -> ResultCode {
        if self.ref_counter == 1 && self.is_mounted {
            if let (Some(handler), Some(controller)) = (&self.vibration_handler, &self.xcd_handle) {
                if handler.get_vibration_volume().is_ok() {
                    controller
                        .lock()
                        .set_vibration(self.device_index, DEFAULT_VIBRATION_VALUE);
                }
            }
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
        if let (Some(handler), Some(controller)) = (&self.vibration_handler, &self.xcd_handle) {
            if handler.get_vibration_volume().is_ok() {
                controller
                    .lock()
                    .set_vibration(self.device_index, DEFAULT_VIBRATION_VALUE);
            }
        }
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
        let handler = self
            .vibration_handler
            .as_ref()
            .expect("mounted vibration device requires a vibration handler");
        let volume = match handler.get_vibration_volume() {
            Ok(volume) => volume,
            Err(result) => return result,
        };
        let controller = self
            .xcd_handle
            .as_ref()
            .expect("mounted vibration device requires an emulated controller");
        if volume <= 0.0 {
            controller
                .lock()
                .set_vibration(self.device_index, DEFAULT_VIBRATION_VALUE);
            return ResultCode::SUCCESS;
        }

        let mut vibration_value = *value;
        vibration_value.high_amplitude *= volume;
        vibration_value.low_amplitude *= volume;
        controller
            .lock()
            .set_vibration(self.device_index, vibration_value);
        ResultCode::SUCCESS
    }

    /// Port of NpadVibrationDevice::SendVibrationNotificationPattern.
    pub fn send_vibration_notification_pattern(&self, pattern: u32) -> ResultCode {
        if !self.is_mounted {
            return ResultCode::SUCCESS;
        }
        let handler = self
            .vibration_handler
            .as_ref()
            .expect("mounted vibration device requires a vibration handler");
        let volume = match handler.get_vibration_volume() {
            Ok(volume) => volume,
            Err(result) => return result,
        };
        let pattern = if volume <= 0.0 { 0 } else { pattern };
        // Upstream also stops here pending SendVibrationNotificationPattern.
        let _ = pattern;
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
        Ok(self
            .xcd_handle
            .as_ref()
            .expect("mounted vibration device requires an emulated controller")
            .lock()
            .get_actual_vibration_value(self.device_index))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::emulated_controller::EmulatedController;
    use crate::hid_types::{NpadIdType, NpadStyleIndex};
    use parking_lot::Mutex;
    use std::sync::Arc;

    #[test]
    fn send_vibration_value_reaches_the_mounted_emulated_controller() {
        let controller = Arc::new(Mutex::new(EmulatedController::new(NpadIdType::Player1)));
        {
            let mut controller = controller.lock();
            controller.set_npad_style_index(NpadStyleIndex::Fullkey);
            controller.connect(false);
            controller.reload_input();
        }
        let handler = NpadVibration::new();
        assert!(handler.begin_permit_vibration_session(1).is_success());
        let mut device = NpadVibrationDevice::new();
        assert!(device
            .mount(Arc::clone(&controller), DeviceIndex::Left, handler)
            .is_success());
        assert!(device.activate().is_success());

        let vibration = VibrationValue {
            low_amplitude: 0.25,
            low_frequency: 160.0,
            high_amplitude: 0.75,
            high_frequency: 320.0,
        };
        assert!(device.send_vibration_value(&vibration).is_success());
        assert_eq!(device.get_actual_vibration_value().unwrap(), vibration);
        assert_eq!(
            controller
                .lock()
                .get_actual_vibration_value(DeviceIndex::Left),
            vibration
        );
    }
}
