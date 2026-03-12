// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/generic_functions.h` and `generic_functions.cpp`.
//!
//! Based on dkms-hid-nintendo implementation, CTCaer joycon toolkit and dekuNukem reverse
//! engineering.
//!
//! Joycon driver functions that are easily implemented.

use common::input::DriverResult;

use super::common_protocol::JoyconCommonProtocol;
use super::joycon_types::*;

/// Port of `GenericProtocol` class from generic_functions.h / generic_functions.cpp
pub struct GenericProtocol {
    protocol: JoyconCommonProtocol,
}

impl GenericProtocol {
    /// Port of GenericProtocol::GenericProtocol
    pub fn new() -> Self {
        Self {
            protocol: JoyconCommonProtocol::new(),
        }
    }

    /// Enables passive mode.
    /// Port of GenericProtocol::EnablePassiveMode
    pub fn enable_passive_mode(&mut self) -> DriverResult {
        todo!()
    }

    /// Enables active mode.
    /// Port of GenericProtocol::EnableActiveMode
    pub fn enable_active_mode(&mut self) -> DriverResult {
        todo!()
    }

    /// Enables or disables the low power mode.
    /// Port of GenericProtocol::SetLowPowerMode
    pub fn set_low_power_mode(&mut self, _enable: bool) -> DriverResult {
        todo!()
    }

    /// Port of GenericProtocol::TriggersElapsed
    pub fn triggers_elapsed(&mut self) -> DriverResult {
        todo!()
    }

    /// Sends a request to obtain the joycon firmware and mac from handle.
    /// Port of GenericProtocol::GetDeviceInfo
    pub fn get_device_info(&mut self, _device_info: &mut DeviceInfo) -> DriverResult {
        todo!()
    }

    /// Sends a request to obtain the joycon type from handle.
    /// Port of GenericProtocol::GetControllerType
    pub fn get_controller_type(
        &mut self,
        _controller_type: &mut ControllerType,
    ) -> DriverResult {
        todo!()
    }

    /// Enables motion input.
    /// Port of GenericProtocol::EnableImu
    pub fn enable_imu(&mut self, _enable: bool) -> DriverResult {
        todo!()
    }

    /// Configures the motion sensor with the specified parameters.
    /// Port of GenericProtocol::SetImuConfig
    pub fn set_imu_config(
        &mut self,
        _gsen: GyroSensitivity,
        _gfrec: GyroPerformance,
        _asen: AccelerometerSensitivity,
        _afrec: AccelerometerPerformance,
    ) -> DriverResult {
        todo!()
    }

    /// Request battery level from the device.
    /// Port of GenericProtocol::GetBattery
    pub fn get_battery(&mut self, _battery_level: &mut u32) -> DriverResult {
        todo!()
    }

    /// Request joycon colors from the device.
    /// Port of GenericProtocol::GetColor
    pub fn get_color(&mut self, _color: &mut Color) -> DriverResult {
        todo!()
    }

    /// Request joycon serial number from the device.
    /// Port of GenericProtocol::GetSerialNumber
    pub fn get_serial_number(&mut self, _serial_number: &mut SerialNumber) -> DriverResult {
        todo!()
    }

    /// Request joycon temperature from the device.
    /// Port of GenericProtocol::GetTemperature
    pub fn get_temperature(&mut self, _temperature: &mut u32) -> DriverResult {
        todo!()
    }

    /// Request joycon firmware version from the device.
    /// Port of GenericProtocol::GetVersionNumber
    pub fn get_version_number(&mut self, _version: &mut FirmwareVersion) -> DriverResult {
        todo!()
    }

    /// Sets home led behaviour.
    /// Port of GenericProtocol::SetHomeLight
    pub fn set_home_light(&mut self) -> DriverResult {
        todo!()
    }

    /// Sets home led into a slow breathing state.
    /// Port of GenericProtocol::SetLedBusy
    pub fn set_led_busy(&mut self) -> DriverResult {
        todo!()
    }

    /// Sets the 4 player leds on the joycon on a solid state.
    /// Port of GenericProtocol::SetLedPattern
    pub fn set_led_pattern(&mut self, _leds: u8) -> DriverResult {
        todo!()
    }

    /// Sets the 4 player leds on the joycon on a blinking state.
    /// Port of GenericProtocol::SetLedBlinkPattern
    pub fn set_led_blink_pattern(&mut self, _leds: u8) -> DriverResult {
        todo!()
    }
}
