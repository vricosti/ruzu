// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/generic_functions.h` and `generic_functions.cpp`.
//!
//! Based on dkms-hid-nintendo implementation, CTCaer joycon toolkit and dekuNukem reverse
//! engineering.
//!
//! Joycon driver functions that are easily implemented.

use common::input::DriverResult;

use super::common_protocol::{JoyconCommonProtocol, ScopedSetBlocking};
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

    /// Enables passive mode. This mode only sends button data on change. Sticks will return digital
    /// data instead of analog. Motion will be disabled.
    /// Port of GenericProtocol::EnablePassiveMode
    pub fn enable_passive_mode(&mut self) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        self.protocol.set_report_mode(ReportMode::SimpleHidMode)
    }

    /// Enables active mode. This mode will return the current status every 5-15ms.
    /// Port of GenericProtocol::EnableActiveMode
    pub fn enable_active_mode(&mut self) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        self.protocol.set_report_mode(ReportMode::StandardFull60Hz)
    }

    /// Enables or disables the low power mode.
    /// Port of GenericProtocol::SetLowPowerMode
    pub fn set_low_power_mode(&mut self, enable: bool) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let buffer = [if enable { 1u8 } else { 0u8 }];
        self.protocol
            .send_sub_command(SubCommand::LowPowerMode, &buffer)
    }

    /// Unknown function used by the switch.
    /// Port of GenericProtocol::TriggersElapsed
    pub fn triggers_elapsed(&mut self) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        self.protocol
            .send_sub_command(SubCommand::TriggersElapsed, &[])
    }

    /// Sends a request to obtain the joycon firmware and mac from handle.
    /// Port of GenericProtocol::GetDeviceInfo
    ///
    /// NOTE: Upstream calls SendSubCommand(REQ_DEV_INFO, {}, output) and extracts
    /// output.device_info from the SubCommandResponse union. The SubCommandResponse struct
    /// requires hidapi handle wiring for the read loop; that infrastructure is not yet
    /// available in this port.
    pub fn get_device_info(&mut self, device_info: &mut DeviceInfo) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        *device_info = DeviceInfo {
            firmware: FirmwareVersion::default(),
            unknown_1: [0; 2],
            mac_address: [0; 6],
            unknown_2: [0; 2],
        };
        log::warn!("get_device_info: SubCommandResponse not yet wired; no hidapi handle available");
        DriverResult::ErrorReadingData
    }

    /// Sends a request to obtain the joycon type from handle.
    /// Port of GenericProtocol::GetControllerType
    pub fn get_controller_type(&mut self, controller_type: &mut ControllerType) -> DriverResult {
        self.protocol.get_device_type(controller_type)
    }

    /// Enables motion input.
    /// Port of GenericProtocol::EnableImu
    pub fn enable_imu(&mut self, enable: bool) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let buffer = [if enable { 1u8 } else { 0u8 }];
        self.protocol
            .send_sub_command(SubCommand::EnableImu, &buffer)
    }

    /// Configures the motion sensor with the specified parameters.
    /// Port of GenericProtocol::SetImuConfig
    pub fn set_imu_config(
        &mut self,
        gsen: GyroSensitivity,
        gfrec: GyroPerformance,
        asen: AccelerometerSensitivity,
        afrec: AccelerometerPerformance,
    ) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let buffer = [gsen as u8, asen as u8, gfrec as u8, afrec as u8];
        self.protocol
            .send_sub_command(SubCommand::SetImuSensitivity, &buffer)
    }

    /// Request battery level from the device.
    /// Port of GenericProtocol::GetBattery
    pub fn get_battery(&mut self, battery_level: &mut u32) -> DriverResult {
        // This function is meant to request the high resolution battery status
        *battery_level = 0;
        DriverResult::NotSupported
    }

    /// Request joycon colors from the device.
    /// Port of GenericProtocol::GetColor
    pub fn get_color(&mut self, color: &mut Color) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut buffer = [0u8; 12];
        let result = self
            .protocol
            .read_raw_spi(SpiAddress::ColorData, &mut buffer);

        *color = Color::default();
        if result == DriverResult::Success {
            color.body =
                ((buffer[0] as u32) << 16) | ((buffer[1] as u32) << 8) | (buffer[2] as u32);
            color.buttons =
                ((buffer[3] as u32) << 16) | ((buffer[4] as u32) << 8) | (buffer[5] as u32);
            color.left_grip =
                ((buffer[6] as u32) << 16) | ((buffer[7] as u32) << 8) | (buffer[8] as u32);
            color.right_grip =
                ((buffer[9] as u32) << 16) | ((buffer[10] as u32) << 8) | (buffer[11] as u32);
        }

        result
    }

    /// Request joycon serial number from the device.
    /// Port of GenericProtocol::GetSerialNumber
    pub fn get_serial_number(&mut self, serial_number: &mut SerialNumber) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut buffer = [0u8; 16];
        let result = self
            .protocol
            .read_raw_spi(SpiAddress::SerialNumber, &mut buffer);

        *serial_number = [0u8; 15];
        if result == DriverResult::Success {
            serial_number.copy_from_slice(&buffer[1..16]);
        }

        result
    }

    /// Request joycon temperature from the device.
    /// Port of GenericProtocol::GetTemperature
    pub fn get_temperature(&mut self, temperature: &mut u32) -> DriverResult {
        // Not all devices have temperature sensor
        *temperature = 25;
        DriverResult::NotSupported
    }

    /// Request joycon firmware version from the device.
    /// Port of GenericProtocol::GetVersionNumber
    pub fn get_version_number(&mut self, version: &mut FirmwareVersion) -> DriverResult {
        let mut device_info = DeviceInfo {
            firmware: FirmwareVersion::default(),
            unknown_1: [0; 2],
            mac_address: [0; 6],
            unknown_2: [0; 2],
        };

        let result = self.get_device_info(&mut device_info);
        *version = device_info.firmware;

        result
    }

    /// Sets home led behaviour.
    /// Port of GenericProtocol::SetHomeLight
    pub fn set_home_light(&mut self) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let buffer: [u8; 3] = [0x0f, 0xf0, 0x00];
        self.protocol
            .send_sub_command(SubCommand::SetHomeLight, &buffer)
    }

    /// Sets home led into a slow breathing state.
    /// Port of GenericProtocol::SetLedBusy
    pub fn set_led_busy(&mut self) -> DriverResult {
        DriverResult::NotSupported
    }

    /// Sets the 4 player leds on the joycon on a solid state.
    /// Port of GenericProtocol::SetLedPattern
    pub fn set_led_pattern(&mut self, leds: u8) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let buffer = [leds];
        self.protocol
            .send_sub_command(SubCommand::SetPlayerLights, &buffer)
    }

    /// Sets the 4 player leds on the joycon on a blinking state.
    /// Port of GenericProtocol::SetLedBlinkPattern
    pub fn set_led_blink_pattern(&mut self, leds: u8) -> DriverResult {
        self.set_led_pattern(leds << 4)
    }
}
