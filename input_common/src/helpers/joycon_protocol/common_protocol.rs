// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/common_protocol.h` and `common_protocol.cpp`.
//!
//! Based on dkms-hid-nintendo implementation, CTCaer joycon toolkit and dekuNukem reverse
//! engineering.
//!
//! Joycon driver functions that handle low level communication.

use common::input::DriverResult;

use super::joycon_types::*;

/// Port of `JoyconCommonProtocol` class from common_protocol.h / common_protocol.cpp
pub struct JoyconCommonProtocol {
    // hidapi_handle would be a shared pointer to JoyconHandle
}

impl JoyconCommonProtocol {
    /// Port of JoyconCommonProtocol::JoyconCommonProtocol
    pub fn new() -> Self {
        Self {}
    }

    /// Sets handle to blocking.
    /// Port of JoyconCommonProtocol::SetBlocking
    pub fn set_blocking(&mut self) {
        todo!()
    }

    /// Sets handle to non blocking.
    /// Port of JoyconCommonProtocol::SetNonBlocking
    pub fn set_non_blocking(&mut self) {
        todo!()
    }

    /// Sends a request to obtain the joycon type from device.
    /// Port of JoyconCommonProtocol::GetDeviceType
    pub fn get_device_type(&mut self, _controller_type: &mut ControllerType) -> DriverResult {
        todo!()
    }

    /// Verifies and sets the joycon_handle if device is valid.
    /// Port of JoyconCommonProtocol::CheckDeviceAccess
    pub fn check_device_access(&mut self) -> DriverResult {
        todo!()
    }

    /// Sends a request to set the polling mode of the joycon.
    /// Port of JoyconCommonProtocol::SetReportMode
    pub fn set_report_mode(&mut self, _report_mode: ReportMode) -> DriverResult {
        todo!()
    }

    /// Sends data to the joycon device.
    /// Port of JoyconCommonProtocol::SendRawData
    pub fn send_raw_data(&mut self, _buffer: &[u8]) -> DriverResult {
        todo!()
    }

    /// Waits for incoming data that matches the subcommand.
    /// Port of JoyconCommonProtocol::GetSubCommandResponse
    pub fn get_sub_command_response(&mut self, _sub_command: SubCommand) -> DriverResult {
        todo!()
    }

    /// Sends a sub command to the device and waits for its reply.
    /// Port of JoyconCommonProtocol::SendSubCommand
    pub fn send_sub_command(&mut self, _sc: SubCommand, _buffer: &[u8]) -> DriverResult {
        todo!()
    }

    /// Sends a mcu command to the device.
    /// Port of JoyconCommonProtocol::SendMCUCommand
    pub fn send_mcu_command(&mut self, _sc: SubCommand, _buffer: &[u8]) -> DriverResult {
        todo!()
    }

    /// Sends vibration data to the joycon.
    /// Port of JoyconCommonProtocol::SendVibrationReport
    pub fn send_vibration_report(&mut self, _buffer: &[u8]) -> DriverResult {
        todo!()
    }

    /// Reads the SPI memory stored on the joycon.
    /// Port of JoyconCommonProtocol::ReadRawSPI
    pub fn read_raw_spi(&mut self, _addr: SpiAddress, _output: &mut [u8]) -> DriverResult {
        todo!()
    }

    /// Enables MCU chip on the joycon.
    /// Port of JoyconCommonProtocol::EnableMCU
    pub fn enable_mcu(&mut self, _enable: bool) -> DriverResult {
        todo!()
    }

    /// Configures the MCU to the corresponding mode.
    /// Port of JoyconCommonProtocol::ConfigureMCU
    pub fn configure_mcu(&mut self) -> DriverResult {
        todo!()
    }

    /// Waits until there's MCU data available.
    /// Port of JoyconCommonProtocol::GetMCUDataResponse
    pub fn get_mcu_data_response(&mut self, _report_mode: ReportMode) -> DriverResult {
        todo!()
    }

    /// Sends data to the MCU chip and waits for its reply.
    /// Port of JoyconCommonProtocol::SendMCUData
    pub fn send_mcu_data(
        &mut self,
        _report_mode: ReportMode,
        _sc: McuSubCommand,
        _buffer: &[u8],
    ) -> DriverResult {
        todo!()
    }

    /// Wait's until the MCU chip is on the specified mode.
    /// Port of JoyconCommonProtocol::WaitSetMCUMode
    pub fn wait_set_mcu_mode(
        &mut self,
        _report_mode: ReportMode,
        _mode: McuMode,
    ) -> DriverResult {
        todo!()
    }

    /// Calculates the checksum from the MCU data.
    /// Port of JoyconCommonProtocol::CalculateMCU_CRC8
    pub fn calculate_mcu_crc8(&self, _buffer: &[u8]) -> u8 {
        todo!()
    }
}

/// RAII helper that sets blocking on creation and non-blocking on drop.
/// Port of `ScopedSetBlocking` from common_protocol.h
///
/// NOTE: In C++ this holds a pointer to the protocol and calls SetNonBlocking
/// in the destructor. In Rust, holding `&mut` would prevent further calls on
/// the protocol. Instead we store a raw pointer and use unsafe in Drop.
pub struct ScopedSetBlocking {
    protocol: *mut JoyconCommonProtocol,
}

impl ScopedSetBlocking {
    pub fn new(protocol: &mut JoyconCommonProtocol) -> Self {
        protocol.set_blocking();
        Self { protocol: protocol as *mut _ }
    }
}

impl Drop for ScopedSetBlocking {
    fn drop(&mut self) {
        // SAFETY: The protocol reference is guaranteed to outlive this guard
        // because it is always created from a &mut in the same scope.
        unsafe { &mut *self.protocol }.set_non_blocking();
    }
}
