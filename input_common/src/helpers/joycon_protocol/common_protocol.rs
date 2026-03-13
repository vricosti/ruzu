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
    ///
    /// NOTE: Upstream calls SDL_hid_set_nonblocking(handle, 0). No hidapi handle is wired
    /// in this port, so this is a no-op stub.
    pub fn set_blocking(&mut self) {
        // No hidapi handle available in this port.
    }

    /// Sets handle to non blocking.
    /// Port of JoyconCommonProtocol::SetNonBlocking
    ///
    /// NOTE: Upstream calls SDL_hid_set_nonblocking(handle, 1). No hidapi handle is wired
    /// in this port, so this is a no-op stub.
    pub fn set_non_blocking(&mut self) {
        // No hidapi handle available in this port.
    }

    /// Sends a request to obtain the joycon type from device.
    /// Port of JoyconCommonProtocol::GetDeviceType
    ///
    /// NOTE: Upstream calls ReadSPI(SpiAddress::DEVICE_TYPE, controller_type) which requires
    /// a live hidapi handle. Without one, we cannot determine the device type.
    pub fn get_device_type(&mut self, _controller_type: &mut ControllerType) -> DriverResult {
        log::warn!("get_device_type: no hidapi handle available");
        DriverResult::InvalidHandle
    }

    /// Verifies and sets the joycon_handle if device is valid.
    /// Port of JoyconCommonProtocol::CheckDeviceAccess
    ///
    /// NOTE: Upstream calls GetDeviceType then SDL_hid_open. No hidapi handle is wired.
    pub fn check_device_access(&mut self) -> DriverResult {
        log::warn!("check_device_access: no hidapi handle available");
        DriverResult::InvalidHandle
    }

    /// Sends a request to set the polling mode of the joycon.
    /// Port of JoyconCommonProtocol::SetReportMode
    pub fn set_report_mode(&mut self, report_mode: ReportMode) -> DriverResult {
        let buffer = [report_mode as u8];
        self.send_sub_command(SubCommand::SetReportMode, &buffer)
    }

    /// Sends data to the joycon device.
    /// Port of JoyconCommonProtocol::SendRawData
    ///
    /// NOTE: Upstream calls SDL_hid_write. No hidapi handle available.
    pub fn send_raw_data(&mut self, _buffer: &[u8]) -> DriverResult {
        log::warn!("send_raw_data: no hidapi handle available");
        DriverResult::ErrorWritingData
    }

    /// Waits for incoming data that matches the subcommand.
    /// Port of JoyconCommonProtocol::GetSubCommandResponse
    ///
    /// NOTE: Upstream calls SDL_hid_read_timeout in a loop. No hidapi handle available.
    pub fn get_sub_command_response(&mut self, _sub_command: SubCommand) -> DriverResult {
        log::warn!("get_sub_command_response: no hidapi handle available");
        DriverResult::Timeout
    }

    /// Sends a sub command to the device and waits for its reply.
    /// Port of JoyconCommonProtocol::SendSubCommand
    ///
    /// NOTE: Upstream builds a SubCommandPacket and calls SendData + GetSubCommandResponse.
    /// Both depend on the hidapi handle.
    pub fn send_sub_command(&mut self, _sc: SubCommand, _buffer: &[u8]) -> DriverResult {
        log::warn!("send_sub_command: no hidapi handle available");
        DriverResult::ErrorWritingData
    }

    /// Sends a mcu command to the device.
    /// Port of JoyconCommonProtocol::SendMCUCommand
    ///
    /// NOTE: Upstream builds a SubCommandPacket with OutputReport::MCU_DATA and calls SendData.
    /// Depends on the hidapi handle.
    pub fn send_mcu_command(&mut self, _sc: SubCommand, _buffer: &[u8]) -> DriverResult {
        log::warn!("send_mcu_command: no hidapi handle available");
        DriverResult::ErrorWritingData
    }

    /// Sends vibration data to the joycon.
    /// Port of JoyconCommonProtocol::SendVibrationReport
    ///
    /// NOTE: Upstream builds a VibrationPacket and calls SendData. Depends on the hidapi handle.
    pub fn send_vibration_report(&mut self, _buffer: &[u8]) -> DriverResult {
        log::warn!("send_vibration_report: no hidapi handle available");
        DriverResult::ErrorWritingData
    }

    /// Reads the SPI memory stored on the joycon.
    /// Port of JoyconCommonProtocol::ReadRawSPI
    ///
    /// NOTE: Upstream sends a SPI_FLASH_READ subcommand and reads back the response via
    /// SDL_hid_read_timeout. No hidapi handle available.
    pub fn read_raw_spi(&mut self, _addr: SpiAddress, _output: &mut [u8]) -> DriverResult {
        log::warn!("read_raw_spi: no hidapi handle available");
        DriverResult::ErrorReadingData
    }

    /// Enables MCU chip on the joycon.
    /// Port of JoyconCommonProtocol::EnableMCU
    pub fn enable_mcu(&mut self, enable: bool) -> DriverResult {
        let mcu_state = [if enable { 1u8 } else { 0u8 }];
        let result = self.send_sub_command(SubCommand::SetMcuState, &mcu_state);
        if result != DriverResult::Success {
            log::error!("enable_mcu: failed with error {:?}", result);
        }
        result
    }

    /// Configures the MCU to the corresponding mode.
    /// Port of JoyconCommonProtocol::ConfigureMCU
    ///
    /// NOTE: Upstream takes an MCUConfig struct, serializes it, computes CRC, and sends it
    /// via send_sub_command(SET_MCU_CONFIG). The MCUConfig struct is not yet in joycon_types.rs,
    /// so this delegates the log and returns an error to avoid a panic.
    pub fn configure_mcu(&mut self) -> DriverResult {
        log::warn!("configure_mcu: MCUConfig struct not yet wired; no hidapi handle available");
        DriverResult::NotSupported
    }

    /// Waits until there's MCU data available.
    /// Port of JoyconCommonProtocol::GetMCUDataResponse
    ///
    /// NOTE: Upstream calls SDL_hid_read_timeout in a loop. No hidapi handle available.
    pub fn get_mcu_data_response(&mut self, _report_mode: ReportMode) -> DriverResult {
        log::warn!("get_mcu_data_response: no hidapi handle available");
        DriverResult::Timeout
    }

    /// Sends data to the MCU chip and waits for its reply.
    /// Port of JoyconCommonProtocol::SendMCUData
    ///
    /// NOTE: Upstream builds a SubCommandPacket and calls SendData + GetMCUDataResponse.
    /// Both depend on the hidapi handle.
    pub fn send_mcu_data(
        &mut self,
        _report_mode: ReportMode,
        _sc: McuSubCommand,
        _buffer: &[u8],
    ) -> DriverResult {
        log::warn!("send_mcu_data: no hidapi handle available");
        DriverResult::ErrorWritingData
    }

    /// Wait's until the MCU chip is on the specified mode.
    /// Port of JoyconCommonProtocol::WaitSetMCUMode
    ///
    /// Upstream calls SendMCUData(SetDeviceMode) in a loop until output.mcu_data[6] matches
    /// the requested mode. Depends on the hidapi handle.
    pub fn wait_set_mcu_mode(
        &mut self,
        _report_mode: ReportMode,
        _mode: McuMode,
    ) -> DriverResult {
        log::warn!("wait_set_mcu_mode: no hidapi handle available");
        DriverResult::Timeout
    }

    /// Calculates the checksum from the MCU data.
    /// Port of JoyconCommonProtocol::CalculateMCU_CRC8
    ///
    /// crc-8-ccitt / polynomial 0x07 look up table.
    pub fn calculate_mcu_crc8(&self, buffer: &[u8]) -> u8 {
        // crc-8-ccitt / polynomial 0x07 look up table
        const MCU_CRC8_TABLE: [u8; 256] = [
            0x00, 0x07, 0x0E, 0x09, 0x1C, 0x1B, 0x12, 0x15, 0x38, 0x3F, 0x36, 0x31, 0x24, 0x23,
            0x2A, 0x2D, 0x70, 0x77, 0x7E, 0x79, 0x6C, 0x6B, 0x62, 0x65, 0x48, 0x4F, 0x46, 0x41,
            0x54, 0x53, 0x5A, 0x5D, 0xE0, 0xE7, 0xEE, 0xE9, 0xFC, 0xFB, 0xF2, 0xF5, 0xD8, 0xDF,
            0xD6, 0xD1, 0xC4, 0xC3, 0xCA, 0xCD, 0x90, 0x97, 0x9E, 0x99, 0x8C, 0x8B, 0x82, 0x85,
            0xA8, 0xAF, 0xA6, 0xA1, 0xB4, 0xB3, 0xBA, 0xBD, 0xC7, 0xC0, 0xC9, 0xCE, 0xDB, 0xDC,
            0xD5, 0xD2, 0xFF, 0xF8, 0xF1, 0xF6, 0xE3, 0xE4, 0xED, 0xEA, 0xB7, 0xB0, 0xB9, 0xBE,
            0xAB, 0xAC, 0xA5, 0xA2, 0x8F, 0x88, 0x81, 0x86, 0x93, 0x94, 0x9D, 0x9A, 0x27, 0x20,
            0x29, 0x2E, 0x3B, 0x3C, 0x35, 0x32, 0x1F, 0x18, 0x11, 0x16, 0x03, 0x04, 0x0D, 0x0A,
            0x57, 0x50, 0x59, 0x5E, 0x4B, 0x4C, 0x45, 0x42, 0x6F, 0x68, 0x61, 0x66, 0x73, 0x74,
            0x7D, 0x7A, 0x89, 0x8E, 0x87, 0x80, 0x95, 0x92, 0x9B, 0x9C, 0xB1, 0xB6, 0xBF, 0xB8,
            0xAD, 0xAA, 0xA3, 0xA4, 0xF9, 0xFE, 0xF7, 0xF0, 0xE5, 0xE2, 0xEB, 0xEC, 0xC1, 0xC6,
            0xCF, 0xC8, 0xDD, 0xDA, 0xD3, 0xD4, 0x69, 0x6E, 0x67, 0x60, 0x75, 0x72, 0x7B, 0x7C,
            0x51, 0x56, 0x5F, 0x58, 0x4D, 0x4A, 0x43, 0x44, 0x19, 0x1E, 0x17, 0x10, 0x05, 0x02,
            0x0B, 0x0C, 0x21, 0x26, 0x2F, 0x28, 0x3D, 0x3A, 0x33, 0x34, 0x4E, 0x49, 0x40, 0x47,
            0x52, 0x55, 0x5C, 0x5B, 0x76, 0x71, 0x78, 0x7F, 0x6A, 0x6D, 0x64, 0x63, 0x3E, 0x39,
            0x30, 0x37, 0x22, 0x25, 0x2C, 0x2B, 0x06, 0x01, 0x08, 0x0F, 0x1A, 0x1D, 0x14, 0x13,
            0xAE, 0xA9, 0xA0, 0xA7, 0xB2, 0xB5, 0xBC, 0xBB, 0x96, 0x91, 0x98, 0x9F, 0x8A, 0x8D,
            0x84, 0x83, 0xDE, 0xD9, 0xD0, 0xD7, 0xC2, 0xC5, 0xCC, 0xCB, 0xE6, 0xE1, 0xE8, 0xEF,
            0xFA, 0xFD, 0xF4, 0xF3,
        ];

        let mut crc8: u8 = 0x0;
        for &byte in buffer {
            crc8 = MCU_CRC8_TABLE[(crc8 ^ byte) as usize];
        }
        crc8
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
