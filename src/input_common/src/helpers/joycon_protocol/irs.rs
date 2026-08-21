// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/irs.h` and `irs.cpp`.
//!
//! Based on dkms-hid-nintendo implementation, CTCaer joycon toolkit and dekuNukem reverse
//! engineering.
//!
//! IR sensor protocol implementation.

use common::input::DriverResult;

use super::common_protocol::JoyconCommonProtocol;
use super::joycon_types::*;

/// Port of `IrsProtocol` class from irs.h / irs.cpp
pub struct IrsProtocol {
    protocol: JoyconCommonProtocol,
    irs_mode: IrsMode,
    resolution: IrsResolution,
    resolution_code: IrsResolutionCode,
    fragments: IrsFragments,
    leds: IrLeds,
    led_filter: IrExLedFilter,
    image_flip: IrImageFlip,
    digital_gain: u8,
    exposure: u16,
    led_intensity: u16,
    denoise: u32,
    packet_fragment: u8,
    buf_image: Vec<u8>,
    is_enabled: bool,
}

impl IrsProtocol {
    /// Port of IrsProtocol::IrsProtocol
    pub fn new() -> Self {
        Self {
            protocol: JoyconCommonProtocol::new(),
            irs_mode: IrsMode::ImageTransfer,
            resolution: IrsResolution::Size40x30,
            resolution_code: IrsResolutionCode::Size40x30,
            fragments: IrsFragments::Size40x30,
            leds: IrLeds::BrightAndDim,
            led_filter: IrExLedFilter::Enabled,
            image_flip: IrImageFlip::Normal,
            digital_gain: 0x01,
            exposure: 0x2490,
            led_intensity: 0x0f10,
            denoise: 0x012344,
            packet_fragment: 0,
            buf_image: Vec::new(),
            is_enabled: false,
        }
    }

    /// Port of IrsProtocol::EnableIrs
    ///
    /// NOTE: Full implementation requires MCU configuration via hidapi handle.
    /// The sequence: SetReportMode(NFC_IR_MODE_60HZ) -> EnableMCU(true) ->
    /// WaitSetMCUMode(Standby) -> ConfigureMCU(IR mode) -> WaitSetMCUMode(IR) ->
    /// ConfigureIrs -> WriteRegistersStep1 -> WriteRegistersStep2.
    /// All depend on the hidapi handle being available.
    pub fn enable_irs(&mut self) -> DriverResult {
        log::info!("enable_irs: IRS enable requested");
        use super::common_protocol::ScopedSetBlocking;
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;

        if result == DriverResult::Success {
            result = self.protocol.set_report_mode(ReportMode::NfcIrMode60Hz);
        }
        if result == DriverResult::Success {
            result = self.protocol.enable_mcu(true);
        }
        if result == DriverResult::Success {
            result = self
                .protocol
                .wait_set_mcu_mode(ReportMode::NfcIrMode60Hz, McuMode::Standby);
        }
        if result == DriverResult::Success {
            result = self.protocol.configure_mcu();
        }
        if result == DriverResult::Success {
            result = self
                .protocol
                .wait_set_mcu_mode(ReportMode::NfcIrMode60Hz, McuMode::Ir);
        }
        if result == DriverResult::Success {
            result = self.configure_irs();
        }
        if result == DriverResult::Success {
            result = self.write_registers_step1();
        }
        if result == DriverResult::Success {
            result = self.write_registers_step2();
        }

        self.is_enabled = true;
        result
    }

    /// Port of IrsProtocol::DisableIrs
    pub fn disable_irs(&mut self) -> DriverResult {
        log::debug!("disable_irs: IRS disable requested");
        use super::common_protocol::ScopedSetBlocking;
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;

        if result == DriverResult::Success {
            result = self.protocol.enable_mcu(false);
        }

        self.is_enabled = false;
        result
    }

    /// Port of IrsProtocol::SetIrsConfig
    pub fn set_irs_config(&mut self, mode: IrsMode, format: IrsResolution) -> DriverResult {
        self.irs_mode = mode;
        match format {
            IrsResolution::Size320x240 => {
                self.resolution_code = IrsResolutionCode::Size320x240;
                self.fragments = IrsFragments::Size320x240;
                self.resolution = IrsResolution::Size320x240;
            }
            IrsResolution::Size160x120 => {
                self.resolution_code = IrsResolutionCode::Size160x120;
                self.fragments = IrsFragments::Size160x120;
                self.resolution = IrsResolution::Size160x120;
            }
            IrsResolution::Size80x60 => {
                self.resolution_code = IrsResolutionCode::Size80x60;
                self.fragments = IrsFragments::Size80x60;
                self.resolution = IrsResolution::Size80x60;
            }
            IrsResolution::Size20x15 => {
                self.resolution_code = IrsResolutionCode::Size20x15;
                self.fragments = IrsFragments::Size20x15;
                self.resolution = IrsResolution::Size20x15;
            }
            IrsResolution::Size40x30 | IrsResolution::None => {
                self.resolution_code = IrsResolutionCode::Size40x30;
                self.fragments = IrsFragments::Size40x30;
                self.resolution = IrsResolution::Size40x30;
            }
        }

        // Restart feature if already enabled
        if self.is_enabled {
            self.disable_irs();
            return self.enable_irs();
        }

        DriverResult::Success
    }

    /// Port of IrsProtocol::RequestImage
    pub fn request_image(&mut self, buffer: &mut [u8]) -> DriverResult {
        let fragments_val = self.fragments as u8;
        let next_packet_fragment = (self.packet_fragment + 1) % (fragments_val + 1);

        if buffer.len() > 52 && buffer[0] == 0x31 && buffer[49] == 0x03 {
            let new_packet_fragment = buffer[52];
            if new_packet_fragment == next_packet_fragment {
                self.packet_fragment = next_packet_fragment;
                let dest_offset = 300 * self.packet_fragment as usize;
                if buffer.len() >= 59 + 300 && self.buf_image.len() >= dest_offset + 300 {
                    self.buf_image[dest_offset..dest_offset + 300]
                        .copy_from_slice(&buffer[59..59 + 300]);
                }
                return self.request_frame(self.packet_fragment);
            }

            if new_packet_fragment == self.packet_fragment {
                return self.request_frame(self.packet_fragment);
            }

            return self.resend_frame(next_packet_fragment);
        }

        self.request_frame(self.packet_fragment)
    }

    /// Port of IrsProtocol::GetImage
    pub fn get_image(&self) -> Vec<u8> {
        self.buf_image.clone()
    }

    /// Port of IrsProtocol::GetIrsFormat
    pub fn get_irs_format(&self) -> IrsResolution {
        self.resolution
    }

    /// Port of IrsProtocol::IsEnabled
    pub fn is_enabled(&self) -> bool {
        self.is_enabled
    }

    // ---- Private methods ----

    /// Port of IrsProtocol::ConfigureIrs
    ///
    /// NOTE: Upstream builds an IrsConfigure packet, computes CRC, and polls
    /// SendSubCommand(SET_MCU_CONFIG) until output.command_data[0] == 0x0b.
    /// Depends on the hidapi handle via send_sub_command.
    fn configure_irs(&mut self) -> DriverResult {
        log::debug!("configure_irs: IRS configuration requires hidapi handle");
        // Resize buf_image for the chosen resolution
        self.buf_image
            .resize((self.fragments as usize + 1) * 300, 0);
        // The actual MCU configuration requires SendSubCommand which depends on hidapi.
        log::warn!("configure_irs: no hidapi handle available; MCU not configured");
        DriverResult::ErrorWritingData
    }

    /// Port of IrsProtocol::WriteRegistersStep1
    ///
    /// NOTE: Depends on SendSubCommand and SendMCUCommand via hidapi handle.
    fn write_registers_step1(&mut self) -> DriverResult {
        log::warn!("write_registers_step1: no hidapi handle available");
        DriverResult::ErrorWritingData
    }

    /// Port of IrsProtocol::WriteRegistersStep2
    ///
    /// NOTE: Depends on SendSubCommand via hidapi handle.
    fn write_registers_step2(&mut self) -> DriverResult {
        log::warn!("write_registers_step2: no hidapi handle available");
        DriverResult::ErrorWritingData
    }

    /// Port of IrsProtocol::RequestFrame
    ///
    /// NOTE: Builds a 38-byte MCU request where byte[3] = frame, computes CRC at [36],
    /// sets [37] = 0xFF, and sends via SendMCUCommand(SET_REPORT_MODE).
    /// Depends on the hidapi handle.
    fn request_frame(&mut self, frame: u8) -> DriverResult {
        let mut mcu_request = [0u8; 38];
        mcu_request[3] = frame;
        mcu_request[36] = self.protocol.calculate_mcu_crc8(&mcu_request[..36]);
        mcu_request[37] = 0xFF;
        self.protocol
            .send_mcu_command(SubCommand::SetReportMode, &mcu_request)
    }

    /// Port of IrsProtocol::ResendFrame
    ///
    /// NOTE: Builds a 38-byte MCU request for resend and sends via SendMCUCommand.
    /// Depends on the hidapi handle.
    fn resend_frame(&mut self, frame: u8) -> DriverResult {
        let mut mcu_request = [0u8; 38];
        mcu_request[1] = 0x1;
        mcu_request[2] = frame;
        mcu_request[3] = 0x0;
        mcu_request[36] = self.protocol.calculate_mcu_crc8(&mcu_request[..36]);
        mcu_request[37] = 0xFF;
        self.protocol
            .send_mcu_command(SubCommand::SetReportMode, &mcu_request)
    }
}
