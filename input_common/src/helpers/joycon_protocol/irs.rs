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
    pub fn enable_irs(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of IrsProtocol::DisableIrs
    pub fn disable_irs(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of IrsProtocol::SetIrsConfig
    pub fn set_irs_config(&mut self, _mode: IrsMode, _format: IrsResolution) -> DriverResult {
        todo!()
    }

    /// Port of IrsProtocol::RequestImage
    pub fn request_image(&mut self, _buffer: &mut [u8]) -> DriverResult {
        todo!()
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

    fn configure_irs(&mut self) -> DriverResult {
        todo!()
    }

    fn write_registers_step1(&mut self) -> DriverResult {
        todo!()
    }

    fn write_registers_step2(&mut self) -> DriverResult {
        todo!()
    }

    fn request_frame(&mut self, _frame: u8) -> DriverResult {
        todo!()
    }

    fn resend_frame(&mut self, _frame: u8) -> DriverResult {
        todo!()
    }
}
