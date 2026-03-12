// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/rumble.h` and `rumble.cpp`.
//!
//! Based on dkms-hid-nintendo implementation, CTCaer joycon toolkit and dekuNukem reverse
//! engineering.
//!
//! Rumble/vibration protocol implementation.

use common::input::DriverResult;

use super::common_protocol::JoyconCommonProtocol;
use super::joycon_types::VibrationValue;

/// Port of `RumbleProtocol` class from rumble.h / rumble.cpp
pub struct RumbleProtocol {
    protocol: JoyconCommonProtocol,
}

impl RumbleProtocol {
    /// Port of RumbleProtocol::RumbleProtocol
    pub fn new() -> Self {
        Self {
            protocol: JoyconCommonProtocol::new(),
        }
    }

    /// Port of RumbleProtocol::EnableRumble
    pub fn enable_rumble(&mut self, _is_enabled: bool) -> DriverResult {
        todo!()
    }

    /// Port of RumbleProtocol::SendVibration
    pub fn send_vibration(&mut self, _vibration: &VibrationValue) -> DriverResult {
        todo!()
    }

    // ---- Private methods ----

    fn encode_high_frequency(&self, _frequency: f32) -> u16 {
        todo!()
    }

    fn encode_low_frequency(&self, _frequency: f32) -> u8 {
        todo!()
    }

    fn encode_high_amplitude(&self, _amplitude: f32) -> u8 {
        todo!()
    }

    fn encode_low_amplitude(&self, _amplitude: f32) -> u16 {
        todo!()
    }
}
