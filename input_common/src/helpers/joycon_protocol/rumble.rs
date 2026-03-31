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
use super::joycon_types::{SubCommand, VibrationValue, DEFAULT_VIBRATION_BUFFER};

/// More information about these values can be found here:
/// https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering/blob/master/rumble_data_table.md
static HIGH_FREQUENCY_AMPLITUDE: [(f32, u8); 101] = [
    (0.0, 0x0),
    (0.01, 0x2),
    (0.012, 0x4),
    (0.014, 0x6),
    (0.017, 0x8),
    (0.02, 0x0a),
    (0.024, 0x0c),
    (0.028, 0x0e),
    (0.033, 0x10),
    (0.04, 0x12),
    (0.047, 0x14),
    (0.056, 0x16),
    (0.067, 0x18),
    (0.08, 0x1a),
    (0.095, 0x1c),
    (0.112, 0x1e),
    (0.117, 0x20),
    (0.123, 0x22),
    (0.128, 0x24),
    (0.134, 0x26),
    (0.14, 0x28),
    (0.146, 0x2a),
    (0.152, 0x2c),
    (0.159, 0x2e),
    (0.166, 0x30),
    (0.173, 0x32),
    (0.181, 0x34),
    (0.189, 0x36),
    (0.198, 0x38),
    (0.206, 0x3a),
    (0.215, 0x3c),
    (0.225, 0x3e),
    (0.23, 0x40),
    (0.235, 0x42),
    (0.24, 0x44),
    (0.245, 0x46),
    (0.251, 0x48),
    (0.256, 0x4a),
    (0.262, 0x4c),
    (0.268, 0x4e),
    (0.273, 0x50),
    (0.279, 0x52),
    (0.286, 0x54),
    (0.292, 0x56),
    (0.298, 0x58),
    (0.305, 0x5a),
    (0.311, 0x5c),
    (0.318, 0x5e),
    (0.325, 0x60),
    (0.332, 0x62),
    (0.34, 0x64),
    (0.347, 0x66),
    (0.355, 0x68),
    (0.362, 0x6a),
    (0.37, 0x6c),
    (0.378, 0x6e),
    (0.387, 0x70),
    (0.395, 0x72),
    (0.404, 0x74),
    (0.413, 0x76),
    (0.422, 0x78),
    (0.431, 0x7a),
    (0.44, 0x7c),
    (0.45, 0x7e),
    (0.46, 0x80),
    (0.47, 0x82),
    (0.48, 0x84),
    (0.491, 0x86),
    (0.501, 0x88),
    (0.512, 0x8a),
    (0.524, 0x8c),
    (0.535, 0x8e),
    (0.547, 0x90),
    (0.559, 0x92),
    (0.571, 0x94),
    (0.584, 0x96),
    (0.596, 0x98),
    (0.609, 0x9a),
    (0.623, 0x9c),
    (0.636, 0x9e),
    (0.65, 0xa0),
    (0.665, 0xa2),
    (0.679, 0xa4),
    (0.694, 0xa6),
    (0.709, 0xa8),
    (0.725, 0xaa),
    (0.741, 0xac),
    (0.757, 0xae),
    (0.773, 0xb0),
    (0.79, 0xb2),
    (0.808, 0xb4),
    (0.825, 0xb6),
    (0.843, 0xb8),
    (0.862, 0xba),
    (0.881, 0xbc),
    (0.9, 0xbe),
    (0.92, 0xc0),
    (0.94, 0xc2),
    (0.96, 0xc4),
    (0.981, 0xc6),
    (1.003, 0xc8),
];

/// More information about these values can be found here:
/// https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering/blob/master/rumble_data_table.md
static LOW_FREQUENCY_AMPLITUDE: [(f32, u16); 101] = [
    (0.0, 0x0040),
    (0.01, 0x8040),
    (0.012, 0x0041),
    (0.014, 0x8041),
    (0.017, 0x0042),
    (0.02, 0x8042),
    (0.024, 0x0043),
    (0.028, 0x8043),
    (0.033, 0x0044),
    (0.04, 0x8044),
    (0.047, 0x0045),
    (0.056, 0x8045),
    (0.067, 0x0046),
    (0.08, 0x8046),
    (0.095, 0x0047),
    (0.112, 0x8047),
    (0.117, 0x0048),
    (0.123, 0x8048),
    (0.128, 0x0049),
    (0.134, 0x8049),
    (0.14, 0x004a),
    (0.146, 0x804a),
    (0.152, 0x004b),
    (0.159, 0x804b),
    (0.166, 0x004c),
    (0.173, 0x804c),
    (0.181, 0x004d),
    (0.189, 0x804d),
    (0.198, 0x004e),
    (0.206, 0x804e),
    (0.215, 0x004f),
    (0.225, 0x804f),
    (0.23, 0x0050),
    (0.235, 0x8050),
    (0.24, 0x0051),
    (0.245, 0x8051),
    (0.251, 0x0052),
    (0.256, 0x8052),
    (0.262, 0x0053),
    (0.268, 0x8053),
    (0.273, 0x0054),
    (0.279, 0x8054),
    (0.286, 0x0055),
    (0.292, 0x8055),
    (0.298, 0x0056),
    (0.305, 0x8056),
    (0.311, 0x0057),
    (0.318, 0x8057),
    (0.325, 0x0058),
    (0.332, 0x8058),
    (0.34, 0x0059),
    (0.347, 0x8059),
    (0.355, 0x005a),
    (0.362, 0x805a),
    (0.37, 0x005b),
    (0.378, 0x805b),
    (0.387, 0x005c),
    (0.395, 0x805c),
    (0.404, 0x005d),
    (0.413, 0x805d),
    (0.422, 0x005e),
    (0.431, 0x805e),
    (0.44, 0x005f),
    (0.45, 0x805f),
    (0.46, 0x0060),
    (0.47, 0x8060),
    (0.48, 0x0061),
    (0.491, 0x8061),
    (0.501, 0x0062),
    (0.512, 0x8062),
    (0.524, 0x0063),
    (0.535, 0x8063),
    (0.547, 0x0064),
    (0.559, 0x8064),
    (0.571, 0x0065),
    (0.584, 0x8065),
    (0.596, 0x0066),
    (0.609, 0x8066),
    (0.623, 0x0067),
    (0.636, 0x8067),
    (0.65, 0x0068),
    (0.665, 0x8068),
    (0.679, 0x0069),
    (0.694, 0x8069),
    (0.709, 0x006a),
    (0.725, 0x806a),
    (0.741, 0x006b),
    (0.757, 0x806b),
    (0.773, 0x006c),
    (0.79, 0x806c),
    (0.808, 0x006d),
    (0.825, 0x806d),
    (0.843, 0x006e),
    (0.862, 0x806e),
    (0.881, 0x006f),
    (0.9, 0x806f),
    (0.92, 0x0070),
    (0.94, 0x8070),
    (0.96, 0x0071),
    (0.981, 0x8071),
    (1.003, 0x0072),
];

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
    pub fn enable_rumble(&mut self, is_enabled: bool) -> DriverResult {
        let _sb = super::common_protocol::ScopedSetBlocking::new(&mut self.protocol);
        let buffer = [if is_enabled { 1u8 } else { 0u8 }];
        self.protocol
            .send_sub_command(SubCommand::EnableVibration, &buffer)
    }

    /// Port of RumbleProtocol::SendVibration
    pub fn send_vibration(&mut self, vibration: &VibrationValue) -> DriverResult {
        if vibration.high_amplitude <= 0.0 && vibration.low_amplitude <= 0.0 {
            return self
                .protocol
                .send_vibration_report(&DEFAULT_VIBRATION_BUFFER);
        }

        // Protect joycons from damage from strong vibrations
        let clamp_amplitude =
            1.0f32 / f32::max(1.0, vibration.high_amplitude + vibration.low_amplitude);

        let encoded_high_frequency = self.encode_high_frequency(vibration.high_frequency);
        let encoded_high_amplitude =
            self.encode_high_amplitude(vibration.high_amplitude * clamp_amplitude);
        let encoded_low_frequency = self.encode_low_frequency(vibration.low_frequency);
        let encoded_low_amplitude =
            self.encode_low_amplitude(vibration.low_amplitude * clamp_amplitude);

        let mut buffer = [0u8; 8];
        buffer[0] = (encoded_high_frequency & 0xFF) as u8;
        buffer[1] = encoded_high_amplitude | ((encoded_high_frequency >> 8) & 0x01) as u8;
        buffer[2] = encoded_low_frequency | ((encoded_low_amplitude >> 8) & 0x80) as u8;
        buffer[3] = (encoded_low_amplitude & 0xFF) as u8;

        // Duplicate rumble for now
        buffer[4] = buffer[0];
        buffer[5] = buffer[1];
        buffer[6] = buffer[2];
        buffer[7] = buffer[3];

        self.protocol.send_vibration_report(&buffer)
    }

    // ---- Private methods ----

    /// Port of RumbleProtocol::EncodeHighFrequency
    fn encode_high_frequency(&self, frequency: f32) -> u16 {
        let new_frequency = ((frequency / 10.0f32).log2() * 32.0).clamp(0.0, 255.0) as u8;
        ((new_frequency.wrapping_sub(0x60)) as u16) * 4
    }

    /// Port of RumbleProtocol::EncodeLowFrequency
    fn encode_low_frequency(&self, frequency: f32) -> u8 {
        let new_frequency = ((frequency / 10.0f32).log2() * 32.0).clamp(0.0, 255.0) as u8;
        new_frequency.wrapping_sub(0x40)
    }

    /// Port of RumbleProtocol::EncodeHighAmplitude
    fn encode_high_amplitude(&self, amplitude: f32) -> u8 {
        for &(amplitude_value, code) in HIGH_FREQUENCY_AMPLITUDE.iter() {
            if amplitude <= amplitude_value {
                return code;
            }
        }
        HIGH_FREQUENCY_AMPLITUDE[HIGH_FREQUENCY_AMPLITUDE.len() - 1].1
    }

    /// Port of RumbleProtocol::EncodeLowAmplitude
    fn encode_low_amplitude(&self, amplitude: f32) -> u16 {
        for &(amplitude_value, code) in LOW_FREQUENCY_AMPLITUDE.iter() {
            if amplitude <= amplitude_value {
                return code;
            }
        }
        LOW_FREQUENCY_AMPLITUDE[LOW_FREQUENCY_AMPLITUDE.len() - 1].1
    }
}
