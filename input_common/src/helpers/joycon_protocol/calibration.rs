// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/calibration.h` and `calibration.cpp`.
//!
//! Based on dkms-hid-nintendo implementation, CTCaer joycon toolkit and dekuNukem reverse
//! engineering.
//!
//! Driver functions related to retrieving calibration data from the device.

use common::input::DriverResult;

use super::common_protocol::{JoyconCommonProtocol, ScopedSetBlocking};
use super::joycon_types::*;

/// Default stick center value.
const DEFAULT_STICK_CENTER: u16 = 0x800;
/// Default stick range value.
const DEFAULT_STICK_RANGE: u16 = 0x6cc;
/// Default accelerometer scale value.
const DEFAULT_ACCELEROMETER_SCALE: i16 = 0x4000;
/// Default gyroscope scale value.
const DEFAULT_GYRO_SCALE: i16 = 0x3be7;
/// Default sensor offset value.
const DEFAULT_OFFSET: i16 = 0;
/// Default ring controller range.
const DEFAULT_RING_RANGE: i16 = 800;

/// Output from SPI read command containing user calibration magic.
/// Port of MagicSpiCalibration from joycon_types.h
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct MagicSpiCalibration {
    pub first: u8,
    pub second: u8,
}
const _: () = assert!(std::mem::size_of::<MagicSpiCalibration>() == 0x2);

/// Output from SPI read command containing left joystick calibration.
/// Port of JoystickLeftSpiCalibration from joycon_types.h
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct JoystickLeftSpiCalibration {
    pub max: [u8; 3],
    pub center: [u8; 3],
    pub min: [u8; 3],
}
const _: () = assert!(std::mem::size_of::<JoystickLeftSpiCalibration>() == 0x9);

/// Output from SPI read command containing right joystick calibration.
/// Port of JoystickRightSpiCalibration from joycon_types.h
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct JoystickRightSpiCalibration {
    pub center: [u8; 3],
    pub min: [u8; 3],
    pub max: [u8; 3],
}
const _: () = assert!(std::mem::size_of::<JoystickRightSpiCalibration>() == 0x9);

/// Output from SPI read command containing IMU calibration.
/// Port of ImuSpiCalibration from joycon_types.h
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ImuSpiCalibration {
    pub accelerometer_offset: [i16; 3],
    pub accelerometer_scale: [i16; 3],
    pub gyroscope_offset: [i16; 3],
    pub gyroscope_scale: [i16; 3],
}
const _: () = assert!(std::mem::size_of::<ImuSpiCalibration>() == 0x18);

/// Port of `CalibrationProtocol` class from calibration.h / calibration.cpp
pub struct CalibrationProtocol {
    protocol: JoyconCommonProtocol,
    ring_data_max: i16,
    ring_data_default: i16,
    ring_data_min: i16,
}

impl CalibrationProtocol {
    /// Port of CalibrationProtocol::CalibrationProtocol
    pub fn new() -> Self {
        Self {
            protocol: JoyconCommonProtocol::new(),
            ring_data_max: 0,
            ring_data_default: 0,
            ring_data_min: 0,
        }
    }

    /// Sends a request to obtain the left stick calibration from memory.
    /// Port of CalibrationProtocol::GetLeftJoyStickCalibration
    pub fn get_left_joy_stick_calibration(
        &mut self,
        _calibration: &mut JoyStickCalibration,
    ) -> DriverResult {
        // Requires ReadSPI which depends on hidapi handle wiring.
        // The full flow is:
        // 1. ScopedSetBlocking
        // 2. HasUserCalibration(USER_LEFT_MAGIC)
        // 3. ReadSPI(USER_LEFT_DATA or FACT_LEFT_DATA, spi_calibration)
        // 4. Extract x/y axis from 3-byte blocks using GetXAxisCalibrationValue/GetYAxisCalibrationValue
        // 5. ValidateCalibration
        todo!("Requires ReadSPI via hidapi handle")
    }

    /// Sends a request to obtain the right stick calibration from memory.
    /// Port of CalibrationProtocol::GetRightJoyStickCalibration
    pub fn get_right_joy_stick_calibration(
        &mut self,
        _calibration: &mut JoyStickCalibration,
    ) -> DriverResult {
        // Same flow as left, but with USER_RIGHT_MAGIC/USER_RIGHT_DATA/FACT_RIGHT_DATA
        todo!("Requires ReadSPI via hidapi handle")
    }

    /// Sends a request to obtain the motion calibration from memory.
    /// Port of CalibrationProtocol::GetImuCalibration
    pub fn get_imu_calibration(
        &mut self,
        _calibration: &mut MotionCalibration,
    ) -> DriverResult {
        // Reads SPI for USER_IMU_MAGIC, then USER_IMU_DATA or FACT_IMU_DATA
        todo!("Requires ReadSPI via hidapi handle")
    }

    /// Calculates on run time the proper calibration of the ring controller.
    /// Port of CalibrationProtocol::GetRingCalibration
    pub fn get_ring_calibration(
        &mut self,
        calibration: &mut RingCalibration,
        current_value: i16,
    ) -> DriverResult {
        // Initialize with default range if not set
        if self.ring_data_max == 0 && self.ring_data_min == 0 {
            self.ring_data_max = current_value + DEFAULT_RING_RANGE;
            self.ring_data_min = current_value - DEFAULT_RING_RANGE;
            self.ring_data_default = current_value;
        }
        self.ring_data_max = self.ring_data_max.max(current_value);
        self.ring_data_min = self.ring_data_min.min(current_value);
        *calibration = RingCalibration {
            default_value: self.ring_data_default,
            max_value: self.ring_data_max,
            min_value: self.ring_data_min,
        };
        DriverResult::Success
    }

    // ---- Private methods ----

    /// Returns true if the specified address corresponds to the magic value of user calibration.
    /// Port of CalibrationProtocol::HasUserCalibration
    fn has_user_calibration(
        &mut self,
        _address: SpiAddress,
        _has_user_calibration: &mut bool,
    ) -> DriverResult {
        // Requires ReadSPI to read MagicSpiCalibration and check against USR_MAGIC_0/USR_MAGIC_1
        todo!("Requires ReadSPI via hidapi handle")
    }

    /// Converts a raw calibration block to an u16 value containing the x axis value.
    /// Port of CalibrationProtocol::GetXAxisCalibrationValue
    fn get_x_axis_calibration_value(&self, block: &[u8]) -> u16 {
        (((block[1] & 0x0F) as u16) << 8) | (block[0] as u16)
    }

    /// Converts a raw calibration block to an u16 value containing the y axis value.
    /// Port of CalibrationProtocol::GetYAxisCalibrationValue
    fn get_y_axis_calibration_value(&self, block: &[u8]) -> u16 {
        ((block[2] as u16) << 4) | ((block[1] >> 4) as u16)
    }

    /// Ensures that all joystick calibration values are set.
    /// Port of CalibrationProtocol::ValidateCalibration (JoyStickCalibration overload)
    fn validate_stick_calibration(&self, calibration: &mut JoyStickCalibration) {
        calibration.x.center = self.validate_u16_value(calibration.x.center, DEFAULT_STICK_CENTER);
        calibration.x.max = self.validate_u16_value(calibration.x.max, DEFAULT_STICK_RANGE);
        calibration.x.min = self.validate_u16_value(calibration.x.min, DEFAULT_STICK_RANGE);

        calibration.y.center = self.validate_u16_value(calibration.y.center, DEFAULT_STICK_CENTER);
        calibration.y.max = self.validate_u16_value(calibration.y.max, DEFAULT_STICK_RANGE);
        calibration.y.min = self.validate_u16_value(calibration.y.min, DEFAULT_STICK_RANGE);
    }

    /// Ensures that all motion calibration values are set.
    /// Port of CalibrationProtocol::ValidateCalibration (MotionCalibration overload)
    fn validate_motion_calibration(&self, calibration: &mut MotionCalibration) {
        for sensor in calibration.accelerometer.iter_mut() {
            sensor.scale = self.validate_i16_value(sensor.scale, DEFAULT_ACCELEROMETER_SCALE);
            sensor.offset = self.validate_i16_value(sensor.offset, DEFAULT_OFFSET);
        }
        for sensor in calibration.gyro.iter_mut() {
            sensor.scale = self.validate_i16_value(sensor.scale, DEFAULT_GYRO_SCALE);
            sensor.offset = self.validate_i16_value(sensor.offset, DEFAULT_OFFSET);
        }
    }

    /// Returns the default value if the value is either zero or 0xFFF.
    /// Port of CalibrationProtocol::ValidateValue (u16 overload)
    fn validate_u16_value(&self, value: u16, default_value: u16) -> u16 {
        if value == 0 || value == 0xFFF {
            return default_value;
        }
        value
    }

    /// Returns the default value if the value is either zero or 0xFFF.
    /// Port of CalibrationProtocol::ValidateValue (s16 overload)
    fn validate_i16_value(&self, value: i16, default_value: i16) -> i16 {
        if value == 0 || value == 0xFFF {
            return default_value;
        }
        value
    }
}
