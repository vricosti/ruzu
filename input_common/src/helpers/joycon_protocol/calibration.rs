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
use super::joycon_types::{CalibrationMagic, JoyStickCalibration, MotionCalibration, RingCalibration, SpiAddress};

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
        calibration: &mut JoyStickCalibration,
    ) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;
        let mut spi_calibration = JoystickLeftSpiCalibration::default();
        let mut has_user_cal = false;
        *calibration = JoyStickCalibration::default();

        if result == DriverResult::Success {
            result = self.has_user_calibration(SpiAddress::UserLeftMagic, &mut has_user_cal);
        }

        // Read user defined calibration
        if result == DriverResult::Success && has_user_cal {
            let buf = unsafe {
                std::slice::from_raw_parts_mut(
                    &mut spi_calibration as *mut _ as *mut u8,
                    std::mem::size_of::<JoystickLeftSpiCalibration>(),
                )
            };
            result = self.protocol.read_raw_spi(SpiAddress::UserLeftData, buf);
        }

        // Read factory calibration
        if result == DriverResult::Success && !has_user_cal {
            let buf = unsafe {
                std::slice::from_raw_parts_mut(
                    &mut spi_calibration as *mut _ as *mut u8,
                    std::mem::size_of::<JoystickLeftSpiCalibration>(),
                )
            };
            result = self.protocol.read_raw_spi(SpiAddress::FactLeftData, buf);
        }

        if result == DriverResult::Success {
            calibration.x.center = self.get_x_axis_calibration_value(&spi_calibration.center);
            calibration.y.center = self.get_y_axis_calibration_value(&spi_calibration.center);
            calibration.x.min = self.get_x_axis_calibration_value(&spi_calibration.min);
            calibration.y.min = self.get_y_axis_calibration_value(&spi_calibration.min);
            calibration.x.max = self.get_x_axis_calibration_value(&spi_calibration.max);
            calibration.y.max = self.get_y_axis_calibration_value(&spi_calibration.max);
        }

        // Set a valid default calibration if data is missing
        self.validate_stick_calibration(calibration);

        result
    }

    /// Sends a request to obtain the right stick calibration from memory.
    /// Port of CalibrationProtocol::GetRightJoyStickCalibration
    pub fn get_right_joy_stick_calibration(
        &mut self,
        calibration: &mut JoyStickCalibration,
    ) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;
        let mut spi_calibration = JoystickRightSpiCalibration::default();
        let mut has_user_cal = false;
        *calibration = JoyStickCalibration::default();

        if result == DriverResult::Success {
            result = self.has_user_calibration(SpiAddress::UserRightMagic, &mut has_user_cal);
        }

        // Read user defined calibration
        if result == DriverResult::Success && has_user_cal {
            let buf = unsafe {
                std::slice::from_raw_parts_mut(
                    &mut spi_calibration as *mut _ as *mut u8,
                    std::mem::size_of::<JoystickRightSpiCalibration>(),
                )
            };
            result = self.protocol.read_raw_spi(SpiAddress::UserRightData, buf);
        }

        // Read factory calibration
        if result == DriverResult::Success && !has_user_cal {
            let buf = unsafe {
                std::slice::from_raw_parts_mut(
                    &mut spi_calibration as *mut _ as *mut u8,
                    std::mem::size_of::<JoystickRightSpiCalibration>(),
                )
            };
            result = self.protocol.read_raw_spi(SpiAddress::FactRightData, buf);
        }

        if result == DriverResult::Success {
            calibration.x.center = self.get_x_axis_calibration_value(&spi_calibration.center);
            calibration.y.center = self.get_y_axis_calibration_value(&spi_calibration.center);
            calibration.x.min = self.get_x_axis_calibration_value(&spi_calibration.min);
            calibration.y.min = self.get_y_axis_calibration_value(&spi_calibration.min);
            calibration.x.max = self.get_x_axis_calibration_value(&spi_calibration.max);
            calibration.y.max = self.get_y_axis_calibration_value(&spi_calibration.max);
        }

        // Set a valid default calibration if data is missing
        self.validate_stick_calibration(calibration);

        result
    }

    /// Sends a request to obtain the motion calibration from memory.
    /// Port of CalibrationProtocol::GetImuCalibration
    pub fn get_imu_calibration(
        &mut self,
        calibration: &mut MotionCalibration,
    ) -> DriverResult {
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;
        let mut spi_calibration = ImuSpiCalibration::default();
        let mut has_user_cal = false;
        *calibration = MotionCalibration::default();

        if result == DriverResult::Success {
            result = self.has_user_calibration(SpiAddress::UserImuMagic, &mut has_user_cal);
        }

        // Read user defined calibration
        if result == DriverResult::Success && has_user_cal {
            let buf = unsafe {
                std::slice::from_raw_parts_mut(
                    &mut spi_calibration as *mut _ as *mut u8,
                    std::mem::size_of::<ImuSpiCalibration>(),
                )
            };
            result = self.protocol.read_raw_spi(SpiAddress::UserImuData, buf);
        }

        // Read factory calibration
        if result == DriverResult::Success && !has_user_cal {
            let buf = unsafe {
                std::slice::from_raw_parts_mut(
                    &mut spi_calibration as *mut _ as *mut u8,
                    std::mem::size_of::<ImuSpiCalibration>(),
                )
            };
            result = self.protocol.read_raw_spi(SpiAddress::FactImuData, buf);
        }

        if result == DriverResult::Success {
            calibration.accelerometer[0].offset = spi_calibration.accelerometer_offset[0];
            calibration.accelerometer[1].offset = spi_calibration.accelerometer_offset[1];
            calibration.accelerometer[2].offset = spi_calibration.accelerometer_offset[2];

            calibration.accelerometer[0].scale = spi_calibration.accelerometer_scale[0];
            calibration.accelerometer[1].scale = spi_calibration.accelerometer_scale[1];
            calibration.accelerometer[2].scale = spi_calibration.accelerometer_scale[2];

            calibration.gyro[0].offset = spi_calibration.gyroscope_offset[0];
            calibration.gyro[1].offset = spi_calibration.gyroscope_offset[1];
            calibration.gyro[2].offset = spi_calibration.gyroscope_offset[2];

            calibration.gyro[0].scale = spi_calibration.gyroscope_scale[0];
            calibration.gyro[1].scale = spi_calibration.gyroscope_scale[1];
            calibration.gyro[2].scale = spi_calibration.gyroscope_scale[2];
        }

        self.validate_motion_calibration(calibration);

        result
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
        address: SpiAddress,
        has_user_calibration: &mut bool,
    ) -> DriverResult {
        let mut spi_magic = MagicSpiCalibration::default();
        let buf = unsafe {
            std::slice::from_raw_parts_mut(
                &mut spi_magic as *mut _ as *mut u8,
                std::mem::size_of::<MagicSpiCalibration>(),
            )
        };
        let result = self.protocol.read_raw_spi(address, buf);
        *has_user_calibration = false;
        if result == DriverResult::Success {
            *has_user_calibration = spi_magic.first == CalibrationMagic::UsrMagic0 as u8
                && spi_magic.second == CalibrationMagic::UsrMagic1 as u8;
        }
        result
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
