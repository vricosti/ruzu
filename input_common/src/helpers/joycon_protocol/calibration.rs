// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/calibration.h` and `calibration.cpp`.
//!
//! Based on dkms-hid-nintendo implementation, CTCaer joycon toolkit and dekuNukem reverse
//! engineering.
//!
//! Driver functions related to retrieving calibration data from the device.

use common::input::DriverResult;

use super::common_protocol::JoyconCommonProtocol;
use super::joycon_types::*;

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
        todo!()
    }

    /// Sends a request to obtain the right stick calibration from memory.
    /// Port of CalibrationProtocol::GetRightJoyStickCalibration
    pub fn get_right_joy_stick_calibration(
        &mut self,
        _calibration: &mut JoyStickCalibration,
    ) -> DriverResult {
        todo!()
    }

    /// Sends a request to obtain the motion calibration from memory.
    /// Port of CalibrationProtocol::GetImuCalibration
    pub fn get_imu_calibration(
        &mut self,
        _calibration: &mut MotionCalibration,
    ) -> DriverResult {
        todo!()
    }

    /// Calculates on run time the proper calibration of the ring controller.
    /// Port of CalibrationProtocol::GetRingCalibration
    pub fn get_ring_calibration(
        &mut self,
        _calibration: &mut RingCalibration,
        _current_value: i16,
    ) -> DriverResult {
        todo!()
    }

    // ---- Private methods ----

    fn has_user_calibration(
        &mut self,
        _address: SpiAddress,
        _has_user_calibration: &mut bool,
    ) -> DriverResult {
        todo!()
    }

    fn get_x_axis_calibration_value(&self, _block: &[u8]) -> u16 {
        todo!()
    }

    fn get_y_axis_calibration_value(&self, _block: &[u8]) -> u16 {
        todo!()
    }

    fn validate_stick_calibration(&self, _calibration: &mut JoyStickCalibration) {
        todo!()
    }

    fn validate_motion_calibration(&self, _calibration: &mut MotionCalibration) {
        todo!()
    }

    fn validate_u16_value(&self, _value: u16, _default_value: u16) -> u16 {
        todo!()
    }

    fn validate_i16_value(&self, _value: i16, _default_value: i16) -> i16 {
        todo!()
    }
}
