// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/poller.h` and `poller.cpp`.
//!
//! Based on dkms-hid-nintendo implementation, CTCaer joycon toolkit and dekuNukem reverse
//! engineering.
//!
//! Handles input packages and triggers the corresponding input events.

use super::joycon_types::*;

/// Port of `JoyconPoller` class from poller.h / poller.cpp
pub struct JoyconPoller {
    device_type: ControllerType,
    left_stick_calibration: JoyStickCalibration,
    right_stick_calibration: JoyStickCalibration,
    motion_calibration: MotionCalibration,
    callbacks: Option<JoyconCallbacks>,
}

impl JoyconPoller {
    /// Port of JoyconPoller::JoyconPoller
    pub fn new(
        device_type: ControllerType,
        left_stick_calibration: JoyStickCalibration,
        right_stick_calibration: JoyStickCalibration,
        motion_calibration: MotionCalibration,
    ) -> Self {
        Self {
            device_type,
            left_stick_calibration,
            right_stick_calibration,
            motion_calibration,
            callbacks: None,
        }
    }

    /// Port of JoyconPoller::SetCallbacks
    pub fn set_callbacks(&mut self, callbacks: JoyconCallbacks) {
        self.callbacks = Some(callbacks);
    }

    /// Handles data from passive packages.
    /// Port of JoyconPoller::ReadPassiveMode
    pub fn read_passive_mode(&mut self, _buffer: &mut [u8]) {
        todo!()
    }

    /// Handles data from active packages.
    /// Port of JoyconPoller::ReadActiveMode
    pub fn read_active_mode(
        &mut self,
        _buffer: &mut [u8],
        _motion_status: &MotionStatus,
        _ring_status: &RingStatus,
    ) {
        todo!()
    }

    /// Handles data from nfc or ir packages.
    /// Port of JoyconPoller::ReadNfcIRMode
    pub fn read_nfc_ir_mode(&mut self, _buffer: &mut [u8], _motion_status: &MotionStatus) {
        todo!()
    }

    /// Port of JoyconPoller::UpdateColor
    pub fn update_color(&mut self, _color: &Color) {
        todo!()
    }

    /// Port of JoyconPoller::UpdateRing
    pub fn update_ring(&mut self, _value: i16, _ring_status: &RingStatus) {
        todo!()
    }

    /// Port of JoyconPoller::UpdateAmiibo
    pub fn update_amiibo(&mut self, _tag_info: &TagInfo) {
        todo!()
    }

    /// Port of JoyconPoller::UpdateCamera
    pub fn update_camera(&mut self, _camera_data: &[u8], _format: IrsResolution) {
        todo!()
    }

    // ---- Private methods ----

    fn get_axis_value(&self, _raw_value: u16, _calibration: &JoyStickAxisCalibration) -> f32 {
        todo!()
    }

    fn get_passive_axis_value(&self, _raw_value: PassivePadStick) -> (f32, f32) {
        todo!()
    }

    fn get_accelerometer_value(
        &self,
        _raw: i16,
        _cal: &MotionSensorCalibration,
        _sensitivity: AccelerometerSensitivity,
    ) -> f32 {
        todo!()
    }

    fn get_gyro_value(
        &self,
        _raw_value: i16,
        _cal: &MotionSensorCalibration,
        _sensitivity: GyroSensitivity,
    ) -> f32 {
        todo!()
    }

    fn get_raw_imu_values(&self, _sensor: usize, _axis: usize) -> i16 {
        todo!()
    }

    fn get_motion_input(&self, _motion_status: &MotionStatus) -> MotionData {
        todo!()
    }
}
