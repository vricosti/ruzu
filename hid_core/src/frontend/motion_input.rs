// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/frontend/motion_input.h and motion_input.cpp

use crate::hid_types::Vec3f;

pub const THRESHOLD_LOOSE: f32 = 0.01;
pub const THRESHOLD_STANDARD: f32 = 0.007;
pub const THRESHOLD_TIGHT: f32 = 0.002;

pub const IS_AT_REST_RELAXED: f32 = 0.05;
pub const IS_AT_REST_LOOSE: f32 = 0.02;
pub const IS_AT_REST_STANDARD: f32 = 0.01;
pub const IS_AT_REST_TIGHT: f32 = 0.005;

pub const GYRO_MAX_VALUE: f32 = 5.0;
pub const ACCEL_MAX_VALUE: f32 = 7.0;
pub const CALIBRATION_SAMPLES: usize = 300;

/// Quaternion representation
#[derive(Debug, Clone, Copy)]
pub struct Quaternion {
    pub w: f32,
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl Default for Quaternion {
    fn default() -> Self {
        Self {
            w: 1.0,
            x: 0.0,
            y: 0.0,
            z: 0.0,
        }
    }
}

pub struct MotionInput {
    // PID constants
    kp: f32,
    ki: f32,
    kd: f32,

    // PID errors
    real_error: Vec3f,
    integral_error: Vec3f,
    derivative_error: Vec3f,

    // Quaternion containing the device orientation
    quat: Quaternion,

    // Number of full rotations in each axis
    rotations: Vec3f,

    // Acceleration vector measurement in G force
    accel: Vec3f,

    // Gyroscope vector measurement in radians/s
    gyro: Vec3f,

    // Vector to be subtracted from gyro measurements
    gyro_bias: Vec3f,

    // Minimum gyro amplitude to detect if the device is moving
    gyro_threshold: f32,

    // Multiplies gyro_threshold by this value
    user_gyro_threshold: f32,

    // Number of invalid sequential data
    reset_counter: u32,

    // If the provided data is invalid the device will be autocalibrated
    reset_enabled: bool,

    // Use accelerometer values to calculate position
    only_accelerometer: bool,

    // When enabled it will aggressively adjust for gyro drift
    calibration_mode: bool,

    // Used to auto disable calibration mode
    calibration_counter: usize,
}

impl Default for MotionInput {
    fn default() -> Self {
        Self::new()
    }
}

impl MotionInput {
    pub fn new() -> Self {
        Self {
            kp: 0.0,
            ki: 0.0,
            kd: 0.0,
            real_error: Vec3f::default(),
            integral_error: Vec3f::default(),
            derivative_error: Vec3f::default(),
            quat: Quaternion::default(),
            rotations: Vec3f::default(),
            accel: Vec3f::default(),
            gyro: Vec3f::default(),
            gyro_bias: Vec3f::default(),
            gyro_threshold: 0.0,
            user_gyro_threshold: 0.0,
            reset_counter: 0,
            reset_enabled: true,
            only_accelerometer: true,
            calibration_mode: false,
            calibration_counter: 0,
        }
    }

    pub fn set_pid(&mut self, new_kp: f32, new_ki: f32, new_kd: f32) {
        self.kp = new_kp;
        self.ki = new_ki;
        self.kd = new_kd;
    }

    pub fn set_acceleration(&mut self, acceleration: Vec3f) {
        self.accel = acceleration;
    }

    pub fn set_gyroscope(&mut self, gyroscope: Vec3f) {
        self.gyro = gyroscope;
    }

    pub fn set_quaternion(&mut self, quaternion: Quaternion) {
        self.quat = quaternion;
    }

    pub fn set_euler_angles(&mut self, _euler_angles: Vec3f) {
        todo!()
    }

    pub fn set_gyro_bias(&mut self, bias: Vec3f) {
        self.gyro_bias = bias;
    }

    pub fn set_gyro_threshold(&mut self, threshold: f32) {
        self.gyro_threshold = threshold;
    }

    pub fn set_user_gyro_threshold(&mut self, threshold: f32) {
        self.user_gyro_threshold = threshold;
    }

    pub fn enable_reset(&mut self, reset: bool) {
        self.reset_enabled = reset;
    }

    pub fn reset_rotations(&mut self) {
        self.rotations = Vec3f::default();
    }

    pub fn reset_quaternion(&mut self) {
        self.quat = Quaternion::default();
    }

    pub fn update_rotation(&mut self, _elapsed_time: u64) {
        todo!()
    }

    pub fn update_orientation(&mut self, _elapsed_time: u64) {
        todo!()
    }

    pub fn calibrate(&mut self) {
        todo!()
    }

    pub fn get_orientation(&self) -> [Vec3f; 3] {
        todo!()
    }

    pub fn get_acceleration(&self) -> Vec3f {
        self.accel
    }

    pub fn get_gyroscope(&self) -> Vec3f {
        self.gyro
    }

    pub fn get_gyro_bias(&self) -> Vec3f {
        self.gyro_bias
    }

    pub fn get_rotations(&self) -> Vec3f {
        self.rotations
    }

    pub fn get_quaternion(&self) -> Quaternion {
        self.quat
    }

    pub fn get_euler_angles(&self) -> Vec3f {
        todo!()
    }

    pub fn is_moving(&self, sensitivity: f32) -> bool {
        let dominated = self.gyro.x * self.gyro.x
            + self.gyro.y * self.gyro.y
            + self.gyro.z * self.gyro.z;
        dominated >= sensitivity * sensitivity
    }

    pub fn is_calibrated(&self, sensitivity: f32) -> bool {
        !self.is_moving(sensitivity)
    }
}
