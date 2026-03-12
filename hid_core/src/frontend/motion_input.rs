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

const PI: f32 = std::f32::consts::PI;

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

impl Quaternion {
    /// Returns the normalized quaternion.
    pub fn normalized(&self) -> Self {
        let len = (self.w * self.w + self.x * self.x + self.y * self.y + self.z * self.z).sqrt();
        if len == 0.0 {
            return *self;
        }
        Self {
            w: self.w / len,
            x: self.x / len,
            y: self.y / len,
            z: self.z / len,
        }
    }

    /// Converts quaternion to a 4x4 matrix (row-major, 16 floats).
    /// Matches Common::Quaternion<f32>::ToMatrix() from upstream.
    pub fn to_matrix(&self) -> [f32; 16] {
        let w = self.w;
        let x = self.x;
        let y = self.y;
        let z = self.z;
        [
            1.0 - 2.0 * (y * y + z * z),
            2.0 * (x * y + w * z),
            2.0 * (x * z - w * y),
            0.0,
            2.0 * (x * y - w * z),
            1.0 - 2.0 * (x * x + z * z),
            2.0 * (y * z + w * x),
            0.0,
            2.0 * (x * z + w * y),
            2.0 * (y * z - w * x),
            1.0 - 2.0 * (x * x + y * y),
            0.0,
            0.0,
            0.0,
            0.0,
            1.0,
        ]
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
        let mut input = Self {
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
        };
        // Initialize PID constants with default values
        input.set_pid(0.3, 0.005, 0.0);
        input.set_gyro_threshold(THRESHOLD_STANDARD);
        input.reset_quaternion();
        input.reset_rotations();
        input
    }

    pub fn set_pid(&mut self, new_kp: f32, new_ki: f32, new_kd: f32) {
        self.kp = new_kp;
        self.ki = new_ki;
        self.kd = new_kd;
    }

    pub fn set_acceleration(&mut self, acceleration: Vec3f) {
        self.accel = acceleration;
        self.accel.x = self.accel.x.clamp(-ACCEL_MAX_VALUE, ACCEL_MAX_VALUE);
        self.accel.y = self.accel.y.clamp(-ACCEL_MAX_VALUE, ACCEL_MAX_VALUE);
        self.accel.z = self.accel.z.clamp(-ACCEL_MAX_VALUE, ACCEL_MAX_VALUE);
    }

    pub fn set_gyroscope(&mut self, gyroscope: Vec3f) {
        self.gyro = gyroscope - self.gyro_bias;

        self.gyro.x = self.gyro.x.clamp(-GYRO_MAX_VALUE, GYRO_MAX_VALUE);
        self.gyro.y = self.gyro.y.clamp(-GYRO_MAX_VALUE, GYRO_MAX_VALUE);
        self.gyro.z = self.gyro.z.clamp(-GYRO_MAX_VALUE, GYRO_MAX_VALUE);

        // Auto adjust gyro_bias to minimize drift
        if !self.is_moving(IS_AT_REST_RELAXED) {
            self.gyro_bias = self.gyro_bias * 0.9999 + gyroscope * 0.0001;
        }

        // Adjust drift when calibration mode is enabled
        if self.calibration_mode {
            self.gyro_bias = self.gyro_bias * 0.99 + gyroscope * 0.01;
            self.stop_calibration();
        }

        if self.gyro.length() < self.gyro_threshold * self.user_gyro_threshold {
            self.gyro = Vec3f::default();
        } else {
            self.only_accelerometer = false;
        }
    }

    pub fn set_quaternion(&mut self, quaternion: Quaternion) {
        self.quat = quaternion;
    }

    pub fn set_euler_angles(&mut self, euler_angles: Vec3f) {
        let cr = (euler_angles.x * 0.5).cos();
        let sr = (euler_angles.x * 0.5).sin();
        let cp = (euler_angles.y * 0.5).cos();
        let sp = (euler_angles.y * 0.5).sin();
        let cy = (euler_angles.z * 0.5).cos();
        let sy = (euler_angles.z * 0.5).sin();

        self.quat.w = cr * cp * cy + sr * sp * sy;
        self.quat.x = sr * cp * cy - cr * sp * sy;
        self.quat.y = cr * sp * cy + sr * cp * sy;
        self.quat.z = cr * cp * sy - sr * sp * cy;
    }

    pub fn set_gyro_bias(&mut self, bias: Vec3f) {
        self.gyro_bias = bias;
    }

    pub fn set_gyro_threshold(&mut self, threshold: f32) {
        self.gyro_threshold = threshold;
    }

    /// Applies a modifier on top of the normal gyro threshold
    pub fn set_user_gyro_threshold(&mut self, threshold: f32) {
        self.user_gyro_threshold = threshold / THRESHOLD_STANDARD;
    }

    pub fn enable_reset(&mut self, reset: bool) {
        self.reset_enabled = reset;
    }

    pub fn reset_rotations(&mut self) {
        self.rotations = Vec3f::default();
    }

    pub fn reset_quaternion(&mut self) {
        self.quat = Quaternion {
            w: 0.0,
            x: 0.0,
            y: 0.0,
            z: -1.0,
        };
    }

    pub fn update_rotation(&mut self, elapsed_time: u64) {
        let sample_period = elapsed_time as f32 / 1_000_000.0;
        if sample_period > 0.1 {
            return;
        }
        self.rotations += self.gyro * sample_period;
    }

    /// Based on Madgwick's implementation of Mayhony's AHRS algorithm.
    pub fn update_orientation(&mut self, elapsed_time: u64) {
        if !self.is_calibrated(0.1) {
            self.reset_orientation();
        }
        // Short name local variable for readability
        let mut q1 = self.quat.w;
        let mut q2 = self.quat.x;
        let mut q3 = self.quat.y;
        let mut q4 = self.quat.z;
        let sample_period = elapsed_time as f32 / 1_000_000.0;

        // Ignore invalid elapsed time
        if sample_period > 0.1 {
            return;
        }

        let normal_accel = self.accel.normalized();
        let mut rad_gyro = self.gyro * PI * 2.0;
        let swap = rad_gyro.x;
        rad_gyro.x = rad_gyro.y;
        rad_gyro.y = -swap;
        rad_gyro.z = -rad_gyro.z;

        // Clear gyro values if there is no gyro present
        if self.only_accelerometer {
            rad_gyro.x = 0.0;
            rad_gyro.y = 0.0;
            rad_gyro.z = 0.0;
        }

        // Ignore drift correction if acceleration is not reliable
        if self.accel.length() >= 0.75 && self.accel.length() <= 1.25 {
            let ax = -normal_accel.x;
            let ay = normal_accel.y;
            let az = -normal_accel.z;

            // Estimated direction of gravity
            let vx = 2.0 * (q2 * q4 - q1 * q3);
            let vy = 2.0 * (q1 * q2 + q3 * q4);
            let vz = q1 * q1 - q2 * q2 - q3 * q3 + q4 * q4;

            // Error is cross product between estimated direction and measured direction of gravity
            let new_real_error = Vec3f {
                x: az * vx - ax * vz,
                y: ay * vz - az * vy,
                z: ax * vy - ay * vx,
            };

            self.derivative_error = new_real_error - self.real_error;
            self.real_error = new_real_error;

            // Prevent integral windup
            if self.ki != 0.0 && !self.is_calibrated(0.05) {
                self.integral_error += self.real_error;
            } else {
                self.integral_error = Vec3f::default();
            }

            // Apply feedback terms
            if !self.only_accelerometer {
                rad_gyro += self.kp * self.real_error;
                rad_gyro += self.ki * self.integral_error;
                rad_gyro += self.kd * self.derivative_error;
            } else {
                // Give more weight to accelerometer values to compensate for the lack of gyro
                rad_gyro += 35.0 * self.kp * self.real_error;
                rad_gyro += 10.0 * self.ki * self.integral_error;
                rad_gyro += 10.0 * self.kd * self.derivative_error;

                // Emulate gyro values for games that need them
                self.gyro.x = -rad_gyro.y;
                self.gyro.y = rad_gyro.x;
                self.gyro.z = -rad_gyro.z;
                self.update_rotation(elapsed_time);
            }
        }

        let gx = rad_gyro.y;
        let gy = rad_gyro.x;
        let gz = rad_gyro.z;

        // Integrate rate of change of quaternion
        let pa = q2;
        let pb = q3;
        let pc = q4;
        q1 = q1 + (-q2 * gx - q3 * gy - q4 * gz) * (0.5 * sample_period);
        q2 = pa + (q1 * gx + pb * gz - pc * gy) * (0.5 * sample_period);
        q3 = pb + (q1 * gy - pa * gz + pc * gx) * (0.5 * sample_period);
        q4 = pc + (q1 * gz + pa * gy - pb * gx) * (0.5 * sample_period);

        self.quat.w = q1;
        self.quat.x = q2;
        self.quat.y = q3;
        self.quat.z = q4;
        self.quat = self.quat.normalized();
    }

    pub fn calibrate(&mut self) {
        self.calibration_mode = true;
        self.calibration_counter = 0;
    }

    fn stop_calibration(&mut self) {
        self.calibration_counter += 1;
        if self.calibration_counter > CALIBRATION_SAMPLES {
            self.calibration_mode = false;
            self.reset_quaternion();
            self.reset_rotations();
        }
    }

    pub fn get_orientation(&self) -> [Vec3f; 3] {
        let quad = Quaternion {
            x: -self.quat.y,
            y: -self.quat.x,
            z: -self.quat.w,
            w: -self.quat.z,
        };
        let m = quad.to_matrix();

        [
            Vec3f::new(m[0], m[1], -m[2]),
            Vec3f::new(m[4], m[5], -m[6]),
            Vec3f::new(-m[8], -m[9], m[10]),
        ]
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
        // roll (x-axis rotation)
        let sinr_cosp = 2.0 * (self.quat.w * self.quat.x + self.quat.y * self.quat.z);
        let cosr_cosp = 1.0 - 2.0 * (self.quat.x * self.quat.x + self.quat.y * self.quat.y);

        // pitch (y-axis rotation)
        let sinp = (1.0 + 2.0 * (self.quat.w * self.quat.y - self.quat.x * self.quat.z)).sqrt();
        let cosp = (1.0 - 2.0 * (self.quat.w * self.quat.y - self.quat.x * self.quat.z)).sqrt();

        // yaw (z-axis rotation)
        let siny_cosp = 2.0 * (self.quat.w * self.quat.z + self.quat.x * self.quat.y);
        let cosy_cosp = 1.0 - 2.0 * (self.quat.y * self.quat.y + self.quat.z * self.quat.z);

        Vec3f {
            x: sinr_cosp.atan2(cosr_cosp),
            y: 2.0 * sinp.atan2(cosp) - PI / 2.0,
            z: siny_cosp.atan2(cosy_cosp),
        }
    }

    pub fn is_moving(&self, sensitivity: f32) -> bool {
        self.gyro.length() >= sensitivity
            || self.accel.length() <= 0.9
            || self.accel.length() >= 1.1
    }

    pub fn is_calibrated(&self, sensitivity: f32) -> bool {
        self.real_error.length() < sensitivity
    }

    fn reset_orientation(&mut self) {
        if !self.reset_enabled || self.only_accelerometer {
            return;
        }
        if !self.is_moving(IS_AT_REST_RELAXED) && self.accel.z <= -0.9 {
            self.reset_counter += 1;
            if self.reset_counter > 900 {
                self.quat.w = 0.0;
                self.quat.x = 0.0;
                self.quat.y = 0.0;
                self.quat.z = -1.0;
                self.set_orientation_from_accelerometer();
                self.integral_error = Vec3f::default();
                self.reset_counter = 0;
            }
        } else {
            self.reset_counter = 0;
        }
    }

    fn set_orientation_from_accelerometer(&mut self) {
        let mut iterations = 0;
        let sample_period: f32 = 0.015;

        let normal_accel = self.accel.normalized();

        while !self.is_calibrated(0.01) && iterations < 100 {
            iterations += 1;

            // Short name local variable for readability
            let mut q1 = self.quat.w;
            let mut q2 = self.quat.x;
            let mut q3 = self.quat.y;
            let mut q4 = self.quat.z;

            let mut rad_gyro = Vec3f::default();
            let ax = -normal_accel.x;
            let ay = normal_accel.y;
            let az = -normal_accel.z;

            // Estimated direction of gravity
            let vx = 2.0 * (q2 * q4 - q1 * q3);
            let vy = 2.0 * (q1 * q2 + q3 * q4);
            let vz = q1 * q1 - q2 * q2 - q3 * q3 + q4 * q4;

            // Error is cross product between estimated direction and measured direction of gravity
            let new_real_error = Vec3f {
                x: az * vx - ax * vz,
                y: ay * vz - az * vy,
                z: ax * vy - ay * vx,
            };

            self.derivative_error = new_real_error - self.real_error;
            self.real_error = new_real_error;

            rad_gyro += 10.0 * self.kp * self.real_error;
            rad_gyro += 5.0 * self.ki * self.integral_error;
            rad_gyro += 10.0 * self.kd * self.derivative_error;

            let gx = rad_gyro.y;
            let gy = rad_gyro.x;
            let gz = rad_gyro.z;

            // Integrate rate of change of quaternion
            let pa = q2;
            let pb = q3;
            let pc = q4;
            q1 = q1 + (-q2 * gx - q3 * gy - q4 * gz) * (0.5 * sample_period);
            q2 = pa + (q1 * gx + pb * gz - pc * gy) * (0.5 * sample_period);
            q3 = pb + (q1 * gy - pa * gz + pc * gx) * (0.5 * sample_period);
            q4 = pc + (q1 * gz + pa * gy - pb * gx) * (0.5 * sample_period);

            self.quat.w = q1;
            self.quat.x = q2;
            self.quat.y = q3;
            self.quat.z = q4;
            self.quat = self.quat.normalized();
        }
    }
}
