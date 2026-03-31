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
    pub fn read_passive_mode(&mut self, buffer: &mut [u8]) {
        if buffer.len() < std::mem::size_of::<InputReportPassive>() {
            return;
        }
        let mut data = InputReportPassive::default();
        unsafe {
            std::ptr::copy_nonoverlapping(
                buffer.as_ptr(),
                &mut data as *mut _ as *mut u8,
                std::mem::size_of::<InputReportPassive>(),
            );
        }

        match self.device_type {
            ControllerType::Left => self.update_passive_left_pad_input(&data),
            ControllerType::Right => self.update_passive_right_pad_input(&data),
            ControllerType::Pro => self.update_passive_pro_pad_input(&data),
            _ => {}
        }
    }

    /// Handles data from active packages.
    /// Port of JoyconPoller::ReadActiveMode
    pub fn read_active_mode(
        &mut self,
        buffer: &mut [u8],
        motion_status: &MotionStatus,
        ring_status: &RingStatus,
    ) {
        if buffer.len() < std::mem::size_of::<InputReportActive>() {
            return;
        }
        let mut data = InputReportActive::default();
        unsafe {
            std::ptr::copy_nonoverlapping(
                buffer.as_ptr(),
                &mut data as *mut _ as *mut u8,
                std::mem::size_of::<InputReportActive>(),
            );
        }

        match self.device_type {
            ControllerType::Left => self.update_active_left_pad_input(&data, motion_status),
            ControllerType::Right => self.update_active_right_pad_input(&data, motion_status),
            ControllerType::Pro => self.update_active_pro_pad_input(&data, motion_status),
            _ => {}
        }

        if ring_status.is_enabled {
            // SAFETY: packed field read — copy to local first
            let ring_input = data.ring_input;
            self.update_ring(ring_input, ring_status);
        }

        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_battery_data {
                let battery = Battery {
                    raw: data.battery_status,
                };
                f(battery);
            }
        }
    }

    /// Handles data from nfc or ir packages.
    /// Port of JoyconPoller::ReadNfcIRMode
    pub fn read_nfc_ir_mode(&mut self, buffer: &mut [u8], motion_status: &MotionStatus) {
        // This mode is compatible with the active mode
        let empty_ring = RingStatus {
            is_enabled: false,
            default_value: 0,
            max_value: 0,
            min_value: 0,
        };
        self.read_active_mode(buffer, motion_status, &empty_ring);
    }

    /// Port of JoyconPoller::UpdateColor
    pub fn update_color(&mut self, color: &Color) {
        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_color_data {
                f(color.clone());
            }
        }
    }

    /// Port of JoyconPoller::UpdateRing
    pub fn update_ring(&mut self, value: i16, ring_status: &RingStatus) {
        let mut normalized_value = (value - ring_status.default_value) as f32;
        if normalized_value > 0.0 {
            normalized_value /= (ring_status.max_value - ring_status.default_value) as f32;
        }
        if normalized_value < 0.0 {
            normalized_value /= (ring_status.default_value - ring_status.min_value) as f32;
        }
        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_ring_data {
                f(normalized_value);
            }
        }
    }

    /// Port of JoyconPoller::UpdateAmiibo
    pub fn update_amiibo(&mut self, tag_info: &TagInfo) {
        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_amiibo_data {
                f(tag_info);
            }
        }
    }

    /// Port of JoyconPoller::UpdateCamera
    pub fn update_camera(&mut self, camera_data: &[u8], format: IrsResolution) {
        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_camera_data {
                f(camera_data, format);
            }
        }
    }

    // ---- Private methods ----

    /// Port of JoyconPoller::UpdateActiveLeftPadInput
    fn update_active_left_pad_input(
        &mut self,
        input: &InputReportActive,
        motion_status: &MotionStatus,
    ) {
        const LEFT_BUTTONS: [PadButton; 11] = [
            PadButton::Down,
            PadButton::Up,
            PadButton::Right,
            PadButton::Left,
            PadButton::LeftSL,
            PadButton::LeftSR,
            PadButton::L,
            PadButton::ZL,
            PadButton::Minus,
            PadButton::Capture,
            PadButton::StickL,
        ];

        // SAFETY: packed field reads — copy to locals first
        let btn2 = input.button_input[2];
        let btn1 = input.button_input[1];
        let raw_button = btn2 as u32 | (((btn1 & 0b00101001) as u32) << 16);

        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_button_data {
                for &button in &LEFT_BUTTONS {
                    let button_status = (raw_button & button as u32) != 0;
                    f(button as i32, button_status);
                }
            }
        }

        let ls0 = input.left_stick_state[0];
        let ls1 = input.left_stick_state[1];
        let ls2 = input.left_stick_state[2];
        let raw_left_axis_x = (ls0 as u16) | (((ls1 & 0xf) as u16) << 8);
        let raw_left_axis_y = ((ls1 >> 4) as u16) | ((ls2 as u16) << 4);
        let left_axis_x =
            self.get_axis_value(raw_left_axis_x, &self.left_stick_calibration.x.clone());
        let left_axis_y =
            self.get_axis_value(raw_left_axis_y, &self.left_stick_calibration.y.clone());

        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_stick_data {
                f(PadAxes::LeftStickX as i32, left_axis_x);
                f(PadAxes::LeftStickY as i32, left_axis_y);
            }
        }

        if motion_status.is_enabled {
            let mut left_motion = self.get_motion_input(input, motion_status);
            // Rotate motion axis to the correct direction
            left_motion.accel_y = -left_motion.accel_y;
            left_motion.accel_z = -left_motion.accel_z;
            left_motion.gyro_x = -left_motion.gyro_x;

            if let Some(ref cb) = self.callbacks {
                if let Some(ref f) = cb.on_motion_data {
                    f(PadMotion::LeftMotion as i32, &left_motion);
                }
            }
        }
    }

    /// Port of JoyconPoller::UpdateActiveRightPadInput
    fn update_active_right_pad_input(
        &mut self,
        input: &InputReportActive,
        motion_status: &MotionStatus,
    ) {
        const RIGHT_BUTTONS: [PadButton; 11] = [
            PadButton::Y,
            PadButton::X,
            PadButton::B,
            PadButton::A,
            PadButton::RightSL,
            PadButton::RightSR,
            PadButton::R,
            PadButton::ZR,
            PadButton::Plus,
            PadButton::Home,
            PadButton::StickR,
        ];

        let btn0 = input.button_input[0];
        let btn1 = input.button_input[1];
        let raw_button = ((btn0 as u32) << 8) | ((btn1 as u32) << 16);

        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_button_data {
                for &button in &RIGHT_BUTTONS {
                    let button_status = (raw_button & button as u32) != 0;
                    f(button as i32, button_status);
                }
            }
        }

        let rs0 = input.right_stick_state[0];
        let rs1 = input.right_stick_state[1];
        let rs2 = input.right_stick_state[2];
        let raw_right_axis_x = (rs0 as u16) | (((rs1 & 0xf) as u16) << 8);
        let raw_right_axis_y = ((rs1 >> 4) as u16) | ((rs2 as u16) << 4);
        let right_axis_x =
            self.get_axis_value(raw_right_axis_x, &self.right_stick_calibration.x.clone());
        let right_axis_y =
            self.get_axis_value(raw_right_axis_y, &self.right_stick_calibration.y.clone());

        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_stick_data {
                f(PadAxes::RightStickX as i32, right_axis_x);
                f(PadAxes::RightStickY as i32, right_axis_y);
            }
        }

        if motion_status.is_enabled {
            let mut right_motion = self.get_motion_input(input, motion_status);
            // Rotate motion axis to the correct direction
            right_motion.accel_x = -right_motion.accel_x;
            right_motion.accel_y = -right_motion.accel_y;
            right_motion.gyro_z = -right_motion.gyro_z;

            if let Some(ref cb) = self.callbacks {
                if let Some(ref f) = cb.on_motion_data {
                    f(PadMotion::RightMotion as i32, &right_motion);
                }
            }
        }
    }

    /// Port of JoyconPoller::UpdateActiveProPadInput
    fn update_active_pro_pad_input(
        &mut self,
        input: &InputReportActive,
        motion_status: &MotionStatus,
    ) {
        const PRO_BUTTONS: [PadButton; 18] = [
            PadButton::Down,
            PadButton::Up,
            PadButton::Right,
            PadButton::Left,
            PadButton::L,
            PadButton::ZL,
            PadButton::Minus,
            PadButton::Capture,
            PadButton::Y,
            PadButton::X,
            PadButton::B,
            PadButton::A,
            PadButton::R,
            PadButton::ZR,
            PadButton::Plus,
            PadButton::Home,
            PadButton::StickL,
            PadButton::StickR,
        ];

        let btn0 = input.button_input[0];
        let btn1 = input.button_input[1];
        let btn2 = input.button_input[2];
        let raw_button = (btn2 as u32) | ((btn0 as u32) << 8) | ((btn1 as u32) << 16);

        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_button_data {
                for &button in &PRO_BUTTONS {
                    let button_status = (raw_button & button as u32) != 0;
                    f(button as i32, button_status);
                }
            }
        }

        let ls0 = input.left_stick_state[0];
        let ls1 = input.left_stick_state[1];
        let ls2 = input.left_stick_state[2];
        let rs0 = input.right_stick_state[0];
        let rs1 = input.right_stick_state[1];
        let rs2 = input.right_stick_state[2];
        let raw_left_axis_x = (ls0 as u16) | (((ls1 & 0xf) as u16) << 8);
        let raw_left_axis_y = ((ls1 >> 4) as u16) | ((ls2 as u16) << 4);
        let raw_right_axis_x = (rs0 as u16) | (((rs1 & 0xf) as u16) << 8);
        let raw_right_axis_y = ((rs1 >> 4) as u16) | ((rs2 as u16) << 4);

        let left_cal_x = self.left_stick_calibration.x.clone();
        let left_cal_y = self.left_stick_calibration.y.clone();
        let right_cal_x = self.right_stick_calibration.x.clone();
        let right_cal_y = self.right_stick_calibration.y.clone();
        let left_axis_x = self.get_axis_value(raw_left_axis_x, &left_cal_x);
        let left_axis_y = self.get_axis_value(raw_left_axis_y, &left_cal_y);
        let right_axis_x = self.get_axis_value(raw_right_axis_x, &right_cal_x);
        let right_axis_y = self.get_axis_value(raw_right_axis_y, &right_cal_y);

        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_stick_data {
                f(PadAxes::LeftStickX as i32, left_axis_x);
                f(PadAxes::LeftStickY as i32, left_axis_y);
                f(PadAxes::RightStickX as i32, right_axis_x);
                f(PadAxes::RightStickY as i32, right_axis_y);
            }
        }

        if motion_status.is_enabled {
            let mut pro_motion = self.get_motion_input(input, motion_status);
            pro_motion.gyro_x = -pro_motion.gyro_x;
            pro_motion.accel_y = -pro_motion.accel_y;
            pro_motion.accel_z = -pro_motion.accel_z;

            if let Some(ref cb) = self.callbacks {
                if let Some(ref f) = cb.on_motion_data {
                    f(PadMotion::LeftMotion as i32, &pro_motion);
                    f(PadMotion::RightMotion as i32, &pro_motion);
                }
            }
        }
    }

    /// Port of JoyconPoller::UpdatePassiveLeftPadInput
    fn update_passive_left_pad_input(&mut self, input: &InputReportPassive) {
        const LEFT_BUTTONS: [PassivePadButton; 11] = [
            PassivePadButton::DownA,
            PassivePadButton::RightX,
            PassivePadButton::LeftB,
            PassivePadButton::UpY,
            PassivePadButton::SL,
            PassivePadButton::SR,
            PassivePadButton::LR,
            PassivePadButton::ZlZr,
            PassivePadButton::Minus,
            PassivePadButton::Capture,
            PassivePadButton::StickL,
        ];

        // SAFETY: packed field — copy to local
        let button_input = input.button_input;
        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_button_data {
                for &button in &LEFT_BUTTONS {
                    let button_status = (button_input as u32 & button as u32) != 0;
                    f(button as i32, button_status);
                }
            }
        }

        let stick_state = input.stick_state;
        let passive_stick = passive_pad_stick_from_u8(stick_state);
        let (left_axis_x, left_axis_y) = self.get_passive_axis_value(passive_stick);
        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_stick_data {
                f(PadAxes::LeftStickX as i32, left_axis_x);
                f(PadAxes::LeftStickY as i32, left_axis_y);
            }
        }
    }

    /// Port of JoyconPoller::UpdatePassiveRightPadInput
    fn update_passive_right_pad_input(&mut self, input: &InputReportPassive) {
        const RIGHT_BUTTONS: [PassivePadButton; 11] = [
            PassivePadButton::DownA,
            PassivePadButton::RightX,
            PassivePadButton::LeftB,
            PassivePadButton::UpY,
            PassivePadButton::SL,
            PassivePadButton::SR,
            PassivePadButton::LR,
            PassivePadButton::ZlZr,
            PassivePadButton::Plus,
            PassivePadButton::Home,
            PassivePadButton::StickR,
        ];

        let button_input = input.button_input;
        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_button_data {
                for &button in &RIGHT_BUTTONS {
                    let button_status = (button_input as u32 & button as u32) != 0;
                    f(button as i32, button_status);
                }
            }
        }

        let stick_state = input.stick_state;
        let passive_stick = passive_pad_stick_from_u8(stick_state);
        let (right_axis_x, right_axis_y) = self.get_passive_axis_value(passive_stick);
        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_stick_data {
                f(PadAxes::RightStickX as i32, right_axis_x);
                f(PadAxes::RightStickY as i32, right_axis_y);
            }
        }
    }

    /// Port of JoyconPoller::UpdatePassiveProPadInput
    fn update_passive_pro_pad_input(&mut self, input: &InputReportPassive) {
        const PRO_BUTTONS: [PassivePadButton; 14] = [
            PassivePadButton::DownA,
            PassivePadButton::RightX,
            PassivePadButton::LeftB,
            PassivePadButton::UpY,
            PassivePadButton::SL,
            PassivePadButton::SR,
            PassivePadButton::LR,
            PassivePadButton::ZlZr,
            PassivePadButton::Minus,
            PassivePadButton::Plus,
            PassivePadButton::Capture,
            PassivePadButton::Home,
            PassivePadButton::StickL,
            PassivePadButton::StickR,
        ];

        let button_input = input.button_input;
        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_button_data {
                for &button in &PRO_BUTTONS {
                    let button_status = (button_input as u32 & button as u32) != 0;
                    f(button as i32, button_status);
                }
            }
        }

        let stick_state = input.stick_state;
        let left_stick = passive_pad_stick_from_u8(stick_state & 0xf);
        let right_stick = passive_pad_stick_from_u8(stick_state >> 4);
        let (left_axis_x, left_axis_y) = self.get_passive_axis_value(left_stick);
        let (right_axis_x, right_axis_y) = self.get_passive_axis_value(right_stick);
        if let Some(ref cb) = self.callbacks {
            if let Some(ref f) = cb.on_stick_data {
                f(PadAxes::LeftStickX as i32, left_axis_x);
                f(PadAxes::LeftStickY as i32, left_axis_y);
                f(PadAxes::RightStickX as i32, right_axis_x);
                f(PadAxes::RightStickY as i32, right_axis_y);
            }
        }
    }

    /// Port of JoyconPoller::GetAxisValue
    fn get_axis_value(&self, raw_value: u16, calibration: &JoyStickAxisCalibration) -> f32 {
        let value = raw_value as f32 - calibration.center as f32;
        if value > 0.0 {
            value / calibration.max as f32
        } else {
            value / calibration.min as f32
        }
    }

    /// Port of JoyconPoller::GetPassiveAxisValue
    fn get_passive_axis_value(&self, raw_value: PassivePadStick) -> (f32, f32) {
        match raw_value {
            PassivePadStick::Right => (1.0, 0.0),
            PassivePadStick::RightDown => (1.0, -1.0),
            PassivePadStick::Down => (0.0, -1.0),
            PassivePadStick::DownLeft => (-1.0, -1.0),
            PassivePadStick::Left => (-1.0, 0.0),
            PassivePadStick::LeftUp => (-1.0, 1.0),
            PassivePadStick::Up => (0.0, 1.0),
            PassivePadStick::UpRight => (1.0, 1.0),
            PassivePadStick::Neutral => (0.0, 0.0),
        }
    }

    /// Port of JoyconPoller::GetAccelerometerValue
    fn get_accelerometer_value(
        &self,
        raw: i16,
        cal: &MotionSensorCalibration,
        sensitivity: AccelerometerSensitivity,
    ) -> f32 {
        let value = raw as f32 * (1.0 / (cal.scale - cal.offset) as f32) * 4.0;
        match sensitivity {
            AccelerometerSensitivity::G2 => value / 4.0,
            AccelerometerSensitivity::G4 => value / 2.0,
            AccelerometerSensitivity::G8 => value,
            AccelerometerSensitivity::G16 => value * 2.0,
        }
    }

    /// Port of JoyconPoller::GetGyroValue
    fn get_gyro_value(
        &self,
        raw_value: i16,
        cal: &MotionSensorCalibration,
        sensitivity: GyroSensitivity,
    ) -> f32 {
        let value =
            (raw_value - cal.offset) as f32 * (936.0 / (cal.scale - cal.offset) as f32) / 360.0;
        match sensitivity {
            GyroSensitivity::Dps250 => value / 8.0,
            GyroSensitivity::Dps500 => value / 4.0,
            GyroSensitivity::Dps1000 => value / 2.0,
            GyroSensitivity::Dps2000 => value,
        }
    }

    /// Port of JoyconPoller::GetMotionInput
    fn get_motion_input(
        &self,
        input: &InputReportActive,
        motion_status: &MotionStatus,
    ) -> MotionData {
        let accel_cal = &self.motion_calibration.accelerometer;
        let gyro_cal = &self.motion_calibration.gyro;

        // Upstream uses motion_input[0..5] for first sample
        // motion_input[1] = raw_accel_x, [0] = raw_accel_y, [2] = raw_accel_z
        // motion_input[4] = raw_gyro_x,  [3] = raw_gyro_y,  [5] = raw_gyro_z
        let raw_accel_x = input.motion_input[1];
        let raw_accel_y = input.motion_input[0];
        let raw_accel_z = input.motion_input[2];
        let raw_gyro_x = input.motion_input[4];
        let raw_gyro_y = input.motion_input[3];
        let raw_gyro_z = input.motion_input[5];

        MotionData {
            delta_timestamp: motion_status.delta_time,
            accel_x: self.get_accelerometer_value(
                raw_accel_x,
                &accel_cal[1],
                motion_status.accelerometer_sensitivity,
            ),
            accel_y: self.get_accelerometer_value(
                raw_accel_y,
                &accel_cal[0],
                motion_status.accelerometer_sensitivity,
            ),
            accel_z: self.get_accelerometer_value(
                raw_accel_z,
                &accel_cal[2],
                motion_status.accelerometer_sensitivity,
            ),
            gyro_x: self.get_gyro_value(raw_gyro_x, &gyro_cal[1], motion_status.gyro_sensitivity),
            gyro_y: self.get_gyro_value(raw_gyro_y, &gyro_cal[0], motion_status.gyro_sensitivity),
            gyro_z: self.get_gyro_value(raw_gyro_z, &gyro_cal[2], motion_status.gyro_sensitivity),
        }
    }
}

/// Helper: convert raw u8 to PassivePadStick with Neutral as fallback.
fn passive_pad_stick_from_u8(v: u8) -> PassivePadStick {
    match v {
        0x00 => PassivePadStick::Right,
        0x01 => PassivePadStick::RightDown,
        0x02 => PassivePadStick::Down,
        0x03 => PassivePadStick::DownLeft,
        0x04 => PassivePadStick::Left,
        0x05 => PassivePadStick::LeftUp,
        0x06 => PassivePadStick::Up,
        0x07 => PassivePadStick::UpRight,
        _ => PassivePadStick::Neutral,
    }
}
