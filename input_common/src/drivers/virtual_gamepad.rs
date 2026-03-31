// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/virtual_gamepad.h` and `input_common/drivers/virtual_gamepad.cpp`.
//!
//! Virtual controller that is always assigned to the game input.

use common::uuid::UUID;

use crate::input_engine::{BasicMotion, InputEngine, PadIdentifier};

const PLAYER_INDEX_COUNT: usize = 10;

/// Port of VirtualGamepad::VirtualButton enum from virtual_gamepad.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum VirtualButton {
    ButtonA = 0,
    ButtonB = 1,
    ButtonX = 2,
    ButtonY = 3,
    StickL = 4,
    StickR = 5,
    TriggerL = 6,
    TriggerR = 7,
    TriggerZL = 8,
    TriggerZR = 9,
    ButtonPlus = 10,
    ButtonMinus = 11,
    ButtonLeft = 12,
    ButtonUp = 13,
    ButtonRight = 14,
    ButtonDown = 15,
    ButtonSL = 16,
    ButtonSR = 17,
    ButtonHome = 18,
    ButtonCapture = 19,
}

/// Port of VirtualGamepad::VirtualStick enum from virtual_gamepad.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum VirtualStick {
    Left = 0,
    Right = 1,
}

/// Port of `VirtualGamepad` class from virtual_gamepad.h / virtual_gamepad.cpp
pub struct VirtualGamepad {
    engine: InputEngine,
}

impl VirtualGamepad {
    /// Port of VirtualGamepad::VirtualGamepad
    pub fn new(input_engine: String) -> Self {
        let mut vg = Self {
            engine: InputEngine::new(input_engine),
        };
        for i in 0..PLAYER_INDEX_COUNT {
            let identifier = vg.get_identifier(i);
            vg.engine.pre_set_controller(&identifier);
        }
        vg
    }

    /// Returns a reference to the underlying input engine.
    pub fn engine(&self) -> &InputEngine {
        &self.engine
    }

    /// Returns a mutable reference to the underlying input engine.
    pub fn engine_mut(&mut self) -> &mut InputEngine {
        &mut self.engine
    }

    pub fn get_engine_name(&self) -> &str {
        self.engine.get_engine_name()
    }

    /// Sets the status of all buttons bound with the key to pressed (by int id).
    /// Port of VirtualGamepad::SetButtonState(size_t, int, bool)
    pub fn set_button_state_by_id(&mut self, player_index: usize, button_id: i32, value: bool) {
        if player_index > PLAYER_INDEX_COUNT {
            return;
        }
        let identifier = self.get_identifier(player_index);
        self.engine.set_button(&identifier, button_id, value);
    }

    /// Sets the status of all buttons bound with the key to pressed (by enum).
    /// Port of VirtualGamepad::SetButtonState(size_t, VirtualButton, bool)
    pub fn set_button_state(&mut self, player_index: usize, button_id: VirtualButton, value: bool) {
        self.set_button_state_by_id(player_index, button_id as i32, value);
    }

    /// Sets the status of a stick to a specific player index (by int id).
    /// Port of VirtualGamepad::SetStickPosition(size_t, int, float, float)
    pub fn set_stick_position_by_id(
        &mut self,
        player_index: usize,
        axis_id: i32,
        x_value: f32,
        y_value: f32,
    ) {
        if player_index > PLAYER_INDEX_COUNT {
            return;
        }
        let identifier = self.get_identifier(player_index);
        self.engine.set_axis(&identifier, axis_id * 2, x_value);
        self.engine
            .set_axis(&identifier, (axis_id * 2) + 1, y_value);
    }

    /// Sets the status of a stick to a specific player index (by enum).
    /// Port of VirtualGamepad::SetStickPosition(size_t, VirtualStick, float, float)
    pub fn set_stick_position(
        &mut self,
        player_index: usize,
        axis_id: VirtualStick,
        x_value: f32,
        y_value: f32,
    ) {
        self.set_stick_position_by_id(player_index, axis_id as i32, x_value, y_value);
    }

    /// Sets the status of the motion sensor to a specific player index.
    /// Port of VirtualGamepad::SetMotionState
    pub fn set_motion_state(
        &mut self,
        player_index: usize,
        delta_timestamp: u64,
        gyro_x: f32,
        gyro_y: f32,
        gyro_z: f32,
        accel_x: f32,
        accel_y: f32,
        accel_z: f32,
    ) {
        let identifier = self.get_identifier(player_index);
        let motion_data = BasicMotion {
            gyro_x,
            gyro_y,
            gyro_z,
            accel_x,
            accel_y,
            accel_z,
            delta_timestamp,
        };
        self.engine.set_motion(&identifier, 0, &motion_data);
    }

    /// Restores all inputs into the neutral position.
    /// Port of VirtualGamepad::ResetControllers
    pub fn reset_controllers(&mut self) {
        for i in 0..PLAYER_INDEX_COUNT {
            self.set_stick_position(i, VirtualStick::Left, 0.0, 0.0);
            self.set_stick_position(i, VirtualStick::Right, 0.0, 0.0);

            self.set_button_state(i, VirtualButton::ButtonA, false);
            self.set_button_state(i, VirtualButton::ButtonB, false);
            self.set_button_state(i, VirtualButton::ButtonX, false);
            self.set_button_state(i, VirtualButton::ButtonY, false);
            self.set_button_state(i, VirtualButton::StickL, false);
            self.set_button_state(i, VirtualButton::StickR, false);
            self.set_button_state(i, VirtualButton::TriggerL, false);
            self.set_button_state(i, VirtualButton::TriggerR, false);
            self.set_button_state(i, VirtualButton::TriggerZL, false);
            self.set_button_state(i, VirtualButton::TriggerZR, false);
            self.set_button_state(i, VirtualButton::ButtonPlus, false);
            self.set_button_state(i, VirtualButton::ButtonMinus, false);
            self.set_button_state(i, VirtualButton::ButtonLeft, false);
            self.set_button_state(i, VirtualButton::ButtonUp, false);
            self.set_button_state(i, VirtualButton::ButtonRight, false);
            self.set_button_state(i, VirtualButton::ButtonDown, false);
            self.set_button_state(i, VirtualButton::ButtonSL, false);
            self.set_button_state(i, VirtualButton::ButtonSR, false);
            self.set_button_state(i, VirtualButton::ButtonHome, false);
            self.set_button_state(i, VirtualButton::ButtonCapture, false);
        }
    }

    /// Returns the correct identifier corresponding to the player index.
    /// Port of VirtualGamepad::GetIdentifier
    fn get_identifier(&self, player_index: usize) -> PadIdentifier {
        PadIdentifier {
            guid: UUID::new(),
            port: player_index,
            pad: 0,
        }
    }
}
