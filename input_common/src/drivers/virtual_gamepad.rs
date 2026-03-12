// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/virtual_gamepad.h` and `input_common/drivers/virtual_gamepad.cpp`.
//!
//! Virtual controller that is always assigned to the game input.

use crate::input_engine::{BasicMotion, InputEngine, PadIdentifier};

/// Port of VirtualGamepad::VirtualButton enum from virtual_gamepad.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VirtualButton {
    ButtonA,
    ButtonB,
    ButtonX,
    ButtonY,
    StickL,
    StickR,
    TriggerL,
    TriggerR,
    TriggerZL,
    TriggerZR,
    ButtonPlus,
    ButtonMinus,
    ButtonLeft,
    ButtonUp,
    ButtonRight,
    ButtonDown,
    ButtonSL,
    ButtonSR,
    ButtonHome,
    ButtonCapture,
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
        Self {
            engine: InputEngine::new(input_engine),
        }
    }

    /// Returns a reference to the underlying input engine.
    pub fn engine(&self) -> &InputEngine {
        &self.engine
    }

    /// Returns a mutable reference to the underlying input engine.
    pub fn engine_mut(&mut self) -> &mut InputEngine {
        &mut self.engine
    }

    /// Sets the status of all buttons bound with the key to pressed (by int id).
    /// Port of VirtualGamepad::SetButtonState(size_t, int, bool)
    pub fn set_button_state_by_id(
        &mut self,
        _player_index: usize,
        _button_id: i32,
        _value: bool,
    ) {
        todo!()
    }

    /// Sets the status of all buttons bound with the key to pressed (by enum).
    /// Port of VirtualGamepad::SetButtonState(size_t, VirtualButton, bool)
    pub fn set_button_state(
        &mut self,
        _player_index: usize,
        _button_id: VirtualButton,
        _value: bool,
    ) {
        todo!()
    }

    /// Sets the status of a stick to a specific player index (by int id).
    /// Port of VirtualGamepad::SetStickPosition(size_t, int, float, float)
    pub fn set_stick_position_by_id(
        &mut self,
        _player_index: usize,
        _axis_id: i32,
        _x_value: f32,
        _y_value: f32,
    ) {
        todo!()
    }

    /// Sets the status of a stick to a specific player index (by enum).
    /// Port of VirtualGamepad::SetStickPosition(size_t, VirtualStick, float, float)
    pub fn set_stick_position(
        &mut self,
        _player_index: usize,
        _axis_id: VirtualStick,
        _x_value: f32,
        _y_value: f32,
    ) {
        todo!()
    }

    /// Sets the status of the motion sensor to a specific player index.
    /// Port of VirtualGamepad::SetMotionState
    pub fn set_motion_state(
        &mut self,
        _player_index: usize,
        _delta_timestamp: u64,
        _gyro_x: f32,
        _gyro_y: f32,
        _gyro_z: f32,
        _accel_x: f32,
        _accel_y: f32,
        _accel_z: f32,
    ) {
        todo!()
    }

    /// Restores all inputs into the neutral position.
    /// Port of VirtualGamepad::ResetControllers
    pub fn reset_controllers(&mut self) {
        todo!()
    }

    // ---- Private methods ----

    /// Returns the correct identifier corresponding to the player index.
    /// Port of VirtualGamepad::GetIdentifier
    fn get_identifier(&self, _player_index: usize) -> PadIdentifier {
        todo!()
    }
}
