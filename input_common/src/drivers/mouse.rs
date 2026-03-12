// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/mouse.h` and `input_common/drivers/mouse.cpp`.
//!
//! Mouse input driver that receives mouse events and forwards them to input devices.

use common::input::ButtonNames;
use common::param_package::ParamPackage;

use crate::input_engine::InputEngine;
use crate::main_common::AnalogMapping;

/// Port of `MouseButton` enum from mouse.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MouseButton {
    Left,
    Right,
    Wheel,
    Backward,
    Forward,
    Task,
    Extra,
    Undefined,
}

/// Port of `Mouse` class from mouse.h / mouse.cpp
pub struct Mouse {
    engine: InputEngine,
    mouse_origin: (i32, i32),
    last_mouse_position: (i32, i32),
    last_mouse_change: (f32, f32),
    last_motion_change: (f32, f32, f32),
    wheel_position: (i32, i32),
    button_pressed: bool,
}

impl Mouse {
    /// Port of Mouse::Mouse
    pub fn new(input_engine: String) -> Self {
        Self {
            engine: InputEngine::new(input_engine),
            mouse_origin: (0, 0),
            last_mouse_position: (0, 0),
            last_mouse_change: (0.0, 0.0),
            last_motion_change: (0.0, 0.0, 0.0),
            wheel_position: (0, 0),
            button_pressed: false,
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

    /// Signals that mouse has moved.
    /// Port of Mouse::Move
    pub fn r#move(&mut self, _x: i32, _y: i32, _center_x: i32, _center_y: i32) {
        todo!()
    }

    /// Signals that real mouse has moved.
    /// Port of Mouse::MouseMove
    pub fn mouse_move(&mut self, _touch_x: f32, _touch_y: f32) {
        todo!()
    }

    /// Signals that touch finger has moved.
    /// Port of Mouse::TouchMove
    pub fn touch_move(&mut self, _touch_x: f32, _touch_y: f32) {
        todo!()
    }

    /// Sets the status of a button to pressed.
    /// Port of Mouse::PressButton
    pub fn press_button(&mut self, _x: i32, _y: i32, _button: MouseButton) {
        todo!()
    }

    /// Sets the status of a mouse button to pressed.
    /// Port of Mouse::PressMouseButton
    pub fn press_mouse_button(&mut self, _button: MouseButton) {
        todo!()
    }

    /// Sets the status of touch finger to pressed.
    /// Port of Mouse::PressTouchButton
    pub fn press_touch_button(&mut self, _touch_x: f32, _touch_y: f32, _button: MouseButton) {
        todo!()
    }

    /// Sets the status of all buttons bound with the key to released.
    /// Port of Mouse::ReleaseButton
    pub fn release_button(&mut self, _button: MouseButton) {
        todo!()
    }

    /// Sets the status of the mouse wheel.
    /// Port of Mouse::MouseWheelChange
    pub fn mouse_wheel_change(&mut self, _x: i32, _y: i32) {
        todo!()
    }

    /// Port of Mouse::ReleaseAllButtons
    pub fn release_all_buttons(&mut self) {
        todo!()
    }

    /// Port of Mouse::GetInputDevices (override)
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        todo!()
    }

    /// Port of Mouse::GetAnalogMappingForDevice (override)
    pub fn get_analog_mapping_for_device(&self, _params: &ParamPackage) -> AnalogMapping {
        todo!()
    }

    /// Port of Mouse::GetUIName (override)
    pub fn get_ui_name(&self, _params: &ParamPackage) -> ButtonNames {
        todo!()
    }

    // ---- Private methods ----

    /// Port of Mouse::UpdateThread
    fn update_thread(&mut self) {
        todo!()
    }

    /// Port of Mouse::UpdateStickInput
    fn update_stick_input(&mut self) {
        todo!()
    }

    /// Port of Mouse::UpdateMotionInput
    fn update_motion_input(&mut self) {
        todo!()
    }

    /// Port of Mouse::IsMousePanningEnabled
    fn is_mouse_panning_enabled(&self) -> bool {
        todo!()
    }

    /// Port of Mouse::GetUIButtonName
    fn get_ui_button_name(&self, _params: &ParamPackage) -> ButtonNames {
        todo!()
    }
}
