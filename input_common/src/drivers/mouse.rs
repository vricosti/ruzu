// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/mouse.h` and `input_common/drivers/mouse.cpp`.
//!
//! Mouse input driver that receives mouse events and forwards them to input devices.

use common::input::ButtonNames;
use common::param_package::ParamPackage;

use crate::input_engine::{InputEngine, PadIdentifier};
use crate::main_common::AnalogMapping;

const UPDATE_TIME: i32 = 10;
const DEFAULT_PANNING_SENSITIVITY: f32 = 0.0010;
const DEFAULT_STICK_SENSITIVITY: f32 = 0.0006;
const DEFAULT_DEADZONE_COUNTERWEIGHT: f32 = 0.01;
const DEFAULT_MOTION_PANNING_SENSITIVITY: f32 = 2.5;
const DEFAULT_MOTION_SENSITIVITY: f32 = 0.416;
const MAXIMUM_ROTATION_SPEED: f32 = 2.0;
const MAXIMUM_STICK_RANGE: f32 = 1.5;
const MOUSE_AXIS_X: i32 = 0;
const MOUSE_AXIS_Y: i32 = 1;
const WHEEL_AXIS_X: i32 = 2;
const WHEEL_AXIS_Y: i32 = 3;

fn identifier() -> PadIdentifier {
    PadIdentifier {
        guid: Default::default(),
        port: 0,
        pad: 0,
    }
}

fn motion_identifier() -> PadIdentifier {
    PadIdentifier {
        guid: Default::default(),
        port: 0,
        pad: 1,
    }
}

fn real_mouse_identifier() -> PadIdentifier {
    PadIdentifier {
        guid: Default::default(),
        port: 1,
        pad: 0,
    }
}

fn touch_identifier() -> PadIdentifier {
    PadIdentifier {
        guid: Default::default(),
        port: 2,
        pad: 0,
    }
}

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
        let mut m = Self {
            engine: InputEngine::new(input_engine),
            mouse_origin: (0, 0),
            last_mouse_position: (0, 0),
            last_mouse_change: (0.0, 0.0),
            last_motion_change: (0.0, 0.0, 0.0),
            wheel_position: (0, 0),
            button_pressed: false,
        };
        // PreSetController for all identifiers
        // PreSetAxis for all mouse axes
        // Note: update_thread would need a separate spawned thread for stick/motion decay
        m
    }

    /// Returns a reference to the underlying input engine.
    pub fn engine(&self) -> &InputEngine {
        &self.engine
    }

    /// Returns a mutable reference to the underlying input engine.
    pub fn engine_mut(&mut self) -> &mut InputEngine {
        &mut self.engine
    }

    /// Signals that real mouse has moved.
    /// Port of Mouse::MouseMove
    pub fn mouse_move(&mut self, touch_x: f32, touch_y: f32) {
        let id = real_mouse_identifier();
        self.engine.set_axis(&id, MOUSE_AXIS_X, touch_x);
        self.engine.set_axis(&id, MOUSE_AXIS_Y, touch_y);
    }

    /// Signals that touch finger has moved.
    /// Port of Mouse::TouchMove
    pub fn touch_move(&mut self, touch_x: f32, touch_y: f32) {
        let id = touch_identifier();
        self.engine.set_axis(&id, MOUSE_AXIS_X, touch_x);
        self.engine.set_axis(&id, MOUSE_AXIS_Y, touch_y);
    }

    /// Sets the status of a button to pressed.
    /// Port of Mouse::PressButton
    pub fn press_button(&mut self, x: i32, y: i32, button: MouseButton) {
        let id = identifier();
        self.engine
            .set_button(&id, button as i32, true);

        // Set initial analog parameters
        self.mouse_origin = (x, y);
        self.last_mouse_position = (x, y);
        self.button_pressed = true;
    }

    /// Sets the status of a mouse button to pressed.
    /// Port of Mouse::PressMouseButton
    pub fn press_mouse_button(&mut self, button: MouseButton) {
        let id = real_mouse_identifier();
        self.engine
            .set_button(&id, button as i32, true);
    }

    /// Sets the status of touch finger to pressed.
    /// Port of Mouse::PressTouchButton
    pub fn press_touch_button(&mut self, touch_x: f32, touch_y: f32, button: MouseButton) {
        let id = touch_identifier();
        self.engine.set_axis(&id, MOUSE_AXIS_X, touch_x);
        self.engine.set_axis(&id, MOUSE_AXIS_Y, touch_y);
        self.engine
            .set_button(&id, button as i32, true);
    }

    /// Sets the status of all buttons bound with the key to released.
    /// Port of Mouse::ReleaseButton
    pub fn release_button(&mut self, button: MouseButton) {
        let id = identifier();
        let real_id = real_mouse_identifier();
        let touch_id = touch_identifier();

        self.engine
            .set_button(&id, button as i32, false);
        self.engine
            .set_button(&real_id, button as i32, false);
        self.engine
            .set_button(&touch_id, button as i32, false);

        if !self.is_mouse_panning_enabled() {
            self.engine.set_axis(&id, MOUSE_AXIS_X, 0.0);
            self.engine.set_axis(&id, MOUSE_AXIS_Y, 0.0);
        }

        self.last_motion_change.0 = 0.0;
        self.last_motion_change.1 = 0.0;

        self.button_pressed = false;
    }

    /// Sets the status of the mouse wheel.
    /// Port of Mouse::MouseWheelChange
    pub fn mouse_wheel_change(&mut self, x: i32, y: i32) {
        self.wheel_position.0 += x;
        self.wheel_position.1 += y;
        self.last_motion_change.2 += y as f32;
        let id = identifier();
        self.engine
            .set_axis(&id, WHEEL_AXIS_X, self.wheel_position.0 as f32);
        self.engine
            .set_axis(&id, WHEEL_AXIS_Y, self.wheel_position.1 as f32);
    }

    /// Port of Mouse::ReleaseAllButtons
    pub fn release_all_buttons(&mut self) {
        self.engine.reset_button_state();
        self.button_pressed = false;
    }

    /// Port of Mouse::GetInputDevices (override)
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        let mut param = ParamPackage::default();
        param.set_str("engine", self.engine.get_engine_name().to_string());
        param.set_str("display", "Keyboard/Mouse".to_string());
        vec![param]
    }

    /// Port of Mouse::GetAnalogMappingForDevice (override)
    pub fn get_analog_mapping_for_device(&self, _params: &ParamPackage) -> AnalogMapping {
        // Only overwrite different buttons from default
        let mut mapping = AnalogMapping::new();
        let mut right_analog_params = ParamPackage::default();
        right_analog_params.set_str("engine", self.engine.get_engine_name().to_string());
        right_analog_params.set_int("axis_x", 0);
        right_analog_params.set_int("axis_y", 1);
        right_analog_params.set_float("threshold", 0.5);
        right_analog_params.set_float("range", 1.0);
        right_analog_params.set_float("deadzone", 0.0);
        // Settings::NativeAnalog::RStick == 1
        mapping.insert(1, right_analog_params);
        mapping
    }

    /// Port of Mouse::GetUIName (override)
    pub fn get_ui_name(&self, params: &ParamPackage) -> ButtonNames {
        if params.has("button") {
            return self.get_ui_button_name(params);
        }
        if params.has("axis") {
            return ButtonNames::Value;
        }
        if params.has("axis_x") && params.has("axis_y") && params.has("axis_z") {
            return ButtonNames::Engine;
        }
        if params.has("motion") {
            return ButtonNames::Engine;
        }
        ButtonNames::Invalid
    }

    // ---- Private methods ----

    /// Port of Mouse::IsMousePanningEnabled
    fn is_mouse_panning_enabled(&self) -> bool {
        // Disable mouse panning when a real mouse is connected
        // Upstream: Settings::values.mouse_panning && !Settings::values.mouse_enabled
        // For now, default to false since settings wiring is not connected
        false
    }

    /// Port of Mouse::GetUIButtonName
    fn get_ui_button_name(&self, params: &ParamPackage) -> ButtonNames {
        let button_value = params.get_int("button", 0);
        // Match MouseButton enum order
        match button_value {
            0 => ButtonNames::ButtonLeft,
            1 => ButtonNames::ButtonRight,
            2 => ButtonNames::ButtonMouseWheel,
            3 => ButtonNames::ButtonBackward,
            4 => ButtonNames::ButtonForward,
            5 => ButtonNames::ButtonTask,
            6 => ButtonNames::ButtonExtra,
            _ => ButtonNames::Undefined,
        }
    }
}
