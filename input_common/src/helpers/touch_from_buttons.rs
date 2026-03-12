// SPDX-FileCopyrightText: 2020 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/touch_from_buttons.h` and `touch_from_buttons.cpp`.
//!
//! A touch device factory that takes a list of button devices and combines
//! them into a touch device.

use std::sync::{Arc, Mutex};

use common::input::{
    self, AnalogProperties, BasicInputDevice, ButtonStatus, CallbackStatus, InputCallback,
    InputDevice, InputType, TouchStatus,
};
use common::param_package::ParamPackage;

/// Default analog properties for touch from buttons.
/// Port of the static constexpr properties in TouchFromButtonDevice.
const TOUCH_PROPERTIES: AnalogProperties = AnalogProperties {
    deadzone: 0.0,
    range: 1.0,
    threshold: 0.5,
    offset: 0.0,
    inverted: false,
    inverted_button: false,
    toggle: false,
};

/// Port of `TouchFromButtonDevice` inner class from touch_from_buttons.cpp.
///
/// Converts button presses into touch events at the specified (x, y) screen position.
struct TouchFromButtonDevice {
    button: Box<dyn InputDevice>,
    last_button_value: Mutex<bool>,
    x: f32,
    y: f32,
    callback: Mutex<InputCallback>,
}

impl TouchFromButtonDevice {
    fn new(button: Box<dyn InputDevice>, x: f32, y: f32) -> Self {
        Self {
            button,
            last_button_value: Mutex::new(false),
            x,
            y,
            callback: Mutex::new(InputCallback { on_change: None }),
        }
    }

    fn get_status(&self, pressed: bool) -> TouchStatus {
        let button_status = ButtonStatus {
            value: pressed,
            ..Default::default()
        };
        let mut status = TouchStatus {
            pressed: button_status,
            x: Default::default(),
            y: Default::default(),
            id: 0,
        };
        status.x.properties = TOUCH_PROPERTIES;
        status.y.properties = TOUCH_PROPERTIES;

        if !pressed {
            return status;
        }

        status.x.raw_value = self.x;
        status.y.raw_value = self.y;
        status
    }
}

impl InputDevice for TouchFromButtonDevice {
    fn force_update(&mut self) {
        self.button.force_update();
    }

    fn set_callback(&mut self, callback: InputCallback) {
        *self.callback.lock().unwrap() = callback;
    }

    fn trigger_on_change(&self, status: &CallbackStatus) {
        let cb = self.callback.lock().unwrap();
        if let Some(ref on_change) = cb.on_change {
            on_change(status);
        }
    }
}

/// Port of `TouchFromButton` class from touch_from_buttons.h / touch_from_buttons.cpp
///
/// Creates a touch device from a button device with associated screen coordinates.
/// The inner TouchFromButtonDevice converts button presses into touch events at
/// the specified (x, y) screen position (normalized to 1280x720 resolution).
pub struct TouchFromButton;

impl TouchFromButton {
    pub fn new() -> Self {
        Self
    }

    /// Port of TouchFromButton::Create (override)
    ///
    /// Creates the inner TouchFromButtonDevice with:
    /// - "button": serialized ParamPackage for creating the button device
    /// - "x": screen x coordinate (normalized against 1280)
    /// - "y": screen y coordinate (normalized against 720)
    pub fn create(&self, params: &ParamPackage) -> Box<dyn InputDevice> {
        let null_engine = {
            let mut p = ParamPackage::default();
            p.set_str("engine", "null".to_string());
            p.serialize()
        };
        let button =
            input::create_input_device_from_string(&params.get_str("button", &null_engine));
        let x = params.get_float("x", 0.0) / 1280.0;
        let y = params.get_float("y", 0.0) / 720.0;
        Box::new(TouchFromButtonDevice::new(button, x, y))
    }
}

impl Default for TouchFromButton {
    fn default() -> Self {
        Self::new()
    }
}
