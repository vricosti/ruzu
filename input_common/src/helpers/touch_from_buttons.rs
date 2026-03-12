// SPDX-FileCopyrightText: 2020 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/touch_from_buttons.h` and `touch_from_buttons.cpp`.
//!
//! A touch device factory that takes a list of button devices and combines
//! them into a touch device.

use common::input::InputDevice;
use common::param_package::ParamPackage;

/// Default analog properties for touch from buttons.
pub struct TouchAnalogProperties {
    pub deadzone: f32,
    pub range: f32,
    pub threshold: f32,
    pub offset: f32,
    pub inverted: bool,
    pub toggle: bool,
}

impl Default for TouchAnalogProperties {
    fn default() -> Self {
        Self {
            deadzone: 0.0,
            range: 1.0,
            threshold: 0.5,
            offset: 0.0,
            inverted: false,
            toggle: false,
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
    pub fn create(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        // Full implementation requires:
        // 1. Common::Input::CreateInputDeviceFromString for the button
        // 2. Setting up a callback on the button device
        // 3. Creating the inner TouchFromButtonDevice with (x/1280, y/720) coordinates
        // These depend on the input device factory registration system
        todo!("Requires input device factory registration system")
    }
}

impl Default for TouchFromButton {
    fn default() -> Self {
        Self::new()
    }
}
