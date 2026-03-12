// SPDX-FileCopyrightText: 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/stick_from_buttons.h` and `stick_from_buttons.cpp`.
//!
//! An analog device factory that takes direction button devices and combines
//! them into an analog device.

use common::input::InputDevice;
use common::param_package::ParamPackage;

/// Port of `StickFromButton` class from stick_from_buttons.h / stick_from_buttons.cpp
///
/// Creates an analog device from direction button devices.
/// Parameters:
///   - "up": serialized ParamPackage for creating a button device for up direction
///   - "down": serialized ParamPackage for creating a button device for down direction
///   - "left": serialized ParamPackage for creating a button device for left direction
///   - "right": serialized ParamPackage for creating a button device for right direction
///   - "modifier": serialized ParamPackage for creating a button device as the modifier
///   - "modifier_scale": a float for the multiplier the modifier gives to the position
pub struct StickFromButton;

impl StickFromButton {
    pub fn new() -> Self {
        Self
    }

    /// Port of StickFromButton::Create (override)
    pub fn create(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }
}

impl Default for StickFromButton {
    fn default() -> Self {
        Self::new()
    }
}
