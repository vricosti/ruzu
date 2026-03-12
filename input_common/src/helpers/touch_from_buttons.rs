// SPDX-FileCopyrightText: 2020 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/touch_from_buttons.h` and `touch_from_buttons.cpp`.
//!
//! A touch device factory that takes a list of button devices and combines
//! them into a touch device.

use common::input::InputDevice;
use common::param_package::ParamPackage;

/// Port of `TouchFromButton` class from touch_from_buttons.h / touch_from_buttons.cpp
///
/// Creates a touch device from a list of button devices.
pub struct TouchFromButton;

impl TouchFromButton {
    pub fn new() -> Self {
        Self
    }

    /// Port of TouchFromButton::Create (override)
    pub fn create(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }
}

impl Default for TouchFromButton {
    fn default() -> Self {
        Self::new()
    }
}
