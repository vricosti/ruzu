// SPDX-FileCopyrightText: 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/stick_from_buttons.h` and `stick_from_buttons.cpp`.
//!
//! An analog device factory that takes direction button devices and combines
//! them into an analog device.

use common::input::InputDevice;
use common::param_package::ParamPackage;

/// Some games such as EARTH DEFENSE FORCE: WORLD BROTHERS
/// do not play nicely with the theoretical maximum range.
/// Using a value one lower from the maximum emulates real stick behavior.
pub const MAX_RANGE: f32 = 32766.0 / 32767.0;
pub const TAU: f32 = common::math_util::PI * 2.0;
/// Use wider angle to ease the transition.
pub const APERTURE: f32 = TAU * 0.15;

/// Default analog properties for stick from buttons.
pub struct StickAnalogProperties {
    pub deadzone: f32,
    pub range: f32,
    pub threshold: f32,
    pub offset: f32,
    pub inverted: bool,
    pub toggle: bool,
}

impl Default for StickAnalogProperties {
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

/// Returns whether old_angle is greater than new_angle within aperture.
/// Port of Stick::IsAngleGreater
pub fn is_angle_greater(old_angle: f32, new_angle: f32) -> bool {
    let top_limit = new_angle + APERTURE;
    (old_angle > new_angle && old_angle <= top_limit)
        || (old_angle + TAU > new_angle && old_angle + TAU <= top_limit)
}

/// Returns whether old_angle is smaller than new_angle within aperture.
/// Port of Stick::IsAngleSmaller
pub fn is_angle_smaller(old_angle: f32, new_angle: f32) -> bool {
    let bottom_limit = new_angle - APERTURE;
    (old_angle >= bottom_limit && old_angle < new_angle)
        || (old_angle - TAU >= bottom_limit && old_angle - TAU < new_angle)
}

/// Sets goal_angle based on button directions.
/// Port of Stick::SetGoalAngle
pub fn compute_goal_angle(r: bool, l: bool, u: bool, d: bool) -> f32 {
    let pi = common::math_util::PI;

    // Move to the right
    if r && !u && !d {
        return 0.0;
    }
    // Move to the upper right
    if r && u && !d {
        return pi * 0.25;
    }
    // Move up
    if u && !l && !r {
        return pi * 0.5;
    }
    // Move to the upper left
    if l && u && !d {
        return pi * 0.75;
    }
    // Move to the left
    if l && !u && !d {
        return pi;
    }
    // Move to the bottom left
    if l && !u && d {
        return pi * 1.25;
    }
    // Move down
    if d && !l && !r {
        return pi * 1.5;
    }
    // Move to the bottom right
    if r && !u && d {
        return pi * 1.75;
    }

    // Default: no change
    0.0
}

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
    ///
    /// Creates the inner Stick device with up/down/left/right/modifier buttons.
    /// The Stick device handles angle interpolation for smooth diagonal transitions,
    /// modifier for reduced-range movement, and callback-based input forwarding.
    pub fn create(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        // Full implementation requires:
        // 1. Common::Input::CreateInputDeviceFromString for each direction button
        // 2. Setting up callbacks on each button device
        // 3. Creating the inner Stick device with angle-interpolation state machine
        // These depend on the input device factory registration system
        todo!("Requires input device factory registration system")
    }
}

impl Default for StickFromButton {
    fn default() -> Self {
        Self::new()
    }
}
