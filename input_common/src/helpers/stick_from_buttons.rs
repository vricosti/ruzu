// SPDX-FileCopyrightText: 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/stick_from_buttons.h` and `stick_from_buttons.cpp`.
//!
//! An analog device factory that takes direction button devices and combines
//! them into an analog device.

use std::sync::Mutex;
use std::time::Instant;

use common::input::{
    self, AnalogProperties, AnalogStatus, CallbackStatus, InputCallback, InputDevice, InputType,
    StickStatus,
};
use common::param_package::ParamPackage;

/// Some games such as EARTH DEFENSE FORCE: WORLD BROTHERS
/// do not play nicely with the theoretical maximum range.
/// Using a value one lower from the maximum emulates real stick behavior.
pub const MAX_RANGE: f32 = 32766.0 / 32767.0;
pub const TAU: f32 = common::math_util::PI * 2.0;
/// Use wider angle to ease the transition.
pub const APERTURE: f32 = TAU * 0.15;

/// Default analog properties for stick from buttons.
const STICK_PROPERTIES: AnalogProperties = AnalogProperties {
    deadzone: 0.0,
    range: 1.0,
    threshold: 0.5,
    offset: 0.0,
    inverted: false,
    inverted_button: false,
    toggle: false,
};

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

/// Port of the inner `Stick` class from stick_from_buttons.cpp.
///
/// Handles angle interpolation for smooth diagonal transitions.
struct Stick {
    up: Box<dyn InputDevice>,
    down: Box<dyn InputDevice>,
    left: Box<dyn InputDevice>,
    right: Box<dyn InputDevice>,
    modifier: Box<dyn InputDevice>,
    updater: Box<dyn InputDevice>,
    modifier_scale: f32,
    modifier_angle: f32,
    angle: f32,
    goal_angle: f32,
    amplitude: f32,
    up_status: bool,
    down_status: bool,
    left_status: bool,
    right_status: bool,
    last_x_axis_value: f32,
    last_y_axis_value: f32,
    modifier_value: bool,
    modifier_toggle: bool,
    modifier_locked: bool,
    last_update: Instant,
    callback: Mutex<InputCallback>,
}

impl Stick {
    fn new(
        up: Box<dyn InputDevice>,
        down: Box<dyn InputDevice>,
        left: Box<dyn InputDevice>,
        right: Box<dyn InputDevice>,
        modifier: Box<dyn InputDevice>,
        updater: Box<dyn InputDevice>,
        modifier_scale: f32,
        modifier_angle: f32,
    ) -> Self {
        Self {
            up,
            down,
            left,
            right,
            modifier,
            updater,
            modifier_scale,
            modifier_angle,
            angle: 0.0,
            goal_angle: 0.0,
            amplitude: 0.0,
            up_status: false,
            down_status: false,
            left_status: false,
            right_status: false,
            last_x_axis_value: 0.0,
            last_y_axis_value: 0.0,
            modifier_value: false,
            modifier_toggle: false,
            modifier_locked: false,
            last_update: Instant::now(),
            callback: Mutex::new(InputCallback { on_change: None }),
        }
    }

    fn get_angle(&self, now: Instant) -> f32 {
        let mut new_angle = self.angle;

        let mut time_difference = now
            .duration_since(self.last_update)
            .as_secs_f32();
        if time_difference > 0.5 {
            time_difference = 0.5;
        }

        if is_angle_greater(new_angle, self.goal_angle) {
            new_angle -= self.modifier_angle * time_difference;
            if new_angle < 0.0 {
                new_angle += TAU;
            }
            if !is_angle_greater(new_angle, self.goal_angle) {
                return self.goal_angle;
            }
        } else if is_angle_smaller(new_angle, self.goal_angle) {
            new_angle += self.modifier_angle * time_difference;
            if new_angle >= TAU {
                new_angle -= TAU;
            }
            if !is_angle_smaller(new_angle, self.goal_angle) {
                return self.goal_angle;
            }
        } else {
            return self.goal_angle;
        }
        new_angle
    }

    fn set_goal_angle(&mut self, r: bool, l: bool, u: bool, d: bool) {
        self.goal_angle = compute_goal_angle(r, l, u, d);
    }

    /// Upstream: `StickFromButtonDevice::GetStatus()` (stick_from_buttons.cpp:267-283).
    fn get_status(&self) -> StickStatus {
        let mut status = StickStatus::default();
        status.x.properties = STICK_PROPERTIES;
        status.y.properties = STICK_PROPERTIES;

        if *common::settings::values().emulate_analog_keyboard.get_value() {
            let now = std::time::Instant::now();
            let angle = self.get_angle(now);
            status.x.raw_value = angle.cos() * self.amplitude;
            status.y.raw_value = angle.sin() * self.amplitude;
            return status;
        }

        status.x.raw_value = self.goal_angle.cos() * self.amplitude;
        status.y.raw_value = self.goal_angle.sin() * self.amplitude;
        status
    }

    fn update_status(&mut self) {
        let mut r = self.right_status;
        let mut l = self.left_status;
        let mut u = self.up_status;
        let mut d = self.down_status;

        // Eliminate contradictory movements
        if r && l {
            r = false;
            l = false;
        }
        if u && d {
            u = false;
            d = false;
        }

        // Move if a key is pressed
        if r || l || u || d {
            self.amplitude = if self.modifier_value {
                self.modifier_scale
            } else {
                MAX_RANGE
            };
        } else {
            self.amplitude = 0.0;
        }

        let now = Instant::now();
        let time_difference = now
            .duration_since(self.last_update)
            .as_millis() as u64;

        if time_difference < 10 {
            // Disable analog mode if inputs are too fast
            self.set_goal_angle(r, l, u, d);
            self.angle = self.goal_angle;
        } else {
            self.angle = self.get_angle(now);
            self.set_goal_angle(r, l, u, d);
        }

        self.last_update = now;
        let stick_status = self.get_status();
        self.last_x_axis_value = stick_status.x.raw_value;
        self.last_y_axis_value = stick_status.y.raw_value;
        let status = CallbackStatus {
            input_type: InputType::Stick,
            stick_status,
            ..Default::default()
        };
        self.trigger_on_change(&status);
    }
}

impl InputDevice for Stick {
    fn force_update(&mut self) {
        self.up.force_update();
        self.down.force_update();
        self.left.force_update();
        self.right.force_update();
        self.modifier.force_update();
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
    pub fn create(&self, params: &ParamPackage) -> Box<dyn InputDevice> {
        let null_engine = {
            let mut p = ParamPackage::default();
            p.set_str("engine", "null".to_string());
            p.serialize()
        };
        let up = input::create_input_device_from_string(&params.get_str("up", &null_engine));
        let down = input::create_input_device_from_string(&params.get_str("down", &null_engine));
        let left = input::create_input_device_from_string(&params.get_str("left", &null_engine));
        let right = input::create_input_device_from_string(&params.get_str("right", &null_engine));
        let modifier =
            input::create_input_device_from_string(&params.get_str("modifier", &null_engine));
        let updater = input::create_input_device_from_string("engine:updater,button:0");
        let modifier_scale = params.get_float("modifier_scale", 0.5);
        let modifier_angle = params.get_float("modifier_angle", 5.5);

        Box::new(Stick::new(
            up,
            down,
            left,
            right,
            modifier,
            updater,
            modifier_scale,
            modifier_angle,
        ))
    }
}

impl Default for StickFromButton {
    fn default() -> Self {
        Self::new()
    }
}
