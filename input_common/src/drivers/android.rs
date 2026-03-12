// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `input_common/drivers/android.h` and `input_common/drivers/android.cpp`.
//!
//! Android input driver for receiving input events from the Android frontend.
//! Note: This driver is platform-specific to Android and uses JNI in the C++ version.
//! The Rust port provides structural parity but JNI integration is not included.

use std::collections::HashMap;

use common::input::{ButtonNames, DriverResult, VibrationStatus};
use common::param_package::ParamPackage;
use common::uuid::UUID;

use crate::input_engine::{InputEngine, PadIdentifier, VibrationRequest};
use crate::main_common::{AnalogMapping, ButtonMapping};

// ---- Constants from android.h ----

const AXIS_X: i32 = 0;
const AXIS_Y: i32 = 1;
const AXIS_Z: i32 = 11;
const AXIS_RX: i32 = 12;
const AXIS_RY: i32 = 13;
const AXIS_RZ: i32 = 14;
const AXIS_HAT_X: i32 = 15;
const AXIS_HAT_Y: i32 = 16;
const AXIS_LTRIGGER: i32 = 17;
const AXIS_RTRIGGER: i32 = 18;

const KEYCODE_DPAD_UP: i32 = 19;
const KEYCODE_DPAD_DOWN: i32 = 20;
const KEYCODE_DPAD_LEFT: i32 = 21;
const KEYCODE_DPAD_RIGHT: i32 = 22;
const KEYCODE_BUTTON_A: i32 = 96;
const KEYCODE_BUTTON_B: i32 = 97;
const KEYCODE_BUTTON_X: i32 = 99;
const KEYCODE_BUTTON_Y: i32 = 100;
const KEYCODE_BUTTON_L1: i32 = 102;
const KEYCODE_BUTTON_R1: i32 = 103;
const KEYCODE_BUTTON_L2: i32 = 104;
const KEYCODE_BUTTON_R2: i32 = 105;
const KEYCODE_BUTTON_THUMBL: i32 = 106;
const KEYCODE_BUTTON_THUMBR: i32 = 107;
const KEYCODE_BUTTON_START: i32 = 108;
const KEYCODE_BUTTON_SELECT: i32 = 109;

/// Port of `Android` class from android.h / android.cpp
pub struct Android {
    engine: InputEngine,
    // input_devices would map PadIdentifier -> jobject in C++; omitted for non-Android platforms
}

impl Android {
    /// Port of Android::Android
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

    /// Sets the status of a button on a specific controller.
    /// Port of Android::SetButtonState
    pub fn set_button_state(
        &mut self,
        _guid: &str,
        _port: usize,
        _button_id: i32,
        _value: bool,
    ) {
        todo!()
    }

    /// Sets the status of an axis on a specific controller.
    /// Port of Android::SetAxisPosition
    pub fn set_axis_position(
        &mut self,
        _guid: &str,
        _port: usize,
        _axis_id: i32,
        _value: f32,
    ) {
        todo!()
    }

    /// Sets the status of the motion sensor on a specific controller.
    /// Port of Android::SetMotionState
    pub fn set_motion_state(
        &mut self,
        _guid: &str,
        _port: usize,
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

    /// Port of Android::SetVibration (override)
    pub fn set_vibration(
        &mut self,
        _identifier: &PadIdentifier,
        _vibration: &VibrationStatus,
    ) -> DriverResult {
        todo!()
    }

    /// Port of Android::IsVibrationEnabled (override)
    pub fn is_vibration_enabled(&self, _identifier: &PadIdentifier) -> bool {
        todo!()
    }

    /// Port of Android::GetInputDevices (override)
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        todo!()
    }

    /// Port of Android::GetAnalogMappingForDevice (override)
    pub fn get_analog_mapping_for_device(&self, _params: &ParamPackage) -> AnalogMapping {
        todo!()
    }

    /// Port of Android::GetButtonMappingForDevice (override)
    pub fn get_button_mapping_for_device(&self, _params: &ParamPackage) -> ButtonMapping {
        todo!()
    }

    /// Port of Android::GetUIName (override)
    pub fn get_ui_name(&self, _params: &ParamPackage) -> ButtonNames {
        todo!()
    }

    // ---- Private methods ----

    /// Returns the correct identifier corresponding to the player index.
    /// Port of Android::GetIdentifier
    fn get_identifier(&self, _guid: &str, _port: usize) -> PadIdentifier {
        todo!()
    }
}
