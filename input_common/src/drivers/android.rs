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

use crate::input_engine::{BasicMotion, InputEngine, PadIdentifier, VibrationRequest};
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
    pub fn set_button_state(&mut self, guid: &str, port: usize, button_id: i32, value: bool) {
        let identifier = self.get_identifier(guid, port);
        self.engine.set_button(&identifier, button_id, value);
    }

    /// Sets the status of an axis on a specific controller.
    /// Port of Android::SetAxisPosition
    pub fn set_axis_position(&mut self, guid: &str, port: usize, axis_id: i32, value: f32) {
        let identifier = self.get_identifier(guid, port);
        self.engine.set_axis(&identifier, axis_id, value);
    }

    /// Sets the status of the motion sensor on a specific controller.
    /// Port of Android::SetMotionState
    pub fn set_motion_state(
        &mut self,
        guid: &str,
        port: usize,
        delta_timestamp: u64,
        gyro_x: f32,
        gyro_y: f32,
        gyro_z: f32,
        accel_x: f32,
        accel_y: f32,
        accel_z: f32,
    ) {
        let identifier = self.get_identifier(guid, port);
        let motion_data = BasicMotion {
            gyro_x,
            gyro_y,
            gyro_z,
            accel_x,
            accel_y,
            accel_z,
            delta_timestamp,
        };
        self.engine.set_motion(&identifier, 0, &motion_data);
    }

    /// Port of Android::SetVibration (override)
    /// Android vibration requires JNI; returns Success as a no-op on non-Android platforms.
    pub fn set_vibration(
        &mut self,
        _identifier: &PadIdentifier,
        _vibration: &VibrationStatus,
    ) -> DriverResult {
        // JNI vibration is platform-specific; no-op here
        DriverResult::Success
    }

    /// Port of Android::IsVibrationEnabled (override)
    /// Returns false on non-Android platforms.
    pub fn is_vibration_enabled(&self, _identifier: &PadIdentifier) -> bool {
        false
    }

    /// Port of Android::GetInputDevices (override)
    /// Returns empty on non-Android platforms.
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        Vec::new()
    }

    /// Port of Android::GetAnalogMappingForDevice (override).
    /// Upstream uses Android JNI to query device axes — excluded from port per CLAUDE.md
    /// (Android JNI integration is platform-specific with no Rust equivalent).
    pub fn get_analog_mapping_for_device(&self, _params: &ParamPackage) -> AnalogMapping {
        HashMap::new()
    }

    /// Port of Android::GetButtonMappingForDevice (override).
    /// Upstream uses Android JNI to query device keys — excluded from port per CLAUDE.md.
    pub fn get_button_mapping_for_device(&self, _params: &ParamPackage) -> ButtonMapping {
        HashMap::new()
    }

    /// Port of Android::GetUIName (override)
    pub fn get_ui_name(&self, _params: &ParamPackage) -> ButtonNames {
        ButtonNames::Undefined
    }

    // ---- Private methods ----

    /// Returns the correct identifier corresponding to the player index.
    /// Port of Android::GetIdentifier
    fn get_identifier(&self, guid: &str, port: usize) -> PadIdentifier {
        PadIdentifier {
            guid: UUID::from_string(guid),
            port,
            pad: 0,
        }
    }
}
