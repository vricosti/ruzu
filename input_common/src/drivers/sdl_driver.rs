// SPDX-FileCopyrightText: 2018 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/sdl_driver.h` and `input_common/drivers/sdl_driver.cpp`.
//!
//! SDL-based input driver for joysticks and game controllers.
//!
//! Note: The C++ implementation is tightly coupled to SDL2's C API
//! (SDL_GameController, SDL_Joystick, SDL_Event, etc.). This port provides
//! the full structural layout and implements all logic that does not require
//! direct SDL FFI calls. The SDL-dependent operations (device enumeration,
//! event handling, controller binding queries, vibration) would require
//! an SDL2 Rust binding crate (e.g., sdl2-rs) to be wired in.

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use parking_lot::Mutex;

use common::input::{ButtonNames, DriverResult, VibrationStatus};
use common::param_package::ParamPackage;
use common::settings_input::{native_button, native_analog, native_motion};
use common::uuid::UUID;

use crate::input_engine::{InputEngine, PadIdentifier, VibrationRequest};
use crate::main_common::{AnalogMapping, ButtonMapping, MotionMapping};

/// SDL HAT direction constants (matching SDL_HAT_* values).
const SDL_HAT_UP: u8 = 0x01;
const SDL_HAT_RIGHT: u8 = 0x02;
const SDL_HAT_DOWN: u8 = 0x04;
const SDL_HAT_LEFT: u8 = 0x08;

/// Port of `SDLDriver` class from sdl_driver.h / sdl_driver.cpp
pub struct SDLDriver {
    engine: InputEngine,
    joystick_map: Mutex<HashMap<UUID, Vec<()>>>,
    start_thread: bool,
    initialized: AtomicBool,
}

impl SDLDriver {
    /// Initializes and registers SDL device factories.
    /// Port of SDLDriver::SDLDriver
    pub fn new(input_engine: String) -> Self {
        log::debug!("SDLDriver initialization started");
        Self {
            engine: InputEngine::new(input_engine),
            joystick_map: Mutex::new(HashMap::new()),
            start_thread: false,
            initialized: AtomicBool::new(false),
        }
    }

    /// Port of SDLDriver::PumpEvents
    pub fn pump_events(&self) {
        // In C++: SDL_PumpEvents() is called.
        // This requires SDL FFI. Stubbed for now.
    }

    /// Handle SDL events for joysticks.
    /// Port of SDLDriver::HandleGameControllerEvent
    pub fn handle_game_controller_event(&mut self, _event: &()) {
        // In C++ this dispatches on the SDL event type:
        // SDL_JOYDEVICEADDED, SDL_JOYBUTTONDOWN, SDL_JOYAXISMOTION, etc.
        // Requires SDL FFI.
    }

    /// Port of SDLDriver::GetInputDevices (override)
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        // In C++ iterates joystick_map and returns ParamPackage per joystick.
        // Requires SDL joystick state.
        Vec::new()
    }

    /// Port of SDLDriver::GetButtonMappingForDevice (override)
    pub fn get_button_mapping_for_device(&self, params: &ParamPackage) -> ButtonMapping {
        if !params.has("guid") || !params.has("port") {
            return ButtonMapping::new();
        }
        // Full implementation requires SDL_GameControllerGetBindForButton.
        // Return empty mapping when SDL is not available.
        ButtonMapping::new()
    }

    /// Port of SDLDriver::GetAnalogMappingForDevice (override)
    pub fn get_analog_mapping_for_device(&self, params: &ParamPackage) -> AnalogMapping {
        if !params.has("guid") || !params.has("port") {
            return AnalogMapping::new();
        }
        // Full implementation requires SDL_GameControllerGetBindForAxis.
        AnalogMapping::new()
    }

    /// Port of SDLDriver::GetMotionMappingForDevice (override)
    pub fn get_motion_mapping_for_device(&self, params: &ParamPackage) -> MotionMapping {
        if !params.has("guid") || !params.has("port") {
            return MotionMapping::new();
        }
        // Full implementation requires SDL game controller motion support.
        MotionMapping::new()
    }

    /// Port of SDLDriver::GetUIName (override)
    pub fn get_ui_name(&self, params: &ParamPackage) -> ButtonNames {
        if params.has("button") {
            // Upstream TODO(German77): Find how to substitute the values for real button names
            return ButtonNames::Value;
        }
        if params.has("hat") {
            return ButtonNames::Value;
        }
        if params.has("axis") {
            return ButtonNames::Value;
        }
        if params.has("axis_x") && params.has("axis_y") && params.has("axis_z") {
            return ButtonNames::Value;
        }
        if params.has("motion") {
            return ButtonNames::Engine;
        }
        ButtonNames::Invalid
    }

    /// Port of SDLDriver::GetHatButtonName (override)
    pub fn get_hat_button_name(&self, direction_value: u8) -> String {
        match direction_value {
            SDL_HAT_UP => "up".to_string(),
            SDL_HAT_DOWN => "down".to_string(),
            SDL_HAT_LEFT => "left".to_string(),
            SDL_HAT_RIGHT => "right".to_string(),
            _ => String::new(),
        }
    }

    /// Port of SDLDriver::GetHatButtonId (override)
    pub fn get_hat_button_id(&self, direction_name: &str) -> u8 {
        match direction_name {
            "up" => SDL_HAT_UP,
            "down" => SDL_HAT_DOWN,
            "left" => SDL_HAT_LEFT,
            "right" => SDL_HAT_RIGHT,
            _ => 0,
        }
    }

    /// Port of SDLDriver::IsStickInverted (override)
    pub fn is_stick_inverted(&self, params: &ParamPackage) -> bool {
        if !params.has("guid") || !params.has("port") {
            return false;
        }
        // Full implementation requires SDL_GameControllerGetBindForAxis to check
        // if axis_x maps to a Y-axis binding and axis_y maps to an X-axis binding.
        // Without SDL FFI we return false.
        false
    }

    /// Port of SDLDriver::SetVibration (override)
    pub fn set_vibration(
        &mut self,
        _identifier: &PadIdentifier,
        _vibration: &VibrationStatus,
    ) -> DriverResult {
        // In C++: queues a VibrationRequest for the vibration thread.
        // Requires SDL_GameControllerRumble.
        DriverResult::NotSupported
    }

    /// Port of SDLDriver::IsVibrationEnabled (override)
    pub fn is_vibration_enabled(&self, _identifier: &PadIdentifier) -> bool {
        // In C++: checks if the joystick supports rumble via SDL_JoystickHasRumble.
        false
    }

    // ---- Private methods ----

    /// Port of SDLDriver::InitJoystick
    fn init_joystick(&mut self, _joystick_index: i32) {
        // Requires SDL_JoystickOpen, SDL_GameControllerOpen, etc.
    }

    /// Port of SDLDriver::CloseJoysticks
    fn close_joysticks(&mut self) {
        let mut map = self.joystick_map.lock();
        map.clear();
    }

    /// Port of SDLDriver::SendVibrations
    fn send_vibrations(&mut self) {
        // In C++: dequeues vibration requests and calls SDL_GameControllerRumble.
    }

    /// Port of SDLDriver::IsButtonOnLeftSide
    fn is_button_on_left_side(&self, button: i32) -> bool {
        matches!(
            button,
            x if x == native_button::Values::DDown as i32
                || x == native_button::Values::DLeft as i32
                || x == native_button::Values::DRight as i32
                || x == native_button::Values::DUp as i32
                || x == native_button::Values::L as i32
                || x == native_button::Values::LStick as i32
                || x == native_button::Values::Minus as i32
                || x == native_button::Values::Screenshot as i32
                || x == native_button::Values::ZL as i32
        )
    }

    /// Helper: build an analog param package for a button.
    /// Port of SDLDriver::BuildAnalogParamPackageForButton
    fn build_analog_param_package_for_button(
        &self,
        port: i32,
        guid: &UUID,
        axis: i32,
        value: f32,
    ) -> ParamPackage {
        let mut params = ParamPackage::default();
        params.set_str("engine", self.engine.get_engine_name().to_string());
        params.set_str("guid", guid.raw_string());
        params.set_int("port", port);
        params.set_int("axis", axis);
        if value > 0.0 {
            params.set_str("direction", "+".to_string());
        } else {
            params.set_str("direction", "-".to_string());
        }
        params.set_float("threshold", 0.5);
        params
    }

    /// Helper: build a button param package.
    /// Port of SDLDriver::BuildButtonParamPackageForButton
    fn build_button_param_package_for_button(
        &self,
        port: i32,
        guid: &UUID,
        button: i32,
    ) -> ParamPackage {
        let mut params = ParamPackage::default();
        params.set_str("engine", self.engine.get_engine_name().to_string());
        params.set_str("guid", guid.raw_string());
        params.set_int("port", port);
        params.set_int("button", button);
        params
    }

    /// Helper: build a hat param package.
    /// Port of SDLDriver::BuildHatParamPackageForButton
    fn build_hat_param_package_for_button(
        &self,
        port: i32,
        guid: &UUID,
        hat: i32,
        value: u8,
    ) -> ParamPackage {
        let mut params = ParamPackage::default();
        params.set_str("engine", self.engine.get_engine_name().to_string());
        params.set_str("guid", guid.raw_string());
        params.set_int("port", port);
        params.set_int("hat", hat);
        params.set_str("direction", self.get_hat_button_name(value));
        params
    }

    /// Helper: build a motion param package.
    /// Port of SDLDriver::BuildMotionParam
    fn build_motion_param(&self, port: i32, guid: &UUID) -> ParamPackage {
        let mut params = ParamPackage::default();
        params.set_str("engine", self.engine.get_engine_name().to_string());
        params.set_str("guid", guid.raw_string());
        params.set_int("port", port);
        params.set_int("motion", 0);
        params
    }

    /// Returns a reference to the inner InputEngine.
    pub fn engine(&self) -> &InputEngine {
        &self.engine
    }

    /// Returns a mutable reference to the inner InputEngine.
    pub fn engine_mut(&mut self) -> &mut InputEngine {
        &mut self.engine
    }
}

impl Drop for SDLDriver {
    fn drop(&mut self) {
        self.close_joysticks();
    }
}
