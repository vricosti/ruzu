// SPDX-FileCopyrightText: 2018 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/sdl_driver.h` and `input_common/drivers/sdl_driver.cpp`.
//!
//! SDL-based input driver for joysticks and game controllers.

use std::collections::HashMap;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use parking_lot::Mutex;

use common::input::{ButtonNames, DriverResult, VibrationStatus};
use common::param_package::ParamPackage;
use common::uuid::UUID;

use crate::input_engine::{InputEngine, PadIdentifier, VibrationRequest};
use crate::main_common::{AnalogMapping, ButtonMapping, MotionMapping};

/// Port of `SDLDriver` class from sdl_driver.h / sdl_driver.cpp
pub struct SDLDriver {
    engine: InputEngine,
    joystick_map: HashMap<UUID, Vec<()>>, // Simplified - would be Vec<Arc<SDLJoystick>>
    start_thread: bool,
    initialized: AtomicBool,
}

impl SDLDriver {
    /// Initializes and registers SDL device factories.
    /// Port of SDLDriver::SDLDriver
    pub fn new(input_engine: String) -> Self {
        Self {
            engine: InputEngine::new(input_engine),
            joystick_map: HashMap::new(),
            start_thread: false,
            initialized: AtomicBool::new(false),
        }
    }

    /// Port of SDLDriver::PumpEvents
    pub fn pump_events(&self) {
        todo!()
    }

    /// Handle SDL events for joysticks.
    /// Port of SDLDriver::HandleGameControllerEvent
    pub fn handle_game_controller_event(&mut self, _event: &()) {
        todo!()
    }

    /// Port of SDLDriver::GetInputDevices (override)
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        todo!()
    }

    /// Port of SDLDriver::GetButtonMappingForDevice (override)
    pub fn get_button_mapping_for_device(&self, _params: &ParamPackage) -> ButtonMapping {
        todo!()
    }

    /// Port of SDLDriver::GetAnalogMappingForDevice (override)
    pub fn get_analog_mapping_for_device(&self, _params: &ParamPackage) -> AnalogMapping {
        todo!()
    }

    /// Port of SDLDriver::GetMotionMappingForDevice (override)
    pub fn get_motion_mapping_for_device(&self, _params: &ParamPackage) -> MotionMapping {
        todo!()
    }

    /// Port of SDLDriver::GetUIName (override)
    pub fn get_ui_name(&self, _params: &ParamPackage) -> ButtonNames {
        todo!()
    }

    /// Port of SDLDriver::GetHatButtonName (override)
    pub fn get_hat_button_name(&self, _direction_value: u8) -> String {
        todo!()
    }

    /// Port of SDLDriver::GetHatButtonId (override)
    pub fn get_hat_button_id(&self, _direction_name: &str) -> u8 {
        todo!()
    }

    /// Port of SDLDriver::IsStickInverted (override)
    pub fn is_stick_inverted(&self, _params: &ParamPackage) -> bool {
        todo!()
    }

    /// Port of SDLDriver::SetVibration (override)
    pub fn set_vibration(
        &mut self,
        _identifier: &PadIdentifier,
        _vibration: &VibrationStatus,
    ) -> DriverResult {
        todo!()
    }

    /// Port of SDLDriver::IsVibrationEnabled (override)
    pub fn is_vibration_enabled(&self, _identifier: &PadIdentifier) -> bool {
        todo!()
    }

    // ---- Private methods ----

    fn init_joystick(&mut self, _joystick_index: i32) {
        todo!()
    }

    fn close_joysticks(&mut self) {
        todo!()
    }

    fn send_vibrations(&mut self) {
        todo!()
    }

    fn is_button_on_left_side(&self, _button: i32) -> bool {
        todo!()
    }
}
