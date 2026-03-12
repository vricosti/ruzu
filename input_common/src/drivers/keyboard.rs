// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/keyboard.h` and `input_common/drivers/keyboard.cpp`.
//!
//! Keyboard input driver that receives keyboard events and forwards them to button devices.

use common::param_package::ParamPackage;

use crate::input_engine::InputEngine;

/// Port of `Keyboard` class from keyboard.h / keyboard.cpp
pub struct Keyboard {
    engine: InputEngine,
}

impl Keyboard {
    /// Port of Keyboard::Keyboard
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

    /// Sets the status of all buttons bound with the key to pressed.
    /// Port of Keyboard::PressKey
    pub fn press_key(&mut self, _key_code: i32) {
        todo!()
    }

    /// Sets the status of all buttons bound with the key to released.
    /// Port of Keyboard::ReleaseKey
    pub fn release_key(&mut self, _key_code: i32) {
        todo!()
    }

    /// Sets the status of the keyboard key to pressed.
    /// Port of Keyboard::PressKeyboardKey
    pub fn press_keyboard_key(&mut self, _key_index: i32) {
        todo!()
    }

    /// Sets the status of the keyboard key to released.
    /// Port of Keyboard::ReleaseKeyboardKey
    pub fn release_keyboard_key(&mut self, _key_index: i32) {
        todo!()
    }

    /// Sets the status of all keyboard modifier keys.
    /// Port of Keyboard::SetKeyboardModifiers
    pub fn set_keyboard_modifiers(&mut self, _key_modifiers: i32) {
        todo!()
    }

    /// Sets all keys to the non pressed state.
    /// Port of Keyboard::ReleaseAllKeys
    pub fn release_all_keys(&mut self) {
        todo!()
    }

    /// Used for automapping features.
    /// Port of Keyboard::GetInputDevices (override)
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        // Keyboard returns a single device entry
        let mut param = ParamPackage::default();
        param.set_str("engine", self.engine.get_engine_name().to_string());
        param.set_str("display", "Keyboard/Mouse".to_string());
        vec![param]
    }
}
