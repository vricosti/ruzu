// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/keyboard.h` and `input_common/drivers/keyboard.cpp`.
//!
//! Keyboard input driver that receives keyboard events and forwards them to button devices.

use common::param_package::ParamPackage;
use common::settings_input::native_keyboard;

use crate::input_engine::{InputEngine, PadIdentifier};

/// Unfiltered key identifier intended for controllers.
fn key_identifier() -> PadIdentifier {
    PadIdentifier {
        guid: Default::default(),
        port: 0,
        pad: 0,
    }
}

/// Keyboard key identifier. Allows only NativeKeyboard::Keys intended for keyboard emulation.
fn keyboard_key_identifier() -> PadIdentifier {
    PadIdentifier {
        guid: Default::default(),
        port: 1,
        pad: 0,
    }
}

/// Keyboard modifier identifier. Allows only NativeKeyboard::Modifiers intended for keyboard emulation.
fn keyboard_modifier_identifier() -> PadIdentifier {
    PadIdentifier {
        guid: Default::default(),
        port: 1,
        pad: 1,
    }
}

/// Port of `Keyboard` class from keyboard.h / keyboard.cpp
pub struct Keyboard {
    engine: InputEngine,
}

impl Keyboard {
    /// Port of Keyboard::Keyboard
    ///
    /// Keyboard is broken into 3 different sets:
    /// - key: Unfiltered intended for controllers.
    /// - keyboard_key: Allows only Settings::NativeKeyboard::Keys intended for keyboard emulation.
    /// - keyboard_modifier: Allows only Settings::NativeKeyboard::Modifiers intended for keyboard emulation.
    pub fn new(input_engine: String) -> Self {
        let mut kb = Self {
            engine: InputEngine::new(input_engine),
        };
        // PreSetController for the three identifier sets
        kb.engine.pre_set_controller(&key_identifier());
        kb.engine.pre_set_controller(&keyboard_key_identifier());
        kb.engine
            .pre_set_controller(&keyboard_modifier_identifier());
        kb
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
    pub fn press_key(&mut self, key_code: i32) {
        self.engine
            .set_button(&key_identifier(), key_code, true);
    }

    /// Sets the status of all buttons bound with the key to released.
    /// Port of Keyboard::ReleaseKey
    pub fn release_key(&mut self, key_code: i32) {
        self.engine
            .set_button(&key_identifier(), key_code, false);
    }

    /// Sets the status of the keyboard key to pressed.
    /// Port of Keyboard::PressKeyboardKey
    pub fn press_keyboard_key(&mut self, key_index: i32) {
        if key_index == native_keyboard::Keys::None as i32 {
            return;
        }
        self.engine
            .set_button(&keyboard_key_identifier(), key_index, true);
    }

    /// Sets the status of the keyboard key to released.
    /// Port of Keyboard::ReleaseKeyboardKey
    pub fn release_keyboard_key(&mut self, key_index: i32) {
        if key_index == native_keyboard::Keys::None as i32 {
            return;
        }
        self.engine
            .set_button(&keyboard_key_identifier(), key_index, false);
    }

    /// Sets the status of all keyboard modifier keys.
    /// Port of Keyboard::SetKeyboardModifiers
    pub fn set_keyboard_modifiers(&mut self, key_modifiers: i32) {
        let kbd_key_id = keyboard_key_identifier();
        let kbd_mod_id = keyboard_modifier_identifier();

        for i in 0..32 {
            let key_value = ((key_modifiers >> i) & 0x1) != 0;
            self.engine.set_button(&kbd_mod_id, i, key_value);

            // Use the modifier to press the key button equivalent
            match i {
                i if i == native_keyboard::Modifiers::LeftControl as i32 => {
                    self.engine.set_button(
                        &kbd_key_id,
                        native_keyboard::Keys::LeftControlKey as i32,
                        key_value,
                    );
                }
                i if i == native_keyboard::Modifiers::LeftShift as i32 => {
                    self.engine.set_button(
                        &kbd_key_id,
                        native_keyboard::Keys::LeftShiftKey as i32,
                        key_value,
                    );
                }
                i if i == native_keyboard::Modifiers::LeftAlt as i32 => {
                    self.engine.set_button(
                        &kbd_key_id,
                        native_keyboard::Keys::LeftAltKey as i32,
                        key_value,
                    );
                }
                i if i == native_keyboard::Modifiers::LeftMeta as i32 => {
                    self.engine.set_button(
                        &kbd_key_id,
                        native_keyboard::Keys::LeftMetaKey as i32,
                        key_value,
                    );
                }
                i if i == native_keyboard::Modifiers::RightControl as i32 => {
                    self.engine.set_button(
                        &kbd_key_id,
                        native_keyboard::Keys::RightControlKey as i32,
                        key_value,
                    );
                }
                i if i == native_keyboard::Modifiers::RightShift as i32 => {
                    self.engine.set_button(
                        &kbd_key_id,
                        native_keyboard::Keys::RightShiftKey as i32,
                        key_value,
                    );
                }
                i if i == native_keyboard::Modifiers::RightAlt as i32 => {
                    self.engine.set_button(
                        &kbd_key_id,
                        native_keyboard::Keys::RightAltKey as i32,
                        key_value,
                    );
                }
                i if i == native_keyboard::Modifiers::RightMeta as i32 => {
                    self.engine.set_button(
                        &kbd_key_id,
                        native_keyboard::Keys::RightMetaKey as i32,
                        key_value,
                    );
                }
                _ => {
                    // Other modifier keys should be pressed with PressKey since they stay enabled
                    // until next press
                }
            }
        }
    }

    /// Sets all keys to the non pressed state.
    /// Port of Keyboard::ReleaseAllKeys
    pub fn release_all_keys(&mut self) {
        self.engine.reset_button_state();
    }

    /// Used for automapping features.
    /// Port of Keyboard::GetInputDevices (override)
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        let mut param = ParamPackage::default();
        param.set_str("engine", self.engine.get_engine_name().to_string());
        param.set_str("display", "Keyboard Only".to_string());
        vec![param]
    }
}
