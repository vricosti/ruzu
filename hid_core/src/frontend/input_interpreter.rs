// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/frontend/input_interpreter.h and input_interpreter.cpp
//!
//! The InputInterpreter class interfaces with HID to retrieve button press states.
//! Input is intended to be polled every 50ms so that a button is considered to be
//! held down after 400ms has elapsed since the initial button press and subsequent
//! repeated presses occur every 50ms.

use crate::hid_types::NpadButton;

pub struct InputInterpreter {
    // TODO: npad: Arc<Mutex<NPad>>,
    /// Stores 9 consecutive button states polled from HID.
    button_states: [NpadButton; 9],
    previous_index: usize,
    current_index: usize,
}

impl InputInterpreter {
    pub fn new() -> Self {
        Self {
            button_states: [NpadButton::empty(); 9],
            previous_index: 0,
            current_index: 0,
        }
    }

    /// Gets a button state from HID and inserts it into the array of button states.
    pub fn poll_input(&mut self) {
        todo!()
    }

    /// Resets all the button states to their defaults.
    pub fn reset_button_states(&mut self) {
        self.button_states = [NpadButton::empty(); 9];
    }

    /// Checks whether the button is pressed.
    pub fn is_button_pressed(&self, button: NpadButton) -> bool {
        self.button_states[self.current_index].contains(button)
    }

    /// The specified button is considered to be pressed once
    /// if it is currently pressed and not pressed previously.
    pub fn is_button_pressed_once(&self, button: NpadButton) -> bool {
        let current = self.button_states[self.current_index].contains(button);
        let previous = self.button_states[self.previous_index].contains(button);
        current && !previous
    }

    /// The specified button is considered to be held down if it is pressed in all 9 button states.
    pub fn is_button_held(&self, button: NpadButton) -> bool {
        self.button_states.iter().all(|state| state.contains(button))
    }
}

impl Default for InputInterpreter {
    fn default() -> Self {
        Self::new()
    }
}
