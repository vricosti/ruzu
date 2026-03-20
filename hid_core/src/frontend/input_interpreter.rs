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
    // Upstream holds a shared_ptr<NPad> obtained from the HID server's ResourceManager.
    // Requires NPad resource integration which is not yet wired up.
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
        // Upstream calls npad->GetAndResetPressState() to get the current button state.
        // Requires NPad resource integration which is not yet wired up.
        // For now, just advance the indices (no actual input).
        self.previous_index = self.current_index;
        self.current_index = (self.current_index + 1) % self.button_states.len();
        self.button_states[self.current_index] = NpadButton::empty();
    }

    /// Resets all the button states to their defaults.
    pub fn reset_button_states(&mut self) {
        self.previous_index = 0;
        self.current_index = 0;

        // First entry is set to All (matching upstream ResetButtonStates)
        self.button_states[0] = NpadButton::all();
        for i in 1..self.button_states.len() {
            self.button_states[i] = NpadButton::empty();
        }
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
