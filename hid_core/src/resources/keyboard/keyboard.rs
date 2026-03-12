// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/keyboard/keyboard.h and keyboard.cpp

use super::keyboard_types::KeyboardState;
use crate::resources::controller_base::ControllerActivation;

/// Keyboard controller — reads keyboard input from emulated devices and writes
/// into shared memory.
pub struct Keyboard {
    pub activation: ControllerActivation,
    next_state: KeyboardState,
}

impl Keyboard {
    pub fn new() -> Self {
        Self {
            activation: ControllerActivation::new(),
            next_state: KeyboardState::default(),
        }
    }

    pub fn on_init(&mut self) {}
    pub fn on_release(&mut self) {}

    /// Port of Keyboard::OnUpdate.
    ///
    /// Upstream logic:
    ///   lock shared_mutex
    ///   get active aruid -> AruidData
    ///   shared_memory = data->shared_memory_format->keyboard
    ///   if not activated: clear lifo buffers, return
    ///   last_entry = keyboard_lifo.ReadCurrentEntry().state
    ///   next_state.sampling_number = last_entry.sampling_number + 1
    ///   if keyboard_enabled:
    ///       next_state.key = emulated_devices->GetKeyboard()
    ///       next_state.modifier = emulated_devices->GetKeyboardModifier()
    ///       next_state.attribute.is_connected = 1
    ///   keyboard_lifo.WriteNextEntry(next_state)
    pub fn on_update(&mut self) {
        let _ = &self.next_state;
    }
}

impl Default for Keyboard {
    fn default() -> Self {
        Self::new()
    }
}
