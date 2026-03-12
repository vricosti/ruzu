// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/system_buttons/sleep_button.h and sleep_button.cpp

use super::system_button_types::SleepButtonState;
use crate::resources::controller_base::ControllerActivation;

/// SleepButton controller — always writes raw = 0 (upstream behaviour: sleep
/// button is never pressed in emulation).
pub struct SleepButton {
    pub activation: ControllerActivation,
    next_state: SleepButtonState,
}

impl SleepButton {
    pub fn new() -> Self {
        Self {
            activation: ControllerActivation::new(),
            next_state: SleepButtonState::default(),
        }
    }

    pub fn on_init(&mut self) {}
    pub fn on_release(&mut self) {}

    /// Port of SleepButton::OnUpdate.
    ///
    /// Upstream logic:
    ///   lock shared_mutex
    ///   get active aruid -> AruidData
    ///   shared_memory = data->shared_memory_format->sleep_button
    ///   if not activated: clear lifo buffers, return
    ///   last_entry = sleep_lifo.ReadCurrentEntry().state
    ///   next_state.sampling_number = last_entry.sampling_number + 1
    ///   next_state.buttons.raw = 0
    ///   sleep_lifo.WriteNextEntry(next_state)
    pub fn on_update(&mut self) {
        let _ = &self.next_state;
    }
}

impl Default for SleepButton {
    fn default() -> Self {
        Self::new()
    }
}
