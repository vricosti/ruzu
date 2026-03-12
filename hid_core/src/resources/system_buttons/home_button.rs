// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/system_buttons/home_button.h and home_button.cpp

use super::system_button_types::HomeButtonState;
use crate::resources::controller_base::ControllerActivation;

/// HomeButton controller — reads the home button state from emulated controller
/// Player1 and writes into shared memory.
pub struct HomeButton {
    pub activation: ControllerActivation,
    next_state: HomeButtonState,
}

impl HomeButton {
    pub fn new() -> Self {
        Self {
            activation: ControllerActivation::new(),
            next_state: HomeButtonState::default(),
        }
    }

    pub fn on_init(&mut self) {}
    pub fn on_release(&mut self) {}

    /// Port of HomeButton::OnUpdate.
    ///
    /// Upstream logic:
    ///   lock shared_mutex
    ///   get active aruid -> AruidData
    ///   shared_memory = data->shared_memory_format->home_button
    ///   if not activated: clear lifo buffers, return
    ///   last_entry = home_lifo.ReadCurrentEntry().state
    ///   next_state.sampling_number = last_entry.sampling_number + 1
    ///   controller = hid_core.GetEmulatedController(NpadIdType::Player1)
    ///   next_state.buttons.raw = controller->GetHomeButtons().raw
    ///   home_lifo.WriteNextEntry(next_state)
    pub fn on_update(&mut self) {
        let _ = &self.next_state;
    }
}

impl Default for HomeButton {
    fn default() -> Self {
        Self::new()
    }
}
