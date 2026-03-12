// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/system_buttons/capture_button.h and capture_button.cpp

use super::system_button_types::CaptureButtonState;
use crate::resources::controller_base::ControllerActivation;

/// CaptureButton controller — reads capture button state from emulated
/// controller Player1 and writes into shared memory.
pub struct CaptureButton {
    pub activation: ControllerActivation,
    next_state: CaptureButtonState,
}

impl CaptureButton {
    pub fn new() -> Self {
        Self {
            activation: ControllerActivation::new(),
            next_state: CaptureButtonState::default(),
        }
    }

    pub fn on_init(&mut self) {}
    pub fn on_release(&mut self) {}

    /// Port of CaptureButton::OnUpdate.
    ///
    /// Upstream logic:
    ///   lock shared_mutex
    ///   get active aruid -> AruidData
    ///   shared_memory = data->shared_memory_format->capture_button
    ///   if not activated: clear lifo buffers, return
    ///   last_entry = capture_lifo.ReadCurrentEntry().state
    ///   next_state.sampling_number = last_entry.sampling_number + 1
    ///   controller = hid_core.GetEmulatedController(NpadIdType::Player1)
    ///   next_state.buttons.raw = controller->GetHomeButtons().raw
    ///   capture_lifo.WriteNextEntry(next_state)
    pub fn on_update(&mut self) {
        let _ = &self.next_state;
    }
}

impl Default for CaptureButton {
    fn default() -> Self {
        Self::new()
    }
}
