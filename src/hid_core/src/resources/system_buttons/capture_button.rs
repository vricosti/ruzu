// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/system_buttons/capture_button.h and capture_button.cpp

use super::system_button_types::CaptureButtonState;
use crate::hid_types;
use crate::resources::controller_base::ControllerActivation;
use crate::resources::shared_memory_format::CaptureButtonSharedMemoryFormat;

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
    /// Upstream:
    ///   lock shared_mutex
    ///   get active aruid -> AruidData
    ///   shared_memory = data->shared_memory_format->capture_button
    ///   if not activated: clear lifo buffers, return
    ///   last_entry = capture_lifo.ReadCurrentEntry().state
    ///   next_state.sampling_number = last_entry.sampling_number + 1
    ///   controller = hid_core.GetEmulatedController(NpadIdType::Player1)
    ///   next_state.buttons.raw = controller->GetHomeButtons().raw
    ///   capture_lifo.WriteNextEntry(next_state)
    ///
    /// Note: upstream uses GetHomeButtons() for capture button too (upstream quirk).
    pub fn on_update(
        &mut self,
        shared_memory: &mut CaptureButtonSharedMemoryFormat,
        home_buttons: hid_types::CaptureButtonState,
    ) {
        if !self.activation.is_controller_activated() {
            shared_memory.capture_lifo.buffer_count = 0;
            shared_memory.capture_lifo.buffer_tail = 0;
            return;
        }

        let last_entry = shared_memory.capture_lifo.read_current_entry();
        self.next_state.sampling_number = last_entry.state.sampling_number + 1;

        self.next_state.buttons = home_buttons;

        shared_memory.capture_lifo.write_next_entry(self.next_state);
    }
}

impl Default for CaptureButton {
    fn default() -> Self {
        Self::new()
    }
}
