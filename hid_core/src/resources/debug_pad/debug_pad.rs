// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/debug_pad/debug_pad.h and debug_pad.cpp

use super::debug_pad_types::DebugPadState;
use crate::resources::controller_base::ControllerActivation;

/// DebugPad controller — reads debug pad input from emulated controller (Other)
/// and writes into shared memory.
pub struct DebugPad {
    pub activation: ControllerActivation,
    next_state: DebugPadState,
}

impl DebugPad {
    pub fn new() -> Self {
        Self {
            activation: ControllerActivation::new(),
            next_state: DebugPadState::default(),
        }
    }

    pub fn on_init(&mut self) {}
    pub fn on_release(&mut self) {}

    /// Port of DebugPad::OnUpdate.
    ///
    /// Upstream logic:
    ///   lock shared_mutex
    ///   get active aruid -> AruidData
    ///   shared_memory = data->shared_memory_format->debug_pad
    ///   if not activated: clear lifo buffers, return
    ///   last_entry = debug_pad_lifo.ReadCurrentEntry().state
    ///   next_state.sampling_number = last_entry.sampling_number + 1
    ///   if debug_pad_enabled:
    ///       next_state.attribute.connected = 1
    ///       next_state.pad_state = controller->GetDebugPadButtons()
    ///       sticks = controller->GetSticks()
    ///       next_state.l_stick = sticks.left
    ///       next_state.r_stick = sticks.right
    ///   debug_pad_lifo.WriteNextEntry(next_state)
    pub fn on_update(&mut self) {
        let _ = &self.next_state;
    }
}

impl Default for DebugPad {
    fn default() -> Self {
        Self::new()
    }
}
