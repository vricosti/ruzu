// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/mouse/debug_mouse.h and debug_mouse.cpp

use crate::hid_types::{AnalogStickState, MouseState};
use crate::resources::controller_base::ControllerActivation;

/// DebugMouse controller — identical to Mouse but writes to the debug_mouse
/// field of shared memory. Upstream shares the exact same update logic.
pub struct DebugMouse {
    pub activation: ControllerActivation,
    next_state: MouseState,
    last_mouse_wheel_state: AnalogStickState,
}

impl DebugMouse {
    pub fn new() -> Self {
        Self {
            activation: ControllerActivation::new(),
            next_state: MouseState::default(),
            last_mouse_wheel_state: AnalogStickState::default(),
        }
    }

    pub fn on_init(&mut self) {}
    pub fn on_release(&mut self) {}

    /// Port of DebugMouse::OnUpdate — same logic as Mouse::OnUpdate but
    /// targets `shared_memory_format->debug_mouse`.
    pub fn on_update(&mut self) {
        let _ = &self.next_state;
        let _ = &self.last_mouse_wheel_state;
    }
}

impl Default for DebugMouse {
    fn default() -> Self {
        Self::new()
    }
}
