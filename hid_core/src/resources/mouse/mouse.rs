// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/mouse/mouse.h and mouse.cpp

use crate::hid_types::{AnalogStickState, MouseState};
use crate::resources::controller_base::ControllerActivation;

/// Mouse controller — reads mouse input from emulated devices and writes into
/// shared memory.
pub struct Mouse {
    pub activation: ControllerActivation,
    next_state: MouseState,
    last_mouse_wheel_state: AnalogStickState,
}

impl Mouse {
    pub fn new() -> Self {
        Self {
            activation: ControllerActivation::new(),
            next_state: MouseState::default(),
            last_mouse_wheel_state: AnalogStickState::default(),
        }
    }

    pub fn on_init(&mut self) {}
    pub fn on_release(&mut self) {}

    /// Port of Mouse::OnUpdate.
    ///
    /// In the full system this would:
    ///  1. Lock shared_mutex
    ///  2. Get active aruid / AruidData
    ///  3. Get MouseSharedMemoryFormat from shared memory
    ///  4. Read emulated device state and write into the LIFO
    ///
    /// The body is intentionally simplified because the shared-memory holder,
    /// applet resource, and emulated devices are not yet wired together. The
    /// per-field update logic is preserved here as documentation of upstream
    /// behaviour.
    pub fn on_update(&mut self) {
        // In a full wiring this reads from applet_resource and
        // emulated_devices, writing into MouseSharedMemoryFormat.
        //
        // Upstream logic summary:
        //   next_state = {};
        //   next_state.sampling_number = last_entry.sampling_number + 1;
        //   if mouse_enabled:
        //       read mouse_button_state, mouse_position_state, mouse_wheel_state
        //       next_state.attribute.is_connected = 1
        //       next_state.x = mouse_pos.x * ScreenWidth
        //       next_state.y = mouse_pos.y * ScreenHeight
        //       next_state.delta_x = next_state.x - last_entry.x
        //       next_state.delta_y = next_state.y - last_entry.y
        //       next_state.delta_wheel_x = wheel.x - last_wheel.x
        //       next_state.delta_wheel_y = wheel.y - last_wheel.y
        //       last_mouse_wheel_state = wheel
        //       next_state.button = mouse_button_state
        //   write_next_entry(next_state)
        let _ = &self.next_state;
        let _ = &self.last_mouse_wheel_state;
    }
}

impl Default for Mouse {
    fn default() -> Self {
        Self::new()
    }
}
