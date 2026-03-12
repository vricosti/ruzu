// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/mouse/mouse.h and mouse.cpp

use crate::hid_types::{AnalogStickState, MouseAttribute, MouseButton, MouseState};
use crate::resources::controller_base::ControllerActivation;
use crate::resources::shared_memory_format::MouseSharedMemoryFormat;

/// Screen dimensions for mouse coordinate mapping (undocked mode).
const SCREEN_WIDTH: f32 = 1280.0;
const SCREEN_HEIGHT: f32 = 720.0;

/// Mouse position as reported by emulated devices (normalized 0..1).
#[derive(Debug, Clone, Copy, Default)]
pub struct MousePosition {
    pub x: f32,
    pub y: f32,
}

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
    /// Upstream:
    ///   lock shared_mutex
    ///   get active aruid -> AruidData
    ///   shared_memory = data->shared_memory_format->mouse
    ///   if not activated: clear lifo buffers, return
    ///   next_state = {}
    ///   last_entry = mouse_lifo.ReadCurrentEntry().state
    ///   next_state.sampling_number = last_entry.sampling_number + 1
    ///   if mouse_enabled:
    ///     attribute.is_connected = 1
    ///     x/y from mouse position * screen dimensions
    ///     delta from last entry
    ///     wheel deltas from last wheel state
    ///     button = mouse_button_state
    ///   mouse_lifo.WriteNextEntry(next_state)
    pub fn on_update(
        &mut self,
        shared_memory: &mut MouseSharedMemoryFormat,
        mouse_enabled: bool,
        mouse_button_state: &MouseButton,
        mouse_position_state: &MousePosition,
        mouse_wheel_state: &AnalogStickState,
    ) {
        if !self.activation.is_controller_activated() {
            shared_memory.mouse_lifo.buffer_count = 0;
            shared_memory.mouse_lifo.buffer_tail = 0;
            return;
        }

        self.next_state = MouseState::default();

        let last_entry = shared_memory.mouse_lifo.read_current_entry().state;
        self.next_state.sampling_number = last_entry.sampling_number + 1;

        if mouse_enabled {
            self.next_state.attribute = MouseAttribute { raw: 1 }; // is_connected = 1
            self.next_state.x = (mouse_position_state.x * SCREEN_WIDTH) as i32;
            self.next_state.y = (mouse_position_state.y * SCREEN_HEIGHT) as i32;
            self.next_state.delta_x = self.next_state.x - last_entry.x;
            self.next_state.delta_y = self.next_state.y - last_entry.y;
            self.next_state.delta_wheel_x =
                mouse_wheel_state.x - self.last_mouse_wheel_state.x;
            self.next_state.delta_wheel_y =
                mouse_wheel_state.y - self.last_mouse_wheel_state.y;

            self.last_mouse_wheel_state = *mouse_wheel_state;
            self.next_state.button = *mouse_button_state;
        }

        shared_memory.mouse_lifo.write_next_entry(self.next_state);
    }
}

impl Default for Mouse {
    fn default() -> Self {
        Self::new()
    }
}
