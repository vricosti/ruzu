// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/frontend/emulated_devices.h and emulated_devices.cpp

use std::collections::HashMap;

use parking_lot::Mutex;

use crate::hid_types::*;

#[derive(Debug, Clone, Copy, Default)]
pub struct MousePosition {
    pub x: f32,
    pub y: f32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DeviceTriggerType {
    Keyboard,
    KeyboardModdifier,
    Mouse,
    RingController,
}

pub struct InterfaceUpdateCallback {
    pub on_change: Box<dyn Fn(DeviceTriggerType) + Send + Sync>,
}

pub struct EmulatedDevices {
    is_configuring: bool,
    mutex: Mutex<()>,
    callback_mutex: Mutex<()>,
    callback_list: HashMap<i32, InterfaceUpdateCallback>,
    last_callback_key: i32,

    // Stores the current status of all external device input
    keyboard_state: KeyboardKey,
    keyboard_modifier_state: KeyboardModifier,
    mouse_button_state: MouseButton,
    mouse_position_state: MousePosition,
    mouse_wheel_state: AnalogStickState,
}

impl EmulatedDevices {
    pub fn new() -> Self {
        Self {
            is_configuring: false,
            mutex: Mutex::new(()),
            callback_mutex: Mutex::new(()),
            callback_list: HashMap::new(),
            last_callback_key: 0,
            keyboard_state: KeyboardKey::default(),
            keyboard_modifier_state: KeyboardModifier::default(),
            mouse_button_state: MouseButton::default(),
            mouse_position_state: MousePosition::default(),
            mouse_wheel_state: AnalogStickState::default(),
        }
    }

    pub fn unload_input(&mut self) {
        // TODO: reset all input devices
    }

    pub fn enable_configuration(&mut self) {
        self.is_configuring = true;
        self.save_current_config();
    }

    pub fn disable_configuration(&mut self) {
        self.is_configuring = false;
    }

    pub fn is_configuring(&self) -> bool {
        self.is_configuring
    }

    pub fn reload_input(&mut self) {
        todo!()
    }

    pub fn reload_from_settings(&mut self) {
        todo!()
    }

    pub fn save_current_config(&mut self) {
        if !self.is_configuring {
            return;
        }
    }

    pub fn restore_config(&mut self) {
        if !self.is_configuring {
            return;
        }
        self.reload_from_settings();
    }

    pub fn get_keyboard(&self) -> KeyboardKey {
        let _lock = self.mutex.lock();
        self.keyboard_state
    }

    pub fn get_keyboard_modifier(&self) -> KeyboardModifier {
        let _lock = self.mutex.lock();
        self.keyboard_modifier_state
    }

    pub fn get_mouse_buttons(&self) -> MouseButton {
        let _lock = self.mutex.lock();
        self.mouse_button_state
    }

    pub fn get_mouse_position(&self) -> MousePosition {
        let _lock = self.mutex.lock();
        self.mouse_position_state
    }

    pub fn get_mouse_wheel(&self) -> AnalogStickState {
        let _lock = self.mutex.lock();
        self.mouse_wheel_state
    }

    fn update_key(&mut self, key_index: usize, status: bool) {
        const KEYS_PER_BYTE: usize = 8;
        let entry = &mut self.keyboard_state.key[key_index / KEYS_PER_BYTE];
        let mask = 1u8 << (key_index % KEYS_PER_BYTE);
        if status {
            *entry |= mask;
        } else {
            *entry &= !mask;
        }
    }

    pub fn set_callback(&mut self, update_callback: InterfaceUpdateCallback) -> i32 {
        let _lock = self.callback_mutex.lock();
        let key = self.last_callback_key;
        self.callback_list.insert(key, update_callback);
        self.last_callback_key += 1;
        key
    }

    pub fn delete_callback(&mut self, key: i32) {
        let _lock = self.callback_mutex.lock();
        if self.callback_list.remove(&key).is_none() {
            log::error!("Tried to delete non-existent callback {}", key);
        }
    }

    fn trigger_on_change(&self, trigger_type: DeviceTriggerType) {
        let _lock = self.callback_mutex.lock();
        for (_key, callback) in &self.callback_list {
            (callback.on_change)(trigger_type);
        }
    }
}

impl Default for EmulatedDevices {
    fn default() -> Self {
        Self::new()
    }
}
