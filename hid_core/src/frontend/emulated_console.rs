// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/frontend/emulated_console.h and emulated_console.cpp

use std::collections::HashMap;

use parking_lot::Mutex;

use crate::hid_types::*;

pub const MAX_TOUCH_DEVICES: usize = 32;
pub const MAX_ACTIVE_TOUCH_INPUTS: usize = 16;

/// Contains all motion related data that is used on the services
#[derive(Debug, Clone, Copy, Default)]
pub struct ConsoleMotion {
    pub accel: Vec3f,
    pub gyro: Vec3f,
    pub rotation: Vec3f,
    pub orientation: [Vec3f; 3],
    pub gyro_bias: Vec3f,
    pub verticalization_error: f32,
    pub is_at_rest: bool,
}

pub type TouchFingerState = [TouchFinger; MAX_ACTIVE_TOUCH_INPUTS];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConsoleTriggerType {
    Motion,
    Touch,
    All,
}

pub struct ConsoleUpdateCallback {
    pub on_change: Box<dyn Fn(ConsoleTriggerType) + Send + Sync>,
}

pub struct EmulatedConsole {
    is_configuring: bool,
    motion_sensitivity: f32,
    mutex: Mutex<()>,
    callback_mutex: Mutex<()>,
    callback_list: HashMap<i32, ConsoleUpdateCallback>,
    last_callback_key: i32,
    motion_state: ConsoleMotion,
    touch_state: TouchFingerState,
}

impl EmulatedConsole {
    pub fn new() -> Self {
        Self {
            is_configuring: false,
            motion_sensitivity: 0.01,
            mutex: Mutex::new(()),
            callback_mutex: Mutex::new(()),
            callback_list: HashMap::new(),
            last_callback_key: 0,
            motion_state: ConsoleMotion::default(),
            touch_state: [TouchFinger::default(); MAX_ACTIVE_TOUCH_INPUTS],
        }
    }

    pub fn unload_input(&mut self) {
        // Upstream (emulated_console.cpp UnloadInput) resets motion_devices[2] and
        // touch_devices[MAX_TOUCH_DEVICES] by calling .reset() on each
        // Common::Input::InputDevice. Depends on Common::Input::InputDevice from
        // zuyu/src/common/input.h and the device arrays stored as
        // std::array<std::unique_ptr<Common::Input::InputDevice>, N>.
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
        // Upstream (emulated_console.cpp ReloadInput) calls SetTouchParams() to populate
        // touch_params[MAX_TOUCH_DEVICES], sets motion_params[1] to virtual_gamepad,
        // creates motion_devices via Common::Input::CreateInputDevice(motion_params[i]),
        // sets SetMotion callbacks on each, restores MotionInput state (accel/gyro/rotation/
        // orientation/is_at_rest), then creates touch_devices from touch_params with
        // SetTouch callbacks. Depends on Common::Input::CreateInputDevice from
        // zuyu/src/common/input.h and MotionInput from hid_core/frontend/motion_input.h.
        log::debug!("EmulatedConsole::reload_input called");
    }

    pub fn reload_from_settings(&mut self) {
        // Upstream (emulated_console.cpp ReloadFromSettings) reads
        // Settings::values.players.GetValue()[0].motions[0] as a ParamPackage into
        // motion_params[0], then calls ReloadInput(). Depends on
        // Settings::values.players from zuyu/src/common/settings.h (PlayerInput struct).
        log::debug!("EmulatedConsole::reload_from_settings called");
        self.reload_input();
    }

    fn get_index_from_finger_id(&self, finger_id: usize) -> Option<usize> {
        // Upstream (emulated_console.cpp GetIndexFromFingerId) iterates
        // console.touch_values[0..MAX_TOUCH_DEVICES] and returns the index of the first
        // entry where pressed.value == true && id == finger_id. touch_values is of type
        // std::array<TouchValues, MAX_TOUCH_DEVICES> populated by SetTouch callbacks from
        // Common::Input::InputDevice. Depends on touch_values state array backed by
        // Common::Input device callbacks from zuyu/src/common/input.h.
        let _ = finger_id;
        None
    }

    fn get_next_free_index(&self) -> Option<usize> {
        // Upstream (emulated_console.cpp GetNextFreeIndex) iterates
        // console.touch_values[0..MAX_TOUCH_DEVICES] and returns the first index where
        // pressed.value == false. Depends on touch_values state array backed by
        // Common::Input device callbacks from zuyu/src/common/input.h.
        None
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

    pub fn get_motion(&self) -> ConsoleMotion {
        let _lock = self.mutex.lock();
        self.motion_state
    }

    pub fn get_touch(&self) -> TouchFingerState {
        let _lock = self.mutex.lock();
        self.touch_state
    }

    pub fn set_callback(&mut self, update_callback: ConsoleUpdateCallback) -> i32 {
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

    fn trigger_on_change(&self, trigger_type: ConsoleTriggerType) {
        let _lock = self.callback_mutex.lock();
        for (_key, callback) in &self.callback_list {
            (callback.on_change)(trigger_type);
        }
    }
}

impl Default for EmulatedConsole {
    fn default() -> Self {
        Self::new()
    }
}
