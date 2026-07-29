// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/frontend/emulated_console.h and emulated_console.cpp

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use common::input::{self, CallbackStatus, InputCallback, InputDevice, TouchStatus};
use common::param_package::ParamPackage;
use parking_lot::Mutex;

use super::input_converter::transform_to_touch;
use crate::hid_types::*;

pub const MAX_TOUCH_DEVICES: usize = 32;
pub const MAX_ACTIVE_TOUCH_INPUTS: usize = 16;

/// Contains all motion related data that is used on the services.
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
type TouchValues = [TouchStatus; MAX_TOUCH_DEVICES];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConsoleTriggerType {
    Motion,
    Touch,
    All,
}

pub struct ConsoleUpdateCallback {
    pub on_change: Box<dyn Fn(ConsoleTriggerType) + Send + Sync>,
}

struct ConsoleStatus {
    touch_values: TouchValues,
    touch_state: TouchFingerState,
}

impl Default for ConsoleStatus {
    fn default() -> Self {
        Self {
            touch_values: [TouchStatus::default(); MAX_TOUCH_DEVICES],
            touch_state: [TouchFinger::default(); MAX_ACTIVE_TOUCH_INPUTS],
        }
    }
}

pub struct EmulatedConsole {
    is_configuring: Arc<AtomicBool>,
    motion_sensitivity: f32,
    touch_params: [ParamPackage; MAX_TOUCH_DEVICES],
    touch_devices: [Option<Box<dyn InputDevice>>; MAX_TOUCH_DEVICES],
    console: Arc<Mutex<ConsoleStatus>>,
    callback_list: Arc<Mutex<HashMap<i32, ConsoleUpdateCallback>>>,
    last_callback_key: i32,
    motion_state: ConsoleMotion,
}

impl EmulatedConsole {
    pub fn new() -> Self {
        Self {
            is_configuring: Arc::new(AtomicBool::new(false)),
            motion_sensitivity: 0.01,
            touch_params: std::array::from_fn(|_| ParamPackage::default()),
            touch_devices: std::array::from_fn(|_| None),
            console: Arc::new(Mutex::new(ConsoleStatus::default())),
            callback_list: Arc::new(Mutex::new(HashMap::new())),
            last_callback_key: 0,
            motion_state: ConsoleMotion::default(),
        }
    }

    pub fn unload_input(&mut self) {
        for touch in &mut self.touch_devices {
            *touch = None;
        }
    }

    pub fn enable_configuration(&mut self) {
        self.is_configuring.store(true, Ordering::Relaxed);
        self.save_current_config();
    }

    pub fn disable_configuration(&mut self) {
        self.is_configuring.store(false, Ordering::Relaxed);
    }

    pub fn is_configuring(&self) -> bool {
        self.is_configuring.load(Ordering::Relaxed)
    }

    pub fn reload_input(&mut self) {
        self.set_touch_params();
        for index in 0..self.touch_devices.len() {
            let mut device = input::create_input_device(&self.touch_params[index]);
            let console = Arc::clone(&self.console);
            let callback_list = Arc::clone(&self.callback_list);
            let is_configuring = Arc::clone(&self.is_configuring);
            device.set_callback(InputCallback {
                on_change: Some(Arc::new(move |callback| {
                    Self::set_touch(&console, &callback_list, &is_configuring, callback, index);
                })),
            });
            self.touch_devices[index] = Some(device);
        }
    }

    pub fn reload_from_settings(&mut self) {
        self.reload_input();
    }

    fn set_touch_params(&mut self) {
        self.touch_params = std::array::from_fn(|_| ParamPackage::default());
        let mut index = 0usize;
        let settings = common::settings::values();

        if !settings.mouse_enabled.get_value() {
            self.touch_params[index] =
                ParamPackage::from_serialized("engine:mouse,axis_x:0,axis_y:1,button:0,port:2");
            index += 1;
        }

        for params in [
            "engine:cemuhookudp,axis_x:17,axis_y:18,button:65536",
            "engine:cemuhookudp,axis_x:19,axis_y:20,button:131072",
        ] {
            self.touch_params[index] = ParamPackage::from_serialized(params);
            index += 1;
        }

        for touch_index in 0..MAX_ACTIVE_TOUCH_INPUTS {
            let mut params = ParamPackage::default();
            params.set_str("engine", "touch".to_string());
            params.set_int("axis_x", (touch_index * 2) as i32);
            params.set_int("axis_y", (touch_index * 2 + 1) as i32);
            params.set_int("button", touch_index as i32);
            self.touch_params[index] = params;
            index += 1;
        }

        if settings.touch_from_button_maps.is_empty() {
            log::warn!("touch_from_button_maps is unset by frontend config");
            return;
        }
        let map_index = (*settings.touch_from_button_map_index.get_value()).max(0) as usize;
        let Some(touch_map) = settings.touch_from_button_maps.get(map_index) else {
            return;
        };
        for config_entry in &touch_map.buttons {
            if index >= MAX_TOUCH_DEVICES {
                break;
            }
            let mut params = ParamPackage::from_serialized(config_entry);
            let x = params.get_int("x", 0);
            let y = params.get_int("y", 0);
            params.erase("x");
            params.erase("y");
            let mut touch_button = ParamPackage::default();
            touch_button.set_str("engine", "touch_from_button".to_string());
            touch_button.set_str("button", params.serialize());
            touch_button.set_int("x", x);
            touch_button.set_int("y", y);
            self.touch_params[index] = touch_button;
            index += 1;
        }
    }

    fn set_touch(
        console: &Arc<Mutex<ConsoleStatus>>,
        callback_list: &Arc<Mutex<HashMap<i32, ConsoleUpdateCallback>>>,
        is_configuring: &AtomicBool,
        callback: &CallbackStatus,
        index: usize,
    ) {
        if index >= MAX_TOUCH_DEVICES {
            return;
        }
        let touch_input = transform_to_touch(callback);
        let mut console = console.lock();
        let mut touch_index = Self::get_index_from_finger_id(&console, index);
        let mut is_new_input = false;
        if touch_index.is_none() && touch_input.pressed.value {
            touch_index = Self::get_next_free_index(&console);
            is_new_input = true;
        }
        let Some(touch_index) = touch_index else {
            return;
        };

        {
            let touch_value = &mut console.touch_values[touch_index];
            if is_new_input {
                touch_value.pressed.value = true;
                touch_value.id = index as i32;
            }
            touch_value.x = touch_input.x;
            touch_value.y = touch_input.y;
            if !touch_input.pressed.value {
                touch_value.pressed.value = false;
            }
        }

        if !is_configuring.load(Ordering::Relaxed) {
            if touch_index >= MAX_ACTIVE_TOUCH_INPUTS {
                return;
            }
            let touch_value = console.touch_values[touch_index];
            console.touch_state[touch_index] = TouchFinger {
                position_x: touch_value.x.value,
                position_y: touch_value.y.value,
                id: touch_index as u32,
                pressed: touch_input.pressed.value,
                ..Default::default()
            };
        }
        drop(console);
        Self::trigger_callbacks(callback_list, ConsoleTriggerType::Touch);
    }

    fn get_index_from_finger_id(console: &ConsoleStatus, finger_id: usize) -> Option<usize> {
        console
            .touch_values
            .iter()
            .position(|finger| finger.pressed.value && finger.id == finger_id as i32)
    }

    fn get_next_free_index(console: &ConsoleStatus) -> Option<usize> {
        console
            .touch_values
            .iter()
            .position(|finger| !finger.pressed.value)
    }

    pub fn save_current_config(&mut self) {
        if !self.is_configuring() {
            return;
        }
    }

    pub fn restore_config(&mut self) {
        if !self.is_configuring() {
            return;
        }
        self.reload_from_settings();
    }

    pub fn get_motion(&self) -> ConsoleMotion {
        self.motion_state
    }

    pub fn get_touch(&self) -> TouchFingerState {
        self.console.lock().touch_state
    }

    pub fn set_callback(&mut self, update_callback: ConsoleUpdateCallback) -> i32 {
        let key = self.last_callback_key;
        self.callback_list.lock().insert(key, update_callback);
        self.last_callback_key += 1;
        key
    }

    pub fn delete_callback(&mut self, key: i32) {
        if self.callback_list.lock().remove(&key).is_none() {
            log::error!("Tried to delete non-existent callback {}", key);
        }
    }

    fn trigger_callbacks(
        callback_list: &Arc<Mutex<HashMap<i32, ConsoleUpdateCallback>>>,
        trigger_type: ConsoleTriggerType,
    ) {
        for callback in callback_list.lock().values() {
            (callback.on_change)(trigger_type);
        }
    }
}

impl Default for EmulatedConsole {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use input_common::drivers::mouse::{Mouse, MouseButton};
    use input_common::input_poller::InputFactory;

    #[test]
    fn mouse_touch_callback_updates_console_state() {
        let mut mouse = Mouse::new("mouse".to_string());
        let factory = InputFactory::new(mouse.engine());
        let params =
            ParamPackage::from_serialized("engine:mouse,axis_x:0,axis_y:1,button:0,port:2");
        let mut device = factory.create(&params);
        let console = Arc::new(Mutex::new(ConsoleStatus::default()));
        let callback_list = Arc::new(Mutex::new(HashMap::new()));
        let is_configuring = Arc::new(AtomicBool::new(false));
        device.set_callback(InputCallback {
            on_change: Some(Arc::new({
                let console = Arc::clone(&console);
                let callback_list = Arc::clone(&callback_list);
                let is_configuring = Arc::clone(&is_configuring);
                move |callback| {
                    EmulatedConsole::set_touch(
                        &console,
                        &callback_list,
                        &is_configuring,
                        callback,
                        0,
                    );
                }
            })),
        });

        mouse.press_touch_button(0.25, 0.75, MouseButton::Left);
        let touch = console.lock().touch_state[0];
        assert!(touch.pressed);
        assert_eq!(touch.position_x, 0.25);
        assert_eq!(touch.position_y, 0.75);

        mouse.release_button(MouseButton::Left);
        assert!(!console.lock().touch_state[0].pressed);
    }
}
