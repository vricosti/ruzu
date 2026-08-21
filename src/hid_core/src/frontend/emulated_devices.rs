// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/frontend/emulated_devices.h and emulated_devices.cpp

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use common::input::{
    self, AnalogStatus, ButtonStatus, CallbackStatus, InputCallback, InputDevice, TouchStatus,
};
use common::param_package::ParamPackage;
use common::settings_input::{native_keyboard, native_mouse_button, native_mouse_wheel};
use parking_lot::Mutex;

use super::input_converter::{transform_to_analog, transform_to_button, transform_to_touch};
use crate::hid_types::*;

pub type KeyboardValues = [ButtonStatus; native_keyboard::NUM_KEYBOARD_KEYS];
pub type KeyboardModifierValues = [ButtonStatus; native_keyboard::NUM_KEYBOARD_MODS];
pub type MouseButtonValues = [ButtonStatus; native_mouse_button::NUM_MOUSE_BUTTONS];
pub type MouseWheelValues = [AnalogStatus; native_mouse_wheel::NUM_MOUSE_WHEELS];

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

struct DeviceStatus {
    keyboard_values: KeyboardValues,
    keyboard_modifier_values: KeyboardModifierValues,
    mouse_button_values: MouseButtonValues,
    mouse_wheel_values: MouseWheelValues,
    mouse_stick_value: TouchStatus,
    keyboard_state: KeyboardKey,
    keyboard_modifier_state: KeyboardModifier,
    mouse_button_state: MouseButton,
    mouse_position_state: MousePosition,
    mouse_wheel_state: AnalogStickState,
}

impl Default for DeviceStatus {
    fn default() -> Self {
        Self {
            keyboard_values: std::array::from_fn(|_| ButtonStatus::default()),
            keyboard_modifier_values: std::array::from_fn(|_| ButtonStatus::default()),
            mouse_button_values: std::array::from_fn(|_| ButtonStatus::default()),
            mouse_wheel_values: std::array::from_fn(|_| AnalogStatus::default()),
            mouse_stick_value: TouchStatus::default(),
            keyboard_state: KeyboardKey::default(),
            keyboard_modifier_state: KeyboardModifier::default(),
            mouse_button_state: MouseButton::default(),
            mouse_position_state: MousePosition::default(),
            mouse_wheel_state: AnalogStickState::default(),
        }
    }
}

pub struct EmulatedDevices {
    is_configuring: Arc<AtomicBool>,
    keyboard_devices: [Option<Box<dyn InputDevice>>; native_keyboard::NUM_KEYBOARD_KEYS],
    keyboard_modifier_devices: [Option<Box<dyn InputDevice>>; native_keyboard::NUM_KEYBOARD_MODS],
    mouse_button_devices: [Option<Box<dyn InputDevice>>; native_mouse_button::NUM_MOUSE_BUTTONS],
    mouse_wheel_devices: [Option<Box<dyn InputDevice>>; native_mouse_wheel::NUM_MOUSE_WHEELS],
    mouse_stick_device: Option<Box<dyn InputDevice>>,
    device_status: Arc<Mutex<DeviceStatus>>,
    callback_list: Arc<Mutex<HashMap<i32, InterfaceUpdateCallback>>>,
    last_callback_key: i32,
}

impl EmulatedDevices {
    pub fn new() -> Self {
        Self {
            is_configuring: Arc::new(AtomicBool::new(false)),
            keyboard_devices: std::array::from_fn(|_| None),
            keyboard_modifier_devices: std::array::from_fn(|_| None),
            mouse_button_devices: std::array::from_fn(|_| None),
            mouse_wheel_devices: std::array::from_fn(|_| None),
            mouse_stick_device: None,
            device_status: Arc::new(Mutex::new(DeviceStatus::default())),
            callback_list: Arc::new(Mutex::new(HashMap::new())),
            last_callback_key: 0,
        }
    }

    pub fn unload_input(&mut self) {
        for device in &mut self.mouse_button_devices {
            *device = None;
        }
        for device in &mut self.mouse_wheel_devices {
            *device = None;
        }
        self.mouse_stick_device = None;
        for device in &mut self.keyboard_devices {
            *device = None;
        }
        for device in &mut self.keyboard_modifier_devices {
            *device = None;
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
        let mouse_params = ParamPackage::from_serialized("engine:mouse,port:1,pad:0");
        let keyboard_params = ParamPackage::from_serialized("engine:keyboard,port:1");

        for (index, device) in self.mouse_button_devices.iter_mut().enumerate() {
            let mut params = mouse_params.clone();
            params.set_int("button", index as i32);
            *device = Some(input::create_input_device(&params));
        }

        let mut mouse_position_params = mouse_params.clone();
        mouse_position_params.set_int("axis_x", 0);
        mouse_position_params.set_int("axis_y", 1);
        mouse_position_params.set_float("deadzone", 0.0);
        mouse_position_params.set_float("range", 1.0);
        mouse_position_params.set_float("threshold", 0.0);
        self.mouse_stick_device = Some(input::create_input_device(&mouse_position_params));

        for (index, device) in self.mouse_wheel_devices.iter_mut().enumerate() {
            let mut params = mouse_params.clone();
            params.set_int("axis", (index + 2) as i32);
            *device = Some(input::create_input_device(&params));
        }
        for (index, device) in self.keyboard_devices.iter_mut().enumerate() {
            let mut params = keyboard_params.clone();
            params.set_int("button", index as i32);
            params.set_int("pad", 0);
            *device = Some(input::create_input_device(&params));
        }
        for (index, device) in self.keyboard_modifier_devices.iter_mut().enumerate() {
            let mut params = keyboard_params.clone();
            params.set_int("button", index as i32);
            params.set_int("pad", 1);
            *device = Some(input::create_input_device(&params));
        }

        for (index, device) in self.mouse_button_devices.iter_mut().enumerate() {
            let status = Arc::clone(&self.device_status);
            let callbacks = Arc::clone(&self.callback_list);
            let is_configuring = Arc::clone(&self.is_configuring);
            device.as_mut().unwrap().set_callback(InputCallback {
                on_change: Some(Arc::new(move |callback| {
                    Self::set_mouse_button(&status, &callbacks, &is_configuring, callback, index);
                })),
            });
        }
        for (index, device) in self.mouse_wheel_devices.iter_mut().enumerate() {
            let status = Arc::clone(&self.device_status);
            let callbacks = Arc::clone(&self.callback_list);
            let is_configuring = Arc::clone(&self.is_configuring);
            device.as_mut().unwrap().set_callback(InputCallback {
                on_change: Some(Arc::new(move |callback| {
                    Self::set_mouse_wheel(&status, &callbacks, &is_configuring, callback, index);
                })),
            });
        }
        {
            let status = Arc::clone(&self.device_status);
            let callbacks = Arc::clone(&self.callback_list);
            let is_configuring = Arc::clone(&self.is_configuring);
            self.mouse_stick_device
                .as_mut()
                .unwrap()
                .set_callback(InputCallback {
                    on_change: Some(Arc::new(move |callback| {
                        Self::set_mouse_position(&status, &callbacks, &is_configuring, callback);
                    })),
                });
        }
        for (index, device) in self.keyboard_devices.iter_mut().enumerate() {
            let status = Arc::clone(&self.device_status);
            let callbacks = Arc::clone(&self.callback_list);
            let is_configuring = Arc::clone(&self.is_configuring);
            device.as_mut().unwrap().set_callback(InputCallback {
                on_change: Some(Arc::new(move |callback| {
                    Self::set_keyboard_button(
                        &status,
                        &callbacks,
                        &is_configuring,
                        callback,
                        index,
                    );
                })),
            });
        }
        for (index, device) in self.keyboard_modifier_devices.iter_mut().enumerate() {
            let status = Arc::clone(&self.device_status);
            let callbacks = Arc::clone(&self.callback_list);
            let is_configuring = Arc::clone(&self.is_configuring);
            device.as_mut().unwrap().set_callback(InputCallback {
                on_change: Some(Arc::new(move |callback| {
                    Self::set_keyboard_modifier(
                        &status,
                        &callbacks,
                        &is_configuring,
                        callback,
                        index,
                    );
                })),
            });
        }
    }

    pub fn reload_from_settings(&mut self) {
        self.reload_input();
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

    fn set_keyboard_button(
        device_status: &Arc<Mutex<DeviceStatus>>,
        callback_list: &Arc<Mutex<HashMap<i32, InterfaceUpdateCallback>>>,
        is_configuring: &AtomicBool,
        callback: &CallbackStatus,
        index: usize,
    ) {
        if index >= native_keyboard::NUM_KEYBOARD_KEYS {
            return;
        }
        let mut device_status = device_status.lock();
        let new_status = transform_to_button(callback);
        let current_status = &mut device_status.keyboard_values[index];
        current_status.toggle = new_status.toggle;
        let mut value_changed = false;
        if !current_status.toggle {
            current_status.locked = false;
            if current_status.value != new_status.value {
                current_status.value = new_status.value;
                value_changed = true;
            }
        } else {
            if new_status.value && !current_status.locked {
                current_status.locked = true;
                current_status.value = !current_status.value;
                value_changed = true;
            }
            if !new_status.value && current_status.locked {
                current_status.locked = false;
            }
        }
        if !value_changed {
            return;
        }
        if is_configuring.load(Ordering::Relaxed) {
            drop(device_status);
            Self::trigger_on_change(callback_list, DeviceTriggerType::Keyboard);
            return;
        }
        let value = device_status.keyboard_values[index].value;
        Self::update_key(&mut device_status, index, value);
        drop(device_status);
        Self::trigger_on_change(callback_list, DeviceTriggerType::Keyboard);
    }

    fn update_key(device_status: &mut DeviceStatus, key_index: usize, status: bool) {
        const KEYS_PER_BYTE: usize = 8;
        let entry = &mut device_status.keyboard_state.key[key_index / KEYS_PER_BYTE];
        let mask = 1u8 << (key_index % KEYS_PER_BYTE);
        if status {
            *entry |= mask;
        } else {
            *entry &= !mask;
        }
    }

    fn assign_bit(raw: &mut u32, bit: u32, value: bool) {
        if value {
            *raw |= 1 << bit;
        } else {
            *raw &= !(1 << bit);
        }
    }

    fn set_keyboard_modifier(
        device_status: &Arc<Mutex<DeviceStatus>>,
        callback_list: &Arc<Mutex<HashMap<i32, InterfaceUpdateCallback>>>,
        is_configuring: &AtomicBool,
        callback: &CallbackStatus,
        index: usize,
    ) {
        if index >= native_keyboard::NUM_KEYBOARD_MODS {
            return;
        }
        let mut device_status = device_status.lock();
        let new_status = transform_to_button(callback);
        let current_status = &mut device_status.keyboard_modifier_values[index];
        current_status.toggle = new_status.toggle;
        let mut value_changed = false;
        if !current_status.toggle {
            current_status.locked = false;
            if current_status.value != new_status.value {
                current_status.value = new_status.value;
                value_changed = true;
            }
        } else {
            if new_status.value && !current_status.locked {
                current_status.locked = true;
                current_status.value = !current_status.value;
                value_changed = true;
            }
            if !new_status.value && current_status.locked {
                current_status.locked = false;
            }
        }
        if !value_changed {
            return;
        }
        if is_configuring.load(Ordering::Relaxed) {
            drop(device_status);
            Self::trigger_on_change(callback_list, DeviceTriggerType::KeyboardModdifier);
            return;
        }
        let value = device_status.keyboard_modifier_values[index].value;
        let bit = match index {
            x if x == native_keyboard::Modifiers::LeftControl as usize
                || x == native_keyboard::Modifiers::RightControl as usize =>
            {
                Some(0)
            }
            x if x == native_keyboard::Modifiers::LeftShift as usize
                || x == native_keyboard::Modifiers::RightShift as usize =>
            {
                Some(1)
            }
            x if x == native_keyboard::Modifiers::LeftAlt as usize => Some(2),
            x if x == native_keyboard::Modifiers::RightAlt as usize => Some(3),
            x if x == native_keyboard::Modifiers::CapsLock as usize => Some(8),
            x if x == native_keyboard::Modifiers::ScrollLock as usize => Some(9),
            x if x == native_keyboard::Modifiers::NumLock as usize => Some(10),
            _ => None,
        };
        if let Some(bit) = bit {
            Self::assign_bit(&mut device_status.keyboard_modifier_state.raw, bit, value);
        }
        drop(device_status);
        Self::trigger_on_change(callback_list, DeviceTriggerType::KeyboardModdifier);
    }

    fn set_mouse_button(
        device_status: &Arc<Mutex<DeviceStatus>>,
        callback_list: &Arc<Mutex<HashMap<i32, InterfaceUpdateCallback>>>,
        is_configuring: &AtomicBool,
        callback: &CallbackStatus,
        index: usize,
    ) {
        if index >= native_mouse_button::NUM_MOUSE_BUTTONS {
            return;
        }
        let mut device_status = device_status.lock();
        let new_status = transform_to_button(callback);
        let current_status = &mut device_status.mouse_button_values[index];
        current_status.toggle = new_status.toggle;
        let mut value_changed = false;
        if !current_status.toggle {
            current_status.locked = false;
            if current_status.value != new_status.value {
                current_status.value = new_status.value;
                value_changed = true;
            }
        } else {
            if new_status.value && !current_status.locked {
                current_status.locked = true;
                current_status.value = !current_status.value;
                value_changed = true;
            }
            if !new_status.value && current_status.locked {
                current_status.locked = false;
            }
        }
        if !value_changed {
            return;
        }
        if is_configuring.load(Ordering::Relaxed) {
            drop(device_status);
            Self::trigger_on_change(callback_list, DeviceTriggerType::Mouse);
            return;
        }
        let value = device_status.mouse_button_values[index].value;
        Self::assign_bit(
            &mut device_status.mouse_button_state.raw,
            index as u32,
            value,
        );
        drop(device_status);
        Self::trigger_on_change(callback_list, DeviceTriggerType::Mouse);
    }

    fn set_mouse_wheel(
        device_status: &Arc<Mutex<DeviceStatus>>,
        callback_list: &Arc<Mutex<HashMap<i32, InterfaceUpdateCallback>>>,
        is_configuring: &AtomicBool,
        callback: &CallbackStatus,
        index: usize,
    ) {
        if index >= native_mouse_wheel::NUM_MOUSE_WHEELS {
            return;
        }
        let mut device_status = device_status.lock();
        let analog_value = transform_to_analog(callback);
        device_status.mouse_wheel_values[index] = analog_value;
        if is_configuring.load(Ordering::Relaxed) {
            device_status.mouse_wheel_state = AnalogStickState::default();
            drop(device_status);
            Self::trigger_on_change(callback_list, DeviceTriggerType::Mouse);
            return;
        }
        match index {
            x if x == native_mouse_wheel::Values::X as usize => {
                device_status.mouse_wheel_state.x = analog_value.value as i32;
            }
            x if x == native_mouse_wheel::Values::Y as usize => {
                device_status.mouse_wheel_state.y = analog_value.value as i32;
            }
            _ => {}
        }
        drop(device_status);
        Self::trigger_on_change(callback_list, DeviceTriggerType::Mouse);
    }

    fn set_mouse_position(
        device_status: &Arc<Mutex<DeviceStatus>>,
        callback_list: &Arc<Mutex<HashMap<i32, InterfaceUpdateCallback>>>,
        is_configuring: &AtomicBool,
        callback: &CallbackStatus,
    ) {
        let mut device_status = device_status.lock();
        let touch_value = transform_to_touch(callback);
        device_status.mouse_stick_value = touch_value;
        if is_configuring.load(Ordering::Relaxed) {
            device_status.mouse_position_state = MousePosition::default();
            drop(device_status);
            Self::trigger_on_change(callback_list, DeviceTriggerType::Mouse);
            return;
        }
        device_status.mouse_position_state.x = touch_value.x.value;
        device_status.mouse_position_state.y = touch_value.y.value;
        drop(device_status);
        Self::trigger_on_change(callback_list, DeviceTriggerType::Mouse);
    }

    pub fn get_keyboard_values(&self) -> KeyboardValues {
        self.device_status.lock().keyboard_values
    }
    pub fn get_keyboard_moddifier_values(&self) -> KeyboardModifierValues {
        self.device_status.lock().keyboard_modifier_values
    }
    pub fn get_mouse_buttons_values(&self) -> MouseButtonValues {
        self.device_status.lock().mouse_button_values
    }
    pub fn get_keyboard(&self) -> KeyboardKey {
        self.device_status.lock().keyboard_state
    }
    pub fn get_keyboard_modifier(&self) -> KeyboardModifier {
        self.device_status.lock().keyboard_modifier_state
    }
    pub fn get_mouse_buttons(&self) -> MouseButton {
        self.device_status.lock().mouse_button_state
    }
    pub fn get_mouse_position(&self) -> MousePosition {
        self.device_status.lock().mouse_position_state
    }
    pub fn get_mouse_wheel(&self) -> AnalogStickState {
        self.device_status.lock().mouse_wheel_state
    }

    pub fn set_callback(&mut self, update_callback: InterfaceUpdateCallback) -> i32 {
        self.last_callback_key += 1;
        self.callback_list
            .lock()
            .insert(self.last_callback_key, update_callback);
        self.last_callback_key
    }

    pub fn delete_callback(&mut self, key: i32) {
        assert!(
            self.callback_list.lock().remove(&key).is_some(),
            "Tried to delete non-existent callback {key}"
        );
    }

    fn trigger_on_change(
        callback_list: &Arc<Mutex<HashMap<i32, InterfaceUpdateCallback>>>,
        trigger_type: DeviceTriggerType,
    ) {
        for callback in callback_list.lock().values() {
            (callback.on_change)(trigger_type);
        }
    }
}

impl Default for EmulatedDevices {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::input::InputType;
    use std::sync::atomic::AtomicUsize;

    fn button_callback(value: bool, toggle: bool) -> CallbackStatus {
        let mut callback = CallbackStatus {
            input_type: InputType::Button,
            ..Default::default()
        };
        callback.button_status.value = value;
        callback.button_status.toggle = toggle;
        callback
    }

    #[test]
    fn keyboard_callback_updates_hid_bit_and_notifies() {
        let status = Arc::new(Mutex::new(DeviceStatus::default()));
        let count = Arc::new(AtomicUsize::new(0));
        let callbacks = Arc::new(Mutex::new(HashMap::from([(
            1,
            InterfaceUpdateCallback {
                on_change: Box::new({
                    let count = Arc::clone(&count);
                    move |trigger| {
                        assert_eq!(trigger, DeviceTriggerType::Keyboard);
                        count.fetch_add(1, Ordering::Relaxed);
                    }
                }),
            },
        )])));
        EmulatedDevices::set_keyboard_button(
            &status,
            &callbacks,
            &AtomicBool::new(false),
            &button_callback(true, false),
            native_keyboard::Keys::A as usize,
        );
        assert_eq!(status.lock().keyboard_state.key[0], 1 << 4);
        assert_eq!(count.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn toggle_key_changes_only_on_unlocked_press() {
        let status = Arc::new(Mutex::new(DeviceStatus::default()));
        let callbacks = Arc::new(Mutex::new(HashMap::new()));
        let configuring = AtomicBool::new(false);
        let index = native_keyboard::Keys::B as usize;
        for value in [true, true] {
            EmulatedDevices::set_keyboard_button(
                &status,
                &callbacks,
                &configuring,
                &button_callback(value, true),
                index,
            );
        }
        assert_ne!(status.lock().keyboard_state.key[0] & (1 << 5), 0);
        for value in [false, true] {
            EmulatedDevices::set_keyboard_button(
                &status,
                &callbacks,
                &configuring,
                &button_callback(value, true),
                index,
            );
        }
        assert_eq!(status.lock().keyboard_state.key[0] & (1 << 5), 0);
    }

    #[test]
    fn configuration_updates_raw_value_without_hid_state() {
        let status = Arc::new(Mutex::new(DeviceStatus::default()));
        let callbacks = Arc::new(Mutex::new(HashMap::new()));
        EmulatedDevices::set_mouse_button(
            &status,
            &callbacks,
            &AtomicBool::new(true),
            &button_callback(true, false),
            native_mouse_button::Values::Left as usize,
        );
        let status = status.lock();
        assert!(status.mouse_button_values[0].value);
        assert_eq!(status.mouse_button_state.raw, 0);
    }

    #[test]
    fn modifier_and_mouse_wheel_project_to_hid_state() {
        let status = Arc::new(Mutex::new(DeviceStatus::default()));
        let callbacks = Arc::new(Mutex::new(HashMap::new()));
        let configuring = AtomicBool::new(false);
        EmulatedDevices::set_keyboard_modifier(
            &status,
            &callbacks,
            &configuring,
            &button_callback(true, false),
            native_keyboard::Modifiers::LeftControl as usize,
        );

        let mut wheel = CallbackStatus {
            input_type: InputType::Analog,
            ..Default::default()
        };
        wheel.analog_status.raw_value = 3.75;
        EmulatedDevices::set_mouse_wheel(
            &status,
            &callbacks,
            &configuring,
            &wheel,
            native_mouse_wheel::Values::X as usize,
        );

        let status = status.lock();
        assert_eq!(status.keyboard_modifier_state.raw, 1);
        assert_eq!(status.mouse_wheel_values[0].value, 3.75);
        assert_eq!(status.mouse_wheel_state.x, 3);
    }

    #[test]
    fn callback_keys_start_at_one_like_eden() {
        let mut devices = EmulatedDevices::new();
        let first = devices.set_callback(InterfaceUpdateCallback {
            on_change: Box::new(|_| {}),
        });
        let second = devices.set_callback(InterfaceUpdateCallback {
            on_change: Box::new(|_| {}),
        });
        assert_eq!((first, second), (1, 2));
    }

    #[test]
    #[should_panic(expected = "Tried to delete non-existent callback")]
    fn deleting_unknown_callback_matches_eden_assertion() {
        EmulatedDevices::new().delete_callback(99);
    }
}
