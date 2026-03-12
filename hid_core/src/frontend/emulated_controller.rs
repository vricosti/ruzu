// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/frontend/emulated_controller.h and emulated_controller.cpp

use std::collections::HashMap;

use parking_lot::Mutex;

use crate::hid_types::*;
use crate::frontend::motion_input::IS_AT_REST_STANDARD;

pub const MAX_EMULATED_CONTROLLERS: usize = 2;
pub const OUTPUT_DEVICES_SIZE: usize = 5;

pub const HID_JOYSTICK_MAX: i32 = 0x7fff;
pub const HID_TRIGGER_MAX: i32 = 0x7fff;
pub const TURBO_BUTTON_DELAY: u32 = 4;

#[derive(Debug, Clone, Copy, Default)]
pub struct AnalogSticks {
    pub left: AnalogStickState,
    pub right: AnalogStickState,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ControllerColors {
    pub fullkey: NpadControllerColor,
    pub left: NpadControllerColor,
    pub right: NpadControllerColor,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct BatteryLevelState {
    pub dual: NpadPowerInfo,
    pub left: NpadPowerInfo,
    pub right: NpadPowerInfo,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct RingSensorForce {
    pub force: f32,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ControllerMotion {
    pub accel: Vec3f,
    pub gyro: Vec3f,
    pub rotation: Vec3f,
    pub euler: Vec3f,
    pub orientation: [Vec3f; 3],
    pub is_at_rest: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum EmulatedDeviceIndex {
    LeftIndex = 0,
    RightIndex = 1,
    DualIndex = 2,
    AllDevices = 3,
}

pub type MotionState = [ControllerMotion; 2];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ControllerTriggerType {
    Button,
    Stick,
    Trigger,
    Motion,
    Color,
    Battery,
    Vibration,
    IrSensor,
    RingController,
    Nfc,
    Connected,
    Disconnected,
    Type,
    All,
}

pub struct ControllerUpdateCallback {
    pub on_change: Box<dyn Fn(ControllerTriggerType) + Send + Sync>,
    pub is_npad_service: bool,
}

pub struct EmulatedController {
    npad_id_type: NpadIdType,
    npad_type: NpadStyleIndex,
    original_npad_type: NpadStyleIndex,
    supported_style_tag: NpadStyleTag,
    is_connected: bool,
    is_configuring: bool,
    is_initialized: bool,
    system_buttons_enabled: bool,
    motion_sensitivity: f32,
    turbo_button_state: u32,
    nfc_handles: usize,
    last_vibration_value: [VibrationValue; 2],

    // Temporary values to avoid doing changes while the controller is in configuring mode
    tmp_npad_type: NpadStyleIndex,
    tmp_is_connected: bool,

    mutex: Mutex<()>,
    callback_mutex: Mutex<()>,
    callback_list: HashMap<i32, ControllerUpdateCallback>,
    last_callback_key: i32,

    // Stores the current status of all controller input
    npad_button_state: NpadButtonState,
    analog_stick_state: AnalogSticks,
    motion_state: MotionState,
    colors_state: ControllerColors,
    battery_state: BatteryLevelState,
}

impl EmulatedController {
    pub fn new(npad_id_type: NpadIdType) -> Self {
        Self {
            npad_id_type,
            npad_type: NpadStyleIndex::None,
            original_npad_type: NpadStyleIndex::None,
            supported_style_tag: NpadStyleTag {
                raw: NpadStyleSet::ALL,
            },
            is_connected: false,
            is_configuring: false,
            is_initialized: false,
            system_buttons_enabled: true,
            motion_sensitivity: IS_AT_REST_STANDARD,
            turbo_button_state: 0,
            nfc_handles: 0,
            last_vibration_value: [DEFAULT_VIBRATION_VALUE; 2],
            tmp_npad_type: NpadStyleIndex::None,
            tmp_is_connected: false,
            mutex: Mutex::new(()),
            callback_mutex: Mutex::new(()),
            callback_list: HashMap::new(),
            last_callback_key: 0,
            npad_button_state: NpadButtonState::default(),
            analog_stick_state: AnalogSticks::default(),
            motion_state: [ControllerMotion::default(); 2],
            colors_state: ControllerColors::default(),
            battery_state: BatteryLevelState::default(),
        }
    }

    pub fn get_npad_id_type(&self) -> NpadIdType {
        self.npad_id_type
    }

    pub fn set_npad_style_index(&mut self, npad_type: NpadStyleIndex) {
        let _lock = self.mutex.lock();
        if self.is_configuring {
            self.tmp_npad_type = npad_type;
        } else {
            self.npad_type = npad_type;
        }
    }

    pub fn get_npad_style_index(&self, get_temporary_value: bool) -> NpadStyleIndex {
        let _lock = self.mutex.lock();
        if get_temporary_value && self.is_configuring {
            self.tmp_npad_type
        } else {
            self.npad_type
        }
    }

    pub fn set_supported_npad_style_tag(&mut self, supported_styles: NpadStyleTag) {
        self.supported_style_tag = supported_styles;
    }

    pub fn connect(&mut self, use_temporary_value: bool) {
        let _lock = self.mutex.lock();
        if use_temporary_value {
            self.tmp_is_connected = true;
        } else {
            self.is_connected = true;
        }
    }

    pub fn disconnect(&mut self) {
        let _lock = self.mutex.lock();
        self.is_connected = false;
    }

    pub fn is_connected(&self) -> bool {
        self.is_connected
    }

    pub fn unload_input(&mut self) {
        // TODO: reset all input devices
    }

    pub fn enable_configuration(&mut self) {
        self.is_configuring = true;
    }

    pub fn disable_configuration(&mut self) {
        self.is_configuring = false;
    }

    pub fn enable_system_buttons(&mut self) {
        self.system_buttons_enabled = true;
    }

    pub fn disable_system_buttons(&mut self) {
        self.system_buttons_enabled = false;
    }

    pub fn reset_system_buttons(&mut self) {
        // TODO: reset home and capture button states to false
    }

    pub fn is_configuring_mode(&self) -> bool {
        self.is_configuring
    }

    pub fn reload_input(&mut self) {
        todo!()
    }

    pub fn reload_from_settings(&mut self) {
        todo!()
    }

    pub fn get_npad_buttons(&self) -> NpadButtonState {
        let _lock = self.mutex.lock();
        self.npad_button_state
    }

    pub fn get_sticks(&self) -> AnalogSticks {
        let _lock = self.mutex.lock();
        self.analog_stick_state
    }

    pub fn get_motions(&self) -> MotionState {
        let _lock = self.mutex.lock();
        self.motion_state
    }

    pub fn get_colors(&self) -> ControllerColors {
        let _lock = self.mutex.lock();
        self.colors_state
    }

    pub fn get_battery(&self) -> BatteryLevelState {
        let _lock = self.mutex.lock();
        self.battery_state
    }

    pub fn get_led_pattern(&self) -> LedPattern {
        match self.npad_id_type {
            NpadIdType::Player1 => LedPattern::new(1, 0, 0, 0),
            NpadIdType::Player2 => LedPattern::new(1, 1, 0, 0),
            NpadIdType::Player3 => LedPattern::new(1, 1, 1, 0),
            NpadIdType::Player4 => LedPattern::new(1, 1, 1, 1),
            NpadIdType::Player5 => LedPattern::new(1, 0, 0, 1),
            NpadIdType::Player6 => LedPattern::new(1, 0, 1, 0),
            NpadIdType::Player7 => LedPattern::new(1, 0, 1, 1),
            NpadIdType::Player8 => LedPattern::new(0, 1, 1, 0),
            _ => LedPattern::new(0, 0, 0, 0),
        }
    }

    pub fn set_callback(&mut self, update_callback: ControllerUpdateCallback) -> i32 {
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

    fn trigger_on_change(&self, trigger_type: ControllerTriggerType, _is_service_update: bool) {
        let _lock = self.callback_mutex.lock();
        for (_key, callback) in &self.callback_list {
            (callback.on_change)(trigger_type);
        }
    }
}
