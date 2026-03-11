// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/lbl/lbl.cpp
//!
//! LBL (backlight) service ("lbl"). Most commands are implemented with state tracking.

/// IPC command IDs for LBL
pub mod commands {
    pub const SAVE_CURRENT_SETTING: u32 = 0;
    pub const LOAD_CURRENT_SETTING: u32 = 1;
    pub const SET_CURRENT_BRIGHTNESS_SETTING: u32 = 2;
    pub const GET_CURRENT_BRIGHTNESS_SETTING: u32 = 3;
    pub const APPLY_CURRENT_BRIGHTNESS_SETTING_TO_BACKLIGHT: u32 = 4;
    pub const GET_BRIGHTNESS_SETTING_APPLIED_TO_BACKLIGHT: u32 = 5;
    pub const SWITCH_BACKLIGHT_ON: u32 = 6;
    pub const SWITCH_BACKLIGHT_OFF: u32 = 7;
    pub const GET_BACKLIGHT_SWITCH_STATUS: u32 = 8;
    pub const ENABLE_DIMMING: u32 = 9;
    pub const DISABLE_DIMMING: u32 = 10;
    pub const IS_DIMMING_ENABLED: u32 = 11;
    pub const ENABLE_AUTO_BRIGHTNESS_CONTROL: u32 = 12;
    pub const DISABLE_AUTO_BRIGHTNESS_CONTROL: u32 = 13;
    pub const IS_AUTO_BRIGHTNESS_CONTROL_ENABLED: u32 = 14;
    pub const SET_AMBIENT_LIGHT_SENSOR_VALUE: u32 = 15;
    pub const GET_AMBIENT_LIGHT_SENSOR_VALUE: u32 = 16;
    pub const SET_BRIGHTNESS_REFLECTION_DELAY_LEVEL: u32 = 17;
    pub const GET_BRIGHTNESS_REFLECTION_DELAY_LEVEL: u32 = 18;
    pub const SET_CURRENT_BRIGHTNESS_MAPPING: u32 = 19;
    pub const GET_CURRENT_BRIGHTNESS_MAPPING: u32 = 20;
    pub const SET_CURRENT_AMBIENT_LIGHT_SENSOR_MAPPING: u32 = 21;
    pub const GET_CURRENT_AMBIENT_LIGHT_SENSOR_MAPPING: u32 = 22;
    pub const IS_AMBIENT_LIGHT_SENSOR_AVAILABLE: u32 = 23;
    pub const SET_CURRENT_BRIGHTNESS_SETTING_FOR_VR_MODE: u32 = 24;
    pub const GET_CURRENT_BRIGHTNESS_SETTING_FOR_VR_MODE: u32 = 25;
    pub const ENABLE_VR_MODE: u32 = 26;
    pub const DISABLE_VR_MODE: u32 = 27;
    pub const IS_VR_MODE_ENABLED: u32 = 28;
    pub const IS_AUTO_BRIGHTNESS_CONTROL_SUPPORTED: u32 = 29;
}

/// BacklightSwitchStatus enum. Upstream: `BacklightSwitchStatus` in `lbl.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum BacklightSwitchStatus {
    Off = 0,
    On = 1,
}

/// LBL service ("lbl").
///
/// Corresponds to `LBL` class in upstream `lbl.cpp`.
pub struct LBL {
    pub vr_mode_enabled: bool,
    pub current_brightness: f32,
    pub ambient_light_value: f32,
    pub current_vr_brightness: f32,
    pub dimming: bool,
    pub backlight_enabled: bool,
    pub update_instantly: bool,
    pub auto_brightness: bool,
    pub auto_brightness_supported: bool,
}

impl LBL {
    pub fn new() -> Self {
        Self {
            vr_mode_enabled: false,
            current_brightness: 1.0,
            ambient_light_value: 0.0,
            current_vr_brightness: 1.0,
            dimming: true,
            backlight_enabled: true,
            update_instantly: false,
            auto_brightness: false,
            auto_brightness_supported: true,
        }
    }

    pub fn save_current_setting(&self) {
        log::warn!("(STUBBED) LBL::save_current_setting called");
    }

    pub fn load_current_setting(&self) {
        log::warn!("(STUBBED) LBL::load_current_setting called");
    }

    pub fn set_current_brightness_setting(&mut self, mut brightness: f32) {
        if !brightness.is_finite() {
            log::error!("LBL: Brightness is infinite!");
            brightness = 0.0;
        }
        log::debug!("LBL::set_current_brightness_setting called, brightness={}", brightness);
        self.current_brightness = brightness;
        self.update_instantly = true;
    }

    pub fn get_current_brightness_setting(&self) -> f32 {
        let mut brightness = self.current_brightness;
        if !brightness.is_finite() {
            log::error!("LBL: Brightness is infinite!");
            brightness = 0.0;
        }
        log::debug!("LBL::get_current_brightness_setting called, brightness={}", brightness);
        brightness
    }

    pub fn switch_backlight_on(&mut self, fade_time: u64) {
        log::warn!("(STUBBED) LBL::switch_backlight_on called, fade_time={}", fade_time);
        self.backlight_enabled = true;
    }

    pub fn switch_backlight_off(&mut self, fade_time: u64) {
        log::warn!("(STUBBED) LBL::switch_backlight_off called, fade_time={}", fade_time);
        self.backlight_enabled = false;
    }

    pub fn get_backlight_switch_status(&self) -> BacklightSwitchStatus {
        if self.backlight_enabled {
            BacklightSwitchStatus::On
        } else {
            BacklightSwitchStatus::Off
        }
    }

    pub fn enable_dimming(&mut self) {
        log::debug!("LBL::enable_dimming called");
        self.dimming = true;
    }

    pub fn disable_dimming(&mut self) {
        log::debug!("LBL::disable_dimming called");
        self.dimming = false;
    }

    pub fn is_dimming_enabled(&self) -> bool {
        self.dimming
    }

    pub fn enable_auto_brightness_control(&mut self) {
        log::debug!("LBL::enable_auto_brightness_control called");
        self.auto_brightness = true;
        self.update_instantly = true;
    }

    pub fn disable_auto_brightness_control(&mut self) {
        log::debug!("LBL::disable_auto_brightness_control called");
        self.auto_brightness = false;
    }

    pub fn is_auto_brightness_control_enabled(&self) -> bool {
        self.auto_brightness
    }

    pub fn set_ambient_light_sensor_value(&mut self, light_value: f32) {
        log::debug!("LBL::set_ambient_light_sensor_value called, light_value={}", light_value);
        self.ambient_light_value = light_value;
    }

    pub fn get_ambient_light_sensor_value(&self) -> f32 {
        self.ambient_light_value
    }

    pub fn set_brightness_reflection_delay_level(&self) {
        // Intentional no-op, matches upstream
        log::debug!("LBL::set_brightness_reflection_delay_level called");
    }

    pub fn get_brightness_reflection_delay_level(&self) -> f32 {
        // Intentional: hard coded to return 0.0f on hardware
        0.0
    }

    pub fn set_current_brightness_mapping(&self) {
        // Intentional no-op
        log::debug!("LBL::set_current_brightness_mapping called");
    }

    pub fn get_current_brightness_mapping(&self) {
        // Intentional no-op
        log::debug!("LBL::get_current_brightness_mapping called");
    }

    pub fn set_current_ambient_light_sensor_mapping(&self) {
        // Intentional no-op
        log::debug!("LBL::set_current_ambient_light_sensor_mapping called");
    }

    pub fn get_current_ambient_light_sensor_mapping(&self) {
        // Intentional no-op
        log::debug!("LBL::get_current_ambient_light_sensor_mapping called");
    }

    pub fn is_ambient_light_sensor_available(&self) -> bool {
        log::warn!("(STUBBED) LBL::is_ambient_light_sensor_available called");
        true
    }

    pub fn set_current_brightness_setting_for_vr_mode(&mut self, mut brightness: f32) {
        if !brightness.is_finite() {
            log::error!("LBL: Brightness is infinite!");
            brightness = 0.0;
        }
        log::debug!("LBL::set_current_brightness_setting_for_vr_mode called, brightness={}", brightness);
        self.current_vr_brightness = brightness;
    }

    pub fn get_current_brightness_setting_for_vr_mode(&self) -> f32 {
        let mut brightness = self.current_vr_brightness;
        if !brightness.is_finite() {
            log::error!("LBL: Brightness is infinite!");
            brightness = 0.0;
        }
        brightness
    }

    pub fn enable_vr_mode(&mut self) {
        log::debug!("LBL::enable_vr_mode called");
        self.vr_mode_enabled = true;
    }

    pub fn disable_vr_mode(&mut self) {
        log::debug!("LBL::disable_vr_mode called");
        self.vr_mode_enabled = false;
    }

    pub fn is_vr_mode_enabled(&self) -> bool {
        self.vr_mode_enabled
    }

    pub fn is_auto_brightness_control_supported(&self) -> bool {
        self.auto_brightness_supported
    }
}

/// Registers "lbl" service.
///
/// Corresponds to `LoopProcess` in upstream `lbl.cpp`.
pub fn loop_process() {
    // TODO: register "lbl" -> LBL with ServerManager
}
