// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resource_manager.h and hid_core/resource_manager.cpp

use std::sync::Arc;
use std::time::Duration;

use parking_lot::Mutex;

use crate::hid_types::*;
use crate::resources::applet_resource::AppletResource;
use crate::resources::hid_firmware_settings::HidFirmwareSettings;

/// Updating period for each HID device.
pub const NPAD_UPDATE_NS: Duration = Duration::from_nanos(1_000_000); // 1ms, 1000Hz
pub const DEFAULT_UPDATE_NS: Duration = Duration::from_nanos(4_000_000); // 4ms, 250Hz
pub const MOUSE_KEYBOARD_UPDATE_NS: Duration = Duration::from_nanos(8_000_000); // 8ms, 125Hz
pub const MOTION_UPDATE_NS: Duration = Duration::from_nanos(5_000_000); // 5ms, 200Hz

/// Handheld configuration state
pub struct HandheldConfig {
    pub is_handheld_hid_enabled: bool,
    pub is_force_handheld: bool,
    pub is_joycon_rail_enabled: bool,
    pub is_force_handheld_style_vibration: bool,
}

pub struct ResourceManager {
    is_initialized: bool,
    shared_mutex: parking_lot::RwLock<()>,
    applet_resource: Option<Arc<Mutex<AppletResource>>>,
    handheld_config: Option<Arc<Mutex<HandheldConfig>>>,
    firmware_settings: Option<Arc<HidFirmwareSettings>>,
    // TODO: Add all resource fields (npad, debug_pad, mouse, keyboard, etc.)
}

impl ResourceManager {
    pub fn new(firmware_settings: Arc<HidFirmwareSettings>) -> Self {
        Self {
            is_initialized: false,
            shared_mutex: parking_lot::RwLock::new(()),
            applet_resource: Some(Arc::new(Mutex::new(AppletResource::new()))),
            handheld_config: None,
            firmware_settings: Some(firmware_settings),
        }
    }

    pub fn initialize(&mut self) {
        if self.is_initialized {
            return;
        }

        self.initialize_handheld_config();
        self.initialize_hid_common_sampler();
        self.initialize_touch_screen_sampler();
        self.initialize_console_six_axis_sampler();
        self.initialize_ahid_sampler();

        self.is_initialized = true;
    }

    pub fn get_applet_resource(&self) -> Option<Arc<Mutex<AppletResource>>> {
        self.applet_resource.clone()
    }

    // TODO: Add all getter methods for resources (get_capture_button, get_npad, etc.)

    pub fn enable_input(&self, _aruid: u64, _is_enabled: bool) {
        let _lock = self.shared_mutex.read();
        // TODO: applet_resource.enable_input(aruid, is_enabled)
        todo!()
    }

    pub fn enable_six_axis_sensor(&self, _aruid: u64, _is_enabled: bool) {
        let _lock = self.shared_mutex.read();
        todo!()
    }

    pub fn enable_pad_input(&self, _aruid: u64, _is_enabled: bool) {
        let _lock = self.shared_mutex.read();
        todo!()
    }

    pub fn enable_touch_screen(&self, _aruid: u64, _is_enabled: bool) {
        let _lock = self.shared_mutex.read();
        todo!()
    }

    pub fn update_controllers(&self, _ns_late: Duration) {
        todo!()
    }

    pub fn update_npad(&self, _ns_late: Duration) {
        todo!()
    }

    pub fn update_mouse_keyboard(&self, _ns_late: Duration) {
        todo!()
    }

    pub fn update_motion(&self, _ns_late: Duration) {
        todo!()
    }

    fn initialize_handheld_config(&mut self) {
        let mut config = HandheldConfig {
            is_handheld_hid_enabled: true,
            is_joycon_rail_enabled: true,
            is_force_handheld_style_vibration: false,
            is_force_handheld: false,
        };
        if let Some(ref fw) = self.firmware_settings {
            if fw.is_handheld_forced() {
                config.is_joycon_rail_enabled = false;
            }
        }
        self.handheld_config = Some(Arc::new(Mutex::new(config)));
    }

    fn initialize_hid_common_sampler(&mut self) {
        // TODO: Create all HID common samplers (debug_pad, mouse, keyboard, npad, etc.)
    }

    fn initialize_touch_screen_sampler(&mut self) {
        // TODO: Create touch screen sampler resources
    }

    fn initialize_console_six_axis_sampler(&mut self) {
        // TODO: Create console six axis sampler resources
    }

    fn initialize_ahid_sampler(&mut self) {
        // TODO
    }
}
