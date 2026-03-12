// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/applet_resource.h and applet_resource.cpp

use common::ResultCode;

pub const ARUID_INDEX_MAX: usize = 0x20;
pub const SYSTEM_ARUID: u64 = 0;

/// This is RegistrationStatus
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum RegistrationStatus {
    #[default]
    None = 0,
    Initialized = 1,
    PendingDelete = 2,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct DataStatusFlag {
    pub raw: u32,
}

impl DataStatusFlag {
    pub fn is_initialized(&self) -> bool {
        (self.raw & (1 << 0)) != 0
    }
    pub fn is_assigned(&self) -> bool {
        (self.raw & (1 << 1)) != 0
    }
    pub fn enable_pad_input(&self) -> bool {
        (self.raw & (1 << 16)) != 0
    }
    pub fn enable_six_axis_sensor(&self) -> bool {
        (self.raw & (1 << 17)) != 0
    }
    pub fn enable_touchscreen(&self) -> bool {
        (self.raw & (1 << 21)) != 0
    }
}

#[derive(Debug, Clone, Default)]
pub struct AruidRegisterList {
    pub flag: [RegistrationStatus; ARUID_INDEX_MAX],
    pub aruid: [u64; ARUID_INDEX_MAX],
}

pub struct HandheldConfig {
    pub is_handheld_hid_enabled: bool,
    pub is_force_handheld: bool,
    pub is_joycon_rail_enabled: bool,
    pub is_force_handheld_style_vibration: bool,
}

pub struct AppletResource {
    active_aruid: u64,
    registration_list: AruidRegisterList,
    ref_counter: i32,
    active_vibration_aruid: u64,
}

impl AppletResource {
    pub fn new() -> Self {
        Self {
            active_aruid: 0,
            registration_list: AruidRegisterList::default(),
            ref_counter: 0,
            active_vibration_aruid: 0,
        }
    }

    pub fn create_applet_resource(&mut self, _aruid: u64) -> ResultCode {
        todo!()
    }

    pub fn register_applet_resource_user_id(
        &mut self,
        _aruid: u64,
        _enable_input: bool,
    ) -> ResultCode {
        todo!()
    }

    pub fn unregister_applet_resource_user_id(&mut self, _aruid: u64) {
        todo!()
    }

    pub fn free_applet_resource_id(&mut self, _aruid: u64) {
        todo!()
    }

    pub fn get_active_aruid(&self) -> u64 {
        self.active_aruid
    }

    pub fn is_vibration_aruid_active(&self, _aruid: u64) -> bool {
        todo!()
    }

    pub fn enable_input(&mut self, _aruid: u64, _is_enabled: bool) {
        todo!()
    }

    pub fn set_aruid_valid_for_vibration(&mut self, _aruid: u64, _is_enabled: bool) -> bool {
        todo!()
    }

    pub fn enable_six_axis_sensor(&mut self, _aruid: u64, _is_enabled: bool) {
        todo!()
    }

    pub fn enable_pad_input(&mut self, _aruid: u64, _is_enabled: bool) {
        todo!()
    }

    pub fn enable_touch_screen(&mut self, _aruid: u64, _is_enabled: bool) {
        todo!()
    }

    pub fn register_core_applet_resource(&mut self) -> ResultCode {
        todo!()
    }

    pub fn unregister_core_applet_resource(&mut self) -> ResultCode {
        todo!()
    }
}

impl Default for AppletResource {
    fn default() -> Self {
        Self::new()
    }
}
