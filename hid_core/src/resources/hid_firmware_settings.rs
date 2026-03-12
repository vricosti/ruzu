// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/hid_firmware_settings.h and hid_firmware_settings.cpp

/// Loads firmware config from nn::settings::fwdbg
pub struct HidFirmwareSettings {
    is_initialized: bool,
    is_debug_pad_enabled: bool,
    is_device_managed: bool,
    is_touch_i2c_managed: bool,
    is_future_devices_emulated: bool,
    is_mcu_hardware_error_emulated: bool,
    is_rail_enabled: bool,
    is_firmware_update_failure_emulated: bool,
    is_ble_disabled: bool,
    is_dscale_disabled: bool,
    is_handheld_forced: bool,
    is_touch_firmware_auto_update_disabled: bool,
}

impl HidFirmwareSettings {
    pub fn new() -> Self {
        Self {
            is_initialized: false,
            is_debug_pad_enabled: false,
            is_device_managed: true,
            is_touch_i2c_managed: true,
            is_future_devices_emulated: false,
            is_mcu_hardware_error_emulated: false,
            is_rail_enabled: true,
            is_firmware_update_failure_emulated: false,
            is_ble_disabled: false,
            is_dscale_disabled: false,
            is_handheld_forced: false,
            is_touch_firmware_auto_update_disabled: false,
        }
    }

    pub fn reload(&mut self) {
        todo!()
    }

    pub fn is_debug_pad_enabled(&self) -> bool { self.is_debug_pad_enabled }
    pub fn is_device_managed(&self) -> bool { self.is_device_managed }
    pub fn is_emulate_future_device(&self) -> bool { self.is_future_devices_emulated }
    pub fn is_touch_i2c_managed(&self) -> bool { self.is_touch_i2c_managed }
    pub fn is_handheld_forced(&self) -> bool { self.is_handheld_forced }
    pub fn is_rail_enabled(&self) -> bool { self.is_rail_enabled }
    pub fn is_hardware_error_emulated(&self) -> bool { self.is_mcu_hardware_error_emulated }
    pub fn is_ble_disabled(&self) -> bool { self.is_ble_disabled }
    pub fn is_dscale_disabled(&self) -> bool { self.is_dscale_disabled }
    pub fn is_touch_auto_update_disabled(&self) -> bool { self.is_touch_firmware_auto_update_disabled }
}

impl Default for HidFirmwareSettings {
    fn default() -> Self {
        Self::new()
    }
}
