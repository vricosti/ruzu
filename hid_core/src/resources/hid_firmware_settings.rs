// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/hid_firmware_settings.h and hid_firmware_settings.cpp

/// FirmwareSetting placeholder for firmware update failure state
#[derive(Debug, Clone, Copy, Default)]
pub struct FirmwareSetting {
    pub raw: u32,
}

/// FeaturesPerId placeholder for per-id feature disable flags
#[derive(Debug, Clone, Copy, Default)]
pub struct FeaturesPerId {
    pub raw: u32,
}

/// PlatformConfig from nn::settings::system
#[derive(Debug, Clone, Copy, Default)]
pub struct PlatformConfig {
    pub raw: u32,
}

impl PlatformConfig {
    pub fn has_rail_interface(&self) -> bool {
        (self.raw & (1 << 0)) != 0
    }

    pub fn has_sio_mcu(&self) -> bool {
        (self.raw & (1 << 1)) != 0
    }

    pub fn set_has_rail_interface(&mut self, value: bool) {
        if value {
            self.raw |= 1 << 0;
        } else {
            self.raw &= !(1 << 0);
        }
    }

    pub fn set_has_sio_mcu(&mut self, value: bool) {
        if value {
            self.raw |= 1 << 1;
        } else {
            self.raw &= !(1 << 1);
        }
    }
}

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
    is_firmware_update_failure: FirmwareSetting,
    is_ble_disabled: bool,
    is_dscale_disabled: bool,
    is_handheld_forced: bool,
    is_touch_firmware_auto_update_disabled: bool,
    features_per_id_disabled: FeaturesPerId,
    platform_config: PlatformConfig,
}

impl HidFirmwareSettings {
    pub fn new() -> Self {
        let mut settings = Self {
            is_initialized: false,
            is_debug_pad_enabled: false,
            is_device_managed: true,
            is_touch_i2c_managed: true,
            is_future_devices_emulated: false,
            is_mcu_hardware_error_emulated: false,
            is_rail_enabled: true,
            is_firmware_update_failure_emulated: false,
            is_firmware_update_failure: FirmwareSetting::default(),
            is_ble_disabled: false,
            is_dscale_disabled: false,
            is_handheld_forced: false,
            is_touch_firmware_auto_update_disabled: false,
            features_per_id_disabled: FeaturesPerId::default(),
            platform_config: PlatformConfig::default(),
        };
        settings.load_settings(true);
        settings
    }

    pub fn reload(&mut self) {
        self.load_settings(true);
    }

    fn load_settings(&mut self, reload_config: bool) {
        if self.is_initialized && !reload_config {
            return;
        }

        // In the upstream C++, these values are loaded from system settings service.
        // Since we don't have the system settings service available here,
        // we use sensible defaults matching typical emulator configuration.
        // The defaults are already set in the constructor.

        self.is_firmware_update_failure = FirmwareSetting::default();
        self.features_per_id_disabled = FeaturesPerId::default();

        self.is_initialized = true;
    }

    pub fn is_debug_pad_enabled(&self) -> bool {
        self.is_debug_pad_enabled
    }

    pub fn is_device_managed(&self) -> bool {
        self.is_device_managed
    }

    pub fn is_emulate_future_device(&self) -> bool {
        self.is_future_devices_emulated
    }

    pub fn is_touch_i2c_managed(&self) -> bool {
        self.is_touch_i2c_managed
    }

    pub fn is_handheld_forced(&self) -> bool {
        self.is_handheld_forced
    }

    pub fn is_rail_enabled(&self) -> bool {
        self.is_rail_enabled
    }

    pub fn is_hardware_error_emulated(&self) -> bool {
        self.is_mcu_hardware_error_emulated
    }

    pub fn is_ble_disabled(&self) -> bool {
        self.is_ble_disabled
    }

    pub fn is_dscale_disabled(&self) -> bool {
        self.is_dscale_disabled
    }

    pub fn is_touch_auto_update_disabled(&self) -> bool {
        self.is_touch_firmware_auto_update_disabled
    }

    pub fn get_firmware_update_failure(&self) -> FirmwareSetting {
        self.is_firmware_update_failure
    }

    pub fn features_disabled_per_id(&self) -> FeaturesPerId {
        self.features_per_id_disabled
    }

    pub fn get_platform_config(&self) -> PlatformConfig {
        self.platform_config
    }
}

impl Default for HidFirmwareSettings {
    fn default() -> Self {
        Self::new()
    }
}
