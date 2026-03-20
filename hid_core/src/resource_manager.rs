// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resource_manager.h and hid_core/resource_manager.cpp

use std::sync::Arc;
use std::time::Duration;

use parking_lot::Mutex;

use common::ResultCode;

use crate::hid_types::*;
use crate::hid_result;
use crate::hid_util;
use crate::resources::applet_resource::{AppletResource, SYSTEM_ARUID};
use crate::resources::hid_firmware_settings::HidFirmwareSettings;

/// Updating period for each HID device.
/// Period time is obtained by measuring the number of samples in a second on HW using a homebrew
/// Correct npad_update_ns is 4ms this is overclocked to lower input lag
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
    // Upstream holds shared_ptr to all HID resource objects:
    // npad, debug_pad, mouse, debug_mouse, keyboard, unique_pad, home_button, sleep_button,
    // capture_button, digitizer, palma, six_axis, seven_six_axis, console_six_axis,
    // touch_screen, touch_resource, touch_driver, gesture.
    // These require their respective resource types to be ported. Fields should be added
    // here as each resource is ported.
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

    // Upstream has getter methods for all resources: GetCaptureButton, GetConsoleSixAxis,
    // GetDebugMouse, GetDebugPad, GetDigitizer, GetGesture, GetHomeButton, GetKeyboard,
    // GetMouse, GetNpad, GetPalma, GetSevenSixAxis, GetSixAxis, GetSleepButton,
    // GetTouchScreen, GetUniquePad. These should be added as each resource is ported.

    pub fn create_applet_resource(&self, aruid: u64) -> ResultCode {
        if aruid == SYSTEM_ARUID {
            let result = self.register_core_applet_resource();
            if result.is_error() {
                return result;
            }
            // Upstream calls GetNpad()->ActivateNpadResource() here.
            // Requires NPad resource integration which is not yet available.
            return ResultCode::SUCCESS;
        }

        let result = self.create_applet_resource_impl(aruid);
        if result.is_error() {
            return result;
        }

        // Homebrew doesn't try to activate some controllers, so we activate them by default.
        // Upstream calls: npad->Activate(), six_axis->Activate(), touch_screen->Activate(),
        // gesture->Activate(). Requires these resource types to be ported.

        // Upstream calls GetNpad()->ActivateNpadResource(aruid).
        // Requires NPad resource integration which is not yet available.
        ResultCode::SUCCESS
    }

    fn create_applet_resource_impl(&self, aruid: u64) -> ResultCode {
        let _lock = self.shared_mutex.write();
        if let Some(ref resource) = self.applet_resource {
            resource.lock().create_applet_resource(aruid)
        } else {
            ResultCode::SUCCESS
        }
    }

    pub fn register_core_applet_resource(&self) -> ResultCode {
        let _lock = self.shared_mutex.write();
        if let Some(ref resource) = self.applet_resource {
            resource.lock().register_core_applet_resource()
        } else {
            ResultCode::SUCCESS
        }
    }

    pub fn unregister_core_applet_resource(&self) -> ResultCode {
        let _lock = self.shared_mutex.write();
        if let Some(ref resource) = self.applet_resource {
            resource.lock().unregister_core_applet_resource()
        } else {
            ResultCode::SUCCESS
        }
    }

    pub fn register_applet_resource_user_id(&self, aruid: u64, enable_input: bool) -> ResultCode {
        let _lock = self.shared_mutex.write();
        if let Some(ref resource) = self.applet_resource {
            let result = resource.lock().register_applet_resource_user_id(aruid, enable_input);
            if result.is_error() {
                return result;
            }
            // Upstream calls npad->RegisterAppletResourceUserId(aruid).
            // Requires NPad resource integration which is not yet available.
        }
        ResultCode::SUCCESS
    }

    pub fn unregister_applet_resource_user_id(&self, aruid: u64) {
        let _lock = self.shared_mutex.write();
        if let Some(ref resource) = self.applet_resource {
            resource.lock().unregister_applet_resource_user_id(aruid);
        }
        // Upstream calls npad->UnregisterAppletResourceUserId(aruid).
        // Requires NPad resource integration which is not yet available.
    }

    pub fn free_applet_resource_id(&self, aruid: u64) {
        let _lock = self.shared_mutex.write();
        if let Some(ref resource) = self.applet_resource {
            resource.lock().free_applet_resource_id(aruid);
        }
        // Upstream calls npad->FreeAppletResourceId(aruid).
        // Requires NPad resource integration which is not yet available.
    }

    pub fn enable_input(&self, aruid: u64, is_enabled: bool) {
        let _lock = self.shared_mutex.read();
        if let Some(ref resource) = self.applet_resource {
            resource.lock().enable_input(aruid, is_enabled);
        }
    }

    pub fn enable_six_axis_sensor(&self, aruid: u64, is_enabled: bool) {
        let _lock = self.shared_mutex.read();
        if let Some(ref resource) = self.applet_resource {
            resource.lock().enable_six_axis_sensor(aruid, is_enabled);
        }
    }

    pub fn enable_pad_input(&self, aruid: u64, is_enabled: bool) {
        let _lock = self.shared_mutex.read();
        if let Some(ref resource) = self.applet_resource {
            resource.lock().enable_pad_input(aruid, is_enabled);
        }
    }

    pub fn enable_touch_screen(&self, aruid: u64, is_enabled: bool) {
        let _lock = self.shared_mutex.read();
        if let Some(ref resource) = self.applet_resource {
            resource.lock().enable_touch_screen(aruid, is_enabled);
        }
    }

    pub fn set_aruid_valid_for_vibration(&self, aruid: u64, is_enabled: bool) -> ResultCode {
        let _lock = self.shared_mutex.write();
        if let Some(ref resource) = self.applet_resource {
            let _has_changed = resource.lock().set_aruid_valid_for_vibration(aruid, is_enabled);
            // Upstream: if has_changed, iterates npad->GetAllVibrationDevices() (but the
            // loop body is an upstream TODO). Also checks vibration_handler session aruid.
            // Requires NPad vibration device integration which is not yet available.
        }
        ResultCode::SUCCESS
    }

    pub fn set_force_handheld_style_vibration(&self, is_forced: bool) {
        if let Some(ref config) = self.handheld_config {
            config.lock().is_force_handheld_style_vibration = is_forced;
        }
    }

    pub fn is_vibration_aruid_active(&self, aruid: u64) -> Result<bool, ResultCode> {
        let _lock = self.shared_mutex.read();
        if let Some(ref resource) = self.applet_resource {
            Ok(resource.lock().is_vibration_aruid_active(aruid))
        } else {
            Ok(false)
        }
    }

    /// Port of ResourceManager::GetVibrationDeviceInfo.
    pub fn get_vibration_device_info(
        &self,
        handle: &VibrationDeviceHandle,
    ) -> Result<VibrationDeviceInfo, ResultCode> {
        let is_valid = hid_util::is_vibration_handle_valid(handle);
        if is_valid.is_error() {
            return Err(is_valid);
        }

        let mut check_device_index = false;

        let device_type = match handle.npad_type {
            NpadStyleIndex::Fullkey
            | NpadStyleIndex::Handheld
            | NpadStyleIndex::JoyconDual
            | NpadStyleIndex::JoyconLeft
            | NpadStyleIndex::JoyconRight => {
                check_device_index = true;
                VibrationDeviceType::LinearResonantActuator
            }
            NpadStyleIndex::GameCube => VibrationDeviceType::GcErm,
            NpadStyleIndex::N64 => VibrationDeviceType::N64,
            _ => VibrationDeviceType::Unknown,
        };

        let position = if check_device_index {
            match handle.device_index {
                DeviceIndex::Left => VibrationDevicePosition::Left,
                DeviceIndex::Right => VibrationDevicePosition::Right,
                _ => {
                    log::error!("DeviceIndex should never be None!");
                    VibrationDevicePosition::None
                }
            }
        } else {
            VibrationDevicePosition::None
        };

        Ok(VibrationDeviceInfo {
            device_type,
            position,
        })
    }

    /// Port of ResourceManager::GetTouchScreenFirmwareVersion.
    pub fn get_touch_screen_firmware_version(&self) -> Result<FirmwareVersion, ResultCode> {
        Ok(FirmwareVersion::default())
    }

    pub fn update_controllers(&self, _ns_late: Duration) {
        // Upstream calls: debug_pad->OnUpdate, digitizer->OnUpdate, unique_pad->OnUpdate,
        // palma->OnUpdate, home_button->OnUpdate, sleep_button->OnUpdate,
        // capture_button->OnUpdate, passing core_timing to each.
        // Requires these resource types to be ported.
    }

    pub fn update_npad(&self, _ns_late: Duration) {
        // Upstream calls npad->OnUpdate(core_timing).
        // Requires NPad resource integration which is not yet available.
    }

    pub fn update_mouse_keyboard(&self, _ns_late: Duration) {
        // Upstream calls: mouse->OnUpdate, debug_mouse->OnUpdate, keyboard->OnUpdate,
        // passing core_timing to each.
        // Requires these resource types to be ported.
    }

    pub fn update_motion(&self, _ns_late: Duration) {
        // Upstream calls: six_axis->OnUpdate, seven_six_axis->OnUpdate,
        // console_six_axis->OnUpdate, passing core_timing to each.
        // Requires these resource types to be ported.
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
        // Upstream creates: debug_pad, mouse, debug_mouse, keyboard, unique_pad, npad,
        // home_button, sleep_button, capture_button, digitizer, palma, six_axis.
        // Then wires them to applet_resource, shared_mutex, handheld_config, input_event,
        // and schedules looping timing events (npad_update, default_update, mouse_keyboard_update,
        // motion_update). Requires these resource types and CoreTiming to be ported.
    }

    fn initialize_touch_screen_sampler(&mut self) {
        // Upstream creates: touch_resource, touch_driver, touch_screen, gesture.
        // Sets up a touch_update_event looping timer and wires touch_resource to
        // touch_driver, applet_resource, input_event, handheld_config, and timer_event.
        // Requires these resource types and CoreTiming to be ported.
    }

    fn initialize_console_six_axis_sampler(&mut self) {
        // Upstream creates: console_six_axis, seven_six_axis.
        // Wires console_six_axis to applet_resource and shared_mutex.
        // Requires these resource types to be ported.
    }

    fn initialize_ahid_sampler(&mut self) {
        // Upstream TODO: not yet implemented in C++ upstream
    }
}
