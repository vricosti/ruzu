// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/frontend/emulated_controller.h and emulated_controller.cpp

use std::collections::HashMap;

use parking_lot::Mutex;

use crate::frontend::motion_input::IS_AT_REST_STANDARD;
use crate::hid_types::*;

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
    home_button_state: HomeButtonState,
    capture_button_state: CaptureButtonState,
    npad_button_state: NpadButtonState,
    debug_pad_button_state: DebugPadButton,
    analog_stick_state: AnalogSticks,
    motion_state: MotionState,
    gc_trigger_state: NpadGcTriggerState,
    colors_state: ControllerColors,
    battery_state: BatteryLevelState,
    ring_analog_state: RingSensorForce,
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
            home_button_state: HomeButtonState::default(),
            capture_button_state: CaptureButtonState::default(),
            npad_button_state: NpadButtonState::default(),
            debug_pad_button_state: DebugPadButton::default(),
            analog_stick_state: AnalogSticks::default(),
            motion_state: [ControllerMotion::default(); 2],
            gc_trigger_state: NpadGcTriggerState::default(),
            colors_state: ControllerColors::default(),
            battery_state: BatteryLevelState::default(),
            ring_analog_state: RingSensorForce::default(),
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
        // Upstream (emulated_controller.cpp UnloadInput) sets is_initialized = false, then
        // resets all device arrays by calling .reset() on each unique_ptr:
        // button_devices, stick_devices, motion_devices, trigger_devices, battery_devices,
        // output_devices, tas_button_devices, tas_stick_devices, virtual_button_devices,
        // virtual_stick_devices, virtual_motion_devices, camera_devices, ring_analog_devices,
        // nfc_devices. Each is a std::unique_ptr<Common::Input::InputDevice> from
        // zuyu/src/common/input.h. Depends on Common::Input::InputDevice arrays.
        self.is_initialized = false;
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
        let _lock = self.mutex.lock();
        self.home_button_state = HomeButtonState::default();
        self.capture_button_state = CaptureButtonState::default();
    }

    pub fn is_configuring_mode(&self) -> bool {
        self.is_configuring
    }

    pub fn reload_input(&mut self) {
        // Upstream (emulated_controller.cpp ReloadInput) creates input devices via
        // Common::Input::CreateInputDevice for each parameter set (button_params,
        // stick_params, motion_params, trigger_params, battery_params, camera_params,
        // ring_params, nfc_params), plus TAS and virtual device variants using TAS_UUID
        // and VIRTUAL_UUID. Sets callbacks (SetButton, SetStick, SetTrigger, SetMotion,
        // SetBattery, SetCamera, SetRingAnalog, SetNfc) on each device. Also creates
        // output_devices[OUTPUT_DEVICES_SIZE] for vibration/LED/polling. Finally restores
        // motion state via MotionInput::ResetRotations/ResetQuaternion. Depends on
        // Common::Input::CreateInputDevice from zuyu/src/common/input.h and
        // Settings::values.players from zuyu/src/common/settings.h.
        log::debug!(
            "EmulatedController::reload_input called for {:?}",
            self.npad_id_type
        );
    }

    pub fn reload_from_settings(&mut self) {
        // Upstream (emulated_controller.cpp ReloadFromSettings) reads player config from
        // Settings::values.players.GetValue()[player_index] where player_index is derived
        // from npad_id_type via NpadIdTypeToIndex. Maps Settings::ControllerType to
        // NpadStyleIndex via MapSettingsTypeToNPad, then populates button_params,
        // stick_params, motion_params from player.buttons/analogs/motions ParamPackage
        // strings. Calls SetNpadStyleIndex, connect/disconnect based on player.connected,
        // then ReloadInput(). Depends on Settings::values.players (PlayerInput struct)
        // from zuyu/src/common/settings.h and NpadIdTypeToIndex from hid_core/hid_util.h.
        log::debug!(
            "EmulatedController::reload_from_settings called for {:?}",
            self.npad_id_type
        );
    }

    /// Port of EmulatedController::SaveCurrentConfig.
    pub fn save_current_config(&self) {
        // Upstream saves current parameters to Settings::values.players[player_index].
        // Requires Settings integration.
        log::debug!(
            "EmulatedController::save_current_config called for {:?}",
            self.npad_id_type
        );
    }

    /// Port of EmulatedController::RestoreConfig.
    pub fn restore_config(&mut self) {
        // Upstream reloads from settings and reconnects if needed.
        log::debug!(
            "EmulatedController::restore_config called for {:?}",
            self.npad_id_type
        );
    }

    /// Port of EmulatedController::ReloadColorsFromSettings.
    pub fn reload_colors_from_settings(&mut self) {
        // Upstream reads body_color_left/right from player settings.
        log::debug!(
            "EmulatedController::reload_colors_from_settings called for {:?}",
            self.npad_id_type
        );
    }

    /// Port of EmulatedController::IsControllerFullkey.
    fn is_controller_fullkey(&self, use_temporary_value: bool) -> bool {
        let npad = if self.is_configuring && use_temporary_value {
            self.tmp_npad_type
        } else {
            self.npad_type
        };
        matches!(
            npad,
            NpadStyleIndex::Fullkey
                | NpadStyleIndex::GameCube
                | NpadStyleIndex::Pokeball
                | NpadStyleIndex::NES
                | NpadStyleIndex::SNES
                | NpadStyleIndex::N64
                | NpadStyleIndex::SegaGenesis
        )
    }

    /// Port of EmulatedController::IsControllerSupported.
    fn is_controller_supported(&self, use_temporary_value: bool) -> bool {
        let npad = if self.is_configuring && use_temporary_value {
            self.tmp_npad_type
        } else {
            self.npad_type
        };
        let styles = self.supported_style_tag.raw;
        match npad {
            NpadStyleIndex::Fullkey => styles.contains(NpadStyleSet::FULLKEY),
            NpadStyleIndex::Handheld => styles.contains(NpadStyleSet::HANDHELD),
            NpadStyleIndex::JoyconDual => styles.contains(NpadStyleSet::JOY_DUAL),
            NpadStyleIndex::JoyconLeft => styles.contains(NpadStyleSet::JOY_LEFT),
            NpadStyleIndex::JoyconRight => styles.contains(NpadStyleSet::JOY_RIGHT),
            NpadStyleIndex::GameCube => styles.contains(NpadStyleSet::GC),
            NpadStyleIndex::Pokeball => styles.contains(NpadStyleSet::PALMA),
            NpadStyleIndex::NES => styles.contains(NpadStyleSet::LARK),
            NpadStyleIndex::SNES => styles.contains(NpadStyleSet::LUCIA),
            NpadStyleIndex::N64 => styles.contains(NpadStyleSet::LAGOON),
            NpadStyleIndex::SegaGenesis => styles.contains(NpadStyleSet::LAGER),
            _ => false,
        }
    }

    /// Port of EmulatedController::GetHomeButtons.
    pub fn get_home_buttons(&self) -> HomeButtonState {
        let _lock = self.mutex.lock();
        if self.is_configuring {
            return HomeButtonState::default();
        }
        self.home_button_state
    }

    /// Port of EmulatedController::GetCaptureButtons.
    pub fn get_capture_buttons(&self) -> CaptureButtonState {
        let _lock = self.mutex.lock();
        if self.is_configuring {
            return CaptureButtonState::default();
        }
        self.capture_button_state
    }

    /// Port of EmulatedController::GetNpadButtons.
    pub fn get_npad_buttons(&self) -> NpadButtonState {
        let _lock = self.mutex.lock();
        if self.is_configuring {
            return NpadButtonState::default();
        }
        let turbo_mask = self.get_turbo_button_mask();
        NpadButtonState {
            raw: self.npad_button_state.raw & NpadButton::from_bits_truncate(turbo_mask),
        }
    }

    /// Port of EmulatedController::GetDebugPadButtons.
    pub fn get_debug_pad_buttons(&self) -> DebugPadButton {
        let _lock = self.mutex.lock();
        if self.is_configuring {
            return DebugPadButton::default();
        }
        self.debug_pad_button_state
    }

    /// Port of EmulatedController::GetSticks.
    pub fn get_sticks(&self) -> AnalogSticks {
        let _lock = self.mutex.lock();
        if self.is_configuring {
            return AnalogSticks::default();
        }
        self.analog_stick_state
    }

    /// Port of EmulatedController::GetTriggers.
    pub fn get_triggers(&self) -> NpadGcTriggerState {
        let _lock = self.mutex.lock();
        if self.is_configuring {
            return NpadGcTriggerState::default();
        }
        self.gc_trigger_state
    }

    /// Port of EmulatedController::GetMotions.
    pub fn get_motions(&self) -> MotionState {
        let _lock = self.mutex.lock();
        self.motion_state
    }

    /// Port of EmulatedController::GetColors.
    pub fn get_colors(&self) -> ControllerColors {
        let _lock = self.mutex.lock();
        self.colors_state
    }

    /// Port of EmulatedController::GetBattery.
    pub fn get_battery(&self) -> BatteryLevelState {
        let _lock = self.mutex.lock();
        self.battery_state
    }

    /// Port of EmulatedController::GetRingSensorForce.
    pub fn get_ring_sensor_force(&self) -> RingSensorForce {
        self.ring_analog_state
    }

    /// Port of EmulatedController::GetNpadColor.
    pub fn get_npad_color(color: u32) -> NpadColor {
        NpadColor {
            r: ((color >> 16) & 0xFF) as u8,
            g: ((color >> 8) & 0xFF) as u8,
            b: (color & 0xFF) as u8,
            a: 0xFF,
        }
    }

    /// Port of EmulatedController::SetVibration (simple on/off version).
    pub fn set_vibration_simple(&mut self, _should_vibrate: bool) -> bool {
        // Upstream sends vibration command to output device
        // Requires output device infrastructure
        true
    }

    /// Port of EmulatedController::GetActualVibrationValue.
    pub fn get_actual_vibration_value(&self, device_index: DeviceIndex) -> VibrationValue {
        let _lock = self.mutex.lock();
        match device_index {
            DeviceIndex::Left => self.last_vibration_value[0],
            DeviceIndex::Right => self.last_vibration_value[1],
            _ => DEFAULT_VIBRATION_VALUE,
        }
    }

    /// Port of EmulatedController::HasNfc.
    pub fn has_nfc(&self) -> bool {
        // Upstream checks nfc_devices[1] for NFC support
        false
    }

    /// Port of EmulatedController::AddNfcHandle.
    pub fn add_nfc_handle(&mut self) -> bool {
        self.nfc_handles += 1;
        true
    }

    /// Port of EmulatedController::RemoveNfcHandle.
    pub fn remove_nfc_handle(&mut self) -> bool {
        if self.nfc_handles == 0 {
            return false;
        }
        self.nfc_handles -= 1;
        true
    }

    /// Port of EmulatedController::SetGyroscopeZeroDriftMode.
    pub fn set_gyroscope_zero_drift_mode(&mut self, _mode: GyroscopeZeroDriftMode) {
        // Upstream iterates over motion_values and sets zero drift mode on each MotionInput.
        // Requires MotionInput integration.
    }

    /// Port of EmulatedController::StatusUpdate.
    pub fn status_update(&mut self) {
        self.turbo_button_state = (self.turbo_button_state + 1) % (TURBO_BUTTON_DELAY * 2);
        // Upstream also force-updates motion devices that need constant refreshing.
    }

    /// Port of EmulatedController::GetTurboButtonMask.
    fn get_turbo_button_mask(&self) -> u64 {
        // Apply no mask when disabled
        if self.turbo_button_state < TURBO_BUTTON_DELAY {
            return u64::MAX; // NpadButton::All
        }
        // Upstream builds a mask from button_values[index].turbo for each button.
        // Without the full button_values infrastructure, return all-ones (no masking).
        u64::MAX
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
