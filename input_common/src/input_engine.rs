// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/input_engine.h` and `input_common/input_engine.cpp`.
//!
//! Provides the base input engine abstraction that all input drivers implement.

use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use parking_lot::Mutex;

use common::input::{
    BatteryLevel, BodyColorStatus, ButtonNames, CameraFormat, CameraStatus, DriverResult,
    InputDevice, LedStatus, MifareRequest, NfcState, NfcStatus, PollingMode, VibrationStatus,
};
use common::param_package::ParamPackage;
use common::uuid::UUID;

use crate::main_common::{AnalogMapping, ButtonMapping, MotionMapping};

// ---- PadIdentifier ----
// Port of `PadIdentifier` struct from input_engine.h

#[derive(Debug, Clone, Default)]
pub struct PadIdentifier {
    pub guid: UUID,
    pub port: usize,
    pub pad: usize,
}

impl PartialEq for PadIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.guid == other.guid && self.port == other.port && self.pad == other.pad
    }
}

impl Eq for PadIdentifier {}

impl Hash for PadIdentifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Port of std::hash<PadIdentifier> from input_engine.h
        let hash_value = self.guid.hash_value()
            ^ ((self.port as u64) << 32)
            ^ (self.pad as u64);
        hash_value.hash(state);
    }
}

// ---- BasicMotion ----
// Port of `BasicMotion` struct from input_engine.h

#[derive(Debug, Clone, Default)]
pub struct BasicMotion {
    pub gyro_x: f32,
    pub gyro_y: f32,
    pub gyro_z: f32,
    pub accel_x: f32,
    pub accel_y: f32,
    pub accel_z: f32,
    pub delta_timestamp: u64,
}

// ---- EngineInputType ----
// Port of `EngineInputType` enum from input_engine.h

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EngineInputType {
    None,
    Analog,
    Battery,
    Button,
    Camera,
    Color,
    HatButton,
    Motion,
    Nfc,
}

impl Default for EngineInputType {
    fn default() -> Self {
        EngineInputType::None
    }
}

// ---- VibrationRequest ----
// Port of `VibrationRequest` struct from input_engine.h

#[derive(Debug, Clone)]
pub struct VibrationRequest {
    pub identifier: PadIdentifier,
    pub vibration: VibrationStatus,
}

// ---- MappingData ----
// Port of `MappingData` struct from input_engine.h

#[derive(Debug, Clone, Default)]
pub struct MappingData {
    pub engine: String,
    pub pad: PadIdentifier,
    pub r#type: EngineInputType,
    pub index: i32,
    pub button_value: bool,
    pub hat_name: String,
    pub axis_value: f32,
    pub motion_value: BasicMotion,
}

// ---- UpdateCallback ----
// Port of `UpdateCallback` struct from input_engine.h

pub struct UpdateCallback {
    pub on_change: Option<Box<dyn Fn() + Send + Sync>>,
}

impl std::fmt::Debug for UpdateCallback {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UpdateCallback")
            .field("on_change", &self.on_change.is_some())
            .finish()
    }
}

// ---- MappingCallback ----
// Port of `MappingCallback` struct from input_engine.h

pub struct MappingCallback {
    pub on_data: Option<Box<dyn Fn(&MappingData) + Send + Sync>>,
}

impl Default for MappingCallback {
    fn default() -> Self {
        Self { on_data: None }
    }
}

impl std::fmt::Debug for MappingCallback {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MappingCallback")
            .field("on_data", &self.on_data.is_some())
            .finish()
    }
}

// ---- InputIdentifier ----
// Port of `InputIdentifier` struct from input_engine.h

pub struct InputIdentifier {
    pub identifier: PadIdentifier,
    pub r#type: EngineInputType,
    pub index: i32,
    pub callback: UpdateCallback,
}

// ---- ControllerData ----
// Port of private `ControllerData` struct from input_engine.h

#[derive(Debug, Default)]
struct ControllerData {
    buttons: HashMap<i32, bool>,
    hat_buttons: HashMap<i32, u8>,
    axes: HashMap<i32, f32>,
    motions: HashMap<i32, BasicMotion>,
    battery: BatteryLevel,
    color: BodyColorStatus,
    camera: CameraStatus,
    nfc: NfcStatus,
}

// ---- InputEngine ----
// Port of `InputEngine` class from input_engine.h / input_engine.cpp

pub struct InputEngine {
    mutex: Mutex<()>,
    mutex_callback: Mutex<()>,
    configuring: bool,
    input_engine: String,
    last_callback_key: i32,
    controller_list: HashMap<PadIdentifier, ControllerData>,
    callback_list: HashMap<i32, InputIdentifier>,
    mapping_callback: MappingCallback,
}

impl InputEngine {
    pub fn new(input_engine: String) -> Self {
        Self {
            mutex: Mutex::new(()),
            mutex_callback: Mutex::new(()),
            configuring: false,
            input_engine,
            last_callback_key: 0,
            controller_list: HashMap::new(),
            callback_list: HashMap::new(),
            mapping_callback: MappingCallback::default(),
        }
    }

    // ---- Configuration mode ----

    /// Enable configuring mode for mapping.
    /// Port of InputEngine::BeginConfiguration
    pub fn begin_configuration(&mut self) {
        self.configuring = true;
    }

    /// Disable configuring mode for mapping.
    /// Port of InputEngine::EndConfiguration
    pub fn end_configuration(&mut self) {
        self.configuring = false;
    }

    // ---- Engine name ----

    /// Returns the engine name.
    /// Port of InputEngine::GetEngineName
    pub fn get_engine_name(&self) -> &str {
        &self.input_engine
    }

    // ---- Pre-set methods ----

    /// Port of InputEngine::PreSetController
    pub fn pre_set_controller(&mut self, identifier: &PadIdentifier) {
        let _lock = self.mutex.lock();
        self.controller_list.entry(identifier.clone()).or_default();
    }

    /// Port of InputEngine::PreSetButton
    pub fn pre_set_button(&mut self, identifier: &PadIdentifier, button: i32) {
        let _lock = self.mutex.lock();
        if let Some(controller) = self.controller_list.get_mut(identifier) {
            controller.buttons.entry(button).or_insert(false);
        }
    }

    /// Port of InputEngine::PreSetHatButton
    pub fn pre_set_hat_button(&mut self, identifier: &PadIdentifier, button: i32) {
        let _lock = self.mutex.lock();
        if let Some(controller) = self.controller_list.get_mut(identifier) {
            controller.hat_buttons.entry(button).or_insert(0u8);
        }
    }

    /// Port of InputEngine::PreSetAxis
    pub fn pre_set_axis(&mut self, identifier: &PadIdentifier, axis: i32) {
        let _lock = self.mutex.lock();
        if let Some(controller) = self.controller_list.get_mut(identifier) {
            controller.axes.entry(axis).or_insert(0.0f32);
        }
    }

    /// Port of InputEngine::PreSetMotion
    pub fn pre_set_motion(&mut self, identifier: &PadIdentifier, motion: i32) {
        let _lock = self.mutex.lock();
        if let Some(controller) = self.controller_list.get_mut(identifier) {
            controller.motions.entry(motion).or_default();
        }
    }

    // ---- Set methods (protected in C++) ----

    /// Port of InputEngine::SetButton
    pub fn set_button(&mut self, identifier: &PadIdentifier, button: i32, value: bool) {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.buttons.insert(button, value);
                }
            }
        }
        self.trigger_on_button_change(identifier, button, value);
    }

    /// Port of InputEngine::SetHatButton
    pub fn set_hat_button(&mut self, identifier: &PadIdentifier, button: i32, value: u8) {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.hat_buttons.insert(button, value);
                }
            }
        }
        self.trigger_on_hat_button_change(identifier, button, value);
    }

    /// Port of InputEngine::SetAxis
    pub fn set_axis(&mut self, identifier: &PadIdentifier, axis: i32, value: f32) {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.axes.insert(axis, value);
                }
            }
        }
        self.trigger_on_axis_change(identifier, axis, value);
    }

    /// Port of InputEngine::SetBattery
    pub fn set_battery(&mut self, identifier: &PadIdentifier, value: BatteryLevel) {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.battery = value;
                }
            }
        }
        self.trigger_on_battery_change(identifier, value);
    }

    /// Port of InputEngine::SetColor
    pub fn set_color(&mut self, identifier: &PadIdentifier, value: BodyColorStatus) {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.color = value.clone();
                }
            }
        }
        self.trigger_on_color_change(identifier, value);
    }

    /// Port of InputEngine::SetMotion
    pub fn set_motion(&mut self, identifier: &PadIdentifier, motion: i32, value: &BasicMotion) {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.motions.insert(motion, value.clone());
                }
            }
        }
        self.trigger_on_motion_change(identifier, motion, value);
    }

    /// Port of InputEngine::SetCamera
    pub fn set_camera(&mut self, identifier: &PadIdentifier, value: &CameraStatus) {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.camera = value.clone();
                }
            }
        }
        self.trigger_on_camera_change(identifier, value);
    }

    /// Port of InputEngine::SetNfc
    pub fn set_nfc(&mut self, identifier: &PadIdentifier, value: &NfcStatus) {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.nfc = value.clone();
                }
            }
        }
        self.trigger_on_nfc_change(identifier, value);
    }

    // ---- Get methods ----

    /// Port of InputEngine::GetButton
    pub fn get_button(&self, identifier: &PadIdentifier, button: i32) -> bool {
        let _lock = self.mutex.lock();
        let Some(controller) = self.controller_list.get(identifier) else {
            log::error!(
                "Invalid identifier guid={}, pad={}, port={}",
                identifier.guid.raw_string(),
                identifier.pad,
                identifier.port
            );
            return false;
        };
        let Some(&value) = controller.buttons.get(&button) else {
            log::error!("Invalid button {}", button);
            return false;
        };
        value
    }

    /// Port of InputEngine::GetHatButton
    pub fn get_hat_button(&self, identifier: &PadIdentifier, button: i32, direction: u8) -> bool {
        let _lock = self.mutex.lock();
        let Some(controller) = self.controller_list.get(identifier) else {
            log::error!(
                "Invalid identifier guid={}, pad={}, port={}",
                identifier.guid.raw_string(),
                identifier.pad,
                identifier.port
            );
            return false;
        };
        let Some(&value) = controller.hat_buttons.get(&button) else {
            log::error!("Invalid hat button {}", button);
            return false;
        };
        (value & direction) != 0
    }

    /// Port of InputEngine::GetAxis
    pub fn get_axis(&self, identifier: &PadIdentifier, axis: i32) -> f32 {
        let _lock = self.mutex.lock();
        let Some(controller) = self.controller_list.get(identifier) else {
            log::error!(
                "Invalid identifier guid={}, pad={}, port={}",
                identifier.guid.raw_string(),
                identifier.pad,
                identifier.port
            );
            return 0.0;
        };
        let Some(&value) = controller.axes.get(&axis) else {
            log::error!("Invalid axis {}", axis);
            return 0.0;
        };
        value
    }

    /// Port of InputEngine::GetBattery
    pub fn get_battery(&self, identifier: &PadIdentifier) -> BatteryLevel {
        let _lock = self.mutex.lock();
        let Some(controller) = self.controller_list.get(identifier) else {
            log::error!(
                "Invalid identifier guid={}, pad={}, port={}",
                identifier.guid.raw_string(),
                identifier.pad,
                identifier.port
            );
            return BatteryLevel::Charging;
        };
        controller.battery
    }

    /// Port of InputEngine::GetColor
    pub fn get_color(&self, identifier: &PadIdentifier) -> BodyColorStatus {
        let _lock = self.mutex.lock();
        let Some(controller) = self.controller_list.get(identifier) else {
            log::error!(
                "Invalid identifier guid={}, pad={}, port={}",
                identifier.guid.raw_string(),
                identifier.pad,
                identifier.port
            );
            return BodyColorStatus::default();
        };
        controller.color.clone()
    }

    /// Port of InputEngine::GetMotion
    pub fn get_motion(&self, identifier: &PadIdentifier, motion: i32) -> BasicMotion {
        let _lock = self.mutex.lock();
        let Some(controller) = self.controller_list.get(identifier) else {
            log::error!(
                "Invalid identifier guid={}, pad={}, port={}",
                identifier.guid.raw_string(),
                identifier.pad,
                identifier.port
            );
            return BasicMotion::default();
        };
        controller
            .motions
            .get(&motion)
            .cloned()
            .unwrap_or_default()
    }

    /// Port of InputEngine::GetCamera
    pub fn get_camera(&self, identifier: &PadIdentifier) -> CameraStatus {
        let _lock = self.mutex.lock();
        let Some(controller) = self.controller_list.get(identifier) else {
            log::error!(
                "Invalid identifier guid={}, pad={}, port={}",
                identifier.guid.raw_string(),
                identifier.pad,
                identifier.port
            );
            return CameraStatus::default();
        };
        controller.camera.clone()
    }

    /// Port of InputEngine::GetNfc
    pub fn get_nfc(&self, identifier: &PadIdentifier) -> NfcStatus {
        let _lock = self.mutex.lock();
        let Some(controller) = self.controller_list.get(identifier) else {
            log::error!(
                "Invalid identifier guid={}, pad={}, port={}",
                identifier.guid.raw_string(),
                identifier.pad,
                identifier.port
            );
            return NfcStatus::default();
        };
        controller.nfc.clone()
    }

    // ---- Reset methods ----

    /// Port of InputEngine::ResetButtonState
    pub fn reset_button_state(&mut self) {
        let pairs: Vec<_> = self
            .controller_list
            .iter()
            .flat_map(|(id, data)| {
                data.buttons
                    .keys()
                    .map(move |&button| (id.clone(), button))
            })
            .collect();
        for (id, button) in pairs {
            self.set_button(&id, button, false);
        }
        let hat_pairs: Vec<_> = self
            .controller_list
            .iter()
            .flat_map(|(id, data)| {
                data.hat_buttons
                    .keys()
                    .map(move |&button| (id.clone(), button))
            })
            .collect();
        for (id, button) in hat_pairs {
            self.set_hat_button(&id, button, 0);
        }
    }

    /// Port of InputEngine::ResetAnalogState
    pub fn reset_analog_state(&mut self) {
        let pairs: Vec<_> = self
            .controller_list
            .iter()
            .flat_map(|(id, data)| data.axes.keys().map(move |&axis| (id.clone(), axis)))
            .collect();
        for (id, axis) in pairs {
            self.set_axis(&id, axis, 0.0);
        }
    }

    // ---- Callback management ----

    /// Port of InputEngine::SetCallback
    pub fn set_callback(&mut self, input_identifier: InputIdentifier) -> i32 {
        let _lock = self.mutex_callback.lock();
        let key = self.last_callback_key;
        self.callback_list.insert(key, input_identifier);
        self.last_callback_key += 1;
        key
    }

    /// Port of InputEngine::SetMappingCallback
    pub fn set_mapping_callback(&mut self, callback: MappingCallback) {
        let _lock = self.mutex_callback.lock();
        self.mapping_callback = callback;
    }

    /// Port of InputEngine::DeleteCallback
    pub fn delete_callback(&mut self, key: i32) {
        let _lock = self.mutex_callback.lock();
        if self.callback_list.remove(&key).is_none() {
            log::error!("Tried to delete non-existent callback {}", key);
        }
    }

    // ---- Virtual methods with default implementations ----
    // These would be overridden by concrete driver types via trait or manual dispatch.

    /// Port of InputEngine::GetHatButtonName (virtual, default "Unknown")
    pub fn get_hat_button_name(&self, _direction_value: u8) -> String {
        "Unknown".to_string()
    }

    // ---- Trigger methods (private in C++) ----

    /// Port of InputEngine::TriggerOnButtonChange
    fn trigger_on_button_change(&self, identifier: &PadIdentifier, button: i32, value: bool) {
        let _lock = self.mutex_callback.lock();
        for poller in self.callback_list.values() {
            if !Self::is_input_identifier_equal(
                poller,
                identifier,
                EngineInputType::Button,
                button,
            ) {
                continue;
            }
            if let Some(ref on_change) = poller.callback.on_change {
                on_change();
            }
        }
        if !self.configuring || self.mapping_callback.on_data.is_none() {
            return;
        }
        if value == self.get_button(identifier, button) {
            return;
        }
        if let Some(ref on_data) = self.mapping_callback.on_data {
            on_data(&MappingData {
                engine: self.get_engine_name().to_string(),
                pad: identifier.clone(),
                r#type: EngineInputType::Button,
                index: button,
                button_value: value,
                ..Default::default()
            });
        }
    }

    /// Port of InputEngine::TriggerOnHatButtonChange
    fn trigger_on_hat_button_change(&self, identifier: &PadIdentifier, button: i32, value: u8) {
        let _lock = self.mutex_callback.lock();
        for poller in self.callback_list.values() {
            if !Self::is_input_identifier_equal(
                poller,
                identifier,
                EngineInputType::HatButton,
                button,
            ) {
                continue;
            }
            if let Some(ref on_change) = poller.callback.on_change {
                on_change();
            }
        }
        if !self.configuring || self.mapping_callback.on_data.is_none() {
            return;
        }
        let mut index: usize = 1;
        while index < 0xff {
            let button_value = (value & index as u8) != 0;
            if button_value == self.get_hat_button(identifier, button, index as u8) {
                index <<= 1;
                continue;
            }
            if let Some(ref on_data) = self.mapping_callback.on_data {
                on_data(&MappingData {
                    engine: self.get_engine_name().to_string(),
                    pad: identifier.clone(),
                    r#type: EngineInputType::HatButton,
                    index: button,
                    hat_name: self.get_hat_button_name(index as u8),
                    ..Default::default()
                });
            }
            index <<= 1;
        }
    }

    /// Port of InputEngine::TriggerOnAxisChange
    fn trigger_on_axis_change(&self, identifier: &PadIdentifier, axis: i32, value: f32) {
        let _lock = self.mutex_callback.lock();
        for poller in self.callback_list.values() {
            if !Self::is_input_identifier_equal(
                poller,
                identifier,
                EngineInputType::Analog,
                axis,
            ) {
                continue;
            }
            if let Some(ref on_change) = poller.callback.on_change {
                on_change();
            }
        }
        if !self.configuring || self.mapping_callback.on_data.is_none() {
            return;
        }
        if (value - self.get_axis(identifier, axis)).abs() < 0.5 {
            return;
        }
        if let Some(ref on_data) = self.mapping_callback.on_data {
            on_data(&MappingData {
                engine: self.get_engine_name().to_string(),
                pad: identifier.clone(),
                r#type: EngineInputType::Analog,
                index: axis,
                axis_value: value,
                ..Default::default()
            });
        }
    }

    /// Port of InputEngine::TriggerOnBatteryChange
    fn trigger_on_battery_change(&self, identifier: &PadIdentifier, _value: BatteryLevel) {
        let _lock = self.mutex_callback.lock();
        for poller in self.callback_list.values() {
            if !Self::is_input_identifier_equal(
                poller,
                identifier,
                EngineInputType::Battery,
                0,
            ) {
                continue;
            }
            if let Some(ref on_change) = poller.callback.on_change {
                on_change();
            }
        }
    }

    /// Port of InputEngine::TriggerOnColorChange
    fn trigger_on_color_change(&self, identifier: &PadIdentifier, _value: BodyColorStatus) {
        let _lock = self.mutex_callback.lock();
        for poller in self.callback_list.values() {
            if !Self::is_input_identifier_equal(
                poller,
                identifier,
                EngineInputType::Color,
                0,
            ) {
                continue;
            }
            if let Some(ref on_change) = poller.callback.on_change {
                on_change();
            }
        }
    }

    /// Port of InputEngine::TriggerOnMotionChange
    fn trigger_on_motion_change(
        &self,
        identifier: &PadIdentifier,
        motion: i32,
        value: &BasicMotion,
    ) {
        let _lock = self.mutex_callback.lock();
        for poller in self.callback_list.values() {
            if !Self::is_input_identifier_equal(
                poller,
                identifier,
                EngineInputType::Motion,
                motion,
            ) {
                continue;
            }
            if let Some(ref on_change) = poller.callback.on_change {
                on_change();
            }
        }
        if !self.configuring || self.mapping_callback.on_data.is_none() {
            return;
        }
        let old_value = self.get_motion(identifier, motion);
        let mut is_active = false;
        if (value.accel_x - old_value.accel_x).abs() > 1.5
            || (value.accel_y - old_value.accel_y).abs() > 1.5
            || (value.accel_z - old_value.accel_z).abs() > 1.5
        {
            is_active = true;
        }
        if (value.gyro_x - old_value.gyro_x).abs() > 0.6
            || (value.gyro_y - old_value.gyro_y).abs() > 0.6
            || (value.gyro_z - old_value.gyro_z).abs() > 0.6
        {
            is_active = true;
        }
        if !is_active {
            return;
        }
        if let Some(ref on_data) = self.mapping_callback.on_data {
            on_data(&MappingData {
                engine: self.get_engine_name().to_string(),
                pad: identifier.clone(),
                r#type: EngineInputType::Motion,
                index: motion,
                motion_value: value.clone(),
                ..Default::default()
            });
        }
    }

    /// Port of InputEngine::TriggerOnCameraChange
    fn trigger_on_camera_change(&self, identifier: &PadIdentifier, _value: &CameraStatus) {
        let _lock = self.mutex_callback.lock();
        for poller in self.callback_list.values() {
            if !Self::is_input_identifier_equal(
                poller,
                identifier,
                EngineInputType::Camera,
                0,
            ) {
                continue;
            }
            if let Some(ref on_change) = poller.callback.on_change {
                on_change();
            }
        }
    }

    /// Port of InputEngine::TriggerOnNfcChange
    fn trigger_on_nfc_change(&self, identifier: &PadIdentifier, _value: &NfcStatus) {
        let _lock = self.mutex_callback.lock();
        for poller in self.callback_list.values() {
            if !Self::is_input_identifier_equal(poller, identifier, EngineInputType::Nfc, 0) {
                continue;
            }
            if let Some(ref on_change) = poller.callback.on_change {
                on_change();
            }
        }
    }

    /// Port of InputEngine::IsInputIdentifierEqual
    fn is_input_identifier_equal(
        input_identifier: &InputIdentifier,
        identifier: &PadIdentifier,
        r#type: EngineInputType,
        index: i32,
    ) -> bool {
        if input_identifier.r#type != r#type {
            return false;
        }
        if input_identifier.index != index {
            return false;
        }
        if input_identifier.identifier != *identifier {
            return false;
        }
        true
    }
}

// ---- Virtual method defaults (would be trait methods in a full port) ----
// These correspond to the virtual methods in the C++ InputEngine class.
// Concrete drivers override these. For now they are provided as default
// free-standing helpers since InputEngine is used via composition.

/// Default implementation for SetLeds virtual method
pub fn default_set_leds(
    _identifier: &PadIdentifier,
    _led_status: &LedStatus,
) -> DriverResult {
    DriverResult::NotSupported
}

/// Default implementation for SetVibration virtual method
pub fn default_set_vibration(
    _identifier: &PadIdentifier,
    _vibration: &VibrationStatus,
) -> DriverResult {
    DriverResult::NotSupported
}

/// Default implementation for IsVibrationEnabled virtual method
pub fn default_is_vibration_enabled(_identifier: &PadIdentifier) -> bool {
    false
}

/// Default implementation for SetPollingMode virtual method
pub fn default_set_polling_mode(
    _identifier: &PadIdentifier,
    _polling_mode: PollingMode,
) -> DriverResult {
    DriverResult::NotSupported
}

/// Default implementation for SetCameraFormat virtual method
pub fn default_set_camera_format(
    _identifier: &PadIdentifier,
    _camera_format: CameraFormat,
) -> DriverResult {
    DriverResult::NotSupported
}

/// Default implementation for SupportsNfc virtual method
pub fn default_supports_nfc(_identifier: &PadIdentifier) -> NfcState {
    NfcState::NotSupported
}

/// Default implementation for GetInputDevices virtual method
pub fn default_get_input_devices() -> Vec<ParamPackage> {
    Vec::new()
}

/// Default implementation for GetButtonMappingForDevice virtual method
pub fn default_get_button_mapping_for_device(_params: &ParamPackage) -> ButtonMapping {
    ButtonMapping::new()
}

/// Default implementation for GetAnalogMappingForDevice virtual method
pub fn default_get_analog_mapping_for_device(_params: &ParamPackage) -> AnalogMapping {
    AnalogMapping::new()
}

/// Default implementation for GetMotionMappingForDevice virtual method
pub fn default_get_motion_mapping_for_device(_params: &ParamPackage) -> MotionMapping {
    MotionMapping::new()
}

/// Default implementation for GetUIName virtual method
pub fn default_get_ui_name(_params: &ParamPackage) -> ButtonNames {
    ButtonNames::Engine
}

/// Default implementation for GetHatButtonId virtual method
pub fn default_get_hat_button_id(_direction_name: &str) -> u8 {
    0
}

/// Default implementation for IsStickInverted virtual method
pub fn default_is_stick_inverted(_params: &ParamPackage) -> bool {
    false
}
