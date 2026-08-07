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
        let hash_value = self.guid.hash_value() ^ ((self.port as u64) << 32) ^ (self.pad as u64);
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
    pub on_change: Option<Arc<dyn Fn(&MappingData) + Send + Sync>>,
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

/// Callbacks that must run once the engine's lock has been released.
///
/// Upstream guards `controller_list` and `callback_list` with two separate
/// mutexes, so a device's `OnChange` can read a value straight back while the
/// engine is dispatching. This port keeps the whole engine behind one
/// `Arc<Mutex<InputEngine>>`, and a driver calls it as
/// `engine.lock().set_axis(..)` — dispatching from inside that call deadlocks
/// the instant a device reads a value back, which is exactly what
/// `InputFromStick::OnChange` does. The matching callbacks are handed to the
/// caller instead, which runs them after its guard is gone.
#[must_use = "the callbacks only run once dispatched, outside the engine lock"]
pub struct PendingCallbacks {
    event: MappingData,
    callbacks: Vec<Arc<dyn Fn(&MappingData) + Send + Sync>>,
}

impl PendingCallbacks {
    /// Run the callbacks. Call this with no engine lock held.
    pub fn dispatch(self) {
        for callback in self.callbacks {
            callback(&self.event);
        }
    }
}

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

/// Rust counterpart of the virtual output methods on upstream `InputEngine`.
///
/// Drivers still own their output behavior; the concrete driver installs one
/// implementation in the composed `InputEngine`, preserving the C++ virtual
/// dispatch without making the input-state container itself driver-specific.
pub trait InputEngineOutput: Send + Sync {
    fn set_leds(&self, _identifier: &PadIdentifier, _status: &LedStatus) -> DriverResult {
        DriverResult::NotSupported
    }

    fn set_vibration(
        &self,
        _identifier: &PadIdentifier,
        _status: &VibrationStatus,
    ) -> DriverResult {
        DriverResult::NotSupported
    }

    fn is_vibration_enabled(&self, _identifier: &PadIdentifier) -> bool {
        false
    }

    fn set_polling_mode(&self, _identifier: &PadIdentifier, _mode: PollingMode) -> DriverResult {
        DriverResult::NotSupported
    }

    fn set_camera_format(
        &self,
        _identifier: &PadIdentifier,
        _format: CameraFormat,
    ) -> DriverResult {
        DriverResult::NotSupported
    }

    fn supports_nfc(&self, _identifier: &PadIdentifier) -> NfcState {
        NfcState::NotSupported
    }

    fn start_nfc_polling(&self, _identifier: &PadIdentifier) -> NfcState {
        NfcState::NotSupported
    }

    fn stop_nfc_polling(&self, _identifier: &PadIdentifier) -> NfcState {
        NfcState::NotSupported
    }

    fn read_amiibo_data(&self, _identifier: &PadIdentifier, _out_data: &mut Vec<u8>) -> NfcState {
        NfcState::NotSupported
    }

    fn write_nfc_data(&self, _identifier: &PadIdentifier, _data: &[u8]) -> NfcState {
        NfcState::NotSupported
    }

    fn read_mifare_data(
        &self,
        _identifier: &PadIdentifier,
        _request: &MifareRequest,
        _out_data: &mut MifareRequest,
    ) -> NfcState {
        NfcState::NotSupported
    }

    fn write_mifare_data(&self, _identifier: &PadIdentifier, _request: &MifareRequest) -> NfcState {
        NfcState::NotSupported
    }
}

/// Rust counterpart of the input-query virtuals overridden by concrete
/// upstream `InputEngine` subclasses.
pub trait InputEngineMetadata: Send + Sync {
    fn get_hat_button_name(&self, _direction_value: u8) -> String {
        "Unknown".to_string()
    }

    fn get_hat_button_id(&self, _direction_name: &str) -> u8 {
        0
    }
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
    metadata: Option<Arc<dyn InputEngineMetadata>>,
    output: Option<Arc<dyn InputEngineOutput>>,
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
            metadata: None,
            output: None,
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

    pub fn set_output_handler(&mut self, output: Arc<dyn InputEngineOutput>) {
        self.output = Some(output);
    }

    pub(crate) fn output_handler(&self) -> Option<Arc<dyn InputEngineOutput>> {
        self.output.as_ref().map(Arc::clone)
    }

    pub fn set_metadata_handler(&mut self, metadata: Arc<dyn InputEngineMetadata>) {
        self.metadata = Some(metadata);
    }

    pub fn set_leds(&self, identifier: &PadIdentifier, status: &LedStatus) -> DriverResult {
        self.output
            .as_ref()
            .map_or(DriverResult::NotSupported, |output| {
                output.set_leds(identifier, status)
            })
    }

    pub fn set_vibration(
        &self,
        identifier: &PadIdentifier,
        status: &VibrationStatus,
    ) -> DriverResult {
        self.output
            .as_ref()
            .map_or(DriverResult::NotSupported, |output| {
                output.set_vibration(identifier, status)
            })
    }

    pub fn is_vibration_enabled(&self, identifier: &PadIdentifier) -> bool {
        self.output
            .as_ref()
            .is_some_and(|output| output.is_vibration_enabled(identifier))
    }

    pub fn set_polling_mode(&self, identifier: &PadIdentifier, mode: PollingMode) -> DriverResult {
        self.output
            .as_ref()
            .map_or(DriverResult::NotSupported, |output| {
                output.set_polling_mode(identifier, mode)
            })
    }

    pub fn set_camera_format(
        &self,
        identifier: &PadIdentifier,
        format: CameraFormat,
    ) -> DriverResult {
        self.output
            .as_ref()
            .map_or(DriverResult::NotSupported, |output| {
                output.set_camera_format(identifier, format)
            })
    }

    pub fn supports_nfc(&self, identifier: &PadIdentifier) -> NfcState {
        self.output
            .as_ref()
            .map_or(NfcState::NotSupported, |output| {
                output.supports_nfc(identifier)
            })
    }

    pub fn start_nfc_polling(&self, identifier: &PadIdentifier) -> NfcState {
        self.output
            .as_ref()
            .map_or(NfcState::NotSupported, |output| {
                output.start_nfc_polling(identifier)
            })
    }

    pub fn stop_nfc_polling(&self, identifier: &PadIdentifier) -> NfcState {
        self.output
            .as_ref()
            .map_or(NfcState::NotSupported, |output| {
                output.stop_nfc_polling(identifier)
            })
    }

    pub fn read_amiibo_data(&self, identifier: &PadIdentifier, out_data: &mut Vec<u8>) -> NfcState {
        self.output
            .as_ref()
            .map_or(NfcState::NotSupported, |output| {
                output.read_amiibo_data(identifier, out_data)
            })
    }

    pub fn write_nfc_data(&self, identifier: &PadIdentifier, data: &[u8]) -> NfcState {
        self.output
            .as_ref()
            .map_or(NfcState::NotSupported, |output| {
                output.write_nfc_data(identifier, data)
            })
    }

    pub fn read_mifare_data(
        &self,
        identifier: &PadIdentifier,
        request: &MifareRequest,
        out_data: &mut MifareRequest,
    ) -> NfcState {
        self.output
            .as_ref()
            .map_or(NfcState::NotSupported, |output| {
                output.read_mifare_data(identifier, request, out_data)
            })
    }

    pub fn write_mifare_data(
        &self,
        identifier: &PadIdentifier,
        request: &MifareRequest,
    ) -> NfcState {
        self.output
            .as_ref()
            .map_or(NfcState::NotSupported, |output| {
                output.write_mifare_data(identifier, request)
            })
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
    pub fn set_button(
        &mut self,
        identifier: &PadIdentifier,
        button: i32,
        value: bool,
    ) -> PendingCallbacks {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.buttons.insert(button, value);
                }
            }
        }
        self.trigger_on_button_change(identifier, button, value)
    }

    /// Port of InputEngine::SetHatButton
    pub fn set_hat_button(
        &mut self,
        identifier: &PadIdentifier,
        button: i32,
        value: u8,
    ) -> PendingCallbacks {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.hat_buttons.insert(button, value);
                }
            }
        }
        self.trigger_on_hat_button_change(identifier, button, value)
    }

    /// Port of InputEngine::SetAxis
    pub fn set_axis(
        &mut self,
        identifier: &PadIdentifier,
        axis: i32,
        value: f32,
    ) -> PendingCallbacks {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.axes.insert(axis, value);
                }
            }
        }
        self.trigger_on_axis_change(identifier, axis, value)
    }

    /// Port of InputEngine::SetBattery
    pub fn set_battery(
        &mut self,
        identifier: &PadIdentifier,
        value: BatteryLevel,
    ) -> PendingCallbacks {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.battery = value;
                }
            }
        }
        self.trigger_on_battery_change(identifier, value)
    }

    /// Port of InputEngine::SetColor
    pub fn set_color(
        &mut self,
        identifier: &PadIdentifier,
        value: BodyColorStatus,
    ) -> PendingCallbacks {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.color = value.clone();
                }
            }
        }
        self.trigger_on_color_change(identifier, value)
    }

    /// Port of InputEngine::SetMotion
    pub fn set_motion(
        &mut self,
        identifier: &PadIdentifier,
        motion: i32,
        value: &BasicMotion,
    ) -> PendingCallbacks {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.motions.insert(motion, value.clone());
                }
            }
        }
        self.trigger_on_motion_change(identifier, motion, value)
    }

    /// Port of InputEngine::SetCamera
    pub fn set_camera(
        &mut self,
        identifier: &PadIdentifier,
        value: &CameraStatus,
    ) -> PendingCallbacks {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.camera = value.clone();
                }
            }
        }
        self.trigger_on_camera_change(identifier, value)
    }

    /// Port of InputEngine::SetNfc
    pub fn set_nfc(&mut self, identifier: &PadIdentifier, value: &NfcStatus) -> PendingCallbacks {
        {
            let _lock = self.mutex.lock();
            if let Some(controller) = self.controller_list.get_mut(identifier) {
                if !self.configuring {
                    controller.nfc = value.clone();
                }
            }
        }
        self.trigger_on_nfc_change(identifier, value)
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
        controller.motions.get(&motion).cloned().unwrap_or_default()
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
    pub fn reset_button_state(&mut self) -> Vec<PendingCallbacks> {
        let pairs: Vec<_> = self
            .controller_list
            .iter()
            .flat_map(|(id, data)| data.buttons.keys().map(move |&button| (id.clone(), button)))
            .collect();
        let mut pending = Vec::with_capacity(pairs.len());
        for (id, button) in pairs {
            pending.push(self.set_button(&id, button, false));
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
        pending.reserve(hat_pairs.len());
        for (id, button) in hat_pairs {
            pending.push(self.set_hat_button(&id, button, 0));
        }
        pending
    }

    /// Port of InputEngine::ResetAnalogState
    pub fn reset_analog_state(&mut self) -> Vec<PendingCallbacks> {
        let pairs: Vec<_> = self
            .controller_list
            .iter()
            .flat_map(|(id, data)| data.axes.keys().map(move |&axis| (id.clone(), axis)))
            .collect();
        pairs
            .into_iter()
            .map(|(id, axis)| self.set_axis(&id, axis, 0.0))
            .collect()
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

    /// Port of InputEngine::GetHatButtonName (virtual, default "Unknown").
    pub fn get_hat_button_name(&self, direction_value: u8) -> String {
        self.metadata.as_ref().map_or_else(
            || "Unknown".to_string(),
            |metadata| metadata.get_hat_button_name(direction_value),
        )
    }

    /// Port of InputEngine::GetHatButtonId (virtual, default zero).
    pub fn get_hat_button_id(&self, direction_name: &str) -> u8 {
        self.metadata
            .as_ref()
            .map_or(0, |metadata| metadata.get_hat_button_id(direction_name))
    }

    // ---- Trigger methods (private in C++) ----

    /// Port of InputEngine::TriggerOnButtonChange
    fn trigger_on_button_change(
        &self,
        identifier: &PadIdentifier,
        button: i32,
        value: bool,
    ) -> PendingCallbacks {
        let _lock = self.mutex_callback.lock();
        let event = MappingData {
            engine: self.get_engine_name().to_string(),
            pad: identifier.clone(),
            r#type: EngineInputType::Button,
            index: button,
            button_value: value,
            ..Default::default()
        };
        let callbacks: Vec<_> = self
            .callback_list
            .values()
            .filter(|poller| {
                Self::is_input_identifier_equal(poller, identifier, EngineInputType::Button, button)
            })
            .filter_map(|poller| poller.callback.on_change.clone())
            .collect();
        let pending = PendingCallbacks {
            event: event.clone(),
            callbacks,
        };
        if !self.configuring || self.mapping_callback.on_data.is_none() {
            return pending;
        }
        if value == self.get_button(identifier, button) {
            return pending;
        }
        if let Some(ref on_data) = self.mapping_callback.on_data {
            on_data(&event);
        }
        pending
    }

    /// Port of InputEngine::TriggerOnHatButtonChange
    fn trigger_on_hat_button_change(
        &self,
        identifier: &PadIdentifier,
        button: i32,
        value: u8,
    ) -> PendingCallbacks {
        let _lock = self.mutex_callback.lock();
        let event = MappingData {
            engine: self.get_engine_name().to_string(),
            pad: identifier.clone(),
            r#type: EngineInputType::HatButton,
            index: button,
            button_value: value != 0,
            ..Default::default()
        };
        let callbacks: Vec<_> = self
            .callback_list
            .values()
            .filter(|poller| {
                Self::is_input_identifier_equal(
                    poller,
                    identifier,
                    EngineInputType::HatButton,
                    button,
                )
            })
            .filter_map(|poller| poller.callback.on_change.clone())
            .collect();
        let pending = PendingCallbacks { event, callbacks };
        if !self.configuring || self.mapping_callback.on_data.is_none() {
            return pending;
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
        pending
    }

    /// Port of InputEngine::TriggerOnAxisChange
    fn trigger_on_axis_change(
        &self,
        identifier: &PadIdentifier,
        axis: i32,
        value: f32,
    ) -> PendingCallbacks {
        let _lock = self.mutex_callback.lock();
        let event = MappingData {
            engine: self.get_engine_name().to_string(),
            pad: identifier.clone(),
            r#type: EngineInputType::Analog,
            index: axis,
            axis_value: value,
            ..Default::default()
        };
        let callbacks: Vec<_> = self
            .callback_list
            .values()
            .filter(|poller| {
                Self::is_input_identifier_equal(poller, identifier, EngineInputType::Analog, axis)
            })
            .filter_map(|poller| poller.callback.on_change.clone())
            .collect();
        let pending = PendingCallbacks {
            event: event.clone(),
            callbacks,
        };
        if !self.configuring || self.mapping_callback.on_data.is_none() {
            return pending;
        }
        if (value - self.get_axis(identifier, axis)).abs() < 0.5 {
            return pending;
        }
        if let Some(ref on_data) = self.mapping_callback.on_data {
            on_data(&event);
        }
        pending
    }

    /// Port of InputEngine::TriggerOnBatteryChange
    fn trigger_on_battery_change(
        &self,
        identifier: &PadIdentifier,
        _value: BatteryLevel,
    ) -> PendingCallbacks {
        let _lock = self.mutex_callback.lock();
        let event = MappingData {
            engine: self.get_engine_name().to_string(),
            pad: identifier.clone(),
            r#type: EngineInputType::Battery,
            ..Default::default()
        };
        let callbacks: Vec<_> = self
            .callback_list
            .values()
            .filter(|poller| {
                Self::is_input_identifier_equal(poller, identifier, EngineInputType::Battery, 0)
            })
            .filter_map(|poller| poller.callback.on_change.clone())
            .collect();
        PendingCallbacks { event, callbacks }
    }

    /// Port of InputEngine::TriggerOnColorChange
    fn trigger_on_color_change(
        &self,
        identifier: &PadIdentifier,
        _value: BodyColorStatus,
    ) -> PendingCallbacks {
        let _lock = self.mutex_callback.lock();
        let event = MappingData {
            engine: self.get_engine_name().to_string(),
            pad: identifier.clone(),
            r#type: EngineInputType::Color,
            ..Default::default()
        };
        let callbacks: Vec<_> = self
            .callback_list
            .values()
            .filter(|poller| {
                Self::is_input_identifier_equal(poller, identifier, EngineInputType::Color, 0)
            })
            .filter_map(|poller| poller.callback.on_change.clone())
            .collect();
        PendingCallbacks { event, callbacks }
    }

    /// Port of InputEngine::TriggerOnMotionChange
    fn trigger_on_motion_change(
        &self,
        identifier: &PadIdentifier,
        motion: i32,
        value: &BasicMotion,
    ) -> PendingCallbacks {
        let _lock = self.mutex_callback.lock();
        let event = MappingData {
            engine: self.get_engine_name().to_string(),
            pad: identifier.clone(),
            r#type: EngineInputType::Motion,
            index: motion,
            motion_value: value.clone(),
            ..Default::default()
        };
        let callbacks: Vec<_> = self
            .callback_list
            .values()
            .filter(|poller| {
                Self::is_input_identifier_equal(poller, identifier, EngineInputType::Motion, motion)
            })
            .filter_map(|poller| poller.callback.on_change.clone())
            .collect();
        let pending = PendingCallbacks { event, callbacks };
        if !self.configuring || self.mapping_callback.on_data.is_none() {
            return pending;
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
            return pending;
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
        pending
    }

    /// Port of InputEngine::TriggerOnCameraChange
    fn trigger_on_camera_change(
        &self,
        identifier: &PadIdentifier,
        _value: &CameraStatus,
    ) -> PendingCallbacks {
        let _lock = self.mutex_callback.lock();
        let event = MappingData {
            engine: self.get_engine_name().to_string(),
            pad: identifier.clone(),
            r#type: EngineInputType::Camera,
            ..Default::default()
        };
        let callbacks: Vec<_> = self
            .callback_list
            .values()
            .filter(|poller| {
                Self::is_input_identifier_equal(poller, identifier, EngineInputType::Camera, 0)
            })
            .filter_map(|poller| poller.callback.on_change.clone())
            .collect();
        PendingCallbacks { event, callbacks }
    }

    /// Port of InputEngine::TriggerOnNfcChange
    fn trigger_on_nfc_change(
        &self,
        identifier: &PadIdentifier,
        _value: &NfcStatus,
    ) -> PendingCallbacks {
        let _lock = self.mutex_callback.lock();
        let event = MappingData {
            engine: self.get_engine_name().to_string(),
            pad: identifier.clone(),
            r#type: EngineInputType::Nfc,
            ..Default::default()
        };
        let callbacks: Vec<_> = self
            .callback_list
            .values()
            .filter(|poller| {
                Self::is_input_identifier_equal(poller, identifier, EngineInputType::Nfc, 0)
            })
            .filter_map(|poller| poller.callback.on_change.clone())
            .collect();
        PendingCallbacks { event, callbacks }
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    struct TestOutput {
        calls: Arc<AtomicUsize>,
    }

    impl InputEngineOutput for TestOutput {
        fn set_vibration(
            &self,
            _identifier: &PadIdentifier,
            _status: &VibrationStatus,
        ) -> DriverResult {
            self.calls.fetch_add(1, Ordering::Relaxed);
            DriverResult::Success
        }

        fn is_vibration_enabled(&self, _identifier: &PadIdentifier) -> bool {
            true
        }
    }

    fn callback_that_relocks_engine(
        engine: &Arc<Mutex<InputEngine>>,
        calls: Arc<AtomicUsize>,
    ) -> UpdateCallback {
        let engine = Arc::downgrade(engine);
        UpdateCallback {
            on_change: Some(Arc::new(move |_| {
                let engine = engine.upgrade().expect("engine must still exist");
                assert!(
                    engine.try_lock().is_some(),
                    "input callbacks must run after the shared engine lock is released"
                );
                calls.fetch_add(1, Ordering::Relaxed);
            })),
        }
    }

    #[test]
    fn pending_callback_can_relock_shared_engine() {
        let engine = Arc::new(Mutex::new(InputEngine::new("test".to_string())));
        let identifier = PadIdentifier::default();
        let calls = Arc::new(AtomicUsize::new(0));
        {
            let mut guard = engine.lock();
            guard.pre_set_controller(&identifier);
            guard.pre_set_button(&identifier, 0);
            guard.set_callback(InputIdentifier {
                identifier: identifier.clone(),
                r#type: EngineInputType::Button,
                index: 0,
                callback: callback_that_relocks_engine(&engine, Arc::clone(&calls)),
            });
        }

        let pending = engine.lock().set_button(&identifier, 0, true);
        assert_eq!(calls.load(Ordering::Relaxed), 0);
        pending.dispatch();
        assert_eq!(calls.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn reset_callbacks_can_relock_shared_engine() {
        let engine = Arc::new(Mutex::new(InputEngine::new("test".to_string())));
        let identifier = PadIdentifier::default();
        let calls = Arc::new(AtomicUsize::new(0));
        {
            let mut guard = engine.lock();
            guard.pre_set_controller(&identifier);
            guard.pre_set_button(&identifier, 0);
            guard.set_callback(InputIdentifier {
                identifier: identifier.clone(),
                r#type: EngineInputType::Button,
                index: 0,
                callback: callback_that_relocks_engine(&engine, Arc::clone(&calls)),
            });
        }

        let pending = engine.lock().reset_button_state();
        assert_eq!(calls.load(Ordering::Relaxed), 0);
        for callback in pending {
            callback.dispatch();
        }
        assert_eq!(calls.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn output_calls_use_the_concrete_engine_handler() {
        let calls = Arc::new(AtomicUsize::new(0));
        let mut engine = InputEngine::new("test".to_string());
        engine.set_output_handler(Arc::new(TestOutput {
            calls: Arc::clone(&calls),
        }));
        let identifier = PadIdentifier::default();

        assert_eq!(
            engine.set_vibration(&identifier, &VibrationStatus::default()),
            DriverResult::Success
        );
        assert!(engine.is_vibration_enabled(&identifier));
        assert_eq!(calls.load(Ordering::Relaxed), 1);
    }
}
