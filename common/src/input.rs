// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/common/input.h
//! Input device factory, device types, and controller abstractions.

use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};

use log::error;

use crate::param_package::ParamPackage;
use crate::uuid::UUID;

// =====================
// Enums
// =====================

/// Type of data that is expected to receive or send.
///
/// Maps to upstream `InputType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputType {
    None,
    Battery,
    Button,
    Stick,
    Analog,
    Trigger,
    Motion,
    Touch,
    Color,
    Vibration,
    Nfc,
    IrSensor,
}

impl Default for InputType {
    fn default() -> Self {
        InputType::None
    }
}

/// Internal battery charge level.
///
/// Maps to upstream `BatteryLevel`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum BatteryLevel {
    None = 0,
    Empty = 1,
    Critical = 2,
    Low = 3,
    Medium = 4,
    Full = 5,
    Charging = 6,
}

impl Default for BatteryLevel {
    fn default() -> Self {
        BatteryLevel::None
    }
}

/// Polling mode.
///
/// Maps to upstream `PollingMode`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PollingMode {
    /// Constant polling of buttons, analogs and motion data.
    Active,
    /// Only update on button change, digital analogs.
    Passive,
    /// Enable near field communication polling.
    NFC,
    /// Enable infrared camera polling.
    IR,
    /// Enable ring controller polling.
    Ring,
}

/// Camera format.
///
/// Maps to upstream `CameraFormat`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CameraFormat {
    Size320x240,
    Size160x120,
    Size80x60,
    Size40x30,
    Size20x15,
    None,
}

impl Default for CameraFormat {
    fn default() -> Self {
        CameraFormat::None
    }
}

/// Different results that can happen from a device request.
///
/// Maps to upstream `DriverResult`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DriverResult {
    Success,
    WrongReply,
    Timeout,
    UnsupportedControllerType,
    HandleInUse,
    ErrorReadingData,
    ErrorWritingData,
    NoDeviceDetected,
    InvalidHandle,
    InvalidParameters,
    NotSupported,
    Disabled,
    Delayed,
    Unknown,
}

/// NFC reply from the controller.
///
/// Maps to upstream `NfcState`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NfcState {
    Success,
    NewAmiibo,
    WaitingForAmiibo,
    AmiiboRemoved,
    InvalidTagType,
    NotSupported,
    WrongDeviceState,
    WriteFailed,
    Unknown,
}

impl Default for NfcState {
    fn default() -> Self {
        NfcState::Unknown
    }
}

/// Hint for amplification curve to be used.
///
/// Maps to upstream `VibrationAmplificationType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VibrationAmplificationType {
    Linear,
    Exponential,
}

impl Default for VibrationAmplificationType {
    fn default() -> Self {
        VibrationAmplificationType::Linear
    }
}

// =====================
// Data Structures
// =====================

/// Analog properties for calibration.
///
/// Maps to upstream `AnalogProperties`.
#[derive(Debug, Clone, Copy)]
pub struct AnalogProperties {
    /// Anything below this value will be detected as zero.
    pub deadzone: f32,
    /// Anything above this value will be detected as one.
    pub range: f32,
    /// Minimum value to be detected as active.
    pub threshold: f32,
    /// Drift correction applied to the raw data.
    pub offset: f32,
    /// Invert direction of the sensor data.
    pub inverted: bool,
    /// Invert the state if it's converted to a button.
    pub inverted_button: bool,
    /// Press once to activate, press again to release.
    pub toggle: bool,
}

impl Default for AnalogProperties {
    fn default() -> Self {
        Self {
            deadzone: 0.0,
            range: 1.0,
            threshold: 0.5,
            offset: 0.0,
            inverted: false,
            inverted_button: false,
            toggle: false,
        }
    }
}

/// Single analog sensor data.
///
/// Maps to upstream `AnalogStatus`.
#[derive(Debug, Clone, Copy, Default)]
pub struct AnalogStatus {
    pub value: f32,
    pub raw_value: f32,
    pub properties: AnalogProperties,
}

/// Button data.
///
/// Maps to upstream `ButtonStatus`.
#[derive(Debug, Clone, Copy)]
pub struct ButtonStatus {
    pub uuid: UUID,
    pub value: bool,
    /// Invert value of the button.
    pub inverted: bool,
    /// Press once to activate, press again to release.
    pub toggle: bool,
    /// Spams the button when active.
    pub turbo: bool,
    /// Internal lock for the toggle status.
    pub locked: bool,
}

impl Default for ButtonStatus {
    fn default() -> Self {
        Self {
            uuid: UUID::new(),
            value: false,
            inverted: false,
            toggle: false,
            turbo: false,
            locked: false,
        }
    }
}

/// Internal battery data.
///
/// Maps to upstream `BatteryStatus`.
pub type BatteryStatus = BatteryLevel;

/// Analog and digital joystick data.
///
/// Maps to upstream `StickStatus`.
#[derive(Debug, Clone, Copy)]
pub struct StickStatus {
    pub uuid: UUID,
    pub x: AnalogStatus,
    pub y: AnalogStatus,
    pub left: bool,
    pub right: bool,
    pub up: bool,
    pub down: bool,
}

impl Default for StickStatus {
    fn default() -> Self {
        Self {
            uuid: UUID::new(),
            x: AnalogStatus::default(),
            y: AnalogStatus::default(),
            left: false,
            right: false,
            up: false,
            down: false,
        }
    }
}

/// Analog and digital trigger data.
///
/// Maps to upstream `TriggerStatus`.
#[derive(Debug, Clone, Copy)]
pub struct TriggerStatus {
    pub uuid: UUID,
    pub analog: AnalogStatus,
    pub pressed: ButtonStatus,
}

impl Default for TriggerStatus {
    fn default() -> Self {
        Self {
            uuid: UUID::new(),
            analog: AnalogStatus::default(),
            pressed: ButtonStatus::default(),
        }
    }
}

/// 3D vector representing motion input.
///
/// Maps to upstream `MotionSensor`.
#[derive(Debug, Clone, Copy, Default)]
pub struct MotionSensor {
    pub x: AnalogStatus,
    pub y: AnalogStatus,
    pub z: AnalogStatus,
}

/// Motion data used to calculate controller orientation.
///
/// Maps to upstream `MotionStatus`.
#[derive(Debug, Clone, Copy, Default)]
pub struct MotionStatus {
    /// Gyroscope vector measurement in radians/s.
    pub gyro: MotionSensor,
    /// Acceleration vector measurement in G force.
    pub accel: MotionSensor,
    /// Time since last measurement in microseconds.
    pub delta_timestamp: u64,
    /// Request to update after reading the value.
    pub force_update: bool,
}

/// Data of a single point on a touch screen.
///
/// Maps to upstream `TouchStatus`.
#[derive(Debug, Clone, Copy, Default)]
pub struct TouchStatus {
    pub pressed: ButtonStatus,
    pub x: AnalogStatus,
    pub y: AnalogStatus,
    pub id: i32,
}

/// Physical controller color in RGB format.
///
/// Maps to upstream `BodyColorStatus`.
#[derive(Debug, Clone, Copy, Default)]
pub struct BodyColorStatus {
    pub body: u32,
    pub buttons: u32,
    pub left_grip: u32,
    pub right_grip: u32,
}

/// HD rumble data.
///
/// Maps to upstream `VibrationStatus`.
#[derive(Debug, Clone, Copy)]
pub struct VibrationStatus {
    pub low_amplitude: f32,
    pub low_frequency: f32,
    pub high_amplitude: f32,
    pub high_frequency: f32,
    pub amplification_type: VibrationAmplificationType,
}

impl Default for VibrationStatus {
    fn default() -> Self {
        Self {
            low_amplitude: 0.0,
            low_frequency: 0.0,
            high_amplitude: 0.0,
            high_frequency: 0.0,
            amplification_type: VibrationAmplificationType::Linear,
        }
    }
}

/// Physical controller LED pattern.
///
/// Maps to upstream `LedStatus`.
#[derive(Debug, Clone, Copy, Default)]
pub struct LedStatus {
    pub led_1: bool,
    pub led_2: bool,
    pub led_3: bool,
    pub led_4: bool,
}

/// Raw data from camera.
///
/// Maps to upstream `CameraStatus`.
#[derive(Debug, Clone, Default)]
pub struct CameraStatus {
    pub format: CameraFormat,
    pub data: Vec<u8>,
}

/// NFC status data.
///
/// Maps to upstream `NfcStatus`.
#[derive(Debug, Clone)]
pub struct NfcStatus {
    pub state: NfcState,
    pub uuid_length: u8,
    pub protocol: u8,
    pub tag_type: u8,
    pub uuid: [u8; 10],
}

impl Default for NfcStatus {
    fn default() -> Self {
        Self {
            state: NfcState::Unknown,
            uuid_length: 0,
            protocol: 0,
            tag_type: 0,
            uuid: [0u8; 10],
        }
    }
}

/// Mifare data for a single sector.
///
/// Maps to upstream `MifareData`.
#[derive(Debug, Clone, Copy)]
pub struct MifareData {
    pub command: u8,
    pub sector: u8,
    pub key: [u8; 0x6],
    pub data: [u8; 0x10],
}

impl Default for MifareData {
    fn default() -> Self {
        Self {
            command: 0,
            sector: 0,
            key: [0u8; 0x6],
            data: [0u8; 0x10],
        }
    }
}

/// Mifare request containing multiple data entries.
///
/// Maps to upstream `MifareRequest`.
#[derive(Debug, Clone)]
pub struct MifareRequest {
    pub data: [MifareData; 0x10],
}

impl Default for MifareRequest {
    fn default() -> Self {
        Self {
            data: [MifareData::default(); 0x10],
        }
    }
}

/// List of buttons to be passed to Qt that can be translated.
///
/// Maps to upstream `ButtonNames`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ButtonNames {
    Undefined,
    Invalid,
    /// This will display the engine name instead of the button name.
    Engine,
    /// This will display the button by value instead of the button name.
    Value,

    // Joycon button names
    ButtonLeft,
    ButtonRight,
    ButtonDown,
    ButtonUp,
    ButtonA,
    ButtonB,
    ButtonX,
    ButtonY,
    ButtonPlus,
    ButtonMinus,
    ButtonHome,
    ButtonCapture,
    ButtonStickL,
    ButtonStickR,
    TriggerL,
    TriggerZL,
    TriggerSL,
    TriggerR,
    TriggerZR,
    TriggerSR,

    // GC button names
    TriggerZ,
    ButtonStart,

    // DS4 button names
    L1,
    L2,
    L3,
    R1,
    R2,
    R3,
    Circle,
    Cross,
    Square,
    Triangle,
    Share,
    Options,
    Home,
    Touch,

    // Mouse buttons
    ButtonMouseWheel,
    ButtonBackward,
    ButtonForward,
    ButtonTask,
    ButtonExtra,
}

// =====================
// Callback types
// =====================

/// Callback data consisting of an input type and the equivalent data status.
///
/// Maps to upstream `CallbackStatus`.
#[derive(Debug, Clone, Default)]
pub struct CallbackStatus {
    pub input_type: InputType,
    pub button_status: ButtonStatus,
    pub stick_status: StickStatus,
    pub analog_status: AnalogStatus,
    pub trigger_status: TriggerStatus,
    pub motion_status: MotionStatus,
    pub touch_status: TouchStatus,
    pub color_status: BodyColorStatus,
    pub battery_status: BatteryStatus,
    pub vibration_status: VibrationStatus,
    pub camera_status: CameraFormat,
    pub nfc_status: NfcStatus,
    pub raw_data: Vec<u8>,
}

/// Triggered once every input change.
///
/// Maps to upstream `InputCallback`.
pub struct InputCallback {
    pub on_change: Option<Box<dyn Fn(&CallbackStatus) + Send + Sync>>,
}

// =====================
// Input/Output Device Traits
// =====================

/// An abstract trait for an input device (a button, an analog input, etc.).
///
/// Maps to upstream `InputDevice` class.
pub trait InputDevice: Send + Sync {
    /// Force input device to update data regardless of the current state.
    fn force_update(&mut self) {}

    /// Sets the function to be triggered when input changes.
    fn set_callback(&mut self, callback: InputCallback);

    /// Triggers the function set in the callback.
    fn trigger_on_change(&self, status: &CallbackStatus);
}

/// A basic InputDevice implementation that holds a callback.
///
/// This can be used as a base for concrete input device types.
pub struct BasicInputDevice {
    callback: InputCallback,
}

impl BasicInputDevice {
    pub fn new() -> Self {
        Self {
            callback: InputCallback { on_change: None },
        }
    }
}

impl InputDevice for BasicInputDevice {
    fn set_callback(&mut self, callback: InputCallback) {
        self.callback = callback;
    }

    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change {
            on_change(status);
        }
    }
}

/// An abstract trait for an output device (rumble, LED pattern, polling mode).
///
/// Maps to upstream `OutputDevice` class.
pub trait OutputDevice: Send + Sync {
    fn set_led(&mut self, _led_status: &LedStatus) -> DriverResult {
        DriverResult::NotSupported
    }

    fn set_vibration(&mut self, _vibration_status: &VibrationStatus) -> DriverResult {
        DriverResult::NotSupported
    }

    fn is_vibration_enabled(&self) -> bool {
        false
    }

    fn set_polling_mode(&mut self, _polling_mode: PollingMode) -> DriverResult {
        DriverResult::NotSupported
    }

    fn set_camera_format(&mut self, _camera_format: CameraFormat) -> DriverResult {
        DriverResult::NotSupported
    }

    fn supports_nfc(&self) -> NfcState {
        NfcState::NotSupported
    }

    fn start_nfc_polling(&mut self) -> NfcState {
        NfcState::NotSupported
    }

    fn stop_nfc_polling(&mut self) -> NfcState {
        NfcState::NotSupported
    }

    fn read_amiibo_data(&mut self, _out_data: &mut Vec<u8>) -> NfcState {
        NfcState::NotSupported
    }

    fn write_nfc_data(&mut self, _data: &[u8]) -> NfcState {
        NfcState::NotSupported
    }

    fn read_mifare_data(&mut self, _request: &MifareRequest, _out_data: &mut MifareRequest) -> NfcState {
        NfcState::NotSupported
    }

    fn write_mifare_data(&mut self, _request: &MifareRequest) -> NfcState {
        NfcState::NotSupported
    }
}

/// A basic OutputDevice implementation (does nothing by default).
pub struct BasicOutputDevice;

impl BasicOutputDevice {
    pub fn new() -> Self {
        Self
    }
}

impl OutputDevice for BasicOutputDevice {}

// =====================
// Factory System
// =====================

/// An abstract trait for a factory that can create input devices.
///
/// Maps to upstream `Factory<InputDeviceType>` template class.
pub trait InputDeviceFactory: Send + Sync {
    fn create(&self, params: &ParamPackage) -> Box<dyn InputDevice>;
}

/// An abstract trait for a factory that can create output devices.
pub trait OutputDeviceFactory: Send + Sync {
    fn create(&self, params: &ParamPackage) -> Box<dyn OutputDevice>;
}

/// Global factory list for input devices.
fn input_factory_list() -> &'static Mutex<HashMap<String, Arc<dyn InputDeviceFactory>>> {
    static INSTANCE: OnceLock<Mutex<HashMap<String, Arc<dyn InputDeviceFactory>>>> = OnceLock::new();
    INSTANCE.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Global factory list for output devices.
fn output_factory_list() -> &'static Mutex<HashMap<String, Arc<dyn OutputDeviceFactory>>> {
    static INSTANCE: OnceLock<Mutex<HashMap<String, Arc<dyn OutputDeviceFactory>>>> = OnceLock::new();
    INSTANCE.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Registers an input device factory.
///
/// Maps to upstream `RegisterFactory<InputDevice>` / `RegisterInputFactory`.
pub fn register_input_factory(name: &str, factory: Arc<dyn InputDeviceFactory>) {
    let mut list = input_factory_list().lock().unwrap();
    if list.contains_key(name) {
        error!("Factory '{}' already registered", name);
        return;
    }
    list.insert(name.to_string(), factory);
}

/// Registers an output device factory.
///
/// Maps to upstream `RegisterFactory<OutputDevice>` / `RegisterOutputFactory`.
pub fn register_output_factory(name: &str, factory: Arc<dyn OutputDeviceFactory>) {
    let mut list = output_factory_list().lock().unwrap();
    if list.contains_key(name) {
        error!("Factory '{}' already registered", name);
        return;
    }
    list.insert(name.to_string(), factory);
}

/// Unregisters an input device factory.
///
/// Maps to upstream `UnregisterFactory<InputDevice>` / `UnregisterInputFactory`.
pub fn unregister_input_factory(name: &str) {
    let mut list = input_factory_list().lock().unwrap();
    if list.remove(name).is_none() {
        error!("Factory '{}' not registered", name);
    }
}

/// Unregisters an output device factory.
///
/// Maps to upstream `UnregisterFactory<OutputDevice>` / `UnregisterOutputFactory`.
pub fn unregister_output_factory(name: &str) {
    let mut list = output_factory_list().lock().unwrap();
    if list.remove(name).is_none() {
        error!("Factory '{}' not registered", name);
    }
}

/// Create an input device from a serialized parameter string.
///
/// Maps to upstream `CreateDeviceFromString<InputDevice>` / `CreateInputDeviceFromString`.
pub fn create_input_device_from_string(params: &str) -> Box<dyn InputDevice> {
    let package = ParamPackage::from_serialized(params);
    create_input_device(&package)
}

/// Create an output device from a serialized parameter string.
///
/// Maps to upstream `CreateDeviceFromString<OutputDevice>` / `CreateOutputDeviceFromString`.
pub fn create_output_device_from_string(params: &str) -> Box<dyn OutputDevice> {
    let package = ParamPackage::from_serialized(params);
    create_output_device(&package)
}

/// Create an input device from a ParamPackage.
///
/// Maps to upstream `CreateDevice<InputDevice>` / `CreateInputDevice`.
pub fn create_input_device(package: &ParamPackage) -> Box<dyn InputDevice> {
    let engine = package.get_str("engine", "null");
    let list = input_factory_list().lock().unwrap();
    match list.get(&engine) {
        Some(factory) => factory.create(package),
        None => {
            if engine != "null" {
                error!("Unknown engine name: {}", engine);
            }
            Box::new(BasicInputDevice::new())
        }
    }
}

/// Create an output device from a ParamPackage.
///
/// Maps to upstream `CreateDevice<OutputDevice>` / `CreateOutputDevice`.
pub fn create_output_device(package: &ParamPackage) -> Box<dyn OutputDevice> {
    let engine = package.get_str("engine", "null");
    let list = output_factory_list().lock().unwrap();
    match list.get(&engine) {
        Some(factory) => factory.create(package),
        None => {
            if engine != "null" {
                error!("Unknown engine name: {}", engine);
            }
            Box::new(BasicOutputDevice::new())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_values() {
        let analog = AnalogProperties::default();
        assert_eq!(analog.deadzone, 0.0);
        assert_eq!(analog.range, 1.0);
        assert_eq!(analog.threshold, 0.5);

        let button = ButtonStatus::default();
        assert!(!button.value);

        let vib = VibrationStatus::default();
        assert_eq!(vib.amplification_type, VibrationAmplificationType::Linear);
    }

    #[test]
    fn test_basic_input_device() {
        let mut device = BasicInputDevice::new();
        let called = Arc::new(Mutex::new(false));
        let called_clone = called.clone();

        device.set_callback(InputCallback {
            on_change: Some(Box::new(move |_status| {
                *called_clone.lock().unwrap() = true;
            })),
        });

        let status = CallbackStatus::default();
        device.trigger_on_change(&status);
        assert!(*called.lock().unwrap());
    }

    #[test]
    fn test_basic_output_device() {
        let mut device = BasicOutputDevice::new();
        assert_eq!(device.set_led(&LedStatus::default()), DriverResult::NotSupported);
        assert_eq!(device.set_vibration(&VibrationStatus::default()), DriverResult::NotSupported);
        assert!(!device.is_vibration_enabled());
        assert_eq!(device.supports_nfc(), NfcState::NotSupported);
    }

    #[test]
    fn test_create_null_devices() {
        let input = create_input_device_from_string("");
        let output = create_output_device_from_string("");
        // They should not panic and return default devices
        let _ = input;
        let _ = output;
    }

    #[test]
    fn test_mifare_request_default() {
        let req = MifareRequest::default();
        assert_eq!(req.data.len(), 0x10);
        assert_eq!(req.data[0].command, 0);
    }
}
