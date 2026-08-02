// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/frontend/emulated_controller.h and emulated_controller.cpp

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, OnceLock};
use std::time::Instant;

use parking_lot::Mutex;

use common::input::{
    ButtonStatus, CallbackStatus, DriverResult, InputCallback, InputDevice, OutputDevice,
    StickStatus, TriggerStatus, VibrationAmplificationType, VibrationStatus,
};
use common::param_package::ParamPackage;
use common::settings_input::{self, ControllerType};
use common::uuid::UUID;

use crate::frontend::input_converter::{
    transform_to_button, transform_to_stick, transform_to_trigger,
};
use crate::frontend::motion_input::IS_AT_REST_STANDARD;
use crate::hid_types::*;

pub const MAX_EMULATED_CONTROLLERS: usize = 2;
pub const OUTPUT_DEVICES_SIZE: usize = 5;

pub const HID_JOYSTICK_MAX: i32 = 0x7fff;
pub const HID_TRIGGER_MAX: i32 = 0x7fff;
pub const TURBO_BUTTON_DELAY: u32 = 4;
// Use a common UUID for TAS and Virtual Gamepad.
const TAS_UUID: UUID = UUID::from_bytes([
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x7, 0xA5, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
]);

static SIMPLE_NPAD_BUTTON_STATE: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone, Copy)]
struct ScriptedNpadPress {
    start_ms: u64,
    duration_ms: u64,
    buttons: u64,
}

fn parse_u64_auto(value: &str) -> Option<u64> {
    let value = value.trim();
    if let Some(hex) = value
        .strip_prefix("0x")
        .or_else(|| value.strip_prefix("0X"))
    {
        u64::from_str_radix(hex, 16).ok()
    } else {
        value.parse::<u64>().ok()
    }
}

fn scripted_npad_presses() -> &'static [ScriptedNpadPress] {
    static PRESSES: OnceLock<Vec<ScriptedNpadPress>> = OnceLock::new();
    PRESSES.get_or_init(|| {
        let Some(spec) = std::env::var("RUZU_SCRIPTED_NPAD").ok() else {
            return Vec::new();
        };
        spec.split(',')
            .filter_map(|entry| {
                let mut parts = entry.split(':');
                let start_ms = parse_u64_auto(parts.next()?)?;
                let buttons = parse_u64_auto(parts.next()?)?;
                let duration_ms = parts.next().and_then(parse_u64_auto).unwrap_or(250);
                Some(ScriptedNpadPress {
                    start_ms,
                    duration_ms,
                    buttons,
                })
            })
            .collect()
    })
}

fn scripted_npad_button_bits() -> u64 {
    static START: OnceLock<Instant> = OnceLock::new();
    let presses = scripted_npad_presses();
    if presses.is_empty() {
        return 0;
    }
    let elapsed_ms = START.get_or_init(Instant::now).elapsed().as_millis() as u64;
    presses.iter().fold(0u64, |bits, press| {
        if elapsed_ms >= press.start_ms
            && elapsed_ms < press.start_ms.saturating_add(press.duration_ms)
        {
            bits | press.buttons
        } else {
            bits
        }
    })
}

/// Temporary frontend bridge for the SDL command-line frontend while the full
/// upstream InputSubsystem -> EmulatedController callback wiring is incomplete.
pub fn set_simple_npad_button(button: NpadButton, pressed: bool) {
    if pressed {
        SIMPLE_NPAD_BUTTON_STATE.fetch_or(button.bits(), Ordering::Relaxed);
    } else {
        SIMPLE_NPAD_BUTTON_STATE.fetch_and(!button.bits(), Ordering::Relaxed);
    }
}

pub fn get_simple_npad_button_state() -> NpadButtonState {
    NpadButtonState {
        raw: NpadButton::from_bits_truncate(
            SIMPLE_NPAD_BUTTON_STATE.load(Ordering::Relaxed) | scripted_npad_button_bits(),
        ),
    }
}

/// Keeps the env-gated scripted stick-direction bits consistent with the
/// analog coordinates a real `EmulatedController::SetStick` update exposes.
pub fn apply_simple_npad_stick_buttons(sticks: &mut AnalogSticks, buttons: NpadButton) {
    let left_x = i32::from(buttons.contains(NpadButton::STICK_L_RIGHT))
        - i32::from(buttons.contains(NpadButton::STICK_L_LEFT));
    let left_y = i32::from(buttons.contains(NpadButton::STICK_L_UP))
        - i32::from(buttons.contains(NpadButton::STICK_L_DOWN));
    let right_x = i32::from(buttons.contains(NpadButton::STICK_R_RIGHT))
        - i32::from(buttons.contains(NpadButton::STICK_R_LEFT));
    let right_y = i32::from(buttons.contains(NpadButton::STICK_R_UP))
        - i32::from(buttons.contains(NpadButton::STICK_R_DOWN));

    let left_active = buttons.intersects(
        NpadButton::STICK_L_LEFT
            | NpadButton::STICK_L_UP
            | NpadButton::STICK_L_RIGHT
            | NpadButton::STICK_L_DOWN,
    );
    let right_active = buttons.intersects(
        NpadButton::STICK_R_LEFT
            | NpadButton::STICK_R_UP
            | NpadButton::STICK_R_RIGHT
            | NpadButton::STICK_R_DOWN,
    );
    if left_active {
        sticks.left.x = left_x * HID_JOYSTICK_MAX;
        sticks.left.y = left_y * HID_JOYSTICK_MAX;
    }
    if right_active {
        sticks.right.x = right_x * HID_JOYSTICK_MAX;
        sticks.right.y = right_y * HID_JOYSTICK_MAX;
    }
}

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
    pub on_change: Arc<dyn Fn(ControllerTriggerType) + Send + Sync>,
    pub is_npad_service: bool,
}

/// State needed by input-device callbacks after `EmulatedController` has
/// handed them to a driver thread.
struct ControllerEventContext {
    npad_id_type: NpadIdType,
    is_connected: AtomicBool,
    supported_style_tag: Mutex<NpadStyleTag>,
    callback_list: Mutex<HashMap<i32, ControllerUpdateCallback>>,
}

fn trigger_on_change(
    context: &ControllerEventContext,
    trigger_type: ControllerTriggerType,
    is_npad_service_update: bool,
) {
    let callbacks: Vec<_> = context
        .callback_list
        .lock()
        .values()
        .filter(|callback| is_npad_service_update || !callback.is_npad_service)
        .map(|callback| Arc::clone(&callback.on_change))
        .collect();
    for callback in callbacks {
        callback(trigger_type);
    }
}

fn is_controller_supported(npad: NpadStyleIndex, supported: NpadStyleTag) -> bool {
    let styles = supported.raw;
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

/// The raw device values behind `ControllerStatus`, upstream's
/// `button_values` / `stick_values` / `trigger_values`.
///
/// Upstream keeps these next to the HID-service state in one `ControllerStatus`
/// struct guarded by the controller's mutex. Here they live behind their own
/// `Arc<Mutex<..>>` because the input devices call back from SDL's thread and
/// must be able to write them without holding a borrow of the controller.
#[derive(Debug, Default)]
pub struct ControllerStatus {
    // Data from input_common
    pub button_values: Vec<ButtonStatus>,
    pub stick_values: Vec<StickStatus>,
    pub trigger_values: Vec<TriggerStatus>,

    // Data for HID services
    pub home_button_state: HomeButtonState,
    pub capture_button_state: CaptureButtonState,
    pub npad_button_state: NpadButtonState,
    pub debug_pad_button_state: DebugPadButton,
    pub analog_stick_state: AnalogSticks,
    pub gc_trigger_state: NpadGcTriggerState,

    /// Mirrors the controller's `npad_type`, so `set_button` can apply
    /// upstream's GameCube special case without reaching back into it.
    pub npad_type: NpadStyleIndex,
    /// Mirrors `is_configuring`; upstream reports nothing to the HID services
    /// while the configuration dialog is open.
    pub is_configuring: bool,
    /// Mirrors `system_buttons_enabled`, which gates Home and Capture.
    pub system_buttons_enabled: bool,
}

impl ControllerStatus {
    fn new() -> Self {
        Self {
            button_values: vec![
                ButtonStatus::default();
                settings_input::native_button::NUM_BUTTONS
            ],
            stick_values: vec![StickStatus::default(); settings_input::native_analog::NUM_ANALOGS],
            trigger_values: vec![
                TriggerStatus::default();
                settings_input::native_trigger::NUM_TRIGGERS
            ],
            home_button_state: HomeButtonState::default(),
            capture_button_state: CaptureButtonState::default(),
            npad_button_state: NpadButtonState::default(),
            debug_pad_button_state: DebugPadButton::default(),
            analog_stick_state: AnalogSticks::default(),
            gc_trigger_state: NpadGcTriggerState::default(),
            npad_type: NpadStyleIndex::None,
            is_configuring: false,
            system_buttons_enabled: true,
        }
    }
}

/// Port of EmulatedController::SetButton.
///
/// Free-standing because the input devices call it from the driver's thread and
/// cannot borrow the controller; everything it touches lives in the shared
/// `ControllerStatus`, including the HID-service state the guest reads.
fn set_button(
    status: &Arc<Mutex<ControllerStatus>>,
    event_context: &Arc<ControllerEventContext>,
    callback: &CallbackStatus,
    index: usize,
    uuid: UUID,
) {
    use settings_input::native_button::Values as NB;

    let new_status = transform_to_button(callback);
    let mut status = status.lock();
    let Some(current_status) = status.button_values.get_mut(index) else {
        return;
    };

    // Only read button values that have the same uuid or are pressed once.
    if current_status.uuid != uuid && !new_status.value {
        return;
    }

    current_status.toggle = new_status.toggle;
    current_status.turbo = new_status.turbo;
    current_status.uuid = uuid;

    let mut value_changed = false;
    if !current_status.toggle {
        current_status.locked = false;
        if current_status.value != new_status.value {
            current_status.value = new_status.value;
            value_changed = true;
        }
    } else {
        // Toggle button and lock status.
        if new_status.value && !current_status.locked {
            current_status.locked = true;
            current_status.value = !current_status.value;
            value_changed = true;
        }
        // Unlock button ready for the next press.
        if !new_status.value && current_status.locked {
            current_status.locked = false;
        }
    }

    if !value_changed {
        return;
    }
    let value = current_status.value;

    if status.is_configuring {
        status.npad_button_state.raw = NpadButton::empty();
        status.debug_pad_button_state.raw = 0;
        status.home_button_state.raw = 0;
        status.capture_button_state.raw = 0;
        drop(status);
        trigger_on_change(event_context, ControllerTriggerType::Button, false);
        return;
    }

    // GC controllers have triggers, not buttons, on ZL and ZR.
    if status.npad_type == NpadStyleIndex::GameCube
        && (index == NB::ZL as usize || index == NB::ZR as usize)
    {
        return;
    }

    let system_buttons_enabled = status.system_buttons_enabled;
    let assign = |raw: &mut NpadButton, flag: NpadButton| {
        raw.set(flag, value);
    };
    let assign_debug = |raw: &mut u32, bit: u32| {
        if value {
            *raw |= 1u32 << bit;
        } else {
            *raw &= !(1u32 << bit);
        }
    };

    // Upstream's switch, in the same order. The debug pad shares the first
    // eleven bits with `DebugPadButton`.
    match index {
        i if i == NB::A as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::A);
            assign_debug(&mut status.debug_pad_button_state.raw, 0);
        }
        i if i == NB::B as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::B);
            assign_debug(&mut status.debug_pad_button_state.raw, 1);
        }
        i if i == NB::X as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::X);
            assign_debug(&mut status.debug_pad_button_state.raw, 2);
        }
        i if i == NB::Y as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::Y);
            assign_debug(&mut status.debug_pad_button_state.raw, 3);
        }
        i if i == NB::LStick as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::STICK_L);
        }
        i if i == NB::RStick as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::STICK_R);
        }
        i if i == NB::L as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::L);
            assign_debug(&mut status.debug_pad_button_state.raw, 4);
        }
        i if i == NB::R as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::R);
            assign_debug(&mut status.debug_pad_button_state.raw, 5);
        }
        i if i == NB::ZL as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::ZL);
            assign_debug(&mut status.debug_pad_button_state.raw, 6);
        }
        i if i == NB::ZR as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::ZR);
            assign_debug(&mut status.debug_pad_button_state.raw, 7);
        }
        i if i == NB::Plus as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::PLUS);
            assign_debug(&mut status.debug_pad_button_state.raw, 8);
        }
        i if i == NB::Minus as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::MINUS);
            assign_debug(&mut status.debug_pad_button_state.raw, 9);
        }
        i if i == NB::DLeft as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::LEFT);
            assign_debug(&mut status.debug_pad_button_state.raw, 10);
        }
        i if i == NB::DUp as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::UP);
            assign_debug(&mut status.debug_pad_button_state.raw, 11);
        }
        i if i == NB::DRight as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::RIGHT);
            assign_debug(&mut status.debug_pad_button_state.raw, 12);
        }
        i if i == NB::DDown as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::DOWN);
            assign_debug(&mut status.debug_pad_button_state.raw, 13);
        }
        i if i == NB::SLLeft as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::LEFT_SL);
        }
        i if i == NB::SLRight as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::RIGHT_SL);
        }
        i if i == NB::SRLeft as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::LEFT_SR);
        }
        i if i == NB::SRRight as usize => {
            assign(&mut status.npad_button_state.raw, NpadButton::RIGHT_SR);
        }
        i if i == NB::Home as usize => {
            if system_buttons_enabled {
                status.home_button_state.raw = u64::from(value);
            }
        }
        i if i == NB::Screenshot as usize => {
            if system_buttons_enabled {
                status.capture_button_state.raw = u64::from(value);
            }
        }
        _ => {}
    }

    let npad_type = status.npad_type;
    drop(status);

    if !event_context.is_connected.load(Ordering::Relaxed) {
        let should_connect = (event_context.npad_id_type == NpadIdType::Player1
            && npad_type != NpadStyleIndex::Handheld)
            || (event_context.npad_id_type == NpadIdType::Handheld
                && npad_type == NpadStyleIndex::Handheld);
        let supported =
            is_controller_supported(npad_type, *event_context.supported_style_tag.lock());
        if should_connect && supported && !event_context.is_connected.swap(true, Ordering::Relaxed)
        {
            trigger_on_change(event_context, ControllerTriggerType::Connected, true);
        }
    }
    trigger_on_change(event_context, ControllerTriggerType::Button, true);
}

/// Port of EmulatedController::SetStick.
fn set_stick(
    status: &Arc<Mutex<ControllerStatus>>,
    event_context: &Arc<ControllerEventContext>,
    callback: &CallbackStatus,
    index: usize,
    uuid: UUID,
) {
    use settings_input::native_analog::Values as NA;

    let stick_value = transform_to_stick(callback);
    let mut status = status.lock();
    let Some(current) = status.stick_values.get_mut(index) else {
        return;
    };

    // Only read stick values that have the same uuid or are over the threshold,
    // to avoid two devices flapping against each other.
    if current.uuid != uuid {
        let is_tas = uuid == TAS_UUID;
        if (is_tas && stick_value.x.value == 0.0 && stick_value.y.value == 0.0)
            || (!is_tas
                && !stick_value.down
                && !stick_value.up
                && !stick_value.left
                && !stick_value.right)
        {
            return;
        }
    }

    *current = stick_value;
    current.uuid = uuid;
    let (x, y) = (current.x.value, current.y.value);
    let (left, right, up, down) = (current.left, current.right, current.up, current.down);

    if status.is_configuring {
        status.analog_stick_state.left = AnalogStickState::default();
        status.analog_stick_state.right = AnalogStickState::default();
        drop(status);
        trigger_on_change(event_context, ControllerTriggerType::Stick, false);
        return;
    }

    let stick = AnalogStickState {
        x: (x * HID_JOYSTICK_MAX as f32) as i32,
        y: (y * HID_JOYSTICK_MAX as f32) as i32,
    };
    let raw = &mut status.npad_button_state.raw;
    if index == NA::LStick as usize {
        raw.set(NpadButton::STICK_L_LEFT, left);
        raw.set(NpadButton::STICK_L_UP, up);
        raw.set(NpadButton::STICK_L_RIGHT, right);
        raw.set(NpadButton::STICK_L_DOWN, down);
        status.analog_stick_state.left = stick;
    } else if index == NA::RStick as usize {
        raw.set(NpadButton::STICK_R_LEFT, left);
        raw.set(NpadButton::STICK_R_UP, up);
        raw.set(NpadButton::STICK_R_RIGHT, right);
        raw.set(NpadButton::STICK_R_DOWN, down);
        status.analog_stick_state.right = stick;
    }
    drop(status);
    trigger_on_change(event_context, ControllerTriggerType::Stick, true);
}

/// Port of EmulatedController::SetTrigger.
fn set_trigger(
    status: &Arc<Mutex<ControllerStatus>>,
    event_context: &Arc<ControllerEventContext>,
    callback: &CallbackStatus,
    index: usize,
    uuid: UUID,
) {
    let trigger_value = transform_to_trigger(callback);
    let mut status = status.lock();
    let Some(current) = status.trigger_values.get_mut(index) else {
        return;
    };

    // Only read trigger values that have the same uuid or are pressed once.
    if current.uuid != uuid && !trigger_value.pressed.value {
        let is_service_update = !status.is_configuring;
        drop(status);
        trigger_on_change(
            event_context,
            ControllerTriggerType::Trigger,
            is_service_update,
        );
        return;
    }

    *current = trigger_value;
    current.uuid = uuid;
    let analog = current.analog.value;
    let pressed = current.pressed.value;

    if status.is_configuring {
        status.gc_trigger_state.left = 0;
        status.gc_trigger_state.right = 0;
        drop(status);
        trigger_on_change(event_context, ControllerTriggerType::Trigger, false);
        return;
    }

    // Only GC controllers have analog triggers.
    if status.npad_type != NpadStyleIndex::GameCube {
        return;
    }

    let scaled = (analog * HID_TRIGGER_MAX as f32) as i32;
    if index == EmulatedDeviceIndex::LeftIndex as usize {
        status.gc_trigger_state.left = scaled;
        status.npad_button_state.raw.set(NpadButton::ZL, pressed);
    } else if index == EmulatedDeviceIndex::RightIndex as usize {
        status.gc_trigger_state.right = scaled;
        status.npad_button_state.raw.set(NpadButton::ZR, pressed);
    }
    drop(status);
    trigger_on_change(event_context, ControllerTriggerType::Trigger, true);
}

pub struct EmulatedController {
    npad_id_type: NpadIdType,
    npad_type: NpadStyleIndex,
    original_npad_type: NpadStyleIndex,
    is_configuring: bool,
    is_initialized: bool,
    system_buttons_enabled: bool,
    motion_sensitivity: f32,
    turbo_button_state: u32,
    nfc_handles: usize,
    last_vibration_value: [VibrationValue; 2],
    last_vibration_timepoint: [Option<Instant>; 2],

    // Temporary values to avoid doing changes while the controller is in configuring mode
    tmp_npad_type: NpadStyleIndex,
    tmp_is_connected: bool,

    mutex: Mutex<()>,
    event_context: Arc<ControllerEventContext>,
    last_callback_key: i32,

    // The parameters each input device is built from — upstream's
    // `button_params`, `stick_params`, `motion_params`, `trigger_params`,
    // `ring_params`.
    button_params: Vec<ParamPackage>,
    stick_params: Vec<ParamPackage>,
    motion_params: Vec<ParamPackage>,
    trigger_params: Vec<ParamPackage>,
    ring_params: [ParamPackage; 1],
    output_params: Vec<ParamPackage>,

    // The live devices, kept alive so their callbacks keep firing. Upstream's
    // `button_devices` / `stick_devices` / `trigger_devices`.
    button_devices: Vec<Box<dyn InputDevice>>,
    stick_devices: Vec<Box<dyn InputDevice>>,
    trigger_devices: Vec<Box<dyn InputDevice>>,
    output_devices: Vec<Box<dyn OutputDevice>>,

    /// The controller's status, shared with the device callbacks: they run on
    /// the driver's thread and cannot borrow the controller itself.
    status: Arc<Mutex<ControllerStatus>>,

    // Stores the current status of all controller input
    motion_state: MotionState,
    colors_state: ControllerColors,
    battery_state: BatteryLevelState,
    ring_analog_state: RingSensorForce,
}

fn vibration_status(vibration: VibrationValue, strength: f32) -> VibrationStatus {
    VibrationStatus {
        low_amplitude: (vibration.low_amplitude * strength).min(1.0),
        low_frequency: vibration.low_frequency,
        high_amplitude: (vibration.high_amplitude * strength).min(1.0),
        high_frequency: vibration.high_frequency,
        amplification_type: if strength > 0.7 {
            VibrationAmplificationType::Exponential
        } else {
            VibrationAmplificationType::Linear
        },
    }
}

impl EmulatedController {
    /// Port of EmulatedController::MapSettingsTypeToNPad.
    pub fn map_settings_type_to_npad(controller_type: ControllerType) -> NpadStyleIndex {
        match controller_type {
            ControllerType::ProController => NpadStyleIndex::Fullkey,
            ControllerType::DualJoyconDetached => NpadStyleIndex::JoyconDual,
            ControllerType::LeftJoycon => NpadStyleIndex::JoyconLeft,
            ControllerType::RightJoycon => NpadStyleIndex::JoyconRight,
            ControllerType::Handheld => NpadStyleIndex::Handheld,
            ControllerType::GameCube => NpadStyleIndex::GameCube,
            ControllerType::Pokeball => NpadStyleIndex::Pokeball,
            ControllerType::NES => NpadStyleIndex::NES,
            ControllerType::SNES => NpadStyleIndex::SNES,
            ControllerType::N64 => NpadStyleIndex::N64,
            ControllerType::SegaGenesis => NpadStyleIndex::SegaGenesis,
        }
    }

    /// Port of EmulatedController::MapNPadToSettingsType.
    pub fn map_npad_to_settings_type(npad_type: NpadStyleIndex) -> ControllerType {
        match npad_type {
            NpadStyleIndex::Fullkey => ControllerType::ProController,
            NpadStyleIndex::JoyconDual => ControllerType::DualJoyconDetached,
            NpadStyleIndex::JoyconLeft => ControllerType::LeftJoycon,
            NpadStyleIndex::JoyconRight => ControllerType::RightJoycon,
            NpadStyleIndex::Handheld => ControllerType::Handheld,
            NpadStyleIndex::GameCube => ControllerType::GameCube,
            NpadStyleIndex::Pokeball => ControllerType::Pokeball,
            NpadStyleIndex::NES => ControllerType::NES,
            NpadStyleIndex::SNES => ControllerType::SNES,
            NpadStyleIndex::N64 => ControllerType::N64,
            NpadStyleIndex::SegaGenesis => ControllerType::SegaGenesis,
            _ => ControllerType::ProController,
        }
    }

    pub fn new(npad_id_type: NpadIdType) -> Self {
        let event_context = Arc::new(ControllerEventContext {
            npad_id_type,
            is_connected: AtomicBool::new(false),
            supported_style_tag: Mutex::new(NpadStyleTag {
                raw: NpadStyleSet::ALL,
            }),
            callback_list: Mutex::new(HashMap::new()),
        });
        Self {
            npad_id_type,
            npad_type: NpadStyleIndex::None,
            original_npad_type: NpadStyleIndex::None,
            is_configuring: false,
            is_initialized: false,
            system_buttons_enabled: true,
            motion_sensitivity: IS_AT_REST_STANDARD,
            turbo_button_state: 0,
            nfc_handles: 0,
            last_vibration_value: [DEFAULT_VIBRATION_VALUE; 2],
            last_vibration_timepoint: [None; 2],
            tmp_npad_type: NpadStyleIndex::None,
            tmp_is_connected: false,
            mutex: Mutex::new(()),
            event_context,
            last_callback_key: 0,
            button_params: vec![
                ParamPackage::default();
                settings_input::native_button::NUM_BUTTONS
            ],
            stick_params: vec![ParamPackage::default(); settings_input::native_analog::NUM_ANALOGS],
            motion_params: vec![
                ParamPackage::default();
                settings_input::native_motion::NUM_MOTIONS
            ],
            trigger_params: vec![
                ParamPackage::default();
                settings_input::native_trigger::NUM_TRIGGERS
            ],
            ring_params: [ParamPackage::default()],
            output_params: vec![ParamPackage::default(); OUTPUT_DEVICES_SIZE],
            button_devices: Vec::new(),
            stick_devices: Vec::new(),
            trigger_devices: Vec::new(),
            output_devices: Vec::new(),
            status: Arc::new(Mutex::new(ControllerStatus::new())),
            motion_state: [ControllerMotion::default(); 2],
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
            if self.tmp_npad_type == npad_type {
                return;
            }
            self.tmp_npad_type = npad_type;
        } else {
            if self.npad_type == npad_type {
                return;
            }
            if self.event_context.is_connected.load(Ordering::Relaxed) {
                log::warn!(
                    "Controller {:?} type changed while it is connected",
                    self.npad_id_type
                );
            }
            self.npad_type = npad_type;
        }
        // `set_button` and `set_trigger` need the type to apply upstream's
        // GameCube special cases without reaching back into the controller.
        self.status.lock().npad_type = npad_type;
        drop(_lock);
        self.trigger_on_change(ControllerTriggerType::Type, !self.is_configuring);
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
        *self.event_context.supported_style_tag.lock() = supported_styles;
        if !self.is_connected(false) {
            return;
        }

        // Attempt to reconnect with the originally configured type first.
        if self.npad_type != self.original_npad_type {
            self.disconnect();
            let current_npad_type = self.npad_type;
            self.set_npad_style_index(self.original_npad_type);
            if self.is_controller_supported(false) {
                self.connect(false);
                return;
            }
            self.set_npad_style_index(current_npad_type);
            self.connect(false);
        }

        if self.is_controller_supported(false) {
            return;
        }

        self.disconnect();

        if self.is_controller_fullkey(false) && supported_styles.raw.contains(NpadStyleSet::FULLKEY)
        {
            log::warn!(
                "Reconnecting controller type {:?} as Pro controller",
                self.npad_type
            );
            self.set_npad_style_index(NpadStyleIndex::Fullkey);
            self.connect(false);
            return;
        }

        if self.npad_type == NpadStyleIndex::JoyconDual
            && supported_styles.raw.contains(NpadStyleSet::FULLKEY)
        {
            log::warn!(
                "Reconnecting controller type {:?} as Pro controller",
                self.npad_type
            );
            self.set_npad_style_index(NpadStyleIndex::Fullkey);
            self.connect(false);
            return;
        }

        if self.npad_type == NpadStyleIndex::Fullkey
            && supported_styles.raw.contains(NpadStyleSet::JOY_DUAL)
        {
            log::warn!(
                "Reconnecting controller type {:?} as Dual Joycons",
                self.npad_type
            );
            self.set_npad_style_index(NpadStyleIndex::JoyconDual);
            self.connect(false);
            return;
        }

        log::error!(
            "Controller type {:?} is not supported. Disconnecting controller",
            self.npad_type
        );
    }

    pub fn connect(&mut self, use_temporary_value: bool) {
        if !self.is_controller_supported(use_temporary_value) {
            let npad_type = if self.is_configuring && use_temporary_value {
                self.tmp_npad_type
            } else {
                self.npad_type
            };
            log::error!("Controller type {:?} is not supported", npad_type);
            return;
        }

        let _lock = self.mutex.lock();
        if self.is_configuring {
            if self.tmp_is_connected {
                return;
            }
            self.tmp_is_connected = true;
            drop(_lock);
            self.trigger_on_change(ControllerTriggerType::Connected, false);
            return;
        }
        if self
            .event_context
            .is_connected
            .swap(true, Ordering::Relaxed)
        {
            return;
        }
        drop(_lock);
        self.trigger_on_change(ControllerTriggerType::Connected, true);
    }

    pub fn disconnect(&mut self) {
        let _lock = self.mutex.lock();
        if self.is_configuring {
            if !self.tmp_is_connected {
                return;
            }
            self.tmp_is_connected = false;
            drop(_lock);
            self.trigger_on_change(ControllerTriggerType::Disconnected, false);
            return;
        }
        if !self
            .event_context
            .is_connected
            .swap(false, Ordering::Relaxed)
        {
            return;
        }
        drop(_lock);
        self.trigger_on_change(ControllerTriggerType::Disconnected, true);
    }

    pub fn is_connected(&self, get_temporary_value: bool) -> bool {
        if get_temporary_value && self.is_configuring {
            self.tmp_is_connected
        } else {
            self.event_context.is_connected.load(Ordering::Relaxed)
        }
    }

    /// Port of EmulatedController::UnloadInput.
    ///
    /// Upstream resets every device `unique_ptr`, which unregisters that
    /// device's callback from the engine through its destructor. Dropping the
    /// vectors here does the same: each `InputFrom*` calls
    /// `InputEngine::delete_callback` in its `Drop`.
    pub fn unload_input(&mut self) {
        self.is_initialized = false;
        self.button_devices.clear();
        self.stick_devices.clear();
        self.trigger_devices.clear();
        self.output_devices.clear();
    }

    pub fn enable_configuration(&mut self) {
        self.is_configuring = true;
        self.tmp_is_connected = self.event_context.is_connected.load(Ordering::Relaxed);
        self.tmp_npad_type = self.npad_type;
        let mut status = self.status.lock();
        status.is_configuring = true;
        status.npad_type = self.tmp_npad_type;
    }

    pub fn disable_configuration(&mut self) {
        self.is_configuring = false;
        self.status.lock().is_configuring = false;

        // The physical-color devices are not part of the currently ported
        // device set. The remaining ordering follows upstream: apply type
        // first, then the temporary connection state.
        if self.tmp_npad_type != self.npad_type {
            if self.is_connected(false) {
                self.disconnect();
            }
            self.set_npad_style_index(self.tmp_npad_type);
            self.original_npad_type = self.tmp_npad_type;
        }

        if self.tmp_is_connected != self.is_connected(false) {
            if self.tmp_is_connected {
                self.connect(false);
                return;
            }
            self.disconnect();
        }
    }

    pub fn enable_system_buttons(&mut self) {
        self.system_buttons_enabled = true;
        self.status.lock().system_buttons_enabled = true;
    }

    pub fn disable_system_buttons(&mut self) {
        self.system_buttons_enabled = false;
        self.status.lock().system_buttons_enabled = false;
    }

    pub fn reset_system_buttons(&mut self) {
        let mut status = self.status.lock();
        status.home_button_state = HomeButtonState::default();
        status.capture_button_state = CaptureButtonState::default();
    }

    pub fn is_configuring_mode(&self) -> bool {
        self.is_configuring
    }

    /// Port of EmulatedController::LoadDevices.
    ///
    /// Upstream derives trigger and output parameters from representative
    /// button mappings before building the corresponding devices.
    fn load_devices(&mut self) {
        // TODO(german77): Use more buttons to detect the correct device.
        let left_joycon =
            self.button_params[settings_input::native_button::Values::DRight as usize].clone();
        let right_joycon =
            self.button_params[settings_input::native_button::Values::A as usize].clone();

        // Triggers for GC controllers, upstream's `trigger_params` assignment.
        self.trigger_params[EmulatedDeviceIndex::LeftIndex as usize] =
            self.button_params[settings_input::native_button::Values::ZL as usize].clone();
        self.trigger_params[EmulatedDeviceIndex::RightIndex as usize] =
            self.button_params[settings_input::native_button::Values::ZR as usize].clone();

        self.output_params[DeviceIndex::Left as usize] = left_joycon;
        self.output_params[DeviceIndex::Right as usize] = right_joycon;
        for output in &mut self.output_params {
            output.set_int("output", 1);
        }

        self.button_devices = self
            .button_params
            .iter()
            .map(common::input::create_input_device)
            .collect();
        self.stick_devices = self
            .stick_params
            .iter()
            .map(common::input::create_input_device)
            .collect();
        self.trigger_devices = self
            .trigger_params
            .iter()
            .map(common::input::create_input_device)
            .collect();
        self.output_devices = self
            .output_params
            .iter()
            .map(common::input::create_output_device)
            .collect();
    }

    /// Port of EmulatedController::ReloadInput.
    ///
    /// Builds the devices, then gives each one a callback that folds its status
    /// into the shared status. Upstream calls `ForceUpdate()` on each device right
    /// after, so a device that already has a value reports it without waiting
    /// for the next change.
    pub fn reload_input(&mut self) {
        self.load_devices();

        for (index, device) in self.button_devices.iter_mut().enumerate() {
            let uuid = UUID::from_string(&self.button_params[index].get_str("guid", ""));
            let values = Arc::clone(&self.status);
            let event_context = Arc::clone(&self.event_context);
            device.set_callback(InputCallback {
                on_change: Some(Arc::new(move |callback| {
                    set_button(&values, &event_context, callback, index, uuid);
                })),
            });
            device.force_update();
        }

        for (index, device) in self.stick_devices.iter_mut().enumerate() {
            let uuid = UUID::from_string(&self.stick_params[index].get_str("guid", ""));
            let values = Arc::clone(&self.status);
            let event_context = Arc::clone(&self.event_context);
            device.set_callback(InputCallback {
                on_change: Some(Arc::new(move |callback| {
                    set_stick(&values, &event_context, callback, index, uuid);
                })),
            });
            device.force_update();
        }

        for (index, device) in self.trigger_devices.iter_mut().enumerate() {
            let uuid = UUID::from_string(&self.trigger_params[index].get_str("guid", ""));
            let values = Arc::clone(&self.status);
            let event_context = Arc::clone(&self.event_context);
            device.set_callback(InputCallback {
                on_change: Some(Arc::new(move |callback| {
                    set_trigger(&values, &event_context, callback, index, uuid);
                })),
            });
            device.force_update();
        }

        self.is_initialized = true;
    }

    /// Port of EmulatedController::ReloadFromSettings.
    pub fn reload_from_settings(&mut self) {
        let player_index = crate::hid_util::npad_id_type_to_index(self.npad_id_type);
        let (buttons, analogs, motions, ringcon_analog, controller_type, connected) = {
            let settings = common::settings::values();
            let player = &settings.players.get_value()[player_index];
            (
                player.buttons.clone(),
                player.analogs.clone(),
                player.motions.clone(),
                settings.ringcon_analogs.clone(),
                player.controller_type,
                player.connected,
            )
        };

        for (index, param) in buttons.iter().enumerate() {
            self.button_params[index] = ParamPackage::from_serialized(param);
        }
        for (index, param) in analogs.iter().enumerate() {
            self.stick_params[index] = ParamPackage::from_serialized(param);
        }
        for (index, param) in motions.iter().enumerate() {
            self.motion_params[index] = ParamPackage::from_serialized(param);
        }
        self.ring_params[0] = ParamPackage::from_serialized(&ringcon_analog);

        // Other or debug controllers are always a Pro Controller upstream.
        let npad_type = if self.npad_id_type == NpadIdType::Other {
            NpadStyleIndex::Fullkey
        } else {
            Self::map_settings_type_to_npad(controller_type)
        };
        self.set_npad_style_index(npad_type);
        self.original_npad_type = self.npad_type;

        self.disconnect();
        if connected {
            self.connect(false);
        }

        self.reload_input();
    }

    /// Port of EmulatedController::SetButtonParam.
    pub fn set_button_param(&mut self, index: usize, param: ParamPackage) {
        if index >= self.button_params.len() {
            return;
        }
        self.button_params[index] = param;
        self.reload_input();
    }

    /// Port of EmulatedController::SetStickParam.
    pub fn set_stick_param(&mut self, index: usize, param: ParamPackage) {
        if index >= self.stick_params.len() {
            return;
        }
        self.stick_params[index] = param;
        self.reload_input();
    }

    /// Port of EmulatedController::SetMotionParam.
    pub fn set_motion_param(&mut self, index: usize, param: ParamPackage) {
        if index >= self.motion_params.len() {
            return;
        }
        self.motion_params[index] = param;
        self.reload_input();
    }

    /// Port of EmulatedController::GetButtonParam.
    pub fn get_button_param(&self, index: usize) -> ParamPackage {
        self.button_params.get(index).cloned().unwrap_or_default()
    }

    /// Port of EmulatedController::GetStickParam.
    pub fn get_stick_param(&self, index: usize) -> ParamPackage {
        self.stick_params.get(index).cloned().unwrap_or_default()
    }

    /// Port of EmulatedController::GetMotionParam.
    pub fn get_motion_param(&self, index: usize) -> ParamPackage {
        self.motion_params.get(index).cloned().unwrap_or_default()
    }

    /// Load every parameter from one `PlayerInput` and reload the devices once.
    ///
    /// Divergence from upstream, and the reason is the configuration dialog:
    /// upstream's `ConfigureInputPlayer` edits the `EmulatedController` itself
    /// and calls `SetButtonParam` per change, so `ReloadFromSettings` only ever
    /// needs to read the global settings. This port's dialog edits a working
    /// copy of `PlayerInput` that is only written back on OK, so it needs a way
    /// to push that copy in without going through the globals. Setting the
    /// parameters one at a time through the `Set*Param` methods above would
    /// rebuild every device once per binding.
    pub fn reload_from_player(&mut self, player: &settings_input::PlayerInput) {
        for (index, param) in player.buttons.iter().enumerate() {
            self.button_params[index] = ParamPackage::from_serialized(param);
        }
        for (index, param) in player.analogs.iter().enumerate() {
            self.stick_params[index] = ParamPackage::from_serialized(param);
        }
        for (index, param) in player.motions.iter().enumerate() {
            self.motion_params[index] = ParamPackage::from_serialized(param);
        }
        self.reload_input();
    }

    /// Port of EmulatedController::GetButtonsValues.
    pub fn get_buttons_values(&self) -> Vec<ButtonStatus> {
        self.status.lock().button_values.clone()
    }

    /// Port of EmulatedController::GetSticksValues.
    pub fn get_sticks_values(&self) -> Vec<StickStatus> {
        self.status.lock().stick_values.clone()
    }

    /// Port of EmulatedController::GetTriggersValues.
    pub fn get_triggers_values(&self) -> Vec<TriggerStatus> {
        self.status.lock().trigger_values.clone()
    }

    /// Port of EmulatedController::SaveCurrentConfig.
    pub fn save_current_config(&self) {
        let player_index = crate::hid_util::npad_id_type_to_index(self.npad_id_type);
        let mut settings = common::settings::values_mut();
        let player = &mut settings.players.get_value_mut()[player_index];
        player.connected = self.is_connected(false);
        player.controller_type = Self::map_npad_to_settings_type(self.npad_type);
        for (destination, source) in player.buttons.iter_mut().zip(&self.button_params) {
            *destination = source.serialize();
        }
        for (destination, source) in player.analogs.iter_mut().zip(&self.stick_params) {
            *destination = source.serialize();
        }
        for (destination, source) in player.motions.iter_mut().zip(&self.motion_params) {
            *destination = source.serialize();
        }
        if self.npad_id_type == NpadIdType::Player1 {
            settings.ringcon_analogs = self.ring_params[0].serialize();
        }
    }

    /// Port of EmulatedController::RestoreConfig.
    pub fn restore_config(&mut self) {
        if !self.is_configuring {
            return;
        }
        self.reload_from_settings();
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
        is_controller_supported(npad, *self.event_context.supported_style_tag.lock())
    }

    /// Port of EmulatedController::GetHomeButtons.
    pub fn get_home_buttons(&self) -> HomeButtonState {
        let status = self.status.lock();
        if self.is_configuring {
            return HomeButtonState::default();
        }
        status.home_button_state
    }

    /// Port of EmulatedController::GetCaptureButtons.
    pub fn get_capture_buttons(&self) -> CaptureButtonState {
        let status = self.status.lock();
        if self.is_configuring {
            return CaptureButtonState::default();
        }
        status.capture_button_state
    }

    /// Port of EmulatedController::GetNpadButtons.
    pub fn get_npad_buttons(&self) -> NpadButtonState {
        let status = self.status.lock();
        if self.is_configuring {
            return NpadButtonState::default();
        }
        NpadButtonState {
            raw: status.npad_button_state.raw & self.get_turbo_button_mask(&status),
        }
    }

    /// Port of EmulatedController::GetDebugPadButtons.
    pub fn get_debug_pad_buttons(&self) -> DebugPadButton {
        let status = self.status.lock();
        if self.is_configuring {
            return DebugPadButton::default();
        }
        status.debug_pad_button_state
    }

    /// Port of EmulatedController::GetSticks.
    pub fn get_sticks(&self) -> AnalogSticks {
        let status = self.status.lock();
        if self.is_configuring {
            return AnalogSticks::default();
        }
        status.analog_stick_state
    }

    /// Port of EmulatedController::GetTriggers.
    pub fn get_triggers(&self) -> NpadGcTriggerState {
        let status = self.status.lock();
        if self.is_configuring {
            return NpadGcTriggerState::default();
        }
        status.gc_trigger_state
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
    pub fn set_vibration_simple(&mut self, should_vibrate: bool) -> bool {
        let mut vibration = DEFAULT_VIBRATION_VALUE;
        if should_vibrate {
            vibration.low_amplitude = 1.0;
            vibration.high_amplitude = 1.0;
        }
        self.set_vibration(DeviceIndex::Left, vibration)
    }

    /// Port of `EmulatedController::SetVibration(DeviceIndex, VibrationValue)`.
    pub fn set_vibration(&mut self, device_index: DeviceIndex, vibration: VibrationValue) -> bool {
        if !self.is_initialized {
            return false;
        }
        let index = match device_index {
            DeviceIndex::Left => DeviceIndex::Left as usize,
            DeviceIndex::Right => DeviceIndex::Right as usize,
            DeviceIndex::None | DeviceIndex::MaxDeviceIndex => return false,
        };
        if index >= self.output_devices.len() {
            return false;
        }

        // Skip duplicated vibrations.
        if self.last_vibration_value[index].is_equal(&vibration) {
            return *common::settings::values().vibration_enabled.get_value();
        }
        self.last_vibration_value[index] = vibration;

        let player_index = crate::hid_util::npad_id_type_to_index(self.npad_id_type);
        let (master_enabled, accurate, player_enabled, strength) = {
            let settings = common::settings::values();
            let player = &settings.players.get_value()[player_index];
            (
                *settings.vibration_enabled.get_value(),
                *settings.enable_accurate_vibrations.get_value(),
                player.vibration_enabled,
                player.vibration_strength as f32 / 100.0,
            )
        };
        if !master_enabled || !player_enabled {
            return false;
        }

        if !accurate {
            let now = Instant::now();
            if (vibration.low_amplitude != 0.0 || vibration.high_amplitude != 0.0)
                && self.last_vibration_timepoint[index]
                    .is_some_and(|last| now.duration_since(last).as_millis() < 15)
            {
                return false;
            }
            self.last_vibration_timepoint[index] = Some(now);
        }

        let status = vibration_status(vibration, strength);

        // Send vibrations to Android's input overlay first.
        if let Some(android) = self.output_devices.get_mut(4) {
            android.set_vibration(&status);
        }
        self.output_devices[index].set_vibration(&status) == DriverResult::Success
    }

    /// Port of `EmulatedController::IsVibrationEnabled`.
    pub fn is_vibration_enabled(&self, device_index: usize) -> bool {
        let player_index = crate::hid_util::npad_id_type_to_index(self.npad_id_type);
        let player_enabled =
            common::settings::values().players.get_value()[player_index].vibration_enabled;
        self.is_initialized
            && player_enabled
            && self
                .output_devices
                .get(device_index)
                .is_some_and(|device| device.is_vibration_enabled())
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
    fn get_turbo_button_mask(&self, status: &ControllerStatus) -> NpadButton {
        // Apply no mask when disabled
        if self.turbo_button_state < TURBO_BUTTON_DELAY {
            return NpadButton::ALL;
        }

        use settings_input::native_button::Values as NB;
        let mut turbo_buttons = NpadButton::empty();
        for (index, button) in status.button_values.iter().enumerate() {
            if !button.turbo {
                continue;
            }
            let flag = match index {
                i if i == NB::A as usize => NpadButton::A,
                i if i == NB::B as usize => NpadButton::B,
                i if i == NB::X as usize => NpadButton::X,
                i if i == NB::Y as usize => NpadButton::Y,
                i if i == NB::L as usize => NpadButton::L,
                i if i == NB::R as usize => NpadButton::R,
                i if i == NB::ZL as usize => NpadButton::ZL,
                i if i == NB::ZR as usize => NpadButton::ZR,
                i if i == NB::DLeft as usize => NpadButton::LEFT,
                i if i == NB::DUp as usize => NpadButton::UP,
                i if i == NB::DRight as usize => NpadButton::RIGHT,
                i if i == NB::DDown as usize => NpadButton::DOWN,
                i if i == NB::SLLeft as usize => NpadButton::LEFT_SL,
                i if i == NB::SLRight as usize => NpadButton::RIGHT_SL,
                i if i == NB::SRLeft as usize => NpadButton::LEFT_SR,
                i if i == NB::SRRight as usize => NpadButton::RIGHT_SR,
                _ => continue,
            };
            turbo_buttons.insert(flag);
        }
        NpadButton::from_bits_truncate(!turbo_buttons.bits())
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
        let key = self.last_callback_key;
        self.event_context
            .callback_list
            .lock()
            .insert(key, update_callback);
        self.last_callback_key += 1;
        key
    }

    pub fn delete_callback(&mut self, key: i32) {
        if self
            .event_context
            .callback_list
            .lock()
            .remove(&key)
            .is_none()
        {
            log::error!("Tried to delete non-existent callback {}", key);
        }
    }

    fn trigger_on_change(&self, trigger_type: ControllerTriggerType, is_service_update: bool) {
        trigger_on_change(&self.event_context, trigger_type, is_service_update);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::input::{AnalogStatus, InputType};

    fn event_context(npad_id_type: NpadIdType) -> Arc<ControllerEventContext> {
        Arc::new(ControllerEventContext {
            npad_id_type,
            is_connected: AtomicBool::new(false),
            supported_style_tag: Mutex::new(NpadStyleTag {
                raw: NpadStyleSet::ALL,
            }),
            callback_list: Mutex::new(HashMap::new()),
        })
    }

    fn button_callback(pressed: bool) -> CallbackStatus {
        CallbackStatus {
            input_type: InputType::Button,
            button_status: ButtonStatus {
                value: pressed,
                ..Default::default()
            },
            ..Default::default()
        }
    }

    fn stick_callback(x: f32, y: f32) -> CallbackStatus {
        CallbackStatus {
            input_type: InputType::Stick,
            stick_status: StickStatus {
                x: AnalogStatus {
                    raw_value: x,
                    ..Default::default()
                },
                y: AnalogStatus {
                    raw_value: y,
                    ..Default::default()
                },
                ..Default::default()
            },
            ..Default::default()
        }
    }

    /// A press has to reach `npad_button_state`, not just the raw values the
    /// configuration preview reads. Folding it only into `button_values` left
    /// the pad working in the dialog and dead in the game.
    #[test]
    fn a_press_reaches_the_state_the_guest_reads() {
        use settings_input::native_button::Values as NB;
        let status = Arc::new(Mutex::new(ControllerStatus::new()));
        let events = event_context(NpadIdType::Player1);
        let uuid = UUID::new();

        set_button(
            &status,
            &events,
            &button_callback(true),
            NB::A as usize,
            uuid,
        );
        {
            let status = status.lock();
            assert!(status.button_values[NB::A as usize].value);
            assert!(status.npad_button_state.raw.contains(NpadButton::A));
            assert_eq!(status.debug_pad_button_state.raw & 1, 1);
        }

        set_button(
            &status,
            &events,
            &button_callback(false),
            NB::A as usize,
            uuid,
        );
        let status = status.lock();
        assert!(!status.button_values[NB::A as usize].value);
        assert!(!status.npad_button_state.raw.contains(NpadButton::A));
    }

    #[test]
    fn debug_pad_uses_the_upstream_bit_positions() {
        use settings_input::native_button::Values as NB;

        let mappings = [
            (NB::A, 0),
            (NB::B, 1),
            (NB::X, 2),
            (NB::Y, 3),
            (NB::L, 4),
            (NB::R, 5),
            (NB::ZL, 6),
            (NB::ZR, 7),
            (NB::Plus, 8),
            (NB::Minus, 9),
            (NB::DLeft, 10),
            (NB::DUp, 11),
            (NB::DRight, 12),
            (NB::DDown, 13),
        ];
        let status = Arc::new(Mutex::new(ControllerStatus::new()));
        let events = event_context(NpadIdType::Other);
        let uuid = UUID::new();

        for (button, bit) in mappings {
            set_button(
                &status,
                &events,
                &button_callback(true),
                button as usize,
                uuid,
            );
            assert_eq!(status.lock().debug_pad_button_state.raw, 1 << bit);
            set_button(
                &status,
                &events,
                &button_callback(false),
                button as usize,
                uuid,
            );
            assert_eq!(status.lock().debug_pad_button_state.raw, 0);
        }
    }

    #[test]
    fn player_one_button_auto_connects_and_notifies_callbacks() {
        use settings_input::native_button::Values as NB;

        let status = Arc::new(Mutex::new(ControllerStatus::new()));
        status.lock().npad_type = NpadStyleIndex::Fullkey;
        let events = event_context(NpadIdType::Player1);
        let observed = Arc::new(Mutex::new(Vec::new()));
        events.callback_list.lock().insert(
            0,
            ControllerUpdateCallback {
                on_change: Arc::new({
                    let observed = Arc::clone(&observed);
                    move |event| observed.lock().push(event)
                }),
                is_npad_service: true,
            },
        );

        set_button(
            &status,
            &events,
            &button_callback(true),
            NB::A as usize,
            UUID::new(),
        );

        assert!(events.is_connected.load(Ordering::Relaxed));
        assert_eq!(
            *observed.lock(),
            vec![
                ControllerTriggerType::Connected,
                ControllerTriggerType::Button
            ]
        );
    }

    /// Home and Capture are gated on `system_buttons_enabled` upstream.
    #[test]
    fn the_system_buttons_can_be_gated_off() {
        use settings_input::native_button::Values as NB;
        let status = Arc::new(Mutex::new(ControllerStatus::new()));
        let events = event_context(NpadIdType::Player1);
        status.lock().system_buttons_enabled = false;
        set_button(
            &status,
            &events,
            &button_callback(true),
            NB::Home as usize,
            UUID::new(),
        );
        assert_eq!(status.lock().home_button_state.raw, 0);

        // Release first: upstream only folds a value into the HID state when the
        // raw value actually transitions, so pressing an already-pressed button
        // is a no-op.
        set_button(
            &status,
            &events,
            &button_callback(false),
            NB::Home as usize,
            UUID::new(),
        );
        status.lock().system_buttons_enabled = true;
        set_button(
            &status,
            &events,
            &button_callback(true),
            NB::Home as usize,
            UUID::new(),
        );
        assert_eq!(status.lock().home_button_state.raw, 1);
    }

    /// A GameCube pad reports ZL and ZR through its analog triggers, so the
    /// digital bindings must not also set the buttons.
    #[test]
    fn a_gamecube_pad_ignores_the_digital_z_buttons() {
        use settings_input::native_button::Values as NB;
        let status = Arc::new(Mutex::new(ControllerStatus::new()));
        let events = event_context(NpadIdType::Player1);
        status.lock().npad_type = NpadStyleIndex::GameCube;

        set_button(
            &status,
            &events,
            &button_callback(true),
            NB::ZL as usize,
            UUID::new(),
        );
        let status = status.lock();
        // The raw value is still recorded — the preview draws it — but the
        // guest-facing state is left to `set_trigger`.
        assert!(status.button_values[NB::ZL as usize].value);
        assert!(!status.npad_button_state.raw.contains(NpadButton::ZL));
    }

    /// A stick has to land in `analog_stick_state`, scaled to the HID range.
    #[test]
    fn a_stick_reaches_the_state_the_guest_reads() {
        use settings_input::native_analog::Values as NA;
        let status = Arc::new(Mutex::new(ControllerStatus::new()));
        let events = event_context(NpadIdType::Player1);

        set_stick(
            &status,
            &events,
            &stick_callback(1.0, 0.0),
            NA::LStick as usize,
            UUID::new(),
        );
        let status = status.lock();
        assert_eq!(status.analog_stick_state.left.x, HID_JOYSTICK_MAX);
        assert_eq!(status.analog_stick_state.left.y, 0);
        assert!(status
            .npad_button_state
            .raw
            .contains(NpadButton::STICK_L_RIGHT));
        // The other stick is untouched.
        assert_eq!(status.analog_stick_state.right.x, 0);
    }

    /// While the configuration dialog is open upstream reports nothing to the
    /// HID services, so a mapping session cannot leak into a running game.
    #[test]
    fn configuring_mode_reports_nothing_to_the_guest() {
        use settings_input::native_button::Values as NB;
        let status = Arc::new(Mutex::new(ControllerStatus::new()));
        let events = event_context(NpadIdType::Player1);
        status.lock().is_configuring = true;

        set_button(
            &status,
            &events,
            &button_callback(true),
            NB::B as usize,
            UUID::new(),
        );
        let status = status.lock();
        assert!(status.button_values[NB::B as usize].value);
        assert!(status.npad_button_state.raw.is_empty());
    }

    #[test]
    fn configuration_applies_temporary_type_and_connection_in_upstream_order() {
        let mut controller = EmulatedController::new(NpadIdType::Player1);
        controller.set_npad_style_index(NpadStyleIndex::Fullkey);
        controller.original_npad_type = NpadStyleIndex::Fullkey;
        controller.connect(false);

        controller.enable_configuration();
        controller.set_npad_style_index(NpadStyleIndex::JoyconDual);
        controller.disconnect();

        // Configuration only changes the temporary values.
        assert_eq!(
            controller.get_npad_style_index(false),
            NpadStyleIndex::Fullkey
        );
        assert_eq!(
            controller.get_npad_style_index(true),
            NpadStyleIndex::JoyconDual
        );
        assert!(controller.is_connected(false));
        assert!(!controller.is_connected(true));

        controller.disable_configuration();
        assert_eq!(
            controller.get_npad_style_index(false),
            NpadStyleIndex::JoyconDual
        );
        assert_eq!(controller.original_npad_type, NpadStyleIndex::JoyconDual);
        assert!(!controller.is_connected(false));
    }

    #[test]
    fn changing_type_during_configuration_preserves_a_connected_controller() {
        let mut controller = EmulatedController::new(NpadIdType::Player1);
        controller.set_npad_style_index(NpadStyleIndex::Fullkey);
        controller.original_npad_type = NpadStyleIndex::Fullkey;
        controller.connect(false);

        controller.enable_configuration();
        controller.set_npad_style_index(NpadStyleIndex::JoyconDual);
        controller.disable_configuration();

        assert_eq!(
            controller.get_npad_style_index(false),
            NpadStyleIndex::JoyconDual
        );
        assert!(controller.is_connected(false));
    }

    #[test]
    fn supported_style_change_uses_upstream_fullkey_fallbacks() {
        let mut controller = EmulatedController::new(NpadIdType::Player1);
        controller.set_npad_style_index(NpadStyleIndex::GameCube);
        controller.original_npad_type = NpadStyleIndex::GameCube;
        controller.connect(false);

        controller.set_supported_npad_style_tag(NpadStyleTag {
            raw: NpadStyleSet::FULLKEY,
        });

        assert_eq!(
            controller.get_npad_style_index(false),
            NpadStyleIndex::Fullkey
        );
        assert!(controller.is_connected(false));
    }

    #[test]
    fn pokeball_is_not_a_fullkey_controller() {
        let mut controller = EmulatedController::new(NpadIdType::Player1);
        controller.set_npad_style_index(NpadStyleIndex::Pokeball);
        controller.original_npad_type = NpadStyleIndex::Pokeball;
        controller.connect(false);

        controller.set_supported_npad_style_tag(NpadStyleTag {
            raw: NpadStyleSet::FULLKEY,
        });

        assert_eq!(
            controller.get_npad_style_index(false),
            NpadStyleIndex::Pokeball
        );
        assert!(!controller.is_connected(false));
    }

    /// `unload_input` has to drop the devices, or their engine callbacks keep
    /// firing into a controller nothing is reading any more.
    #[test]
    fn unloading_releases_every_device() {
        let mut controller = EmulatedController::new(NpadIdType::Player1);
        controller.reload_input();
        assert!(!controller.button_devices.is_empty());
        assert_eq!(controller.output_devices.len(), OUTPUT_DEVICES_SIZE);

        controller.unload_input();
        assert!(controller.button_devices.is_empty());
        assert!(controller.stick_devices.is_empty());
        assert!(controller.trigger_devices.is_empty());
        assert!(controller.output_devices.is_empty());
    }

    #[test]
    fn load_devices_derives_vibration_outputs_from_upstream_buttons() {
        let mut controller = EmulatedController::new(NpadIdType::Player1);
        controller.button_params[settings_input::native_button::Values::DRight as usize] =
            ParamPackage::from_serialized("engine:null,pad:7");
        controller.button_params[settings_input::native_button::Values::A as usize] =
            ParamPackage::from_serialized("engine:null,pad:9");

        controller.load_devices();

        assert_eq!(controller.output_params.len(), OUTPUT_DEVICES_SIZE);
        assert_eq!(
            controller.output_params[DeviceIndex::Left as usize].get_int("pad", 0),
            7
        );
        assert_eq!(
            controller.output_params[DeviceIndex::Right as usize].get_int("pad", 0),
            9
        );
        assert!(controller
            .output_params
            .iter()
            .all(|param| param.get_int("output", 0) == 1));
    }

    #[test]
    fn save_and_restore_config_use_the_controller_owned_parameters() {
        let player_index = crate::hid_util::npad_id_type_to_index(NpadIdType::Player8);
        let original_player = common::settings::values().players.get_value()[player_index].clone();

        let mut controller = EmulatedController::new(NpadIdType::Player8);
        controller.npad_type = NpadStyleIndex::GameCube;
        controller
            .event_context
            .is_connected
            .store(true, Ordering::Relaxed);
        controller.button_params[0] = ParamPackage::from_serialized("engine:save_test,button:4");
        controller.stick_params[0] = ParamPackage::from_serialized("engine:save_test,axis_x:2");
        controller.motion_params[0] = ParamPackage::from_serialized("engine:save_test,motion:1");

        controller.save_current_config();

        {
            let settings = common::settings::values();
            let saved = &settings.players.get_value()[player_index];
            assert!(saved.connected);
            assert_eq!(saved.controller_type, ControllerType::GameCube);
            assert_eq!(
                ParamPackage::from_serialized(&saved.buttons[0]).get_str("engine", ""),
                "save_test"
            );
            assert_eq!(
                ParamPackage::from_serialized(&saved.analogs[0]).get_int("axis_x", -1),
                2
            );
            assert_eq!(
                ParamPackage::from_serialized(&saved.motions[0]).get_int("motion", -1),
                1
            );
        }

        controller.enable_configuration();
        controller.set_npad_style_index(NpadStyleIndex::JoyconDual);
        controller.disconnect();
        controller.button_params[0] = ParamPackage::from_serialized("engine:discarded");
        controller.restore_config();

        assert_eq!(
            controller.get_npad_style_index(true),
            NpadStyleIndex::GameCube
        );
        assert!(controller.is_connected(true));
        assert_eq!(
            controller.get_button_param(0).get_str("engine", ""),
            "save_test"
        );
        controller.disable_configuration();
        controller.unload_input();

        common::settings::values_mut().players.get_value_mut()[player_index] = original_player;
    }

    #[test]
    fn vibration_strength_uses_upstream_curve_and_amplitude_cap() {
        let strong = vibration_status(
            VibrationValue {
                low_amplitude: 0.75,
                low_frequency: 160.0,
                high_amplitude: 1.0,
                high_frequency: 320.0,
            },
            1.5,
        );
        assert_eq!(strong.low_amplitude, 1.0);
        assert_eq!(strong.high_amplitude, 1.0);
        assert_eq!(
            strong.amplification_type,
            VibrationAmplificationType::Exponential
        );

        let weak = vibration_status(DEFAULT_VIBRATION_VALUE, 0.7);
        assert_eq!(weak.amplification_type, VibrationAmplificationType::Linear);
    }

    use super::{apply_simple_npad_stick_buttons, parse_u64_auto, AnalogSticks};

    #[test]
    fn scripted_npad_parser_uses_decimal_unless_prefixed_hex() {
        assert_eq!(parse_u64_auto("1000"), Some(1000));
        assert_eq!(parse_u64_auto("0x1000"), Some(0x1000));
        assert_eq!(parse_u64_auto("0X4C0"), Some(0x4C0));
        assert_eq!(parse_u64_auto("not-a-number"), None);
    }

    #[test]
    fn scripted_stick_direction_bits_also_drive_analog_coordinates() {
        let mut sticks = AnalogSticks::default();
        apply_simple_npad_stick_buttons(
            &mut sticks,
            NpadButton::STICK_L_DOWN | NpadButton::STICK_R_LEFT,
        );

        assert_eq!(sticks.left.x, 0);
        assert_eq!(sticks.left.y, -HID_JOYSTICK_MAX);
        assert_eq!(sticks.right.x, -HID_JOYSTICK_MAX);
        assert_eq!(sticks.right.y, 0);
    }
}
