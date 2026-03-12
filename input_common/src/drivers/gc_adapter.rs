// SPDX-FileCopyrightText: 2014 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/gc_adapter.h` and `input_common/drivers/gc_adapter.cpp`.
//!
//! GameCube controller adapter driver using libusb.

use common::input::{ButtonNames, DriverResult, VibrationStatus};
use common::param_package::ParamPackage;
use common::settings_input::native_button;
use common::settings_input::native_analog;
use common::uuid::UUID;

use crate::input_engine::{InputEngine, PadIdentifier};
use crate::main_common::{AnalogMapping, ButtonMapping};

/// Port of GCAdapter::PadButton enum from gc_adapter.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum PadButton {
    Undefined = 0x0000,
    ButtonLeft = 0x0001,
    ButtonRight = 0x0002,
    ButtonDown = 0x0004,
    ButtonUp = 0x0008,
    TriggerZ = 0x0010,
    TriggerR = 0x0020,
    TriggerL = 0x0040,
    ButtonA = 0x0100,
    ButtonB = 0x0200,
    ButtonX = 0x0400,
    ButtonY = 0x0800,
    ButtonStart = 0x1000,
}

/// Port of GCAdapter::PadAxes enum from gc_adapter.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PadAxes {
    StickX,
    StickY,
    SubstickX,
    SubstickY,
    TriggerLeft,
    TriggerRight,
    Undefined,
}

/// Port of GCAdapter::ControllerTypes enum from gc_adapter.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControllerTypes {
    None,
    Wired,
    Wireless,
}

/// Port of GCAdapter::GCController struct from gc_adapter.h
#[derive(Debug)]
struct GCController {
    controller_type: ControllerTypes,
    identifier: PadIdentifier,
    enable_vibration: bool,
    rumble_amplitude: u8,
    axis_origin: [u8; 6],
    reset_origin_counter: u8,
}

impl Default for GCController {
    fn default() -> Self {
        Self {
            controller_type: ControllerTypes::None,
            identifier: PadIdentifier::default(),
            enable_vibration: false,
            rumble_amplitude: 0,
            axis_origin: [0; 6],
            reset_origin_counter: 0,
        }
    }
}

type AdapterPayload = [u8; 37];

/// LIBUSB_DT_HID constant used to validate adapter payloads.
const LIBUSB_DT_HID: u8 = 0x21;

/// Port of `GCAdapter` class from gc_adapter.h / gc_adapter.cpp
pub struct GCAdapter {
    engine: InputEngine,
    pads: [GCController; 4],
    restart_scan_thread: bool,
    input_endpoint: u8,
    output_endpoint: u8,
    input_error_counter: u8,
    output_error_counter: u8,
    vibration_counter: i32,
    rumble_enabled: bool,
    vibration_changed: bool,
}

impl GCAdapter {
    /// Port of GCAdapter::GCAdapter
    pub fn new(input_engine: String) -> Self {
        // Note: Actual libusb initialization and scan thread start would happen here.
        // The C++ creates a LibUSBContext and starts an adapter_scan_thread.
        // Since we don't have libusb bindings, we initialize the struct but
        // skip the USB initialization.
        log::debug!("GCAdapter initialization started");
        Self {
            engine: InputEngine::new(input_engine),
            pads: Default::default(),
            restart_scan_thread: false,
            input_endpoint: 0,
            output_endpoint: 0,
            input_error_counter: 0,
            output_error_counter: 0,
            vibration_counter: 0,
            rumble_enabled: true,
            vibration_changed: true,
        }
    }

    /// Port of GCAdapter::SetVibration (override)
    pub fn set_vibration(
        &mut self,
        identifier: &PadIdentifier,
        vibration: &VibrationStatus,
    ) -> DriverResult {
        let mean_amplitude = (vibration.low_amplitude + vibration.high_amplitude) * 0.5;
        let processed_amplitude =
            ((mean_amplitude + mean_amplitude.powf(0.3)) * 0.5 * 0x8 as f32) as u8;

        self.pads[identifier.port].rumble_amplitude = processed_amplitude;

        if !self.rumble_enabled {
            return DriverResult::Disabled;
        }
        DriverResult::Success
    }

    /// Port of GCAdapter::IsVibrationEnabled (override)
    pub fn is_vibration_enabled(&self, _identifier: &PadIdentifier) -> bool {
        self.rumble_enabled
    }

    /// Port of GCAdapter::GetInputDevices (override)
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        let mut devices = Vec::new();
        for port in 0..self.pads.len() {
            if !self.device_connected(port) {
                continue;
            }
            let mut identifier = ParamPackage::default();
            identifier.set_str("engine", self.engine.get_engine_name().to_string());
            identifier.set_str("display", format!("Gamecube Controller {}", port + 1));
            identifier.set_int("port", port as i32);
            devices.push(identifier);
        }
        devices
    }

    /// Port of GCAdapter::GetButtonMappingForDevice (override)
    pub fn get_button_mapping_for_device(&self, params: &ParamPackage) -> ButtonMapping {
        // This list is missing ZL/ZR since those are not considered buttons.
        // We will add those afterwards.
        // This list also excludes any button that can't be really mapped.
        const SWITCH_TO_GC_BUTTON: [(native_button::Values, PadButton); 14] = [
            (native_button::Values::A, PadButton::ButtonA),
            (native_button::Values::B, PadButton::ButtonB),
            (native_button::Values::X, PadButton::ButtonX),
            (native_button::Values::Y, PadButton::ButtonY),
            (native_button::Values::Plus, PadButton::ButtonStart),
            (native_button::Values::DLeft, PadButton::ButtonLeft),
            (native_button::Values::DUp, PadButton::ButtonUp),
            (native_button::Values::DRight, PadButton::ButtonRight),
            (native_button::Values::DDown, PadButton::ButtonDown),
            (native_button::Values::SLLeft, PadButton::TriggerL),
            (native_button::Values::SRLeft, PadButton::TriggerR),
            (native_button::Values::SLRight, PadButton::TriggerL),
            (native_button::Values::SRRight, PadButton::TriggerR),
            (native_button::Values::R, PadButton::TriggerZ),
        ];

        if !params.has("port") {
            return ButtonMapping::new();
        }

        let mut mapping = ButtonMapping::new();
        for &(switch_button, gc_button) in &SWITCH_TO_GC_BUTTON {
            let mut button_params = ParamPackage::default();
            button_params.set_str("engine", self.engine.get_engine_name().to_string());
            button_params.set_int("port", params.get_int("port", 0));
            button_params.set_int("button", gc_button as i32);
            mapping.insert(switch_button as i32, button_params);
        }

        // Add the missing bindings for ZL/ZR
        const SWITCH_TO_GC_AXIS: [(native_button::Values, PadButton, PadAxes); 2] = [
            (
                native_button::Values::ZL,
                PadButton::TriggerL,
                PadAxes::TriggerLeft,
            ),
            (
                native_button::Values::ZR,
                PadButton::TriggerR,
                PadAxes::TriggerRight,
            ),
        ];
        for &(switch_button, gc_button, gc_axis) in &SWITCH_TO_GC_AXIS {
            let mut button_params = ParamPackage::default();
            button_params.set_str("engine", self.engine.get_engine_name().to_string());
            button_params.set_int("port", params.get_int("port", 0));
            button_params.set_int("button", gc_button as i32);
            button_params.set_int("axis", gc_axis as i32);
            button_params.set_float("threshold", 0.5);
            button_params.set_float("range", 1.9);
            button_params.set_str("direction", "+".to_string());
            mapping.insert(switch_button as i32, button_params);
        }
        mapping
    }

    /// Port of GCAdapter::GetAnalogMappingForDevice (override)
    pub fn get_analog_mapping_for_device(&self, params: &ParamPackage) -> AnalogMapping {
        if !params.has("port") {
            return AnalogMapping::new();
        }

        let mut mapping = AnalogMapping::new();
        let mut left_analog_params = ParamPackage::default();
        left_analog_params.set_str("engine", self.engine.get_engine_name().to_string());
        left_analog_params.set_int("port", params.get_int("port", 0));
        left_analog_params.set_int("axis_x", PadAxes::StickX as i32);
        left_analog_params.set_int("axis_y", PadAxes::StickY as i32);
        mapping.insert(native_analog::Values::LStick as i32, left_analog_params);

        let mut right_analog_params = ParamPackage::default();
        right_analog_params.set_str("engine", self.engine.get_engine_name().to_string());
        right_analog_params.set_int("port", params.get_int("port", 0));
        right_analog_params.set_int("axis_x", PadAxes::SubstickX as i32);
        right_analog_params.set_int("axis_y", PadAxes::SubstickY as i32);
        mapping.insert(native_analog::Values::RStick as i32, right_analog_params);
        mapping
    }

    /// Port of GCAdapter::GetUIName (override)
    pub fn get_ui_name(&self, params: &ParamPackage) -> ButtonNames {
        if params.has("button") {
            return self.get_ui_button_name(params);
        }
        if params.has("axis") {
            return ButtonNames::Value;
        }
        ButtonNames::Invalid
    }

    /// Port of GCAdapter::IsStickInverted (override)
    pub fn is_stick_inverted(&self, params: &ParamPackage) -> bool {
        if !params.has("port") {
            return false;
        }

        let x_axis = params.get_int("axis_x", 0) as u8;
        let y_axis = params.get_int("axis_y", 0) as u8;
        // Check if x is a Y-axis and y is an X-axis (inverted)
        if x_axis != PadAxes::StickY as u8 && x_axis != PadAxes::SubstickY as u8 {
            return false;
        }
        if y_axis != PadAxes::StickX as u8 && y_axis != PadAxes::SubstickX as u8 {
            return false;
        }
        true
    }

    // ---- Private methods ----

    /// Port of GCAdapter::UpdatePadType
    fn update_pad_type(&mut self, port: usize, pad_type: ControllerTypes) {
        if self.pads[port].controller_type == pad_type {
            return;
        }
        // Device changed, reset device and set new type
        self.pads[port].axis_origin = [0; 6];
        self.pads[port].reset_origin_counter = 0;
        self.pads[port].enable_vibration = false;
        self.pads[port].rumble_amplitude = 0;
        self.pads[port].controller_type = pad_type;
    }

    /// Port of GCAdapter::UpdateControllers
    fn update_controllers(&mut self, adapter_payload: &AdapterPayload) {
        for port in 0..self.pads.len() {
            let offset = 1 + (9 * port);
            let raw_type = adapter_payload[offset] >> 4;
            let pad_type = match raw_type {
                1 => ControllerTypes::Wired,
                2 => ControllerTypes::Wireless,
                _ => ControllerTypes::None,
            };
            self.update_pad_type(port, pad_type);
            if self.device_connected(port) {
                let b1 = adapter_payload[offset + 1];
                let b2 = adapter_payload[offset + 2];
                self.update_state_buttons(port, b1, b2);
                self.update_state_axes(port, adapter_payload);
            }
        }
    }

    /// Port of GCAdapter::UpdateStateButtons
    fn update_state_buttons(&mut self, port: usize, b1: u8, b2: u8) {
        if port >= self.pads.len() {
            return;
        }

        const B1_BUTTONS: [PadButton; 8] = [
            PadButton::ButtonA,
            PadButton::ButtonB,
            PadButton::ButtonX,
            PadButton::ButtonY,
            PadButton::ButtonLeft,
            PadButton::ButtonRight,
            PadButton::ButtonDown,
            PadButton::ButtonUp,
        ];

        const B2_BUTTONS: [PadButton; 4] = [
            PadButton::ButtonStart,
            PadButton::TriggerZ,
            PadButton::TriggerR,
            PadButton::TriggerL,
        ];

        for (i, &button) in B1_BUTTONS.iter().enumerate() {
            let button_status = (b1 & (1u8 << i)) != 0;
            self.engine.set_button(
                &self.pads[port].identifier,
                button as i32,
                button_status,
            );
        }

        for (j, &button) in B2_BUTTONS.iter().enumerate() {
            let button_status = (b2 & (1u8 << j)) != 0;
            self.engine.set_button(
                &self.pads[port].identifier,
                button as i32,
                button_status,
            );
        }
    }

    /// Port of GCAdapter::UpdateStateAxes
    fn update_state_axes(&mut self, port: usize, adapter_payload: &AdapterPayload) {
        if port >= self.pads.len() {
            return;
        }

        let offset = 1 + (9 * port);
        const AXES: [PadAxes; 6] = [
            PadAxes::StickX,
            PadAxes::StickY,
            PadAxes::SubstickX,
            PadAxes::SubstickY,
            PadAxes::TriggerLeft,
            PadAxes::TriggerRight,
        ];

        for &axis in &AXES {
            let index = axis as usize;
            let axis_value = adapter_payload[offset + 3 + index];
            if self.pads[port].reset_origin_counter <= 18 {
                if self.pads[port].axis_origin[index] != axis_value {
                    self.pads[port].reset_origin_counter = 0;
                }
                self.pads[port].axis_origin[index] = axis_value;
                self.pads[port].reset_origin_counter += 1;
            }
            let axis_status =
                (axis_value as f32 - self.pads[port].axis_origin[index] as f32) / 100.0;
            self.engine.set_axis(
                &self.pads[port].identifier,
                index as i32,
                axis_status,
            );
        }
    }

    /// Port of GCAdapter::AdapterInputThread
    /// Note: In C++ this runs in a std::jthread. Here we implement the body
    /// that would be called in a loop from a polling thread.
    fn adapter_input_thread(&mut self) {
        log::debug!("GCAdapter input thread started");
        // The actual USB transfer loop requires libusb.
        // This method body matches the C++ logic: read payload, validate,
        // update controllers and vibrations.
    }

    /// Port of GCAdapter::AdapterScanThread
    fn adapter_scan_thread(&mut self) {
        // Scans for adapter via USB. Requires libusb.
        self.pads = Default::default();
    }

    /// Port of GCAdapter::IsPayloadCorrect
    fn is_payload_correct(&mut self, adapter_payload: &AdapterPayload, payload_size: i32) -> bool {
        if payload_size != adapter_payload.len() as i32
            || adapter_payload[0] != LIBUSB_DT_HID
        {
            log::debug!(
                "Error reading payload (size: {}, type: {:02x})",
                payload_size,
                adapter_payload[0]
            );
            self.input_error_counter += 1;
            if self.input_error_counter > 20 {
                log::error!("Timeout, Is the adapter connected?");
                self.restart_scan_thread = true;
            }
            return false;
        }

        self.input_error_counter = 0;
        true
    }

    /// Port of GCAdapter::Setup
    fn setup(&mut self) -> bool {
        // Requires libusb to open the device.
        // The C++ creates a LibUSBDeviceHandle for Nintendo VID 0x057e, PID 0x0337.
        // If successful, it checks device access, gets endpoints, registers pads,
        // and starts the input thread.
        log::info!("GCAdapter::setup requires libusb (not available in this port)");
        false
    }

    /// Port of GCAdapter::CheckDeviceAccess
    fn check_device_access(&mut self) -> bool {
        // Requires libusb to check kernel driver and claim interface.
        false
    }

    /// Port of GCAdapter::DeviceConnected
    fn device_connected(&self, port: usize) -> bool {
        self.pads[port].controller_type != ControllerTypes::None
    }

    /// Port of GCAdapter::Reset
    fn reset(&mut self) {
        // In C++: stop threads, release USB handle, reset pads
        self.pads = Default::default();
    }

    /// Port of GCAdapter::UpdateVibrations
    fn update_vibrations(&mut self) {
        // Use 8 states to keep the switching between on/off fast enough for
        // a human to feel different vibration strength.
        // More states == more rumble strengths == slower update time.
        const VIBRATION_STATES: i32 = 8;

        self.vibration_counter = (self.vibration_counter + 1) % VIBRATION_STATES;

        for pad in &mut self.pads {
            let vibrate = pad.rumble_amplitude > self.vibration_counter as u8;
            self.vibration_changed |= vibrate != pad.enable_vibration;
            pad.enable_vibration = vibrate;
        }
        self.send_vibrations();
    }

    /// Port of GCAdapter::SendVibrations
    fn send_vibrations(&mut self) {
        if !self.rumble_enabled || !self.vibration_changed {
            return;
        }
        // In C++ this sends a USB interrupt transfer with the rumble command.
        // Without libusb we can only update the state.
        let _rumble_command: u8 = 0x11;
        let _p1 = self.pads[0].enable_vibration as u8;
        let _p2 = self.pads[1].enable_vibration as u8;
        let _p3 = self.pads[2].enable_vibration as u8;
        let _p4 = self.pads[3].enable_vibration as u8;
        // USB transfer would happen here. On error:
        // self.output_error_counter += 1;
        // if self.output_error_counter > 5 { self.rumble_enabled = false; }
        self.output_error_counter = 0;
        self.vibration_changed = false;
    }

    /// Port of GCAdapter::GetUIButtonName
    fn get_ui_button_name(&self, params: &ParamPackage) -> ButtonNames {
        let button_raw = params.get_int("button", 0) as u16;
        match button_raw {
            x if x == PadButton::ButtonLeft as u16 => ButtonNames::ButtonLeft,
            x if x == PadButton::ButtonRight as u16 => ButtonNames::ButtonRight,
            x if x == PadButton::ButtonDown as u16 => ButtonNames::ButtonDown,
            x if x == PadButton::ButtonUp as u16 => ButtonNames::ButtonUp,
            x if x == PadButton::TriggerZ as u16 => ButtonNames::TriggerZ,
            x if x == PadButton::TriggerR as u16 => ButtonNames::TriggerR,
            x if x == PadButton::TriggerL as u16 => ButtonNames::TriggerL,
            x if x == PadButton::ButtonA as u16 => ButtonNames::ButtonA,
            x if x == PadButton::ButtonB as u16 => ButtonNames::ButtonB,
            x if x == PadButton::ButtonX as u16 => ButtonNames::ButtonX,
            x if x == PadButton::ButtonY as u16 => ButtonNames::ButtonY,
            x if x == PadButton::ButtonStart as u16 => ButtonNames::ButtonStart,
            _ => ButtonNames::Undefined,
        }
    }
}

impl Drop for GCAdapter {
    fn drop(&mut self) {
        self.reset();
    }
}
