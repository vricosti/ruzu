// SPDX-FileCopyrightText: 2014 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/gc_adapter.h` and `input_common/drivers/gc_adapter.cpp`.
//!
//! GameCube controller adapter driver using libusb.

use common::input::{ButtonNames, DriverResult, VibrationStatus};
use common::param_package::ParamPackage;

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
        _identifier: &PadIdentifier,
        _vibration: &VibrationStatus,
    ) -> DriverResult {
        todo!()
    }

    /// Port of GCAdapter::IsVibrationEnabled (override)
    pub fn is_vibration_enabled(&self, _identifier: &PadIdentifier) -> bool {
        todo!()
    }

    /// Port of GCAdapter::GetInputDevices (override)
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        todo!()
    }

    /// Port of GCAdapter::GetButtonMappingForDevice (override)
    pub fn get_button_mapping_for_device(&self, _params: &ParamPackage) -> ButtonMapping {
        todo!()
    }

    /// Port of GCAdapter::GetAnalogMappingForDevice (override)
    pub fn get_analog_mapping_for_device(&self, _params: &ParamPackage) -> AnalogMapping {
        todo!()
    }

    /// Port of GCAdapter::GetUIName (override)
    pub fn get_ui_name(&self, _params: &ParamPackage) -> ButtonNames {
        todo!()
    }

    /// Port of GCAdapter::IsStickInverted (override)
    pub fn is_stick_inverted(&self, _params: &ParamPackage) -> bool {
        todo!()
    }

    // ---- Private methods ----

    fn update_pad_type(&mut self, _port: usize, _pad_type: ControllerTypes) {
        todo!()
    }

    fn update_controllers(&mut self, _adapter_payload: &AdapterPayload) {
        todo!()
    }

    fn update_state_buttons(&mut self, _port: usize, _b1: u8, _b2: u8) {
        todo!()
    }

    fn update_state_axes(&mut self, _port: usize, _adapter_payload: &AdapterPayload) {
        todo!()
    }

    fn adapter_input_thread(&mut self) {
        todo!()
    }

    fn adapter_scan_thread(&mut self) {
        todo!()
    }

    fn is_payload_correct(&self, _adapter_payload: &AdapterPayload, _payload_size: i32) -> bool {
        todo!()
    }

    fn setup(&mut self) -> bool {
        todo!()
    }

    fn check_device_access(&mut self) -> bool {
        todo!()
    }

    fn device_connected(&self, _port: usize) -> bool {
        todo!()
    }

    fn reset(&mut self) {
        todo!()
    }

    fn update_vibrations(&mut self) {
        todo!()
    }

    fn send_vibrations(&mut self) {
        todo!()
    }

    fn get_ui_button_name(&self, _params: &ParamPackage) -> ButtonNames {
        todo!()
    }
}
