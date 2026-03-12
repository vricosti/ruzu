// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/joycon.h` and `input_common/drivers/joycon.cpp`.
//!
//! Nintendo Joy-Con and Pro Controller driver via HID API.

use std::sync::Arc;

use common::input::{
    ButtonNames, CameraFormat, DriverResult, LedStatus, MifareRequest, NfcState, PollingMode,
    VibrationStatus,
};
use common::param_package::ParamPackage;

use crate::input_engine::{InputEngine, PadIdentifier};
use crate::main_common::{AnalogMapping, ButtonMapping, MotionMapping};

/// Maximum number of supported controllers.
const MAX_SUPPORTED_CONTROLLERS: usize = 8;

/// Port of `Joycons` class from joycon.h / joycon.cpp
pub struct Joycons {
    engine: InputEngine,
    // Joycon types are split by type to ease supporting dualjoycon configurations
    // In a full port these would hold JoyconDriver instances
}

impl Joycons {
    /// Port of Joycons::Joycons
    pub fn new(input_engine: String) -> Self {
        Self {
            engine: InputEngine::new(input_engine),
        }
    }

    /// Port of Joycons::IsVibrationEnabled (override)
    pub fn is_vibration_enabled(&self, _identifier: &PadIdentifier) -> bool {
        todo!()
    }

    /// Port of Joycons::SetVibration (override)
    pub fn set_vibration(
        &mut self,
        _identifier: &PadIdentifier,
        _vibration: &VibrationStatus,
    ) -> DriverResult {
        todo!()
    }

    /// Port of Joycons::SetLeds (override)
    pub fn set_leds(
        &mut self,
        _identifier: &PadIdentifier,
        _led_status: &LedStatus,
    ) -> DriverResult {
        todo!()
    }

    /// Port of Joycons::SetCameraFormat (override)
    pub fn set_camera_format(
        &mut self,
        _identifier: &PadIdentifier,
        _camera_format: CameraFormat,
    ) -> DriverResult {
        todo!()
    }

    /// Port of Joycons::SupportsNfc (override)
    pub fn supports_nfc(&self, _identifier: &PadIdentifier) -> NfcState {
        todo!()
    }

    /// Port of Joycons::StartNfcPolling (override)
    pub fn start_nfc_polling(&mut self, _identifier: &PadIdentifier) -> NfcState {
        todo!()
    }

    /// Port of Joycons::StopNfcPolling (override)
    pub fn stop_nfc_polling(&mut self, _identifier: &PadIdentifier) -> NfcState {
        todo!()
    }

    /// Port of Joycons::ReadAmiiboData (override)
    pub fn read_amiibo_data(
        &mut self,
        _identifier: &PadIdentifier,
        _out_data: &mut Vec<u8>,
    ) -> NfcState {
        todo!()
    }

    /// Port of Joycons::WriteNfcData (override)
    pub fn write_nfc_data(&mut self, _identifier: &PadIdentifier, _data: &[u8]) -> NfcState {
        todo!()
    }

    /// Port of Joycons::ReadMifareData (override)
    pub fn read_mifare_data(
        &mut self,
        _identifier: &PadIdentifier,
        _request: &MifareRequest,
        _out_data: &mut MifareRequest,
    ) -> NfcState {
        todo!()
    }

    /// Port of Joycons::WriteMifareData (override)
    pub fn write_mifare_data(
        &mut self,
        _identifier: &PadIdentifier,
        _request: &MifareRequest,
    ) -> NfcState {
        todo!()
    }

    /// Port of Joycons::SetPollingMode (override)
    pub fn set_polling_mode(
        &mut self,
        _identifier: &PadIdentifier,
        _polling_mode: PollingMode,
    ) -> DriverResult {
        todo!()
    }

    /// Port of Joycons::GetInputDevices (override)
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        todo!()
    }

    /// Port of Joycons::GetButtonMappingForDevice (override)
    pub fn get_button_mapping_for_device(&self, _params: &ParamPackage) -> ButtonMapping {
        todo!()
    }

    /// Port of Joycons::GetAnalogMappingForDevice (override)
    pub fn get_analog_mapping_for_device(&self, _params: &ParamPackage) -> AnalogMapping {
        todo!()
    }

    /// Port of Joycons::GetMotionMappingForDevice (override)
    pub fn get_motion_mapping_for_device(&self, _params: &ParamPackage) -> MotionMapping {
        todo!()
    }

    /// Port of Joycons::GetUIName (override)
    pub fn get_ui_name(&self, _params: &ParamPackage) -> ButtonNames {
        todo!()
    }

    // ---- Private methods ----

    fn reset(&mut self) {
        todo!()
    }

    fn setup(&mut self) {
        todo!()
    }

    fn scan_thread(&mut self) {
        todo!()
    }

    fn get_identifier(&self, _port: usize, _controller_type: u8) -> PadIdentifier {
        todo!()
    }

    fn get_ui_button_name(&self, _params: &ParamPackage) -> ButtonNames {
        todo!()
    }
}
