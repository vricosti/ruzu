// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_driver.h` and `joycon_driver.cpp`.
//!
//! Main Joy-Con driver that manages communication with Joy-Con controllers.

use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use parking_lot::Mutex;

use common::input::DriverResult;

use crate::helpers::joycon_protocol::joycon_types::*;

/// Port of JoyconDriver::SupportedFeatures struct from joycon_driver.h
#[derive(Debug, Default)]
struct SupportedFeatures {
    passive: bool,
    hidbus: bool,
    irs: bool,
    motion: bool,
    nfc: bool,
    vibration: bool,
}

/// Port of `JoyconDriver` class from joycon_driver.h / joycon_driver.cpp
pub struct JoyconDriver {
    // Connection status
    is_connected: AtomicBool,
    delta_time: u64,
    error_counter: usize,
    // last_update: Instant, // would use std::time::Instant

    // External device status
    starlink_connected: bool,
    ring_connected: bool,
    amiibo_detected: bool,
    is_ring_disabled_by_irs: bool,

    // Hardware configuration
    leds: u8,
    mode: ReportMode,
    input_only_device: bool,
    passive_enabled: bool,
    hidbus_enabled: bool,
    irs_enabled: bool,
    motion_enabled: bool,
    nfc_enabled: bool,
    vibration_enabled: bool,

    // Calibration data
    gyro_sensitivity: GyroSensitivity,
    gyro_performance: GyroPerformance,
    accelerometer_sensitivity: AccelerometerSensitivity,
    accelerometer_performance: AccelerometerPerformance,
    left_stick_calibration: JoyStickCalibration,
    right_stick_calibration: JoyStickCalibration,
    motion_calibration: MotionCalibration,
    ring_calibration: RingCalibration,

    // Fixed joycon info
    version: FirmwareVersion,
    color: Color,
    port: usize,
    device_type: ControllerType,
    handle_device_type: ControllerType,
    serial_number: SerialNumber,
    handle_serial_number: SerialNumber,
    supported_features: SupportedFeatures,

    // Vibration
    last_vibration_result: DriverResult,

    // Thread related
    mutex: Mutex<()>,
    input_thread_running: bool,
    disable_input_thread: bool,
}

impl JoyconDriver {
    /// Port of JoyconDriver::JoyconDriver
    pub fn new(port: usize) -> Self {
        Self {
            is_connected: AtomicBool::new(false),
            delta_time: 0,
            error_counter: 0,
            starlink_connected: false,
            ring_connected: false,
            amiibo_detected: false,
            is_ring_disabled_by_irs: false,
            leds: 0,
            mode: ReportMode::SimpleHidMode,
            input_only_device: false,
            passive_enabled: false,
            hidbus_enabled: false,
            irs_enabled: false,
            motion_enabled: false,
            nfc_enabled: false,
            vibration_enabled: false,
            gyro_sensitivity: GyroSensitivity::Dps2000,
            gyro_performance: GyroPerformance::Hz208,
            accelerometer_sensitivity: AccelerometerSensitivity::G8,
            accelerometer_performance: AccelerometerPerformance::Hz100,
            left_stick_calibration: JoyStickCalibration::default(),
            right_stick_calibration: JoyStickCalibration::default(),
            motion_calibration: MotionCalibration::default(),
            ring_calibration: RingCalibration::default(),
            version: FirmwareVersion::default(),
            color: Color::default(),
            port,
            device_type: ControllerType::None,
            handle_device_type: ControllerType::None,
            serial_number: [0u8; 15],
            handle_serial_number: [0u8; 15],
            supported_features: SupportedFeatures::default(),
            last_vibration_result: DriverResult::Success,
            mutex: Mutex::new(()),
            input_thread_running: false,
            disable_input_thread: false,
        }
    }

    /// Port of JoyconDriver::InitializeDevice
    pub fn initialize_device(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of JoyconDriver::Stop
    pub fn stop(&mut self) {
        todo!()
    }

    /// Port of JoyconDriver::IsConnected
    pub fn is_connected(&self) -> bool {
        self.is_connected.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Port of JoyconDriver::IsVibrationEnabled
    pub fn is_vibration_enabled(&self) -> bool {
        self.vibration_enabled
    }

    /// Port of JoyconDriver::GetDeviceVersion
    pub fn get_device_version(&self) -> FirmwareVersion {
        self.version
    }

    /// Port of JoyconDriver::GetDeviceColor
    pub fn get_device_color(&self) -> Color {
        self.color.clone()
    }

    /// Port of JoyconDriver::GetDevicePort
    pub fn get_device_port(&self) -> usize {
        self.port
    }

    /// Port of JoyconDriver::GetDeviceType
    pub fn get_device_type(&self) -> ControllerType {
        self.device_type
    }

    /// Port of JoyconDriver::GetHandleDeviceType
    pub fn get_handle_device_type(&self) -> ControllerType {
        self.handle_device_type
    }

    /// Port of JoyconDriver::GetSerialNumber
    pub fn get_serial_number(&self) -> SerialNumber {
        self.serial_number
    }

    /// Port of JoyconDriver::GetHandleSerialNumber
    pub fn get_handle_serial_number(&self) -> SerialNumber {
        self.handle_serial_number
    }

    /// Port of JoyconDriver::SetVibration
    pub fn set_vibration(&mut self, _vibration: &VibrationValue) -> DriverResult {
        todo!()
    }

    /// Port of JoyconDriver::SetLedConfig
    pub fn set_led_config(&mut self, _led_pattern: u8) -> DriverResult {
        todo!()
    }

    /// Port of JoyconDriver::SetIrsConfig
    pub fn set_irs_config(&mut self, _mode: IrsMode, _format: IrsResolution) -> DriverResult {
        todo!()
    }

    /// Port of JoyconDriver::SetPassiveMode
    pub fn set_passive_mode(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of JoyconDriver::SetActiveMode
    pub fn set_active_mode(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of JoyconDriver::SetIrMode
    pub fn set_ir_mode(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of JoyconDriver::SetNfcMode
    pub fn set_nfc_mode(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of JoyconDriver::SetRingConMode
    pub fn set_ring_con_mode(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of JoyconDriver::StartNfcPolling
    pub fn start_nfc_polling(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of JoyconDriver::StopNfcPolling
    pub fn stop_nfc_polling(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of JoyconDriver::ReadAmiiboData
    pub fn read_amiibo_data(&mut self, _out_data: &mut Vec<u8>) -> DriverResult {
        todo!()
    }

    /// Port of JoyconDriver::WriteNfcData
    pub fn write_nfc_data(&mut self, _data: &[u8]) -> DriverResult {
        todo!()
    }

    /// Port of JoyconDriver::SetCallbacks
    pub fn set_callbacks(&mut self, _callbacks: &JoyconCallbacks) {
        todo!()
    }

    // ---- Private methods ----

    fn input_thread(&mut self) {
        todo!()
    }

    fn on_new_data(&mut self, _buffer: &mut [u8]) {
        todo!()
    }

    fn set_polling_mode(&mut self) -> DriverResult {
        todo!()
    }

    fn is_input_thread_valid(&self) -> bool {
        todo!()
    }

    fn is_payload_correct(&self, _status: i32, _buffer: &[u8]) -> bool {
        todo!()
    }

    fn get_supported_features(&self) -> SupportedFeatures {
        todo!()
    }
}
