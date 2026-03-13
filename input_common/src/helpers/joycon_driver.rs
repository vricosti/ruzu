// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_driver.h` and `joycon_driver.cpp`.
//!
//! Main Joy-Con driver that manages communication with Joy-Con controllers.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;

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
    last_update: Instant,

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
            last_update: Instant::now(),
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
    ///
    /// In C++: resets counters, initializes protocol objects (calibration, generic,
    /// IRS, NFC, ring, rumble), queries device info, gets calibration data,
    /// sets LED pattern, applies polling mode, creates JoyconPoller, and starts
    /// the input thread. Most of this requires the HID API handle.
    pub fn initialize_device(&mut self) -> DriverResult {
        let _lock = self.mutex.lock();
        self.disable_input_thread = true;

        // Reset counters
        self.error_counter = 0;

        // Reset external device status
        self.starlink_connected = false;
        self.ring_connected = false;
        self.amiibo_detected = false;

        // Set HW default configuration
        self.vibration_enabled = true;
        self.motion_enabled = true;
        self.hidbus_enabled = false;
        self.nfc_enabled = false;
        self.passive_enabled = false;
        self.irs_enabled = false;
        self.input_only_device = false;
        self.gyro_sensitivity = GyroSensitivity::Dps2000;
        self.gyro_performance = GyroPerformance::Hz208;
        self.accelerometer_sensitivity = AccelerometerSensitivity::G8;
        self.accelerometer_performance = AccelerometerPerformance::Hz100;

        // In C++: protocol objects are initialized, device info is queried,
        // calibration is loaded, LEDs are set, polling mode is applied,
        // and the input thread is started.
        // Without the HID handle these operations cannot proceed.

        self.supported_features = self.get_supported_features();

        self.is_connected.store(true, Ordering::Relaxed);
        self.disable_input_thread = false;
        DriverResult::Success
    }

    /// Port of JoyconDriver::Stop
    pub fn stop(&mut self) {
        self.is_connected.store(false, Ordering::Relaxed);
        // In C++: input_thread = {} which joins and destroys the thread
    }

    /// Port of JoyconDriver::IsConnected
    pub fn is_connected(&self) -> bool {
        self.is_connected.load(Ordering::Relaxed)
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
        let _lock = self.mutex.lock();
        if self.disable_input_thread {
            return DriverResult::HandleInUse;
        }
        // In C++: pushes to vibration_queue, returns last_vibration_result
        self.last_vibration_result
    }

    /// Port of JoyconDriver::SetLedConfig
    pub fn set_led_config(&mut self, _led_pattern: u8) -> DriverResult {
        let _lock = self.mutex.lock();
        if self.disable_input_thread {
            return DriverResult::HandleInUse;
        }
        // In C++: calls generic_protocol->SetLedPattern(led_pattern)
        DriverResult::NotSupported
    }

    /// Port of JoyconDriver::SetIrsConfig
    pub fn set_irs_config(&mut self, _mode: IrsMode, _format: IrsResolution) -> DriverResult {
        let _lock = self.mutex.lock();
        if self.disable_input_thread {
            return DriverResult::HandleInUse;
        }
        // In C++: disables input thread, calls irs_protocol->SetIrsConfig()
        DriverResult::NotSupported
    }

    /// Port of JoyconDriver::SetPassiveMode
    pub fn set_passive_mode(&mut self) -> DriverResult {
        {
            let _lock = self.mutex.lock();
            self.motion_enabled = false;
            self.hidbus_enabled = false;
            self.nfc_enabled = false;
            self.passive_enabled = true;
            self.irs_enabled = false;
        }
        self.set_polling_mode()
    }

    /// Port of JoyconDriver::SetActiveMode
    pub fn set_active_mode(&mut self) -> DriverResult {
        if self.is_ring_disabled_by_irs {
            self.is_ring_disabled_by_irs = false;
            // Recursive call to set active first, then ring con
            let _ = self.set_active_mode_inner();
            return self.set_ring_con_mode();
        }

        self.set_active_mode_inner()
    }

    /// Inner implementation for SetActiveMode to avoid infinite recursion
    fn set_active_mode_inner(&mut self) -> DriverResult {
        {
            let _lock = self.mutex.lock();
            self.motion_enabled = true;
            self.hidbus_enabled = false;
            self.nfc_enabled = false;
            self.passive_enabled = false;
            self.irs_enabled = false;
        }
        self.set_polling_mode()
    }

    /// Port of JoyconDriver::SetIrMode
    pub fn set_ir_mode(&mut self) -> DriverResult {
        // Rust &mut self already guarantees exclusive access; no mutex needed.

        if !self.supported_features.irs {
            return DriverResult::NotSupported;
        }

        if self.ring_connected {
            self.is_ring_disabled_by_irs = true;
        }

        self.motion_enabled = false;
        self.hidbus_enabled = false;
        self.nfc_enabled = false;
        self.passive_enabled = false;
        self.irs_enabled = true;
        self.set_polling_mode()
    }

    /// Port of JoyconDriver::SetNfcMode
    pub fn set_nfc_mode(&mut self) -> DriverResult {
        // Rust &mut self already guarantees exclusive access; no mutex needed.

        if !self.supported_features.nfc {
            return DriverResult::NotSupported;
        }

        self.motion_enabled = true;
        self.hidbus_enabled = false;
        self.nfc_enabled = true;
        self.passive_enabled = false;
        self.irs_enabled = false;
        self.set_polling_mode()
    }

    /// Port of JoyconDriver::SetRingConMode
    pub fn set_ring_con_mode(&mut self) -> DriverResult {
        // Rust &mut self already guarantees exclusive access; no mutex needed.

        if !self.supported_features.hidbus {
            return DriverResult::NotSupported;
        }

        self.motion_enabled = true;
        self.hidbus_enabled = true;
        self.nfc_enabled = false;
        self.passive_enabled = false;
        self.irs_enabled = false;

        let result = self.set_polling_mode();

        if !self.ring_connected {
            return DriverResult::NoDeviceDetected;
        }

        result
    }

    /// Port of JoyconDriver::StartNfcPolling
    pub fn start_nfc_polling(&mut self) -> DriverResult {
        let _lock = self.mutex.lock();

        if !self.supported_features.nfc {
            return DriverResult::NotSupported;
        }
        // In C++: checks nfc_protocol->IsEnabled(), disables input thread,
        // calls nfc_protocol->StartNFCPollingMode()
        DriverResult::Disabled
    }

    /// Port of JoyconDriver::StopNfcPolling
    pub fn stop_nfc_polling(&mut self) -> DriverResult {
        let _lock = self.mutex.lock();

        if !self.supported_features.nfc {
            return DriverResult::NotSupported;
        }
        // In C++: calls nfc_protocol->StopNFCPollingMode()
        if self.amiibo_detected {
            self.amiibo_detected = false;
        }
        DriverResult::Disabled
    }

    /// Port of JoyconDriver::ReadAmiiboData
    pub fn read_amiibo_data(&mut self, _out_data: &mut Vec<u8>) -> DriverResult {
        let _lock = self.mutex.lock();

        if !self.supported_features.nfc {
            return DriverResult::NotSupported;
        }
        if !self.amiibo_detected {
            return DriverResult::ErrorWritingData;
        }
        // In C++: resizes out_data to 0x21C, calls nfc_protocol->ReadAmiibo()
        DriverResult::Disabled
    }

    /// Port of JoyconDriver::WriteNfcData
    pub fn write_nfc_data(&mut self, _data: &[u8]) -> DriverResult {
        let _lock = self.mutex.lock();

        if !self.supported_features.nfc {
            return DriverResult::NotSupported;
        }
        if !self.amiibo_detected {
            return DriverResult::ErrorWritingData;
        }
        // In C++: calls nfc_protocol->WriteAmiibo()
        DriverResult::Disabled
    }

    /// Port of JoyconDriver::ReadMifareData
    pub fn read_mifare_data(
        &mut self,
        _data: &[MifareReadChunk],
        _out_data: &mut [MifareReadData],
    ) -> DriverResult {
        let _lock = self.mutex.lock();

        if !self.supported_features.nfc {
            return DriverResult::NotSupported;
        }
        if !self.amiibo_detected {
            return DriverResult::ErrorWritingData;
        }
        // In C++: calls nfc_protocol->ReadMifare()
        DriverResult::Disabled
    }

    /// Port of JoyconDriver::WriteMifareData
    pub fn write_mifare_data(&mut self, _data: &[MifareWriteChunk]) -> DriverResult {
        let _lock = self.mutex.lock();

        if !self.supported_features.nfc {
            return DriverResult::NotSupported;
        }
        if !self.amiibo_detected {
            return DriverResult::ErrorWritingData;
        }
        // In C++: calls nfc_protocol->WriteMifare()
        DriverResult::Disabled
    }

    /// Port of JoyconDriver::SetCallbacks
    pub fn set_callbacks(&mut self, _callbacks: &JoyconCallbacks) {
        // In C++: joycon_poller->SetCallbacks(callbacks)
    }

    /// Port of static JoyconDriver::GetDeviceType
    ///
    /// Returns the controller type for a given product ID.
    pub fn get_device_type_from_product_id(product_id: u16, vendor_id: u16) -> (DriverResult, ControllerType) {
        const NINTENDO_VENDOR_ID: u16 = 0x057e;
        const SUPPORTED_DEVICES: [(u16, ControllerType); 3] = [
            (0x2006, ControllerType::Left),
            (0x2007, ControllerType::Right),
            (0x2009, ControllerType::Pro),
        ];

        if vendor_id != NINTENDO_VENDOR_ID {
            return (DriverResult::UnsupportedControllerType, ControllerType::None);
        }

        for &(pid, ctype) in &SUPPORTED_DEVICES {
            if product_id == pid {
                return (DriverResult::Success, ctype);
            }
        }
        (DriverResult::UnsupportedControllerType, ControllerType::None)
    }

    // ---- Private methods ----

    /// Port of JoyconDriver::InputThread
    fn input_thread(&mut self) {
        log::info!("Joycon Adapter input thread started");
        self.input_thread_running = true;

        // Max update rate is 5ms, ensure we are always able to read a bit faster
        const THREAD_DELAY_MS: u64 = 3;
        let mut buffer = vec![0u8; MAX_BUFFER_SIZE as usize];

        // The thread loop would read from the HID handle, validate payloads,
        // call on_new_data(), and process the vibration queue.
        // Without the HID handle this cannot operate.

        self.is_connected.store(false, Ordering::Relaxed);
        self.input_thread_running = false;
        log::info!("Joycon Adapter input thread stopped");
    }

    /// Port of JoyconDriver::OnNewData
    fn on_new_data(&mut self, buffer: &mut [u8]) {
        let report_mode = buffer[0];

        // Average the delta time for smoother motion
        match report_mode {
            x if x == ReportMode::StandardFull60Hz as u8
                || x == ReportMode::NfcIrMode60Hz as u8
                || x == ReportMode::SimpleHidMode as u8 =>
            {
                let now = Instant::now();
                let new_delta_time = now.duration_since(self.last_update).as_micros() as u64;
                self.delta_time = ((self.delta_time * 8) + (new_delta_time * 2)) / 10;
                self.last_update = now;
            }
            _ => {}
        }

        // The rest of OnNewData processes motion, ring, IRS, NFC, and input data
        // through the various protocol objects and the joycon_poller.
    }

    /// Port of JoyconDriver::SetPollingMode
    fn set_polling_mode(&mut self) -> DriverResult {
        // In C++: configures rumble, IMU, IRS, NFC, ring con, and passive/active modes
        // through the protocol objects. Returns the result of the final mode switch.
        // Without protocol objects, return NotSupported for input-only devices.
        if self.input_only_device {
            return DriverResult::NotSupported;
        }
        DriverResult::Success
    }

    /// Port of JoyconDriver::IsInputThreadValid
    fn is_input_thread_valid(&self) -> bool {
        if !self.is_connected.load(Ordering::Relaxed) {
            return false;
        }
        // In C++: also checks hidapi_handle->handle != nullptr
        if self.error_counter > MAX_ERROR_COUNT as usize {
            return false;
        }
        true
    }

    /// Port of JoyconDriver::IsPayloadCorrect
    fn is_payload_correct(&mut self, status: i32, buffer: &[u8]) -> bool {
        if status <= -1 {
            self.error_counter += 1;
            return false;
        }
        // There's no new data
        if status == 0 {
            return false;
        }
        // No reply ever starts with zero
        if buffer[0] == 0x00 {
            self.error_counter += 1;
            return false;
        }
        self.error_counter = 0;
        true
    }

    /// Port of JoyconDriver::GetSupportedFeatures
    fn get_supported_features(&self) -> SupportedFeatures {
        let mut features = SupportedFeatures {
            passive: true,
            motion: true,
            vibration: true,
            ..Default::default()
        };

        if self.input_only_device {
            return features;
        }

        if self.device_type == ControllerType::Right {
            features.nfc = true;
            features.irs = true;
            features.hidbus = true;
        }

        if self.device_type == ControllerType::Pro {
            features.nfc = true;
        }
        features
    }
}

impl Drop for JoyconDriver {
    fn drop(&mut self) {
        self.stop();
    }
}
