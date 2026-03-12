// SPDX-FileCopyrightText: 2018 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/udp_client.h` and `input_common/drivers/udp_client.cpp`.
//!
//! UDP client driver for Cemuhook protocol (e.g., DS4Windows, BetterJoy).

use std::sync::Mutex;

use common::input::{BatteryLevel, ButtonNames, DriverResult};
use common::param_package::ParamPackage;
use common::uuid::UUID;

use crate::input_engine::{InputEngine, PadIdentifier};
use crate::main_common::{AnalogMapping, ButtonMapping, MotionMapping};

/// Port of CemuhookUDP namespace types

/// Port of `PadTouch` enum from udp_client.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PadTouch {
    Click,
    Undefined,
}

/// Port of `UDPPadStatus` struct from udp_client.h
#[derive(Debug, Clone)]
pub struct UdpPadStatus {
    pub host: String,
    pub port: u16,
    pub pad_index: usize,
}

impl Default for UdpPadStatus {
    fn default() -> Self {
        Self {
            host: "127.0.0.1".to_string(),
            port: 26760,
            pad_index: 0,
        }
    }
}

/// Port of `CalibrationData` struct from udp_client.h
#[derive(Debug, Clone, Default)]
pub struct CalibrationData {
    pub min_x: u16,
    pub min_y: u16,
    pub max_x: u16,
    pub max_y: u16,
}

/// Port of `DeviceStatus` struct from udp_client.h
pub struct DeviceStatus {
    pub update_mutex: Mutex<()>,
    pub touch_calibration: Option<CalibrationData>,
}

impl Default for DeviceStatus {
    fn default() -> Self {
        Self {
            update_mutex: Mutex::new(()),
            touch_calibration: None,
        }
    }
}

/// Port of UDPClient::PadButton enum from udp_client.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum PadButton {
    Undefined = 0x0000,
    Share = 0x0001,
    L3 = 0x0002,
    R3 = 0x0004,
    Options = 0x0008,
    Up = 0x0010,
    Right = 0x0020,
    Down = 0x0040,
    Left = 0x0080,
    L2 = 0x0100,
    R2 = 0x0200,
    L1 = 0x0400,
    R1 = 0x0800,
    Triangle = 0x1000,
    Circle = 0x2000,
    Cross = 0x4000,
    Square = 0x8000,
    Touch1 = 0x10000,
    Touch2 = 0x20000,
    Home = 0x40000,
    TouchHardPress = 0x80000,
}

/// Port of UDPClient::PadAxes enum from udp_client.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PadAxes {
    LeftStickX,
    LeftStickY,
    RightStickX,
    RightStickY,
    AnalogLeft,
    AnalogDown,
    AnalogRight,
    AnalogUp,
    AnalogSquare,
    AnalogCross,
    AnalogCircle,
    AnalogTriangle,
    AnalogR1,
    AnalogL1,
    AnalogR2,
    AnalogL3,
    AnalogR3,
    Touch1X,
    Touch1Y,
    Touch2X,
    Touch2Y,
    Undefined,
}

/// Maximum number of UDP clients.
const MAX_UDP_CLIENTS: usize = 8;
/// Pads per client.
const PADS_PER_CLIENT: usize = 4;

/// Port of UDPClient::PadData struct from udp_client.h
struct PadData {
    pad_index: usize,
    connected: bool,
    status: DeviceStatus,
    packet_sequence: u64,
}

/// Port of `UDPClient` class from udp_client.h / udp_client.cpp
pub struct UdpClient {
    engine: InputEngine,
    pads: Vec<PadData>, // MAX_UDP_CLIENTS * PADS_PER_CLIENT
}

impl UdpClient {
    /// Port of UDPClient::UDPClient
    pub fn new(input_engine: String) -> Self {
        Self {
            engine: InputEngine::new(input_engine),
            pads: Vec::new(),
        }
    }

    /// Port of UDPClient::ReloadSockets
    pub fn reload_sockets(&mut self) {
        todo!()
    }

    /// Port of UDPClient::GetInputDevices (override)
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        todo!()
    }

    /// Port of UDPClient::GetButtonMappingForDevice (override)
    pub fn get_button_mapping_for_device(&self, _params: &ParamPackage) -> ButtonMapping {
        todo!()
    }

    /// Port of UDPClient::GetAnalogMappingForDevice (override)
    pub fn get_analog_mapping_for_device(&self, _params: &ParamPackage) -> AnalogMapping {
        todo!()
    }

    /// Port of UDPClient::GetMotionMappingForDevice (override)
    pub fn get_motion_mapping_for_device(&self, _params: &ParamPackage) -> MotionMapping {
        todo!()
    }

    /// Port of UDPClient::GetUIName (override)
    pub fn get_ui_name(&self, _params: &ParamPackage) -> ButtonNames {
        todo!()
    }

    /// Port of UDPClient::IsStickInverted (override)
    pub fn is_stick_inverted(&self, _params: &ParamPackage) -> bool {
        todo!()
    }

    // ---- Private methods ----

    fn reset(&mut self) {
        todo!()
    }

    fn get_client_number(&self, _host: &str, _port: u16) -> usize {
        todo!()
    }

    fn get_battery_level(&self, _battery: u8) -> BatteryLevel {
        todo!()
    }

    fn get_pad_identifier(&self, _pad_index: usize) -> PadIdentifier {
        todo!()
    }

    fn get_host_uuid(&self, _host: &str) -> UUID {
        todo!()
    }

    fn get_ui_button_name(&self, _params: &ParamPackage) -> ButtonNames {
        todo!()
    }
}

/// Port of `CalibrationConfigurationJob` class from udp_client.h
pub struct CalibrationConfigurationJob {
    // Port of Status enum
}

/// Port of CalibrationConfigurationJob::Status enum from udp_client.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CalibrationStatus {
    Initialized,
    Ready,
    Stage1Completed,
    Completed,
}

impl CalibrationConfigurationJob {
    /// Port of CalibrationConfigurationJob constructor
    pub fn new(
        _host: &str,
        _port: u16,
        _status_callback: Box<dyn Fn(CalibrationStatus)>,
        _data_callback: Box<dyn Fn(u16, u16, u16, u16)>,
    ) -> Self {
        todo!()
    }

    /// Port of CalibrationConfigurationJob::Stop
    pub fn stop(&mut self) {
        todo!()
    }
}

/// Port of TestCommunication free function from udp_client.h
pub fn test_communication(
    _host: &str,
    _port: u16,
    _success_callback: Box<dyn Fn()>,
    _failure_callback: Box<dyn Fn()>,
) {
    todo!()
}
