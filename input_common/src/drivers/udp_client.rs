// SPDX-FileCopyrightText: 2018 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/udp_client.h` and `input_common/drivers/udp_client.cpp`.
//!
//! UDP client driver for Cemuhook protocol (e.g., DS4Windows, BetterJoy).

use std::sync::Mutex;
use std::time::Instant;

use common::input::{BatteryLevel, ButtonNames, DriverResult};
use common::param_package::ParamPackage;
use common::settings_input::{native_analog, native_button, native_motion};
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
    last_update: Instant,
}

impl Default for PadData {
    fn default() -> Self {
        Self {
            pad_index: 0,
            connected: false,
            status: DeviceStatus::default(),
            packet_sequence: 0,
            last_update: Instant::now(),
        }
    }
}

/// Port of UDPClient::ClientConnection struct from udp_client.h
struct ClientConnection {
    uuid: UUID,
    host: String,
    port: u16,
    active: i8,
    // socket and thread would be here for actual networking
}

impl Default for ClientConnection {
    fn default() -> Self {
        Self {
            uuid: UUID::from_string("00000000-0000-0000-0000-00007F000001"),
            host: "127.0.0.1".to_string(),
            port: 26760,
            active: -1,
        }
    }
}

/// Port of `UDPClient` class from udp_client.h / udp_client.cpp
pub struct UdpClient {
    engine: InputEngine,
    pads: Vec<PadData>,
    clients: Vec<ClientConnection>,
}

impl UdpClient {
    /// Port of UDPClient::UDPClient
    pub fn new(input_engine: String) -> Self {
        log::info!("Udp Initialization started");
        let mut client = Self {
            engine: InputEngine::new(input_engine),
            pads: (0..MAX_UDP_CLIENTS * PADS_PER_CLIENT)
                .map(|_| PadData::default())
                .collect(),
            clients: (0..MAX_UDP_CLIENTS)
                .map(|_| ClientConnection::default())
                .collect(),
        };
        client.reload_sockets();
        client
    }

    /// Port of UDPClient::ReloadSockets
    pub fn reload_sockets(&mut self) {
        self.reset();

        // In C++: parses Settings::values.udp_input_servers, splits by comma,
        // then by colon to get host:port pairs, and starts communication.
        // Without Settings integration we just initialize empty.
        log::debug!("UDPClient::reload_sockets called");
    }

    /// Port of UDPClient::GetInputDevices (override)
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        let mut devices = Vec::new();
        // In C++: checks Settings::values.enable_udp_controller first
        for client in 0..self.clients.len() {
            if self.clients[client].active != 1 {
                continue;
            }
            for index in 0..PADS_PER_CLIENT {
                let pad_index = client * PADS_PER_CLIENT + index;
                if !self.pads[pad_index].connected {
                    continue;
                }
                let pad_identifier = self.get_pad_identifier(pad_index);
                let mut identifier = ParamPackage::default();
                identifier.set_str("engine", self.engine.get_engine_name().to_string());
                identifier.set_str("display", format!("UDP Controller {}", pad_identifier.pad));
                identifier.set_str("guid", pad_identifier.guid.raw_string());
                identifier.set_int("port", pad_identifier.port as i32);
                identifier.set_int("pad", pad_identifier.pad as i32);
                devices.push(identifier);
            }
        }
        devices
    }

    /// Port of UDPClient::GetButtonMappingForDevice (override)
    pub fn get_button_mapping_for_device(&self, params: &ParamPackage) -> ButtonMapping {
        // This list excludes any button that can't be really mapped
        const SWITCH_TO_DSU_BUTTON: [(native_button::Values, PadButton); 22] = [
            (native_button::Values::A, PadButton::Circle),
            (native_button::Values::B, PadButton::Cross),
            (native_button::Values::X, PadButton::Triangle),
            (native_button::Values::Y, PadButton::Square),
            (native_button::Values::Plus, PadButton::Options),
            (native_button::Values::Minus, PadButton::Share),
            (native_button::Values::DLeft, PadButton::Left),
            (native_button::Values::DUp, PadButton::Up),
            (native_button::Values::DRight, PadButton::Right),
            (native_button::Values::DDown, PadButton::Down),
            (native_button::Values::L, PadButton::L1),
            (native_button::Values::R, PadButton::R1),
            (native_button::Values::ZL, PadButton::L2),
            (native_button::Values::ZR, PadButton::R2),
            (native_button::Values::SLLeft, PadButton::L2),
            (native_button::Values::SRLeft, PadButton::R2),
            (native_button::Values::SLRight, PadButton::L2),
            (native_button::Values::SRRight, PadButton::R2),
            (native_button::Values::LStick, PadButton::L3),
            (native_button::Values::RStick, PadButton::R3),
            (native_button::Values::Home, PadButton::Home),
            (native_button::Values::Screenshot, PadButton::TouchHardPress),
        ];

        if !params.has("guid") || !params.has("port") || !params.has("pad") {
            return ButtonMapping::new();
        }

        let mut mapping = ButtonMapping::new();
        for &(switch_button, dsu_button) in &SWITCH_TO_DSU_BUTTON {
            let mut button_params = ParamPackage::default();
            button_params.set_str("engine", self.engine.get_engine_name().to_string());
            button_params.set_str("guid", params.get_str("guid", ""));
            button_params.set_int("port", params.get_int("port", 0));
            button_params.set_int("pad", params.get_int("pad", 0));
            button_params.set_int("button", dsu_button as i32);
            mapping.insert(switch_button as i32, button_params);
        }

        mapping
    }

    /// Port of UDPClient::GetAnalogMappingForDevice (override)
    pub fn get_analog_mapping_for_device(&self, params: &ParamPackage) -> AnalogMapping {
        if !params.has("guid") || !params.has("port") || !params.has("pad") {
            return AnalogMapping::new();
        }

        let mut mapping = AnalogMapping::new();
        let mut left_analog_params = ParamPackage::default();
        left_analog_params.set_str("engine", self.engine.get_engine_name().to_string());
        left_analog_params.set_str("guid", params.get_str("guid", ""));
        left_analog_params.set_int("port", params.get_int("port", 0));
        left_analog_params.set_int("pad", params.get_int("pad", 0));
        left_analog_params.set_int("axis_x", PadAxes::LeftStickX as i32);
        left_analog_params.set_int("axis_y", PadAxes::LeftStickY as i32);
        mapping.insert(native_analog::Values::LStick as i32, left_analog_params);

        let mut right_analog_params = ParamPackage::default();
        right_analog_params.set_str("engine", self.engine.get_engine_name().to_string());
        right_analog_params.set_str("guid", params.get_str("guid", ""));
        right_analog_params.set_int("port", params.get_int("port", 0));
        right_analog_params.set_int("pad", params.get_int("pad", 0));
        right_analog_params.set_int("axis_x", PadAxes::RightStickX as i32);
        right_analog_params.set_int("axis_y", PadAxes::RightStickY as i32);
        mapping.insert(native_analog::Values::RStick as i32, right_analog_params);
        mapping
    }

    /// Port of UDPClient::GetMotionMappingForDevice (override)
    pub fn get_motion_mapping_for_device(&self, params: &ParamPackage) -> MotionMapping {
        if !params.has("guid") || !params.has("port") || !params.has("pad") {
            return MotionMapping::new();
        }

        let mut mapping = MotionMapping::new();
        let mut left_motion_params = ParamPackage::default();
        left_motion_params.set_str("engine", self.engine.get_engine_name().to_string());
        left_motion_params.set_str("guid", params.get_str("guid", ""));
        left_motion_params.set_int("port", params.get_int("port", 0));
        left_motion_params.set_int("pad", params.get_int("pad", 0));
        left_motion_params.set_int("motion", 0);

        let mut right_motion_params = ParamPackage::default();
        right_motion_params.set_str("engine", self.engine.get_engine_name().to_string());
        right_motion_params.set_str("guid", params.get_str("guid", ""));
        right_motion_params.set_int("port", params.get_int("port", 0));
        right_motion_params.set_int("pad", params.get_int("pad", 0));
        right_motion_params.set_int("motion", 0);

        mapping.insert(native_motion::Values::MotionLeft as i32, left_motion_params);
        mapping.insert(
            native_motion::Values::MotionRight as i32,
            right_motion_params,
        );
        mapping
    }

    /// Port of UDPClient::GetUIName (override)
    pub fn get_ui_name(&self, params: &ParamPackage) -> ButtonNames {
        if params.has("button") {
            return self.get_ui_button_name(params);
        }
        if params.has("axis") {
            return ButtonNames::Value;
        }
        if params.has("motion") {
            return ButtonNames::Engine;
        }
        ButtonNames::Invalid
    }

    /// Port of UDPClient::IsStickInverted (override)
    pub fn is_stick_inverted(&self, params: &ParamPackage) -> bool {
        if !params.has("guid") || !params.has("port") || !params.has("pad") {
            return false;
        }

        let x_axis = params.get_int("axis_x", 0) as u8;
        let y_axis = params.get_int("axis_y", 0) as u8;
        if x_axis != PadAxes::LeftStickY as u8 && x_axis != PadAxes::RightStickY as u8 {
            return false;
        }
        if y_axis != PadAxes::LeftStickX as u8 && y_axis != PadAxes::RightStickX as u8 {
            return false;
        }
        true
    }

    // ---- Private methods ----

    /// Port of UDPClient::Reset
    fn reset(&mut self) {
        for client in &mut self.clients {
            if client.active != -1 {
                client.active = -1;
                // In C++: stops socket and joins thread
            }
        }
    }

    /// Port of UDPClient::GetClientNumber
    fn get_client_number(&self, host: &str, port: u16) -> usize {
        for (client, conn) in self.clients.iter().enumerate() {
            if conn.active == -1 {
                continue;
            }
            if conn.host == host && conn.port == port {
                return client;
            }
        }
        MAX_UDP_CLIENTS
    }

    /// Port of UDPClient::GetBatteryLevel
    fn get_battery_level(&self, battery: u8) -> BatteryLevel {
        // Maps UDP battery enum values to input engine battery levels
        // C++ Response::Battery enum: Dying=0, Low=1, Medium=2, High=3, Full=4, Charged=5, Charging=6
        match battery {
            0 => BatteryLevel::Empty,    // Dying
            1 => BatteryLevel::Critical, // Low
            2 => BatteryLevel::Low,      // Medium
            3 => BatteryLevel::Medium,   // High
            4 | 5 => BatteryLevel::Full, // Full | Charged
            _ => BatteryLevel::Charging, // Charging or unknown
        }
    }

    /// Port of UDPClient::GetPadIdentifier
    fn get_pad_identifier(&self, pad_index: usize) -> PadIdentifier {
        let client = pad_index / PADS_PER_CLIENT;
        PadIdentifier {
            guid: self.clients[client].uuid,
            port: self.clients[client].port as usize,
            pad: pad_index,
        }
    }

    /// Port of UDPClient::GetHostUUID
    fn get_host_uuid(&self, host: &str) -> UUID {
        // In C++: parses IPv4, formats as hex UUID
        // "00000000-0000-0000-0000-0000" + hex(ip)
        let parts: Vec<&str> = host.split('.').collect();
        if parts.len() == 4 {
            let ip_val: u32 = parts
                .iter()
                .fold(0u32, |acc, p| (acc << 8) | p.parse::<u32>().unwrap_or(0));
            let hex_host = format!("00000000-0000-0000-0000-0000{:06x}", ip_val);
            UUID::from_string(&hex_host)
        } else {
            UUID::default()
        }
    }

    /// Port of UDPClient::GetUIButtonName
    fn get_ui_button_name(&self, params: &ParamPackage) -> ButtonNames {
        let button_raw = params.get_int("button", 0) as u32;
        match button_raw {
            x if x == PadButton::Left as u32 => ButtonNames::ButtonLeft,
            x if x == PadButton::Right as u32 => ButtonNames::ButtonRight,
            x if x == PadButton::Down as u32 => ButtonNames::ButtonDown,
            x if x == PadButton::Up as u32 => ButtonNames::ButtonUp,
            x if x == PadButton::L1 as u32 => ButtonNames::L1,
            x if x == PadButton::L2 as u32 => ButtonNames::L2,
            x if x == PadButton::L3 as u32 => ButtonNames::L3,
            x if x == PadButton::R1 as u32 => ButtonNames::R1,
            x if x == PadButton::R2 as u32 => ButtonNames::R2,
            x if x == PadButton::R3 as u32 => ButtonNames::R3,
            x if x == PadButton::Circle as u32 => ButtonNames::Circle,
            x if x == PadButton::Cross as u32 => ButtonNames::Cross,
            x if x == PadButton::Square as u32 => ButtonNames::Square,
            x if x == PadButton::Triangle as u32 => ButtonNames::Triangle,
            x if x == PadButton::Share as u32 => ButtonNames::Share,
            x if x == PadButton::Options as u32 => ButtonNames::Options,
            x if x == PadButton::Home as u32 => ButtonNames::Home,
            x if x == PadButton::Touch1 as u32
                || x == PadButton::Touch2 as u32
                || x == PadButton::TouchHardPress as u32 =>
            {
                ButtonNames::Touch
            }
            _ => ButtonNames::Undefined,
        }
    }
}

impl Drop for UdpClient {
    fn drop(&mut self) {
        self.reset();
    }
}

/// Port of `CalibrationConfigurationJob` class from udp_client.h
pub struct CalibrationConfigurationJob {
    // In C++: holds a Common::Event for completion signaling
    completed: bool,
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
    ///
    /// In C++: spawns a thread that creates a Socket, listens for touch data,
    /// computes min/max calibration values, and calls status/data callbacks.
    /// This requires the UDP Socket implementation (boost::asio).
    pub fn new(
        _host: &str,
        _port: u16,
        _status_callback: Box<dyn Fn(CalibrationStatus)>,
        _data_callback: Box<dyn Fn(u16, u16, u16, u16)>,
    ) -> Self {
        // The full implementation would create a UDP socket connection
        // and spawn a calibration thread. Without boost::asio/tokio,
        // we mark as immediately completed.
        Self { completed: false }
    }

    /// Port of CalibrationConfigurationJob::Stop
    pub fn stop(&mut self) {
        self.completed = true;
    }
}

impl Drop for CalibrationConfigurationJob {
    fn drop(&mut self) {
        self.stop();
    }
}

/// Port of TestCommunication free function from udp_client.h
///
/// In C++: spawns a thread that creates a Socket, waits for pad data,
/// and calls success/failure callbacks based on whether data arrives
/// within 10 seconds. Requires UDP socket implementation.
pub fn test_communication(
    _host: &str,
    _port: u16,
    _success_callback: Box<dyn Fn()>,
    _failure_callback: Box<dyn Fn()>,
) {
    // Without async networking, call failure callback immediately
    _failure_callback();
}
