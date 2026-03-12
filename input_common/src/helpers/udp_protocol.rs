// SPDX-FileCopyrightText: 2018 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/udp_protocol.h` and `udp_protocol.cpp`.
//!
//! Defines the Cemuhook UDP protocol structures for communication with
//! DSU (DSU = DualShock UDP) compatible servers.

/// Maximum packet size.
pub const MAX_PACKET_SIZE: usize = 100;
/// Protocol version.
pub const PROTOCOL_VERSION: u16 = 1001;
/// Client magic: DSUC (little-endian).
pub const CLIENT_MAGIC: u32 = 0x43555344;
/// Server magic: DSUS (little-endian).
pub const SERVER_MAGIC: u32 = 0x53555344;

/// Port of `Type` enum from udp_protocol.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum MessageType {
    Version = 0x00100000,
    PortInfo = 0x00100001,
    PadData = 0x00100002,
}

/// Port of `Header` struct from udp_protocol.h
#[repr(C)]
#[derive(Debug, Clone, Default)]
pub struct Header {
    pub magic: u32,
    pub protocol_version: u16,
    pub payload_length: u16,
    pub crc: u32,
    pub id: u32,
    pub message_type: u32,
}

// static_assert equivalent: Header should be 20 bytes
const _: () = assert!(std::mem::size_of::<Header>() == 20);

/// MAC address type.
pub type MacAddress = [u8; 6];

/// Empty MAC address constant.
pub const EMPTY_MAC_ADDRESS: MacAddress = [0, 0, 0, 0, 0, 0];

/// Port of `Message<T>` struct from udp_protocol.h
#[repr(C)]
pub struct Message<T> {
    pub header: Header,
    pub data: T,
}

// ---- Request types ----

pub mod request {
    //! Port of `Request` namespace from udp_protocol.h

    /// Port of Request::RegisterFlags enum from udp_protocol.h
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(u8)]
    pub enum RegisterFlags {
        AllPads,
        PadID,
        PadMACAddress,
    }

    /// Port of Request::Version struct from udp_protocol.h
    #[derive(Debug, Clone, Default)]
    pub struct Version;

    /// Maximum ports for port info request.
    pub const MAX_PORTS: u32 = 4;

    /// Port of Request::PortInfo struct from udp_protocol.h
    #[repr(C)]
    #[derive(Debug, Clone, Default)]
    pub struct PortInfo {
        pub pad_count: u32,
        pub port: [u8; 4],
    }

    /// Port of Request::PadData struct from udp_protocol.h
    #[repr(C)]
    #[derive(Debug, Clone)]
    pub struct PadData {
        pub flags: RegisterFlags,
        pub port_id: u8,
        pub mac: super::MacAddress,
    }
}

// ---- Response types ----

pub mod response {
    //! Port of `Response` namespace from udp_protocol.h

    /// Port of Response::ConnectionType enum from udp_protocol.h
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(u8)]
    pub enum ConnectionType {
        None,
        Usb,
        Bluetooth,
    }

    /// Port of Response::State enum from udp_protocol.h
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(u8)]
    pub enum State {
        Disconnected,
        Reserved,
        Connected,
    }

    /// Port of Response::Model enum from udp_protocol.h
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(u8)]
    pub enum Model {
        None,
        PartialGyro,
        FullGyro,
        Generic,
    }

    /// Port of Response::Battery enum from udp_protocol.h
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(u8)]
    pub enum Battery {
        None = 0x00,
        Dying = 0x01,
        Low = 0x02,
        Medium = 0x03,
        High = 0x04,
        Full = 0x05,
        Charging = 0xEE,
        Charged = 0xEF,
    }

    /// Port of Response::Version struct from udp_protocol.h
    #[repr(C)]
    #[derive(Debug, Clone, Default)]
    pub struct Version {
        pub version: u16,
    }

    /// Port of Response::PortInfo struct from udp_protocol.h
    #[repr(C)]
    #[derive(Debug, Clone)]
    pub struct PortInfo {
        pub id: u8,
        pub state: State,
        pub model: Model,
        pub connection_type: ConnectionType,
        pub mac: super::MacAddress,
        pub battery: Battery,
        pub is_pad_active: u8,
    }

    /// Port of Response::TouchPad struct from udp_protocol.h
    #[repr(C)]
    #[derive(Debug, Clone, Default)]
    pub struct TouchPad {
        pub is_active: u8,
        pub id: u8,
        pub x: u16,
        pub y: u16,
    }

    /// Port of Response::PadData::AnalogButton struct from udp_protocol.h
    #[repr(C)]
    #[derive(Debug, Clone, Default)]
    pub struct AnalogButton {
        pub button_dpad_left_analog: u8,
        pub button_dpad_down_analog: u8,
        pub button_dpad_right_analog: u8,
        pub button_dpad_up_analog: u8,
        pub button_square_analog: u8,
        pub button_cross_analog: u8,
        pub button_circle_analog: u8,
        pub button_triangle_analog: u8,
        pub button_r1_analog: u8,
        pub button_l1_analog: u8,
        pub trigger_r2: u8,
        pub trigger_l2: u8,
    }

    /// Port of Response::PadData::Accelerometer struct from udp_protocol.h
    #[repr(C)]
    #[derive(Debug, Clone, Default)]
    pub struct Accelerometer {
        pub x: f32,
        pub y: f32,
        pub z: f32,
    }

    /// Port of Response::PadData::Gyroscope struct from udp_protocol.h
    #[repr(C)]
    #[derive(Debug, Clone, Default)]
    pub struct Gyroscope {
        pub pitch: f32,
        pub yaw: f32,
        pub roll: f32,
    }

    /// Port of Response::PadData struct from udp_protocol.h
    #[repr(C)]
    #[derive(Debug, Clone)]
    pub struct PadData {
        pub info: PortInfo,
        pub packet_counter: u32,
        pub digital_button: u16,
        pub home: u8,
        pub touch_hard_press: u8,
        pub left_stick_x: u8,
        pub left_stick_y: u8,
        pub right_stick_x: u8,
        pub right_stick_y: u8,
        pub analog_button: AnalogButton,
        pub touch: [TouchPad; 2],
        pub motion_timestamp: u64,
        pub accel: Accelerometer,
        pub gyro: Gyroscope,
    }

    /// Validates response data and returns the message type if valid.
    /// Port of Response::Validate from udp_protocol.cpp
    pub fn validate(_data: &[u8]) -> Option<super::MessageType> {
        todo!()
    }
}
