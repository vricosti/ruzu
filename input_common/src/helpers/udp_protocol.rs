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

const HEADER_SIZE: usize = std::mem::size_of::<Header>();

/// Port of `Request::Create(PortInfo, client_id)`.
pub fn create_port_info_request(client_id: u32) -> Vec<u8> {
    let mut data = Vec::with_capacity(8);
    data.extend_from_slice(&request::MAX_PORTS.to_le_bytes());
    data.extend_from_slice(&[0, 1, 2, 3]);
    create_request(MessageType::PortInfo, client_id, &data)
}

/// Port of `Request::Create(PadData, client_id)` for `AllPads`.
pub fn create_pad_data_request(client_id: u32) -> Vec<u8> {
    let mut data = Vec::with_capacity(8);
    data.push(request::RegisterFlags::AllPads as u8);
    data.push(0);
    data.extend_from_slice(&EMPTY_MAC_ADDRESS);
    create_request(MessageType::PadData, client_id, &data)
}

fn create_request(message_type: MessageType, client_id: u32, data: &[u8]) -> Vec<u8> {
    let mut message = Vec::with_capacity(HEADER_SIZE + data.len());
    message.extend_from_slice(&CLIENT_MAGIC.to_le_bytes());
    message.extend_from_slice(&PROTOCOL_VERSION.to_le_bytes());
    message.extend_from_slice(&((data.len() + std::mem::size_of::<u32>()) as u16).to_le_bytes());
    message.extend_from_slice(&0u32.to_le_bytes());
    message.extend_from_slice(&client_id.to_le_bytes());
    message.extend_from_slice(&(message_type as u32).to_le_bytes());
    message.extend_from_slice(data);
    let crc = crc32_compute(&message);
    message[8..12].copy_from_slice(&crc.to_le_bytes());
    message
}

/// Port of `Message<T>` struct from udp_protocol.h
#[repr(C, packed)]
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

    pub fn decode_version(data: &[u8]) -> Option<Version> {
        let payload = data.get(super::HEADER_SIZE..super::HEADER_SIZE + 2)?;
        Some(Version {
            version: u16::from_le_bytes(payload.try_into().ok()?),
        })
    }

    pub fn decode_port_info(data: &[u8]) -> Option<PortInfo> {
        let payload = data.get(super::HEADER_SIZE..super::HEADER_SIZE + 12)?;
        Some(PortInfo {
            id: payload[0],
            state: state(payload[1])?,
            model: model(payload[2])?,
            connection_type: connection_type(payload[3])?,
            mac: payload[4..10].try_into().ok()?,
            battery: battery(payload[10])?,
            is_pad_active: payload[11],
        })
    }

    /// Decodes a validated DSU pad-data payload without relying on host struct
    /// alignment. Upstream can `memcpy` because its protocol structs are
    /// explicitly packed; parsing fields keeps the same wire layout in Rust.
    pub fn decode_pad_data(data: &[u8]) -> Option<PadData> {
        const PAD_DATA_SIZE: usize = 80;
        let payload = data.get(super::HEADER_SIZE..super::HEADER_SIZE + PAD_DATA_SIZE)?;
        let connection_type = connection_type(payload[3])?;
        let state = state(payload[1])?;
        let model = model(payload[2])?;
        let battery = battery(payload[10])?;
        let info = PortInfo {
            id: payload[0],
            state,
            model,
            connection_type,
            mac: payload[4..10].try_into().ok()?,
            battery,
            is_pad_active: payload[11],
        };
        Some(PadData {
            info,
            packet_counter: u32::from_le_bytes(payload[12..16].try_into().ok()?),
            digital_button: u16::from_le_bytes(payload[16..18].try_into().ok()?),
            home: payload[18],
            touch_hard_press: payload[19],
            left_stick_x: payload[20],
            left_stick_y: payload[21],
            right_stick_x: payload[22],
            right_stick_y: payload[23],
            analog_button: AnalogButton {
                button_dpad_left_analog: payload[24],
                button_dpad_down_analog: payload[25],
                button_dpad_right_analog: payload[26],
                button_dpad_up_analog: payload[27],
                button_square_analog: payload[28],
                button_cross_analog: payload[29],
                button_circle_analog: payload[30],
                button_triangle_analog: payload[31],
                button_r1_analog: payload[32],
                button_l1_analog: payload[33],
                trigger_r2: payload[34],
                trigger_l2: payload[35],
            },
            touch: [decode_touch(payload, 36)?, decode_touch(payload, 42)?],
            motion_timestamp: u64::from_le_bytes(payload[48..56].try_into().ok()?),
            accel: Accelerometer {
                x: f32::from_le_bytes(payload[56..60].try_into().ok()?),
                y: f32::from_le_bytes(payload[60..64].try_into().ok()?),
                z: f32::from_le_bytes(payload[64..68].try_into().ok()?),
            },
            gyro: Gyroscope {
                pitch: f32::from_le_bytes(payload[68..72].try_into().ok()?),
                yaw: f32::from_le_bytes(payload[72..76].try_into().ok()?),
                roll: f32::from_le_bytes(payload[76..80].try_into().ok()?),
            },
        })
    }

    fn decode_touch(payload: &[u8], offset: usize) -> Option<TouchPad> {
        Some(TouchPad {
            is_active: *payload.get(offset)?,
            id: *payload.get(offset + 1)?,
            x: u16::from_le_bytes(payload.get(offset + 2..offset + 4)?.try_into().ok()?),
            y: u16::from_le_bytes(payload.get(offset + 4..offset + 6)?.try_into().ok()?),
        })
    }

    fn connection_type(value: u8) -> Option<ConnectionType> {
        match value {
            0 => Some(ConnectionType::None),
            1 => Some(ConnectionType::Usb),
            2 => Some(ConnectionType::Bluetooth),
            _ => None,
        }
    }

    fn state(value: u8) -> Option<State> {
        match value {
            0 => Some(State::Disconnected),
            1 => Some(State::Reserved),
            2 => Some(State::Connected),
            _ => None,
        }
    }

    fn model(value: u8) -> Option<Model> {
        match value {
            0 => Some(Model::None),
            1 => Some(Model::PartialGyro),
            2 => Some(Model::FullGyro),
            3 => Some(Model::Generic),
            _ => None,
        }
    }

    fn battery(value: u8) -> Option<Battery> {
        match value {
            0x00 => Some(Battery::None),
            0x01 => Some(Battery::Dying),
            0x02 => Some(Battery::Low),
            0x03 => Some(Battery::Medium),
            0x04 => Some(Battery::High),
            0x05 => Some(Battery::Full),
            0xEE => Some(Battery::Charging),
            0xEF => Some(Battery::Charged),
            _ => None,
        }
    }

    /// Returns the expected size of the response data for a given message type.
    fn get_size_of_response_type(t: super::MessageType) -> usize {
        match t {
            super::MessageType::Version => std::mem::size_of::<Version>(),
            super::MessageType::PortInfo => std::mem::size_of::<PortInfo>(),
            super::MessageType::PadData => std::mem::size_of::<PadData>(),
        }
    }

    /// Validates response data and returns the message type if valid.
    /// Port of Response::Validate from udp_protocol.cpp
    ///
    /// Note: Modifies the buffer to zero out the crc (since that's the easiest way to check
    /// without copying the buffer).
    pub fn validate(data: &mut [u8]) -> Option<super::MessageType> {
        let header_size = std::mem::size_of::<super::Header>();
        if data.len() < header_size {
            return None;
        }

        // Read header fields
        let magic = u32::from_le_bytes([data[0], data[1], data[2], data[3]]);
        let protocol_version = u16::from_le_bytes([data[4], data[5]]);
        let payload_length = u16::from_le_bytes([data[6], data[7]]);
        let crc32 = u32::from_le_bytes([data[8], data[9], data[10], data[11]]);
        // id at offset 12..16
        let message_type_raw = u32::from_le_bytes([data[16], data[17], data[18], data[19]]);

        if magic != super::SERVER_MAGIC {
            return None;
        }
        if protocol_version != super::PROTOCOL_VERSION {
            return None;
        }

        let message_type = match message_type_raw {
            0x00100000 => super::MessageType::Version,
            0x00100001 => super::MessageType::PortInfo,
            0x00100002 => super::MessageType::PadData,
            _ => return None,
        };

        // Packet size must equal sizeof(Header) + sizeof(Data)
        // payload_length == sizeof(T) + sizeof(Type)
        let data_len = get_size_of_response_type(message_type);
        let type_size = std::mem::size_of::<u32>(); // sizeof(Type) == 4
        if payload_length as usize != data_len + type_size || data.len() < data_len + header_size {
            return None;
        }

        // Zero out the CRC field in the buffer and compute CRC-32
        data[8] = 0;
        data[9] = 0;
        data[10] = 0;
        data[11] = 0;

        let computed_crc = crc32_compute(&data[..data_len + header_size]);
        if crc32 != computed_crc {
            return None;
        }

        Some(message_type)
    }

    /// Simple CRC-32 implementation (ISO 3309 / ITU-T V.42, same as boost::crc_32_type).
    pub(super) fn crc32_compute(data: &[u8]) -> u32 {
        let mut crc: u32 = 0xFFFFFFFF;
        for &byte in data {
            crc ^= byte as u32;
            for _ in 0..8 {
                if crc & 1 != 0 {
                    crc = (crc >> 1) ^ 0xEDB88320;
                } else {
                    crc >>= 1;
                }
            }
        }
        !crc
    }
}

fn crc32_compute(data: &[u8]) -> u32 {
    response::crc32_compute(data)
}

#[cfg(test)]
mod tests {
    use super::response::{Battery, ConnectionType, Model, State};
    use super::*;

    #[test]
    fn request_packets_match_upstream_wire_layout() {
        let port_info = create_port_info_request(0x1234_5678);
        assert_eq!(port_info.len(), 28);
        assert_eq!(&port_info[0..4], &CLIENT_MAGIC.to_le_bytes());
        assert_eq!(&port_info[6..8], &12u16.to_le_bytes());
        assert_eq!(&port_info[12..16], &0x1234_5678u32.to_le_bytes());
        assert_eq!(
            &port_info[16..20],
            &(MessageType::PortInfo as u32).to_le_bytes()
        );
        assert_eq!(&port_info[20..28], &[4, 0, 0, 0, 0, 1, 2, 3]);

        let mut crc_input = port_info.clone();
        let crc = u32::from_le_bytes(crc_input[8..12].try_into().unwrap());
        crc_input[8..12].fill(0);
        assert_eq!(crc, crc32_compute(&crc_input));

        let pad_data = create_pad_data_request(7);
        assert_eq!(pad_data.len(), 28);
        assert_eq!(&pad_data[20..28], &[0; 8]);
    }

    #[test]
    fn decode_pad_data_uses_packed_protocol_offsets() {
        let mut packet = vec![0u8; HEADER_SIZE + 80];
        packet[HEADER_SIZE + 1] = State::Connected as u8;
        packet[HEADER_SIZE + 2] = Model::FullGyro as u8;
        packet[HEADER_SIZE + 3] = ConnectionType::Usb as u8;
        packet[HEADER_SIZE + 10] = Battery::Full as u8;
        packet[HEADER_SIZE + 12..HEADER_SIZE + 16].copy_from_slice(&42u32.to_le_bytes());
        packet[HEADER_SIZE + 36] = 1;
        packet[HEADER_SIZE + 38..HEADER_SIZE + 40].copy_from_slice(&321u16.to_le_bytes());
        packet[HEADER_SIZE + 40..HEADER_SIZE + 42].copy_from_slice(&654u16.to_le_bytes());
        packet[HEADER_SIZE + 68..HEADER_SIZE + 72].copy_from_slice(&1.5f32.to_le_bytes());

        let decoded = response::decode_pad_data(&packet).unwrap();
        assert_eq!(decoded.packet_counter, 42);
        assert_eq!(decoded.touch[0].x, 321);
        assert_eq!(decoded.touch[0].y, 654);
        assert_eq!(decoded.gyro.pitch, 1.5);
    }
}
