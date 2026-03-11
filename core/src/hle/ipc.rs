// SPDX-FileCopyrightText: 2016 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/ipc.h
//! Status: COMPLET
//!
//! IPC command headers and buffer descriptors. These are `#[repr(C)]` binary-layout structs
//! with the same field layout as the C++ upstream.

use common::bit_field;

/// Size of the command buffer area, in 32-bit words.
pub const COMMAND_BUFFER_LENGTH: usize = 0x100 / core::mem::size_of::<u32>();

/// IPC control command types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ControlCommand {
    ConvertSessionToDomain = 0,
    ConvertDomainToSession = 1,
    DuplicateSession = 2,
    QueryPointerBufferSize = 3,
    DuplicateSessionEx = 4,
}

impl ControlCommand {
    /// Convert from a raw u32 value.
    pub fn from_u32(value: u32) -> Option<Self> {
        match value {
            0 => Some(Self::ConvertSessionToDomain),
            1 => Some(Self::ConvertDomainToSession),
            2 => Some(Self::DuplicateSession),
            3 => Some(Self::QueryPointerBufferSize),
            4 => Some(Self::DuplicateSessionEx),
            _ => None,
        }
    }
}

/// IPC command types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum CommandType {
    Invalid = 0,
    LegacyRequest = 1,
    Close = 2,
    LegacyControl = 3,
    Request = 4,
    Control = 5,
    RequestWithContext = 6,
    ControlWithContext = 7,
    TipcClose = 15,
    /// Start of TIPC commands, this is an offset.
    TipcCommandRegion = 16,
}

impl CommandType {
    /// Convert from a raw u32 value.
    pub fn from_u32(value: u32) -> Option<Self> {
        match value {
            0 => Some(Self::Invalid),
            1 => Some(Self::LegacyRequest),
            2 => Some(Self::Close),
            3 => Some(Self::LegacyControl),
            4 => Some(Self::Request),
            5 => Some(Self::Control),
            6 => Some(Self::RequestWithContext),
            7 => Some(Self::ControlWithContext),
            15 => Some(Self::TipcClose),
            x if x >= 16 => Some(Self::TipcCommandRegion),
            _ => None,
        }
    }
}

/// Flags for buffer descriptor C in the command header.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum BufferDescriptorCFlag {
    Disabled = 0,
    InlineDescriptor = 1,
    OneDescriptor = 2,
}

impl BufferDescriptorCFlag {
    pub fn from_u32(value: u32) -> Option<Self> {
        match value {
            0 => Some(Self::Disabled),
            1 => Some(Self::InlineDescriptor),
            2 => Some(Self::OneDescriptor),
            _ => None,
        }
    }
}

/// IPC command header. Two 32-bit words with bitfield layout.
///
/// Word 0 (raw_low):
///   bits [0, 16):  CommandType type
///   bits [16, 20): num_buf_x_descriptors
///   bits [20, 24): num_buf_a_descriptors
///   bits [24, 28): num_buf_b_descriptors
///   bits [28, 32): num_buf_w_descriptors
///
/// Word 1 (raw_high):
///   bits [0, 10):  data_size
///   bits [10, 14): buf_c_descriptor_flags (BufferDescriptorCFlag)
///   bit  31:       enable_handle_descriptor
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct CommandHeader {
    pub raw_low: u32,
    pub raw_high: u32,
}

const _: () = assert!(core::mem::size_of::<CommandHeader>() == 8);

impl CommandHeader {
    /// Get the command type.
    #[inline]
    pub fn command_type(&self) -> CommandType {
        let raw = bit_field::extract_unsigned(self.raw_low, 0, 16);
        CommandType::from_u32(raw).unwrap_or(CommandType::Invalid)
    }

    /// Get the raw command type value (useful for TIPC commands with value >= 16).
    #[inline]
    pub fn command_type_raw(&self) -> u32 {
        bit_field::extract_unsigned(self.raw_low, 0, 16)
    }

    /// Number of X (receive list) buffer descriptors.
    #[inline]
    pub fn num_buf_x_descriptors(&self) -> u32 {
        bit_field::extract_unsigned(self.raw_low, 16, 4)
    }

    /// Number of A (send) buffer descriptors.
    #[inline]
    pub fn num_buf_a_descriptors(&self) -> u32 {
        bit_field::extract_unsigned(self.raw_low, 20, 4)
    }

    /// Number of B (receive) buffer descriptors.
    #[inline]
    pub fn num_buf_b_descriptors(&self) -> u32 {
        bit_field::extract_unsigned(self.raw_low, 24, 4)
    }

    /// Number of W (exchange) buffer descriptors.
    #[inline]
    pub fn num_buf_w_descriptors(&self) -> u32 {
        bit_field::extract_unsigned(self.raw_low, 28, 4)
    }

    /// Data payload size in 32-bit words.
    #[inline]
    pub fn data_size(&self) -> u32 {
        bit_field::extract_unsigned(self.raw_high, 0, 10)
    }

    /// Buffer descriptor C flags.
    #[inline]
    pub fn buf_c_descriptor_flags(&self) -> BufferDescriptorCFlag {
        let raw = bit_field::extract_unsigned(self.raw_high, 10, 4);
        BufferDescriptorCFlag::from_u32(raw).unwrap_or(BufferDescriptorCFlag::Disabled)
    }

    /// Whether the handle descriptor is enabled.
    #[inline]
    pub fn enable_handle_descriptor(&self) -> bool {
        bit_field::extract_unsigned(self.raw_high, 31, 1) != 0
    }

    /// Returns true if this is a TIPC command (type >= TipcCommandRegion).
    #[inline]
    pub fn is_tipc(&self) -> bool {
        self.command_type_raw() >= CommandType::TipcCommandRegion as u32
    }

    /// Returns true if this is a close command (Close or TIPC_Close).
    #[inline]
    pub fn is_close_command(&self) -> bool {
        let raw_type = self.command_type_raw();
        raw_type == CommandType::Close as u32 || raw_type == CommandType::TipcClose as u32
    }
}

/// Handle descriptor header. Single 32-bit word with bitfield layout.
///
///   bit  0:        send_current_pid
///   bits [1, 5):   num_handles_to_copy
///   bits [5, 9):   num_handles_to_move
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct HandleDescriptorHeader {
    pub raw: u32,
}

const _: () = assert!(core::mem::size_of::<HandleDescriptorHeader>() == 4);

impl HandleDescriptorHeader {
    /// Whether the current PID should be sent.
    #[inline]
    pub fn send_current_pid(&self) -> bool {
        bit_field::extract_unsigned(self.raw, 0, 1) != 0
    }

    /// Number of handles to copy.
    #[inline]
    pub fn num_handles_to_copy(&self) -> u32 {
        bit_field::extract_unsigned(self.raw, 1, 4)
    }

    /// Number of handles to move.
    #[inline]
    pub fn num_handles_to_move(&self) -> u32 {
        bit_field::extract_unsigned(self.raw, 5, 4)
    }
}

/// Buffer descriptor X (receive list entry). Two 32-bit words.
///
/// Word 0:
///   bits [0, 6):   counter_bits_0_5
///   bits [6, 9):   address_bits_36_38
///   bits [9, 12):  counter_bits_9_11
///   bits [12, 16): address_bits_32_35
///   bits [16, 32): size
///
/// Word 1:
///   bits [0, 32):  address_bits_0_31
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct BufferDescriptorX {
    pub raw: u32,
    pub address_bits_0_31: u32,
}

const _: () = assert!(core::mem::size_of::<BufferDescriptorX>() == 8);

impl BufferDescriptorX {
    /// Get the counter value (bits 0-5 and 9-11 combined).
    #[inline]
    pub fn counter(&self) -> u32 {
        let bits_0_5 = bit_field::extract_unsigned(self.raw, 0, 6);
        let bits_9_11 = bit_field::extract_unsigned(self.raw, 9, 3);
        bits_0_5 | (bits_9_11 << 9)
    }

    /// Get the full 39-bit virtual address.
    #[inline]
    pub fn address(&self) -> u64 {
        let addr_0_31 = self.address_bits_0_31 as u64;
        let addr_32_35 = bit_field::extract_unsigned(self.raw, 12, 4) as u64;
        let addr_36_38 = bit_field::extract_unsigned(self.raw, 6, 3) as u64;
        addr_0_31 | (addr_32_35 << 32) | (addr_36_38 << 36)
    }

    /// Get the buffer size.
    #[inline]
    pub fn size(&self) -> u64 {
        bit_field::extract_unsigned(self.raw, 16, 16) as u64
    }
}

/// Buffer descriptor A/B/W (send/receive/exchange). Three 32-bit words.
///
/// Word 0: size_bits_0_31
/// Word 1: address_bits_0_31
/// Word 2:
///   bits [0, 2):   flags
///   bits [2, 5):   address_bits_36_38
///   bits [24, 28): size_bits_32_35
///   bits [28, 32): address_bits_32_35
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct BufferDescriptorABW {
    pub size_bits_0_31: u32,
    pub address_bits_0_31: u32,
    pub raw_word2: u32,
}

const _: () = assert!(core::mem::size_of::<BufferDescriptorABW>() == 12);

impl BufferDescriptorABW {
    /// Get the flags.
    #[inline]
    pub fn flags(&self) -> u32 {
        bit_field::extract_unsigned(self.raw_word2, 0, 2)
    }

    /// Get the full 39-bit virtual address.
    #[inline]
    pub fn address(&self) -> u64 {
        let addr_0_31 = self.address_bits_0_31 as u64;
        let addr_32_35 = bit_field::extract_unsigned(self.raw_word2, 28, 4) as u64;
        let addr_36_38 = bit_field::extract_unsigned(self.raw_word2, 2, 3) as u64;
        addr_0_31 | (addr_32_35 << 32) | (addr_36_38 << 36)
    }

    /// Get the full 36-bit buffer size.
    #[inline]
    pub fn size(&self) -> u64 {
        let size_0_31 = self.size_bits_0_31 as u64;
        let size_32_35 = bit_field::extract_unsigned(self.raw_word2, 24, 4) as u64;
        size_0_31 | (size_32_35 << 32)
    }
}

/// Buffer descriptor C (receive list). Two 32-bit words.
///
/// Word 0: address_bits_0_31
/// Word 1:
///   bits [0, 16):  address_bits_32_47
///   bits [16, 32): size
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct BufferDescriptorC {
    pub address_bits_0_31: u32,
    pub raw_word1: u32,
}

const _: () = assert!(core::mem::size_of::<BufferDescriptorC>() == 8);

impl BufferDescriptorC {
    /// Get the full 48-bit virtual address.
    #[inline]
    pub fn address(&self) -> u64 {
        let addr_0_31 = self.address_bits_0_31 as u64;
        let addr_32_47 = bit_field::extract_unsigned(self.raw_word1, 0, 16) as u64;
        addr_0_31 | (addr_32_47 << 32)
    }

    /// Get the buffer size.
    #[inline]
    pub fn size(&self) -> u64 {
        bit_field::extract_unsigned(self.raw_word1, 16, 16) as u64
    }
}

/// Data payload header. Two 32-bit words: magic + padding.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct DataPayloadHeader {
    pub magic: u32,
    pub _padding: u32,
}

const _: () = assert!(core::mem::size_of::<DataPayloadHeader>() == 8);

/// Domain message header command type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum DomainCommandType {
    SendMessage = 1,
    CloseVirtualHandle = 2,
}

impl DomainCommandType {
    pub fn from_u32(value: u32) -> Option<Self> {
        match value {
            1 => Some(Self::SendMessage),
            2 => Some(Self::CloseVirtualHandle),
            _ => None,
        }
    }
}

/// Domain message header. Four 32-bit words (16 bytes).
///
/// Used as a union in C++ with two overlapping interpretations:
///
/// Server -> Client (response):
///   Word 0: num_objects
///   Words 1-3: padding
///
/// Client -> Server (request):
///   Word 0:
///     bits [0, 8):   command (DomainCommandType)
///     bits [8, 16):  input_object_count
///     bits [16, 32): size
///   Word 1: object_id
///   Words 2-3: padding
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct DomainMessageHeader {
    pub raw: [u32; 4],
}

const _: () = assert!(core::mem::size_of::<DomainMessageHeader>() == 16);

impl DomainMessageHeader {
    // --- Server -> Client (response) accessors ---

    /// Number of objects (response, word 0).
    #[inline]
    pub fn num_objects(&self) -> u32 {
        self.raw[0]
    }

    /// Set number of objects (response).
    #[inline]
    pub fn set_num_objects(&mut self, value: u32) {
        self.raw[0] = value;
    }

    // --- Client -> Server (request) accessors ---

    /// Get the domain command type (request, word 0, bits [0, 8)).
    #[inline]
    pub fn command(&self) -> DomainCommandType {
        let raw = bit_field::extract_unsigned(self.raw[0], 0, 8);
        DomainCommandType::from_u32(raw).unwrap_or(DomainCommandType::SendMessage)
    }

    /// Get the input object count (request, word 0, bits [8, 16)).
    #[inline]
    pub fn input_object_count(&self) -> u32 {
        bit_field::extract_unsigned(self.raw[0], 8, 8)
    }

    /// Get the data size (request, word 0, bits [16, 32)).
    #[inline]
    pub fn data_size(&self) -> u32 {
        bit_field::extract_unsigned(self.raw[0], 16, 16)
    }

    /// Get the object ID (request, word 1).
    #[inline]
    pub fn object_id(&self) -> u32 {
        self.raw[1]
    }

    /// Set the object ID (request, word 1).
    #[inline]
    pub fn set_object_id(&mut self, value: u32) {
        self.raw[1] = value;
    }

    /// Set the command type (request, word 0, bits [0, 8)).
    #[inline]
    pub fn set_command(&mut self, cmd: DomainCommandType) {
        self.raw[0] = bit_field::assign(self.raw[0], cmd as u32, 0, 8);
    }

    /// Set the input object count (request, word 0, bits [8, 16)).
    #[inline]
    pub fn set_input_object_count(&mut self, count: u32) {
        self.raw[0] = bit_field::assign(self.raw[0], count, 8, 8);
    }

    /// Set the data size (request, word 0, bits [16, 32)).
    #[inline]
    pub fn set_data_size(&mut self, size: u32) {
        self.raw[0] = bit_field::assign(self.raw[0], size, 16, 16);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_command_buffer_length() {
        assert_eq!(COMMAND_BUFFER_LENGTH, 64);
    }

    #[test]
    fn test_command_header_size() {
        assert_eq!(core::mem::size_of::<CommandHeader>(), 8);
    }

    #[test]
    fn test_command_header_type() {
        let header = CommandHeader {
            raw_low: 4, // Request
            raw_high: 0,
        };
        assert_eq!(header.command_type(), CommandType::Request);
        assert!(!header.is_tipc());
        assert!(!header.is_close_command());
    }

    #[test]
    fn test_command_header_close() {
        let header = CommandHeader {
            raw_low: 2, // Close
            raw_high: 0,
        };
        assert!(header.is_close_command());
    }

    #[test]
    fn test_command_header_tipc_close() {
        let header = CommandHeader {
            raw_low: 15, // TIPC_Close
            raw_high: 0,
        };
        assert!(header.is_close_command());
    }

    #[test]
    fn test_command_header_tipc() {
        let header = CommandHeader {
            raw_low: 16, // TipcCommandRegion
            raw_high: 0,
        };
        assert!(header.is_tipc());
    }

    #[test]
    fn test_command_header_descriptors() {
        // num_buf_x = 2 at bits [16,20), num_buf_a = 3 at bits [20,24),
        // num_buf_b = 1 at bits [24,28), num_buf_w = 0 at bits [28,32)
        let raw_low = 4u32 | (2 << 16) | (3 << 20) | (1 << 24);
        let header = CommandHeader {
            raw_low,
            raw_high: 0,
        };
        assert_eq!(header.num_buf_x_descriptors(), 2);
        assert_eq!(header.num_buf_a_descriptors(), 3);
        assert_eq!(header.num_buf_b_descriptors(), 1);
        assert_eq!(header.num_buf_w_descriptors(), 0);
    }

    #[test]
    fn test_command_header_data_size_and_flags() {
        // data_size = 0x10 at bits [0,10), buf_c_flags = 2 at bits [10,14),
        // enable_handle_descriptor = 1 at bit 31
        let raw_high = 0x10u32 | (2 << 10) | (1 << 31);
        let header = CommandHeader {
            raw_low: 0,
            raw_high,
        };
        assert_eq!(header.data_size(), 0x10);
        assert_eq!(
            header.buf_c_descriptor_flags(),
            BufferDescriptorCFlag::OneDescriptor
        );
        assert!(header.enable_handle_descriptor());
    }

    #[test]
    fn test_handle_descriptor_header() {
        // send_current_pid = 1 at bit 0, num_copy = 3 at bits [1,5), num_move = 2 at bits [5,9)
        let raw = 1u32 | (3 << 1) | (2 << 5);
        let header = HandleDescriptorHeader { raw };
        assert!(header.send_current_pid());
        assert_eq!(header.num_handles_to_copy(), 3);
        assert_eq!(header.num_handles_to_move(), 2);
    }

    #[test]
    fn test_buffer_descriptor_x_size() {
        assert_eq!(core::mem::size_of::<BufferDescriptorX>(), 8);
    }

    #[test]
    fn test_buffer_descriptor_x_address() {
        // address_bits_0_31 = 0x12345678
        // address_bits_32_35 = 0xA at bits [12,16) of raw
        // address_bits_36_38 = 0x5 at bits [6,9) of raw
        let raw = (0x5u32 << 6) | (0xAu32 << 12);
        let desc = BufferDescriptorX {
            raw,
            address_bits_0_31: 0x12345678,
        };
        let expected = 0x12345678u64 | (0xAu64 << 32) | (0x5u64 << 36);
        assert_eq!(desc.address(), expected);
    }

    #[test]
    fn test_buffer_descriptor_x_counter() {
        // counter_bits_0_5 = 0x3F at bits [0,6), counter_bits_9_11 = 0x7 at bits [9,12)
        let raw = 0x3Fu32 | (0x7u32 << 9);
        let desc = BufferDescriptorX {
            raw,
            address_bits_0_31: 0,
        };
        // counter = 0x3F | (0x7 << 9) = 63 | 3584 = 3647
        assert_eq!(desc.counter(), 0x3F | (0x7 << 9));
    }

    #[test]
    fn test_buffer_descriptor_abw_size() {
        assert_eq!(core::mem::size_of::<BufferDescriptorABW>(), 12);
    }

    #[test]
    fn test_buffer_descriptor_abw_address() {
        let desc = BufferDescriptorABW {
            size_bits_0_31: 0,
            address_bits_0_31: 0xAABBCCDD,
            raw_word2: (0x3u32 << 2) | (0xFu32 << 28), // addr_36_38=3, addr_32_35=0xF
        };
        let expected = 0xAABBCCDDu64 | (0xFu64 << 32) | (0x3u64 << 36);
        assert_eq!(desc.address(), expected);
    }

    #[test]
    fn test_buffer_descriptor_abw_data_size() {
        let desc = BufferDescriptorABW {
            size_bits_0_31: 0x1000,
            address_bits_0_31: 0,
            raw_word2: 0x5u32 << 24, // size_bits_32_35 = 5
        };
        let expected = 0x1000u64 | (0x5u64 << 32);
        assert_eq!(desc.size(), expected);
    }

    #[test]
    fn test_buffer_descriptor_c_size() {
        assert_eq!(core::mem::size_of::<BufferDescriptorC>(), 8);
    }

    #[test]
    fn test_buffer_descriptor_c_address() {
        let desc = BufferDescriptorC {
            address_bits_0_31: 0xDEADBEEF,
            raw_word1: 0x1234, // address_bits_32_47 = 0x1234
        };
        let expected = 0xDEADBEEFu64 | (0x1234u64 << 32);
        assert_eq!(desc.address(), expected);
    }

    #[test]
    fn test_buffer_descriptor_c_buf_size() {
        let desc = BufferDescriptorC {
            address_bits_0_31: 0,
            raw_word1: 0x0100_0000, // size = 0x100 at bits [16,32)
        };
        assert_eq!(desc.size(), 0x100);
    }

    #[test]
    fn test_data_payload_header_size() {
        assert_eq!(core::mem::size_of::<DataPayloadHeader>(), 8);
    }

    #[test]
    fn test_domain_message_header_size() {
        assert_eq!(core::mem::size_of::<DomainMessageHeader>(), 16);
    }

    #[test]
    fn test_domain_message_header_response() {
        let mut header = DomainMessageHeader::default();
        header.set_num_objects(5);
        assert_eq!(header.num_objects(), 5);
    }

    #[test]
    fn test_domain_message_header_request() {
        let mut header = DomainMessageHeader::default();
        header.set_command(DomainCommandType::SendMessage);
        header.set_input_object_count(3);
        header.set_data_size(0x100);
        header.set_object_id(42);

        assert_eq!(header.command(), DomainCommandType::SendMessage);
        assert_eq!(header.input_object_count(), 3);
        assert_eq!(header.data_size(), 0x100);
        assert_eq!(header.object_id(), 42);
    }
}
