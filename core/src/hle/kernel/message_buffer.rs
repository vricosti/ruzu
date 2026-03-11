//! Port of zuyu/src/core/hle/kernel/message_buffer.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! MessageBuffer: IPC message buffer manipulation types and helpers.
//! Contains MessageHeader, SpecialHeader, MapAliasDescriptor,
//! PointerDescriptor, and ReceiveListEntry.

use crate::hle::kernel::svc_common::Handle;

/// Size of the IPC message buffer in u32 words (0x100 = 256 words = 1024 bytes).
pub const MESSAGE_BUFFER_SIZE: usize = 0x100;

/// Receive list count type.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReceiveListCountType {
    None = 0,
    ToMessageBuffer = 1,
    ToSingleBuffer = 2,
}

/// Offset added to receive list count for per-buffer entries.
pub const RECEIVE_LIST_COUNT_TYPE_COUNT_OFFSET: u32 = 2;
/// Maximum receive list count value.
pub const RECEIVE_LIST_COUNT_TYPE_COUNT_MAX: u32 = 13;

/// IPC message header (2 words / 8 bytes).
#[derive(Debug, Clone, Copy, Default)]
pub struct MessageHeader {
    raw: [u32; 2],
}

impl MessageHeader {
    pub const DATA_SIZE: usize = 8; // 2 * sizeof(u32)

    pub fn new(
        tag: u16,
        special: bool,
        ptr: i32,
        send: i32,
        recv: i32,
        exch: i32,
        raw_count: i32,
        recv_list: u32,
    ) -> Self {
        let w0 = (tag as u32)
            | ((ptr as u32 & 0xF) << 16)
            | ((send as u32 & 0xF) << 20)
            | ((recv as u32 & 0xF) << 24)
            | ((exch as u32 & 0xF) << 28);
        let w1 = (raw_count as u32 & 0x3FF)
            | ((recv_list & 0xF) << 10)
            | (if special { 1u32 << 31 } else { 0 });
        Self { raw: [w0, w1] }
    }

    pub fn from_raw(raw: [u32; 2]) -> Self {
        Self { raw }
    }

    pub fn get_tag(&self) -> u16 {
        (self.raw[0] & 0xFFFF) as u16
    }
    pub fn get_pointer_count(&self) -> i32 {
        ((self.raw[0] >> 16) & 0xF) as i32
    }
    pub fn get_send_count(&self) -> i32 {
        ((self.raw[0] >> 20) & 0xF) as i32
    }
    pub fn get_receive_count(&self) -> i32 {
        ((self.raw[0] >> 24) & 0xF) as i32
    }
    pub fn get_exchange_count(&self) -> i32 {
        ((self.raw[0] >> 28) & 0xF) as i32
    }
    pub fn get_map_alias_count(&self) -> i32 {
        self.get_send_count() + self.get_receive_count() + self.get_exchange_count()
    }
    pub fn get_raw_count(&self) -> i32 {
        (self.raw[1] & 0x3FF) as i32
    }
    pub fn get_receive_list_count(&self) -> u32 {
        (self.raw[1] >> 10) & 0xF
    }
    pub fn get_receive_list_offset(&self) -> i32 {
        ((self.raw[1] >> 20) & 0x7FF) as i32
    }
    pub fn get_has_special_header(&self) -> bool {
        (self.raw[1] >> 31) != 0
    }
    pub fn set_receive_list_count(&mut self, recv_list: u32) {
        self.raw[1] = (self.raw[1] & !(0xF << 10)) | ((recv_list & 0xF) << 10);
    }
    pub fn get_data(&self) -> &[u32; 2] {
        &self.raw
    }
}

/// IPC special header (1 word / 4 bytes).
#[derive(Debug, Clone, Copy, Default)]
pub struct SpecialHeader {
    raw: u32,
    has_header: bool,
}

impl SpecialHeader {
    pub fn new(pid: bool, copy: i32, mov: i32) -> Self {
        let raw = (pid as u32) | ((copy as u32 & 0xF) << 1) | ((mov as u32 & 0xF) << 5);
        Self {
            raw,
            has_header: true,
        }
    }

    pub fn new_with_flag(pid: bool, copy: i32, mov: i32, has_header: bool) -> Self {
        let raw = (pid as u32) | ((copy as u32 & 0xF) << 1) | ((mov as u32 & 0xF) << 5);
        Self { raw, has_header }
    }

    pub fn get_has_process_id(&self) -> bool {
        (self.raw & 1) != 0
    }
    pub fn get_copy_handle_count(&self) -> i32 {
        ((self.raw >> 1) & 0xF) as i32
    }
    pub fn get_move_handle_count(&self) -> i32 {
        ((self.raw >> 5) & 0xF) as i32
    }
    pub fn get_header_size(&self) -> usize {
        if self.has_header {
            4
        } else {
            0
        }
    }
    pub fn get_data_size(&self) -> usize {
        if self.has_header {
            (if self.get_has_process_id() { 8 } else { 0 })
                + (self.get_copy_handle_count() as usize * std::mem::size_of::<Handle>())
                + (self.get_move_handle_count() as usize * std::mem::size_of::<Handle>())
        } else {
            0
        }
    }
}

/// Map alias descriptor attribute.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MapAliasAttribute {
    Ipc = 0,
    NonSecureIpc = 1,
    NonDeviceIpc = 3,
}

/// Map alias descriptor (3 words / 12 bytes).
#[derive(Debug, Clone, Copy, Default)]
pub struct MapAliasDescriptor {
    raw: [u32; 3],
}

impl MapAliasDescriptor {
    pub const DATA_SIZE: usize = 12;

    pub fn from_raw(raw: [u32; 3]) -> Self {
        Self { raw }
    }

    pub fn get_address(&self) -> u64 {
        let addr_low = self.raw[1] as u64;
        let addr_mid = ((self.raw[2] >> 28) & 0xF) as u64;
        let addr_high = ((self.raw[2] >> 2) & 0x7) as u64;
        ((addr_high << 4 | addr_mid) << 32) | addr_low
    }

    pub fn get_size(&self) -> u64 {
        let size_low = self.raw[0] as u64;
        let size_high = ((self.raw[2] >> 24) & 0xF) as u64;
        (size_high << 32) | size_low
    }

    pub fn get_attribute(&self) -> u32 {
        self.raw[2] & 0x3
    }

    pub fn get_data(&self) -> &[u32; 3] {
        &self.raw
    }
}

/// Pointer descriptor (2 words / 8 bytes).
#[derive(Debug, Clone, Copy, Default)]
pub struct PointerDescriptor {
    raw: [u32; 2],
}

impl PointerDescriptor {
    pub const DATA_SIZE: usize = 8;

    pub fn from_raw(raw: [u32; 2]) -> Self {
        Self { raw }
    }

    pub fn get_index(&self) -> i32 {
        (self.raw[0] & 0xF) as i32
    }

    pub fn get_address(&self) -> u64 {
        let addr_low = self.raw[1] as u64;
        let addr_mid = ((self.raw[0] >> 12) & 0xF) as u64;
        let addr_high = ((self.raw[0] >> 6) & 0x7) as u64;
        ((addr_high << 4 | addr_mid) << 32) | addr_low
    }

    pub fn get_size(&self) -> usize {
        ((self.raw[0] >> 16) & 0xFFFF) as usize
    }

    pub fn get_data(&self) -> &[u32; 2] {
        &self.raw
    }
}

/// Receive list entry (2 words / 8 bytes).
#[derive(Debug, Clone, Copy, Default)]
pub struct ReceiveListEntry {
    raw: [u32; 2],
}

impl ReceiveListEntry {
    pub const DATA_SIZE: usize = 8;

    pub fn from_raw(a: u32, b: u32) -> Self {
        Self { raw: [a, b] }
    }

    pub fn get_address(&self) -> u64 {
        let addr_low = self.raw[0] as u64;
        let addr_high = (self.raw[1] & 0x7F) as u64;
        (addr_high << 32) | addr_low
    }

    pub fn get_size(&self) -> usize {
        ((self.raw[1] >> 16) & 0xFFFF) as usize
    }

    pub fn get_data(&self) -> &[u32; 2] {
        &self.raw
    }
}

/// IPC message buffer: a typed view over a u32 word array.
pub struct MessageBuffer<'a> {
    buffer: &'a mut [u32],
}

impl<'a> MessageBuffer<'a> {
    pub fn new(buffer: &'a mut [u32]) -> Self {
        Self { buffer }
    }

    pub fn get_buffer_size(&self) -> usize {
        self.buffer.len()
    }

    pub fn get(&self, index: usize, dst: &mut [u32]) {
        std::sync::atomic::fence(std::sync::atomic::Ordering::SeqCst);
        dst.copy_from_slice(&self.buffer[index..index + dst.len()]);
    }

    pub fn set(&mut self, index: usize, src: &[u32]) -> usize {
        std::sync::atomic::fence(std::sync::atomic::Ordering::SeqCst);
        self.buffer[index..index + src.len()].copy_from_slice(src);
        std::sync::atomic::fence(std::sync::atomic::Ordering::SeqCst);
        index + src.len()
    }

    pub fn get32(&self, index: usize) -> u32 {
        self.buffer[index]
    }

    pub fn get64(&self, index: usize) -> u64 {
        let lo = self.buffer[index] as u64;
        let hi = self.buffer[index + 1] as u64;
        lo | (hi << 32)
    }

    pub fn set_null(&mut self) {
        let hdr = MessageHeader::default();
        self.set_message_header(&hdr);
    }

    pub fn set_message_header(&mut self, hdr: &MessageHeader) -> usize {
        self.buffer[0] = hdr.raw[0];
        self.buffer[1] = hdr.raw[1];
        2
    }

    /// Compute the index of special data fields.
    pub fn get_special_data_index(spc: &SpecialHeader) -> usize {
        (MessageHeader::DATA_SIZE / 4) + (spc.get_header_size() / 4)
    }

    /// Compute the index of pointer descriptors.
    pub fn get_pointer_descriptor_index(hdr: &MessageHeader, spc: &SpecialHeader) -> usize {
        Self::get_special_data_index(spc) + (spc.get_data_size() / 4)
    }

    /// Compute the index of map alias descriptors.
    pub fn get_map_alias_descriptor_index(hdr: &MessageHeader, spc: &SpecialHeader) -> usize {
        Self::get_pointer_descriptor_index(hdr, spc)
            + (hdr.get_pointer_count() as usize * PointerDescriptor::DATA_SIZE / 4)
    }

    /// Compute the index of raw data.
    pub fn get_raw_data_index(hdr: &MessageHeader, spc: &SpecialHeader) -> usize {
        Self::get_map_alias_descriptor_index(hdr, spc)
            + (hdr.get_map_alias_count() as usize * MapAliasDescriptor::DATA_SIZE / 4)
    }

    /// Compute the index of the receive list.
    pub fn get_receive_list_index(hdr: &MessageHeader, spc: &SpecialHeader) -> usize {
        let offset = hdr.get_receive_list_offset();
        if offset != 0 {
            offset as usize
        } else {
            Self::get_raw_data_index(hdr, spc) + hdr.get_raw_count() as usize
        }
    }

    /// Compute the total message buffer size.
    pub fn get_message_buffer_size(hdr: &MessageHeader, spc: &SpecialHeader) -> usize {
        let mut msg_size = Self::get_receive_list_index(hdr, spc) * 4;

        let count = hdr.get_receive_list_count();
        match count {
            0 => {} // None
            1 => {} // ToMessageBuffer
            2 => {
                msg_size += ReceiveListEntry::DATA_SIZE;
            }
            _ => {
                msg_size += (count as usize - RECEIVE_LIST_COUNT_TYPE_COUNT_OFFSET as usize)
                    * ReceiveListEntry::DATA_SIZE;
            }
        }

        msg_size
    }
}
