// SPDX-FileCopyrightText: 2016 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ipc_helpers.h
//! Status: Structural port
//!
//! Contains:
//! - ResultSessionClosed: IPC result code for closed sessions
//! - RequestHelperBase: base for request parsing/building
//! - ResponseBuilder: builds IPC responses
//! - RequestParser: parses IPC requests

use crate::hle::ipc;
use crate::hle::result::{ErrorModule, ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::HLERequestContext;

/// Result code indicating a session has been closed.
pub const RESULT_SESSION_CLOSED: ResultCode =
    ResultCode::from_module_description(ErrorModule::HIPC, 301);

/// Flags used for customizing the behavior of ResponseBuilder.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ResponseBuilderFlags {
    None = 0,
    /// Uses move handles to move objects in the response, even when in a domain.
    AlwaysMoveHandles = 1,
}

/// Builds IPC response messages.
///
/// Corresponds to upstream `IPC::ResponseBuilder`.
pub struct ResponseBuilder<'a> {
    context: &'a mut HLERequestContext,
    index: usize,
    normal_params_size: u32,
    num_handles_to_copy: u32,
    num_objects_to_move: u32,
    data_payload_index: usize,
}

impl<'a> ResponseBuilder<'a> {
    /// Creates a new ResponseBuilder.
    ///
    /// # Arguments
    /// * `ctx` - The HLE request context.
    /// * `normal_params_size` - Number of normal parameter words.
    /// * `num_handles_to_copy` - Number of handles to copy.
    /// * `num_objects_to_move` - Number of objects to move (domain objects or move handles).
    pub fn new(
        ctx: &'a mut HLERequestContext,
        normal_params_size: u32,
        num_handles_to_copy: u32,
        num_objects_to_move: u32,
    ) -> Self {
        Self::new_with_flags(
            ctx,
            normal_params_size,
            num_handles_to_copy,
            num_objects_to_move,
            ResponseBuilderFlags::None,
        )
    }

    /// Creates a new ResponseBuilder with flags.
    pub fn new_with_flags(
        ctx: &'a mut HLERequestContext,
        normal_params_size: u32,
        num_handles_to_copy: u32,
        num_objects_to_move: u32,
        flags: ResponseBuilderFlags,
    ) -> Self {
        // Zero the command buffer.
        for i in 0..ipc::COMMAND_BUFFER_LENGTH {
            ctx.cmd_buf[i] = 0;
        }

        let is_tipc = ctx.is_tipc();
        let is_domain = ctx
            .get_manager()
            .map_or(false, |m| m.lock().unwrap().is_domain());

        let mut raw_data_size = if is_tipc {
            normal_params_size.wrapping_sub(1)
        } else {
            normal_params_size
        };
        ctx.write_size = raw_data_size;

        let always_move =
            (flags as u32 & ResponseBuilderFlags::AlwaysMoveHandles as u32) != 0;

        let num_handles_to_move;
        let num_domain_objects;
        if !is_domain || always_move {
            num_handles_to_move = num_objects_to_move;
            num_domain_objects = 0u32;
        } else {
            num_handles_to_move = 0;
            num_domain_objects = num_objects_to_move;
        }

        if is_domain {
            raw_data_size += (core::mem::size_of::<ipc::DomainMessageHeader>() as u32)
                / (core::mem::size_of::<u32>() as u32)
                + num_domain_objects;
            ctx.write_size += num_domain_objects;
        }

        if !is_tipc {
            raw_data_size += (core::mem::size_of::<ipc::DataPayloadHeader>() as u32)
                / (core::mem::size_of::<u32>() as u32)
                + 4
                + normal_params_size;
        }

        let mut index = 0usize;

        // Build CommandHeader.
        let mut header = ipc::CommandHeader {
            raw_low: 0,
            raw_high: 0,
        };

        if is_tipc {
            // Assign the original command type back.
            let raw_type = ctx.get_command_type() as u32;
            header.raw_low = assign_bits(header.raw_low, raw_type, 0, 16);
        }

        // data_size in bits [0, 10) of raw_high.
        header.raw_high = assign_bits(header.raw_high, raw_data_size, 0, 10);

        if num_handles_to_copy > 0 || num_handles_to_move > 0 {
            // enable_handle_descriptor at bit 31 of raw_high.
            header.raw_high |= 1 << 31;
        }

        ctx.cmd_buf[index] = header.raw_low;
        ctx.cmd_buf[index + 1] = header.raw_high;
        index += 2;

        if header.enable_handle_descriptor() {
            let mut hdh_raw = 0u32;
            hdh_raw = assign_bits(hdh_raw, num_handles_to_copy, 1, 4);
            hdh_raw = assign_bits(hdh_raw, num_handles_to_move, 5, 4);
            ctx.cmd_buf[index] = hdh_raw;
            index += 1;

            ctx.handles_offset = index as u32;

            // Skip space for handles.
            let total_handles = (num_handles_to_copy + num_handles_to_move) as usize;
            index += total_handles;
        }

        if !is_tipc {
            // Align to 16 bytes (4 words).
            if index & 3 != 0 {
                let padding = 4 - (index & 3);
                index += padding;
            }

            // Domain message header.
            if is_domain && ctx.has_domain_message_header() {
                let mut domain_header = ipc::DomainMessageHeader::default();
                domain_header.set_num_objects(num_domain_objects);
                ctx.cmd_buf[index] = domain_header.raw[0];
                ctx.cmd_buf[index + 1] = domain_header.raw[1];
                ctx.cmd_buf[index + 2] = domain_header.raw[2];
                ctx.cmd_buf[index + 3] = domain_header.raw[3];
                index += 4;
            }

            // Data payload header.
            // SFCO magic = 'S','F','C','O'
            let sfco_magic = u32::from_le_bytes([b'S', b'F', b'C', b'O']);
            ctx.cmd_buf[index] = sfco_magic;
            ctx.cmd_buf[index + 1] = 0; // padding
            index += 2;
        }

        let data_payload_index = index;
        ctx.data_payload_offset = index as u32;
        ctx.write_size += index as u32;
        ctx.domain_offset = (index as u32) + raw_data_size / (core::mem::size_of::<u32>() as u32);

        Self {
            context: ctx,
            index,
            normal_params_size,
            num_handles_to_copy,
            num_objects_to_move,
            data_payload_index,
        }
    }

    /// Push a ResultCode value. Result codes are 64-bit in the IPC buffer (high part is discarded).
    pub fn push_result(&mut self, value: ResultCode) {
        self.push_u32(value.get_inner_value());
        self.push_u32(0);
    }

    /// Push a copy handle into the outgoing copy objects list.
    ///
    /// Matches upstream `ResponseBuilder::PushCopyObjects(O* ptr)`.
    /// Upstream stores the KAutoObject* pointer; handle creation is deferred
    /// to WriteToOutgoingCommandBuffer() where handle_table.Add() is called.
    /// We store a KAutoObjectRef; handle resolution happens in WriteToOutgoing.
    pub fn push_copy_objects(&mut self, handle: u32) {
        use super::hle_ipc::KAutoObjectRef;
        self.context.outgoing_copy_objects.push(KAutoObjectRef::Handle(handle));
    }

    /// Push a move handle into the outgoing move objects list.
    ///
    /// Matches upstream `ResponseBuilder::PushMoveObjects(O* ptr)`.
    /// Upstream stores the KAutoObject* pointer and calls object->Close()
    /// after handle_table.Add() in WriteToOutgoingCommandBuffer().
    pub fn push_move_objects(&mut self, handle: u32) {
        use super::hle_ipc::KAutoObjectRef;
        self.context.outgoing_move_objects.push(KAutoObjectRef::Handle(handle));
    }

    /// Push a u32 value.
    pub fn push_u32(&mut self, value: u32) {
        if self.index < ipc::COMMAND_BUFFER_LENGTH {
            self.context.cmd_buf[self.index] = value;
            self.index += 1;
        }
    }

    /// Push a u64 value (little-endian, low word first).
    pub fn push_u64(&mut self, value: u64) {
        self.push_u32(value as u32);
        self.push_u32((value >> 32) as u32);
    }

    /// Push a u16 value (occupies one word, upper bits zeroed).
    pub fn push_u16(&mut self, value: u16) {
        self.push_raw_bytes(&value.to_le_bytes());
    }

    /// Push a u8 value.
    pub fn push_u8(&mut self, value: u8) {
        self.push_raw_bytes(&[value]);
    }

    /// Push a bool value (as u8).
    pub fn push_bool(&mut self, value: bool) {
        self.push_u8(value as u8);
    }

    /// Push a f32 value.
    pub fn push_f32(&mut self, value: f32) {
        self.push_u32(value.to_bits());
    }

    /// Push a i32 value.
    pub fn push_i32(&mut self, value: i32) {
        self.push_u32(value as u32);
    }

    /// Push a i64 value.
    pub fn push_i64(&mut self, value: i64) {
        self.push_u64(value as u64);
    }

    /// Push raw bytes, advancing the index by ceil(len/4) words.
    pub fn push_raw_bytes(&mut self, data: &[u8]) {
        if data.is_empty() {
            return;
        }
        let start = self.index;
        let words_needed = (data.len() + 3) / 4;
        if start + words_needed > ipc::COMMAND_BUFFER_LENGTH {
            return;
        }
        // Zero the target words first.
        for i in 0..words_needed {
            self.context.cmd_buf[start + i] = 0;
        }
        // Copy byte-by-byte into the command buffer.
        let buf_bytes: &mut [u8] = unsafe {
            core::slice::from_raw_parts_mut(
                self.context.cmd_buf[start..].as_mut_ptr() as *mut u8,
                words_needed * 4,
            )
        };
        buf_bytes[..data.len()].copy_from_slice(data);
        self.index += words_needed;
    }

    /// Push a trivially-copyable struct as raw data.
    pub fn push_raw<T: Copy>(&mut self, value: &T) {
        let bytes = unsafe {
            core::slice::from_raw_parts(value as *const T as *const u8, core::mem::size_of::<T>())
        };
        self.push_raw_bytes(bytes);
    }

    /// Returns the current write offset (in words) into the command buffer.
    pub fn get_current_offset(&self) -> usize {
        self.index
    }
}

/// Parses IPC request messages.
///
/// Corresponds to upstream `IPC::RequestParser`.
pub struct RequestParser<'a> {
    context: &'a HLERequestContext,
    index: usize,
}

impl<'a> RequestParser<'a> {
    /// Creates a new RequestParser from an HLERequestContext.
    pub fn new(ctx: &'a HLERequestContext) -> Self {
        let mut index = 0usize;

        // TIPC does not have data payload offset.
        if !ctx.is_tipc() {
            let offset = ctx.get_data_payload_offset() as usize;
            index = offset;
        }

        // Skip the u64 command id (2 words).
        index += 2;

        Self {
            context: ctx,
            index,
        }
    }

    /// Creates a new RequestParser from a raw command buffer.
    pub fn from_buffer(ctx: &'a HLERequestContext) -> Self {
        Self { context: ctx, index: 0 }
    }

    /// Pop a u32 value.
    pub fn pop_u32(&mut self) -> u32 {
        let value = self.context.cmd_buf[self.index];
        self.index += 1;
        value
    }

    /// Pop a u64 value.
    pub fn pop_u64(&mut self) -> u64 {
        let lsw = self.pop_u32() as u64;
        let msw = self.pop_u32() as u64;
        (msw << 32) | lsw
    }

    /// Pop a u16 value.
    pub fn pop_u16(&mut self) -> u16 {
        self.pop_raw::<u16>()
    }

    /// Pop a u8 value.
    pub fn pop_u8(&mut self) -> u8 {
        self.pop_raw::<u8>()
    }

    /// Pop a bool value.
    pub fn pop_bool(&mut self) -> bool {
        self.pop_u8() != 0
    }

    /// Pop a i32 value.
    pub fn pop_i32(&mut self) -> i32 {
        self.pop_u32() as i32
    }

    /// Pop a i64 value.
    pub fn pop_i64(&mut self) -> i64 {
        self.pop_u64() as i64
    }

    /// Pop a f32 value.
    pub fn pop_f32(&mut self) -> f32 {
        let bits = self.pop_u32();
        f32::from_bits(bits)
    }

    /// Pop a f64 value.
    pub fn pop_f64(&mut self) -> f64 {
        let bits = self.pop_u64();
        f64::from_bits(bits)
    }

    /// Pop a Result value.
    pub fn pop_result(&mut self) -> ResultCode {
        ResultCode::new(self.pop_u32())
    }

    /// Pop a raw trivially-copyable struct.
    pub fn pop_raw<T: Copy + Default>(&mut self) -> T {
        let size = core::mem::size_of::<T>();
        let words = (size + 3) / 4;
        let mut value = T::default();
        if self.index + words <= ipc::COMMAND_BUFFER_LENGTH {
            unsafe {
                let src = self.context.cmd_buf[self.index..].as_ptr() as *const u8;
                let dst = &mut value as *mut T as *mut u8;
                core::ptr::copy_nonoverlapping(src, dst, size);
            }
        }
        self.index += words;
        value
    }

    /// Skip a number of words.
    pub fn skip(&mut self, count: usize) {
        self.index += count;
    }

    /// Get the current offset.
    pub fn get_current_offset(&self) -> usize {
        self.index
    }

    /// Set the current offset.
    pub fn set_current_offset(&mut self, offset: usize) {
        self.index = offset;
    }

    /// Align the current position forward to a 16-byte boundary.
    pub fn align_with_padding(&mut self) {
        if self.index & 3 != 0 {
            self.index += 4 - (self.index & 3);
        }
    }
}

/// Assign bits into a u32 value at position with given width.
fn assign_bits(value: u32, field: u32, position: usize, bits: usize) -> u32 {
    let mask = ((1u32 << bits) - 1) << position;
    (value & !mask) | ((field << position) & mask)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_result_session_closed() {
        assert!(RESULT_SESSION_CLOSED.is_error());
        assert_eq!(RESULT_SESSION_CLOSED.get_module(), ErrorModule::HIPC);
        assert_eq!(RESULT_SESSION_CLOSED.get_description(), 301);
    }

    #[test]
    fn test_assign_bits() {
        let value = 0u32;
        let result = assign_bits(value, 5, 4, 4);
        assert_eq!(result, 5 << 4);
    }

    #[test]
    fn test_response_builder_basic() {
        let mut ctx = HLERequestContext::new();
        {
            let mut rb = ResponseBuilder::new(&mut ctx, 2, 0, 0);
            rb.push_result(RESULT_SUCCESS);
        }
        // The command buffer should have been written to.
        // Basic smoke test that it doesn't panic.
    }
}
