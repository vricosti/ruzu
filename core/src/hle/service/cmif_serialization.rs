// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/cmif_serialization.h
//! Status: Structural partial port
//!
//! The upstream file owns:
//! - argument unwrapping
//! - argument classification
//! - request/reply layout computation
//! - argument read/write recursion
//! - `CmifReplyWrapImpl(...)`
//!
//! Rust still does not have the full template-generated wrapper machinery, but
//! this file now owns the argument-type and layout logic instead of leaving it
//! implicit in service owners.

use crate::hle::result::ResultCode;
use crate::hle::service::cmif_types::{
    buffer_attr, Buffer, ClientProcessId, InCopyHandle, InLargeData, Out, OutCopyHandle,
    OutInterface, OutLargeData, OutMoveHandle, SharedPointer,
};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandlerPtr};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};

/// Upstream `ArgumentType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgumentType {
    InProcessId,
    InData,
    InInterface,
    InCopyHandle,
    OutData,
    OutInterface,
    OutCopyHandle,
    OutMoveHandle,
    InBuffer,
    InLargeData,
    OutBuffer,
    OutLargeData,
}

/// Upstream `RequestLayout`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct RequestLayout {
    pub copy_handle_count: u32,
    pub move_handle_count: u32,
    pub cmif_raw_data_size: u32,
    pub domain_interface_count: u32,
}

/// Runtime argument layout descriptor used to reproduce the upstream CMIF
/// request/reply layout algorithms in Rust.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArgumentDescriptor {
    pub argument_type: ArgumentType,
    pub size: usize,
    pub align: usize,
}

impl ArgumentDescriptor {
    pub const fn new(argument_type: ArgumentType, size: usize, align: usize) -> Self {
        Self {
            argument_type,
            size,
            align,
        }
    }
}

/// Rust counterpart to upstream `UnwrapArg<T>`.
pub trait UnwrapArg {
    type Type;
}

impl<T> UnwrapArg for T {
    type Type = T;
}

/// Rust counterpart to upstream `ArgumentTraits<T>`.
pub trait ArgumentTraits {
    const TYPE: ArgumentType;
}

impl ArgumentTraits for ClientProcessId {
    const TYPE: ArgumentType = ArgumentType::InProcessId;
}

impl<T> ArgumentTraits for SharedPointer<T> {
    const TYPE: ArgumentType = ArgumentType::InInterface;
}

impl<T> ArgumentTraits for InCopyHandle<T> {
    const TYPE: ArgumentType = ArgumentType::InCopyHandle;
}

impl<T> ArgumentTraits for OutInterface<T> {
    const TYPE: ArgumentType = ArgumentType::OutInterface;
}

impl<T> ArgumentTraits for Out<T> {
    const TYPE: ArgumentType = ArgumentType::OutData;
}

impl<T> ArgumentTraits for OutCopyHandle<T> {
    const TYPE: ArgumentType = ArgumentType::OutCopyHandle;
}

impl<T> ArgumentTraits for OutMoveHandle<T> {
    const TYPE: ArgumentType = ArgumentType::OutMoveHandle;
}

impl<T, const A: i32> ArgumentTraits for Buffer<T, A> {
    const TYPE: ArgumentType = if (A & buffer_attr::BufferAttr_In) == 0 {
        ArgumentType::OutBuffer
    } else {
        ArgumentType::InBuffer
    };
}

impl<T, const A: i32> ArgumentTraits for InLargeData<T, A> {
    const TYPE: ArgumentType = ArgumentType::InLargeData;
}

impl<T, const A: i32> ArgumentTraits for OutLargeData<T, A> {
    const TYPE: ArgumentType = ArgumentType::OutLargeData;
}

const fn align_up(value: usize, align: usize) -> usize {
    if align <= 1 {
        value
    } else {
        (value + (align - 1)) & !(align - 1)
    }
}

pub const fn get_argument_type_count(
    argument_type: ArgumentType,
    descriptors: &[ArgumentDescriptor],
) -> u32 {
    let mut count = 0;
    let mut index = 0;
    while index < descriptors.len() {
        if descriptors[index].argument_type as u8 == argument_type as u8 {
            count += 1;
        }
        index += 1;
    }
    count
}

pub const fn get_in_raw_data_size(descriptors: &[ArgumentDescriptor]) -> u32 {
    let mut data_offset = 0usize;
    let mut prev_align = 1usize;
    let mut index = 0usize;
    while index < descriptors.len() {
        let descriptor = descriptors[index];
        match descriptor.argument_type {
            ArgumentType::InData | ArgumentType::InProcessId => {
                assert!(prev_align <= descriptor.align);
                let arg_offset = align_up(data_offset, descriptor.align);
                data_offset = arg_offset + descriptor.size;
                prev_align = descriptor.align;
            }
            _ => {}
        }
        index += 1;
    }
    data_offset as u32
}

pub const fn get_out_raw_data_size(descriptors: &[ArgumentDescriptor]) -> u32 {
    let mut data_offset = 0usize;
    let mut prev_align = 1usize;
    let mut index = 0usize;
    while index < descriptors.len() {
        let descriptor = descriptors[index];
        if matches!(descriptor.argument_type, ArgumentType::OutData) {
            assert!(prev_align <= descriptor.align);
            let arg_offset = align_up(data_offset, descriptor.align);
            data_offset = arg_offset + descriptor.size;
            prev_align = descriptor.align;
        }
        index += 1;
    }
    data_offset as u32
}

pub const fn get_non_domain_reply_in_layout(descriptors: &[ArgumentDescriptor]) -> RequestLayout {
    RequestLayout {
        copy_handle_count: get_argument_type_count(ArgumentType::InCopyHandle, descriptors),
        move_handle_count: 0,
        cmif_raw_data_size: get_in_raw_data_size(descriptors),
        domain_interface_count: 0,
    }
}

pub const fn get_domain_reply_in_layout(descriptors: &[ArgumentDescriptor]) -> RequestLayout {
    RequestLayout {
        copy_handle_count: get_argument_type_count(ArgumentType::InCopyHandle, descriptors),
        move_handle_count: 0,
        cmif_raw_data_size: get_in_raw_data_size(descriptors),
        domain_interface_count: get_argument_type_count(ArgumentType::InInterface, descriptors),
    }
}

pub const fn get_non_domain_reply_out_layout(descriptors: &[ArgumentDescriptor]) -> RequestLayout {
    RequestLayout {
        copy_handle_count: get_argument_type_count(ArgumentType::OutCopyHandle, descriptors),
        move_handle_count: get_argument_type_count(ArgumentType::OutMoveHandle, descriptors)
            + get_argument_type_count(ArgumentType::OutInterface, descriptors),
        cmif_raw_data_size: get_out_raw_data_size(descriptors),
        domain_interface_count: 0,
    }
}

pub const fn get_domain_reply_out_layout(descriptors: &[ArgumentDescriptor]) -> RequestLayout {
    RequestLayout {
        copy_handle_count: get_argument_type_count(ArgumentType::OutCopyHandle, descriptors),
        move_handle_count: get_argument_type_count(ArgumentType::OutMoveHandle, descriptors),
        cmif_raw_data_size: get_out_raw_data_size(descriptors),
        domain_interface_count: get_argument_type_count(ArgumentType::OutInterface, descriptors),
    }
}

pub const fn get_reply_in_layout(
    is_domain: bool,
    descriptors: &[ArgumentDescriptor],
) -> RequestLayout {
    if is_domain {
        get_domain_reply_in_layout(descriptors)
    } else {
        get_non_domain_reply_in_layout(descriptors)
    }
}

pub const fn get_reply_out_layout(
    is_domain: bool,
    descriptors: &[ArgumentDescriptor],
) -> RequestLayout {
    if is_domain {
        get_domain_reply_out_layout(descriptors)
    } else {
        get_non_domain_reply_out_layout(descriptors)
    }
}

/// Minimal CMIF input wrapper for scalar/raw request arguments.
pub struct CmifRequest<'a> {
    parser: RequestParser<'a>,
}

impl<'a> CmifRequest<'a> {
    pub fn new(ctx: &'a mut HLERequestContext) -> Self {
        Self {
            parser: RequestParser::new(ctx),
        }
    }

    pub fn align_for<T>(&mut self) {
        self.parser.align_for::<T>();
    }

    pub fn raw<T: Copy + Default>(&mut self) -> T {
        self.parser.pop_raw::<T>()
    }

    pub fn u32(&mut self) -> u32 {
        self.parser.pop_u32()
    }

    pub fn u64(&mut self) -> u64 {
        self.parser.pop_u64()
    }

    pub fn f32(&mut self) -> f32 {
        self.parser.pop_f32()
    }
}

/// Minimal CMIF output wrapper for `Result + Out<T>` style responses.
pub struct CmifResponse<'a> {
    builder: ResponseBuilder<'a>,
}

impl<'a> CmifResponse<'a> {
    pub fn new(
        ctx: &'a mut HLERequestContext,
        normal_params_size: u32,
        num_handles_to_copy: u32,
        num_objects_to_move: u32,
    ) -> Self {
        let builder = ResponseBuilder::new(
            ctx,
            normal_params_size,
            num_handles_to_copy,
            num_objects_to_move,
        );
        Self { builder }
    }

    pub fn result_only(ctx: &'a mut HLERequestContext, result: ResultCode) -> Self {
        let mut response = Self::new(ctx, 2, 0, 0);
        response.push_result(result);
        response
    }

    pub fn push_result(&mut self, value: ResultCode) {
        self.builder.push_result(value);
    }

    pub fn push_raw<T: Copy>(&mut self, value: &T) {
        self.builder.push_raw(value);
    }

    pub fn push_bool(&mut self, value: bool) {
        self.builder.push_bool(value);
    }

    pub fn push_u32(&mut self, value: u32) {
        self.builder.push_u32(value);
    }

    pub fn push_i32(&mut self, value: i32) {
        self.builder.push_i32(value);
    }

    pub fn push_u64(&mut self, value: u64) {
        self.builder.push_u64(value);
    }

    pub fn push_f32(&mut self, value: f32) {
        self.builder.push_f32(value);
    }

    pub fn push_copy_objects(&mut self, handle: u32) {
        self.builder.push_copy_objects(handle);
    }

    pub fn push_copy_object_id(&mut self, object_id: u64) {
        self.builder.push_copy_object_id(object_id);
    }

    pub fn push_move_objects(&mut self, handle: u32) {
        self.builder.push_move_objects(handle);
    }

    pub fn push_interface(&mut self, object: SessionRequestHandlerPtr) {
        self.builder.push_ipc_interface(object);
    }
}

/// Writes an `OutArray<T>` / `OutLargeData<T>` style payload to a HIPC write buffer.
pub fn write_out_array_bytes(ctx: &mut HLERequestContext, index: usize, bytes: &[u8]) {
    if !bytes.is_empty() {
        ctx.write_buffer(bytes, index);
    }
}

/// Typed backing storage for an upstream `InArray<T, A>` reconstructed from a
/// copied HIPC buffer.
pub struct CmifInArrayBuffer<T: Copy + Default, const A: i32> {
    storage: Vec<T>,
}

impl<T: Copy + Default, const A: i32> CmifInArrayBuffer<T, A> {
    pub fn from_ctx(ctx: &HLERequestContext, index: usize) -> Self {
        let element_size = std::mem::size_of::<T>();
        if element_size == 0 || !ctx.can_read_buffer(index) {
            return Self {
                storage: Vec::new(),
            };
        }

        let bytes = ctx.read_buffer(index);
        let count = bytes.len() / element_size;
        let mut storage = Vec::with_capacity(count);
        for i in 0..count {
            let start = i * element_size;
            let value = unsafe { std::ptr::read_unaligned(bytes[start..].as_ptr() as *const T) };
            storage.push(value);
        }

        Self { storage }
    }

    pub fn as_in_array(&mut self) -> Buffer<T, A> {
        Buffer::from_slice(self.storage.as_mut_slice())
    }
}

/// Typed backing storage for an upstream `OutArray<T, A>` reconstructed from a
/// HIPC write-buffer shape.
pub struct CmifOutArrayBuffer<T: Copy + Default, const A: i32> {
    storage: Vec<T>,
}

impl<T: Copy + Default, const A: i32> CmifOutArrayBuffer<T, A> {
    pub fn from_ctx(ctx: &HLERequestContext, index: usize) -> Self {
        let element_size = std::mem::size_of::<T>();
        let count = if element_size == 0 {
            0
        } else {
            ctx.get_write_buffer_size(index) / element_size
        };
        Self {
            storage: vec![T::default(); count],
        }
    }

    pub fn as_out_array(&mut self) -> Buffer<T, A> {
        Buffer::from_slice(self.storage.as_mut_slice())
    }

    pub fn write_back(&self, ctx: &HLERequestContext, index: usize, _count: usize) {
        if self.storage.is_empty() {
            return;
        }

        let byte_len = self.storage.len() * std::mem::size_of::<T>();
        let bytes =
            unsafe { std::slice::from_raw_parts(self.storage.as_ptr() as *const u8, byte_len) };
        ctx.write_buffer(bytes, index);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_request_layout_default() {
        let layout = RequestLayout::default();
        assert_eq!(layout.copy_handle_count, 0);
        assert_eq!(layout.move_handle_count, 0);
        assert_eq!(layout.cmif_raw_data_size, 0);
        assert_eq!(layout.domain_interface_count, 0);
    }

    #[test]
    fn test_get_in_raw_data_size_matches_upstream_alignment_rule() {
        let descriptors = [
            ArgumentDescriptor::new(ArgumentType::InData, 0x34, 4),
            ArgumentDescriptor::new(ArgumentType::InData, 8, 8),
            ArgumentDescriptor::new(ArgumentType::InCopyHandle, 0, 1),
        ];
        assert_eq!(get_in_raw_data_size(&descriptors), 0x40);
    }

    #[test]
    fn test_get_out_raw_data_size_counts_only_out_data() {
        let descriptors = [
            ArgumentDescriptor::new(ArgumentType::OutInterface, 0, 4),
            ArgumentDescriptor::new(ArgumentType::OutData, 4, 4),
            ArgumentDescriptor::new(ArgumentType::OutData, 8, 8),
        ];
        assert_eq!(get_out_raw_data_size(&descriptors), 16);
    }

    #[test]
    fn test_reply_out_layout_matches_upstream_counts() {
        let descriptors = [
            ArgumentDescriptor::new(ArgumentType::OutCopyHandle, 0, 1),
            ArgumentDescriptor::new(ArgumentType::OutMoveHandle, 0, 1),
            ArgumentDescriptor::new(ArgumentType::OutInterface, 0, 4),
            ArgumentDescriptor::new(ArgumentType::OutData, 4, 4),
        ];
        let non_domain = get_reply_out_layout(false, &descriptors);
        assert_eq!(non_domain.copy_handle_count, 1);
        assert_eq!(non_domain.move_handle_count, 2);
        assert_eq!(non_domain.domain_interface_count, 0);
        assert_eq!(non_domain.cmif_raw_data_size, 4);

        let domain = get_reply_out_layout(true, &descriptors);
        assert_eq!(domain.copy_handle_count, 1);
        assert_eq!(domain.move_handle_count, 1);
        assert_eq!(domain.domain_interface_count, 1);
    }
}
