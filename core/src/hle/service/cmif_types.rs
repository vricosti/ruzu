// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/cmif_types.h
//! Status: Structural port
//!
//! Contains:
//! - AutoOut: wrapper for output parameters
//! - Out: typed output parameter wrapper
//! - SharedPointer: alias for Arc<T>
//! - ClientProcessId / ProcessId: process identifier types
//! - InCopyHandle, OutCopyHandle, OutMoveHandle: handle wrapper types
//! - BufferAttr: buffer descriptor flags
//! - Buffer, InBuffer, OutBuffer, InArray, OutArray, InLargeData, OutLargeData

use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::ptr::{null, null_mut};
use std::slice;
use std::sync::Arc;

/// Wrapper for auto-generated output parameters.
///
/// Corresponds to upstream `AutoOut<T>`.
#[derive(Debug, Clone, Default)]
pub struct AutoOut<T: Default> {
    pub raw: T,
}

/// Typed output parameter.
///
/// Corresponds to upstream `Out<T>`.
pub struct Out<T> {
    raw: *mut T,
}

impl<T> Out<T> {
    pub fn new(raw: *mut T) -> Self {
        Self { raw }
    }

    pub fn from_ref(raw: &mut T) -> Self {
        Self { raw }
    }

    pub fn from_auto_out(raw: &mut AutoOut<T>) -> Self
    where
        T: Default,
    {
        Self { raw: &mut raw.raw }
    }

    pub fn get(&self) -> *mut T {
        self.raw
    }

    pub fn is_null(&self) -> bool {
        self.raw.is_null()
    }
}

impl<T> Default for Out<T> {
    fn default() -> Self {
        Self { raw: null_mut() }
    }
}

impl<T> Deref for Out<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        assert!(!self.raw.is_null());
        unsafe { &*self.raw }
    }
}

impl<T> DerefMut for Out<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        assert!(!self.raw.is_null());
        unsafe { &mut *self.raw }
    }
}

/// Shared pointer type alias, corresponding to upstream `SharedPointer<T>`.
pub type SharedPointer<T> = Arc<T>;

/// Output interface wrapper.
///
/// Upstream spells this as `using OutInterface = Out<SharedPointer<T>>;`.
/// Rust uses a dedicated wrapper type so the CMIF framework can classify it
/// without conflicting with the generic `Out<T>` trait impls.
pub struct OutInterface<T> {
    raw: *mut SharedPointer<T>,
}

impl<T> OutInterface<T> {
    pub fn new(raw: *mut SharedPointer<T>) -> Self {
        Self { raw }
    }

    pub fn from_ref(raw: &mut SharedPointer<T>) -> Self {
        Self { raw }
    }

    pub fn from_auto_out(raw: &mut AutoOut<SharedPointer<T>>) -> Self
    where
        SharedPointer<T>: Default,
    {
        Self { raw: &mut raw.raw }
    }

    pub fn get(&self) -> *mut SharedPointer<T> {
        self.raw
    }

    pub fn is_null(&self) -> bool {
        self.raw.is_null()
    }
}

impl<T> Default for OutInterface<T> {
    fn default() -> Self {
        Self { raw: null_mut() }
    }
}

impl<T> Deref for OutInterface<T> {
    type Target = SharedPointer<T>;

    fn deref(&self) -> &Self::Target {
        assert!(!self.raw.is_null());
        unsafe { &*self.raw }
    }
}

impl<T> DerefMut for OutInterface<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        assert!(!self.raw.is_null());
        unsafe { &mut *self.raw }
    }
}

/// Client process ID, passed via IPC.
///
/// Corresponds to upstream `ClientProcessId`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[repr(C)]
pub struct ClientProcessId {
    pub pid: u64,
}

impl ClientProcessId {
    pub fn is_valid(&self) -> bool {
        self.pid != 0
    }
}

/// Process ID with explicit construction.
///
/// Corresponds to upstream `ProcessId`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ProcessId {
    pub pid: u64,
}

impl ProcessId {
    pub fn new(pid: u64) -> Self {
        Self { pid }
    }

    pub fn from_client(client: &ClientProcessId) -> Self {
        Self { pid: client.pid }
    }

    pub fn is_valid(&self) -> bool {
        self.pid != 0
    }
}

pub type ClientAppletResourceUserId = ClientProcessId;
pub type AppletResourceUserId = ProcessId;

/// Input copy handle wrapper.
///
/// Corresponds to upstream `InCopyHandle<T>`.
pub struct InCopyHandle<T> {
    raw: *const T,
}

impl<T> InCopyHandle<T> {
    pub fn new(raw: *const T) -> Self {
        Self { raw }
    }

    pub fn get(&self) -> *const T {
        self.raw
    }

    pub fn is_valid(&self) -> bool {
        !self.raw.is_null()
    }
}

impl<T> Default for InCopyHandle<T> {
    fn default() -> Self {
        Self { raw: null() }
    }
}

impl<T> Deref for InCopyHandle<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        assert!(!self.raw.is_null());
        unsafe { &*self.raw }
    }
}

/// Output copy handle wrapper.
///
/// Corresponds to upstream `OutCopyHandle<T>`.
pub struct OutCopyHandle<T> {
    raw: *mut *mut T,
}

impl<T> OutCopyHandle<T> {
    pub fn new(raw: *mut *mut T) -> Self {
        Self { raw }
    }

    pub fn from_auto_out(raw: &mut AutoOut<*mut T>) -> Self {
        Self { raw: &mut raw.raw }
    }

    pub fn get(&self) -> *mut *mut T {
        self.raw
    }
}

impl<T> Default for OutCopyHandle<T> {
    fn default() -> Self {
        Self { raw: null_mut() }
    }
}

/// Output move handle wrapper.
///
/// Corresponds to upstream `OutMoveHandle<T>`.
pub struct OutMoveHandle<T> {
    raw: *mut *mut T,
}

impl<T> OutMoveHandle<T> {
    pub fn new(raw: *mut *mut T) -> Self {
        Self { raw }
    }

    pub fn from_auto_out(raw: &mut AutoOut<*mut T>) -> Self {
        Self { raw: &mut raw.raw }
    }

    pub fn get(&self) -> *mut *mut T {
        self.raw
    }
}

impl<T> Default for OutMoveHandle<T> {
    fn default() -> Self {
        Self { raw: null_mut() }
    }
}

/// Buffer attribute flags.
///
/// Corresponds to upstream `BufferAttr`.
#[allow(non_upper_case_globals)]
pub mod buffer_attr {
    pub const BufferAttr_In: i32 = 1 << 0;
    pub const BufferAttr_Out: i32 = 1 << 1;
    pub const BufferAttr_HipcMapAlias: i32 = 1 << 2;
    pub const BufferAttr_HipcPointer: i32 = 1 << 3;
    pub const BufferAttr_FixedSize: i32 = 1 << 4;
    pub const BufferAttr_HipcAutoSelect: i32 = 1 << 5;
    pub const BufferAttr_HipcMapTransferAllowsNonSecure: i32 = 1 << 6;
    pub const BufferAttr_HipcMapTransferAllowsNonDevice: i32 = 1 << 7;
}

/// Slice-backed CMIF buffer wrapper.
///
/// Corresponds to upstream `Buffer<T, A> : std::span<T>`.
pub struct Buffer<T, const A: i32> {
    ptr: *mut T,
    len: usize,
    _marker: PhantomData<T>,
}

impl<T, const A: i32> Buffer<T, A> {
    pub const ATTR: i32 = A;

    pub fn new(ptr: *mut T, len: usize) -> Self {
        Self {
            ptr,
            len,
            _marker: PhantomData,
        }
    }

    pub fn from_slice(slice: &mut [T]) -> Self {
        Self::new(slice.as_mut_ptr(), slice.len())
    }

    pub fn from_const_slice(slice: &[T]) -> Self {
        Self::new(slice.as_ptr() as *mut T, slice.len())
    }

    pub fn data(&self) -> *mut T {
        self.ptr
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn as_slice(&self) -> &[T] {
        unsafe { slice::from_raw_parts(self.ptr as *const T, self.len) }
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr, self.len) }
    }
}

impl<T, const A: i32> Default for Buffer<T, A> {
    fn default() -> Self {
        Self::new(null_mut(), 0)
    }
}

impl<T, const A: i32> Deref for Buffer<T, A> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T, const A: i32> DerefMut for Buffer<T, A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}

pub type InBuffer<const A: i32> = Buffer<u8, A>;
pub type InArray<T, const A: i32> = Buffer<T, A>;
pub type OutBuffer<const A: i32> = Buffer<u8, A>;
pub type OutArray<T, const A: i32> = Buffer<T, A>;

/// Fixed-size input data transported by CMIF buffers.
///
/// Corresponds to upstream `InLargeData<T, A>`.
pub struct InLargeData<T, const A: i32> {
    raw: *const T,
}

impl<T, const A: i32> InLargeData<T, A> {
    pub const ATTR: i32 = A | buffer_attr::BufferAttr_In | buffer_attr::BufferAttr_FixedSize;

    pub fn new(raw: *const T) -> Self {
        Self { raw }
    }

    pub fn from_ref(raw: &T) -> Self {
        Self { raw }
    }

    pub fn get(&self) -> *const T {
        self.raw
    }

    pub fn is_valid(&self) -> bool {
        !self.raw.is_null()
    }
}

impl<T, const A: i32> Deref for InLargeData<T, A> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        assert!(!self.raw.is_null());
        unsafe { &*self.raw }
    }
}

/// Fixed-size output data transported by CMIF buffers.
///
/// Corresponds to upstream `OutLargeData<T, A>`.
pub struct OutLargeData<T, const A: i32> {
    raw: *mut T,
}

impl<T, const A: i32> OutLargeData<T, A> {
    pub const ATTR: i32 = A | buffer_attr::BufferAttr_Out | buffer_attr::BufferAttr_FixedSize;

    pub fn new(raw: *mut T) -> Self {
        Self { raw }
    }

    pub fn from_ref(raw: &mut T) -> Self {
        Self { raw }
    }

    pub fn from_auto_out(raw: &mut AutoOut<T>) -> Self
    where
        T: Default,
    {
        Self { raw: &mut raw.raw }
    }

    pub fn get(&self) -> *mut T {
        self.raw
    }
}

impl<T, const A: i32> Default for OutLargeData<T, A> {
    fn default() -> Self {
        Self { raw: null_mut() }
    }
}

impl<T, const A: i32> Deref for OutLargeData<T, A> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        assert!(!self.raw.is_null());
        unsafe { &*self.raw }
    }
}

impl<T, const A: i32> DerefMut for OutLargeData<T, A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        assert!(!self.raw.is_null());
        unsafe { &mut *self.raw }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_client_process_id() {
        let pid = ClientProcessId { pid: 42 };
        assert!(pid.is_valid());
        let zero = ClientProcessId { pid: 0 };
        assert!(!zero.is_valid());
    }

    #[test]
    fn test_process_id() {
        let pid = ProcessId::new(100);
        assert!(pid.is_valid());
        let client = ClientProcessId { pid: 200 };
        let from_client = ProcessId::from_client(&client);
        assert_eq!(from_client.pid, 200);
    }

    #[test]
    fn test_buffer_attr_flags() {
        use buffer_attr::*;
        assert_eq!(BufferAttr_In, 0x01);
        assert_eq!(BufferAttr_Out, 0x02);
        assert_eq!(BufferAttr_HipcMapAlias, 0x04);
        assert_eq!(BufferAttr_HipcPointer, 0x08);
        assert_eq!(BufferAttr_FixedSize, 0x10);
        assert_eq!(BufferAttr_HipcAutoSelect, 0x20);
        assert_eq!(BufferAttr_HipcMapTransferAllowsNonSecure, 0x40);
        assert_eq!(BufferAttr_HipcMapTransferAllowsNonDevice, 0x80);
    }

    #[test]
    fn test_out_wrapper_points_to_backing_storage() {
        let mut value = 7u32;
        let mut out = Out::from_ref(&mut value);
        *out = 9;
        assert_eq!(value, 9);
    }

    #[test]
    fn test_buffer_wrapper_exposes_slice() {
        let mut values = [1u32, 2, 3];
        let mut buffer = OutArray::<
            u32,
            { buffer_attr::BufferAttr_Out | buffer_attr::BufferAttr_HipcMapAlias },
        >::from_slice(&mut values);
        buffer[1] = 5;
        assert_eq!(values[1], 5);
        assert_eq!(buffer.len(), 3);
    }

    #[test]
    fn test_large_data_wrapper_points_to_backing_storage() {
        let mut out_storage = 0x1234u32;
        let mut out = OutLargeData::<u32, { buffer_attr::BufferAttr_HipcMapAlias }>::from_ref(
            &mut out_storage,
        );
        *out = 0x5678;
        assert_eq!(out_storage, 0x5678);

        let in_storage = 0x9abcu32;
        let input =
            InLargeData::<u32, { buffer_attr::BufferAttr_HipcMapAlias }>::from_ref(&in_storage);
        assert_eq!(*input, 0x9abc);
    }
}
