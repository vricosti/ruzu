// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/cmif_types.h
//! Status: Structural port
//!
//! Contains:
//! - AutoOut: wrapper for output parameters
//! - Out: typed output parameter pointer wrapper
//! - SharedPointer: alias for Arc<T>
//! - ClientProcessId / ProcessId: process identifier types
//! - InCopyHandle, OutCopyHandle, OutMoveHandle: handle wrapper types
//! - BufferAttr: buffer attribute flags
//! - Buffer, InBuffer, OutBuffer, InLargeData, OutLargeData: buffer descriptor types

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
/// In Rust this is represented as a mutable reference or Option wrapper rather than a raw pointer.
pub struct Out<T> {
    value: Option<T>,
}

impl<T> Out<T> {
    pub fn new() -> Self {
        Self { value: None }
    }

    pub fn set(&mut self, val: T) {
        self.value = Some(val);
    }

    pub fn get(&self) -> Option<&T> {
        self.value.as_ref()
    }

    pub fn take(self) -> Option<T> {
        self.value
    }
}

impl<T: Default> Default for Out<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Shared pointer type alias, corresponding to upstream `SharedPointer<T>`.
pub type SharedPointer<T> = Arc<T>;

/// Output interface type alias.
pub type OutInterface<T> = Out<SharedPointer<T>>;

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

/// Alias matching upstream.
pub type ClientAppletResourceUserId = ClientProcessId;

/// Alias matching upstream.
pub type AppletResourceUserId = ProcessId;

/// Input copy handle wrapper.
///
/// Corresponds to upstream `InCopyHandle<T>`.
/// In Rust, we use a simple handle value rather than a raw pointer.
#[derive(Debug, Clone, Copy, Default)]
pub struct InCopyHandle {
    pub handle: u32,
}

impl InCopyHandle {
    pub fn is_valid(&self) -> bool {
        self.handle != 0
    }
}

/// Output copy handle wrapper.
///
/// Corresponds to upstream `OutCopyHandle<T>`.
#[derive(Debug, Clone, Default)]
pub struct OutCopyHandle {
    pub handle: Option<u32>,
}

/// Output move handle wrapper.
///
/// Corresponds to upstream `OutMoveHandle<T>`.
#[derive(Debug, Clone, Default)]
pub struct OutMoveHandle {
    pub handle: Option<u32>,
}

/// Buffer attribute flags.
///
/// Corresponds to upstream `BufferAttr` enum.
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
}
