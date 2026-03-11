// SPDX-FileCopyrightText: 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: 2021 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/nvdata.h

pub const MAX_SYNC_POINTS: u32 = 192;
pub const MAX_NV_EVENTS: u32 = 64;

pub type DeviceFD = i32;

pub const INVALID_NVDRV_FD: DeviceFD = -1;

#[repr(C)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct NvFence {
    pub id: i32,
    pub value: u32,
}
const _: () = assert!(std::mem::size_of::<NvFence>() == 8);

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NvResult {
    Success = 0x0,
    NotImplemented = 0x1,
    NotSupported = 0x2,
    NotInitialized = 0x3,
    BadParameter = 0x4,
    Timeout = 0x5,
    InsufficientMemory = 0x6,
    ReadOnlyAttribute = 0x7,
    InvalidState = 0x8,
    InvalidAddress = 0x9,
    InvalidSize = 0xA,
    BadValue = 0xB,
    AlreadyAllocated = 0xD,
    Busy = 0xE,
    ResourceError = 0xF,
    CountMismatch = 0x10,
    OverFlow = 0x11,
    InsufficientTransferMemory = 0x1000,
    InsufficientVideoMemory = 0x10000,
    BadSurfaceColorScheme = 0x10001,
    InvalidSurface = 0x10002,
    SurfaceNotSupported = 0x10003,
    DispInitFailed = 0x20000,
    DispAlreadyAttached = 0x20001,
    DispTooManyDisplays = 0x20002,
    DispNoDisplaysAttached = 0x20003,
    DispModeNotSupported = 0x20004,
    DispNotFound = 0x20005,
    DispAttachDisallowed = 0x20006,
    DispTypeNotSupported = 0x20007,
    DispAuthenticationFailed = 0x20008,
    DispNotAttached = 0x20009,
    DispSamePwrState = 0x2000A,
    DispEdidFailure = 0x2000B,
    DispDsiReadAckError = 0x2000C,
    DispDsiReadInvalidResp = 0x2000D,
    FileWriteFailed = 0x30000,
    FileReadFailed = 0x30001,
    EndOfFile = 0x30002,
    FileOperationFailed = 0x30003,
    DirOperationFailed = 0x30004,
    EndOfDirList = 0x30005,
    ConfigVarNotFound = 0x30006,
    InvalidConfigVar = 0x30007,
    LibraryNotFound = 0x30008,
    SymbolNotFound = 0x30009,
    MemoryMapFailed = 0x3000A,
    IoctlFailed = 0x3000F,
    AccessDenied = 0x30010,
    DeviceNotFound = 0x30011,
    KernelDriverNotFound = 0x30012,
    FileNotFound = 0x30013,
    PathAlreadyExists = 0x30014,
    ModuleNotPresent = 0xA000E,
}

impl Default for NvResult {
    fn default() -> Self {
        NvResult::Success
    }
}

/// Event states obtained from Skyline.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventState {
    Available = 0,
    Waiting = 1,
    Cancelling = 2,
    Signalling = 3,
    Signalled = 4,
    Cancelled = 5,
}

impl Default for EventState {
    fn default() -> Self {
        EventState::Available
    }
}

/// Ioctl command descriptor.
///
/// Maps to the C++ union Ioctl with bit fields:
/// - bits [0..8):   cmd
/// - bits [8..16):  group
/// - bits [16..30): length
/// - bit  30:       is_in
/// - bit  31:       is_out
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Ioctl {
    pub raw: u32,
}

impl Ioctl {
    pub fn cmd(&self) -> u32 {
        self.raw & 0xFF
    }

    pub fn group(&self) -> u8 {
        ((self.raw >> 8) & 0xFF) as u8
    }

    pub fn length(&self) -> u32 {
        (self.raw >> 16) & 0x3FFF
    }

    pub fn is_in(&self) -> bool {
        (self.raw >> 30) & 1 != 0
    }

    pub fn is_out(&self) -> bool {
        (self.raw >> 31) & 1 != 0
    }
}
