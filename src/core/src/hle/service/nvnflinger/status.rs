// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/status.h

/// Android buffer queue status codes.
///
/// Note: In C++ this is `enum class Status : s32` with `DECLARE_ENUM_FLAG_OPERATORS`.
/// Some values have multiple names (e.g., NoError == None == 0, StaleBufferSlot ==
/// BufferNeedsReallocation == 1). In Rust we cannot have duplicate discriminants,
/// so we define the canonical names and provide aliases as constants.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Status {
    NoError = 0,
    StaleBufferSlot = 1,
    NoBufferAvailable = 2,
    PresentLater = 3,
    WouldBlock = -11,
    NoMemory = -12,
    Busy = -16,
    NoInit = -19,
    BadValue = -22,
    InvalidOperation = -38,
}

impl Status {
    /// Alias: None == NoError == 0
    pub const NONE: Status = Status::NoError;
    /// Alias: BufferNeedsReallocation == StaleBufferSlot == 1
    pub const BUFFER_NEEDS_REALLOCATION: Status = Status::StaleBufferSlot;
    /// Alias: ReleaseAllBuffers == NoBufferAvailable == 2
    pub const RELEASE_ALL_BUFFERS: Status = Status::NoBufferAvailable;
}

/// Raw Android status value.
///
/// Upstream declares `Status` as an enum with flag operators, so some return
/// paths can legally produce combined values such as
/// `BufferNeedsReallocation | ReleaseAllBuffers == 3`. Keep `Status` for
/// simple non-combined results and use this raw wrapper where upstream relies
/// on bitwise status composition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct StatusCode(i32);

impl StatusCode {
    pub const NONE: Self = Self(0);
    pub const NO_ERROR: Self = Self(Status::NoError as i32);
    pub const BUFFER_NEEDS_REALLOCATION: Self = Self(Status::BUFFER_NEEDS_REALLOCATION as i32);
    pub const RELEASE_ALL_BUFFERS: Self = Self(Status::RELEASE_ALL_BUFFERS as i32);

    pub const fn from_raw(raw: i32) -> Self {
        Self(raw)
    }

    pub const fn raw(self) -> i32 {
        self.0
    }

    pub const fn contains(self, flag: Self) -> bool {
        (self.0 & flag.0) != 0
    }
}

impl From<Status> for StatusCode {
    fn from(status: Status) -> Self {
        Self(status as i32)
    }
}

impl std::ops::BitOrAssign for StatusCode {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl std::ops::BitOr for StatusCode {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl std::ops::BitAnd for StatusCode {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl Default for Status {
    fn default() -> Self {
        Status::NoError
    }
}

impl std::fmt::Display for Status {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({})", self, *self as i32)
    }
}

impl std::ops::BitOr for Status {
    type Output = i32;
    fn bitor(self, rhs: Self) -> i32 {
        (self as i32) | (rhs as i32)
    }
}

impl std::ops::BitAnd for Status {
    type Output = i32;
    fn bitand(self, rhs: Self) -> i32 {
        (self as i32) & (rhs as i32)
    }
}
