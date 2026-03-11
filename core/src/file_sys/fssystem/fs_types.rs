// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fs_types.h

/// Split 64-bit integer stored as two 32-bit halves.
/// Matches upstream `FileSys::Int64` layout exactly.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Int64 {
    pub low: u32,
    pub high: u32,
}

impl Int64 {
    pub const fn set(&mut self, v: i64) {
        self.low = ((v as u64) & 0x0000_0000_FFFF_FFFF) as u32;
        self.high = (((v as u64) & 0xFFFF_FFFF_0000_0000) >> 32) as u32;
    }

    pub const fn get(&self) -> i64 {
        ((self.high as i64) << 32) | (self.low as i64)
    }

    pub const fn from_i64(v: i64) -> Self {
        let mut r = Self { low: 0, high: 0 };
        r.low = ((v as u64) & 0x0000_0000_FFFF_FFFF) as u32;
        r.high = (((v as u64) & 0xFFFF_FFFF_0000_0000) >> 32) as u32;
        r
    }
}

impl From<i64> for Int64 {
    fn from(v: i64) -> Self {
        Self::from_i64(v)
    }
}

impl From<Int64> for i64 {
    fn from(v: Int64) -> i64 {
        v.get()
    }
}

/// Hash salt for integrity verification.
/// Matches upstream `HashSalt`.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct HashSalt {
    pub value: [u8; Self::SIZE],
}

impl HashSalt {
    pub const SIZE: usize = 32;
}

impl Default for HashSalt {
    fn default() -> Self {
        Self {
            value: [0u8; Self::SIZE],
        }
    }
}

const _: () = assert!(std::mem::size_of::<HashSalt>() == HashSalt::SIZE);

pub const INTEGRITY_MIN_LAYER_COUNT: usize = 2;
pub const INTEGRITY_MAX_LAYER_COUNT: usize = 7;
pub const INTEGRITY_LAYER_COUNT_SAVE: usize = 5;
pub const INTEGRITY_LAYER_COUNT_SAVE_DATA_META: usize = 4;
