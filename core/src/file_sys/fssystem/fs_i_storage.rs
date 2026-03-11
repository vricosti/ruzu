// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fs_i_storage.h

use crate::file_sys::errors::*;
use common::ResultCode;

/// Base trait for storage implementations.
/// Corresponds to upstream `IStorage`.
pub trait IStorage: Send + Sync {
    fn read(&self, buffer: &mut [u8], offset: usize) -> usize;
    fn write(&self, buffer: &[u8], offset: usize) -> usize;
    fn get_size(&self) -> usize;
    fn is_writable(&self) -> bool {
        true
    }
    fn is_readable(&self) -> bool {
        true
    }
    fn resize(&self, _size: usize) -> bool {
        false
    }
}

/// Check that an access range [offset, offset+size) is valid within total_size.
/// Corresponds to upstream `IStorage::CheckAccessRange`.
pub fn check_access_range(offset: i64, size: i64, total_size: i64) -> Result<(), ResultCode> {
    if offset < 0 {
        return Err(RESULT_INVALID_OFFSET);
    }
    if size < 0 {
        return Err(RESULT_INVALID_SIZE);
    }
    if offset.wrapping_add(size) < offset {
        return Err(RESULT_OUT_OF_RANGE);
    }
    if offset + size > total_size {
        return Err(RESULT_OUT_OF_RANGE);
    }
    Ok(())
}

/// Read-only storage trait.
/// Corresponds to upstream `IReadOnlyStorage`.
pub trait IReadOnlyStorage: Send + Sync {
    fn read(&self, buffer: &mut [u8], offset: usize) -> usize;
    fn get_size(&self) -> usize;
}
