// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fs_i_storage.h
// Status: COMPLETE (structural parity)
//
// Base storage interfaces. IStorage extends VfsFile conceptually,
// providing storage-centric access. IReadOnlyStorage further restricts
// to read-only access.

use crate::file_sys::errors::*;
use common::ResultCode;

/// Base trait for storage implementations.
///
/// In upstream, IStorage extends VfsFile with default implementations
/// for GetName, GetContainingDirectory, IsWritable, IsReadable, Resize,
/// and Rename. The key methods are Read, Write, and GetSize.
///
/// Corresponds to upstream `IStorage`.
pub trait IStorage: Send + Sync {
    /// Read data from the storage at the given offset.
    fn read(&self, buffer: &mut [u8], offset: usize) -> usize;

    /// Write data to the storage at the given offset.
    fn write(&self, buffer: &[u8], offset: usize) -> usize;

    /// Get the total size of the storage.
    fn get_size(&self) -> usize;

    /// Whether the storage is writable.
    fn is_writable(&self) -> bool {
        true
    }

    /// Whether the storage is readable.
    fn is_readable(&self) -> bool {
        true
    }

    /// Resize the storage. Returns whether the operation was successful.
    fn resize(&self, _size: usize) -> bool {
        false
    }
}

/// Check that an access range [offset, offset+size) is valid within total_size.
///
/// Returns an error if:
/// - offset < 0
/// - size < 0
/// - offset + size overflows
/// - offset + size > total_size
///
/// Corresponds to upstream `IStorage::CheckAccessRange`.
pub fn check_access_range(offset: i64, size: i64, total_size: i64) -> Result<(), ResultCode> {
    if offset < 0 {
        return Err(RESULT_INVALID_OFFSET);
    }
    if size < 0 {
        return Err(RESULT_INVALID_SIZE);
    }
    // Check for wrapping addition (overflow).
    if offset.wrapping_add(size) < offset {
        return Err(RESULT_OUT_OF_RANGE);
    }
    if offset + size > total_size {
        return Err(RESULT_OUT_OF_RANGE);
    }
    Ok(())
}

/// Read-only storage trait.
///
/// Upstream `IReadOnlyStorage` extends `IStorage` with IsWritable() = false
/// and Write() returning 0.
///
/// Corresponds to upstream `IReadOnlyStorage`.
pub trait IReadOnlyStorage: Send + Sync {
    /// Read data from the storage at the given offset.
    fn read(&self, buffer: &mut [u8], offset: usize) -> usize;

    /// Get the total size of the storage.
    fn get_size(&self) -> usize;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_check_access_range_valid() {
        assert!(check_access_range(0, 10, 100).is_ok());
        assert!(check_access_range(90, 10, 100).is_ok());
        assert!(check_access_range(0, 0, 100).is_ok());
        assert!(check_access_range(100, 0, 100).is_ok());
    }

    #[test]
    fn test_check_access_range_negative_offset() {
        let err = check_access_range(-1, 10, 100).unwrap_err();
        assert_eq!(err, RESULT_INVALID_OFFSET);
    }

    #[test]
    fn test_check_access_range_negative_size() {
        let err = check_access_range(0, -1, 100).unwrap_err();
        assert_eq!(err, RESULT_INVALID_SIZE);
    }

    #[test]
    fn test_check_access_range_out_of_range() {
        let err = check_access_range(91, 10, 100).unwrap_err();
        assert_eq!(err, RESULT_OUT_OF_RANGE);
    }

    #[test]
    fn test_check_access_range_overflow() {
        let err = check_access_range(i64::MAX, 1, i64::MAX).unwrap_err();
        assert_eq!(err, RESULT_OUT_OF_RANGE);
    }

    #[test]
    fn test_check_access_range_exact_boundary() {
        // offset + size == total_size is valid
        assert!(check_access_range(50, 50, 100).is_ok());
    }
}
