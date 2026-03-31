// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_memory_resource_buffer_hold_storage.h
// Status: COMPLETE (structural parity)
//
// Memory resource buffer hold storage: wraps a VirtualFile and holds
// a preallocated buffer that can be used by callers for I/O operations.
// The buffer ownership is tied to the storage lifetime.

use crate::file_sys::vfs::vfs_types::VirtualFile;

/// A storage that wraps a VirtualFile and holds a preallocated buffer.
///
/// The held buffer can be used for aligned I/O or as a temporary scratch
/// space. The storage itself delegates read/write to the underlying file.
///
/// Corresponds to upstream `MemoryResourceBufferHoldStorage`.
pub struct MemoryResourceBufferHoldStorage {
    storage: VirtualFile,
    buffer: Vec<u8>,
}

impl MemoryResourceBufferHoldStorage {
    /// Create a new MemoryResourceBufferHoldStorage with the given buffer size.
    ///
    /// Corresponds to upstream constructor.
    pub fn new(storage: VirtualFile, buffer_size: usize) -> Self {
        Self {
            storage,
            buffer: vec![0u8; buffer_size],
        }
    }

    /// Check if the buffer was successfully allocated (non-empty).
    ///
    /// Corresponds to upstream `MemoryResourceBufferHoldStorage::IsValid`.
    pub fn is_valid(&self) -> bool {
        !self.buffer.is_empty()
    }

    /// Get a read-only reference to the held buffer.
    pub fn get_buffer(&self) -> &[u8] {
        &self.buffer
    }

    /// Get a mutable reference to the held buffer.
    pub fn get_buffer_mut(&mut self) -> &mut [u8] {
        &mut self.buffer
    }

    /// Get the size of the underlying storage.
    pub fn get_size(&self) -> usize {
        self.storage.get_size()
    }

    /// Read from the underlying storage.
    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        self.storage.read(buffer, buffer.len(), offset)
    }

    /// Write to the underlying storage.
    pub fn write(&self, buffer: &[u8], offset: usize) -> usize {
        self.storage.write(buffer, buffer.len(), offset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs_vector::VectorVfsFile;
    use std::sync::Arc;

    fn make_test_storage(data: Vec<u8>) -> VirtualFile {
        Arc::new(VectorVfsFile::new(data, "test".to_string(), None))
    }

    #[test]
    fn test_is_valid() {
        let storage = MemoryResourceBufferHoldStorage::new(make_test_storage(vec![0u8; 10]), 64);
        assert!(storage.is_valid());

        let storage_zero =
            MemoryResourceBufferHoldStorage::new(make_test_storage(vec![0u8; 10]), 0);
        assert!(!storage_zero.is_valid());
    }

    #[test]
    fn test_buffer_size() {
        let storage = MemoryResourceBufferHoldStorage::new(make_test_storage(vec![0u8; 10]), 128);
        assert_eq!(storage.get_buffer().len(), 128);
    }

    #[test]
    fn test_get_size() {
        let data = vec![0xABu8; 42];
        let storage = MemoryResourceBufferHoldStorage::new(make_test_storage(data), 64);
        assert_eq!(storage.get_size(), 42);
    }

    #[test]
    fn test_read() {
        let data = vec![1u8, 2, 3, 4, 5];
        let storage = MemoryResourceBufferHoldStorage::new(make_test_storage(data), 64);
        let mut buf = [0u8; 3];
        let read = storage.read(&mut buf, 1);
        assert_eq!(read, 3);
        assert_eq!(buf, [2, 3, 4]);
    }

    #[test]
    fn test_write() {
        let data = vec![0u8; 10];
        let storage = MemoryResourceBufferHoldStorage::new(make_test_storage(data), 64);
        let write_data = [0xAAu8, 0xBB, 0xCC];
        let written = storage.write(&write_data, 2);
        assert_eq!(written, 3);
    }
}
