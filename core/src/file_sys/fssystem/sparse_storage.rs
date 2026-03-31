// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_sparse_storage.h / .cpp
// Status: COMPLETE (structural parity; read delegates to IndirectStorage)
//
// Sparse storage: wraps an IndirectStorage with a zero-fill fallback.
// Storage index 0 is the real data, storage index 1 is a ZeroStorage
// that returns zero-filled buffers for any unresolved range.

use std::sync::Arc;

use super::indirect_storage::{IndirectStorage, NODE_SIZE};
use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use common::ResultCode;

/// A virtual storage that returns all zeros.
///
/// Corresponds to upstream `SparseStorage::ZeroStorage`.
struct ZeroStorage;

impl VfsFile for ZeroStorage {
    fn get_name(&self) -> String {
        "ZeroStorage".to_string()
    }

    fn get_size(&self) -> usize {
        usize::MAX
    }

    fn resize(&self, _new_size: usize) -> bool {
        false
    }

    fn get_containing_directory(&self) -> Option<crate::file_sys::vfs::vfs_types::VirtualDir> {
        None
    }

    fn is_writable(&self) -> bool {
        false
    }

    fn is_readable(&self) -> bool {
        true
    }

    fn read(&self, data: &mut [u8], length: usize, _offset: usize) -> usize {
        let actual = length.min(data.len());
        for byte in &mut data[..actual] {
            *byte = 0;
        }
        actual
    }

    fn write(&self, _data: &[u8], _length: usize, _offset: usize) -> usize {
        0
    }

    fn rename(&self, _name: &str) -> bool {
        false
    }
}

/// Sparse storage — wraps IndirectStorage with a zero-fill fallback storage.
///
/// Corresponds to upstream `SparseStorage`.
pub struct SparseStorage {
    inner: IndirectStorage,
    zero_storage: VirtualFile,
}

impl SparseStorage {
    /// Create a new SparseStorage.
    ///
    /// Corresponds to upstream `SparseStorage::SparseStorage`.
    pub fn new() -> Self {
        Self {
            inner: IndirectStorage::new(),
            zero_storage: Arc::new(ZeroStorage),
        }
    }

    /// Initialize from node and entry storage files.
    ///
    /// Corresponds to upstream `SparseStorage::Initialize(node, entry, count)`.
    pub fn initialize(
        &mut self,
        node_storage: VirtualFile,
        entry_storage: VirtualFile,
        entry_count: i32,
    ) -> Result<(), ResultCode> {
        self.inner
            .initialize(node_storage, entry_storage, entry_count)?;
        self.set_zero_storage();
        Ok(())
    }

    /// Initialize with an empty entry table ending at the given offset.
    ///
    /// Corresponds to upstream `SparseStorage::Initialize(end_offset)`.
    pub fn initialize_empty(&mut self, end_offset: i64) {
        self.inner
            .get_entry_table()
            .initialize_empty(NODE_SIZE, end_offset);
        self.set_zero_storage();
    }

    /// Finalize the storage.
    pub fn finalize(&mut self) {
        self.inner.finalize();
    }

    /// Check if the storage is initialized.
    pub fn is_initialized(&self) -> bool {
        self.inner.is_initialized()
    }

    /// Set the data storage (index 0) and re-set the zero storage (index 1).
    ///
    /// Corresponds to upstream `SparseStorage::SetDataStorage`.
    pub fn set_data_storage(&mut self, storage: VirtualFile) {
        assert!(self.is_initialized());
        self.inner.set_storage(0, storage);
        self.set_zero_storage();
    }

    /// Get the size of the storage.
    pub fn get_size(&self) -> usize {
        self.inner.get_size()
    }

    /// Read from the sparse storage.
    ///
    /// Delegates to the underlying IndirectStorage which resolves ranges
    /// to either the data storage (index 0) or zero storage (index 1).
    ///
    /// Corresponds to upstream `SparseStorage::Read`.
    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        self.inner.read(buffer, offset)
    }

    /// Set storage index 1 to the zero storage.
    ///
    /// Corresponds to upstream `SparseStorage::SetZeroStorage`.
    fn set_zero_storage(&mut self) {
        self.inner.set_storage(1, self.zero_storage.clone());
    }
}

/// Implement VfsFile so SparseStorage can be used as a VirtualFile.
impl VfsFile for SparseStorage {
    fn get_name(&self) -> String {
        String::from("SparseStorage")
    }

    fn get_size(&self) -> usize {
        self.inner.get_size()
    }

    fn resize(&self, _new_size: usize) -> bool {
        false
    }

    fn get_containing_directory(&self) -> Option<crate::file_sys::vfs::vfs_types::VirtualDir> {
        None
    }

    fn is_writable(&self) -> bool {
        false
    }

    fn is_readable(&self) -> bool {
        true
    }

    fn read(&self, data: &mut [u8], length: usize, offset: usize) -> usize {
        let actual = length.min(data.len());
        if actual == 0 {
            return 0;
        }
        self.read(&mut data[..actual], offset)
    }

    fn write(&self, _data: &[u8], _length: usize, _offset: usize) -> usize {
        0
    }

    fn rename(&self, _new_name: &str) -> bool {
        false
    }
}

impl Default for SparseStorage {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zero_storage_read() {
        let zs = ZeroStorage;
        let mut buf = [0xFFu8; 10];
        let read = zs.read(&mut buf, 10, 0);
        assert_eq!(read, 10);
        assert!(buf.iter().all(|&b| b == 0));
    }

    #[test]
    fn test_zero_storage_size() {
        let zs = ZeroStorage;
        assert_eq!(zs.get_size(), usize::MAX);
    }

    #[test]
    fn test_sparse_storage_new() {
        let storage = SparseStorage::new();
        assert!(!storage.is_initialized());
    }
}
