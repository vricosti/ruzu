// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_sparse_storage.h / .cpp

use super::indirect_storage::{IndirectStorage, NODE_SIZE};
use crate::file_sys::vfs::vfs_types::VirtualFile;
use common::ResultCode;

/// Sparse storage — wraps IndirectStorage with a zero-fill fallback storage.
/// Corresponds to upstream `SparseStorage`.
pub struct SparseStorage {
    inner: IndirectStorage,
    // The zero storage is storage index 1 in the indirect table.
}

impl SparseStorage {
    pub fn new() -> Self {
        Self {
            inner: IndirectStorage::new(),
        }
    }

    pub fn initialize(
        &mut self,
        node_storage: VirtualFile,
        entry_storage: VirtualFile,
        entry_count: i32,
    ) -> Result<(), ResultCode> {
        self.inner
            .initialize(node_storage, entry_storage, entry_count)?;
        // Zero storage would be set as storage index 1 with a ZeroStorage.
        // Stubbed for now — the indirect read will handle missing storage.
        Ok(())
    }

    pub fn initialize_empty(&mut self, end_offset: i64) {
        self.inner.get_entry_table().initialize_empty(NODE_SIZE, end_offset);
    }

    pub fn finalize(&mut self) {
        self.inner.finalize();
    }

    pub fn is_initialized(&self) -> bool {
        self.inner.is_initialized()
    }

    pub fn set_data_storage(&mut self, storage: VirtualFile) {
        assert!(self.is_initialized());
        self.inner.set_storage(0, storage);
    }

    pub fn get_size(&self) -> usize {
        self.inner.get_size()
    }

    /// Read from the sparse storage.
    /// TODO: implement proper sparse/zero resolution.
    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        self.inner.read(buffer, offset)
    }
}

impl Default for SparseStorage {
    fn default() -> Self {
        Self::new()
    }
}
