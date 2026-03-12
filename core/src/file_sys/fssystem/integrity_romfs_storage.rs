// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_integrity_romfs_storage.h / .cpp
// Status: COMPLETE (structural parity)
//
// Integrity RomFS storage: wraps a HierarchicalIntegrityVerificationStorage
// with a master hash. On initialization, creates an in-memory VFS file for
// the master hash and sets it as storage[0] of the hierarchy.

use std::sync::Arc;

use super::hierarchical_integrity_verification_storage::{
    HierarchicalIntegrityVerificationInformation, HierarchicalIntegrityVerificationStorage,
    HierarchicalStorageInformation,
};
use super::nca_header::Hash;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use crate::file_sys::vfs::vfs_vector::VectorVfsFile;
use common::ResultCode;

/// Number of layers in the integrity hierarchy for RomFS.
///
/// Corresponds to upstream `IntegrityLayerCountRomFs`.
pub const INTEGRITY_LAYER_COUNT_ROMFS: usize = 7;

/// Default hash layer block size.
///
/// Corresponds to upstream `IntegrityHashLayerBlockSize`.
pub const INTEGRITY_HASH_LAYER_BLOCK_SIZE: usize = 16 * 1024;

/// Integrity RomFS storage.
///
/// Wraps a HierarchicalIntegrityVerificationStorage with a master hash.
/// On initialization, creates an in-memory VFS file for the master hash
/// bytes and sets it as the first storage in the hierarchy.
///
/// Corresponds to upstream `IntegrityRomFsStorage`.
pub struct IntegrityRomFsStorage {
    integrity_storage: HierarchicalIntegrityVerificationStorage,
    master_hash: Hash,
    master_hash_storage: Option<VirtualFile>,
}

impl IntegrityRomFsStorage {
    /// Create a new uninitialized IntegrityRomFsStorage.
    pub fn new() -> Self {
        Self {
            integrity_storage: HierarchicalIntegrityVerificationStorage::new(),
            master_hash: Hash::default(),
            master_hash_storage: None,
        }
    }

    /// Initialize the storage.
    ///
    /// Creates an in-memory VFS file from the master hash bytes and sets
    /// it as storage_info[0]. Then initializes the underlying integrity storage.
    ///
    /// Corresponds to upstream `IntegrityRomFsStorage::Initialize`.
    pub fn initialize(
        &mut self,
        level_hash_info: HierarchicalIntegrityVerificationInformation,
        master_hash: Hash,
        mut storage_info: HierarchicalStorageInformation,
        max_data_cache_entries: i32,
        max_hash_cache_entries: i32,
        buffer_level: i8,
    ) -> Result<(), ResultCode> {
        // Set master hash.
        self.master_hash = master_hash;

        // Create an in-memory VFS file for the master hash bytes.
        // Corresponds to upstream:
        //   m_master_hash_storage = std::make_shared<ArrayVfsFile<sizeof(Hash)>>(m_master_hash.value);
        let hash_file: VirtualFile = Arc::new(VectorVfsFile::new(
            self.master_hash.value.to_vec(),
            "master_hash".to_string(),
            None,
        ));
        self.master_hash_storage = Some(hash_file.clone());

        // Set the master hash storage as layer 0.
        storage_info.set_master_hash_storage(hash_file);

        // Initialize our integrity storage.
        self.integrity_storage.initialize(
            &level_hash_info,
            storage_info,
            max_data_cache_entries,
            max_hash_cache_entries,
            buffer_level,
        )
    }

    /// Finalize the storage, releasing all references.
    ///
    /// Corresponds to upstream `IntegrityRomFsStorage::Finalize`.
    pub fn finalize(&mut self) {
        self.integrity_storage.finalize();
    }

    /// Get the size of the data storage.
    ///
    /// Corresponds to upstream `IntegrityRomFsStorage::GetSize`.
    pub fn get_size(&self) -> usize {
        self.integrity_storage.get_size()
    }

    /// Read data from the storage.
    ///
    /// Corresponds to upstream `IntegrityRomFsStorage::Read`.
    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        self.integrity_storage.read(buffer, buffer.len(), offset)
    }
}

impl Default for IntegrityRomFsStorage {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constants() {
        assert_eq!(INTEGRITY_LAYER_COUNT_ROMFS, 7);
        assert_eq!(INTEGRITY_HASH_LAYER_BLOCK_SIZE, 16384);
    }

    #[test]
    fn test_new() {
        let storage = IntegrityRomFsStorage::new();
        assert_eq!(storage.get_size(), 0);
        assert!(storage.master_hash_storage.is_none());
    }

    #[test]
    fn test_default() {
        let storage = IntegrityRomFsStorage::default();
        assert_eq!(storage.get_size(), 0);
    }
}
