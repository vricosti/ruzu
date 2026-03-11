// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_integrity_romfs_storage.h / .cpp

use super::hierarchical_integrity_verification_storage::{
    HierarchicalIntegrityVerificationInformation, HierarchicalIntegrityVerificationStorage,
    HierarchicalStorageInformation,
};
use super::nca_header::Hash;
use common::ResultCode;

pub const INTEGRITY_LAYER_COUNT_ROMFS: usize = 7;
pub const INTEGRITY_HASH_LAYER_BLOCK_SIZE: usize = 16 * 1024;

/// Integrity RomFS storage.
/// Corresponds to upstream `IntegrityRomFsStorage`.
pub struct IntegrityRomFsStorage {
    integrity_storage: HierarchicalIntegrityVerificationStorage,
    master_hash: Hash,
}

impl IntegrityRomFsStorage {
    pub fn new() -> Self {
        Self {
            integrity_storage: HierarchicalIntegrityVerificationStorage::new(),
            master_hash: Hash::default(),
        }
    }

    pub fn initialize(
        &mut self,
        level_hash_info: HierarchicalIntegrityVerificationInformation,
        master_hash: Hash,
        storage_info: HierarchicalStorageInformation,
        max_data_cache_entries: i32,
        max_hash_cache_entries: i32,
        buffer_level: i8,
    ) -> Result<(), ResultCode> {
        self.master_hash = master_hash;
        self.integrity_storage.initialize(
            &level_hash_info,
            storage_info,
            max_data_cache_entries,
            max_hash_cache_entries,
            buffer_level,
        )
    }

    pub fn finalize(&mut self) {
        self.integrity_storage.finalize();
    }

    pub fn get_size(&self) -> usize {
        self.integrity_storage.get_size()
    }

    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        self.integrity_storage.read(buffer, offset)
    }
}

impl Default for IntegrityRomFsStorage {
    fn default() -> Self {
        Self::new()
    }
}
