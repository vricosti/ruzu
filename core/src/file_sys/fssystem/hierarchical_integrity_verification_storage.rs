// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_hierarchical_integrity_verification_storage.h / .cpp

use super::fs_types::{HashSalt, Int64, INTEGRITY_MAX_LAYER_COUNT};
use super::integrity_verification_storage::IntegrityVerificationStorage;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use common::ResultCode;

/// Level information for hierarchical integrity verification.
/// Corresponds to upstream `HierarchicalIntegrityVerificationLevelInformation`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct HierarchicalIntegrityVerificationLevelInformation {
    pub offset: Int64,
    pub size: Int64,
    pub block_order: i32,
    pub reserved: [u8; 4],
}

const _: () = assert!(
    std::mem::size_of::<HierarchicalIntegrityVerificationLevelInformation>() == 0x18
);

/// Hierarchical integrity verification information.
/// Corresponds to upstream `HierarchicalIntegrityVerificationInformation`.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct HierarchicalIntegrityVerificationInformation {
    pub max_layers: u32,
    pub info:
        [HierarchicalIntegrityVerificationLevelInformation; INTEGRITY_MAX_LAYER_COUNT - 1],
    pub seed: HashSalt,
}

impl HierarchicalIntegrityVerificationInformation {
    pub fn get_layered_hash_size(&self) -> i64 {
        let idx = (self.max_layers as usize).saturating_sub(2);
        self.info[idx].offset.get()
    }

    pub fn get_data_offset(&self) -> i64 {
        let idx = (self.max_layers as usize).saturating_sub(2);
        self.info[idx].offset.get()
    }

    pub fn get_data_size(&self) -> i64 {
        let idx = (self.max_layers as usize).saturating_sub(2);
        self.info[idx].size.get()
    }
}

/// Meta information for hierarchical integrity verification.
/// Corresponds to upstream `HierarchicalIntegrityVerificationMetaInformation`.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct HierarchicalIntegrityVerificationMetaInformation {
    pub magic: u32,
    pub version: u32,
    pub master_hash_size: u32,
    pub level_hash_info: HierarchicalIntegrityVerificationInformation,
}

/// Size set for hierarchical integrity verification.
/// Corresponds to upstream `HierarchicalIntegrityVerificationSizeSet`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct HierarchicalIntegrityVerificationSizeSet {
    pub control_size: i64,
    pub master_hash_size: i64,
    pub layered_hash_sizes: [i64; INTEGRITY_MAX_LAYER_COUNT - 2],
}

/// Hierarchical storage information — array of storages for each layer.
/// Corresponds to upstream `HierarchicalIntegrityVerificationStorage::HierarchicalStorageInformation`.
pub struct HierarchicalStorageInformation {
    pub storages: [Option<VirtualFile>; Self::DATA_STORAGE + 1],
}

impl HierarchicalStorageInformation {
    pub const MASTER_STORAGE: usize = 0;
    pub const LAYER1_STORAGE: usize = 1;
    pub const LAYER2_STORAGE: usize = 2;
    pub const LAYER3_STORAGE: usize = 3;
    pub const LAYER4_STORAGE: usize = 4;
    pub const LAYER5_STORAGE: usize = 5;
    pub const DATA_STORAGE: usize = 6;

    pub fn new() -> Self {
        Self {
            storages: Default::default(),
        }
    }

    pub fn set_master_hash_storage(&mut self, s: VirtualFile) {
        self.storages[Self::MASTER_STORAGE] = Some(s);
    }
    pub fn set_layer1_hash_storage(&mut self, s: VirtualFile) {
        self.storages[Self::LAYER1_STORAGE] = Some(s);
    }
    pub fn set_layer2_hash_storage(&mut self, s: VirtualFile) {
        self.storages[Self::LAYER2_STORAGE] = Some(s);
    }
    pub fn set_layer3_hash_storage(&mut self, s: VirtualFile) {
        self.storages[Self::LAYER3_STORAGE] = Some(s);
    }
    pub fn set_layer4_hash_storage(&mut self, s: VirtualFile) {
        self.storages[Self::LAYER4_STORAGE] = Some(s);
    }
    pub fn set_layer5_hash_storage(&mut self, s: VirtualFile) {
        self.storages[Self::LAYER5_STORAGE] = Some(s);
    }
    pub fn set_data_storage(&mut self, s: VirtualFile) {
        self.storages[Self::DATA_STORAGE] = Some(s);
    }
}

impl Default for HierarchicalStorageInformation {
    fn default() -> Self {
        Self::new()
    }
}

/// Hierarchical integrity verification storage.
/// Corresponds to upstream `HierarchicalIntegrityVerificationStorage`.
pub struct HierarchicalIntegrityVerificationStorage {
    verify_storages: Vec<IntegrityVerificationStorage>,
    buffer_storages: Vec<Option<VirtualFile>>,
    data_size: i64,
    max_layers: i32,
}

impl HierarchicalIntegrityVerificationStorage {
    pub const HASH_SIZE: i64 = 256 / 8;
    pub const MAX_LAYERS: usize = INTEGRITY_MAX_LAYER_COUNT;

    pub fn new() -> Self {
        Self {
            verify_storages: Vec::new(),
            buffer_storages: Vec::new(),
            data_size: -1,
            max_layers: 0,
        }
    }

    pub fn initialize(
        &mut self,
        _info: &HierarchicalIntegrityVerificationInformation,
        _storage: HierarchicalStorageInformation,
        _max_data_cache_entries: i32,
        _max_hash_cache_entries: i32,
        _buffer_level: i8,
    ) -> Result<(), ResultCode> {
        // TODO: implement proper initialization with hash verification layers.
        // For now, stub that marks as initialized.
        self.data_size = 0;
        self.max_layers = 0;
        Ok(())
    }

    pub fn finalize(&mut self) {
        self.verify_storages.clear();
        self.buffer_storages.clear();
        self.data_size = -1;
        self.max_layers = 0;
    }

    pub fn is_initialized(&self) -> bool {
        self.data_size >= 0
    }

    pub fn get_size(&self) -> usize {
        // TODO: return actual data size from the data layer.
        0
    }

    /// Read from the storage. Stub.
    /// TODO: implement proper hierarchical hash verification reads.
    pub fn read(&self, _buffer: &mut [u8], _offset: usize) -> usize {
        0
    }

    pub fn get_default_data_cache_buffer_level(max_layers: u32) -> i8 {
        (16 + max_layers as i8 - 2) as i8
    }
}

impl Default for HierarchicalIntegrityVerificationStorage {
    fn default() -> Self {
        Self::new()
    }
}
