// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_hierarchical_sha256_storage.h / .cpp

use crate::file_sys::vfs::vfs_types::VirtualFile;
use common::ResultCode;

pub struct HierarchicalSha256Storage {
    base_storage: Option<VirtualFile>,
    base_storage_size: i64,
    hash_buffer: Vec<u8>,
    hash_target_block_size: i32,
    log_size_ratio: i32,
}

impl HierarchicalSha256Storage {
    pub const LAYER_COUNT: i32 = 3;
    pub const HASH_SIZE: usize = 256 / 8;

    pub fn new() -> Self {
        Self { base_storage: None, base_storage_size: 0, hash_buffer: Vec::new(), hash_target_block_size: 0, log_size_ratio: 0 }
    }

    pub fn initialize(&mut self, _base_storages: &[VirtualFile], _layer_count: i32, _htbs: usize, _hash_buf: &[u8]) -> Result<(), ResultCode> {
        Ok(())
    }

    pub fn get_size(&self) -> usize {
        if let Some(ref bs) = self.base_storage { bs.get_size() } else { 0 }
    }

    /// Stub: reads without hash verification.
    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        if let Some(ref bs) = self.base_storage { bs.read(buffer, buffer.len(), offset) } else { 0 }
    }
}

impl Default for HierarchicalSha256Storage { fn default() -> Self { Self::new() } }
