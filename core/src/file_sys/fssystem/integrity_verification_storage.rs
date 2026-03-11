// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_integrity_verification_storage.h / .cpp

use crate::file_sys::vfs::vfs_types::VirtualFile;

pub const HASH_SIZE: i64 = 256 / 8;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct BlockHash {
    pub hash: [u8; HASH_SIZE as usize],
}

impl Default for BlockHash {
    fn default() -> Self { Self { hash: [0u8; HASH_SIZE as usize] } }
}

pub struct IntegrityVerificationStorage {
    hash_storage: Option<VirtualFile>,
    data_storage: Option<VirtualFile>,
    verification_block_size: i64,
    verification_block_order: i64,
    upper_layer_verification_block_size: i64,
    upper_layer_verification_block_order: i64,
    is_real_data: bool,
}

impl IntegrityVerificationStorage {
    pub fn new() -> Self {
        Self { hash_storage: None, data_storage: None, verification_block_size: 0, verification_block_order: 0, upper_layer_verification_block_size: 0, upper_layer_verification_block_order: 0, is_real_data: false }
    }

    pub fn initialize(&mut self, hash_storage: VirtualFile, data_storage: VirtualFile, verif_block_size: i64, upper_layer_verif_block_size: i64, is_real_data: bool) {
        self.hash_storage = Some(hash_storage);
        self.data_storage = Some(data_storage);
        self.verification_block_size = verif_block_size;
        self.verification_block_order = (verif_block_size as u64).trailing_zeros() as i64;
        self.upper_layer_verification_block_size = upper_layer_verif_block_size;
        self.upper_layer_verification_block_order = (upper_layer_verif_block_size as u64).trailing_zeros() as i64;
        self.is_real_data = is_real_data;
    }

    pub fn finalize(&mut self) { self.hash_storage = None; self.data_storage = None; }
    pub fn get_block_size(&self) -> i64 { self.verification_block_size }

    pub fn get_size(&self) -> usize {
        if let Some(ref ds) = self.data_storage { ds.get_size() } else { 0 }
    }

    /// Stub: reads without hash verification.
    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        if let Some(ref ds) = self.data_storage { ds.read(buffer, buffer.len(), offset) } else { 0 }
    }
}

impl Default for IntegrityVerificationStorage { fn default() -> Self { Self::new() } }
