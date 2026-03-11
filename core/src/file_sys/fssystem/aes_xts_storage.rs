// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_aes_xts_storage.h / .cpp

use crate::file_sys::vfs::vfs_types::VirtualFile;

pub const AES_BLOCK_SIZE: usize = 0x10;
pub const KEY_SIZE: usize = 0x20;
pub const IV_SIZE: usize = 0x10;

/// AES-XTS storage layer (read-only).
/// Corresponds to upstream `AesXtsStorage`.
pub struct AesXtsStorage {
    base_storage: VirtualFile,
    key: [u8; KEY_SIZE],
    iv: [u8; IV_SIZE],
    block_size: usize,
}

impl AesXtsStorage {
    pub fn new(base: VirtualFile, key1: &[u8], key2: &[u8], iv: &[u8], block_size: usize) -> Self {
        let mut key = [0u8; KEY_SIZE];
        let k1_len = std::cmp::min(key1.len(), KEY_SIZE / 2);
        let k2_len = std::cmp::min(key2.len(), KEY_SIZE / 2);
        key[..k1_len].copy_from_slice(&key1[..k1_len]);
        key[KEY_SIZE / 2..KEY_SIZE / 2 + k2_len].copy_from_slice(&key2[..k2_len]);
        let mut i = [0u8; IV_SIZE];
        let iv_len = std::cmp::min(iv.len(), IV_SIZE);
        i[..iv_len].copy_from_slice(&iv[..iv_len]);
        Self {
            base_storage: base,
            key,
            iv: i,
            block_size,
        }
    }

    pub fn make_aes_xts_iv(dst: &mut [u8; IV_SIZE], offset: i64, block_size: usize) {
        let block_index = if block_size > 0 { offset as u64 / block_size as u64 } else { 0 };
        let bytes = block_index.to_be_bytes();
        dst[..8].copy_from_slice(&[0u8; 8]);
        dst[8..].copy_from_slice(&bytes);
    }

    pub fn get_size(&self) -> usize {
        self.base_storage.get_size()
    }

    /// Stub: reads raw bytes without decryption.
    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        self.base_storage.read(buffer, buffer.len(), offset)
    }
}
