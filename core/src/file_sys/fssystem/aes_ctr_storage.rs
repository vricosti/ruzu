// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_aes_ctr_storage.h / .cpp

use crate::file_sys::vfs::vfs_types::VirtualFile;

pub const BLOCK_SIZE: usize = 0x10;
pub const KEY_SIZE: usize = 0x10;
pub const IV_SIZE: usize = 0x10;

/// AES-CTR storage layer.
/// Corresponds to upstream `AesCtrStorage`.
pub struct AesCtrStorage {
    base_storage: VirtualFile,
    key: [u8; KEY_SIZE],
    iv: [u8; IV_SIZE],
}

impl AesCtrStorage {
    pub fn new(base: VirtualFile, key: &[u8], iv: &[u8]) -> Self {
        let mut k = [0u8; KEY_SIZE];
        let mut i = [0u8; IV_SIZE];
        let key_len = std::cmp::min(key.len(), KEY_SIZE);
        let iv_len = std::cmp::min(iv.len(), IV_SIZE);
        k[..key_len].copy_from_slice(&key[..key_len]);
        i[..iv_len].copy_from_slice(&iv[..iv_len]);
        Self {
            base_storage: base,
            key: k,
            iv: i,
        }
    }

    /// Make an IV from an upper 64-bit value and an offset.
    /// Corresponds to upstream `AesCtrStorage::MakeIv`.
    pub fn make_iv(dst: &mut [u8; IV_SIZE], upper: u64, offset: i64) {
        let upper_bytes = upper.to_be_bytes();
        dst[..8].copy_from_slice(&upper_bytes);
        let lower = (offset / BLOCK_SIZE as i64) as u64;
        let lower_bytes = lower.to_be_bytes();
        dst[8..].copy_from_slice(&lower_bytes);
    }

    pub fn get_size(&self) -> usize {
        self.base_storage.get_size()
    }

    /// Read and decrypt. Stub: reads raw bytes without decryption.
    /// TODO: implement actual AES-CTR decryption.
    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        self.base_storage.read(buffer, buffer.len(), offset)
    }

    /// Write and encrypt. Stub: writes raw bytes without encryption.
    /// TODO: implement actual AES-CTR encryption.
    pub fn write(&self, buffer: &[u8], offset: usize) -> usize {
        self.base_storage.write(buffer, buffer.len(), offset)
    }
}
