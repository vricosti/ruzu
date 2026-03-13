// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_aes_xts_storage.h / .cpp
// Status: COMPLETE (structural parity; actual AES-XTS decryption deferred)
//
// AES-XTS storage layer (read-only). Reads data through an AES-XTS cipher,
// operating on block-aligned accesses.

use std::sync::Mutex;

use super::utility::add_counter;
use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};

/// AES block size in bytes.
///
/// Corresponds to upstream `AesXtsStorage::AesBlockSize`.
pub const AES_BLOCK_SIZE: usize = 0x10;

/// AES-XTS key size in bytes (two 128-bit keys = 256 bits total).
///
/// Corresponds to upstream `AesXtsStorage::KeySize`.
pub const KEY_SIZE: usize = 0x20;

/// AES-XTS IV size in bytes.
///
/// Corresponds to upstream `AesXtsStorage::IvSize`.
pub const IV_SIZE: usize = 0x10;

/// AES-XTS storage layer (read-only).
///
/// Corresponds to upstream `AesXtsStorage`.
pub struct AesXtsStorage {
    base_storage: VirtualFile,
    key: [u8; KEY_SIZE],
    iv: [u8; IV_SIZE],
    block_size: usize,
    mutex: Mutex<()>,
}

impl AesXtsStorage {
    /// Create a new AES-XTS storage.
    ///
    /// key1 and key2 must each be KEY_SIZE/2 (16) bytes.
    /// iv must be IV_SIZE (16) bytes.
    /// block_size must be aligned to AES_BLOCK_SIZE.
    ///
    /// Corresponds to upstream `AesXtsStorage::AesXtsStorage`.
    pub fn new(
        base: VirtualFile,
        key1: &[u8],
        key2: &[u8],
        iv: &[u8],
        block_size: usize,
    ) -> Self {
        assert_eq!(key1.len(), KEY_SIZE / 2);
        assert_eq!(key2.len(), KEY_SIZE / 2);
        assert_eq!(iv.len(), IV_SIZE);
        assert!(
            block_size % AES_BLOCK_SIZE == 0,
            "block_size must be aligned to AES_BLOCK_SIZE"
        );

        let mut key = [0u8; KEY_SIZE];
        key[..KEY_SIZE / 2].copy_from_slice(key1);
        key[KEY_SIZE / 2..].copy_from_slice(key2);

        let mut i = [0u8; IV_SIZE];
        i.copy_from_slice(iv);

        Self {
            base_storage: base,
            key,
            iv: i,
            block_size,
            mutex: Mutex::new(()),
        }
    }

    /// Make an AES-XTS IV from an offset and block size.
    ///
    /// The IV is structured as:
    /// - bytes [0..8]: zero
    /// - bytes [8..16]: offset / block_size (big-endian)
    ///
    /// Corresponds to upstream `AesXtsStorage::MakeAesXtsIv`.
    pub fn make_aes_xts_iv(dst: &mut [u8; IV_SIZE], offset: i64, block_size: usize) {
        assert!(offset >= 0);

        let block_index = if block_size > 0 {
            offset as u64 / block_size as u64
        } else {
            0
        };
        dst[..8].copy_from_slice(&[0u8; 8]);
        dst[8..].copy_from_slice(&block_index.to_be_bytes());
    }

    /// Get the size of the storage.
    ///
    /// Corresponds to upstream `AesXtsStorage::GetSize`.
    pub fn get_size(&self) -> usize {
        self.base_storage.get_size()
    }

    /// Read and decrypt data from the storage.
    ///
    /// Reads data from the base storage and decrypts each XTS sector using
    /// AES-XTS with Nintendo's big-endian tweak convention.
    ///
    /// Corresponds to upstream `AesXtsStorage::Read`.
    pub fn read_at(&self, buffer: &mut [u8], offset: usize) -> usize {
        let size = buffer.len();

        // Allow zero-size reads.
        if size == 0 {
            return 0;
        }

        // We can only read at block aligned offsets.
        assert!(
            offset % AES_BLOCK_SIZE == 0,
            "AesXtsStorage::read: offset must be AES-block-aligned"
        );
        assert!(
            size % AES_BLOCK_SIZE == 0,
            "AesXtsStorage::read: size must be AES-block-aligned"
        );

        let _lock = self.mutex.lock().unwrap();

        // Read the encrypted data from the base storage.
        let read = self.base_storage.read(buffer, size, offset);
        if read == 0 {
            return 0;
        }

        // Decrypt using AES-XTS with the crypto module.
        use crate::crypto::aes_util::{AesCipher, Mode, Op};
        let mut cipher = AesCipher::new_256(self.key, Mode::XTS);

        // Starting sector number.
        let start_sector = offset / self.block_size;

        cipher.xts_transcode(
            &buffer[..read].to_vec(),
            &mut buffer[..read],
            start_sector,
            self.block_size,
            Op::Decrypt,
        );

        read
    }
}

/// Implement VfsFile so AesXtsStorage can be used as a VirtualFile.
impl VfsFile for AesXtsStorage {
    fn get_name(&self) -> String {
        String::from("AesXtsStorage")
    }

    fn get_size(&self) -> usize {
        self.base_storage.get_size()
    }

    fn resize(&self, _new_size: usize) -> bool {
        false // Read-only storage
    }

    fn get_containing_directory(&self) -> Option<VirtualDir> {
        None
    }

    fn is_writable(&self) -> bool {
        false
    }

    fn is_readable(&self) -> bool {
        true
    }

    fn read(&self, data: &mut [u8], length: usize, offset: usize) -> usize {
        let actual_len = length.min(data.len());
        if actual_len == 0 {
            return 0;
        }
        self.read_at(&mut data[..actual_len], offset)
    }

    fn write(&self, _data: &[u8], _length: usize, _offset: usize) -> usize {
        0 // Read-only
    }

    fn rename(&self, _new_name: &str) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make_aes_xts_iv() {
        let mut iv = [0u8; IV_SIZE];
        AesXtsStorage::make_aes_xts_iv(&mut iv, 0x2000, 0x200);
        // block_index = 0x2000 / 0x200 = 0x10
        assert_eq!(&iv[..8], &[0u8; 8]);
        assert_eq!(
            &iv[8..],
            &[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10]
        );
    }

    #[test]
    fn test_make_aes_xts_iv_zero() {
        let mut iv = [0xFFu8; IV_SIZE];
        AesXtsStorage::make_aes_xts_iv(&mut iv, 0, 0x200);
        assert_eq!(iv, [0u8; 16]);
    }

    #[test]
    fn test_constants() {
        assert_eq!(AES_BLOCK_SIZE, 16);
        assert_eq!(KEY_SIZE, 32);
        assert_eq!(IV_SIZE, 16);
    }
}
