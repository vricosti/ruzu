// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_aes_ctr_storage.h / .cpp
// Status: COMPLETE (structural parity with AES-CTR decryption implemented)
//
// AES-CTR storage layer. Reads/writes data through an AES-CTR cipher,
// operating on block-aligned accesses of BLOCK_SIZE bytes.

use super::utility::add_counter;
use crate::crypto::aes_util::{AesCipher, Mode, Op};
use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use parking_lot::Mutex;

/// AES block size in bytes.
///
/// Corresponds to upstream `AesCtrStorage::BlockSize`.
pub const BLOCK_SIZE: usize = 0x10;

/// AES-CTR key size in bytes.
///
/// Corresponds to upstream `AesCtrStorage::KeySize`.
pub const KEY_SIZE: usize = 0x10;

/// AES-CTR IV size in bytes.
///
/// Corresponds to upstream `AesCtrStorage::IvSize`.
pub const IV_SIZE: usize = 0x10;

/// AES-CTR storage layer.
///
/// Corresponds to upstream `AesCtrStorage`.
pub struct AesCtrStorage {
    base_storage: VirtualFile,
    key: [u8; KEY_SIZE],
    iv: [u8; IV_SIZE],
    /// AES cipher context; interior-mutable because cipher state changes during read.
    cipher: Mutex<AesCipher>,
}

impl AesCtrStorage {
    /// Create a new AES-CTR storage.
    ///
    /// Corresponds to upstream `AesCtrStorage::AesCtrStorage`.
    pub fn new(base: VirtualFile, key: &[u8], iv: &[u8]) -> Self {
        assert_eq!(key.len(), KEY_SIZE);
        assert_eq!(iv.len(), IV_SIZE);

        let mut k = [0u8; KEY_SIZE];
        let mut i = [0u8; IV_SIZE];
        k.copy_from_slice(key);
        i.copy_from_slice(iv);

        let cipher = AesCipher::new_128(k, Mode::CTR);

        Self {
            base_storage: base,
            key: k,
            iv: i,
            cipher: Mutex::new(cipher),
        }
    }

    /// Make an IV from an upper 64-bit value and an offset.
    ///
    /// The IV is structured as:
    /// - bytes [0..8]: upper (big-endian)
    /// - bytes [8..16]: offset / BlockSize (big-endian)
    ///
    /// Corresponds to upstream `AesCtrStorage::MakeIv`.
    pub fn make_iv(dst: &mut [u8; IV_SIZE], upper: u64, offset: i64) {
        assert!(offset >= 0);

        let upper_bytes = upper.to_be_bytes();
        dst[..8].copy_from_slice(&upper_bytes);
        let lower = (offset / BLOCK_SIZE as i64) as u64;
        let lower_bytes = lower.to_be_bytes();
        dst[8..].copy_from_slice(&lower_bytes);
    }

    /// Get the size of the storage.
    ///
    /// Corresponds to upstream `AesCtrStorage::GetSize`.
    pub fn get_size(&self) -> usize {
        self.base_storage.get_size()
    }

    /// Read and decrypt data from the storage.
    ///
    /// In upstream, this reads data in block-aligned chunks and decrypts
    /// using AES-CTR. Currently reads without decryption.
    ///
    /// Corresponds to upstream `AesCtrStorage::Read`.
    pub fn read_at(&self, buffer: &mut [u8], offset: usize) -> usize {
        let size = buffer.len();

        // Allow zero-size reads.
        if size == 0 {
            return 0;
        }

        // We can only read at block aligned offsets.
        assert!(
            offset % BLOCK_SIZE == 0,
            "AesCtrStorage::read: offset must be block-aligned"
        );
        assert!(
            size % BLOCK_SIZE == 0,
            "AesCtrStorage::read: size must be block-aligned"
        );

        // Read the encrypted data.
        let read_len = self.base_storage.read(buffer, size, offset);

        // Setup the counter.
        let mut ctr = self.iv;
        add_counter(&mut ctr, (offset / BLOCK_SIZE) as u64);

        // Decrypt using AES-CTR.
        let mut cipher = self.cipher.lock();
        cipher.set_iv(&ctr);
        let src = buffer[..read_len].to_vec();
        cipher.transcode(&src, &mut buffer[..read_len], Op::Decrypt);

        size
    }

    /// Write encrypted data to the storage.
    ///
    /// In upstream, this encrypts data using AES-CTR before writing.
    /// Currently writes without encryption.
    ///
    /// Corresponds to upstream `AesCtrStorage::Write`.
    pub fn write_at(&self, buffer: &[u8], offset: usize) -> usize {
        let size = buffer.len();

        // Allow zero-size writes.
        if size == 0 {
            return 0;
        }

        // We can only write at block aligned offsets.
        assert!(
            offset % BLOCK_SIZE == 0,
            "AesCtrStorage::write: offset must be block-aligned"
        );
        assert!(
            size % BLOCK_SIZE == 0,
            "AesCtrStorage::write: size must be block-aligned"
        );

        // Setup the counter.
        let mut ctr = self.iv;
        add_counter(&mut ctr, (offset / BLOCK_SIZE) as u64);

        // Encrypt data before writing, matching upstream AesCtrStorage::Write.
        let mut encrypted = vec![0u8; size];
        {
            let mut cipher = self.cipher.lock();
            cipher.set_iv(&ctr);
            cipher.transcode(&buffer[..size], &mut encrypted, Op::Encrypt);
        }
        self.base_storage.write(&encrypted, size, offset)
    }
}

/// Implement VfsFile so AesCtrStorage can be used as a VirtualFile.
impl VfsFile for AesCtrStorage {
    fn get_name(&self) -> String {
        String::from("AesCtrStorage")
    }

    fn get_size(&self) -> usize {
        self.base_storage.get_size()
    }

    fn resize(&self, _new_size: usize) -> bool {
        false
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

        // If already block-aligned, use the direct path.
        if offset % BLOCK_SIZE == 0 && actual_len % BLOCK_SIZE == 0 {
            return self.read_at(&mut data[..actual_len], offset);
        }

        // Handle non-block-aligned reads by reading aligned blocks and copying
        // the relevant portion.
        let aligned_offset = offset / BLOCK_SIZE * BLOCK_SIZE;
        let end = offset + actual_len;
        let aligned_end = (end + BLOCK_SIZE - 1) / BLOCK_SIZE * BLOCK_SIZE;
        let aligned_size = aligned_end - aligned_offset;

        let mut aligned_buf = vec![0u8; aligned_size];
        self.read_at(&mut aligned_buf, aligned_offset);

        let start_in_buf = offset - aligned_offset;
        data[..actual_len].copy_from_slice(&aligned_buf[start_in_buf..start_in_buf + actual_len]);
        actual_len
    }

    fn write(&self, data: &[u8], length: usize, offset: usize) -> usize {
        let actual_len = length.min(data.len());
        if actual_len == 0 {
            return 0;
        }
        self.write_at(&data[..actual_len], offset)
    }

    fn rename(&self, _new_name: &str) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make_iv() {
        let mut iv = [0u8; IV_SIZE];
        AesCtrStorage::make_iv(&mut iv, 0x0102030405060708, 0x100);
        // upper: big-endian 0x0102030405060708
        assert_eq!(&iv[..8], &[0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]);
        // lower: 0x100 / 0x10 = 0x10, big-endian
        assert_eq!(
            &iv[8..],
            &[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10]
        );
    }

    #[test]
    fn test_make_iv_zero() {
        let mut iv = [0xFFu8; IV_SIZE];
        AesCtrStorage::make_iv(&mut iv, 0, 0);
        assert_eq!(iv, [0u8; 16]);
    }

    #[test]
    fn test_constants() {
        assert_eq!(BLOCK_SIZE, 16);
        assert_eq!(KEY_SIZE, 16);
        assert_eq!(IV_SIZE, 16);
    }
}
