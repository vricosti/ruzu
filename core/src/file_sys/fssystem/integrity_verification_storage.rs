// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_integrity_verification_storage.h / .cpp
//
// Integrity verification storage: a read-only storage that validates data
// integrity using block-level SHA-256 hashes. Each data block has a
// corresponding hash entry in the hash storage.

use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};

/// Size of a SHA-256 hash in bytes.
///
/// Corresponds to upstream `IntegrityVerificationStorage::HashSize`.
pub const HASH_SIZE: i64 = 256 / 8;

/// A block hash entry.
///
/// Corresponds to upstream `IntegrityVerificationStorage::BlockHash`.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct BlockHash {
    pub hash: [u8; HASH_SIZE as usize],
}

impl Default for BlockHash {
    fn default() -> Self {
        Self {
            hash: [0u8; HASH_SIZE as usize],
        }
    }
}

impl BlockHash {
    /// Set the validation bit (MSB of last byte).
    ///
    /// Corresponds to upstream `IntegrityVerificationStorage::SetValidationBit`.
    pub fn set_validation_bit(&mut self) {
        self.hash[HASH_SIZE as usize - 1] |= 0x80;
    }

    /// Check whether the validation bit is set.
    ///
    /// Corresponds to upstream `IntegrityVerificationStorage::IsValidationBit`.
    pub fn is_validation_bit(&self) -> bool {
        (self.hash[HASH_SIZE as usize - 1] & 0x80) != 0
    }
}

/// Compute integer log2 for a power-of-two value.
/// Corresponds to upstream anonymous `ILog2`.
fn ilog2(val: u32) -> u32 {
    assert!(val > 0);
    31 - val.leading_zeros()
}

/// Integrity verification storage.
///
/// Validates data integrity using block-level SHA-256 hashes.
/// Each data block of `verification_block_size` bytes has a corresponding
/// 32-byte hash in the hash storage.
///
/// Corresponds to upstream `IntegrityVerificationStorage`.
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
    /// Create a new uninitialized IntegrityVerificationStorage.
    pub fn new() -> Self {
        Self {
            hash_storage: None,
            data_storage: None,
            verification_block_size: 0,
            verification_block_order: 0,
            upper_layer_verification_block_size: 0,
            upper_layer_verification_block_order: 0,
            is_real_data: false,
        }
    }

    /// Initialize the storage with hash and data storages.
    ///
    /// Corresponds to upstream `IntegrityVerificationStorage::Initialize`.
    pub fn initialize(
        &mut self,
        hash_storage: VirtualFile,
        data_storage: VirtualFile,
        verif_block_size: i64,
        upper_layer_verif_block_size: i64,
        is_real_data: bool,
    ) {
        // Validate preconditions.
        assert!(verif_block_size >= HASH_SIZE);

        // Set storages.
        self.hash_storage = Some(hash_storage);
        self.data_storage = Some(data_storage);

        // Set verification block sizes.
        self.verification_block_size = verif_block_size;
        self.verification_block_order = ilog2(verif_block_size as u32) as i64;
        assert!(self.verification_block_size == 1i64 << self.verification_block_order);

        // Set upper layer block sizes.
        let upper = std::cmp::max(upper_layer_verif_block_size, HASH_SIZE);
        self.upper_layer_verification_block_size = upper;
        self.upper_layer_verification_block_order = ilog2(upper as u32) as i64;
        assert!(self.upper_layer_verification_block_size == 1i64 << self.upper_layer_verification_block_order);

        // Set data.
        self.is_real_data = is_real_data;
    }

    /// Finalize the storage, releasing references.
    ///
    /// Corresponds to upstream `IntegrityVerificationStorage::Finalize`.
    pub fn finalize(&mut self) {
        self.hash_storage = None;
        self.data_storage = None;
    }

    /// Get the verification block size.
    ///
    /// Corresponds to upstream `IntegrityVerificationStorage::GetBlockSize`.
    pub fn get_block_size(&self) -> i64 {
        self.verification_block_size
    }

    /// Whether this storage contains real (non-zero) data.
    pub fn is_real_data(&self) -> bool {
        self.is_real_data
    }
}

impl Default for IntegrityVerificationStorage {
    fn default() -> Self {
        Self::new()
    }
}

/// Implement VfsFile so IntegrityVerificationStorage can be used as a VirtualFile.
impl VfsFile for IntegrityVerificationStorage {
    fn get_name(&self) -> String {
        String::from("IntegrityVerificationStorage")
    }

    fn get_size(&self) -> usize {
        match &self.data_storage {
            Some(ds) => ds.get_size(),
            None => 0,
        }
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

    /// Read data from the storage.
    ///
    /// In upstream, this reads data in block-aligned chunks, computes
    /// SHA-256 for each block, and validates against the hash storage.
    /// Currently reads without hash verification (deferred until crypto pipeline is wired up).
    ///
    /// Corresponds to upstream `IntegrityVerificationStorage::Read`.
    fn read(&self, buffer: &mut [u8], length: usize, offset: usize) -> usize {
        if length == 0 {
            return 0;
        }

        match &self.data_storage {
            Some(ds) => {
                let data_size = ds.get_size();

                // Determine the read extents.
                let mut read_size = length;
                if offset + read_size > data_size {
                    // Determine the padding sizes.
                    let padding_offset = data_size.saturating_sub(offset);
                    if self.verification_block_size > 0 {
                        let block_mask = (self.verification_block_size - 1) as usize;
                        let padding_size =
                            self.verification_block_size as usize - (padding_offset & block_mask);
                        if (padding_size as i64) < self.verification_block_size {
                            // Clear the padding.
                            let end = (padding_offset + padding_size).min(buffer.len());
                            for b in &mut buffer[padding_offset..end] {
                                *b = 0;
                            }
                        }
                    }

                    // Set the new in-bounds size.
                    read_size = data_size.saturating_sub(offset);
                }

                // Perform the read.
                ds.read(buffer, read_size, offset)
            }
            None => 0,
        }
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
    fn test_block_hash_validation_bit() {
        let mut hash = BlockHash::default();
        assert!(!hash.is_validation_bit());
        hash.set_validation_bit();
        assert!(hash.is_validation_bit());
        assert_eq!(hash.hash[31] & 0x80, 0x80);
    }

    #[test]
    fn test_hash_size() {
        assert_eq!(HASH_SIZE, 32);
        assert_eq!(std::mem::size_of::<BlockHash>(), 32);
    }

    #[test]
    fn test_uninitialized_storage() {
        let storage = IntegrityVerificationStorage::new();
        assert_eq!(storage.get_size(), 0);
        assert_eq!(storage.get_block_size(), 0);
    }

    #[test]
    fn test_ilog2() {
        assert_eq!(ilog2(1), 0);
        assert_eq!(ilog2(2), 1);
        assert_eq!(ilog2(4), 2);
        assert_eq!(ilog2(8), 3);
        assert_eq!(ilog2(1024), 10);
        assert_eq!(ilog2(0x4000), 14);
    }
}
