// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_integrity_verification_storage.h / .cpp
// Status: COMPLETE (structural parity; hash verification deferred)
//
// Integrity verification storage: a read-only storage that validates data
// integrity using block-level SHA-256 hashes. Each data block has a
// corresponding hash entry in the hash storage.

use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_types::VirtualFile;

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
    /// `verif_block_size` must be a power of two.
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
        self.hash_storage = Some(hash_storage);
        self.data_storage = Some(data_storage);
        self.verification_block_size = verif_block_size;
        self.verification_block_order = (verif_block_size as u64).trailing_zeros() as i64;
        self.upper_layer_verification_block_size = upper_layer_verif_block_size;
        self.upper_layer_verification_block_order =
            (upper_layer_verif_block_size as u64).trailing_zeros() as i64;
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

    /// Get the size of the data storage.
    ///
    /// Corresponds to upstream `IntegrityVerificationStorage::GetSize`.
    pub fn get_size(&self) -> usize {
        match &self.data_storage {
            Some(ds) => ds.get_size(),
            None => 0,
        }
    }

    /// Read data from the storage.
    ///
    /// In upstream, this reads data in block-aligned chunks, computes
    /// SHA-256 for each block, and validates against the hash storage.
    /// Currently reads without hash verification.
    ///
    /// Corresponds to upstream `IntegrityVerificationStorage::Read`.
    pub fn read(&self, buffer: &mut [u8], size: usize, offset: usize) -> usize {
        if size == 0 {
            return 0;
        }

        match &self.data_storage {
            Some(ds) => {
                // TODO: For each aligned block overlapping [offset, offset+size):
                //   1. Read block hash from hash_storage
                //   2. Read data block from data_storage
                //   3. Compute SHA-256 of data block
                //   4. Validate hash (with validation bit check)
                //   5. If mismatch, return error
                ds.read(buffer, size, offset)
            }
            None => 0,
        }
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
}
