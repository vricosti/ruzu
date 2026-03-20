// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_hierarchical_sha256_storage.h / .cpp
// Status: COMPLETE (structural parity; hash verification deferred)
//
// Hierarchical SHA-256 storage: a read-only storage that validates data
// integrity using a two-level hash tree. Layer 0 is the master hash,
// layer 1 is the hash table, and layer 2 is the data storage.

use std::sync::Mutex;

use crate::file_sys::errors;
use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use common::ResultCode;

/// Compute floor(log2(value)), assuming value > 0 and is a power of two.
///
/// Corresponds to upstream anonymous `Log2`.
fn log2(mut value: i32) -> i32 {
    assert!(value > 0);
    assert!(value.count_ones() == 1, "value must be a power of two");

    let mut log = 0;
    value >>= 1;
    while value > 0 {
        log += 1;
        value >>= 1;
    }
    log
}

/// Hierarchical SHA-256 storage.
///
/// Corresponds to upstream `HierarchicalSha256Storage`.
pub struct HierarchicalSha256Storage {
    base_storage: Option<VirtualFile>,
    base_storage_size: i64,
    hash_buffer: Vec<u8>,
    hash_buffer_size: usize,
    hash_target_block_size: i32,
    log_size_ratio: i32,
    mutex: Mutex<()>,
}

impl HierarchicalSha256Storage {
    /// Number of layers in the hash hierarchy.
    pub const LAYER_COUNT: i32 = 3;

    /// Size of a SHA-256 hash in bytes.
    pub const HASH_SIZE: usize = 256 / 8;

    /// Create a new uninitialized HierarchicalSha256Storage.
    pub fn new() -> Self {
        Self {
            base_storage: None,
            base_storage_size: 0,
            hash_buffer: Vec::new(),
            hash_buffer_size: 0,
            hash_target_block_size: 0,
            log_size_ratio: 0,
            mutex: Mutex::new(()),
        }
    }

    /// Initialize the storage with base storages and hash buffer.
    ///
    /// `base_storages` must contain exactly 3 entries:
    /// - [0]: master hash storage (single hash)
    /// - [1]: hash table storage
    /// - [2]: data storage
    ///
    /// Corresponds to upstream `HierarchicalSha256Storage::Initialize`.
    pub fn initialize(
        &mut self,
        base_storages: &[VirtualFile],
        layer_count: i32,
        htbs: usize,
        hash_buf: &[u8],
    ) -> Result<(), ResultCode> {
        // Validate preconditions.
        assert!(layer_count == Self::LAYER_COUNT);
        assert!(htbs.is_power_of_two());

        // Set size tracking members.
        self.hash_target_block_size = htbs as i32;
        self.log_size_ratio = log2(self.hash_target_block_size / Self::HASH_SIZE as i32);

        // Get the base storage size.
        self.base_storage_size = base_storages[2].get_size() as i64;

        // Validate that base storage size is within bounds.
        let max_size = (Self::HASH_SIZE as i64) << self.log_size_ratio << self.log_size_ratio;
        if self.base_storage_size > max_size {
            self.base_storage_size = 0;
            return Err(errors::RESULT_HIERARCHICAL_SHA256_BASE_STORAGE_TOO_LARGE);
        }

        // Set hash buffer tracking members.
        self.base_storage = Some(base_storages[2].clone());

        // Copy hash buffer.
        self.hash_buffer = hash_buf.to_vec();
        self.hash_buffer_size = hash_buf.len();

        // Read the master hash from layer 0.
        let mut master_hash = [0u8; Self::HASH_SIZE];
        base_storages[0].read(&mut master_hash, Self::HASH_SIZE, 0);

        // Read and validate the hash table data from layer 1.
        let hash_storage_size = base_storages[1].get_size() as i64;
        assert!(hash_storage_size % Self::HASH_SIZE as i64 == 0);
        assert!(hash_storage_size <= self.hash_target_block_size as i64);
        assert!(hash_storage_size <= self.hash_buffer_size as i64);

        // Read hash table into the hash buffer.
        self.hash_buffer.resize(hash_storage_size as usize, 0);
        base_storages[1].read(&mut self.hash_buffer, hash_storage_size as usize, 0);

        // Upstream TODO: master hash verification is read but not checked.
        // Upstream reads the master hash (base_storages[0]) but R_SUCCEED()s
        // without comparing it against SHA-256(hash_buffer).

        Ok(())
    }

    /// Get the size of the data storage.
    ///
    /// Corresponds to upstream `HierarchicalSha256Storage::GetSize`.
    pub fn get_size(&self) -> usize {
        if let Some(ref bs) = self.base_storage {
            bs.get_size()
        } else {
            0
        }
    }

    /// Read data from the storage.
    ///
    /// Corresponds to upstream `HierarchicalSha256Storage::Read`.
    /// In upstream, this acquires the mutex and validates block hashes.
    /// Currently reads without hash verification.
    pub fn read(&self, buffer: &mut [u8], size: usize, offset: usize) -> usize {
        // Succeed if zero-size.
        if size == 0 {
            return 0;
        }

        let _lock = self.mutex.lock().unwrap();

        // Read the data from the base storage.
        if let Some(ref bs) = self.base_storage {
            // Upstream TODO: block hash validation not implemented in upstream either.
            // Upstream Read() (fssystem_hierarchical_sha256_storage.cpp:69-80)
            // reads directly from base_storage without hash verification.
            bs.read(buffer, size, offset)
        } else {
            0
        }
    }
}

/// Implement VfsFile so HierarchicalSha256Storage can be used as a VirtualFile.
impl VfsFile for HierarchicalSha256Storage {
    fn get_name(&self) -> String {
        String::from("HierarchicalSha256Storage")
    }

    fn get_size(&self) -> usize {
        self.get_size()
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
        let actual = length.min(data.len());
        if actual == 0 {
            return 0;
        }
        self.read(&mut data[..actual], actual, offset)
    }

    fn write(&self, _data: &[u8], _length: usize, _offset: usize) -> usize {
        0
    }

    fn rename(&self, _new_name: &str) -> bool {
        false
    }
}

impl Default for HierarchicalSha256Storage {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_log2() {
        assert_eq!(log2(1), 0);
        assert_eq!(log2(2), 1);
        assert_eq!(log2(4), 2);
        assert_eq!(log2(8), 3);
        assert_eq!(log2(1024), 10);
    }

    #[test]
    fn test_constants() {
        assert_eq!(HierarchicalSha256Storage::LAYER_COUNT, 3);
        assert_eq!(HierarchicalSha256Storage::HASH_SIZE, 32);
    }

    #[test]
    fn test_empty_storage() {
        let storage = HierarchicalSha256Storage::new();
        assert_eq!(storage.get_size(), 0);
        let mut buf = [0u8; 10];
        assert_eq!(storage.read(&mut buf, 10, 0), 0);
    }
}
