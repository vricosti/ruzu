// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_hierarchical_sha3_storage.h / .cpp

use crate::file_sys::errors;
use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use common::ResultCode;

fn log2(mut value: i32) -> i32 {
    assert!(value > 0);
    assert!(value.count_ones() == 1);

    let mut log = 0;
    value >>= 1;
    while value > 0 {
        log += 1;
        value >>= 1;
    }
    log
}

/// Hierarchical SHA3-256 storage.
/// Corresponds to upstream `HierarchicalSha3Storage`.
pub struct HierarchicalSha3Storage {
    base_storage: Option<VirtualFile>,
    base_storage_size: i64,
    hash_buffer: Vec<u8>,
    hash_buffer_size: usize,
    hash_target_block_size: i32,
    log_size_ratio: i32,
}

impl HierarchicalSha3Storage {
    pub const LAYER_COUNT: i32 = 3;
    pub const HASH_SIZE: usize = 256 / 8;

    pub fn new() -> Self {
        Self {
            base_storage: None,
            base_storage_size: 0,
            hash_buffer: Vec::new(),
            hash_buffer_size: 0,
            hash_target_block_size: 0,
            log_size_ratio: 0,
        }
    }

    /// Corresponds to upstream `HierarchicalSha3Storage::Initialize`.
    pub fn initialize(
        &mut self,
        base_storages: &[VirtualFile],
        layer_count: i32,
        htbs: usize,
        hash_buf: &[u8],
    ) -> Result<(), ResultCode> {
        assert_eq!(layer_count, Self::LAYER_COUNT);
        assert!(htbs.is_power_of_two());

        self.hash_target_block_size = htbs as i32;
        self.log_size_ratio = log2(self.hash_target_block_size / Self::HASH_SIZE as i32);

        self.base_storage_size = base_storages[2].get_size() as i64;
        let max_size = (Self::HASH_SIZE as i64) << self.log_size_ratio << self.log_size_ratio;
        if self.base_storage_size > max_size {
            self.base_storage_size = 0;
            return Err(errors::RESULT_HIERARCHICAL_SHA256_BASE_STORAGE_TOO_LARGE);
        }

        self.base_storage = Some(base_storages[2].clone());
        self.hash_buffer = hash_buf.to_vec();
        self.hash_buffer_size = hash_buf.len();

        let mut master_hash = [0u8; Self::HASH_SIZE];
        base_storages[0].read(&mut master_hash, Self::HASH_SIZE, 0);

        let hash_storage_size = base_storages[1].get_size() as i64;
        assert_eq!(hash_storage_size % Self::HASH_SIZE as i64, 0);
        assert!(hash_storage_size <= self.hash_target_block_size as i64);
        assert!(hash_storage_size <= self.hash_buffer_size as i64);

        self.hash_buffer.resize(hash_storage_size as usize, 0);
        base_storages[1].read(&mut self.hash_buffer, hash_storage_size as usize, 0);
        Ok(())
    }

    pub fn get_size(&self) -> usize {
        self.base_storage
            .as_ref()
            .map_or(0, |storage| storage.get_size())
    }

    /// Corresponds to upstream `HierarchicalSha3Storage::Read`.
    pub fn read(&self, buffer: &mut [u8], size: usize, offset: usize) -> usize {
        if size == 0 {
            return 0;
        }
        self.base_storage
            .as_ref()
            .map_or(0, |storage| storage.read(buffer, size, offset))
    }
}

impl VfsFile for HierarchicalSha3Storage {
    fn get_name(&self) -> String {
        String::from("HierarchicalSha3Storage")
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
        self.read(&mut data[..actual], actual, offset)
    }

    fn write(&self, _data: &[u8], _length: usize, _offset: usize) -> usize {
        0
    }

    fn rename(&self, _new_name: &str) -> bool {
        false
    }
}

impl Default for HierarchicalSha3Storage {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs_vector::VectorVfsFile;
    use std::sync::Arc;

    #[test]
    fn constants_match_upstream() {
        assert_eq!(HierarchicalSha3Storage::LAYER_COUNT, 3);
        assert_eq!(HierarchicalSha3Storage::HASH_SIZE, 32);
    }

    #[test]
    fn initialized_storage_reads_from_the_data_layer() {
        let master: VirtualFile = Arc::new(VectorVfsFile::new(
            vec![0; HierarchicalSha3Storage::HASH_SIZE],
            "master.sha3".to_string(),
            None,
        ));
        let hashes: VirtualFile = Arc::new(VectorVfsFile::new(
            vec![0; HierarchicalSha3Storage::HASH_SIZE],
            "hashes.sha3".to_string(),
            None,
        ));
        let data_bytes: Vec<u8> = (0..HierarchicalSha3Storage::HASH_SIZE)
            .map(|value| value as u8)
            .collect();
        let data: VirtualFile = Arc::new(VectorVfsFile::new(
            data_bytes.clone(),
            "data.bin".to_string(),
            None,
        ));

        let mut storage = HierarchicalSha3Storage::new();
        storage
            .initialize(
                &[master, hashes, data],
                HierarchicalSha3Storage::LAYER_COUNT,
                HierarchicalSha3Storage::HASH_SIZE,
                &[0; HierarchicalSha3Storage::HASH_SIZE],
            )
            .unwrap();

        let mut output = [0; 8];
        let output_size = output.len();
        assert_eq!(storage.read(&mut output, output_size, 4), output_size);
        assert_eq!(output, data_bytes[4..12]);
    }
}
