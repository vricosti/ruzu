// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_compressed_storage.h

use super::bucket_tree::BucketTree;
use super::compression_common::{
    CompressionType, GetDecompressorFunction, COMPRESSION_BLOCK_ALIGNMENT,
    compression_type_utility,
};
use super::pooled_buffer::PooledBuffer;
use crate::file_sys::errors::*;
use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use common::ResultCode;

pub const NODE_SIZE: usize = 16 * 1024;

/// Compressed storage entry.
/// Corresponds to upstream `CompressedStorage::Entry`.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Entry {
    pub virt_offset: i64,
    pub phys_offset: i64,
    pub compression_type: CompressionType,
    pub _padding: [u8; 3],
    pub phys_size: i32,
}
const _: () = assert!(std::mem::size_of::<Entry>() == 0x18);

impl Entry {
    pub fn get_physical_size(&self) -> i64 {
        self.phys_size as i64
    }
}

/// Maximum number of entries to batch during a read operation.
/// Corresponds to upstream `EntriesCountMax`.
const ENTRIES_COUNT_MAX: usize = 0x80;

/// Batch entry used during read operations.
/// Corresponds to upstream anonymous `Entries` struct in `CompressedStorageCore::Read`.
#[derive(Debug, Clone, Copy, Default)]
struct ReadEntry {
    compression_type: CompressionType,
    gap_from_prev: u32,
    physical_size: u32,
    virtual_size: u32,
}

/// Core logic for compressed storage.
/// Corresponds to upstream `CompressedStorage::CompressedStorageCore`.
struct CompressedStorageCore {
    table: BucketTree,
    data_storage: Option<VirtualFile>,
    block_size_max: usize,
    continuous_reading_size_max: usize,
    get_decompressor_function: Option<GetDecompressorFunction>,
}

impl CompressedStorageCore {
    pub fn new() -> Self {
        Self {
            table: BucketTree::new(),
            data_storage: None,
            block_size_max: 0,
            continuous_reading_size_max: 0,
            get_decompressor_function: None,
        }
    }

    pub fn initialize(
        &mut self,
        data_storage: VirtualFile,
        node_storage: VirtualFile,
        entry_storage: VirtualFile,
        bktr_entry_count: i32,
        block_size_max: usize,
        continuous_reading_size_max: usize,
        get_decompressor: GetDecompressorFunction,
    ) -> Result<(), ResultCode> {
        // Check pre-conditions.
        assert!(block_size_max > 0);
        assert!(block_size_max <= continuous_reading_size_max);

        // Initialize our entry table.
        self.table.initialize(
            node_storage,
            entry_storage,
            NODE_SIZE,
            std::mem::size_of::<Entry>(),
            bktr_entry_count,
        )?;

        // Set our other fields.
        self.block_size_max = block_size_max;
        self.continuous_reading_size_max = continuous_reading_size_max;
        self.data_storage = Some(data_storage);
        self.get_decompressor_function = Some(get_decompressor);

        Ok(())
    }

    pub fn finalize(&mut self) {
        if self.is_initialized() {
            self.table.finalize();
            self.data_storage = None;
        }
    }

    pub fn get_data_storage(&self) -> Option<&VirtualFile> {
        self.data_storage.as_ref()
    }

    pub fn get_data_storage_size(&self) -> Result<i64, ResultCode> {
        if let Some(ref ds) = self.data_storage {
            Ok(ds.get_size() as i64)
        } else {
            Err(RESULT_INVALID_ARGUMENT)
        }
    }

    pub fn get_entry_table(&mut self) -> &mut BucketTree {
        &mut self.table
    }

    pub fn get_size(&self) -> Result<i64, ResultCode> {
        let offsets = self.table.get_offsets()?;
        Ok(offsets.end_offset)
    }

    pub fn is_initialized(&self) -> bool {
        self.table.is_initialized()
    }
}

/// Cache manager for compressed storage.
/// Corresponds to upstream `CompressedStorage::CacheManager`.
struct CacheManager {
    storage_size: i64,
}

impl CacheManager {
    pub fn new() -> Self {
        Self { storage_size: 0 }
    }

    /// Initialize the cache manager.
    /// Corresponds to upstream `CacheManager::Initialize`.
    pub fn initialize(
        &mut self,
        storage_size: i64,
        _cache_size_0: usize,
        _cache_size_1: usize,
        _max_cache_entries: i32,
    ) -> Result<(), ResultCode> {
        self.storage_size = storage_size;
        Ok(())
    }

    /// Read data from the compressed storage via the core.
    /// Corresponds to upstream `CacheManager::Read`.
    ///
    /// This is a simplified implementation that reads directly from the
    /// data storage without the full caching/decompression pipeline.
    /// The read delegates to the core's Read method.
    pub fn read(
        &self,
        core: &CompressedStorageCore,
        offset: i64,
        buffer: &mut [u8],
        size: usize,
    ) -> Result<(), ResultCode> {
        // If we have nothing to read, succeed.
        if size == 0 {
            return Ok(());
        }

        // Check that the read is in bounds.
        if offset > self.storage_size {
            return Err(RESULT_INVALID_OFFSET);
        }

        // Determine how much we can read.
        let read_size = std::cmp::min(size, (self.storage_size - offset) as usize);

        // Read from the data storage directly.
        // In the full implementation, this would go through the core's Read method
        // with decompression and caching. For now, read directly.
        if let Some(ref ds) = core.data_storage {
            ds.read(buffer, read_size, offset as usize);
        }

        Ok(())
    }
}

/// Compressed storage.
/// Corresponds to upstream `CompressedStorage`.
pub struct CompressedStorage {
    core: CompressedStorageCore,
    cache_manager: CacheManager,
}

impl CompressedStorage {
    pub fn new() -> Self {
        Self {
            core: CompressedStorageCore::new(),
            cache_manager: CacheManager::new(),
        }
    }

    /// Initialize the compressed storage.
    /// Corresponds to upstream `CompressedStorage::Initialize`.
    pub fn initialize(
        &mut self,
        data_storage: VirtualFile,
        node_storage: VirtualFile,
        entry_storage: VirtualFile,
        bktr_entry_count: i32,
        block_size_max: usize,
        continuous_reading_size_max: usize,
        get_decompressor: GetDecompressorFunction,
        cache_size_0: usize,
        cache_size_1: usize,
        max_cache_entries: i32,
    ) -> Result<(), ResultCode> {
        // Initialize our core.
        self.core.initialize(
            data_storage,
            node_storage,
            entry_storage,
            bktr_entry_count,
            block_size_max,
            continuous_reading_size_max,
            get_decompressor,
        )?;

        // Get our core size.
        let core_size = self.core.get_size()?;

        // Initialize our cache manager.
        self.cache_manager.initialize(
            core_size,
            cache_size_0,
            cache_size_1,
            max_cache_entries,
        )?;

        Ok(())
    }

    /// Finalize the compressed storage.
    /// Corresponds to upstream `CompressedStorage::Finalize`.
    pub fn finalize(&mut self) {
        self.core.finalize();
    }

    /// Get the data storage.
    /// Corresponds to upstream `CompressedStorage::GetDataStorage`.
    pub fn get_data_storage(&self) -> Option<&VirtualFile> {
        self.core.get_data_storage()
    }

    /// Get the data storage size.
    /// Corresponds to upstream `CompressedStorage::GetDataStorageSize`.
    pub fn get_data_storage_size(&self) -> Result<i64, ResultCode> {
        self.core.get_data_storage_size()
    }

    /// Get the entry table.
    /// Corresponds to upstream `CompressedStorage::GetEntryTable`.
    pub fn get_entry_table(&mut self) -> &mut BucketTree {
        self.core.get_entry_table()
    }

    /// Query the node storage size for a given entry count.
    /// Corresponds to upstream `CompressedStorage::QueryNodeStorageSize`.
    pub fn query_node_storage_size(entry_count: i32) -> i64 {
        BucketTree::query_node_storage_size(NODE_SIZE, std::mem::size_of::<Entry>(), entry_count)
    }

    /// Query the entry storage size for a given entry count.
    /// Corresponds to upstream `CompressedStorage::QueryEntryStorageSize`.
    pub fn query_entry_storage_size(entry_count: i32) -> i64 {
        BucketTree::query_entry_storage_size(NODE_SIZE, std::mem::size_of::<Entry>(), entry_count)
    }
}

impl Default for CompressedStorage {
    fn default() -> Self {
        Self::new()
    }
}

/// Implement VfsFile so CompressedStorage can be used as a VirtualFile.
impl VfsFile for CompressedStorage {
    fn get_name(&self) -> String {
        String::from("CompressedStorage")
    }

    fn get_size(&self) -> usize {
        self.core.get_size().unwrap_or(0) as usize
    }

    fn resize(&self, _new_size: usize) -> bool {
        false // Read-only
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

    /// Read data from the compressed storage.
    /// Corresponds to upstream `CompressedStorage::Read`.
    fn read(&self, buffer: &mut [u8], length: usize, offset: usize) -> usize {
        let actual_len = length.min(buffer.len());
        if self
            .cache_manager
            .read(&self.core, offset as i64, &mut buffer[..actual_len], actual_len)
            .is_ok()
        {
            actual_len
        } else {
            0
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
    fn test_entry_size() {
        assert_eq!(std::mem::size_of::<Entry>(), 0x18);
    }

    #[test]
    fn test_node_size() {
        assert_eq!(NODE_SIZE, 16 * 1024);
    }

    #[test]
    fn test_new() {
        let storage = CompressedStorage::new();
        assert!(storage.get_data_storage().is_none());
    }

    #[test]
    fn test_default() {
        let _storage = CompressedStorage::default();
    }

    #[test]
    fn test_entry_get_physical_size() {
        let entry = Entry {
            virt_offset: 0,
            phys_offset: 0,
            compression_type: CompressionType::None,
            _padding: [0u8; 3],
            phys_size: 0x1000,
        };
        assert_eq!(entry.get_physical_size(), 0x1000);
    }

    #[test]
    fn test_entry_negative_physical_size() {
        let entry = Entry {
            virt_offset: 0,
            phys_offset: 0,
            compression_type: CompressionType::None,
            _padding: [0u8; 3],
            phys_size: -1,
        };
        assert_eq!(entry.get_physical_size(), -1);
    }

    #[test]
    fn test_entries_count_max() {
        assert_eq!(ENTRIES_COUNT_MAX, 0x80);
    }
}
