// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_compressed_storage.h

use super::bucket_tree::BucketTree;
use super::compression_common::{CompressionType, GetDecompressorFunction};
use crate::file_sys::vfs::vfs_types::VirtualFile;
use common::ResultCode;

pub const NODE_SIZE: usize = 16 * 1024;

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
    pub fn get_physical_size(&self) -> i64 { self.phys_size as i64 }
}

pub struct CompressedStorage {
    table: BucketTree,
    data_storage: Option<VirtualFile>,
    block_size_max: usize,
    continuous_reading_size_max: usize,
    get_decompressor: Option<GetDecompressorFunction>,
}

impl CompressedStorage {
    pub fn new() -> Self {
        Self { table: BucketTree::new(), data_storage: None, block_size_max: 0, continuous_reading_size_max: 0, get_decompressor: None }
    }

    pub fn initialize(&mut self, data_storage: VirtualFile, node_storage: VirtualFile, entry_storage: VirtualFile, bktr_entry_count: i32, block_size_max: usize, continuous_reading_size_max: usize, get_decompressor: GetDecompressorFunction, _cache_size_0: usize, _cache_size_1: usize, _max_cache_entries: i32) -> Result<(), ResultCode> {
        self.table.initialize(node_storage, entry_storage, NODE_SIZE, std::mem::size_of::<Entry>(), bktr_entry_count)?;
        self.block_size_max = block_size_max;
        self.continuous_reading_size_max = continuous_reading_size_max;
        self.data_storage = Some(data_storage);
        self.get_decompressor = Some(get_decompressor);
        Ok(())
    }

    pub fn finalize(&mut self) { self.table.finalize(); self.data_storage = None; self.get_decompressor = None; }
    pub fn get_data_storage(&self) -> Option<&VirtualFile> { self.data_storage.as_ref() }
    pub fn get_entry_table(&mut self) -> &mut BucketTree { &mut self.table }

    pub fn get_size(&self) -> usize {
        if let Ok(offsets) = self.table.get_offsets() { offsets.end_offset as usize } else { 0 }
    }

    /// Stub: reads from data storage directly.
    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        if let Some(ref storage) = self.data_storage {
            storage.read(buffer, buffer.len(), offset)
        } else { 0 }
    }

    pub fn query_node_storage_size(entry_count: i32) -> i64 { BucketTree::query_node_storage_size(NODE_SIZE, std::mem::size_of::<Entry>(), entry_count) }
    pub fn query_entry_storage_size(entry_count: i32) -> i64 { BucketTree::query_entry_storage_size(NODE_SIZE, std::mem::size_of::<Entry>(), entry_count) }
}

impl Default for CompressedStorage { fn default() -> Self { Self::new() } }
