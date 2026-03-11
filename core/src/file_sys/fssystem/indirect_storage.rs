// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_indirect_storage.h / .cpp

use super::bucket_tree::BucketTree;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use common::ResultCode;

pub const STORAGE_COUNT: i32 = 2;
pub const NODE_SIZE: usize = 16 * 1024;

/// Entry within the indirect storage bucket tree.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Entry {
    pub virt_offset: [u8; 8],
    pub phys_offset: [u8; 8],
    pub storage_index: i32,
}
const _: () = assert!(std::mem::size_of::<Entry>() == 0x14);

impl Entry {
    pub fn get_virtual_offset(&self) -> i64 { i64::from_le_bytes(self.virt_offset) }
    pub fn set_virtual_offset(&mut self, ofs: i64) { self.virt_offset = ofs.to_le_bytes(); }
    pub fn get_physical_offset(&self) -> i64 { i64::from_le_bytes(self.phys_offset) }
    pub fn set_physical_offset(&mut self, ofs: i64) { self.phys_offset = ofs.to_le_bytes(); }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct EntryData {
    pub virt_offset: i64,
    pub phys_offset: i64,
    pub storage_index: i32,
}

impl EntryData {
    pub fn from_entry(entry: &Entry) -> Self {
        Self {
            virt_offset: entry.get_virtual_offset(),
            phys_offset: entry.get_physical_offset(),
            storage_index: entry.storage_index,
        }
    }
}

pub struct IndirectStorage {
    table: BucketTree,
    data_storage: [Option<VirtualFile>; STORAGE_COUNT as usize],
}

impl IndirectStorage {
    pub fn new() -> Self {
        Self { table: BucketTree::new(), data_storage: [None, None] }
    }

    pub fn initialize(&mut self, node_storage: VirtualFile, entry_storage: VirtualFile, entry_count: i32) -> Result<(), ResultCode> {
        self.table.initialize(node_storage, entry_storage, NODE_SIZE, std::mem::size_of::<Entry>(), entry_count)
    }

    pub fn finalize(&mut self) { self.table.finalize(); self.data_storage = [None, None]; }
    pub fn is_initialized(&self) -> bool { self.table.is_initialized() }

    pub fn set_storage(&mut self, idx: i32, storage: VirtualFile) {
        assert!(0 <= idx && idx < STORAGE_COUNT);
        self.data_storage[idx as usize] = Some(storage);
    }

    pub fn get_size(&self) -> usize {
        if let Ok(offsets) = self.table.get_offsets() { offsets.end_offset as usize } else { 0 }
    }

    pub fn get_entry_table(&mut self) -> &mut BucketTree { &mut self.table }

    /// Stub: reads from storage[0].
    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        if let Some(ref storage) = self.data_storage[0] {
            storage.read(buffer, buffer.len(), offset)
        } else { 0 }
    }

    pub fn query_header_storage_size() -> i64 { BucketTree::query_header_storage_size() }
    pub fn query_node_storage_size(entry_count: i32) -> i64 { BucketTree::query_node_storage_size(NODE_SIZE, std::mem::size_of::<Entry>(), entry_count) }
    pub fn query_entry_storage_size(entry_count: i32) -> i64 { BucketTree::query_entry_storage_size(NODE_SIZE, std::mem::size_of::<Entry>(), entry_count) }
}

impl Default for IndirectStorage { fn default() -> Self { Self::new() } }
