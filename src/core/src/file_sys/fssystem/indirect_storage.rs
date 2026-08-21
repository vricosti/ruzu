// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_indirect_storage.h / .cpp
// Status: COMPLETE (structural parity; read uses BucketTree Visitor for entry resolution)
//
// Indirect storage: maps virtual address ranges to physical storage locations
// using a BucketTree-based entry table. Each entry specifies which of the
// configured storages (0 or 1) holds the data for a given virtual range.

use std::sync::Arc;

use super::bucket_tree::{BucketTree, BucketTreeHeader};
use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_offset::OffsetVfsFile;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use common::ResultCode;

/// Number of backing storages in an indirect storage.
///
/// Corresponds to upstream `IndirectStorage::StorageCount`.
pub const STORAGE_COUNT: i32 = 2;

/// Node size for the bucket tree.
///
/// Corresponds to upstream `IndirectStorage::NodeSize`.
pub const NODE_SIZE: usize = 16 * 1024;

/// Entry within the indirect storage bucket tree.
///
/// The virtual and physical offsets are stored as raw little-endian bytes
/// to match the on-disk binary layout exactly.
///
/// Corresponds to upstream `IndirectStorage::Entry`.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Entry {
    pub virt_offset: [u8; 8],
    pub phys_offset: [u8; 8],
    pub storage_index: i32,
}
const _: () = assert!(std::mem::size_of::<Entry>() == 0x14);

impl Entry {
    /// Get the virtual offset as an i64.
    pub fn get_virtual_offset(&self) -> i64 {
        i64::from_le_bytes(self.virt_offset)
    }

    /// Set the virtual offset from an i64.
    pub fn set_virtual_offset(&mut self, ofs: i64) {
        self.virt_offset = ofs.to_le_bytes();
    }

    /// Get the physical offset as an i64.
    pub fn get_physical_offset(&self) -> i64 {
        i64::from_le_bytes(self.phys_offset)
    }

    /// Set the physical offset from an i64.
    pub fn set_physical_offset(&mut self, ofs: i64) {
        self.phys_offset = ofs.to_le_bytes();
    }
}

/// Deserialized entry data for convenient access.
///
/// Corresponds to upstream `IndirectStorage::EntryData`.
#[derive(Debug, Clone, Copy, Default)]
pub struct EntryData {
    pub virt_offset: i64,
    pub phys_offset: i64,
    pub storage_index: i32,
}

impl EntryData {
    /// Create from a raw Entry.
    pub fn from_entry(entry: &Entry) -> Self {
        Self {
            virt_offset: entry.get_virtual_offset(),
            phys_offset: entry.get_physical_offset(),
            storage_index: entry.storage_index,
        }
    }
}

/// Indirect storage.
///
/// Maps virtual address ranges to physical storage locations using a
/// BucketTree-based entry table. Supports two backing storages (index 0 and 1).
///
/// Corresponds to upstream `IndirectStorage`.
pub struct IndirectStorage {
    table: BucketTree,
    data_storage: [Option<VirtualFile>; STORAGE_COUNT as usize],
}

impl IndirectStorage {
    /// Create a new uninitialized IndirectStorage.
    pub fn new() -> Self {
        Self {
            table: BucketTree::new(),
            data_storage: [None, None],
        }
    }

    /// Initialize from a single table storage file that contains the header,
    /// node data, and entry data in sequence.
    ///
    /// Corresponds to upstream `IndirectStorage::Initialize(VirtualFile table_storage)`.
    pub fn initialize_from_table(&mut self, table_storage: VirtualFile) -> Result<(), ResultCode> {
        // Read and verify the bucket tree header.
        let mut header_buf = [0u8; std::mem::size_of::<BucketTreeHeader>()];
        table_storage.read(&mut header_buf, std::mem::size_of::<BucketTreeHeader>(), 0);
        let header: BucketTreeHeader = unsafe { *(header_buf.as_ptr() as *const BucketTreeHeader) };
        header.verify()?;

        // Determine extents.
        let node_storage_size = Self::query_node_storage_size(header.entry_count);
        let entry_storage_size = Self::query_entry_storage_size(header.entry_count);
        let node_storage_offset = Self::query_header_storage_size();
        let entry_storage_offset = node_storage_offset + node_storage_size;

        // Create sub-storages and initialize.
        let node_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            table_storage.clone(),
            node_storage_size as usize,
            node_storage_offset as usize,
            String::new(),
        ));
        let entry_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            table_storage,
            entry_storage_size as usize,
            entry_storage_offset as usize,
            String::new(),
        ));

        self.initialize(node_storage, entry_storage, header.entry_count)
    }

    /// Initialize from node and entry storage files.
    ///
    /// Corresponds to upstream `IndirectStorage::Initialize(node, entry, count)`.
    pub fn initialize(
        &mut self,
        node_storage: VirtualFile,
        entry_storage: VirtualFile,
        entry_count: i32,
    ) -> Result<(), ResultCode> {
        self.table.initialize(
            node_storage,
            entry_storage,
            NODE_SIZE,
            std::mem::size_of::<Entry>(),
            entry_count,
        )
    }

    /// Finalize the storage, releasing all references.
    ///
    /// Corresponds to upstream `IndirectStorage::Finalize`.
    pub fn finalize(&mut self) {
        self.table.finalize();
        self.data_storage = [None, None];
    }

    /// Check if the storage is initialized.
    pub fn is_initialized(&self) -> bool {
        self.table.is_initialized()
    }

    /// Set a backing storage at the given index.
    ///
    /// Corresponds to upstream `IndirectStorage::SetStorage`.
    pub fn set_storage(&mut self, idx: i32, storage: VirtualFile) {
        assert!(0 <= idx && idx < STORAGE_COUNT);
        self.data_storage[idx as usize] = Some(storage);
    }

    /// Get the size of the virtual address space.
    ///
    /// Corresponds to upstream `IndirectStorage::GetSize`.
    pub fn get_size(&self) -> usize {
        if let Ok(offsets) = self.table.get_offsets() {
            offsets.end_offset as usize
        } else {
            0
        }
    }

    /// Get a mutable reference to the entry table.
    pub fn get_entry_table(&mut self) -> &mut BucketTree {
        &mut self.table
    }

    /// Read data from the indirect storage.
    ///
    /// Uses the BucketTree visitor to resolve each virtual range to the
    /// appropriate backing storage (index 0 or 1).
    ///
    /// Corresponds to upstream `IndirectStorage::Read`.
    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        assert!(self.is_initialized());
        let size = buffer.len();

        // Succeed if there's nothing to read.
        if size == 0 {
            return 0;
        }

        // If the tree is empty, read directly from storage[0].
        if self.table.is_empty() {
            if let Some(ref storage) = self.data_storage[0] {
                return storage.read(buffer, size, offset);
            }
            return 0;
        }

        // Use operate_per_entry to read from the correct storage for each range.
        self.operate_per_entry(
            offset as i64,
            size as i64,
            |storage, data_offset, cur_offset, cur_size| {
                let buf_offset = (cur_offset - offset as i64) as usize;
                let read_size = cur_size as usize;
                storage.read(
                    &mut buffer[buf_offset..buf_offset + read_size],
                    read_size,
                    data_offset as usize,
                );
                Ok(())
            },
        );

        size
    }

    /// Operate on each entry that overlaps with the given range.
    ///
    /// This matches the upstream `IndirectStorage::OperatePerEntry` pattern:
    /// 1. Find the initial entry via BucketTree::Find
    /// 2. For each iteration: read current entry, advance visitor to get next entry's
    ///    offset, compute the data range, then call the function.
    ///
    /// Corresponds to upstream `IndirectStorage::OperatePerEntry<false, true>`.
    fn operate_per_entry<F>(&self, offset: i64, size: i64, mut func: F)
    where
        F: FnMut(
            &crate::file_sys::vfs::vfs_types::VirtualFile,
            i64,
            i64,
            i64,
        ) -> Result<(), common::ResultCode>,
    {
        assert!(offset >= 0);
        assert!(size >= 0);
        assert!(self.is_initialized());

        if size == 0 {
            return;
        }

        // Get the table offsets.
        let table_offsets = match self.table.get_offsets() {
            Ok(o) => o,
            Err(_) => return,
        };

        if !table_offsets.is_include_range(offset, size) {
            return;
        }

        // Find the entry for the start offset.
        let mut visitor = match self.table.find(offset) {
            Ok(v) => v,
            Err(_) => return,
        };

        // Validate the initial entry.
        {
            let cur_entry: Entry = unsafe { *visitor.get::<Entry>() };
            let entry_offset = cur_entry.get_virtual_offset();
            if entry_offset < 0 || !table_offsets.is_include_offset(entry_offset) {
                return;
            }
        }

        let mut cur_offset = offset;
        let end_offset = offset + size;

        while cur_offset < end_offset {
            // Get the current entry.
            let cur_entry: Entry = unsafe { *visitor.get::<Entry>() };
            let cur_entry_offset = cur_entry.get_virtual_offset();

            if cur_entry_offset > cur_offset {
                break;
            }

            // Validate storage index.
            if cur_entry.storage_index < 0 || cur_entry.storage_index >= STORAGE_COUNT {
                break;
            }

            // Get the next entry offset by advancing the visitor.
            let next_entry_offset = if visitor.can_move_next() {
                if visitor.move_next().is_err() {
                    break;
                }
                let next: Entry = unsafe { *visitor.get::<Entry>() };
                let neo = next.get_virtual_offset();
                if !table_offsets.is_include_offset(neo) {
                    break;
                }
                neo
            } else {
                table_offsets.end_offset
            };

            if cur_offset >= next_entry_offset {
                break;
            }

            // Compute data range.
            let data_offset = cur_offset - cur_entry_offset;
            let data_size = next_entry_offset - cur_entry_offset;
            assert!(data_size > 0);

            let remaining = end_offset - cur_offset;
            let cur_size = remaining.min(data_size - data_offset);
            assert!(cur_size <= size);

            let cur_entry_phys_offset = cur_entry.get_physical_offset();

            // Operate on the range using the captured cur_entry (not the advanced visitor).
            if let Some(ref storage) = self.data_storage[cur_entry.storage_index as usize] {
                let _ = func(
                    storage,
                    cur_entry_phys_offset + data_offset,
                    cur_offset,
                    cur_size,
                );
            }

            cur_offset += cur_size;
        }
    }

    /// Query the header storage size.
    pub fn query_header_storage_size() -> i64 {
        BucketTree::query_header_storage_size()
    }

    /// Query the node storage size for a given entry count.
    pub fn query_node_storage_size(entry_count: i32) -> i64 {
        BucketTree::query_node_storage_size(NODE_SIZE, std::mem::size_of::<Entry>(), entry_count)
    }

    /// Query the entry storage size for a given entry count.
    pub fn query_entry_storage_size(entry_count: i32) -> i64 {
        BucketTree::query_entry_storage_size(NODE_SIZE, std::mem::size_of::<Entry>(), entry_count)
    }
}

/// Implement VfsFile so IndirectStorage can be used as a VirtualFile.
impl VfsFile for IndirectStorage {
    fn get_name(&self) -> String {
        String::from("IndirectStorage")
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
        let actual_len = length.min(data.len());
        if actual_len == 0 {
            return 0;
        }
        self.read(&mut data[..actual_len], offset)
    }

    fn write(&self, _data: &[u8], _length: usize, _offset: usize) -> usize {
        0
    }

    fn rename(&self, _new_name: &str) -> bool {
        false
    }
}

impl Default for IndirectStorage {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_entry_size() {
        assert_eq!(std::mem::size_of::<Entry>(), 0x14);
    }

    #[test]
    fn test_entry_offset_round_trip() {
        let mut entry = Entry {
            virt_offset: [0u8; 8],
            phys_offset: [0u8; 8],
            storage_index: 0,
        };
        entry.set_virtual_offset(0x12345678);
        assert_eq!(entry.get_virtual_offset(), 0x12345678);

        entry.set_physical_offset(-1);
        assert_eq!(entry.get_physical_offset(), -1);
    }

    #[test]
    fn test_entry_data_from_entry() {
        let mut entry = Entry {
            virt_offset: [0u8; 8],
            phys_offset: [0u8; 8],
            storage_index: 1,
        };
        entry.set_virtual_offset(100);
        entry.set_physical_offset(200);
        let data = EntryData::from_entry(&entry);
        assert_eq!(data.virt_offset, 100);
        assert_eq!(data.phys_offset, 200);
        assert_eq!(data.storage_index, 1);
    }

    #[test]
    fn test_constants() {
        assert_eq!(STORAGE_COUNT, 2);
        assert_eq!(NODE_SIZE, 16384);
    }

    #[test]
    fn test_new() {
        let storage = IndirectStorage::new();
        assert!(!storage.is_initialized());
    }

    #[test]
    fn test_default() {
        let storage = IndirectStorage::default();
        assert!(!storage.is_initialized());
    }
}
