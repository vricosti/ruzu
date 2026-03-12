// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_bucket_tree.h / .cpp

use std::sync::Mutex;

use crate::file_sys::errors::*;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use common::ResultCode;

pub const BUCKET_TREE_MAGIC: u32 = u32::from_le_bytes(*b"BKTR");
pub const BUCKET_TREE_VERSION: u32 = 1;

pub const NODE_SIZE_MIN: usize = 1024;
pub const NODE_SIZE_MAX: usize = 512 * 1024;

/// Bucket tree header.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct BucketTreeHeader {
    pub magic: u32,
    pub version: u32,
    pub entry_count: i32,
    pub reserved: i32,
}
const _: () = assert!(std::mem::size_of::<BucketTreeHeader>() == 0x10);

impl BucketTreeHeader {
    pub fn format(&mut self, entry_count: i32) {
        self.magic = BUCKET_TREE_MAGIC;
        self.version = BUCKET_TREE_VERSION;
        self.entry_count = entry_count;
        self.reserved = 0;
    }

    pub fn verify(&self) -> Result<(), ResultCode> {
        if self.magic != BUCKET_TREE_MAGIC {
            return Err(RESULT_INVALID_BUCKET_TREE_SIGNATURE);
        }
        if self.version != BUCKET_TREE_VERSION {
            return Err(RESULT_INVALID_BUCKET_TREE_SIGNATURE);
        }
        if self.entry_count < 0 {
            return Err(RESULT_INVALID_BUCKET_TREE_ENTRY_COUNT);
        }
        Ok(())
    }
}

/// Node header.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NodeHeader {
    pub index: i32,
    pub count: i32,
    pub offset: i64,
}
const _: () = assert!(std::mem::size_of::<NodeHeader>() == 0x10);

impl NodeHeader {
    pub fn verify(&self, node_index: i32, node_size: usize, entry_size: usize) -> Result<(), ResultCode> {
        if self.index != node_index {
            return Err(RESULT_INVALID_BUCKET_TREE_NODE_INDEX);
        }
        let entry_count = get_entry_count(node_size, entry_size);
        if self.count <= 0 || self.count > entry_count {
            return Err(RESULT_INVALID_BUCKET_TREE_NODE_ENTRY_COUNT);
        }
        Ok(())
    }
}

/// Offsets.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Offsets {
    pub start_offset: i64,
    pub end_offset: i64,
}
const _: () = assert!(std::mem::size_of::<Offsets>() == 0x10);

impl Offsets {
    pub fn is_include_offset(&self, offset: i64) -> bool {
        self.start_offset <= offset && offset < self.end_offset
    }

    pub fn is_include_range(&self, offset: i64, size: i64) -> bool {
        size > 0 && self.start_offset <= offset && size <= (self.end_offset - offset)
    }
}

/// Continuous reading info.
#[derive(Debug, Default)]
pub struct ContinuousReadingInfo {
    read_size: usize,
    skip_count: i32,
    done: bool,
}

impl ContinuousReadingInfo {
    pub fn new() -> Self { Self::default() }
    pub fn reset(&mut self) { self.read_size = 0; self.skip_count = 0; self.done = false; }
    pub fn set_skip_count(&mut self, count: i32) { assert!(count >= 0); self.skip_count = count; }
    pub fn get_skip_count(&self) -> i32 { self.skip_count }
    pub fn check_need_scan(&mut self) -> bool { self.skip_count -= 1; self.skip_count <= 0 }
    pub fn done(&mut self) { self.read_size = 0; self.done = true; }
    pub fn is_done(&self) -> bool { self.done }
    pub fn set_read_size(&mut self, size: usize) { self.read_size = size; }
    pub fn get_read_size(&self) -> usize { self.read_size }
    pub fn can_do(&self) -> bool { self.read_size > 0 }
}

fn get_entry_count(node_size: usize, entry_size: usize) -> i32 {
    ((node_size - std::mem::size_of::<NodeHeader>()) / entry_size) as i32
}

fn get_offset_count(node_size: usize) -> i32 {
    ((node_size - std::mem::size_of::<NodeHeader>()) / std::mem::size_of::<i64>()) as i32
}

fn get_entry_set_count(node_size: usize, entry_size: usize, entry_count: i32) -> i32 {
    let entry_count_per_node = get_entry_count(node_size, entry_size);
    (entry_count + entry_count_per_node - 1) / entry_count_per_node
}

fn get_node_l2_count(node_size: usize, entry_size: usize, entry_count: i32) -> i32 {
    let offset_count_per_node = get_offset_count(node_size);
    let entry_set_count = get_entry_set_count(node_size, entry_size, entry_count);
    if entry_set_count <= offset_count_per_node { return 0; }
    let node_l2_count = (entry_set_count + offset_count_per_node - 1) / offset_count_per_node;
    assert!(node_l2_count <= offset_count_per_node);
    let remainder = entry_set_count - (offset_count_per_node - (node_l2_count - 1));
    (remainder + offset_count_per_node - 1) / offset_count_per_node
}

struct OffsetCache {
    offsets: Offsets,
    is_initialized: bool,
}

/// Bucket tree implementation.
pub struct BucketTree {
    node_storage: Option<VirtualFile>,
    entry_storage: Option<VirtualFile>,
    node_l1: Vec<u8>,
    node_size: usize,
    entry_size: usize,
    entry_count: i32,
    offset_count: i32,
    entry_set_count: i32,
    offset_cache: Mutex<OffsetCache>,
}

impl BucketTree {
    pub fn new() -> Self {
        Self {
            node_storage: None,
            entry_storage: None,
            node_l1: Vec::new(),
            node_size: 0,
            entry_size: 0,
            entry_count: 0,
            offset_count: 0,
            entry_set_count: 0,
            offset_cache: Mutex::new(OffsetCache {
                offsets: Offsets { start_offset: -1, end_offset: -1 },
                is_initialized: false,
            }),
        }
    }

    pub fn initialize(
        &mut self,
        node_storage: VirtualFile,
        entry_storage: VirtualFile,
        node_size: usize,
        entry_size: usize,
        entry_count: i32,
    ) -> Result<(), ResultCode> {
        assert!(entry_size >= std::mem::size_of::<i64>());
        assert!(node_size >= entry_size + std::mem::size_of::<NodeHeader>());
        assert!(NODE_SIZE_MIN <= node_size && node_size <= NODE_SIZE_MAX);
        assert!(node_size.is_power_of_two());

        if entry_count <= 0 {
            self.node_size = node_size;
            return Ok(());
        }

        self.node_l1 = vec![0u8; node_size];
        node_storage.read(&mut self.node_l1, node_size, 0);

        self.node_storage = Some(node_storage);
        self.entry_storage = Some(entry_storage);
        self.node_size = node_size;
        self.entry_size = entry_size;
        self.entry_count = entry_count;
        self.offset_count = get_offset_count(node_size);
        self.entry_set_count = get_entry_set_count(node_size, entry_size, entry_count);

        Ok(())
    }

    pub fn initialize_empty(&mut self, node_size: usize, end_offset: i64) {
        self.node_size = node_size;
        let mut cache = self.offset_cache.lock().unwrap();
        cache.offsets = Offsets { start_offset: 0, end_offset };
        cache.is_initialized = true;
    }

    pub fn finalize(&mut self) {
        self.node_storage = None;
        self.entry_storage = None;
        self.node_l1.clear();
        self.node_size = 0;
        self.entry_size = 0;
        self.entry_count = 0;
        self.offset_count = 0;
        self.entry_set_count = 0;
    }

    pub fn is_initialized(&self) -> bool { self.node_size > 0 }
    pub fn is_empty(&self) -> bool { self.entry_size == 0 }
    pub fn get_entry_count(&self) -> i32 { self.entry_count }

    pub fn get_offsets(&self) -> Result<Offsets, ResultCode> {
        self.ensure_offset_cache()?;
        let cache = self.offset_cache.lock().unwrap();
        Ok(cache.offsets)
    }

    fn ensure_offset_cache(&self) -> Result<(), ResultCode> {
        let mut cache = self.offset_cache.lock().unwrap();
        if cache.is_initialized { return Ok(()); }

        if self.node_l1.len() >= std::mem::size_of::<NodeHeader>() {
            let header: &NodeHeader = unsafe { &*(self.node_l1.as_ptr() as *const NodeHeader) };
            cache.offsets.start_offset = header.offset;

            if let Some(ref es) = self.entry_storage {
                let last_set_index = self.entry_set_count - 1;
                let offset_in_storage = last_set_index as usize * self.node_size;
                let mut buf = vec![0u8; self.node_size];
                es.read(&mut buf, self.node_size, offset_in_storage);

                let entry_set_header: &NodeHeader = unsafe { &*(buf.as_ptr() as *const NodeHeader) };
                let last_entry_offset = std::mem::size_of::<NodeHeader>()
                    + (entry_set_header.count as usize - 1) * self.entry_size;
                if last_entry_offset + 8 <= buf.len() {
                    let end_offset_bytes = &buf[last_entry_offset..last_entry_offset + 8];
                    cache.offsets.end_offset = i64::from_le_bytes(end_offset_bytes.try_into().unwrap());
                }
            }
        }

        cache.is_initialized = true;
        Ok(())
    }

    /// Find an entry in the bucket tree for the given virtual address.
    ///
    /// Corresponds to upstream `BucketTree::Find`.
    pub fn find(&self, virtual_address: i64) -> Result<Visitor, ResultCode> {
        assert!(self.is_initialized());

        if virtual_address < 0 {
            return Err(RESULT_INVALID_OFFSET);
        }
        if self.is_empty() {
            return Err(RESULT_OUT_OF_RANGE);
        }

        let offsets = self.get_offsets()?;
        let mut visitor = Visitor::new();
        visitor.initialize(self, offsets)?;
        visitor.find(virtual_address)?;
        Ok(visitor)
    }

    fn is_exist_l2(&self) -> bool {
        self.offset_count < self.entry_set_count
    }

    fn is_exist_offset_l2_on_l1(&self) -> bool {
        if !self.is_exist_l2() {
            return false;
        }
        if self.node_l1.len() < std::mem::size_of::<NodeHeader>() {
            return false;
        }
        let header: &NodeHeader = unsafe { &*(self.node_l1.as_ptr() as *const NodeHeader) };
        header.count < self.offset_count
    }

    fn get_entry_set_index(&self, node_index: i32, offset_index: i32) -> i64 {
        let header: &NodeHeader = unsafe { &*(self.node_l1.as_ptr() as *const NodeHeader) };
        (self.offset_count - header.count) as i64
            + (self.offset_count as i64 * node_index as i64)
            + offset_index as i64
    }

    /// Get the begin offset from the L1 node (offset of the first entry in node body).
    fn get_l1_begin_offset(&self) -> i64 {
        if self.node_l1.len() < std::mem::size_of::<NodeHeader>() {
            return 0;
        }
        let header: &NodeHeader = unsafe { &*(self.node_l1.as_ptr() as *const NodeHeader) };
        header.offset
    }

    /// Get the end offset from the L1 node (last i64 in the node body).
    fn get_l1_end_offset(&self) -> i64 {
        if self.node_l1.len() < std::mem::size_of::<NodeHeader>() {
            return 0;
        }
        let header: &NodeHeader = unsafe { &*(self.node_l1.as_ptr() as *const NodeHeader) };
        // The end offset is stored at the position after all count entries.
        // In the node, after NodeHeader, there are `count` i64 offsets, followed by more.
        // The end offset is the i64 at NodeHeader + count * sizeof(i64).
        let end_pos = std::mem::size_of::<NodeHeader>() + header.count as usize * std::mem::size_of::<i64>();
        if end_pos + 8 <= self.node_l1.len() {
            i64::from_le_bytes(self.node_l1[end_pos..end_pos + 8].try_into().unwrap())
        } else {
            0
        }
    }

    /// Read i64 offsets from the L1 node body (after NodeHeader).
    fn read_l1_offsets(&self, start_idx: usize, count: usize) -> Vec<i64> {
        let header_size = std::mem::size_of::<NodeHeader>();
        let mut result = Vec::with_capacity(count);
        for i in start_idx..start_idx + count {
            let pos = header_size + i * std::mem::size_of::<i64>();
            if pos + 8 <= self.node_l1.len() {
                result.push(i64::from_le_bytes(
                    self.node_l1[pos..pos + 8].try_into().unwrap(),
                ));
            }
        }
        result
    }

    pub const fn query_header_storage_size() -> i64 {
        std::mem::size_of::<BucketTreeHeader>() as i64
    }

    pub fn query_node_storage_size(node_size: usize, entry_size: usize, entry_count: i32) -> i64 {
        if entry_count <= 0 { return 0; }
        (1 + get_node_l2_count(node_size, entry_size, entry_count)) as i64 * node_size as i64
    }

    pub fn query_entry_storage_size(node_size: usize, entry_size: usize, entry_count: i32) -> i64 {
        if entry_count <= 0 { return 0; }
        get_entry_set_count(node_size, entry_size, entry_count) as i64 * node_size as i64
    }
}

impl Default for BucketTree {
    fn default() -> Self { Self::new() }
}

/// Entry set header.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct EntrySetHeader {
    pub index: i32,
    pub count: i32,
    pub end: i64,
    pub start: i64,
}

/// Compute the byte offset of an entry within an entry set.
///
/// Corresponds to upstream `impl::GetBucketTreeEntryOffset`.
pub fn get_bucket_tree_entry_offset(
    entry_set_offset: i64,
    entry_size: usize,
    entry_index: i32,
) -> usize {
    let node_header_size = std::mem::size_of::<NodeHeader>();
    (entry_set_offset as usize) + node_header_size + entry_index as usize * entry_size
}

/// Compute entry offset within an entry set at a given entry_set_index.
///
/// Corresponds to upstream `impl::GetBucketTreeEntryOffset(entry_set_index, node_size, entry_size, entry_index)`.
pub fn get_bucket_tree_entry_offset_by_set(
    entry_set_index: i32,
    node_size: usize,
    entry_size: usize,
    entry_index: i32,
) -> usize {
    let entry_set_offset = entry_set_index as usize * node_size;
    get_bucket_tree_entry_offset(entry_set_offset as i64, entry_size, entry_index)
}

/// Binary search within a buffer of i64 offsets to find the entry containing `virtual_address`.
///
/// Corresponds to upstream `StorageNode::Find(buffer, virtual_address)`.
fn storage_node_find_in_buffer(
    buffer: &[u8],
    stride: usize,
    count: i32,
    base_offset: usize,
    virtual_address: i64,
) -> i32 {
    let mut end = count;
    let mut pos: i64 = 0;

    while end > 0 {
        let half = end / 2;
        let mid = pos + half as i64;
        let byte_offset = base_offset + mid as usize * stride;

        if byte_offset + 8 <= buffer.len() {
            let offset =
                i64::from_le_bytes(buffer[byte_offset..byte_offset + 8].try_into().unwrap());
            if offset <= virtual_address {
                pos = mid + 1;
                end -= half + 1;
            } else {
                end = half;
            }
        } else {
            break;
        }
    }

    pos as i32 - 1
}

/// Visitor for traversing a BucketTree.
///
/// Corresponds to upstream `BucketTree::Visitor`.
pub struct Visitor<'a> {
    tree: Option<&'a BucketTree>,
    offsets: Offsets,
    entry: Vec<u8>,
    entry_index: i32,
    entry_set_count: i32,
    entry_set: EntrySetHeader,
}

impl<'a> Visitor<'a> {
    pub fn new() -> Self {
        Self {
            tree: None,
            offsets: Offsets::default(),
            entry: Vec::new(),
            entry_index: -1,
            entry_set_count: 0,
            entry_set: EntrySetHeader::default(),
        }
    }

    /// Check if the visitor points to a valid entry.
    pub fn is_valid(&self) -> bool {
        self.entry_index >= 0
    }

    /// Check if the visitor can move to the next entry.
    ///
    /// Corresponds to upstream `Visitor::CanMoveNext`.
    pub fn can_move_next(&self) -> bool {
        self.is_valid()
            && (self.entry_index + 1 < self.entry_set.count
                || self.entry_set.index + 1 < self.entry_set_count)
    }

    /// Check if the visitor can move to the previous entry.
    ///
    /// Corresponds to upstream `Visitor::CanMovePrevious`.
    pub fn can_move_previous(&self) -> bool {
        self.is_valid() && (self.entry_index > 0 || self.entry_set.index > 0)
    }

    /// Get the raw entry data as a byte slice.
    pub fn get_raw(&self) -> &[u8] {
        assert!(self.is_valid());
        &self.entry
    }

    /// Get the entry interpreted as type T.
    ///
    /// # Safety
    /// T must be a valid repr(C) type that matches the entry size.
    pub unsafe fn get<T>(&self) -> &T {
        assert!(self.is_valid());
        assert!(self.entry.len() >= std::mem::size_of::<T>());
        &*(self.entry.as_ptr() as *const T)
    }

    /// Initialize the visitor with a tree and offsets.
    ///
    /// Corresponds to upstream `Visitor::Initialize`.
    fn initialize(
        &mut self,
        tree: &'a BucketTree,
        offsets: Offsets,
    ) -> Result<(), ResultCode> {
        if self.entry.is_empty() {
            self.entry = vec![0u8; tree.entry_size];
            self.tree = Some(tree);
            self.offsets = offsets;
        }
        Ok(())
    }

    /// Find the entry containing the given virtual address.
    ///
    /// Corresponds to upstream `Visitor::Find`.
    fn find(&mut self, virtual_address: i64) -> Result<(), ResultCode> {
        let tree = self.tree.unwrap();

        // Check that the virtual address is within the L1 node's end offset.
        let l1_end = tree.get_l1_end_offset();
        if virtual_address >= l1_end {
            return Err(RESULT_OUT_OF_RANGE);
        }

        // Get the entry set index.
        let mut entry_set_index: i32;

        if tree.is_exist_offset_l2_on_l1() && virtual_address < tree.get_l1_begin_offset() {
            // Search in the L2 offset area on L1.
            let header: &NodeHeader =
                unsafe { &*(tree.node_l1.as_ptr() as *const NodeHeader) };
            let l2_start = header.count as usize;
            let l2_count = tree.offset_count as usize - l2_start;
            let offsets = tree.read_l1_offsets(l2_start, l2_count);

            // upper_bound: find first element > virtual_address, then go back one
            let pos = offsets
                .partition_point(|&o| o <= virtual_address);
            if pos == 0 {
                return Err(RESULT_OUT_OF_RANGE);
            }
            entry_set_index = (pos - 1) as i32;
        } else {
            // Search in the main offset area on L1.
            let header: &NodeHeader =
                unsafe { &*(tree.node_l1.as_ptr() as *const NodeHeader) };
            let count = header.count as usize;
            let offsets = tree.read_l1_offsets(0, count);

            let pos = offsets
                .partition_point(|&o| o <= virtual_address);
            if pos == 0 {
                return Err(RESULT_OUT_OF_RANGE);
            }
            let index = (pos - 1) as i32;

            if tree.is_exist_l2() {
                // Need to descend into L2 node.
                if index < 0 || index >= tree.offset_count {
                    return Err(RESULT_INVALID_BUCKET_TREE_NODE_OFFSET);
                }
                entry_set_index = self.find_entry_set(virtual_address, index)?;
            } else {
                entry_set_index = index;
            }
        }

        // Validate.
        if entry_set_index < 0 || entry_set_index >= tree.entry_set_count {
            return Err(RESULT_INVALID_BUCKET_TREE_NODE_OFFSET);
        }

        // Find the entry within the entry set.
        self.find_entry(virtual_address, entry_set_index)?;

        // Set count.
        self.entry_set_count = tree.entry_set_count;
        Ok(())
    }

    /// Find the entry set index by descending into an L2 node.
    ///
    /// Corresponds to upstream `Visitor::FindEntrySet`.
    fn find_entry_set(
        &self,
        virtual_address: i64,
        node_index: i32,
    ) -> Result<i32, ResultCode> {
        let tree = self.tree.unwrap();
        let node_size = tree.node_size;
        let node_offset = (node_index as usize + 1) * node_size;

        // Read the L2 node.
        let node_storage = tree
            .node_storage
            .as_ref()
            .ok_or(RESULT_OUT_OF_RANGE)?;
        let mut buf = vec![0u8; node_size];
        node_storage.read(&mut buf, node_size, node_offset);

        // Validate header.
        let header: &NodeHeader = unsafe { &*(buf.as_ptr() as *const NodeHeader) };
        header.verify(node_index, node_size, std::mem::size_of::<i64>())?;

        // Binary search within the node.
        let node_header_size = std::mem::size_of::<NodeHeader>();
        let index = storage_node_find_in_buffer(
            &buf,
            std::mem::size_of::<i64>(),
            header.count,
            node_header_size,
            virtual_address,
        );
        if index < 0 {
            return Err(RESULT_INVALID_BUCKET_TREE_VIRTUAL_OFFSET);
        }

        Ok(tree.get_entry_set_index(header.index, index) as i32)
    }

    /// Find the entry within an entry set.
    ///
    /// Corresponds to upstream `Visitor::FindEntry`.
    fn find_entry(
        &mut self,
        virtual_address: i64,
        entry_set_index: i32,
    ) -> Result<(), ResultCode> {
        let tree = self.tree.unwrap();
        let entry_size = tree.entry_size;
        let entry_set_size = tree.node_size;
        let entry_set_offset = entry_set_index as usize * entry_set_size;

        let entry_storage = tree
            .entry_storage
            .as_ref()
            .ok_or(RESULT_OUT_OF_RANGE)?;

        // Read the entry set.
        let mut buf = vec![0u8; entry_set_size];
        entry_storage.read(&mut buf, entry_set_size, entry_set_offset);

        // Parse and validate the entry set header.
        let entry_set = unsafe { *(buf.as_ptr() as *const EntrySetHeader) };
        // Verify as a NodeHeader.
        let header = NodeHeader {
            index: entry_set.index,
            count: entry_set.count,
            offset: entry_set.end,
        };
        header.verify(entry_set_index, entry_set_size, entry_size)?;

        // Binary search for the virtual address in entry offsets.
        let node_header_size = std::mem::size_of::<NodeHeader>();
        let index = storage_node_find_in_buffer(
            &buf,
            entry_size,
            entry_set.count,
            node_header_size,
            virtual_address,
        );
        if index < 0 {
            return Err(RESULT_OUT_OF_RANGE);
        }

        // Copy the entry data.
        let entry_offset = get_bucket_tree_entry_offset(0, entry_size, index);
        if entry_offset + entry_size <= buf.len() {
            self.entry[..entry_size]
                .copy_from_slice(&buf[entry_offset..entry_offset + entry_size]);
        }

        self.entry_set = entry_set;
        self.entry_index = index;
        Ok(())
    }

    /// Move to the next entry.
    ///
    /// Corresponds to upstream `Visitor::MoveNext`.
    pub fn move_next(&mut self) -> Result<(), ResultCode> {
        if !self.is_valid() {
            return Err(RESULT_OUT_OF_RANGE);
        }

        let tree = self.tree.unwrap();
        let mut entry_index = self.entry_index + 1;

        if entry_index == self.entry_set.count {
            let entry_set_index = self.entry_set.index + 1;
            if entry_set_index >= self.entry_set_count {
                return Err(RESULT_OUT_OF_RANGE);
            }

            self.entry_index = -1;
            let end = self.entry_set.end;

            let entry_set_size = tree.node_size;
            let entry_set_offset = entry_set_index as usize * entry_set_size;

            let entry_storage = tree
                .entry_storage
                .as_ref()
                .ok_or(RESULT_OUT_OF_RANGE)?;

            // Read entry set header.
            let mut header_buf = [0u8; std::mem::size_of::<EntrySetHeader>()];
            entry_storage.read(
                &mut header_buf,
                std::mem::size_of::<EntrySetHeader>(),
                entry_set_offset,
            );
            self.entry_set = unsafe { *(header_buf.as_ptr() as *const EntrySetHeader) };

            // Verify.
            let header = NodeHeader {
                index: self.entry_set.index,
                count: self.entry_set.count,
                offset: self.entry_set.end,
            };
            header.verify(entry_set_index, entry_set_size, tree.entry_size)?;

            if self.entry_set.start != end || self.entry_set.start >= self.entry_set.end {
                return Err(RESULT_INVALID_BUCKET_TREE_ENTRY_SET_OFFSET);
            }

            entry_index = 0;
        } else {
            self.entry_index = -1;
        }

        // Read the new entry.
        let entry_size = tree.entry_size;
        let entry_offset = get_bucket_tree_entry_offset_by_set(
            self.entry_set.index,
            tree.node_size,
            entry_size,
            entry_index,
        );
        let entry_storage = tree
            .entry_storage
            .as_ref()
            .ok_or(RESULT_OUT_OF_RANGE)?;
        entry_storage.read(&mut self.entry, entry_size, entry_offset);

        self.entry_index = entry_index;
        Ok(())
    }

    /// Move to the previous entry.
    ///
    /// Corresponds to upstream `Visitor::MovePrevious`.
    pub fn move_previous(&mut self) -> Result<(), ResultCode> {
        if !self.is_valid() {
            return Err(RESULT_OUT_OF_RANGE);
        }

        let tree = self.tree.unwrap();
        let mut entry_index = self.entry_index;

        if entry_index == 0 {
            if self.entry_set.index <= 0 {
                return Err(RESULT_OUT_OF_RANGE);
            }

            self.entry_index = -1;
            let start = self.entry_set.start;

            let entry_set_size = tree.node_size;
            let entry_set_index = self.entry_set.index - 1;
            let entry_set_offset = entry_set_index as usize * entry_set_size;

            let entry_storage = tree
                .entry_storage
                .as_ref()
                .ok_or(RESULT_OUT_OF_RANGE)?;

            let mut header_buf = [0u8; std::mem::size_of::<EntrySetHeader>()];
            entry_storage.read(
                &mut header_buf,
                std::mem::size_of::<EntrySetHeader>(),
                entry_set_offset,
            );
            self.entry_set = unsafe { *(header_buf.as_ptr() as *const EntrySetHeader) };

            let header = NodeHeader {
                index: self.entry_set.index,
                count: self.entry_set.count,
                offset: self.entry_set.end,
            };
            header.verify(entry_set_index, entry_set_size, tree.entry_size)?;

            if self.entry_set.end != start || self.entry_set.start >= self.entry_set.end {
                return Err(RESULT_INVALID_BUCKET_TREE_ENTRY_SET_OFFSET);
            }

            entry_index = self.entry_set.count;
        } else {
            self.entry_index = -1;
        }

        entry_index -= 1;

        // Read the new entry.
        let entry_size = tree.entry_size;
        let entry_offset = get_bucket_tree_entry_offset_by_set(
            self.entry_set.index,
            tree.node_size,
            entry_size,
            entry_index,
        );
        let entry_storage = tree
            .entry_storage
            .as_ref()
            .ok_or(RESULT_OUT_OF_RANGE)?;
        entry_storage.read(&mut self.entry, entry_size, entry_offset);

        self.entry_index = entry_index;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constants() {
        assert_eq!(BUCKET_TREE_MAGIC, u32::from_le_bytes(*b"BKTR"));
        assert_eq!(BUCKET_TREE_VERSION, 1);
        assert_eq!(NODE_SIZE_MIN, 1024);
        assert_eq!(NODE_SIZE_MAX, 512 * 1024);
    }

    #[test]
    fn test_bucket_tree_header_size() {
        assert_eq!(std::mem::size_of::<BucketTreeHeader>(), 0x10);
    }

    #[test]
    fn test_bucket_tree_header_format_and_verify() {
        let mut header = BucketTreeHeader::default();
        header.format(42);
        assert_eq!(header.magic, BUCKET_TREE_MAGIC);
        assert_eq!(header.version, BUCKET_TREE_VERSION);
        assert_eq!(header.entry_count, 42);
        assert_eq!(header.reserved, 0);
        assert!(header.verify().is_ok());
    }

    #[test]
    fn test_bucket_tree_header_verify_bad_magic() {
        let header = BucketTreeHeader {
            magic: 0xDEADBEEF,
            version: BUCKET_TREE_VERSION,
            entry_count: 0,
            reserved: 0,
        };
        assert!(header.verify().is_err());
    }

    #[test]
    fn test_bucket_tree_header_verify_bad_version() {
        let header = BucketTreeHeader {
            magic: BUCKET_TREE_MAGIC,
            version: 99,
            entry_count: 0,
            reserved: 0,
        };
        assert!(header.verify().is_err());
    }

    #[test]
    fn test_bucket_tree_header_verify_negative_entry_count() {
        let header = BucketTreeHeader {
            magic: BUCKET_TREE_MAGIC,
            version: BUCKET_TREE_VERSION,
            entry_count: -1,
            reserved: 0,
        };
        assert!(header.verify().is_err());
    }

    #[test]
    fn test_node_header_size() {
        assert_eq!(std::mem::size_of::<NodeHeader>(), 0x10);
    }

    #[test]
    fn test_offsets_size() {
        assert_eq!(std::mem::size_of::<Offsets>(), 0x10);
    }

    #[test]
    fn test_offsets_is_include_offset() {
        let offsets = Offsets {
            start_offset: 10,
            end_offset: 100,
        };
        assert!(offsets.is_include_offset(10));
        assert!(offsets.is_include_offset(50));
        assert!(offsets.is_include_offset(99));
        assert!(!offsets.is_include_offset(9));
        assert!(!offsets.is_include_offset(100));
    }

    #[test]
    fn test_offsets_is_include_range() {
        let offsets = Offsets {
            start_offset: 0,
            end_offset: 100,
        };
        assert!(offsets.is_include_range(0, 100));
        assert!(offsets.is_include_range(0, 50));
        assert!(offsets.is_include_range(50, 50));
        assert!(!offsets.is_include_range(50, 51));
        assert!(!offsets.is_include_range(0, 0));
        assert!(!offsets.is_include_range(-1, 10));
    }

    #[test]
    fn test_continuous_reading_info() {
        let mut info = ContinuousReadingInfo::new();
        assert!(!info.is_done());
        assert!(!info.can_do());
        assert_eq!(info.get_read_size(), 0);
        assert_eq!(info.get_skip_count(), 0);

        info.set_read_size(1024);
        assert!(info.can_do());
        assert_eq!(info.get_read_size(), 1024);

        info.set_skip_count(3);
        assert_eq!(info.get_skip_count(), 3);
        assert!(!info.check_need_scan()); // skip_count = 2
        assert!(!info.check_need_scan()); // skip_count = 1
        assert!(info.check_need_scan());  // skip_count = 0

        info.done();
        assert!(info.is_done());
        assert!(!info.can_do()); // read_size reset to 0

        info.reset();
        assert!(!info.is_done());
        assert_eq!(info.get_read_size(), 0);
        assert_eq!(info.get_skip_count(), 0);
    }

    #[test]
    fn test_get_entry_count_helper() {
        // node_size=1024, entry_size=16 -> (1024-16)/16 = 63
        assert_eq!(get_entry_count(1024, 16), 63);
    }

    #[test]
    fn test_get_offset_count_helper() {
        // node_size=1024 -> (1024-16)/8 = 126
        assert_eq!(get_offset_count(1024), 126);
    }

    #[test]
    fn test_get_entry_set_count_helper() {
        // 100 entries, 63 per node -> ceil(100/63) = 2
        assert_eq!(get_entry_set_count(1024, 16, 100), 2);
        // 63 entries exactly -> 1
        assert_eq!(get_entry_set_count(1024, 16, 63), 1);
    }

    #[test]
    fn test_bucket_tree_new() {
        let tree = BucketTree::new();
        assert!(!tree.is_initialized());
        assert!(tree.is_empty());
        assert_eq!(tree.get_entry_count(), 0);
    }

    #[test]
    fn test_bucket_tree_default() {
        let tree = BucketTree::default();
        assert!(!tree.is_initialized());
    }

    #[test]
    fn test_bucket_tree_initialize_empty() {
        let mut tree = BucketTree::new();
        tree.initialize_empty(1024, 0x10000);
        assert!(tree.is_initialized());
        let offsets = tree.get_offsets().unwrap();
        assert_eq!(offsets.start_offset, 0);
        assert_eq!(offsets.end_offset, 0x10000);
    }

    #[test]
    fn test_query_header_storage_size() {
        assert_eq!(BucketTree::query_header_storage_size(), 0x10);
    }

    #[test]
    fn test_query_node_storage_size_zero_entries() {
        assert_eq!(BucketTree::query_node_storage_size(1024, 16, 0), 0);
        assert_eq!(BucketTree::query_node_storage_size(1024, 16, -1), 0);
    }

    #[test]
    fn test_query_entry_storage_size_zero_entries() {
        assert_eq!(BucketTree::query_entry_storage_size(1024, 16, 0), 0);
        assert_eq!(BucketTree::query_entry_storage_size(1024, 16, -1), 0);
    }

    #[test]
    fn test_query_node_storage_size_small() {
        // 1 entry, node_size=1024, entry_size=16
        // entry_set_count = 1, offset_count = 126, 1 <= 126 so l2=0
        // result = (1 + 0) * 1024 = 1024
        assert_eq!(BucketTree::query_node_storage_size(1024, 16, 1), 1024);
    }

    #[test]
    fn test_query_entry_storage_size_small() {
        // 1 entry -> entry_set_count = 1 -> 1 * 1024 = 1024
        assert_eq!(BucketTree::query_entry_storage_size(1024, 16, 1), 1024);
    }

    #[test]
    fn test_entry_set_header_size() {
        assert_eq!(std::mem::size_of::<EntrySetHeader>(), 0x18);
    }

    #[test]
    fn test_finalize() {
        let mut tree = BucketTree::new();
        tree.initialize_empty(1024, 0x10000);
        assert!(tree.is_initialized());
        tree.finalize();
        assert!(!tree.is_initialized());
        assert_eq!(tree.get_entry_count(), 0);
    }
}
