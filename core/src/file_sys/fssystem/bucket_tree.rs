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
