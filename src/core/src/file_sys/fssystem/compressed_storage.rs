// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_compressed_storage.h

use super::bucket_tree::BucketTree;
use super::compression_common::{
    compression_type_utility, CompressionType, GetDecompressorFunction, COMPRESSION_BLOCK_ALIGNMENT,
};
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

    /// Corresponds to upstream `CompressedStorageCore::GetEntryList`.
    // This remains part of the nested core's parity API even though the outer C++ class likewise
    // does not forward it.
    #[allow(dead_code)]
    pub fn get_entry_list(
        &self,
        out_entries: Option<&mut [Entry]>,
        max_entry_count: i32,
        offset: i64,
        size: i64,
    ) -> Result<i32, ResultCode> {
        assert!(offset >= 0);
        assert!(size >= 0);
        assert!(self.is_initialized());

        if size == 0 {
            return Ok(0);
        }
        if max_entry_count != 0 && out_entries.is_none() {
            return Err(RESULT_NULLPTR_ARGUMENT);
        }

        let table_offsets = self.table.get_offsets()?;
        if !table_offsets.is_include_range(offset, size) {
            return Err(RESULT_OUT_OF_RANGE);
        }

        let mut visitor = self.table.find(offset)?;
        let first_offset = unsafe { visitor.get::<Entry>() }.virt_offset;
        if first_offset < 0 || !table_offsets.is_include_offset(first_offset) {
            return Err(RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_A);
        }

        let end_offset = offset + size;
        let mut read_count = 0i32;
        let mut out_entries = out_entries;
        while unsafe { visitor.get::<Entry>() }.virt_offset < end_offset {
            if max_entry_count != 0 {
                if read_count >= max_entry_count {
                    break;
                }
                let output = out_entries.as_deref_mut().unwrap();
                output[read_count as usize] = *unsafe { visitor.get::<Entry>() };
            }

            read_count += 1;
            if !visitor.can_move_next() {
                break;
            }
            visitor.move_next()?;
        }

        Ok(read_count)
    }

    pub fn get_size(&self) -> Result<i64, ResultCode> {
        let offsets = self.table.get_offsets()?;
        Ok(offsets.end_offset)
    }

    fn operate_per_entry<F>(
        &self,
        offset: i64,
        size: i64,
        mut operation: F,
    ) -> Result<(), ResultCode>
    where
        F: FnMut(&mut bool, Entry, i64, i64, i64) -> Result<(), ResultCode>,
    {
        assert!(offset >= 0);
        assert!(size >= 0);
        assert!(self.is_initialized());

        if size == 0 {
            return Ok(());
        }

        let table_offsets = self.table.get_offsets()?;
        if !table_offsets.is_include_range(offset, size) {
            return Err(RESULT_OUT_OF_RANGE);
        }

        let mut visitor = self.table.find(offset)?;
        let first_offset = unsafe { visitor.get::<Entry>() }.virt_offset;
        if first_offset < 0 || !table_offsets.is_include_offset(first_offset) {
            return Err(RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_A);
        }

        let mut cur_offset = offset;
        let end_offset = offset + size;
        while cur_offset < end_offset {
            let cur_entry = *unsafe { visitor.get::<Entry>() };
            let cur_entry_offset = cur_entry.virt_offset;
            if cur_entry_offset > cur_offset {
                return Err(RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_A);
            }

            let next_entry_offset = if visitor.can_move_next() {
                visitor.move_next()?;
                let next = unsafe { visitor.get::<Entry>() }.virt_offset;
                if !table_offsets.is_include_offset(next) {
                    return Err(RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_A);
                }
                next
            } else {
                table_offsets.end_offset
            };
            if cur_offset >= next_entry_offset {
                return Err(RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_A);
            }

            let data_offset = cur_offset - cur_entry_offset;
            let data_size = next_entry_offset - cur_entry_offset;
            assert!(data_size > 0);
            let remaining_size = end_offset - cur_offset;
            let cur_size = remaining_size.min(data_size - data_offset);
            assert!(cur_size <= size);

            let storage_size = self
                .data_storage
                .as_ref()
                .expect("initialized compressed storage must own data storage")
                .get_size() as i64;
            if cur_entry.phys_offset < 0 || cur_entry.phys_offset > storage_size {
                return Err(RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_C);
            }
            if cur_entry.compression_type != CompressionType::None
                && cur_entry.phys_offset + cur_entry.get_physical_size() > storage_size
            {
                return Err(RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_C);
            }
            if compression_type_utility::is_block_alignment_required(cur_entry.compression_type)
                && cur_entry.phys_offset % COMPRESSION_BLOCK_ALIGNMENT != 0
            {
                return Err(RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_A);
            }

            let mut is_continuous = true;
            operation(
                &mut is_continuous,
                cur_entry,
                data_size,
                data_offset,
                cur_size,
            )?;
            if !is_continuous {
                break;
            }
            cur_offset += cur_size;
        }

        Ok(())
    }

    fn perform_required_read<F>(
        &self,
        entries: &[ReadEntry],
        will_allocate_pooled_buffer: bool,
        mut required_access_physical_offset: i64,
        mut required_access_physical_size: i64,
        read_func: &mut F,
    ) -> Result<(), ResultCode>
    where
        F: FnMut(
            usize,
            &mut dyn FnMut(&mut [u8]) -> Result<(), ResultCode>,
        ) -> Result<(), ResultCode>,
    {
        if entries.is_empty() {
            return Ok(());
        }

        let total_required_size = required_access_physical_size as usize;
        let data_storage = self
            .data_storage
            .as_ref()
            .expect("initialized compressed storage must own data storage");

        if will_allocate_pooled_buffer {
            let mut pooled_buffer = vec![0u8; self.block_size_max.max(total_required_size)];
            let mut entry_idx = 0usize;
            while entry_idx < entries.len() {
                let target_entry_size = entries[entry_idx].physical_size as usize
                    + entries[entry_idx].gap_from_prev as usize;
                let will_use_pooled_buffer = target_entry_size <= pooled_buffer.len();
                let cur_read_size = if will_use_pooled_buffer {
                    let max_size =
                        (required_access_physical_size as usize).min(pooled_buffer.len());
                    let mut read_size = 0usize;
                    for entry in &entries[entry_idx..] {
                        let cur_entry_size =
                            entry.physical_size as usize + entry.gap_from_prev as usize;
                        if read_size + cur_entry_size > max_size {
                            break;
                        }
                        read_size += cur_entry_size;
                    }
                    read_size
                } else {
                    assert_eq!(entries[entry_idx].compression_type, CompressionType::None);
                    entries[entry_idx].virtual_size as usize
                };

                if will_use_pooled_buffer {
                    data_storage.read(
                        &mut pooled_buffer[..cur_read_size],
                        cur_read_size,
                        required_access_physical_offset as usize,
                    );

                    let mut buffer_offset = 0usize;
                    while entry_idx < entries.len()
                        && (entries[entry_idx].physical_size as usize
                            + entries[entry_idx].gap_from_prev as usize
                            == 0
                            || buffer_offset < cur_read_size)
                    {
                        let entry = entries[entry_idx];
                        buffer_offset += entry.gap_from_prev as usize;
                        match entry.compression_type {
                            CompressionType::None => {
                                assert!(
                                    buffer_offset + entry.virtual_size as usize <= cur_read_size
                                );
                                let source = &pooled_buffer
                                    [buffer_offset..buffer_offset + entry.virtual_size as usize];
                                let mut read_impl = |dst: &mut [u8]| {
                                    assert_eq!(dst.len(), entry.virtual_size as usize);
                                    dst.copy_from_slice(source);
                                    Ok(())
                                };
                                read_func(entry.virtual_size as usize, &mut read_impl)?;
                            }
                            CompressionType::Zeros => {
                                assert!(buffer_offset <= cur_read_size);
                                let mut read_impl = |dst: &mut [u8]| {
                                    assert_eq!(dst.len(), entry.virtual_size as usize);
                                    dst.fill(0);
                                    Ok(())
                                };
                                read_func(entry.virtual_size as usize, &mut read_impl)?;
                            }
                            compression_type => {
                                assert!(
                                    buffer_offset + entry.physical_size as usize <= cur_read_size
                                );
                                let decompressor = self
                                    .get_decompressor(compression_type)
                                    .ok_or(RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_B)?;
                                let source = &pooled_buffer
                                    [buffer_offset..buffer_offset + entry.physical_size as usize];
                                let mut read_impl = |dst: &mut [u8]| {
                                    assert_eq!(dst.len(), entry.virtual_size as usize);
                                    decompressor(dst, source)
                                };
                                read_func(entry.virtual_size as usize, &mut read_impl)?;
                            }
                        }
                        buffer_offset += entry.physical_size as usize;
                        entry_idx += 1;
                    }
                    assert_eq!(buffer_offset, cur_read_size);
                } else {
                    let entry = entries[entry_idx];
                    required_access_physical_offset += entry.gap_from_prev as i64;
                    required_access_physical_size -= entry.gap_from_prev as i64;
                    let mut read_impl = |dst: &mut [u8]| {
                        assert_eq!(dst.len(), cur_read_size);
                        data_storage.read(
                            dst,
                            cur_read_size,
                            required_access_physical_offset as usize,
                        );
                        Ok(())
                    };
                    read_func(cur_read_size, &mut read_impl)?;
                    entry_idx += 1;
                }

                required_access_physical_offset += cur_read_size as i64;
                required_access_physical_size -= cur_read_size as i64;
            }
            assert_eq!(required_access_physical_size, 0);
        } else {
            let mut read_impl = |dst: &mut [u8]| {
                assert_eq!(dst.len(), total_required_size);
                data_storage.read(
                    dst,
                    total_required_size,
                    required_access_physical_offset as usize,
                );
                Ok(())
            };
            read_func(total_required_size, &mut read_impl)?;
        }

        Ok(())
    }

    fn read<F>(&self, offset: i64, size: i64, mut read_func: F) -> Result<(), ResultCode>
    where
        F: FnMut(
            usize,
            &mut dyn FnMut(&mut [u8]) -> Result<(), ResultCode>,
        ) -> Result<(), ResultCode>,
    {
        assert!(offset >= 0);
        assert!(self.is_initialized());
        if size == 0 {
            return Ok(());
        }

        let mut entries = Vec::<ReadEntry>::with_capacity(ENTRIES_COUNT_MAX);
        let mut prev_entry: Option<Entry> = None;
        let mut will_allocate_pooled_buffer = false;
        let mut required_access_physical_offset = 0i64;
        let mut required_access_physical_size = 0i64;

        self.operate_per_entry(
            offset,
            size,
            |out_continuous, entry, virtual_data_size, data_offset, read_size| {
                let (physical_offset, physical_size) =
                    if compression_type_utility::is_random_accessible(entry.compression_type) {
                        (entry.phys_offset + data_offset, read_size)
                    } else {
                        (entry.phys_offset, entry.get_physical_size())
                    };

                let required_access_physical_end =
                    required_access_physical_offset + required_access_physical_size;
                if required_access_physical_size > 0 {
                    let aligned_end = (required_access_physical_end + COMPRESSION_BLOCK_ALIGNMENT
                        - 1)
                        & !(COMPRESSION_BLOCK_ALIGNMENT - 1);
                    let required_by_gap = !(required_access_physical_end <= physical_offset
                        && physical_offset <= aligned_end);
                    let required_by_continuous_size = (physical_size + physical_offset
                        - required_access_physical_end)
                        + required_access_physical_size
                        > self.continuous_reading_size_max as i64;
                    let required_by_entry_count = entries.len() == ENTRIES_COUNT_MAX;
                    if required_by_gap || required_by_continuous_size || required_by_entry_count {
                        assert!(
                            !will_allocate_pooled_buffer
                                || required_access_physical_size
                                    <= self.continuous_reading_size_max as i64
                        );
                        self.perform_required_read(
                            &entries,
                            will_allocate_pooled_buffer,
                            required_access_physical_offset,
                            required_access_physical_size,
                            &mut read_func,
                        )?;
                        prev_entry = None;
                        required_access_physical_size = 0;
                        entries.clear();
                        will_allocate_pooled_buffer = false;
                    }
                }

                assert!(entries.len() < ENTRIES_COUNT_MAX);
                if entry.compression_type != CompressionType::None
                    || prev_entry.is_some_and(|prev| {
                        entry.virt_offset - prev.virt_offset != entry.phys_offset - prev.phys_offset
                    })
                {
                    will_allocate_pooled_buffer = true;
                }

                if compression_type_utility::is_data_storage_access_required(entry.compression_type)
                {
                    if entry.compression_type != CompressionType::None {
                        if data_offset != 0 {
                            return Err(RESULT_INVALID_OFFSET);
                        }
                        if virtual_data_size != read_size {
                            return Err(RESULT_INVALID_SIZE);
                        }
                        if entry.get_physical_size() > self.block_size_max as i64 {
                            return Err(RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_D);
                        }
                    }

                    let gap_from_prev = if required_access_physical_size > 0 {
                        physical_offset - required_access_physical_end
                    } else {
                        required_access_physical_offset = physical_offset;
                        0
                    };
                    required_access_physical_size += physical_size + gap_from_prev;
                    entries.push(ReadEntry {
                        compression_type: entry.compression_type,
                        gap_from_prev: gap_from_prev as u32,
                        physical_size: physical_size as u32,
                        virtual_size: read_size as u32,
                    });
                } else {
                    if entry.compression_type != CompressionType::Zeros {
                        return Err(RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_B);
                    }
                    if !entries.is_empty() {
                        if entry.get_physical_size() == 0 {
                            return Err(RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_D);
                        }
                        entries.push(ReadEntry {
                            compression_type: CompressionType::Zeros,
                            gap_from_prev: 0,
                            physical_size: 0,
                            virtual_size: read_size as u32,
                        });
                    } else {
                        let mut read_impl = |dst: &mut [u8]| {
                            assert_eq!(dst.len(), read_size as usize);
                            dst.fill(0);
                            Ok(())
                        };
                        read_func(read_size as usize, &mut read_impl)?;
                    }
                }

                prev_entry = Some(entry);
                *out_continuous = true;
                Ok(())
            },
        )?;

        if required_access_physical_size != 0 {
            self.perform_required_read(
                &entries,
                will_allocate_pooled_buffer,
                required_access_physical_offset,
                required_access_physical_size,
                &mut read_func,
            )?;
        }

        Ok(())
    }

    fn get_decompressor(
        &self,
        compression_type: CompressionType,
    ) -> Option<super::compression_common::DecompressorFunction> {
        if compression_type_utility::is_unknown_type(compression_type) {
            return None;
        }
        self.get_decompressor_function?(compression_type)
    }

    pub fn is_initialized(&self) -> bool {
        self.table.is_initialized()
    }
}

impl Drop for CompressedStorageCore {
    fn drop(&mut self) {
        self.finalize();
    }
}

/// Cache manager for compressed storage.
/// Corresponds to upstream `CompressedStorage::CacheManager`.
struct CacheManager {
    storage_size: i64,
}

#[derive(Debug, Clone, Copy, Default)]
struct AccessRange {
    virtual_offset: i64,
    virtual_size: i64,
    is_block_alignment_required: bool,
}

impl AccessRange {
    fn get_end_virtual_offset(self) -> i64 {
        self.virtual_offset + self.virtual_size
    }
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
        if offset > self.storage_size {
            return Err(RESULT_INVALID_OFFSET);
        }

        let read_size = size.min((self.storage_size - offset) as usize);
        let mut head_range = AccessRange::default();
        let mut tail_range = AccessRange::default();
        let mut is_tail_set = false;

        core.operate_per_entry(
            offset,
            1,
            |out_continuous, entry, virtual_data_size, _, _| {
                head_range = AccessRange {
                    virtual_offset: entry.virt_offset,
                    virtual_size: virtual_data_size,
                    is_block_alignment_required:
                        compression_type_utility::is_block_alignment_required(
                            entry.compression_type,
                        ),
                };
                if offset + read_size as i64 <= entry.virt_offset + virtual_data_size {
                    tail_range = head_range;
                    is_tail_set = true;
                }
                *out_continuous = false;
                Ok(())
            },
        )?;

        if !is_tail_set {
            core.operate_per_entry(
                offset + read_size as i64 - 1,
                1,
                |out_continuous, entry, virtual_data_size, _, _| {
                    tail_range = AccessRange {
                        virtual_offset: entry.virt_offset,
                        virtual_size: virtual_data_size,
                        is_block_alignment_required:
                            compression_type_utility::is_block_alignment_required(
                                entry.compression_type,
                            ),
                    };
                    *out_continuous = false;
                    Ok(())
                },
            )?;
        }

        let mut cur_offset = offset;
        let mut cur_size = read_size;
        let mut cur_dst = 0usize;
        let head_unaligned = head_range.is_block_alignment_required
            && (cur_offset != head_range.virtual_offset
                || (cur_size as i64) < head_range.virtual_size);
        let tail_unaligned = if tail_range.is_block_alignment_required {
            if cur_size as i64 + cur_offset == tail_range.get_end_virtual_offset() {
                false
            } else if !head_unaligned {
                true
            } else {
                head_range.get_end_virtual_offset() < cur_size as i64 + cur_offset
            }
        } else {
            false
        };

        let start_offset = if head_range.is_block_alignment_required {
            head_range.virtual_offset
        } else {
            cur_offset
        };
        let end_offset = if tail_range.is_block_alignment_required {
            tail_range.get_end_virtual_offset()
        } else {
            cur_offset + cur_size as i64
        };

        let mut is_burst_reading = false;
        core.read(
            start_offset,
            end_offset - start_offset,
            |size_buffer_required, read_impl| {
                let unaligned_range = if !is_burst_reading {
                    if head_unaligned
                        && head_range.virtual_offset <= cur_offset
                        && cur_offset < head_range.get_end_virtual_offset()
                    {
                        Some(head_range)
                    } else if tail_unaligned
                        && tail_range.virtual_offset <= cur_offset
                        && cur_offset < tail_range.get_end_virtual_offset()
                    {
                        Some(tail_range)
                    } else {
                        is_burst_reading = true;
                        None
                    }
                } else {
                    None
                };
                assert!(is_burst_reading ^ unaligned_range.is_some());

                if is_burst_reading {
                    assert!(size_buffer_required <= cur_size);
                    read_impl(&mut buffer[cur_dst..cur_dst + size_buffer_required])?;
                    cur_dst += size_buffer_required;
                    cur_offset += size_buffer_required as i64;
                    cur_size -= size_buffer_required;

                    let offset_aligned = if tail_unaligned {
                        tail_range.virtual_offset
                    } else {
                        end_offset
                    };
                    assert!(cur_offset <= offset_aligned);
                    if offset_aligned <= cur_offset {
                        is_burst_reading = false;
                    }
                } else {
                    let unaligned_range = unaligned_range.unwrap();
                    assert_eq!(size_buffer_required, unaligned_range.virtual_size as usize);
                    let mut pooled_buffer = vec![0u8; size_buffer_required];
                    read_impl(&mut pooled_buffer)?;

                    let skip_size = (cur_offset - unaligned_range.virtual_offset) as usize;
                    let copy_size = cur_size
                        .min((unaligned_range.get_end_virtual_offset() - cur_offset) as usize);
                    buffer[cur_dst..cur_dst + copy_size]
                        .copy_from_slice(&pooled_buffer[skip_size..skip_size + copy_size]);
                    cur_dst += copy_size;
                    cur_offset += copy_size as i64;
                    cur_size -= copy_size;
                }

                Ok(())
            },
        )?;

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
        self.cache_manager
            .initialize(core_size, cache_size_0, cache_size_1, max_cache_entries)?;

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

impl Drop for CompressedStorage {
    fn drop(&mut self) {
        self.finalize();
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
        let storage_size = self.core.get_size().unwrap_or(0) as usize;
        let read_len = actual_len.min(storage_size.saturating_sub(offset));
        if self
            .cache_manager
            .read(&self.core, offset as i64, &mut buffer[..read_len], read_len)
            .is_ok()
        {
            read_len
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
    use crate::file_sys::vfs::vfs_vector::VectorVfsFile;
    use std::sync::Arc;

    fn expand_pairs(dst: &mut [u8], src: &[u8]) -> Result<(), ResultCode> {
        assert_eq!(dst.len(), src.len() * 2);
        for (pair, value) in dst.chunks_exact_mut(2).zip(src.iter().copied()) {
            pair.fill(value);
        }
        Ok(())
    }

    fn test_decompressor(
        compression_type: CompressionType,
    ) -> Option<super::super::compression_common::DecompressorFunction> {
        match compression_type {
            CompressionType::Lz4 => Some(expand_pairs),
            _ => None,
        }
    }

    fn make_compressed_storage(
        entries: &[Entry],
        end_offset: i64,
        data: Vec<u8>,
    ) -> CompressedStorage {
        assert!(!entries.is_empty());
        let mut node = vec![0u8; NODE_SIZE];
        node[0..4].copy_from_slice(&0i32.to_le_bytes());
        node[4..8].copy_from_slice(&1i32.to_le_bytes());
        node[8..16].copy_from_slice(&end_offset.to_le_bytes());
        node[16..24].copy_from_slice(&entries[0].virt_offset.to_le_bytes());

        let mut entry_storage = vec![0u8; NODE_SIZE];
        entry_storage[0..4].copy_from_slice(&0i32.to_le_bytes());
        entry_storage[4..8].copy_from_slice(&(entries.len() as i32).to_le_bytes());
        entry_storage[8..16].copy_from_slice(&end_offset.to_le_bytes());
        for (index, entry) in entries.iter().enumerate() {
            let offset = std::mem::size_of::<super::super::bucket_tree::NodeHeader>()
                + index * std::mem::size_of::<Entry>();
            let bytes = unsafe {
                std::slice::from_raw_parts(
                    (entry as *const Entry).cast::<u8>(),
                    std::mem::size_of::<Entry>(),
                )
            };
            entry_storage[offset..offset + bytes.len()].copy_from_slice(bytes);
        }

        let mut storage = CompressedStorage::new();
        storage
            .initialize(
                Arc::new(VectorVfsFile::new(data, String::new(), None)),
                Arc::new(VectorVfsFile::new(node, String::new(), None)),
                Arc::new(VectorVfsFile::new(entry_storage, String::new(), None)),
                entries.len() as i32,
                0x100,
                0x1000,
                test_decompressor,
                0,
                0,
                0,
            )
            .unwrap();
        storage
    }

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

    #[test]
    fn uncompressed_read_uses_physical_offset_and_clamps_at_virtual_end() {
        let storage = make_compressed_storage(
            &[Entry {
                virt_offset: 0,
                phys_offset: 4,
                compression_type: CompressionType::None,
                _padding: [0; 3],
                phys_size: 8,
            }],
            8,
            (0u8..16).collect(),
        );
        let mut output = [0xFF; 8];

        assert_eq!(storage.read(&mut output, 8, 2), 6);
        assert_eq!(&output[..6], &[6, 7, 8, 9, 10, 11]);
        assert_eq!(&output[6..], &[0xFF, 0xFF]);
    }

    #[test]
    fn zero_entry_fills_requested_virtual_range() {
        let storage = make_compressed_storage(
            &[Entry {
                virt_offset: 0,
                phys_offset: 0,
                compression_type: CompressionType::Zeros,
                _padding: [0; 3],
                phys_size: 0,
            }],
            8,
            Vec::new(),
        );
        let mut output = [0xFF; 4];

        assert_eq!(storage.read(&mut output, 4, 2), 4);
        assert_eq!(output, [0; 4]);
    }

    #[test]
    fn compressed_partial_read_decompresses_full_aligned_entry_then_slices() {
        let storage = make_compressed_storage(
            &[Entry {
                virt_offset: 0,
                phys_offset: 0,
                compression_type: CompressionType::Lz4,
                _padding: [0; 3],
                phys_size: 2,
            }],
            4,
            vec![0x12, 0x34],
        );
        let mut output = [0; 2];

        assert_eq!(storage.read(&mut output, 2, 1), 2);
        assert_eq!(output, [0x12, 0x34]);
    }

    #[test]
    fn contiguous_uncompressed_entries_are_read_as_one_physical_range() {
        let storage = make_compressed_storage(
            &[
                Entry {
                    virt_offset: 0,
                    phys_offset: 2,
                    compression_type: CompressionType::None,
                    _padding: [0; 3],
                    phys_size: 4,
                },
                Entry {
                    virt_offset: 4,
                    phys_offset: 6,
                    compression_type: CompressionType::None,
                    _padding: [0; 3],
                    phys_size: 4,
                },
            ],
            8,
            (0u8..12).collect(),
        );
        let mut output = [0; 8];

        assert_eq!(storage.read(&mut output, 8, 0), 8);
        assert_eq!(output, [2, 3, 4, 5, 6, 7, 8, 9]);
    }

    #[test]
    fn discontinuous_uncompressed_entries_skip_the_physical_gap() {
        let storage = make_compressed_storage(
            &[
                Entry {
                    virt_offset: 0,
                    phys_offset: 0,
                    compression_type: CompressionType::None,
                    _padding: [0; 3],
                    phys_size: 4,
                },
                Entry {
                    virt_offset: 4,
                    phys_offset: 8,
                    compression_type: CompressionType::None,
                    _padding: [0; 3],
                    phys_size: 4,
                },
            ],
            8,
            (0u8..12).collect(),
        );
        let mut output = [0; 8];

        assert_eq!(storage.read(&mut output, 8, 0), 8);
        assert_eq!(output, [0, 1, 2, 3, 8, 9, 10, 11]);
    }
}
