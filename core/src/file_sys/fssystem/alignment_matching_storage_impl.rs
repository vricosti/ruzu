// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/file_sys/fssystem/fssystem_alignment_matching_storage_impl.h/.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-12
//!
//! Provides aligned read/write operations for storage backends that
//! require specific alignment. The core logic splits a read/write into:
//! - Head portion (unaligned start)
//! - Core portion (aligned middle, directly into user buffer when possible)
//! - Tail portion (unaligned end)

use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_types::VirtualFile;

/// Round down `x` to alignment `align`, return the difference.
#[inline]
fn get_round_down_difference(x: usize, align: usize) -> usize {
    x - align_down(x, align)
}

/// Round up `x` to alignment `align`, return the difference.
#[inline]
fn get_round_up_difference(x: usize, align: usize) -> usize {
    align_up(x, align) - x
}

#[inline]
fn align_down(x: usize, align: usize) -> usize {
    x & !(align - 1)
}

#[inline]
fn align_up(x: usize, align: usize) -> usize {
    align_down(x + align - 1, align)
}

#[inline]
fn is_aligned(x: usize, align: usize) -> bool {
    x & (align - 1) == 0
}

pub struct AlignmentMatchingStorageImpl;

impl AlignmentMatchingStorageImpl {
    /// Read with alignment matching.
    ///
    /// Corresponds to upstream `AlignmentMatchingStorageImpl::Read`.
    ///
    /// Handles the case where the underlying storage requires reads to be
    /// aligned to `data_alignment`, and the destination buffer may have
    /// its own alignment requirement `buffer_alignment`.
    pub fn read(
        base_storage: &VirtualFile,
        work_buf: &mut [u8],
        _work_buf_size: usize,
        data_alignment: usize,
        buffer_alignment: usize,
        offset: usize,
        buffer: &mut [u8],
        size: usize,
    ) -> usize {
        // Check preconditions.
        assert!(work_buf.len() >= data_alignment);

        // Succeed if zero size.
        if size == 0 {
            return size;
        }

        // Determine extents.
        let offset_round_up_diff = get_round_up_difference(offset, data_alignment);

        // Check if the buffer is usable for aligned core reads.
        let buf_ptr = buffer.as_ptr() as usize;
        let (core_offset, core_size, buffer_gap, offset_gap, covered_offset);

        if is_aligned(buf_ptr + offset_round_up_diff, buffer_alignment) {
            core_offset = align_up(offset, data_alignment);
            core_size = if size < offset_round_up_diff {
                0
            } else {
                align_down(size - offset_round_up_diff, data_alignment)
            };
            buffer_gap = 0;
            offset_gap = 0;
            covered_offset = if core_size > 0 { core_offset } else { offset };
        } else {
            let buffer_round_up_diff = get_round_up_difference(buf_ptr, buffer_alignment);
            core_offset = align_down(offset, data_alignment);
            core_size = if size < buffer_round_up_diff {
                0
            } else {
                align_down(size - buffer_round_up_diff, data_alignment)
            };
            buffer_gap = buffer_round_up_diff;
            offset_gap = get_round_down_difference(offset, data_alignment);
            covered_offset = offset;
        }

        // Read the core portion.
        if core_size > 0 {
            let core_buf_start = buffer_gap;
            // Buffer may not be perfectly aligned here; we read into it offset
            // by buffer_gap from the start.
            // Read is offset_gap bytes earlier than needed, so we shift after.
            let read_end = core_buf_start + core_size;
            // Safety: we must have enough room in buffer.
            if read_end <= buffer.len() {
                base_storage.read(
                    &mut buffer[core_buf_start..read_end],
                    core_size,
                    core_offset,
                );

                if offset_gap != 0 || buffer_gap != 0 {
                    // Shift data from aligned_core_buffer + offset_gap to
                    // aligned_core_buffer - buffer_gap.
                    let src_start = core_buf_start + offset_gap;
                    let copy_len = core_size - offset_gap;
                    let dst_start = core_buf_start - buffer_gap;
                    buffer.copy_within(src_start..src_start + copy_len, dst_start);
                }
            }
        }

        // Effective core size after adjustment.
        let effective_core_size = if core_size > offset_gap {
            core_size - offset_gap
        } else {
            0
        };

        // Handle the head portion.
        if offset < covered_offset {
            let head_offset = align_down(offset, data_alignment);
            let head_size = covered_offset - offset;

            base_storage.read(work_buf, data_alignment, head_offset);
            let src_offset = get_round_down_difference(offset, data_alignment);
            buffer[..head_size].copy_from_slice(&work_buf[src_offset..src_offset + head_size]);
        }

        // Handle the tail portion.
        let mut tail_offset = covered_offset + effective_core_size;
        let end = offset + size;
        while tail_offset < end {
            let aligned_tail_offset = align_down(tail_offset, data_alignment);
            let remaining = end - tail_offset;
            let max_from_aligned = aligned_tail_offset + data_alignment - tail_offset;
            let cur_size = remaining.min(max_from_aligned);

            base_storage.read(work_buf, data_alignment, aligned_tail_offset);

            let buf_offset = tail_offset - offset;
            let work_offset = tail_offset - aligned_tail_offset;
            buffer[buf_offset..buf_offset + cur_size]
                .copy_from_slice(&work_buf[work_offset..work_offset + cur_size]);

            tail_offset += cur_size;
        }

        size
    }

    /// Write with alignment matching.
    ///
    /// Corresponds to upstream `AlignmentMatchingStorageImpl::Write`.
    ///
    /// Handles unaligned writes by performing read-modify-write for head/tail
    /// portions that don't fall on alignment boundaries.
    pub fn write(
        base_storage: &VirtualFile,
        work_buf: &mut [u8],
        _work_buf_size: usize,
        data_alignment: usize,
        buffer_alignment: usize,
        offset: usize,
        buffer: &[u8],
        size: usize,
    ) -> usize {
        // Check preconditions.
        assert!(work_buf.len() >= data_alignment);

        // Succeed if zero size.
        if size == 0 {
            return size;
        }

        // Determine extents.
        let offset_round_up_diff = get_round_up_difference(offset, data_alignment);
        let buf_ptr = buffer.as_ptr() as usize;

        let (core_offset, core_size, covered_offset);

        if is_aligned(buf_ptr + offset_round_up_diff, buffer_alignment) {
            core_offset = align_up(offset, data_alignment);
            core_size = if size < offset_round_up_diff {
                0
            } else {
                align_down(size - offset_round_up_diff, data_alignment)
            };
            covered_offset = if core_size > 0 { core_offset } else { offset };
        } else {
            core_offset = align_down(offset, data_alignment);
            core_size = 0;
            covered_offset = offset;
        }

        // Write the core portion.
        if core_size > 0 {
            let buf_start = core_offset - offset;
            base_storage.write(&buffer[buf_start..buf_start + core_size], core_size, core_offset);
        }

        // Handle the head portion.
        if offset < covered_offset {
            let head_offset = align_down(offset, data_alignment);
            let head_size = covered_offset - offset;

            base_storage.read(work_buf, data_alignment, head_offset);
            let dst_offset = offset - head_offset;
            work_buf[dst_offset..dst_offset + head_size]
                .copy_from_slice(&buffer[..head_size]);
            base_storage.write(work_buf, data_alignment, head_offset);
        }

        // Handle the tail portion.
        let mut tail_offset = covered_offset + core_size;
        let end = offset + size;
        while tail_offset < end {
            let aligned_tail_offset = align_down(tail_offset, data_alignment);
            let remaining = end - tail_offset;
            let max_from_aligned = aligned_tail_offset + data_alignment - tail_offset;
            let cur_size = remaining.min(max_from_aligned);

            base_storage.read(work_buf, data_alignment, aligned_tail_offset);
            let work_offset = get_round_down_difference(tail_offset, data_alignment);
            let buf_offset = tail_offset - offset;
            work_buf[work_offset..work_offset + cur_size]
                .copy_from_slice(&buffer[buf_offset..buf_offset + cur_size]);
            base_storage.write(work_buf, data_alignment, aligned_tail_offset);

            tail_offset += cur_size;
        }

        size
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_align_helpers() {
        assert_eq!(align_down(0x1234, 0x100), 0x1200);
        assert_eq!(align_up(0x1234, 0x100), 0x1300);
        assert_eq!(align_up(0x1200, 0x100), 0x1200);
        assert!(is_aligned(0x1000, 0x100));
        assert!(!is_aligned(0x1001, 0x100));
        assert_eq!(get_round_down_difference(0x1234, 0x100), 0x34);
        assert_eq!(get_round_up_difference(0x1234, 0x100), 0xCC);
    }
}
