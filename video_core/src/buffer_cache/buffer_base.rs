// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `video_core/buffer_cache/buffer_base.h`
//!
//! Range-tracking buffer container.
//!
//! Keeps track of the modified CPU and GPU ranges on a CPU page granularity,
//! notifying the given rasterizer about state changes in the tracking behavior
//! of the buffer.
//!
//! The buffer size and address is forcefully aligned to CPU page boundaries.

use bitflags::bitflags;
use common::types::VAddr;

// ---------------------------------------------------------------------------
// BufferFlagBits
// ---------------------------------------------------------------------------

bitflags! {
    /// Flags associated with a buffer instance.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
    pub struct BufferFlagBits: u32 {
        const PICKED             = 1 << 0;
        const CACHED_WRITES      = 1 << 1;
        const PREEMTIVE_DOWNLOAD = 1 << 2;
    }
}

// ---------------------------------------------------------------------------
// NullBufferParams
// ---------------------------------------------------------------------------

/// Tag for creating null buffers with no storage or size.
pub struct NullBufferParams;

// ---------------------------------------------------------------------------
// BufferBase
// ---------------------------------------------------------------------------

/// Constants from the upstream `BufferBase` class.
pub const BASE_PAGE_BITS: u64 = 16;
pub const BASE_PAGE_SIZE: u64 = 1u64 << BASE_PAGE_BITS;

/// Base buffer tracking structure.
///
/// This is the Rust counterpart of the C++ `BufferBase` class. It holds the
/// CPU address, size, flags, stream score, and LRU cache identifier but does
/// not own any GPU-side resource — that is the responsibility of the
/// backend-specific `Buffer` type.
pub struct BufferBase {
    cpu_addr: VAddr,
    flags: BufferFlagBits,
    stream_score: i32,
    lru_id: usize,
    size_bytes: usize,
}

impl BufferBase {
    /// Create a new buffer base for the given CPU address and size.
    pub fn new(cpu_addr: VAddr, size_bytes: u64) -> Self {
        Self {
            cpu_addr,
            flags: BufferFlagBits::empty(),
            stream_score: 0,
            lru_id: usize::MAX,
            size_bytes: size_bytes as usize,
        }
    }

    /// Create a null buffer (no storage, no size).
    pub fn null(_params: NullBufferParams) -> Self {
        Self {
            cpu_addr: 0,
            flags: BufferFlagBits::empty(),
            stream_score: 0,
            lru_id: usize::MAX,
            size_bytes: 0,
        }
    }

    /// Mark buffer as picked.
    #[inline]
    pub fn pick(&mut self) {
        self.flags |= BufferFlagBits::PICKED;
    }

    /// Mark buffer for preemptive download.
    #[inline]
    pub fn mark_preemtive_download(&mut self) {
        self.flags |= BufferFlagBits::PREEMTIVE_DOWNLOAD;
    }

    /// Unmark buffer as picked.
    #[inline]
    pub fn unpick(&mut self) {
        self.flags -= BufferFlagBits::PICKED;
    }

    /// Increases the likeliness of this being a stream buffer.
    #[inline]
    pub fn increase_stream_score(&mut self, score: i32) {
        self.stream_score += score;
    }

    /// Returns the likeliness of this being a stream buffer.
    #[inline]
    pub fn stream_score(&self) -> i32 {
        self.stream_score
    }

    /// Returns true when `vaddr .. vaddr+size` is fully contained in the buffer.
    #[inline]
    pub fn is_in_bounds(&self, addr: VAddr, size: u64) -> bool {
        addr >= self.cpu_addr && addr + size <= self.cpu_addr + self.size_bytes() as u64
    }

    /// Returns true if the buffer has been marked as picked.
    #[inline]
    pub fn is_picked(&self) -> bool {
        self.flags.contains(BufferFlagBits::PICKED)
    }

    /// Returns true when the buffer has pending cached writes.
    #[inline]
    pub fn has_cached_writes(&self) -> bool {
        self.flags.contains(BufferFlagBits::CACHED_WRITES)
    }

    /// Returns true when the buffer has been marked for preemptive download.
    #[inline]
    pub fn is_preemtive_download(&self) -> bool {
        self.flags.contains(BufferFlagBits::PREEMTIVE_DOWNLOAD)
    }

    /// Returns the base CPU address of the buffer.
    #[inline]
    pub fn cpu_addr(&self) -> VAddr {
        self.cpu_addr
    }

    /// Returns the offset relative to the given CPU address.
    ///
    /// Precondition: `is_in_bounds` returns true for this address.
    #[inline]
    pub fn offset(&self, other_cpu_addr: VAddr) -> u32 {
        (other_cpu_addr - self.cpu_addr) as u32
    }

    /// Returns the LRU cache identifier.
    #[inline]
    pub fn get_lru_id(&self) -> usize {
        self.lru_id
    }

    /// Sets the LRU cache identifier.
    #[inline]
    pub fn set_lru_id(&mut self, lru_id: usize) {
        self.lru_id = lru_id;
    }

    /// Returns the size in bytes.
    #[inline]
    pub fn size_bytes(&self) -> usize {
        self.size_bytes
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_null_buffer() {
        let buf = BufferBase::null(NullBufferParams);
        assert_eq!(buf.cpu_addr(), 0);
        assert_eq!(buf.size_bytes(), 0);
        assert!(!buf.is_picked());
    }

    #[test]
    fn test_pick_unpick() {
        let mut buf = BufferBase::new(0x1000, 0x2000);
        assert!(!buf.is_picked());
        buf.pick();
        assert!(buf.is_picked());
        buf.unpick();
        assert!(!buf.is_picked());
    }

    #[test]
    fn test_is_in_bounds() {
        let buf = BufferBase::new(0x1000, 0x2000);
        assert!(buf.is_in_bounds(0x1000, 0x2000));
        assert!(buf.is_in_bounds(0x1500, 0x500));
        assert!(!buf.is_in_bounds(0x0FFF, 1));
        assert!(!buf.is_in_bounds(0x1000, 0x2001));
    }

    #[test]
    fn test_offset() {
        let buf = BufferBase::new(0x1000, 0x2000);
        assert_eq!(buf.offset(0x1000), 0);
        assert_eq!(buf.offset(0x1100), 0x100);
    }

    #[test]
    fn test_stream_score() {
        let mut buf = BufferBase::new(0x1000, 0x100);
        assert_eq!(buf.stream_score(), 0);
        buf.increase_stream_score(5);
        assert_eq!(buf.stream_score(), 5);
        buf.increase_stream_score(-2);
        assert_eq!(buf.stream_score(), 3);
    }

    #[test]
    fn test_lru_id() {
        let mut buf = BufferBase::new(0x1000, 0x100);
        assert_eq!(buf.get_lru_id(), usize::MAX);
        buf.set_lru_id(42);
        assert_eq!(buf.get_lru_id(), 42);
    }
}
