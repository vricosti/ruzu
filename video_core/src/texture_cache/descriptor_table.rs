// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/descriptor_table.h
//!
//! Generic descriptor table that reads GPU-memory-resident descriptor arrays
//! on demand, tracking which entries have been read and whether they changed.

use super::image_base::GPUVAddr;

// ── DescriptorTable<T> ────────────────────────────────────────────────

/// A lazily-synchronised descriptor array backed by GPU memory.
///
/// Port of `VideoCommon::DescriptorTable<Descriptor>`.
///
/// The upstream implementation reads descriptors directly from
/// `Tegra::MemoryManager`.  Here we store a type-erased reference to the
/// GPU memory manager and perform the same caching logic.
///
/// `T` must be `Copy + PartialEq + Default`.
pub struct DescriptorTable<T: Copy + PartialEq + Default> {
    current_gpu_addr: GPUVAddr,
    current_limit: u32,
    /// Bitset: one bit per descriptor; 1 = previously read.
    read_descriptors: Vec<u64>,
    /// Cached descriptor values.
    descriptors: Vec<T>,
}

impl<T: Copy + PartialEq + Default> DescriptorTable<T> {
    /// Create a new, empty descriptor table.
    pub fn new() -> Self {
        Self {
            current_gpu_addr: 0,
            current_limit: 0,
            read_descriptors: Vec::new(),
            descriptors: Vec::new(),
        }
    }

    /// Synchronise the table pointer and limit.
    ///
    /// Returns `true` if the table was refreshed (address or limit changed).
    ///
    /// Port of `DescriptorTable::Synchronize`.
    pub fn synchronize(&mut self, gpu_addr: GPUVAddr, limit: u32) -> bool {
        if self.current_gpu_addr == gpu_addr && self.current_limit == limit {
            return false;
        }
        self.refresh(gpu_addr, limit);
        true
    }

    /// Mark all descriptors as unread.
    ///
    /// Port of `DescriptorTable::Invalidate`.
    pub fn invalidate(&mut self) {
        self.read_descriptors.fill(0);
    }

    /// Read a descriptor at `index`.
    ///
    /// Returns `(descriptor, changed)`.  `changed` is `true` if this is the
    /// first read or the value differs from the cached copy.
    ///
    /// Port of `DescriptorTable::Read`.
    ///
    /// NOTE: The actual GPU memory read is stubbed out — the full
    /// implementation requires a reference to `Tegra::MemoryManager` which
    /// is not yet ported.
    pub fn read(&mut self, index: u32) -> (T, bool) {
        debug_assert!(index <= self.current_limit);
        // TODO: gpu_memory.read_block_unsafe(gpu_addr + index * size_of::<T>(), &descriptor)
        let descriptor = T::default(); // placeholder

        let changed = if self.is_descriptor_read(index) {
            descriptor != self.descriptors[index as usize]
        } else {
            self.mark_descriptor_as_read(index);
            true
        };

        if changed {
            self.descriptors[index as usize] = descriptor;
        }
        (descriptor, changed)
    }

    /// Current descriptor limit.
    pub fn limit(&self) -> u32 {
        self.current_limit
    }

    // ── Private helpers ────────────────────────────────────────────────

    fn refresh(&mut self, gpu_addr: GPUVAddr, limit: u32) {
        self.current_gpu_addr = gpu_addr;
        self.current_limit = limit;

        let num_descriptors = limit as usize + 1;
        self.read_descriptors.clear();
        self.read_descriptors.resize((num_descriptors + 63) / 64, 0);
        self.descriptors.resize(num_descriptors, T::default());
    }

    fn mark_descriptor_as_read(&mut self, index: u32) {
        self.read_descriptors[(index / 64) as usize] |= 1u64 << (index % 64);
    }

    fn is_descriptor_read(&self, index: u32) -> bool {
        (self.read_descriptors[(index / 64) as usize] & (1u64 << (index % 64))) != 0
    }
}

impl<T: Copy + PartialEq + Default> Default for DescriptorTable<T> {
    fn default() -> Self {
        Self::new()
    }
}
