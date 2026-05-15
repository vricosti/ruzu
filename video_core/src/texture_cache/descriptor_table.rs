// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/descriptor_table.h
//!
//! Generic descriptor table that reads GPU-memory-resident descriptor arrays
//! on demand, tracking which entries have been read and whether they changed.

use super::image_base::GPUVAddr;

/// Read N bytes from a GPU device address into `output`.
///
/// Port of upstream `Tegra::MemoryManager::ReadBlockUnsafe`: the descriptor
/// table calls this with the table base + `index * sizeof(Descriptor)` so
/// `Read(index)` lands on a real TICEntry / TSCEntry from GPU memory.
/// Returns `true` if every byte was successfully read (all underlying
/// pages mapped). Returning `false` is treated as a transient read failure
/// — `Read` then falls back to the cached descriptor + reports `changed=false`.
pub trait GpuMemoryReader {
    fn read_block(&self, d_address: u64, output: &mut [u8]) -> bool;
}

// ── DescriptorTable<T> ────────────────────────────────────────────────

/// A lazily-synchronised descriptor array backed by GPU memory.
///
/// Port of `VideoCommon::DescriptorTable<Descriptor>`.
///
/// The upstream implementation stores a `Tegra::MemoryManager&` reference;
/// ruzu instead threads a `&dyn GpuMemoryReader` through `Read` so the
/// table doesn't need a back-reference to the memory manager. The caller
/// (texture-cache visit_image_view) already has access to the channel's
/// `MaxwellDeviceMemoryManager` via the cache's `device_memory` Arc.
///
/// `T` must be `Copy + PartialEq + Default`. Reads are done via a byte
/// buffer + `ptr::read_unaligned`, so `T` should be a plain
/// `#[repr(C)]` POD type (TICEntry / TSCEntry both qualify — fixed-size
/// `[u64; 4]` wrappers).
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
    /// Port of upstream `DescriptorTable::Read`: reads `sizeof(Descriptor)`
    /// bytes from `current_gpu_addr + index * sizeof(Descriptor)` via the
    /// supplied `gpu_memory` reader. The caller (texture-cache
    /// `visit_image_view`) wires this up to the channel's
    /// `MaxwellDeviceMemoryManager::smmu_read_block`.
    ///
    /// If the GPU read fails (page not mapped at the table location)
    /// falls back to the previously-cached descriptor with `changed=false`,
    /// so a transient unmapped page does not poison the cache with a
    /// `Default::default()` value.
    pub fn read(&mut self, gpu_memory: &dyn GpuMemoryReader, index: u32) -> (T, bool) {
        debug_assert!(index <= self.current_limit);
        let item_size = std::mem::size_of::<T>();
        let descriptor_addr = self.current_gpu_addr + (index as u64) * (item_size as u64);

        let mut buf = vec![0u8; item_size];
        let descriptor = if gpu_memory.read_block(descriptor_addr, &mut buf) {
            // SAFETY: `T` is bounded by `Copy + PartialEq + Default`. The
            // descriptor table is only instantiated with `TicEntry` /
            // `TscEntry`, both `#[repr(C)]` `[u64; 4]` wrappers with no
            // invalid bit patterns. `read_block` writes exactly
            // `item_size` bytes into `buf` when it returns `true`.
            unsafe { std::ptr::read_unaligned(buf.as_ptr() as *const T) }
        } else {
            // Read failure (unmapped page at the descriptor location).
            // Upstream would have UB'd through ReadBlockUnsafe; ruzu
            // treats it as "descriptor unchanged" so the cached image
            // view id is reused.
            self.descriptors[index as usize]
        };

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
