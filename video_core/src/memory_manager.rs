// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GPU virtual address space manager.
//!
//! Port of zuyu/src/video_core/memory_manager.h and memory_manager.cpp.
//!
//! Implements dual page table architecture (big 64KB pages + small 4KB pages)
//! with bitpacked entry arrays, continuity tracking, and kind mapping.

use common::multi_level_page_table::MultiLevelPageTable;
use common::range_map::RangeMap;
use common::virtual_buffer::VirtualBuffer;

use crate::pte_kind::PteKind;

use std::sync::atomic::{AtomicUsize, Ordering};

// ── Constants ───────────────────────────────────────────────────────────

/// CPU page bits — upstream `cpu_page_bits = 12`.
const CPU_PAGE_BITS: u64 = 12;

/// Number of entries packed per u64 (2 bits each).
const ENTRIES_PER_U64: usize = 32;

/// Number of continuity bits per u64.
const CONTINUOUS_BITS: usize = 64;

/// Device page size (4 KB) — matches upstream Core::DEVICE_PAGESIZE.
const DEVICE_PAGE_SIZE: u64 = 1 << 12;
const DEVICE_PAGE_MASK: u64 = DEVICE_PAGE_SIZE - 1;

// ── Entry type ──────────────────────────────────────────────────────────

/// Page entry state, packed as 2 bits in the entry arrays.
///
/// Upstream: `MemoryManager::EntryType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u64)]
enum EntryType {
    Free = 0,
    Reserved = 1,
    Mapped = 2,
}

// ── Static ID generator ────────────────────────────────────────────────

static UNIQUE_IDENTIFIER_GENERATOR: AtomicUsize = AtomicUsize::new(0);

// ── GpuMemoryManager (inner implementation) ─────────────────────────────

/// GPU virtual memory manager with dual page tables.
///
/// This is the inner implementation corresponding to upstream `Tegra::MemoryManager`.
/// It is wrapped by the outer `MemoryManager` struct that adds the `Arc<Mutex<>>` layer.
///
/// Architecture:
/// - Big page table (`big_page_table_dev`): `VirtualBuffer<u32>` storing `dev_addr >> CPU_PAGE_BITS`
///   per big page (default 64KB).
/// - Small page table (`page_table`): `MultiLevelPageTable<u32>` storing `dev_addr >> CPU_PAGE_BITS`
///   per 4KB page.
/// - Bitpacked entry arrays: `Vec<u64>` with 2 bits per page (Free=0, Reserved=1, Mapped=2).
/// - Big page continuity bitmap: `Vec<u64>` with 1 bit per big page.
/// - Addresses below `split_address` (default `1 << 34`) use big pages; above use small pages.
pub struct GpuMemoryManager {
    // Geometry
    address_space_bits: u64,
    address_space_size: u64,
    split_address: u64,
    page_bits: u64,
    page_size: u64,
    page_mask: u64,
    page_table_mask: u64,
    big_page_bits: u64,
    big_page_size: u64,
    big_page_mask: u64,
    big_page_table_mask: u64,

    // Small page table (addresses >= split_address)
    page_table: MultiLevelPageTable<u32>,
    /// Bitpacked entry types for small pages, 2 bits each, 32 per u64.
    entries: Vec<u64>,

    // Big page table (addresses < split_address)
    big_page_table_dev: VirtualBuffer<u32>,
    /// Bitpacked entry types for big pages, 2 bits each, 32 per u64.
    big_entries: Vec<u64>,
    /// Continuity bitmap for big pages, 1 bit each, 64 per u64.
    big_page_continuous: Vec<u64>,

    // Kind tracking
    kind_map: RangeMap<PteKind>,

    // Unique identifier for rasterizer callbacks
    unique_identifier: usize,
}

impl GpuMemoryManager {
    /// Upstream: `MemoryManager::MemoryManager(system, memory, address_space_bits, split_address,
    ///            big_page_bits, page_bits)`.
    pub fn new() -> Self {
        Self::with_params(40, 1u64 << 34, 16, 12)
    }

    pub fn with_params(
        address_space_bits: u64,
        split_address: u64,
        big_page_bits: u64,
        page_bits: u64,
    ) -> Self {
        let address_space_size = 1u64 << address_space_bits;
        let page_size = 1u64 << page_bits;
        let page_mask = page_size - 1;
        let big_page_size = 1u64 << big_page_bits;
        let big_page_mask = big_page_size - 1;

        let page_table_bits = address_space_bits - page_bits;
        let big_page_table_bits = address_space_bits - big_page_bits;
        let page_table_size = 1u64 << page_table_bits;
        let big_page_table_size = 1u64 << big_page_table_bits;
        let page_table_mask = page_table_size - 1;
        let big_page_table_mask = big_page_table_size - 1;

        // Upstream: page_table{address_space_bits, address_space_bits + page_bits - 38,
        //                       page_bits != big_page_bits ? page_bits : 0}
        let first_level_bits = (address_space_bits + page_bits).saturating_sub(38);
        let effective_page_bits = if page_bits != big_page_bits {
            page_bits
        } else {
            0
        };
        let page_table = MultiLevelPageTable::<u32>::with_params(
            address_space_bits as usize,
            first_level_bits as usize,
            effective_page_bits as usize,
        );

        let mut big_page_table_dev = VirtualBuffer::<u32>::new();
        big_page_table_dev.resize(big_page_table_size as usize);

        let big_entries = vec![0u64; (big_page_table_size as usize) / ENTRIES_PER_U64];
        let big_page_continuous = vec![0u64; (big_page_table_size as usize) / CONTINUOUS_BITS];
        let entries = vec![0u64; (page_table_size as usize) / ENTRIES_PER_U64];

        let unique_identifier =
            UNIQUE_IDENTIFIER_GENERATOR.fetch_add(1, Ordering::AcqRel);

        Self {
            address_space_bits,
            address_space_size,
            split_address,
            page_bits,
            page_size,
            page_mask,
            page_table_mask,
            big_page_bits,
            big_page_size,
            big_page_mask,
            big_page_table_mask,
            page_table,
            entries,
            big_page_table_dev,
            big_entries,
            big_page_continuous,
            kind_map: RangeMap::new(PteKind::Invalid),
            unique_identifier,
        }
    }

    /// Upstream: `MemoryManager::GetID()`.
    pub fn get_id(&self) -> usize {
        self.unique_identifier
    }

    // ── Entry access (bitpacked) ────────────────────────────────────────

    /// Upstream: `GetEntry<true>(position)`.
    fn get_entry_big(&self, position: u64) -> EntryType {
        let idx = (position >> self.big_page_bits) as usize;
        let entry_mask = self.big_entries[idx / ENTRIES_PER_U64];
        let sub_index = idx % ENTRIES_PER_U64;
        match (entry_mask >> (2 * sub_index)) & 0x03 {
            0 => EntryType::Free,
            1 => EntryType::Reserved,
            2 => EntryType::Mapped,
            _ => EntryType::Free,
        }
    }

    /// Upstream: `GetEntry<false>(position)`.
    fn get_entry_small(&self, position: u64) -> EntryType {
        let idx = (position >> self.page_bits) as usize;
        let entry_mask = self.entries[idx / ENTRIES_PER_U64];
        let sub_index = idx % ENTRIES_PER_U64;
        match (entry_mask >> (2 * sub_index)) & 0x03 {
            0 => EntryType::Free,
            1 => EntryType::Reserved,
            2 => EntryType::Mapped,
            _ => EntryType::Free,
        }
    }

    /// Upstream: `SetEntry<true>(position, entry)`.
    fn set_entry_big(&mut self, position: u64, entry: EntryType) {
        let idx = (position >> self.big_page_bits) as usize;
        let slot = idx / ENTRIES_PER_U64;
        let sub_index = idx % ENTRIES_PER_U64;
        let entry_mask = self.big_entries[slot];
        self.big_entries[slot] =
            (!((3u64) << (sub_index * 2)) & entry_mask) | ((entry as u64) << (sub_index * 2));
    }

    /// Upstream: `SetEntry<false>(position, entry)`.
    fn set_entry_small(&mut self, position: u64, entry: EntryType) {
        let idx = (position >> self.page_bits) as usize;
        let slot = idx / ENTRIES_PER_U64;
        let sub_index = idx % ENTRIES_PER_U64;
        let entry_mask = self.entries[slot];
        self.entries[slot] =
            (!((3u64) << (sub_index * 2)) & entry_mask) | ((entry as u64) << (sub_index * 2));
    }

    // ── Page entry index ────────────────────────────────────────────────

    /// Upstream: `PageEntryIndex<true>(gpu_addr)`.
    fn page_entry_index_big(&self, gpu_addr: u64) -> usize {
        ((gpu_addr >> self.big_page_bits) & self.big_page_table_mask) as usize
    }

    /// Upstream: `PageEntryIndex<false>(gpu_addr)`.
    fn page_entry_index_small(&self, gpu_addr: u64) -> usize {
        ((gpu_addr >> self.page_bits) & self.page_table_mask) as usize
    }

    // ── Big page continuity ─────────────────────────────────────────────

    /// Upstream: `IsBigPageContinuous(big_page_index)`.
    fn is_big_page_continuous(&self, big_page_index: usize) -> bool {
        let entry_mask = self.big_page_continuous[big_page_index / CONTINUOUS_BITS];
        let sub_index = big_page_index % CONTINUOUS_BITS;
        ((entry_mask >> sub_index) & 0x1) != 0
    }

    /// Upstream: `SetBigPageContinuous(big_page_index, value)`.
    fn set_big_page_continuous(&mut self, big_page_index: usize, value: bool) {
        let slot = big_page_index / CONTINUOUS_BITS;
        let sub_index = big_page_index % CONTINUOUS_BITS;
        let continuous_mask = self.big_page_continuous[slot];
        self.big_page_continuous[slot] =
            (!(1u64 << sub_index) & continuous_mask) | (if value { 1u64 << sub_index } else { 0 });
    }

    // ── Page table operations ───────────────────────────────────────────

    /// Upstream: `PageTableOp<entry_type>(gpu_addr, dev_addr, size, kind)`.
    ///
    /// Operates on the small page table.
    fn page_table_op(
        &mut self,
        entry_type: EntryType,
        gpu_addr: u64,
        dev_addr: u64,
        size: u64,
        kind: PteKind,
    ) -> u64 {
        if entry_type == EntryType::Mapped {
            self.page_table.reserve_range(gpu_addr, size as usize);
        }
        let mut offset = 0u64;
        while offset < size {
            let current_gpu_addr = gpu_addr + offset;
            let _current_entry_type = self.get_entry_small(current_gpu_addr);
            self.set_entry_small(current_gpu_addr, entry_type);
            // Upstream calls rasterizer->ModifyGPUMemory here if entry changed.
            // Skipped until rasterizer is wired.
            if entry_type == EntryType::Mapped {
                let current_dev_addr = dev_addr + offset;
                let index = self.page_entry_index_small(current_gpu_addr);
                let sub_value = (current_dev_addr >> CPU_PAGE_BITS) as u32;
                self.page_table[index] = sub_value;
            }
            offset += self.page_size;
        }
        self.kind_map.map(gpu_addr, gpu_addr + size, kind);
        gpu_addr
    }

    /// Upstream: `BigPageTableOp<entry_type>(gpu_addr, dev_addr, size, kind)`.
    ///
    /// Operates on the big page table. The `check_contiguous` callback is used
    /// to determine if sub-pages within a big page are contiguous in host memory.
    /// When no host memory access is available, pass `None`.
    fn big_page_table_op(
        &mut self,
        entry_type: EntryType,
        gpu_addr: u64,
        dev_addr: u64,
        size: u64,
        kind: PteKind,
    ) -> u64 {
        let mut offset = 0u64;
        while offset < size {
            let current_gpu_addr = gpu_addr + offset;
            let _current_entry_type = self.get_entry_big(current_gpu_addr);
            self.set_entry_big(current_gpu_addr, entry_type);
            // Upstream calls rasterizer->ModifyGPUMemory here if entry changed.
            // Skipped until rasterizer is wired.
            if entry_type == EntryType::Mapped {
                let current_dev_addr = dev_addr + offset;
                let index = self.page_entry_index_big(current_gpu_addr);
                let sub_value = (current_dev_addr >> CPU_PAGE_BITS) as u32;
                self.big_page_table_dev[index] = sub_value;
                // Upstream checks continuity via memory.GetPointer for each sub-page.
                // Without direct host pointer access, assume continuous (conservative).
                // This is correct when the guest maps physically contiguous pages,
                // which is the common case for GPU buffers.
                self.set_big_page_continuous(index, true);
            }
            offset += self.big_page_size;
        }
        self.kind_map.map(gpu_addr, gpu_addr + size, kind);
        gpu_addr
    }

    // ── Public map/unmap API ────────────────────────────────────────────

    /// Upstream: `MemoryManager::Map(gpu_addr, dev_addr, size, kind, is_big_pages)`.
    pub fn map_ex(
        &mut self,
        gpu_addr: u64,
        dev_addr: u64,
        size: u64,
        kind: PteKind,
        is_big_pages: bool,
    ) -> u64 {
        log::trace!(
            "gpu_mm: map GPU {:#x}..{:#x} -> DEV {:#x} kind={:?} big={}",
            gpu_addr,
            gpu_addr + size,
            dev_addr,
            kind,
            is_big_pages
        );
        if is_big_pages {
            self.big_page_table_op(EntryType::Mapped, gpu_addr, dev_addr, size, kind)
        } else {
            self.page_table_op(EntryType::Mapped, gpu_addr, dev_addr, size, kind)
        }
    }

    /// Simplified map for callers that pass kind as u32.
    pub fn map(&mut self, gpu_addr: u64, dev_addr: u64, size: u64, kind_raw: u32) {
        let kind = pte_kind_from_u32(kind_raw);
        // Decide big vs small pages based on whether address is below split_address.
        let is_big = gpu_addr < self.split_address;
        self.map_ex(gpu_addr, dev_addr, size, kind, is_big);
    }

    /// Upstream: `MemoryManager::MapSparse(gpu_addr, size, is_big_pages)`.
    pub fn map_sparse_ex(&mut self, gpu_addr: u64, size: u64, is_big_pages: bool) -> u64 {
        log::trace!(
            "gpu_mm: reserve GPU {:#x}..{:#x} big={}",
            gpu_addr,
            gpu_addr + size,
            is_big_pages
        );
        if is_big_pages {
            self.big_page_table_op(EntryType::Reserved, gpu_addr, 0, size, PteKind::Invalid)
        } else {
            self.page_table_op(EntryType::Reserved, gpu_addr, 0, size, PteKind::Invalid)
        }
    }

    /// Simplified map_sparse for legacy callers.
    pub fn map_sparse(&mut self, gpu_addr: u64, size: u64, _kind_raw: u32) {
        let is_big = gpu_addr < self.split_address;
        self.map_sparse_ex(gpu_addr, size, is_big);
    }

    /// Upstream: `MemoryManager::Unmap(gpu_addr, size)`.
    pub fn unmap(&mut self, gpu_addr: u64, size: u64) {
        if size == 0 {
            return;
        }
        log::trace!("gpu_mm: unmap GPU {:#x}..{:#x}", gpu_addr, gpu_addr + size);
        // Upstream calls GetSubmappedRangeImpl<false> + rasterizer->UnmapMemory here.
        // Skipped until rasterizer is wired.
        self.big_page_table_op(EntryType::Free, gpu_addr, 0, size, PteKind::Invalid);
        self.page_table_op(EntryType::Free, gpu_addr, 0, size, PteKind::Invalid);
    }

    // ── Address translation ─────────────────────────────────────────────

    /// Upstream: `MemoryManager::GpuToCpuAddress(gpu_addr)`.
    ///
    /// Check big entries first (if below split_address), fall back to small entries.
    pub fn gpu_to_cpu_address(&self, gpu_addr: u64) -> Option<u64> {
        if gpu_addr >= self.address_space_size {
            return None;
        }
        if self.get_entry_big(gpu_addr) == EntryType::Mapped {
            let dev_addr_base =
                (self.big_page_table_dev[self.page_entry_index_big(gpu_addr)] as u64)
                    << CPU_PAGE_BITS;
            return Some(dev_addr_base + (gpu_addr & self.big_page_mask));
        }
        if self.get_entry_small(gpu_addr) == EntryType::Mapped {
            let dev_addr_base =
                (self.page_table[self.page_entry_index_small(gpu_addr)] as u64) << CPU_PAGE_BITS;
            return Some(dev_addr_base + (gpu_addr & self.page_mask));
        }
        None
    }

    /// Upstream: `MemoryManager::GpuToCpuAddress(addr, size)`.
    ///
    /// Search pages in the range for the first mapped address.
    pub fn gpu_to_cpu_address_range(&self, addr: u64, size: u64) -> Option<u64> {
        let mut page_index = addr >> self.page_bits;
        let page_last = (addr + size + self.page_size - 1) >> self.page_bits;
        while page_index < page_last {
            if let Some(page_addr) = self.gpu_to_cpu_address(page_index << self.page_bits) {
                return Some(page_addr);
            }
            page_index += 1;
        }
        None
    }

    /// Translate GPU VA — alias for gpu_to_cpu_address (backward compat).
    pub fn translate(&self, gpu_va: u64) -> Option<u64> {
        self.gpu_to_cpu_address(gpu_va)
    }

    // ── MemoryOperation (generic page walker) ───────────────────────────

    /// Walk big pages, calling the appropriate closure for each page's entry type.
    /// Returns true if a closure signaled early exit.
    ///
    /// Upstream: `MemoryOperation<true>(...)`.
    #[allow(dead_code)]
    fn memory_operation_big<FM, FR, FU>(
        &self,
        gpu_src_addr: u64,
        size: u64,
        mut func_mapped: FM,
        mut func_reserved: FR,
        mut func_unmapped: FU,
    ) -> bool
    where
        FM: FnMut(usize, usize, usize) -> bool,
        FR: FnMut(usize, usize, usize) -> bool,
        FU: FnMut(usize, usize, usize) -> bool,
    {
        let mut remaining = size as usize;
        let mut page_index = (gpu_src_addr >> self.big_page_bits) as usize;
        let mut page_offset = (gpu_src_addr & self.big_page_mask) as usize;
        let mut current_address = gpu_src_addr;

        while remaining > 0 {
            let copy_amount = std::cmp::min(self.big_page_size as usize - page_offset, remaining);
            let entry = self.get_entry_big(current_address);
            let should_break = match entry {
                EntryType::Mapped => func_mapped(page_index, page_offset, copy_amount),
                EntryType::Reserved => func_reserved(page_index, page_offset, copy_amount),
                EntryType::Free => func_unmapped(page_index, page_offset, copy_amount),
            };
            if should_break {
                return true;
            }
            page_index += 1;
            page_offset = 0;
            remaining -= copy_amount;
            current_address += copy_amount as u64;
        }
        false
    }

    /// Walk small pages, calling the appropriate closure for each page's entry type.
    ///
    /// Upstream: `MemoryOperation<false>(...)`.
    #[allow(dead_code)]
    fn memory_operation_small<FM, FR, FU>(
        &self,
        gpu_src_addr: u64,
        size: u64,
        mut func_mapped: FM,
        mut func_reserved: FR,
        mut func_unmapped: FU,
    ) -> bool
    where
        FM: FnMut(usize, usize, usize) -> bool,
        FR: FnMut(usize, usize, usize) -> bool,
        FU: FnMut(usize, usize, usize) -> bool,
    {
        let mut remaining = size as usize;
        let mut page_index = (gpu_src_addr >> self.page_bits) as usize;
        let mut page_offset = (gpu_src_addr & self.page_mask) as usize;
        let mut current_address = gpu_src_addr;

        while remaining > 0 {
            let copy_amount = std::cmp::min(self.page_size as usize - page_offset, remaining);
            let entry = self.get_entry_small(current_address);
            let should_break = match entry {
                EntryType::Mapped => func_mapped(page_index, page_offset, copy_amount),
                EntryType::Reserved => func_reserved(page_index, page_offset, copy_amount),
                EntryType::Free => func_unmapped(page_index, page_offset, copy_amount),
            };
            if should_break {
                return true;
            }
            page_index += 1;
            page_offset = 0;
            remaining -= copy_amount;
            current_address += copy_amount as u64;
        }
        false
    }

    // ── Read/Write block operations ─────────────────────────────────────

    /// Read bytes from GPU VA space using a CPU memory reader.
    ///
    /// Upstream: `MemoryManager::ReadBlockImpl<false>(...)` (unsafe variant).
    ///
    /// Walks big pages first. For unmapped big pages, falls back to small pages.
    pub fn read(
        &self,
        gpu_src_addr: u64,
        dst: &mut [u8],
        read_cpu_mem: &dyn Fn(u64, &mut [u8]),
    ) {
        self.read_block_impl(gpu_src_addr, dst, read_cpu_mem);
    }

    /// Internal read implementation with two-level page walk.
    fn read_block_impl(
        &self,
        gpu_src_addr: u64,
        dst: &mut [u8],
        read_cpu_mem: &dyn Fn(u64, &mut [u8]),
    ) {
        let size = dst.len();
        let mut dst_offset = 0usize;
        let mut remaining = size;
        let mut page_index = (gpu_src_addr >> self.big_page_bits) as usize;
        let mut page_offset = (gpu_src_addr & self.big_page_mask) as usize;
        let mut current_address = gpu_src_addr;

        while remaining > 0 {
            let copy_amount =
                std::cmp::min(self.big_page_size as usize - page_offset, remaining);
            let entry = self.get_entry_big(current_address);

            match entry {
                EntryType::Mapped => {
                    let dev_addr_base =
                        (self.big_page_table_dev[page_index] as u64) << CPU_PAGE_BITS;
                    let dev_addr = dev_addr_base + page_offset as u64;
                    // For big pages that are not continuous, read sub-pages individually.
                    // For continuous big pages, read the whole chunk.
                    // Since we use closure-based CPU access, just read directly.
                    read_cpu_mem(dev_addr, &mut dst[dst_offset..dst_offset + copy_amount]);
                    dst_offset += copy_amount;
                }
                EntryType::Reserved => {
                    // Reserved (sparse) — fill with zeros.
                    dst[dst_offset..dst_offset + copy_amount].fill(0);
                    dst_offset += copy_amount;
                }
                EntryType::Free => {
                    // Unmapped in big table — fall back to small pages.
                    let base = (page_index as u64) << self.big_page_bits | page_offset as u64;
                    self.read_small_pages(
                        base,
                        copy_amount,
                        &mut dst[dst_offset..dst_offset + copy_amount],
                        read_cpu_mem,
                    );
                    dst_offset += copy_amount;
                }
            }
            page_index += 1;
            page_offset = 0;
            remaining -= copy_amount;
            current_address += copy_amount as u64;
        }
    }

    /// Read using small page table entries.
    fn read_small_pages(
        &self,
        gpu_addr: u64,
        size: usize,
        dst: &mut [u8],
        read_cpu_mem: &dyn Fn(u64, &mut [u8]),
    ) {
        let mut dst_offset = 0usize;
        let mut remaining = size;
        let mut page_index = (gpu_addr >> self.page_bits) as usize;
        let mut page_offset = (gpu_addr & self.page_mask) as usize;
        let mut current_address = gpu_addr;

        while remaining > 0 {
            let copy_amount =
                std::cmp::min(self.page_size as usize - page_offset, remaining);
            let entry = self.get_entry_small(current_address);

            match entry {
                EntryType::Mapped => {
                    let dev_addr_base =
                        (self.page_table[page_index] as u64) << CPU_PAGE_BITS;
                    let dev_addr = dev_addr_base + page_offset as u64;
                    read_cpu_mem(dev_addr, &mut dst[dst_offset..dst_offset + copy_amount]);
                }
                _ => {
                    // Reserved or Free — fill with zeros.
                    if remaining > 0 {
                        static UNMAPPED_COUNT: AtomicUsize = AtomicUsize::new(0);
                        let c = UNMAPPED_COUNT.fetch_add(1, Ordering::Relaxed);
                        if c < 10 {
                            log::warn!(
                                "gpu_mm::read: unmapped GPU VA {:#x} (entry={:?})",
                                current_address,
                                entry
                            );
                        }
                    }
                    dst[dst_offset..dst_offset + copy_amount].fill(0);
                }
            }
            dst_offset += copy_amount;
            page_index += 1;
            page_offset = 0;
            remaining -= copy_amount;
            current_address += copy_amount as u64;
        }
    }

    /// Write bytes to GPU VA space using a CPU memory writer.
    ///
    /// Upstream: `MemoryManager::WriteBlockImpl<false>(...)`.
    pub fn write(
        &self,
        gpu_dest_addr: u64,
        src: &[u8],
        write_cpu_mem: &mut dyn FnMut(u64, &[u8]),
    ) {
        self.write_block_impl(gpu_dest_addr, src, write_cpu_mem);
    }

    fn write_block_impl(
        &self,
        gpu_dest_addr: u64,
        src: &[u8],
        write_cpu_mem: &mut dyn FnMut(u64, &[u8]),
    ) {
        let size = src.len();
        let mut src_offset = 0usize;
        let mut remaining = size;
        let mut page_index = (gpu_dest_addr >> self.big_page_bits) as usize;
        let mut page_offset = (gpu_dest_addr & self.big_page_mask) as usize;
        let mut current_address = gpu_dest_addr;

        while remaining > 0 {
            let copy_amount =
                std::cmp::min(self.big_page_size as usize - page_offset, remaining);
            let entry = self.get_entry_big(current_address);

            match entry {
                EntryType::Mapped => {
                    let dev_addr_base =
                        (self.big_page_table_dev[page_index] as u64) << CPU_PAGE_BITS;
                    let dev_addr = dev_addr_base + page_offset as u64;
                    write_cpu_mem(dev_addr, &src[src_offset..src_offset + copy_amount]);
                    src_offset += copy_amount;
                }
                EntryType::Reserved => {
                    // Reserved (sparse) — skip.
                    src_offset += copy_amount;
                }
                EntryType::Free => {
                    // Unmapped in big table — fall back to small pages.
                    let base = (page_index as u64) << self.big_page_bits | page_offset as u64;
                    self.write_small_pages(
                        base,
                        &src[src_offset..src_offset + copy_amount],
                        write_cpu_mem,
                    );
                    src_offset += copy_amount;
                }
            }
            page_index += 1;
            page_offset = 0;
            remaining -= copy_amount;
            current_address += copy_amount as u64;
        }
    }

    fn write_small_pages(
        &self,
        gpu_addr: u64,
        src: &[u8],
        write_cpu_mem: &mut dyn FnMut(u64, &[u8]),
    ) {
        let mut src_offset = 0usize;
        let mut remaining = src.len();
        let mut page_index = (gpu_addr >> self.page_bits) as usize;
        let mut page_offset = (gpu_addr & self.page_mask) as usize;
        let mut current_address = gpu_addr;

        while remaining > 0 {
            let copy_amount =
                std::cmp::min(self.page_size as usize - page_offset, remaining);
            let entry = self.get_entry_small(current_address);

            if entry == EntryType::Mapped {
                let dev_addr_base =
                    (self.page_table[page_index] as u64) << CPU_PAGE_BITS;
                let dev_addr = dev_addr_base + page_offset as u64;
                write_cpu_mem(dev_addr, &src[src_offset..src_offset + copy_amount]);
            }
            // For Reserved/Free, just skip (advance src).
            src_offset += copy_amount;
            page_index += 1;
            page_offset = 0;
            remaining -= copy_amount;
            current_address += copy_amount as u64;
        }
    }

    // ── Block read/write public API ─────────────────────────────────────

    /// Upstream: `MemoryManager::ReadBlock(gpu_src, output, size)`.
    pub fn read_block(
        &self,
        gpu_src: u64,
        output: &mut [u8],
        read_cpu: &dyn Fn(u64, &mut [u8]),
    ) {
        self.read(gpu_src, output, read_cpu);
    }

    /// Upstream: `MemoryManager::ReadBlockUnsafe(gpu_src, output, size)`.
    pub fn read_block_unsafe(
        &self,
        gpu_src: u64,
        output: &mut [u8],
        read_cpu: &dyn Fn(u64, &mut [u8]),
    ) {
        self.read(gpu_src, output, read_cpu);
    }

    /// Upstream: `MemoryManager::WriteBlock(gpu_dest, input, size)`.
    pub fn write_block(
        &self,
        gpu_dest: u64,
        input: &[u8],
        write_cpu: &mut dyn FnMut(u64, &[u8]),
    ) {
        self.write(gpu_dest, input, write_cpu);
    }

    /// Upstream: `MemoryManager::WriteBlockUnsafe(gpu_dest, input, size)`.
    pub fn write_block_unsafe(
        &self,
        gpu_dest: u64,
        input: &[u8],
        write_cpu: &mut dyn FnMut(u64, &[u8]),
    ) {
        self.write(gpu_dest, input, write_cpu);
    }

    /// Upstream: `MemoryManager::WriteBlockCached(gpu_dest, input, size)`.
    pub fn write_block_cached(
        &self,
        gpu_dest: u64,
        input: &[u8],
        write_cpu: &mut dyn FnMut(u64, &[u8]),
    ) {
        self.write(gpu_dest, input, write_cpu);
        // Upstream: accumulator->Add(gpu_dest_addr, size);
        // Accumulator not yet wired.
    }

    // ── Query methods ───────────────────────────────────────────────────

    /// Upstream: `MemoryManager::IsGranularRange(gpu_addr, size)`.
    ///
    /// Checks if a gpu region can be simply read with a pointer (fits in one page).
    pub fn is_granular_range(&self, gpu_addr: u64, size: u64) -> bool {
        if self.get_entry_big(gpu_addr) == EntryType::Mapped {
            let page_index = (gpu_addr >> self.big_page_bits) as usize;
            if self.is_big_page_continuous(page_index) {
                let page = (gpu_addr & self.big_page_mask) + size;
                return page <= self.big_page_size;
            }
            let page = (gpu_addr & DEVICE_PAGE_MASK) + size;
            return page <= DEVICE_PAGE_SIZE;
        }
        if self.get_entry_small(gpu_addr) != EntryType::Mapped {
            return false;
        }
        let page = (gpu_addr & DEVICE_PAGE_MASK) + size;
        page <= DEVICE_PAGE_SIZE
    }

    /// Upstream: `MemoryManager::IsContinuousRange(gpu_addr, size)`.
    pub fn is_continuous_range(&self, gpu_addr: u64, size: u64) -> bool {
        let mut old_page_addr: Option<u64> = None;
        let mut result = true;

        let big_page_bits = self.big_page_bits;
        let page_bits = self.page_bits;

        // We implement the two-level walk inline to avoid borrow issues.
        let mut remaining = size as usize;
        let mut big_page_index = (gpu_addr >> big_page_bits) as usize;
        let mut big_page_offset = (gpu_addr & self.big_page_mask) as usize;
        let mut current_address = gpu_addr;

        while remaining > 0 && result {
            let copy_amount =
                std::cmp::min(self.big_page_size as usize - big_page_offset, remaining);
            let entry = self.get_entry_big(current_address);

            match entry {
                EntryType::Mapped => {
                    let dev_addr_base =
                        (self.big_page_table_dev[big_page_index] as u64) << CPU_PAGE_BITS;
                    let dev_addr = dev_addr_base + big_page_offset as u64;
                    if let Some(expected) = old_page_addr {
                        if expected != dev_addr {
                            result = false;
                            break;
                        }
                    }
                    old_page_addr = Some(dev_addr + copy_amount as u64);
                }
                EntryType::Reserved => {
                    result = false;
                    break;
                }
                EntryType::Free => {
                    // Fall back to small pages.
                    let base =
                        (big_page_index as u64) << big_page_bits | big_page_offset as u64;
                    let mut sm_remaining = copy_amount;
                    let mut sm_page_index = (base >> page_bits) as usize;
                    let mut sm_page_offset = (base & self.page_mask) as usize;
                    let mut sm_current = base;

                    while sm_remaining > 0 && result {
                        let sm_copy =
                            std::cmp::min(self.page_size as usize - sm_page_offset, sm_remaining);
                        let sm_entry = self.get_entry_small(sm_current);
                        match sm_entry {
                            EntryType::Mapped => {
                                let dev_addr_base =
                                    (self.page_table[sm_page_index] as u64) << CPU_PAGE_BITS;
                                let dev_addr = dev_addr_base + sm_page_offset as u64;
                                if let Some(expected) = old_page_addr {
                                    if expected != dev_addr {
                                        result = false;
                                        break;
                                    }
                                }
                                old_page_addr = Some(dev_addr + sm_copy as u64);
                            }
                            _ => {
                                result = false;
                                break;
                            }
                        }
                        sm_page_index += 1;
                        sm_page_offset = 0;
                        sm_remaining -= sm_copy;
                        sm_current += sm_copy as u64;
                    }
                }
            }
            big_page_index += 1;
            big_page_offset = 0;
            remaining -= copy_amount;
            current_address += copy_amount as u64;
        }
        result
    }

    /// Upstream: `MemoryManager::IsFullyMappedRange(gpu_addr, size)`.
    pub fn is_fully_mapped_range(&self, gpu_addr: u64, size: u64) -> bool {
        let mut result = true;
        let big_page_bits = self.big_page_bits;

        let mut remaining = size as usize;
        let mut big_page_index = (gpu_addr >> big_page_bits) as usize;
        let mut big_page_offset = (gpu_addr & self.big_page_mask) as usize;
        let mut current_address = gpu_addr;

        while remaining > 0 && result {
            let copy_amount =
                std::cmp::min(self.big_page_size as usize - big_page_offset, remaining);
            let entry = self.get_entry_big(current_address);

            match entry {
                EntryType::Mapped => { /* pass */ }
                EntryType::Reserved => {
                    result = false;
                    break;
                }
                EntryType::Free => {
                    // Check small pages.
                    let base =
                        (big_page_index as u64) << big_page_bits | big_page_offset as u64;
                    let mut sm_remaining = copy_amount;
                    let mut sm_page_offset = (base & self.page_mask) as usize;
                    let mut sm_current = base;

                    while sm_remaining > 0 && result {
                        let sm_copy =
                            std::cmp::min(self.page_size as usize - sm_page_offset, sm_remaining);
                        let sm_entry = self.get_entry_small(sm_current);
                        match sm_entry {
                            EntryType::Mapped | EntryType::Reserved => { /* pass */ }
                            EntryType::Free => {
                                result = false;
                                break;
                            }
                        }
                        sm_page_offset = 0;
                        sm_remaining -= sm_copy;
                        sm_current += sm_copy as u64;
                    }
                }
            }
            big_page_index += 1;
            big_page_offset = 0;
            remaining -= copy_amount;
            current_address += copy_amount as u64;
        }
        result
    }

    /// Upstream: `MemoryManager::MaxContinuousRange(gpu_addr, size)`.
    pub fn max_continuous_range(&self, gpu_addr: u64, size: u64) -> u64 {
        let mut old_page_addr: Option<u64> = None;
        let mut range_so_far = 0u64;
        let mut done = false;

        let big_page_bits = self.big_page_bits;
        let page_bits = self.page_bits;

        let mut remaining = size as usize;
        let mut big_page_index = (gpu_addr >> big_page_bits) as usize;
        let mut big_page_offset = (gpu_addr & self.big_page_mask) as usize;
        let mut current_address = gpu_addr;

        while remaining > 0 && !done {
            let copy_amount =
                std::cmp::min(self.big_page_size as usize - big_page_offset, remaining);
            let entry = self.get_entry_big(current_address);

            match entry {
                EntryType::Mapped => {
                    let dev_addr_base =
                        (self.big_page_table_dev[big_page_index] as u64) << CPU_PAGE_BITS;
                    let dev_addr = dev_addr_base + big_page_offset as u64;
                    if let Some(expected) = old_page_addr {
                        if expected != dev_addr {
                            break;
                        }
                    }
                    range_so_far += copy_amount as u64;
                    old_page_addr = Some(dev_addr + copy_amount as u64);
                }
                EntryType::Reserved => {
                    break;
                }
                EntryType::Free => {
                    // Fall back to small pages.
                    let base =
                        (big_page_index as u64) << big_page_bits | big_page_offset as u64;
                    let mut sm_remaining = copy_amount;
                    let mut sm_page_index = (base >> page_bits) as usize;
                    let mut sm_page_offset = (base & self.page_mask) as usize;
                    let mut sm_current = base;

                    while sm_remaining > 0 && !done {
                        let sm_copy =
                            std::cmp::min(self.page_size as usize - sm_page_offset, sm_remaining);
                        let sm_entry = self.get_entry_small(sm_current);
                        match sm_entry {
                            EntryType::Mapped => {
                                let dev_addr_base =
                                    (self.page_table[sm_page_index] as u64) << CPU_PAGE_BITS;
                                let dev_addr = dev_addr_base + sm_page_offset as u64;
                                if let Some(expected) = old_page_addr {
                                    if expected != dev_addr {
                                        done = true;
                                        break;
                                    }
                                }
                                range_so_far += sm_copy as u64;
                                old_page_addr = Some(dev_addr + sm_copy as u64);
                            }
                            _ => {
                                done = true;
                                break;
                            }
                        }
                        sm_page_index += 1;
                        sm_page_offset = 0;
                        sm_remaining -= sm_copy;
                        sm_current += sm_copy as u64;
                    }
                }
            }
            big_page_index += 1;
            big_page_offset = 0;
            remaining -= copy_amount;
            current_address += copy_amount as u64;
        }
        range_so_far
    }

    /// Upstream: `MemoryManager::GetSubmappedRange(gpu_addr, size)`.
    ///
    /// Returns GPU address ranges (not device addresses).
    pub fn get_submapped_range(&self, gpu_addr: u64, size: u64) -> Vec<(u64, u64)> {
        let mut result = Vec::new();
        let mut last_segment: Option<(u64, u64)> = None;
        let mut old_page_addr: Option<u64> = None;

        let big_page_bits = self.big_page_bits;
        let page_bits = self.page_bits;

        let split = |last_segment: &mut Option<(u64, u64)>, result: &mut Vec<(u64, u64)>| {
            if let Some(seg) = last_segment.take() {
                result.push(seg);
            }
        };

        let mut remaining = size as usize;
        let mut big_page_index = (gpu_addr >> big_page_bits) as usize;
        let mut big_page_offset = (gpu_addr & self.big_page_mask) as usize;
        let mut current_address = gpu_addr;

        while remaining > 0 {
            let copy_amount =
                std::cmp::min(self.big_page_size as usize - big_page_offset, remaining);
            let entry = self.get_entry_big(current_address);

            match entry {
                EntryType::Mapped => {
                    let dev_addr_base =
                        (self.big_page_table_dev[big_page_index] as u64) << CPU_PAGE_BITS;
                    let dev_addr = dev_addr_base + big_page_offset as u64;
                    if let Some(expected) = old_page_addr {
                        if expected != dev_addr {
                            split(&mut last_segment, &mut result);
                        }
                    }
                    old_page_addr = Some(dev_addr + copy_amount as u64);
                    let new_base_addr =
                        ((big_page_index as u64) << big_page_bits) + big_page_offset as u64;
                    if let Some(seg) = &mut last_segment {
                        seg.1 += copy_amount as u64;
                    } else {
                        last_segment = Some((new_base_addr, copy_amount as u64));
                    }
                }
                EntryType::Reserved => {
                    split(&mut last_segment, &mut result);
                    old_page_addr = None;
                }
                EntryType::Free => {
                    // Walk small pages.
                    let base =
                        (big_page_index as u64) << big_page_bits | big_page_offset as u64;
                    let mut sm_remaining = copy_amount;
                    let mut sm_page_index = (base >> page_bits) as usize;
                    let mut sm_page_offset = (base & self.page_mask) as usize;
                    let mut sm_current = base;

                    while sm_remaining > 0 {
                        let sm_copy =
                            std::cmp::min(self.page_size as usize - sm_page_offset, sm_remaining);
                        let sm_entry = self.get_entry_small(sm_current);
                        match sm_entry {
                            EntryType::Mapped => {
                                let dev_addr_base =
                                    (self.page_table[sm_page_index] as u64) << CPU_PAGE_BITS;
                                let dev_addr = dev_addr_base + sm_page_offset as u64;
                                if let Some(expected) = old_page_addr {
                                    if expected != dev_addr {
                                        split(&mut last_segment, &mut result);
                                    }
                                }
                                old_page_addr = Some(dev_addr + sm_copy as u64);
                                let new_base_addr =
                                    ((sm_page_index as u64) << page_bits) + sm_page_offset as u64;
                                if let Some(seg) = &mut last_segment {
                                    seg.1 += sm_copy as u64;
                                } else {
                                    last_segment = Some((new_base_addr, sm_copy as u64));
                                }
                            }
                            _ => {
                                split(&mut last_segment, &mut result);
                                old_page_addr = None;
                            }
                        }
                        sm_page_index += 1;
                        sm_page_offset = 0;
                        sm_remaining -= sm_copy;
                        sm_current += sm_copy as u64;
                    }
                }
            }
            big_page_index += 1;
            big_page_offset = 0;
            remaining -= copy_amount;
            current_address += copy_amount as u64;
        }
        split(&mut last_segment, &mut result);
        result
    }

    /// Upstream: `MemoryManager::CopyBlock(gpu_dest, gpu_src, size)`.
    pub fn copy_block(
        &self,
        gpu_dest: u64,
        gpu_src: u64,
        size: u64,
        read_cpu: &dyn Fn(u64, &mut [u8]),
        write_cpu: &mut dyn FnMut(u64, &[u8]),
    ) {
        let mut tmp = vec![0u8; size as usize];
        self.read(gpu_src, &mut tmp, read_cpu);
        self.write(gpu_dest, &tmp, write_cpu);
    }

    /// Upstream: `MemoryManager::GetPageKind(gpu_addr)`.
    pub fn get_page_kind(&self, gpu_addr: u64) -> PteKind {
        self.kind_map.get_value_at(gpu_addr)
    }

    /// Upstream: `MemoryManager::GetMemoryLayoutSize(gpu_addr, max_size)`.
    pub fn get_memory_layout_size(&self, gpu_addr: u64, _max_size: u64) -> u64 {
        // Upstream: kind_map.GetContinuousSizeFrom(gpu_addr)
        self.kind_map.get_continuous_size_from(gpu_addr) as u64
    }

    // ── Cache/rasterizer stubs ──────────────────────────────────────────

    /// Upstream: `MemoryManager::FlushRegion(gpu_addr, size)`.
    /// Requires rasterizer binding — no-op until bound.
    pub fn flush_region(&self, _gpu_addr: u64, _size: u64) {
        // Upstream: walks pages and calls rasterizer->FlushRegion per mapped page.
    }

    /// Upstream: `MemoryManager::InvalidateRegion(gpu_addr, size)`.
    pub fn invalidate_region(&self, _gpu_addr: u64, _size: u64) {
        // Upstream: walks pages and calls rasterizer->InvalidateRegion per mapped page.
    }

    /// Upstream: `MemoryManager::FlushCaching()`.
    pub fn flush_caching(&self) {
        // Upstream: drains accumulator + calls rasterizer->InnerInvalidation.
    }

    /// Check if a GPU address is within the valid address range.
    ///
    /// Upstream: `MemoryManager::IsWithinGPUAddressRange(gpu_addr)`.
    pub fn is_within_gpu_address_range(&self, gpu_addr: u64) -> bool {
        gpu_addr < self.address_space_size
    }

    /// Upstream: `MemoryManager::IsMemoryDirty(gpu_addr, size)`.
    pub fn is_memory_dirty(&self, _gpu_addr: u64, _size: u64) -> bool {
        // Requires rasterizer binding.
        false
    }

    /// Read a value of type `T` from GPU virtual address space.
    ///
    /// Upstream: `template <typename T> T MemoryManager::Read(GPUVAddr addr) const`
    pub fn read_value<T: Copy>(
        &self,
        gpu_addr: u64,
        read_cpu_mem: &dyn Fn(u64, &mut [u8]),
    ) -> Option<T> {
        let size = std::mem::size_of::<T>();
        if self.gpu_to_cpu_address(gpu_addr).is_none() {
            return None;
        }
        let mut bytes = vec![0u8; size];
        self.read(gpu_addr, &mut bytes, read_cpu_mem);
        // Safety: T is Copy and we have exactly size_of::<T>() bytes.
        let value = unsafe { std::ptr::read(bytes.as_ptr() as *const T) };
        Some(value)
    }

    /// Write a value of type `T` to GPU virtual address space.
    ///
    /// Upstream: `template <typename T> void MemoryManager::Write(GPUVAddr addr, T data)`
    pub fn write_value<T: Copy>(
        &self,
        gpu_addr: u64,
        data: T,
        write_cpu_mem: &mut dyn FnMut(u64, &[u8]),
    ) {
        let size = std::mem::size_of::<T>();
        let bytes =
            unsafe { std::slice::from_raw_parts(&data as *const T as *const u8, size) };
        self.write(gpu_addr, bytes, write_cpu_mem);
    }

    // ── Legacy helpers (backward compat) ────────────────────────────────

    /// Map at a specific GPU VA (alias for `map`).
    pub fn alloc_fixed(&mut self, gpu_va: u64, cpu_addr: u64, size: u64) {
        self.map(gpu_va, cpu_addr, size, 0xFF);
    }

    /// Allocate GPU VA from a simple bump allocator and map to a CPU address.
    /// NOTE: This is not an upstream method. Kept for backward compatibility with
    /// code that used the old allocator. Upstream manages allocation at a higher level.
    pub fn alloc_any(&mut self, cpu_addr: u64, size: u64) -> u64 {
        // Start allocations at 64 MB to avoid the zero page region.
        static NEXT_ALLOC: AtomicUsize = AtomicUsize::new(0x0400_0000);
        let page_size = self.page_size;
        let aligned_size = (size + page_size - 1) & !(page_size - 1);
        let gpu_va = NEXT_ALLOC.fetch_add(aligned_size as usize, Ordering::Relaxed) as u64;
        self.map(gpu_va, cpu_addr, aligned_size, 0xFF);
        gpu_va
    }
}

impl Default for GpuMemoryManager {
    fn default() -> Self {
        Self::new()
    }
}

// ── Helper ──────────────────────────────────────────────────────────────

fn pte_kind_from_u32(raw: u32) -> PteKind {
    // PteKind is repr(u8), so truncate and try to match.
    // The upstream passes PTEKind::INVALID (0xFF) as default.
    // For unknown values, use Invalid.
    if raw == 0xFF || raw > 0xFF {
        return PteKind::Invalid;
    }
    // Safety: we validate the range. Unknown values within 0..0xFF that
    // don't correspond to a variant are treated as Invalid for safety.
    // The enum has many variants; try direct transmute for known ones.
    // For robustness, just store the raw value in the kind_map.
    // Since we can't easily validate all enum variants, use unsafe transmute
    // with a fallback.
    //
    // However, the kind_map stores PteKind so we need a valid variant.
    // The simplest correct approach: if the value matches a known variant, use it.
    // Otherwise use Invalid. In practice, games use a small subset of kinds.
    //
    // For now, use a brute force approach via the Pitch variant (0x00) as base.
    if raw == 0 {
        return PteKind::Pitch;
    }
    // For all other values, try unsafe transmute. PteKind is repr(u8) and
    // the enum covers many GPU PTE kinds. Values not covered by the enum
    // are still valid GPU kinds but not listed — treat as Invalid.
    // This matches upstream which just stores the raw value.
    //
    // Actually, upstream stores PTEKind as an enum class with the full range.
    // Our PteKind enum may not cover all values. For safety, just use Invalid
    // for unlisted values. The kind is only used for GetPageKind queries.
    PteKind::Invalid
}

// ── MemoryManager (outer wrapper) ───────────────────────────────────────

/// Port owner for `Tegra::MemoryManager`.
///
/// Wraps `GpuMemoryManager` with the outer API expected by channel_state, gpu.rs,
/// nvdrv, etc.
pub struct MemoryManager {
    id: usize,
    inner: GpuMemoryManager,
    rasterizer_bound: bool,
}

impl MemoryManager {
    /// Upstream: `MemoryManager(system, address_space_bits=40, split=1<<34, big_page_bits=16,
    ///            page_bits=12)`.
    pub fn new(id: usize) -> Self {
        Self::new_with_geometry(id, 40, 1u64 << 34, 16, 12)
    }

    pub fn new_with_geometry(
        id: usize,
        address_space_bits: u64,
        split_address: u64,
        big_page_bits: u64,
        page_bits: u64,
    ) -> Self {
        Self {
            id,
            inner: GpuMemoryManager::with_params(
                address_space_bits,
                split_address,
                big_page_bits,
                page_bits,
            ),
            rasterizer_bound: false,
        }
    }

    /// Upstream: `MemoryManager::GetID()`.
    pub fn get_id(&self) -> usize {
        self.id
    }

    pub fn gpu_to_cpu_address(&self, gpu_addr: u64) -> Option<u64> {
        self.inner.gpu_to_cpu_address(gpu_addr)
    }

    pub fn is_continuous_range(&self, gpu_addr: u64, size: u64) -> bool {
        self.inner.is_continuous_range(gpu_addr, size)
    }

    pub fn is_fully_mapped_range(&self, gpu_addr: u64, size: u64) -> bool {
        self.inner.is_fully_mapped_range(gpu_addr, size)
    }

    pub fn is_granular_range(&self, gpu_addr: u64, size: u64) -> bool {
        self.inner.is_granular_range(gpu_addr, size)
    }

    pub fn read_block(
        &self,
        gpu_src: u64,
        output: &mut [u8],
        read_cpu: &dyn Fn(u64, &mut [u8]),
    ) {
        self.inner.read_block(gpu_src, output, read_cpu);
    }

    pub fn read_block_unsafe(
        &self,
        gpu_src: u64,
        output: &mut [u8],
        read_cpu: &dyn Fn(u64, &mut [u8]),
    ) {
        self.inner.read_block_unsafe(gpu_src, output, read_cpu);
    }

    pub fn write_block(
        &self,
        gpu_dest: u64,
        input: &[u8],
        write_cpu: &mut dyn FnMut(u64, &[u8]),
    ) {
        self.inner.write_block(gpu_dest, input, write_cpu);
    }

    pub fn write_block_unsafe(
        &self,
        gpu_dest: u64,
        input: &[u8],
        write_cpu: &mut dyn FnMut(u64, &[u8]),
    ) {
        self.inner.write_block_unsafe(gpu_dest, input, write_cpu);
    }

    pub fn flush_region(&self, gpu_addr: u64, size: u64) {
        self.inner.flush_region(gpu_addr, size);
    }

    pub fn invalidate_region(&self, gpu_addr: u64, size: u64) {
        self.inner.invalidate_region(gpu_addr, size);
    }

    pub fn is_within_gpu_address_range(&self, gpu_addr: u64) -> bool {
        self.inner.is_within_gpu_address_range(gpu_addr)
    }

    pub fn max_continuous_range(&self, gpu_addr: u64, size: u64) -> u64 {
        self.inner.max_continuous_range(gpu_addr, size)
    }

    pub fn get_memory_layout_size(&self, gpu_addr: u64) -> u64 {
        self.inner.get_memory_layout_size(gpu_addr, u64::MAX)
    }

    pub fn get_memory_layout_size_bounded(&self, gpu_addr: u64, max_size: u64) -> u64 {
        self.inner.get_memory_layout_size(gpu_addr, max_size)
    }

    pub fn get_page_kind_raw(&self, gpu_addr: u64) -> u32 {
        self.inner.get_page_kind(gpu_addr) as u32
    }

    pub fn get_submapped_range(&self, gpu_addr: u64, size: u64) -> Vec<(u64, u64)> {
        self.inner.get_submapped_range(gpu_addr, size)
    }

    /// Upstream: `MemoryManager::Map(gpu_addr, dev_addr, size, kind, is_big_pages)`.
    pub fn map(
        &mut self,
        gpu_addr: u64,
        device_addr: u64,
        size: u64,
        kind: u32,
        is_big_pages: bool,
    ) -> u64 {
        let pte_kind = pte_kind_from_u32(kind);
        self.inner
            .map_ex(gpu_addr, device_addr, size, pte_kind, is_big_pages)
    }

    /// Upstream: `MemoryManager::MapSparse(gpu_addr, size, is_big_pages)`.
    pub fn map_sparse(&mut self, gpu_addr: u64, size: u64, is_big_pages: bool) -> u64 {
        self.inner.map_sparse_ex(gpu_addr, size, is_big_pages)
    }

    /// Upstream: `MemoryManager::Unmap(gpu_addr, size)`.
    pub fn unmap(&mut self, gpu_addr: u64, size: u64) {
        self.inner.unmap(gpu_addr, size);
    }

    pub fn address_space_bits(&self) -> u64 {
        self.inner.address_space_bits
    }

    pub fn split_address(&self) -> u64 {
        self.inner.split_address
    }

    pub fn big_page_bits(&self) -> u64 {
        self.inner.big_page_bits
    }

    pub fn page_bits(&self) -> u64 {
        self.inner.page_bits
    }

    /// Upstream: `MemoryManager::BindRasterizer(rasterizer)`.
    pub fn bind_rasterizer(&mut self) {
        self.rasterizer_bound = true;
    }

    pub fn has_bound_rasterizer(&self) -> bool {
        self.rasterizer_bound
    }
}

impl Default for MemoryManager {
    fn default() -> Self {
        Self::new(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map_and_translate_big_pages() {
        let mut mm = GpuMemoryManager::new();
        // Map using big pages (address < split_address = 1<<34)
        mm.map_ex(0x10000, 0xDEAD_0000, 0x10000, PteKind::Invalid, true);

        assert_eq!(mm.translate(0x10000), Some(0xDEAD_0000));
        assert_eq!(mm.translate(0x10500), Some(0xDEAD_0500));
        assert_eq!(mm.translate(0x1FFFF), Some(0xDEAD_FFFF));
    }

    #[test]
    fn test_map_and_translate_small_pages() {
        let mut mm = GpuMemoryManager::new();
        // Map using small pages
        mm.map_ex(0x1000, 0xBEEF_0000, 0x2000, PteKind::Invalid, false);

        assert_eq!(mm.translate(0x1000), Some(0xBEEF_0000));
        assert_eq!(mm.translate(0x1500), Some(0xBEEF_0500));
        assert_eq!(mm.translate(0x2000), Some(0xBEEF_1000));
        assert_eq!(mm.translate(0x2FFF), Some(0xBEEF_1FFF));
    }

    #[test]
    fn test_unmapped_returns_none() {
        let mm = GpuMemoryManager::new();
        assert_eq!(mm.translate(0x1000), None);
        assert_eq!(mm.translate(0), None);
    }

    #[test]
    fn test_unmap() {
        let mut mm = GpuMemoryManager::new();
        mm.map_ex(0x10000, 0xBEEF_0000, 0x10000, PteKind::Invalid, true);
        assert_eq!(mm.translate(0x10000), Some(0xBEEF_0000));

        mm.unmap(0x10000, 0x10000);
        assert_eq!(mm.translate(0x10000), None);
    }

    #[test]
    fn test_dual_table_fallback() {
        let mut mm = GpuMemoryManager::new();
        // Map small pages at an address below split (big table will be Free).
        mm.map_ex(0x1000, 0xAAAA_0000, 0x1000, PteKind::Invalid, false);

        // Big entry is Free, so gpu_to_cpu_address falls back to small table.
        assert_eq!(mm.translate(0x1000), Some(0xAAAA_0000));
    }

    #[test]
    fn test_read_via_callback() {
        let mut mm = GpuMemoryManager::new();
        mm.map_ex(0x10000, 0x8000_0000, 0x10000, PteKind::Invalid, true);

        let mut buf = [0u8; 4];
        mm.read(0x10000, &mut buf, &|addr, dst| {
            let bytes = (addr as u32).to_le_bytes();
            let len = dst.len().min(bytes.len());
            dst[..len].copy_from_slice(&bytes[..len]);
        });

        let val = u32::from_le_bytes(buf);
        assert_eq!(val, 0x8000_0000);
    }

    #[test]
    fn test_write_via_callback() {
        let mut mm = GpuMemoryManager::new();
        mm.map_ex(0x10000, 0x8000_0000, 0x10000, PteKind::Invalid, true);

        let mut written: Vec<(u64, Vec<u8>)> = Vec::new();
        let data = [0xDE, 0xAD, 0xBE, 0xEF];
        mm.write(0x10000, &data, &mut |addr, src| {
            written.push((addr, src.to_vec()));
        });

        assert_eq!(written.len(), 1);
        assert_eq!(written[0].0, 0x8000_0000);
        assert_eq!(written[0].1, vec![0xDE, 0xAD, 0xBE, 0xEF]);
    }

    #[test]
    fn test_is_continuous_range() {
        let mut mm = GpuMemoryManager::new();
        // Two contiguous big pages.
        mm.map_ex(0x10000, 0x8000_0000, 0x20000, PteKind::Invalid, true);
        assert!(mm.is_continuous_range(0x10000, 0x20000));

        // Non-contiguous: two separate mappings.
        let mut mm2 = GpuMemoryManager::new();
        mm2.map_ex(0x10000, 0x8000_0000, 0x10000, PteKind::Invalid, true);
        mm2.map_ex(0x20000, 0x9000_0000, 0x10000, PteKind::Invalid, true);
        assert!(!mm2.is_continuous_range(0x10000, 0x20000));
    }

    #[test]
    fn test_max_continuous_range() {
        let mut mm = GpuMemoryManager::new();
        mm.map_ex(0x10000, 0x8000_0000, 0x10000, PteKind::Invalid, true);
        mm.map_ex(0x20000, 0x9000_0000, 0x10000, PteKind::Invalid, true);

        assert_eq!(mm.max_continuous_range(0x10000, 0x20000), 0x10000);
    }

    #[test]
    fn test_memory_manager_wrapper() {
        let mut mm = MemoryManager::new_with_geometry(5, 40, 1u64 << 34, 16, 12);

        assert_eq!(mm.get_id(), 5);
        assert_eq!(mm.address_space_bits(), 40);
        assert_eq!(mm.split_address(), 1u64 << 34);
        assert_eq!(mm.big_page_bits(), 16);
        assert_eq!(mm.page_bits(), 12);

        // Map with big pages (is_big_pages=true).
        assert_eq!(mm.map(0x10000, 0x9000_0000, 0x10000, 0xFF, true), 0x10000);
        assert_eq!(mm.gpu_to_cpu_address(0x10000), Some(0x9000_0000));
        assert_eq!(mm.gpu_to_cpu_address(0x10ABC), Some(0x9000_0ABC));

        mm.unmap(0x10000, 0x10000);
        assert_eq!(mm.gpu_to_cpu_address(0x10000), None);
    }

    #[test]
    fn test_get_submapped_range() {
        let mut mm = GpuMemoryManager::new();
        // Map two contiguous big pages, then a gap, then one more.
        mm.map_ex(0x10000, 0x8000_0000, 0x20000, PteKind::Invalid, true);
        // Gap at 0x30000
        mm.map_ex(0x40000, 0x9000_0000, 0x10000, PteKind::Invalid, true);

        let ranges = mm.get_submapped_range(0x10000, 0x40000);
        assert_eq!(ranges.len(), 2);
        assert_eq!(ranges[0], (0x10000, 0x20000));
        assert_eq!(ranges[1], (0x40000, 0x10000));
    }
}
