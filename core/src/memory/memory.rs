//! Port of zuyu/src/core/memory.h / memory.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-17
//!
//! Core::Memory::Memory — bridges KPageTableBase with the dynarmic page table
//! and the DeviceMemory backing store.

use std::sync::Arc;

use common::host_memory::HostMemory;
use common::page_table::{PageInfo, PageTable, PageType};

use crate::device_memory::{dram_memory_map, DeviceMemory};

/// Page size constants matching upstream YUZU_PAGEBITS / YUZU_PAGESIZE.
const PAGE_BITS: usize = 12;
const PAGE_SIZE: u64 = 1 << PAGE_BITS;
const PAGE_MASK: u64 = PAGE_SIZE - 1;

/// Memory permission for mapping operations.
/// Matches upstream Common::MemoryPermission.
pub use common::host_memory::MemoryPermission;

/// Port of Core::Memory::Memory.
///
/// Manages the mapping between guest virtual addresses, physical addresses
/// (in DeviceMemory), and host pointers (in the PageTable used by dynarmic).
pub struct Memory {
    /// Pointer to the device memory backing store.
    device_memory: *const DeviceMemory,
    /// Pointer to the HostMemory buffer (for Map/Unmap/Protect).
    buffer: *const HostMemory,
    /// Current page table (set by SetCurrentPageTable when switching processes).
    current_page_table: *mut PageTable,
}

// SAFETY: Memory is used behind Arc<Mutex<>> and all raw pointers are
// to long-lived objects (DeviceMemory, HostMemory, PageTable) that outlive Memory.
unsafe impl Send for Memory {}
unsafe impl Sync for Memory {}

impl Memory {
    /// Create a new Memory instance.
    ///
    /// # Safety
    /// The caller must ensure that `device_memory` and `buffer` outlive this Memory.
    pub unsafe fn new(device_memory: *const DeviceMemory, buffer: *const HostMemory) -> Self {
        Self {
            device_memory,
            buffer,
            current_page_table: std::ptr::null_mut(),
        }
    }

    /// Set the current page table (called when switching processes).
    /// Matches upstream `Memory::SetCurrentPageTable`.
    pub fn set_current_page_table(&mut self, page_table: *mut PageTable) {
        self.current_page_table = page_table;
    }

    /// Map a physical memory region into the guest virtual address space.
    ///
    /// Matches upstream `Memory::Impl::MapMemoryRegion`:
    /// - Updates PageTable entries (pointers, backing_addr, blocks) per page
    /// - Maps into fastmem arena if available
    ///
    /// # Arguments
    /// * `page_table` - The page table to update
    /// * `base` - Guest virtual address (page-aligned)
    /// * `size` - Size in bytes (page-aligned)
    /// * `target` - Physical address (≥ DramMemoryMap::Base)
    /// * `perms` - Memory permissions
    /// * `separate_heap` - Whether this is a separate heap mapping
    pub fn map_memory_region(
        &self,
        page_table: &mut PageTable,
        base: u64,
        size: u64,
        target: u64,
        perms: MemoryPermission,
        separate_heap: bool,
    ) {
        debug_assert!(
            (size & PAGE_MASK) == 0,
            "non-page aligned size: {:#x}",
            size
        );
        debug_assert!(
            (base & PAGE_MASK) == 0,
            "non-page aligned base: {:#x}",
            base
        );
        debug_assert!(
            target >= dram_memory_map::BASE,
            "Out of bounds target: {:#x}",
            target
        );

        self.map_pages(
            page_table,
            base / PAGE_SIZE,
            size / PAGE_SIZE,
            target,
            PageType::Memory,
        );

        if !page_table.fastmem_arena.is_null() {
            unsafe {
                (*self.buffer).map(
                    base as usize,
                    (target - dram_memory_map::BASE) as usize,
                    size as usize,
                    perms,
                    separate_heap,
                );
            }
        }
    }

    /// Unmap a region of the guest virtual address space.
    ///
    /// Matches upstream `Memory::Impl::UnmapRegion`.
    pub fn unmap_region(
        &self,
        page_table: &mut PageTable,
        base: u64,
        size: u64,
        separate_heap: bool,
    ) {
        debug_assert!((size & PAGE_MASK) == 0);
        debug_assert!((base & PAGE_MASK) == 0);

        self.map_pages(page_table, base / PAGE_SIZE, size / PAGE_SIZE, 0, PageType::Unmapped);

        if !page_table.fastmem_arena.is_null() {
            unsafe {
                (*self.buffer).unmap(base as usize, size as usize, separate_heap);
            }
        }
    }

    /// Change protection on a region of the guest virtual address space.
    ///
    /// Matches upstream `Memory::Impl::ProtectRegion`.
    pub fn protect_region(
        &self,
        page_table: &mut PageTable,
        vaddr: u64,
        size: u64,
        perms: MemoryPermission,
    ) {
        debug_assert!((size & PAGE_MASK) == 0);
        debug_assert!((vaddr & PAGE_MASK) == 0);

        if page_table.fastmem_arena.is_null() {
            return;
        }

        let mut protect_bytes: u64 = 0;
        let mut protect_begin: u64 = 0;

        let mut addr = vaddr;
        while addr < vaddr + size {
            let page_idx = (addr >> PAGE_BITS) as usize;
            let page_type = if page_idx < page_table.pointers.size() {
                page_table.pointers[page_idx].page_type()
            } else {
                PageType::Unmapped
            };

            match page_type {
                PageType::RasterizerCachedMemory => {
                    if protect_bytes > 0 {
                        unsafe {
                            (*self.buffer).protect(
                                protect_begin as usize,
                                protect_bytes as usize,
                                perms,
                            );
                        }
                        protect_bytes = 0;
                    }
                }
                _ => {
                    if protect_bytes == 0 {
                        protect_begin = addr;
                    }
                    protect_bytes += PAGE_SIZE;
                }
            }

            addr += PAGE_SIZE;
        }

        if protect_bytes > 0 {
            unsafe {
                (*self.buffer).protect(
                    protect_begin as usize,
                    protect_bytes as usize,
                    perms,
                );
            }
        }
    }

    /// Internal: update page table entries for a range of pages.
    ///
    /// Matches upstream `Memory::Impl::MapPages`.
    fn map_pages(
        &self,
        page_table: &mut PageTable,
        base_page: u64,
        num_pages: u64,
        mut target: u64,
        page_type: PageType,
    ) {
        let end = base_page + num_pages;
        debug_assert!(
            (end as usize) <= page_table.pointers.size(),
            "out of range mapping at {:#x}",
            base_page * PAGE_SIZE
        );

        if target == 0 {
            debug_assert!(
                page_type != PageType::Memory,
                "Mapping memory page without a pointer @ {:#x}",
                base_page * PAGE_SIZE
            );

            let mut page = base_page as usize;
            while page < end as usize {
                page_table.pointers[page].store(0usize, page_type);
                page_table.backing_addr[page] = 0u64;
                page_table.blocks[page] = 0u64;
                page += 1;
            }
        } else {
            let orig_base = base_page;
            let mut page = base_page as usize;
            while page < end as usize {
                // Compute host pointer: DeviceMemory base + physical offset - virtual page offset
                let host_ptr = unsafe {
                    let dm = &*self.device_memory;
                    dm.buffer.backing_base_pointer() as usize
                        + (target - dram_memory_map::BASE) as usize
                        - (page << PAGE_BITS)
                };
                let backing = target as usize - (page << PAGE_BITS);

                page_table.pointers[page].store(host_ptr, page_type);
                page_table.backing_addr[page] = backing as u64;
                page_table.blocks[page] = orig_base << (PAGE_BITS as u64);

                page += 1;
                target += PAGE_SIZE;
            }
        }
    }
}
