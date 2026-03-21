//! Port of zuyu/src/core/memory.h / memory.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-17
//!
//! Core::Memory::Memory — bridges KPageTableBase with the dynarmic page table
//! and the DeviceMemory backing store.

use common::host_memory::HostMemory;
#[cfg(target_os = "linux")]
use common::heap_tracker::HeapTracker;
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
    /// Pointer to the HostMemory buffer (used for fastmem arena base).
    buffer: *const HostMemory,
    /// On Linux: HeapTracker wrapping HostMemory for separate heap fault handling.
    /// Upstream: `std::optional<Common::HeapTracker> heap_tracker` + `HeapTracker* buffer`.
    #[cfg(target_os = "linux")]
    heap_tracker: Option<Box<HeapTracker>>,
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
            #[cfg(target_os = "linux")]
            heap_tracker: None,
            current_page_table: std::ptr::null_mut(),
        }
    }

    /// Set the current page table (called when switching processes).
    /// Set the current page table and wire up the fastmem arena.
    /// Matches upstream `Memory::Impl::SetCurrentPageTable`.
    ///
    /// Upstream conditionally sets fastmem_arena based on
    /// `process.IsApplication() && Settings::IsFastmemEnabled()`.
    /// Settings are available via `common::settings::values()` but fastmem
    /// enablement depends on the process context which varies at runtime.
    pub fn set_current_page_table(&mut self, page_table: *mut PageTable) {
        self.current_page_table = page_table;
        if !page_table.is_null() && !self.buffer.is_null() {
            let pt = unsafe { &mut *page_table };
            // Upstream: if (process.IsApplication() && Settings::IsFastmemEnabled())
            //     page_table.fastmem_arena = buffer.VirtualBasePointer();
            // else
            //     page_table.fastmem_arena = nullptr;
            pt.fastmem_arena = unsafe { (*self.buffer).virtual_base_pointer() };

            // On Linux, create a HeapTracker wrapping the HostMemory buffer.
            // Upstream: heap_tracker.emplace(system.DeviceMemory().buffer);
            //           buffer = std::addressof(*heap_tracker);
            #[cfg(target_os = "linux")]
            {
                let host_mem = unsafe { &mut *(self.buffer as *mut HostMemory) };
                self.heap_tracker = Some(Box::new(HeapTracker::new(host_mem)));
            }
        }
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
            // Upstream: buffer->Map(base, target - DramBase, size, perms, separate_heap)
            // On Linux, buffer is HeapTracker*; on non-Linux, buffer is HostMemory*.
            #[cfg(target_os = "linux")]
            if let Some(ref heap_tracker) = self.heap_tracker {
                heap_tracker.map(
                    base as usize,
                    (target - dram_memory_map::BASE) as usize,
                    size as usize,
                    perms,
                    separate_heap,
                );
            }
            #[cfg(not(target_os = "linux"))]
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
            #[cfg(target_os = "linux")]
            if let Some(ref heap_tracker) = self.heap_tracker {
                heap_tracker.unmap(base as usize, size as usize, separate_heap);
            }
            #[cfg(not(target_os = "linux"))]
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
                        self.protect_buffer(protect_begin as usize, protect_bytes as usize, perms);
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
            self.protect_buffer(protect_begin as usize, protect_bytes as usize, perms);
        }
    }

    // =========================================================================
    // Read/Write via PageTable pointers
    // Matches upstream Core::Memory::Memory::Read/Write methods.
    // =========================================================================

    /// Get a host pointer for a guest virtual address (fast path).
    /// Matches upstream `Memory::Impl::GetPointerImpl`.
    ///
    /// Returns null if the page is unmapped.
    /// Route protect calls through HeapTracker on Linux, HostMemory otherwise.
    fn protect_buffer(&self, offset: usize, size: usize, perms: MemoryPermission) {
        #[cfg(target_os = "linux")]
        if let Some(ref heap_tracker) = self.heap_tracker {
            heap_tracker.protect(offset, size, perms);
            return;
        }
        unsafe {
            (*self.buffer).protect(offset, size, perms);
        }
    }

    #[inline]
    fn get_pointer_impl(&self, vaddr: u64) -> *mut u8 {
        // AARCH64 masks the upper 16 bits of all memory accesses.
        let vaddr = vaddr & 0xffff_ffff_ffff;

        if self.current_page_table.is_null() {
            return std::ptr::null_mut();
        }
        let pt = unsafe { &*self.current_page_table };
        let page_idx = (vaddr >> PAGE_BITS) as usize;
        if page_idx >= pt.pointers.size() {
            return std::ptr::null_mut();
        }

        let raw = pt.pointers[page_idx].raw_value();
        let pointer = PageInfo::extract_pointer(raw);
        if pointer != 0 {
            // Fast path: direct host pointer + vaddr
            return (pointer + vaddr as usize) as *mut u8;
        }

        // Slow path: check page type
        match PageInfo::extract_type(raw) {
            PageType::Unmapped => std::ptr::null_mut(),
            PageType::Memory => {
                // Upstream: ASSERT_MSG(false, "Mapped memory page without a pointer")
                debug_assert!(false, "Mapped memory page without a pointer @ {:#018x}", vaddr);
                std::ptr::null_mut()
            }
            PageType::DebugMemory => {
                self.get_pointer_from_debug_memory(vaddr)
            }
            PageType::RasterizerCachedMemory => {
                self.get_pointer_from_rasterizer_cached_memory(vaddr)
            }
        }
    }

    /// Get pointer from debug memory (slow path).
    /// Matches upstream `Memory::Impl::GetPointerFromDebugMemory`.
    fn get_pointer_from_debug_memory(&self, vaddr: u64) -> *mut u8 {
        if self.current_page_table.is_null() {
            return std::ptr::null_mut();
        }
        let pt = unsafe { &*self.current_page_table };
        let page_idx = (vaddr >> PAGE_BITS) as usize;
        if page_idx >= pt.backing_addr.size() {
            return std::ptr::null_mut();
        }
        let backing = pt.backing_addr[page_idx] as usize;
        if backing == 0 {
            return std::ptr::null_mut();
        }
        unsafe {
            let dm = &*self.device_memory;
            dm.buffer.backing_base_pointer().add(backing + vaddr as usize) as *mut u8
        }
    }

    /// Get pointer from rasterizer cached memory (slow path).
    /// Matches upstream `Memory::Impl::GetPointerFromRasterizerCachedMemory`.
    fn get_pointer_from_rasterizer_cached_memory(&self, vaddr: u64) -> *mut u8 {
        // For now, same as debug memory (rasterizer cache not yet implemented).
        self.get_pointer_from_debug_memory(vaddr)
    }

    /// Get a host pointer for a guest virtual address.
    /// Matches upstream `Memory::GetPointer`.
    pub fn get_pointer(&self, vaddr: u64) -> *mut u8 {
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!("Unmapped GetPointer @ {:#018x}", vaddr);
        }
        ptr
    }

    /// Get a host pointer without logging on unmapped addresses.
    /// Matches upstream `Memory::GetPointerSilent`.
    pub fn get_pointer_silent(&self, vaddr: u64) -> *mut u8 {
        self.get_pointer_impl(vaddr)
    }

    /// Read a value of type T from guest virtual address.
    /// Matches upstream `Memory::Impl::Read<T>`.
    #[inline]
    unsafe fn read_raw<T: Copy + Default>(&self, vaddr: u64) -> T {
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!("Unmapped Read{} @ {:#018x}", std::mem::size_of::<T>() * 8, vaddr);
            return T::default();
        }
        std::ptr::read_unaligned(ptr as *const T)
    }

    /// Write a value of type T to guest virtual address.
    /// Matches upstream `Memory::Impl::Write<T>`.
    #[inline]
    unsafe fn write_raw<T: Copy>(&self, vaddr: u64, data: T) {
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!("Unmapped Write{} @ {:#018x}", std::mem::size_of::<T>() * 8, vaddr);
            return;
        }
        std::ptr::write_unaligned(ptr as *mut T, data);
    }

    /// Read a u8. Matches upstream `Memory::Read8`.
    pub fn read_8(&self, vaddr: u64) -> u8 {
        unsafe { self.read_raw::<u8>(vaddr) }
    }

    /// Read a u16 (LE). Matches upstream `Memory::Read16`.
    pub fn read_16(&self, vaddr: u64) -> u16 {
        if (vaddr & 1) == 0 {
            unsafe { self.read_raw::<u16>(vaddr) }
        } else {
            let a = self.read_8(vaddr) as u16;
            let b = self.read_8(vaddr + 1) as u16;
            (b << 8) | a
        }
    }

    /// Read a u32 (LE). Matches upstream `Memory::Read32`.
    pub fn read_32(&self, vaddr: u64) -> u32 {
        if (vaddr & 3) == 0 {
            unsafe { self.read_raw::<u32>(vaddr) }
        } else {
            let a = self.read_16(vaddr) as u32;
            let b = self.read_16(vaddr + 2) as u32;
            (b << 16) | a
        }
    }

    /// Read a u64 (LE). Matches upstream `Memory::Read64`.
    pub fn read_64(&self, vaddr: u64) -> u64 {
        if (vaddr & 7) == 0 {
            unsafe { self.read_raw::<u64>(vaddr) }
        } else {
            let a = self.read_32(vaddr) as u64;
            let b = self.read_32(vaddr + 4) as u64;
            (b << 32) | a
        }
    }

    /// Write a u8. Matches upstream `Memory::Write8`.
    pub fn write_8(&self, vaddr: u64, data: u8) {
        unsafe { self.write_raw::<u8>(vaddr, data) }
    }

    /// Write a u16 (LE). Matches upstream `Memory::Write16`.
    pub fn write_16(&self, vaddr: u64, data: u16) {
        if (vaddr & 1) == 0 {
            unsafe { self.write_raw::<u16>(vaddr, data) }
        } else {
            self.write_8(vaddr, data as u8);
            self.write_8(vaddr + 1, (data >> 8) as u8);
        }
    }

    /// Write a u32 (LE). Matches upstream `Memory::Write32`.
    pub fn write_32(&self, vaddr: u64, data: u32) {
        if (vaddr & 3) == 0 {
            unsafe { self.write_raw::<u32>(vaddr, data) }
        } else {
            self.write_16(vaddr, data as u16);
            self.write_16(vaddr + 2, (data >> 16) as u16);
        }
    }

    /// Write a u64 (LE). Matches upstream `Memory::Write64`.
    pub fn write_64(&self, vaddr: u64, data: u64) {
        if (vaddr & 7) == 0 {
            unsafe { self.write_raw::<u64>(vaddr, data) }
        } else {
            self.write_32(vaddr, data as u32);
            self.write_32(vaddr + 4, (data >> 32) as u32);
        }
    }

    /// Check if an address range is within the current address space.
    /// Matches upstream `AddressSpaceContains`.
    fn address_space_contains(&self, addr: u64, size: usize) -> bool {
        if self.current_page_table.is_null() {
            return false;
        }
        let pt = unsafe { &*self.current_page_table };
        let max_addr = 1u64 << pt.current_address_space_width_in_bits;
        let end = addr.checked_add(size as u64);
        match end {
            Some(e) => e >= addr && e <= max_addr,
            None => false,
        }
    }

    /// Read a block of data from guest memory.
    /// Matches upstream `Memory::ReadBlock` (via WalkBlock pattern).
    pub fn read_block(&self, src_addr: u64, dest: &mut [u8]) -> bool {
        let size = dest.len();

        // Upstream: AddressSpaceContains check before walking pages.
        if !self.address_space_contains(src_addr, size) {
            log::error!("Unmapped ReadBlock @ {:#018x} size={:#x}", src_addr, size);
            dest.fill(0);
            return false;
        }

        let mut remaining = size;
        let mut offset = 0usize;
        let mut vaddr = src_addr;
        let mut user_accessible = true;

        while remaining > 0 {
            let page_offset = (vaddr & PAGE_MASK) as usize;
            let copy_amount = ((PAGE_SIZE as usize) - page_offset).min(remaining);

            let ptr = self.get_pointer_impl(vaddr);
            if ptr.is_null() {
                log::error!("Unmapped ReadBlock @ {:#018x}", vaddr);
                // Zero destination for unmapped pages, matching upstream.
                dest[offset..offset + copy_amount].fill(0);
                user_accessible = false;
            } else {
                unsafe {
                    std::ptr::copy_nonoverlapping(ptr, dest[offset..].as_mut_ptr(), copy_amount);
                }
            }

            vaddr += copy_amount as u64;
            offset += copy_amount;
            remaining -= copy_amount;
        }
        user_accessible
    }

    /// Write a block of data to guest memory.
    /// Matches upstream `Memory::WriteBlock` (via WalkBlock pattern).
    pub fn write_block(&self, dest_addr: u64, src: &[u8]) -> bool {
        let size = src.len();

        // Upstream: AddressSpaceContains check before walking pages.
        if !self.address_space_contains(dest_addr, size) {
            log::error!("Unmapped WriteBlock @ {:#018x} size={:#x}", dest_addr, size);
            return false;
        }

        let mut remaining = size;
        let mut offset = 0usize;
        let mut vaddr = dest_addr;
        let mut user_accessible = true;

        while remaining > 0 {
            let page_offset = (vaddr & PAGE_MASK) as usize;
            let copy_amount = ((PAGE_SIZE as usize) - page_offset).min(remaining);

            let ptr = self.get_pointer_impl(vaddr);
            if ptr.is_null() {
                log::error!("Unmapped WriteBlock @ {:#018x}", vaddr);
                user_accessible = false;
            } else {
                unsafe {
                    std::ptr::copy_nonoverlapping(src[offset..].as_ptr(), ptr, copy_amount);
                }
            }

            vaddr += copy_amount as u64;
            offset += copy_amount;
            remaining -= copy_amount;
        }
        user_accessible
    }

    /// Zero a block of guest memory.
    /// Matches upstream `Memory::ZeroBlock` (via WalkBlock pattern).
    pub fn zero_block(&self, dest_addr: u64, size: usize) -> bool {
        if !self.address_space_contains(dest_addr, size) {
            log::error!("Unmapped ZeroBlock @ {:#018x} size={:#x}", dest_addr, size);
            return false;
        }

        let mut remaining = size;
        let mut vaddr = dest_addr;
        let mut user_accessible = true;

        while remaining > 0 {
            let page_offset = (vaddr & PAGE_MASK) as usize;
            let copy_amount = ((PAGE_SIZE as usize) - page_offset).min(remaining);

            let ptr = self.get_pointer_impl(vaddr);
            if ptr.is_null() {
                log::error!("Unmapped ZeroBlock @ {:#018x}", vaddr);
                user_accessible = false;
            } else {
                unsafe {
                    std::ptr::write_bytes(ptr, 0, copy_amount);
                }
            }

            vaddr += copy_amount as u64;
            remaining -= copy_amount;
        }
        user_accessible
    }

    /// Copy a block within guest memory.
    /// Matches upstream `Memory::CopyBlock`.
    pub fn copy_block(&self, dest_addr: u64, src_addr: u64, size: usize) -> bool {
        let mut buf = vec![0u8; size];
        self.read_block(src_addr, &mut buf);
        self.write_block(dest_addr, &buf)
    }

    /// Check if a virtual address is valid (mapped).
    /// Matches upstream `Memory::IsValidVirtualAddress`.
    pub fn is_valid_virtual_address(&self, vaddr: u64) -> bool {
        if self.current_page_table.is_null() {
            return false;
        }
        let pt = unsafe { &*self.current_page_table };
        let page = (vaddr >> PAGE_BITS) as usize;
        if page >= pt.pointers.size() {
            return false;
        }
        let (pointer, ptype) = pt.pointers[page].pointer_type();
        pointer != 0
            || ptype == PageType::RasterizerCachedMemory
            || ptype == PageType::DebugMemory
    }

    /// Check if a virtual address range is valid (all pages mapped).
    /// Matches upstream `Memory::IsValidVirtualAddressRange`.
    pub fn is_valid_virtual_address_range(&self, base: u64, size: u64) -> bool {
        let end = base + size;
        let mut page = base & !(PAGE_MASK);
        while page < end {
            if !self.is_valid_virtual_address(page) {
                return false;
            }
            page += PAGE_SIZE;
        }
        true
    }

    // =========================================================================
    // Exclusive Write (atomic CAS) via PageTable pointers
    // Matches upstream Core::Memory::Memory::WriteExclusive* methods.
    // =========================================================================

    /// Exclusive write u8 with atomic CAS.
    /// Matches upstream `Memory::WriteExclusive8`.
    pub fn write_exclusive_8(&self, vaddr: u64, value: u8, expected: u8) -> bool {
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!("Unmapped WriteExclusive8 @ {:#018x}", vaddr);
            return true;
        }
        unsafe {
            let atomic = &*(ptr as *const std::sync::atomic::AtomicU8);
            atomic
                .compare_exchange(expected, value, std::sync::atomic::Ordering::SeqCst, std::sync::atomic::Ordering::SeqCst)
                .is_ok()
        }
    }

    /// Exclusive write u16 with atomic CAS.
    /// Matches upstream `Memory::WriteExclusive16`.
    pub fn write_exclusive_16(&self, vaddr: u64, value: u16, expected: u16) -> bool {
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!("Unmapped WriteExclusive16 @ {:#018x}", vaddr);
            return true;
        }
        unsafe {
            let atomic = &*(ptr as *const std::sync::atomic::AtomicU16);
            atomic
                .compare_exchange(expected, value, std::sync::atomic::Ordering::SeqCst, std::sync::atomic::Ordering::SeqCst)
                .is_ok()
        }
    }

    /// Exclusive write u32 with atomic CAS.
    /// Matches upstream `Memory::WriteExclusive32`.
    pub fn write_exclusive_32(&self, vaddr: u64, value: u32, expected: u32) -> bool {
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!("Unmapped WriteExclusive32 @ {:#018x}", vaddr);
            return true;
        }
        unsafe {
            let atomic = &*(ptr as *const std::sync::atomic::AtomicU32);
            atomic
                .compare_exchange(expected, value, std::sync::atomic::Ordering::SeqCst, std::sync::atomic::Ordering::SeqCst)
                .is_ok()
        }
    }

    /// Exclusive write u64 with atomic CAS.
    /// Matches upstream `Memory::WriteExclusive64`.
    pub fn write_exclusive_64(&self, vaddr: u64, value: u64, expected: u64) -> bool {
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!("Unmapped WriteExclusive64 @ {:#018x}", vaddr);
            return true;
        }
        unsafe {
            let atomic = &*(ptr as *const std::sync::atomic::AtomicU64);
            atomic
                .compare_exchange(expected, value, std::sync::atomic::Ordering::SeqCst, std::sync::atomic::Ordering::SeqCst)
                .is_ok()
        }
    }

    /// Exclusive write 128-bit with two 64-bit atomic CAS operations.
    /// Matches upstream `Memory::WriteExclusive128`.
    pub fn write_exclusive_128(
        &self, vaddr: u64,
        value_lo: u64, value_hi: u64,
        expected_lo: u64, expected_hi: u64,
    ) -> bool {
        // Upstream uses AtomicCompareAndSwap for 128-bit. On x86-64 without
        // native 128-bit CAS, fall back to two 64-bit CAS operations.
        // The low half must succeed before attempting the high half.
        let lo_ok = self.write_exclusive_64(vaddr, value_lo, expected_lo);
        if !lo_ok {
            return false;
        }
        self.write_exclusive_64(vaddr + 8, value_hi, expected_hi)
    }

    /// Invalidate a separate heap fault address.
    ///
    /// Upstream: `Memory::InvalidateSeparateHeap(void* fault_address)` (memory.cpp:1104).
    /// On Linux, delegates to `HeapTracker::DeferredMapSeparateHeap(fault_address)`.
    /// On non-Linux, returns false.
    pub fn invalidate_separate_heap(&self, fault_address: *const u8) -> bool {
        #[cfg(target_os = "linux")]
        {
            if let Some(ref heap_tracker) = self.heap_tracker {
                return heap_tracker.deferred_map_separate_heap(fault_address);
            }
            false
        }
        #[cfg(not(target_os = "linux"))]
        {
            let _ = fault_address;
            false
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
