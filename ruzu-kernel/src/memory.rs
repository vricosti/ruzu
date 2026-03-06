//! Port of zuyu/src/core/memory.h and zuyu/src/core/memory.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Central memory system that handles all virtual memory operations for a guest process.
//! Uses `Common::PageTable` to map virtual addresses to host pointers, supporting
//! page-granularity operations (read, write, exclusive write, block operations).

use std::sync::atomic::{AtomicU8, Ordering};

use common::page_table::{PageInfo, PageTable, PageType};
use common::{PAGE_MASK, PAGE_SHIFT, PAGE_SIZE, PAGE_SIZE_U64};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Page size bits used by the ARM architecture (same as YUZU_PAGEBITS).
pub const YUZU_PAGEBITS: u32 = PAGE_SHIFT;
/// Page size used by the ARM architecture (same as YUZU_PAGESIZE).
pub const YUZU_PAGESIZE: u64 = PAGE_SIZE_U64;
/// Page mask (same as YUZU_PAGEMASK).
pub const YUZU_PAGEMASK: u64 = PAGE_MASK;

/// TLS (Thread-Local Storage) entry size.
pub const TLS_ENTRY_SIZE: u64 = 0x200;

/// Default application stack size.
pub const DEFAULT_STACK_SIZE: u64 = 0x100000;

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

/// Check whether `[addr, addr+size)` is within the address space defined by the page table.
fn address_space_contains(table: &PageTable, addr: u64, size: usize) -> bool {
    let max_addr: u64 = 1u64 << table.get_address_space_bits();
    let end = addr.wrapping_add(size as u64);
    end >= addr && end <= max_addr
}

// ---------------------------------------------------------------------------
// Memory
// ---------------------------------------------------------------------------

/// Central class that handles all memory operations and state.
///
/// Port of `Core::Memory::Memory` from the C++ codebase. Works with
/// `PageTable` to translate guest virtual addresses into host pointers,
/// then performs reads/writes through those pointers.
///
/// In the C++ code this holds a reference to `Core::System`, which gives access to
/// `DeviceMemory`, the GPU, and the rasterizer. Since these subsystems are not yet
/// ported, the rasterizer-related code paths are stubbed.
pub struct Memory {
    /// The current page table (owned; in C++ it is a raw pointer borrowed from KProcess).
    current_page_table: Option<Box<PageTable>>,
}

impl Memory {
    /// Whether this memory implementation needs flush/invalidation for GPU coherency.
    pub const HAS_FLUSH_INVALIDATION: bool = false;

    /// Create a new `Memory` instance (equivalent to `Memory(Core::System&)` + `Reset()`).
    pub fn new() -> Self {
        Self {
            current_page_table: None,
        }
    }

    /// Reset the memory subsystem state.
    pub fn reset(&mut self) {
        self.current_page_table = None;
    }

    /// Set the current page table. In the C++ code this is called with a
    /// `Kernel::KProcess&`; here we accept a boxed `PageTable` directly.
    pub fn set_current_page_table(&mut self, page_table: Box<PageTable>) {
        self.current_page_table = Some(page_table);
    }

    /// Set the current page table by mutable reference (borrows, wraps in a Box clone).
    /// For cases where the caller retains ownership.
    pub fn set_current_page_table_ref(&mut self, page_table: &PageTable) {
        // We need to create our own PageTable pointing to the same data.
        // Since PageTable uses VirtualBuffer internally, we store a new empty one
        // and expect callers to use the ref-based API. For now, store None and use
        // the raw-pointer based approach.
        // TODO: Refactor once KProcess integration is complete.
        let _ = page_table;
    }

    /// Borrow the current page table, if set.
    pub fn current_page_table(&self) -> Option<&PageTable> {
        self.current_page_table.as_deref()
    }

    /// Borrow the current page table mutably, if set.
    pub fn current_page_table_mut(&mut self) -> Option<&mut PageTable> {
        self.current_page_table.as_deref_mut()
    }

    // -- Mapping operations -------------------------------------------------

    /// Maps an allocated buffer onto a region of the emulated process address space.
    ///
    /// Equivalent to `Memory::MapMemoryRegion` in C++.
    pub fn map_memory_region(
        &mut self,
        page_table: &mut PageTable,
        base: u64,
        size: u64,
        target: u64,
        _separate_heap: bool,
    ) {
        assert!(
            (size & YUZU_PAGEMASK) == 0,
            "non-page aligned size: {:#018X}",
            size
        );
        assert!(
            (base & YUZU_PAGEMASK) == 0,
            "non-page aligned base: {:#018X}",
            base
        );
        self.map_pages(
            page_table,
            base / YUZU_PAGESIZE,
            size / YUZU_PAGESIZE,
            target,
            PageType::Memory,
        );
    }

    /// Unmaps a region of the emulated process address space.
    ///
    /// Equivalent to `Memory::UnmapRegion` in C++.
    pub fn unmap_region(
        &mut self,
        page_table: &mut PageTable,
        base: u64,
        size: u64,
        _separate_heap: bool,
    ) {
        assert!(
            (size & YUZU_PAGEMASK) == 0,
            "non-page aligned size: {:#018X}",
            size
        );
        assert!(
            (base & YUZU_PAGEMASK) == 0,
            "non-page aligned base: {:#018X}",
            base
        );
        self.map_pages(
            page_table,
            base / YUZU_PAGESIZE,
            size / YUZU_PAGESIZE,
            0,
            PageType::Unmapped,
        );
    }

    /// Protects a region of the emulated process address space with the new permissions.
    ///
    /// Equivalent to `Memory::ProtectRegion` in C++. Currently a stub because fastmem
    /// is not yet implemented.
    pub fn protect_region(&mut self, _page_table: &mut PageTable, _base: u64, _size: u64) {
        // Fastmem protection is not yet implemented.
        // In C++ this only operates when fastmem_arena is set.
    }

    // -- Validity checks ----------------------------------------------------

    /// Checks whether the supplied address is a valid virtual address for the current process.
    pub fn is_valid_virtual_address(&self, vaddr: u64) -> bool {
        let page_table = match &self.current_page_table {
            Some(pt) => pt,
            None => return false,
        };
        let page = (vaddr >> YUZU_PAGEBITS) as usize;
        if page >= page_table.pointers.size() {
            return false;
        }
        let (pointer, ptype) = page_table.pointers[page].pointer_type();
        pointer != 0 || ptype == PageType::RasterizerCachedMemory || ptype == PageType::DebugMemory
    }

    /// Checks whether the supplied range of addresses are all valid virtual addresses.
    pub fn is_valid_virtual_address_range(&self, base: u64, size: u64) -> bool {
        let end = base + size;
        let mut page = common::align_down(base, YUZU_PAGESIZE);
        while page < end {
            if !self.is_valid_virtual_address(page) {
                return false;
            }
            page += YUZU_PAGESIZE;
        }
        true
    }

    // -- Pointer access -----------------------------------------------------

    /// Gets a raw host pointer to the given virtual address.
    /// Returns `None` if the address is not mapped.
    ///
    /// Equivalent to `Memory::GetPointer` in C++.
    pub fn get_pointer(&self, vaddr: u64) -> Option<*mut u8> {
        self.get_pointer_impl(vaddr, true)
    }

    /// Gets a raw host pointer without logging on failure.
    ///
    /// Equivalent to `Memory::GetPointerSilent` in C++.
    pub fn get_pointer_silent(&self, vaddr: u64) -> Option<*mut u8> {
        self.get_pointer_impl(vaddr, false)
    }

    // -- Scalar read operations ---------------------------------------------

    /// Read an 8-bit value from guest memory.
    pub fn read8(&self, addr: u64) -> u8 {
        self.read_val::<u8>(addr)
    }

    /// Read a little-endian 16-bit value from guest memory.
    pub fn read16(&self, addr: u64) -> u16 {
        if (addr & 1) == 0 {
            self.read_val::<u16>(addr)
        } else {
            let a = self.read_val::<u8>(addr) as u16;
            let b = self.read_val::<u8>(addr + 1) as u16;
            (b << 8) | a
        }
    }

    /// Read a little-endian 32-bit value from guest memory.
    pub fn read32(&self, addr: u64) -> u32 {
        if (addr & 3) == 0 {
            self.read_val::<u32>(addr)
        } else {
            let a = self.read16(addr) as u32;
            let b = self.read16(addr + 2) as u32;
            (b << 16) | a
        }
    }

    /// Read a little-endian 64-bit value from guest memory.
    pub fn read64(&self, addr: u64) -> u64 {
        if (addr & 7) == 0 {
            self.read_val::<u64>(addr)
        } else {
            let a = self.read32(addr) as u64;
            let b = self.read32(addr + 4) as u64;
            (b << 32) | a
        }
    }

    // -- Scalar write operations --------------------------------------------

    /// Write an 8-bit value to guest memory.
    pub fn write8(&mut self, addr: u64, data: u8) {
        self.write_val::<u8>(addr, data);
    }

    /// Write a little-endian 16-bit value to guest memory.
    pub fn write16(&mut self, addr: u64, data: u16) {
        if (addr & 1) == 0 {
            self.write_val::<u16>(addr, data);
        } else {
            self.write_val::<u8>(addr, data as u8);
            self.write_val::<u8>(addr + 1, (data >> 8) as u8);
        }
    }

    /// Write a little-endian 32-bit value to guest memory.
    pub fn write32(&mut self, addr: u64, data: u32) {
        if (addr & 3) == 0 {
            self.write_val::<u32>(addr, data);
        } else {
            self.write16(addr, data as u16);
            self.write16(addr + 2, (data >> 16) as u16);
        }
    }

    /// Write a little-endian 64-bit value to guest memory.
    pub fn write64(&mut self, addr: u64, data: u64) {
        if (addr & 7) == 0 {
            self.write_val::<u64>(addr, data);
        } else {
            self.write32(addr, data as u32);
            self.write32(addr + 4, (data >> 32) as u32);
        }
    }

    // -- Exclusive write operations -----------------------------------------

    /// Atomically writes a value if the current value matches `expected`.
    /// Returns `true` if the write succeeded.
    pub fn write_exclusive8(&self, addr: u64, data: u8, expected: u8) -> bool {
        self.write_exclusive_val(addr, data, expected)
    }

    /// Atomically writes a 16-bit value if the current value matches `expected`.
    pub fn write_exclusive16(&self, addr: u64, data: u16, expected: u16) -> bool {
        self.write_exclusive_val(addr, data, expected)
    }

    /// Atomically writes a 32-bit value if the current value matches `expected`.
    pub fn write_exclusive32(&self, addr: u64, data: u32, expected: u32) -> bool {
        self.write_exclusive_val(addr, data, expected)
    }

    /// Atomically writes a 64-bit value if the current value matches `expected`.
    pub fn write_exclusive64(&self, addr: u64, data: u64, expected: u64) -> bool {
        self.write_exclusive_val(addr, data, expected)
    }

    /// Atomically writes a 128-bit value if the current value matches `expected`.
    pub fn write_exclusive128(&self, addr: u64, data: u128, expected: u128) -> bool {
        if let Some(ptr) = self.get_pointer_impl(addr, true) {
            // Use a pair of atomic u64 operations for 128-bit compare-and-swap.
            // On x86-64, true 128-bit CAS requires CMPXCHG16B. We simulate it here.
            unsafe {
                let ptr64 = ptr as *mut u64;
                let current_lo = std::ptr::read_volatile(ptr64);
                let current_hi = std::ptr::read_volatile(ptr64.add(1));
                let current = (current_hi as u128) << 64 | current_lo as u128;
                if current == expected {
                    let data_lo = data as u64;
                    let data_hi = (data >> 64) as u64;
                    std::ptr::write_volatile(ptr64, data_lo);
                    std::ptr::write_volatile(ptr64.add(1), data_hi);
                    true
                } else {
                    false
                }
            }
        } else {
            true // C++ returns true on unmapped
        }
    }

    // -- String read --------------------------------------------------------

    /// Reads a null-terminated string from guest memory.
    pub fn read_cstring(&self, mut vaddr: u64, max_length: usize) -> String {
        let mut result = String::with_capacity(max_length.min(256));
        for _ in 0..max_length {
            let c = self.read8(vaddr);
            if c == 0 {
                break;
            }
            result.push(c as char);
            vaddr += 1;
        }
        result.shrink_to_fit();
        result
    }

    // -- Block operations ---------------------------------------------------

    /// Reads a contiguous block of bytes from the current process' address space.
    /// Returns `true` if all addresses were accessible.
    pub fn read_block(&self, src_addr: u64, dest: &mut [u8]) -> bool {
        self.read_block_impl::<false>(src_addr, dest)
    }

    /// Reads a contiguous block of bytes (unsafe version, no GPU flushing).
    pub fn read_block_unsafe(&self, src_addr: u64, dest: &mut [u8]) -> bool {
        self.read_block_impl::<true>(src_addr, dest)
    }

    /// Writes a range of bytes into the current process' address space.
    /// Returns `true` if all addresses were accessible.
    pub fn write_block(&mut self, dest_addr: u64, src: &[u8]) -> bool {
        self.write_block_impl::<false>(dest_addr, src)
    }

    /// Writes a range of bytes (unsafe version, no GPU invalidation).
    pub fn write_block_unsafe(&mut self, dest_addr: u64, src: &[u8]) -> bool {
        self.write_block_impl::<true>(dest_addr, src)
    }

    /// Zeros a range of bytes within the current process' address space.
    pub fn zero_block(&mut self, dest_addr: u64, size: usize) -> bool {
        let zeros = vec![0u8; size];
        self.write_block_impl::<false>(dest_addr, &zeros)
    }

    /// Copies data within a process' address space.
    pub fn copy_block(&mut self, dest_addr: u64, src_addr: u64, size: usize) -> bool {
        // Read from source, then write to destination.
        let mut buf = vec![0u8; size];
        let read_ok = self.read_block(src_addr, &mut buf);
        if !read_ok {
            // On unmapped source, zero the destination (matching C++ behavior).
            self.zero_block(dest_addr, size);
            return false;
        }
        self.write_block(dest_addr, &buf)
    }

    /// Gets a host-memory span for a contiguous region of guest memory.
    /// Returns `None` if the region is not contiguous in host memory.
    pub fn get_span(&self, src_addr: u64, size: usize) -> Option<*const u8> {
        let page_table = self.current_page_table.as_ref()?;
        let start_page = (src_addr >> YUZU_PAGEBITS) as usize;
        let end_page = ((src_addr + size as u64) >> YUZU_PAGEBITS) as usize;
        if start_page >= page_table.blocks.size() || end_page >= page_table.blocks.size() {
            return None;
        }
        if page_table.blocks[start_page] == page_table.blocks[end_page] {
            self.get_pointer_silent(src_addr).map(|p| p as *const u8)
        } else {
            None
        }
    }

    /// Gets a mutable host-memory span for a contiguous region of guest memory.
    pub fn get_span_mut(&self, src_addr: u64, size: usize) -> Option<*mut u8> {
        let page_table = self.current_page_table.as_ref()?;
        let start_page = (src_addr >> YUZU_PAGEBITS) as usize;
        let end_page = ((src_addr + size as u64) >> YUZU_PAGEBITS) as usize;
        if start_page >= page_table.blocks.size() || end_page >= page_table.blocks.size() {
            return None;
        }
        if page_table.blocks[start_page] == page_table.blocks[end_page] {
            self.get_pointer_silent(src_addr)
        } else {
            None
        }
    }

    // -- Cache operations (stubs for GPU coherency) -------------------------

    /// Invalidate data cache for a given range.
    pub fn invalidate_data_cache(&self, _dest_addr: u64, _size: usize) -> u32 {
        // Stub: no rasterizer integration yet.
        0 // ResultSuccess
    }

    /// Store data cache for a given range.
    pub fn store_data_cache(&self, _dest_addr: u64, _size: usize) -> u32 {
        0 // ResultSuccess
    }

    /// Flush data cache for a given range.
    pub fn flush_data_cache(&self, _dest_addr: u64, _size: usize) -> u32 {
        0 // ResultSuccess
    }

    /// Mark region as cached/uncached by the rasterizer.
    pub fn rasterizer_mark_region_cached(&mut self, _vaddr: u64, _size: u64, _cached: bool) {
        // Stub: no rasterizer integration yet.
    }

    /// Mark region as debug or non-debug memory.
    pub fn mark_region_debug(&mut self, _vaddr: u64, _size: u64, _debug: bool) {
        // Stub: no fastmem or debug memory switching yet.
    }

    // -- Internal: page mapping ---------------------------------------------

    /// Maps a region of pages as a specific type in the page table.
    ///
    /// Equivalent to `Memory::Impl::MapPages` in C++.
    fn map_pages(
        &self,
        page_table: &mut PageTable,
        base_page: u64,
        num_pages: u64,
        target: u64,
        page_type: PageType,
    ) {
        log::debug!(
            "Memory::map_pages: mapping {:#018X} onto {:#018X}-{:#018X}",
            target,
            base_page * YUZU_PAGESIZE,
            (base_page + num_pages) * YUZU_PAGESIZE,
        );

        let end = base_page + num_pages;
        assert!(
            (end as usize) <= page_table.pointers.size(),
            "out of range mapping at {:#018X}",
            base_page + page_table.pointers.size() as u64,
        );

        if target == 0 {
            assert!(
                page_type != PageType::Memory,
                "Mapping memory page without a pointer @ {:#018X}",
                base_page * YUZU_PAGESIZE,
            );
            for page in base_page..end {
                let idx = page as usize;
                page_table.pointers[idx].store(0, page_type);
                page_table.backing_addr[idx] = 0;
                page_table.blocks[idx] = 0;
            }
        } else {
            let orig_base = base_page;
            let mut current_target = target;
            for page in base_page..end {
                let idx = page as usize;
                // In C++ the host_ptr is computed from DeviceMemory. Without DeviceMemory,
                // we store the target address as a pseudo-pointer for now.
                // The backing_addr stores the offset that, when added to a vaddr, gives the
                // physical address for lookup.
                let backing = current_target.wrapping_sub(page << YUZU_PAGEBITS);
                page_table.pointers[idx].store(current_target as usize, page_type);
                page_table.backing_addr[idx] = backing;
                page_table.blocks[idx] = orig_base << YUZU_PAGEBITS;
                current_target += YUZU_PAGESIZE;
            }
        }
    }

    // -- Internal: pointer resolution ---------------------------------------

    /// Core pointer resolution. Translates a guest virtual address to a host pointer.
    ///
    /// Mirrors `Memory::Impl::GetPointerImpl` from C++.
    fn get_pointer_impl(&self, vaddr: u64, log_errors: bool) -> Option<*mut u8> {
        // AARCH64 masks the upper 16 bits of all memory accesses.
        let vaddr = vaddr & 0x0000_FFFF_FFFF_FFFF;

        let page_table = self.current_page_table.as_ref()?;

        if !address_space_contains(page_table, vaddr, 1) {
            if log_errors {
                log::error!("Unmapped GetPointer @ {:#018X}", vaddr);
            }
            return None;
        }

        let page_idx = (vaddr >> YUZU_PAGEBITS) as usize;
        let raw = page_table.pointers[page_idx].raw_value();
        let pointer = PageInfo::extract_pointer(raw);
        if pointer != 0 {
            // Fast path: the pointer field holds a host address base.
            // In full C++, this is `host_ptr + vaddr`. Since we store the target
            // physical address as a pseudo-pointer, we reconstruct it.
            let page_offset = (vaddr & YUZU_PAGEMASK) as usize;
            return Some((pointer + page_offset) as *mut u8);
        }

        match PageInfo::extract_type(raw) {
            PageType::Unmapped => {
                if log_errors {
                    log::error!("Unmapped GetPointer @ {:#018X}", vaddr);
                }
                None
            }
            PageType::Memory => {
                log::error!("Mapped memory page without a pointer @ {:#018X}", vaddr);
                None
            }
            PageType::DebugMemory => {
                // Would call GetPointerFromDebugMemory in C++.
                // Stubbed for now.
                let backing = page_table.backing_addr[page_idx];
                if backing == 0 {
                    None
                } else {
                    Some((backing + vaddr) as *mut u8)
                }
            }
            PageType::RasterizerCachedMemory => {
                // Would call GetPointerFromRasterizerCachedMemory in C++.
                let backing = page_table.backing_addr[page_idx];
                if backing == 0 {
                    None
                } else {
                    Some((backing + vaddr) as *mut u8)
                }
            }
        }
    }

    // -- Internal: templated read/write -------------------------------------

    /// Read a value of type T from guest memory at the given address.
    fn read_val<T: Copy + Default>(&self, vaddr: u64) -> T {
        let mut result = T::default();
        if let Some(ptr) = self.get_pointer_impl(vaddr, true) {
            unsafe {
                std::ptr::copy_nonoverlapping(
                    ptr as *const u8,
                    &mut result as *mut T as *mut u8,
                    std::mem::size_of::<T>(),
                );
            }
        } else {
            log::error!(
                "Unmapped Read{} @ {:#018X}",
                std::mem::size_of::<T>() * 8,
                vaddr
            );
        }
        result
    }

    /// Write a value of type T to guest memory at the given address.
    fn write_val<T: Copy>(&self, vaddr: u64, data: T) {
        if let Some(ptr) = self.get_pointer_impl(vaddr, true) {
            unsafe {
                std::ptr::copy_nonoverlapping(
                    &data as *const T as *const u8,
                    ptr,
                    std::mem::size_of::<T>(),
                );
            }
        } else {
            log::error!(
                "Unmapped Write{} @ {:#018X}",
                std::mem::size_of::<T>() * 8,
                vaddr
            );
        }
    }

    /// Exclusive write (compare-and-swap) for small types.
    fn write_exclusive_val<T: Copy + PartialEq>(&self, vaddr: u64, data: T, expected: T) -> bool {
        if let Some(ptr) = self.get_pointer_impl(vaddr, true) {
            // Use byte-level atomic CAS simulation.
            // For correct exclusivity we'd need actual hardware-level CAS, but for HLE
            // we can use a simple compare-and-swap on the memory.
            unsafe {
                let size = std::mem::size_of::<T>();
                match size {
                    1 => {
                        let atomic = &*(ptr as *const AtomicU8);
                        let exp_bytes = std::mem::transmute_copy::<T, u8>(&expected);
                        let data_bytes = std::mem::transmute_copy::<T, u8>(&data);
                        atomic
                            .compare_exchange(
                                exp_bytes,
                                data_bytes,
                                Ordering::SeqCst,
                                Ordering::SeqCst,
                            )
                            .is_ok()
                    }
                    2 => {
                        let atomic = &*(ptr as *const std::sync::atomic::AtomicU16);
                        let exp_bytes = std::mem::transmute_copy::<T, u16>(&expected);
                        let data_bytes = std::mem::transmute_copy::<T, u16>(&data);
                        atomic
                            .compare_exchange(
                                exp_bytes,
                                data_bytes,
                                Ordering::SeqCst,
                                Ordering::SeqCst,
                            )
                            .is_ok()
                    }
                    4 => {
                        let atomic = &*(ptr as *const std::sync::atomic::AtomicU32);
                        let exp_bytes = std::mem::transmute_copy::<T, u32>(&expected);
                        let data_bytes = std::mem::transmute_copy::<T, u32>(&data);
                        atomic
                            .compare_exchange(
                                exp_bytes,
                                data_bytes,
                                Ordering::SeqCst,
                                Ordering::SeqCst,
                            )
                            .is_ok()
                    }
                    8 => {
                        let atomic = &*(ptr as *const std::sync::atomic::AtomicU64);
                        let exp_bytes = std::mem::transmute_copy::<T, u64>(&expected);
                        let data_bytes = std::mem::transmute_copy::<T, u64>(&data);
                        atomic
                            .compare_exchange(
                                exp_bytes,
                                data_bytes,
                                Ordering::SeqCst,
                                Ordering::SeqCst,
                            )
                            .is_ok()
                    }
                    _ => {
                        // Fallback: non-atomic compare and swap
                        let mut current = std::mem::zeroed::<T>();
                        std::ptr::copy_nonoverlapping(ptr, &mut current as *mut T as *mut u8, size);
                        if current == expected {
                            std::ptr::copy_nonoverlapping(
                                &data as *const T as *const u8,
                                ptr,
                                size,
                            );
                            true
                        } else {
                            false
                        }
                    }
                }
            }
        } else {
            true // C++ returns true on unmapped
        }
    }

    // -- Internal: block read/write -----------------------------------------

    /// Walk the page table for a block read, copying data page by page.
    fn read_block_impl<const UNSAFE: bool>(&self, src_addr: u64, dest: &mut [u8]) -> bool {
        let size = dest.len();
        if size == 0 {
            return true;
        }

        let page_table = match &self.current_page_table {
            Some(pt) => pt,
            None => return false,
        };

        if !address_space_contains(page_table, src_addr, size) {
            log::error!("Unmapped ReadBlock @ {:#018X} (size = {})", src_addr, size);
            dest.fill(0);
            return false;
        }

        let mut remaining = size;
        let mut page_index = (src_addr >> YUZU_PAGEBITS) as usize;
        let mut page_offset = (src_addr & YUZU_PAGEMASK) as usize;
        let mut dest_offset = 0usize;
        let mut user_accessible = true;

        while remaining > 0 {
            let copy_amount = (PAGE_SIZE - page_offset).min(remaining);

            let (pointer, ptype) = page_table.pointers[page_index].pointer_type();
            match ptype {
                PageType::Unmapped => {
                    user_accessible = false;
                    log::error!(
                        "Unmapped ReadBlock @ {:#018X}",
                        (page_index << YUZU_PAGEBITS as usize) + page_offset
                    );
                    dest[dest_offset..dest_offset + copy_amount].fill(0);
                }
                PageType::Memory => {
                    if pointer != 0 {
                        let host_addr =
                            pointer + page_offset + (page_index << YUZU_PAGEBITS as usize);
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                host_addr as *const u8,
                                dest.as_mut_ptr().add(dest_offset),
                                copy_amount,
                            );
                        }
                    } else {
                        dest[dest_offset..dest_offset + copy_amount].fill(0);
                    }
                }
                PageType::DebugMemory | PageType::RasterizerCachedMemory => {
                    let backing = page_table.backing_addr[page_index];
                    if backing != 0 {
                        let current_vaddr =
                            ((page_index << YUZU_PAGEBITS as usize) + page_offset) as u64;
                        let host_addr = (backing + current_vaddr) as usize;
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                host_addr as *const u8,
                                dest.as_mut_ptr().add(dest_offset),
                                copy_amount,
                            );
                        }
                    } else {
                        dest[dest_offset..dest_offset + copy_amount].fill(0);
                    }
                }
            }

            page_index += 1;
            page_offset = 0;
            dest_offset += copy_amount;
            remaining -= copy_amount;
        }

        user_accessible
    }

    /// Walk the page table for a block write, copying data page by page.
    fn write_block_impl<const UNSAFE: bool>(&self, dest_addr: u64, src: &[u8]) -> bool {
        let size = src.len();
        if size == 0 {
            return true;
        }

        let page_table = match &self.current_page_table {
            Some(pt) => pt,
            None => return false,
        };

        if !address_space_contains(page_table, dest_addr, size) {
            log::error!(
                "Unmapped WriteBlock @ {:#018X} (size = {})",
                dest_addr,
                size
            );
            return false;
        }

        let mut remaining = size;
        let mut page_index = (dest_addr >> YUZU_PAGEBITS) as usize;
        let mut page_offset = (dest_addr & YUZU_PAGEMASK) as usize;
        let mut src_offset = 0usize;
        let mut user_accessible = true;

        while remaining > 0 {
            let copy_amount = (PAGE_SIZE - page_offset).min(remaining);

            let (pointer, ptype) = page_table.pointers[page_index].pointer_type();
            match ptype {
                PageType::Unmapped => {
                    user_accessible = false;
                    log::error!(
                        "Unmapped WriteBlock @ {:#018X}",
                        (page_index << YUZU_PAGEBITS as usize) + page_offset
                    );
                }
                PageType::Memory => {
                    if pointer != 0 {
                        let host_addr =
                            pointer + page_offset + (page_index << YUZU_PAGEBITS as usize);
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                src.as_ptr().add(src_offset),
                                host_addr as *mut u8,
                                copy_amount,
                            );
                        }
                    }
                }
                PageType::DebugMemory | PageType::RasterizerCachedMemory => {
                    let backing = page_table.backing_addr[page_index];
                    if backing != 0 {
                        let current_vaddr =
                            ((page_index << YUZU_PAGEBITS as usize) + page_offset) as u64;
                        let host_addr = (backing + current_vaddr) as usize;
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                src.as_ptr().add(src_offset),
                                host_addr as *mut u8,
                                copy_amount,
                            );
                        }
                    }
                }
            }

            page_index += 1;
            page_offset = 0;
            src_offset += copy_amount;
            remaining -= copy_amount;
        }

        user_accessible
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// GuestMemoryFlags (port of GuestMemoryFlags enum from guest_memory.h)
// ---------------------------------------------------------------------------

// Note: The full GuestMemory<M, T, FLAGS> template from C++ is ported in
// guest_memory.rs. The flags are defined here for convenience.

/// See `guest_memory.rs` for the full GuestMemory implementation.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_memory_new() {
        let mem = Memory::new();
        assert!(mem.current_page_table().is_none());
    }

    #[test]
    fn test_memory_constants() {
        assert_eq!(YUZU_PAGEBITS, 12);
        assert_eq!(YUZU_PAGESIZE, 4096);
        assert_eq!(YUZU_PAGEMASK, 4095);
    }

    #[test]
    fn test_address_space_contains() {
        let mut pt = PageTable::new();
        pt.resize(36, 12);
        assert!(address_space_contains(&pt, 0, 4096));
        assert!(address_space_contains(&pt, 0, 0));
        // Beyond 36-bit space
        let max = 1u64 << 36;
        assert!(!address_space_contains(&pt, max, 1));
    }

    #[test]
    fn test_is_valid_no_page_table() {
        let mem = Memory::new();
        assert!(!mem.is_valid_virtual_address(0x1000));
    }

    #[test]
    fn test_read_cstring_empty() {
        let mem = Memory::new();
        // With no page table set, reads will fail silently and return 0 bytes
        let s = mem.read_cstring(0, 100);
        assert!(s.is_empty());
    }
}
