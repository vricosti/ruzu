//! Port of zuyu/src/common/page_table.h and zuyu/src/common/page_table.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use crate::virtual_buffer::VirtualBuffer;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Page type attribute, describing the mapping type of a page.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PageType {
    /// Page is unmapped and should cause an access error.
    Unmapped = 0,
    /// Page is mapped to regular memory. This is the only type you can get pointers to.
    Memory = 1,
    /// Page is mapped to regular memory, but inaccessible from CPU fastmem and must use
    /// the callbacks.
    DebugMemory = 2,
    /// Page is mapped to regular memory, but also needs to check for rasterizer cache flushing
    /// and invalidation.
    RasterizerCachedMemory = 3,
}

impl PageType {
    fn from_raw(val: usize) -> Self {
        match val & ((1 << PageInfo::ATTRIBUTE_BITS) - 1) {
            0 => PageType::Unmapped,
            1 => PageType::Memory,
            2 => PageType::DebugMemory,
            3 => PageType::RasterizerCachedMemory,
            _ => PageType::Unmapped,
        }
    }
}

/// Pair of host pointer and page type attribute.
/// This uses the lower bits of a given pointer to store the attribute tag.
/// Writing and reading the pointer attribute pair is guaranteed to be atomic for the same method
/// call.
#[repr(transparent)]
pub struct PageInfo {
    raw: AtomicUsize,
}

impl PageInfo {
    /// Number of bits reserved for attribute tagging.
    pub const ATTRIBUTE_BITS: usize = 2;

    /// Returns the page pointer.
    pub fn pointer(&self) -> usize {
        Self::extract_pointer(self.raw.load(Ordering::Relaxed))
    }

    /// Returns the page type attribute.
    pub fn page_type(&self) -> PageType {
        Self::extract_type(self.raw.load(Ordering::Relaxed))
    }

    /// Returns the page pointer and attribute pair, extracted from the same atomic read.
    pub fn pointer_type(&self) -> (usize, PageType) {
        let raw = self.raw.load(Ordering::Relaxed);
        (Self::extract_pointer(raw), Self::extract_type(raw))
    }

    /// Returns the raw representation of the page information.
    pub fn raw_value(&self) -> usize {
        self.raw.load(Ordering::Relaxed)
    }

    /// Write a page pointer and type pair atomically.
    pub fn store(&self, pointer: usize, page_type: PageType) {
        self.raw
            .store(pointer | page_type as usize, Ordering::Relaxed);
    }

    /// Unpack a pointer from a page info raw representation.
    pub fn extract_pointer(raw: usize) -> usize {
        raw & (!0usize << Self::ATTRIBUTE_BITS)
    }

    /// Unpack a page type from a page info raw representation.
    pub fn extract_type(raw: usize) -> PageType {
        PageType::from_raw(raw)
    }
}

/// Traversal entry returned by page table traversal.
pub struct TraversalEntry {
    pub phys_addr: u64,
    pub block_size: usize,
}

/// Context for continuing page table traversal.
pub struct TraversalContext {
    pub next_page: u64,
    pub next_offset: u64,
}

/// A (reasonably) fast way of allowing switchable and remappable process address spaces.
/// It loosely mimics the way a real CPU page table works.
pub struct PageTable {
    /// Vector of memory pointers backing each page. An entry can only be non-null if the
    /// corresponding attribute element is of type `Memory`.
    pub pointers: VirtualBuffer<PageInfo>,
    pub blocks: VirtualBuffer<u64>,
    pub backing_addr: VirtualBuffer<u64>,

    pub current_address_space_width_in_bits: usize,
    pub fastmem_arena: *mut u8,
    pub page_size: usize,
}

impl PageTable {
    pub fn new() -> Self {
        Self {
            pointers: VirtualBuffer::new(),
            blocks: VirtualBuffer::new(),
            backing_addr: VirtualBuffer::new(),
            current_address_space_width_in_bits: 0,
            fastmem_arena: std::ptr::null_mut(),
            page_size: 0,
        }
    }

    /// Begin traversal from the given virtual address.
    pub fn begin_traversal(&self, address: u64) -> Option<(TraversalEntry, TraversalContext)> {
        let mut context = TraversalContext {
            next_offset: address,
            next_page: address / self.page_size as u64,
        };
        let entry = self.continue_traversal(&mut context)?;
        Some((entry, context))
    }

    /// Continue traversal from the given context.
    pub fn continue_traversal(&self, context: &mut TraversalContext) -> Option<TraversalEntry> {
        let mut entry = TraversalEntry {
            phys_addr: 0,
            block_size: self.page_size,
        };

        let page = context.next_page as usize;

        // Advance on exit (equivalent to SCOPE_EXIT in C++)
        let next_page = context.next_page;
        let next_offset = context.next_offset;
        context.next_page = next_page + 1;
        context.next_offset = next_offset + self.page_size as u64;

        // Validate that we can read the actual entry
        if page >= self.backing_addr.size() {
            return None;
        }

        // Validate that the entry is mapped
        let phys_addr = self.backing_addr[page];
        if phys_addr == 0 {
            return None;
        }

        // Populate the results
        entry.phys_addr = phys_addr + next_offset;

        Some(entry)
    }

    /// Resizes the page table to be able to accommodate enough pages within
    /// a given address space.
    pub fn resize(&mut self, address_space_width_in_bits: usize, page_size_in_bits: usize) {
        let num_page_table_entries: usize = 1 << (address_space_width_in_bits - page_size_in_bits);
        self.pointers.resize(num_page_table_entries);
        self.backing_addr.resize(num_page_table_entries);
        self.blocks.resize(num_page_table_entries);
        self.current_address_space_width_in_bits = address_space_width_in_bits;
        self.page_size = 1 << page_size_in_bits;
    }

    /// Get the address space width in bits.
    pub fn get_address_space_bits(&self) -> usize {
        self.current_address_space_width_in_bits
    }

    /// Map a range of pages to a physical address with a given page type.
    /// Port of upstream `Memory::Impl::MapPages`.
    ///
    /// - `base_page`: starting page index (not byte address)
    /// - `num_pages`: number of pages to map
    /// - `target`: physical address to map to (0 for unmap)
    /// - `page_type`: the type to set for these pages
    /// Map a range of pages to a physical address with a given page type.
    /// Port of upstream `Memory::Impl::MapPages`.
    ///
    /// - `base_page`: starting page index (not byte address)
    /// - `num_pages`: number of pages to map
    /// - `target`: physical address to map to (0 for unmap)
    /// - `page_type`: the type to set for these pages
    /// - `host_ptr`: host pointer base for Memory-type mappings
    pub fn map_pages(
        &mut self,
        base_page: usize,
        num_pages: usize,
        target: u64,
        page_type: PageType,
        host_ptr: usize,
    ) {
        let end = base_page + num_pages;
        assert!(
            end <= self.pointers.size(),
            "out of range mapping at {:016X}",
            base_page
        );

        if target == 0 {
            assert!(
                page_type != PageType::Memory,
                "Mapping memory page without a pointer @ {:016x}",
                base_page * self.page_size
            );
            for page in base_page..end {
                self.pointers[page].store(0, page_type);
                self.backing_addr[page] = 0;
                self.blocks[page] = 0;
            }
        } else {
            for (i, page) in (base_page..end).enumerate() {
                let offset_ptr = host_ptr.wrapping_sub(page * self.page_size);
                self.pointers[page].store(offset_ptr, page_type);
                self.backing_addr[page] = target;
                self.blocks[page] = target + (i * self.page_size) as u64;
            }
        }
    }

    /// Unmap a range of pages, setting them to Unmapped type.
    /// Port of upstream `Memory::Impl::UnmapRegion` (page table portion).
    ///
    /// - `base_address`: byte address to start unmapping
    /// - `size`: number of bytes to unmap
    pub fn unmap_region(&mut self, base_address: u64, size: u64) {
        assert!(
            size & (self.page_size as u64 - 1) == 0,
            "non-page aligned size: {:016X}",
            size
        );
        assert!(
            base_address & (self.page_size as u64 - 1) == 0,
            "non-page aligned base: {:016X}",
            base_address
        );
        let base_page = (base_address / self.page_size as u64) as usize;
        let num_pages = (size / self.page_size as u64) as usize;
        self.map_pages(base_page, num_pages, 0, PageType::Unmapped, 0);
    }

    /// Get the physical address for a virtual address.
    pub fn get_physical_address(&self, virt_addr: u64) -> Option<u64> {
        if virt_addr > (1u64 << self.get_address_space_bits()) {
            return None;
        }
        let page_index = (virt_addr / self.page_size as u64) as usize;
        if page_index >= self.backing_addr.size() {
            return None;
        }
        Some(self.backing_addr[page_index] + virt_addr)
    }
}

impl Default for PageTable {
    fn default() -> Self {
        Self::new()
    }
}

// Safety: PageTable uses VirtualBuffer which is Send+Sync, and raw pointer fastmem_arena
// is managed carefully.
unsafe impl Send for PageTable {}
unsafe impl Sync for PageTable {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_page_table_resize() {
        let mut pt = PageTable::new();
        pt.resize(36, 12); // 36-bit address space, 4KB pages
        assert_eq!(pt.get_address_space_bits(), 36);
        assert_eq!(pt.page_size, 4096);
        assert_eq!(pt.pointers.size(), 1 << 24);
    }

    #[test]
    fn test_page_type_extract() {
        let raw = 0x1000usize | PageType::Memory as usize;
        assert_eq!(PageInfo::extract_pointer(raw), 0x1000);
        assert_eq!(PageInfo::extract_type(raw), PageType::Memory);
    }
}
