//! Port of zuyu/src/core/hle/kernel/k_page_table_slab_heap.h
//! Status: Stubbed
//! Derniere synchro: 2026-03-11

use super::k_memory_block::PAGE_SIZE;

/// Port of impl::PageTablePage — a page-sized buffer.
#[repr(C)]
pub struct PageTablePage {
    m_buffer: [u8; PAGE_SIZE],
}

impl PageTablePage {
    pub fn new() -> Self {
        // Intentionally uninitialized semantics — we zero it here for safety.
        Self {
            m_buffer: [0u8; PAGE_SIZE],
        }
    }
}

impl Default for PageTablePage {
    fn default() -> Self {
        Self::new()
    }
}

const _: () = assert!(std::mem::size_of::<PageTablePage>() == PAGE_SIZE);

/// Reference count type for page table slab heap.
pub type RefCount = u16;

/// Port of Kernel::KPageTableSlabHeap.
///
/// Stubbed: depends on KDynamicSlabHeap infrastructure.
pub struct KPageTableSlabHeap {
    ref_counts: Vec<RefCount>,
    address: u64,
    size: usize,
}

impl KPageTableSlabHeap {
    pub const PAGE_TABLE_SIZE: usize = PAGE_SIZE;

    pub fn new() -> Self {
        Self {
            ref_counts: Vec::new(),
            address: 0,
            size: 0,
        }
    }

    pub fn calculate_reference_count_size(size: usize) -> usize {
        (size / PAGE_SIZE) * std::mem::size_of::<RefCount>()
    }

    pub fn get_address(&self) -> u64 {
        self.address
    }

    pub fn get_size(&self) -> usize {
        self.size
    }

    pub fn is_in_range(&self, addr: u64) -> bool {
        addr >= self.address && addr < self.address + self.size as u64
    }

    pub fn get_ref_count(&self, addr: u64) -> RefCount {
        debug_assert!(self.is_in_range(addr));
        let index = ((addr - self.address) / PAGE_SIZE as u64) as usize;
        self.ref_counts[index]
    }

    pub fn open(&mut self, addr: u64, count: i32) {
        debug_assert!(self.is_in_range(addr));
        let index = ((addr - self.address) / PAGE_SIZE as u64) as usize;
        self.ref_counts[index] += count as RefCount;
        debug_assert!(self.ref_counts[index] > 0);
    }

    pub fn close(&mut self, addr: u64, count: i32) -> bool {
        debug_assert!(self.is_in_range(addr));
        let index = ((addr - self.address) / PAGE_SIZE as u64) as usize;
        debug_assert!(self.ref_counts[index] >= count as RefCount);
        self.ref_counts[index] -= count as RefCount;
        self.ref_counts[index] == 0
    }
}

impl Default for KPageTableSlabHeap {
    fn default() -> Self {
        Self::new()
    }
}
