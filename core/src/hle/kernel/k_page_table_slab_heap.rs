//! Port of zuyu/src/core/hle/kernel/k_page_table_slab_heap.h
//! Status: Ported (free-list backed; refcount table per entry).
//! Derniere synchro: 2026-05-04

use super::k_memory_block::PAGE_SIZE;
use std::sync::Mutex;

/// Port of impl::PageTablePage — a page-sized buffer used as the storage
/// for one level of guest page-table entries. Upstream skips constructor
/// init to leave the page contents undefined; ruzu zero-initializes for
/// safety since the slab grows lazily.
#[repr(C, align(4096))]
#[derive(Clone)]
pub struct PageTablePage {
    pub m_buffer: [u8; PAGE_SIZE],
}

impl PageTablePage {
    pub fn new() -> Self {
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
/// Upstream: `using RefCount = u16;`.
pub type RefCount = u16;

/// Slab heap of `PageTablePage` entries with companion refcount array.
///
/// Port of upstream `Kernel::KPageTableSlabHeap` — a
/// `KDynamicSlabHeap<PageTablePage, ClearNode=true>` plus an
/// `m_ref_counts` Vec indexed by entry. `Open(addr, count)` increments,
/// `Close(addr, count)` decrements and returns true when the count hits
/// zero so the caller can free the page.
///
/// ruzu's flat-page-table model never produces freeable page-table pages
/// (no L1/L2/L3 levels in `common::page_table::PageTable`), so the
/// allocate/free path stays cold in practice. The structural port is
/// kept so when multi-level guest page tables are added the wiring is
/// already in place.
pub struct KPageTableSlabHeap {
    free_list: Mutex<Vec<Box<PageTablePage>>>,
    ref_counts: Mutex<Vec<RefCount>>,
    /// Backing-region base address. Upstream sets this to the kernel
    /// virtual address handed to `Initialize` from `KDynamicPageManager`.
    /// ruzu doesn't have a dedicated kernel-VA region for the slab so
    /// `address` stays 0; `is_in_range` and refcount lookups use entry
    /// indices instead of an addr→index translation.
    address: u64,
    /// Total slab capacity in bytes (= `capacity * PAGE_SIZE`).
    size: usize,
    capacity: usize,
}

impl KPageTableSlabHeap {
    pub const PAGE_TABLE_SIZE: usize = PAGE_SIZE;

    pub fn new() -> Self {
        Self {
            free_list: Mutex::new(Vec::new()),
            ref_counts: Mutex::new(Vec::new()),
            address: 0,
            size: 0,
            capacity: 0,
        }
    }

    /// Compute the bytes of refcount table needed to track `size` worth
    /// of page-table pages. Mirrors upstream's
    /// `CalculateReferenceCountSize(size_t size)` static helper used by
    /// the kernel boot init path to size the management region.
    pub fn calculate_reference_count_size(size: usize) -> usize {
        (size / PAGE_SIZE) * std::mem::size_of::<RefCount>()
    }

    /// Initialize the slab with `object_count` pre-allocated entries plus
    /// a refcount slot per entry. Upstream:
    ///   `Initialize(KDynamicPageManager*, object_count, RefCount* rc)`
    /// with the refcount buffer carved from the kernel management region.
    pub fn initialize(&mut self, object_count: usize) {
        let mut list = self.free_list.lock().unwrap();
        list.clear();
        list.reserve(object_count);
        for _ in 0..object_count {
            list.push(Box::new(PageTablePage::default()));
        }
        drop(list);
        let mut rc = self.ref_counts.lock().unwrap();
        rc.clear();
        rc.resize(object_count, 0);
        drop(rc);
        self.capacity = object_count;
        self.size = object_count * PAGE_SIZE;
    }

    /// Pop one entry from the free list. Returns `None` on exhaustion to
    /// match upstream's `Allocate` returning null when the slab can't
    /// satisfy a request.
    pub fn allocate(&self) -> Option<Box<PageTablePage>> {
        self.free_list.lock().unwrap().pop()
    }

    /// Return an entry to the slab. Upstream's `Free(KVirtualAddress)`.
    pub fn free(&self, page: Box<PageTablePage>) {
        self.free_list.lock().unwrap().push(page);
    }

    pub fn get_address(&self) -> u64 {
        self.address
    }

    pub fn get_size(&self) -> usize {
        self.size
    }

    pub fn get_capacity(&self) -> usize {
        self.capacity
    }

    /// Address-based membership test. ruzu's slab isn't anchored to a
    /// real kernel VA range, so this returns false until multi-level
    /// page tables are ported.
    pub fn is_in_range(&self, addr: u64) -> bool {
        if self.size == 0 || self.address == 0 {
            return false;
        }
        addr >= self.address && addr < self.address + self.size as u64
    }

    /// Read the refcount for the entry at `addr`. ruzu's flat page table
    /// never produces a real refcount-tracked page, so this returns 0
    /// for all addresses (matching the upstream contract that hit-zero
    /// means "free the page").
    pub fn get_ref_count(&self, addr: u64) -> RefCount {
        if !self.is_in_range(addr) {
            return 0;
        }
        let index = ((addr - self.address) / PAGE_SIZE as u64) as usize;
        self.ref_counts.lock().unwrap()[index]
    }

    /// Increment the refcount for `addr` by `count`. No-op when the
    /// address isn't in the slab (matches upstream's `is_in_range`
    /// debug-assertion path which becomes a no-op in release builds).
    pub fn open(&self, addr: u64, count: i32) {
        if !self.is_in_range(addr) {
            return;
        }
        let index = ((addr - self.address) / PAGE_SIZE as u64) as usize;
        let mut rc = self.ref_counts.lock().unwrap();
        rc[index] = rc[index].wrapping_add(count as RefCount);
        debug_assert!(rc[index] > 0);
    }

    /// Decrement the refcount for `addr` by `count`. Returns true when
    /// the refcount hits zero — caller is expected to free the page back
    /// to the slab via [`KPageTableManager::free`].
    pub fn close(&self, addr: u64, count: i32) -> bool {
        if !self.is_in_range(addr) {
            return false;
        }
        let index = ((addr - self.address) / PAGE_SIZE as u64) as usize;
        let mut rc = self.ref_counts.lock().unwrap();
        debug_assert!(rc[index] >= count as RefCount);
        rc[index] = rc[index].wrapping_sub(count as RefCount);
        rc[index] == 0
    }
}

impl Default for KPageTableSlabHeap {
    fn default() -> Self {
        Self::new()
    }
}
