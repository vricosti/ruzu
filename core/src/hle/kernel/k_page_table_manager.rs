//! Port of zuyu/src/core/hle/kernel/k_page_table_manager.h
//! Status: Ported (structural — slab + refcount layered properly).
//! Derniere synchro: 2026-05-04
//!
//! `KPageTableManager` is the kernel-side allocator for guest-physical
//! pages used as page-table backing (L1/L2/L3 entries). Upstream:
//!
//! ```cpp
//! class KPageTableManager
//!     : public KDynamicResourceManager<impl::PageTablePage, true> {
//!     KPageTableSlabHeap* m_pt_heap{};
//! };
//! ```
//!
//! ruzu's page table is a flat host-side `common::page_table::PageTable`
//! rather than a multi-level guest-physical structure, so no `Operate`
//! actually pushes pages onto the `PageLinkedList` for `FinalizeUpdate`
//! to free. The manager is still ported so that:
//!
//! * the API shape matches upstream (callers can hold a reference and
//!   route freed pages through `Free` without conditional compilation),
//! * a future port of multi-level guest page tables drops in without
//!   touching every caller.

use super::k_page_table_slab_heap::{KPageTableSlabHeap, PageTablePage, RefCount};

/// `KPageTableManager` — typed wrapper around `KPageTableSlabHeap`.
///
/// Mirrors upstream's `KDynamicResourceManager<PageTablePage, true>`
/// inheritance with a `KPageTableSlabHeap*` reserved member. ruzu uses
/// composition (`Arc<KPageTableSlabHeap>`) since the slab heap owns a
/// `Mutex` for its free list.
pub struct KPageTableManager {
    slab_heap: std::sync::Arc<KPageTableSlabHeap>,
}

impl KPageTableManager {
    pub const PAGE_TABLE_SIZE: usize = KPageTableSlabHeap::PAGE_TABLE_SIZE;

    /// Construct a manager backed by an existing slab heap. Upstream's
    /// `KPageTableManager()` is default-constructible; `Initialize` then
    /// sets the slab pointer. ruzu folds those two phases into the
    /// constructor so the manager is always in a usable state.
    pub fn new(slab_heap: std::sync::Arc<KPageTableSlabHeap>) -> Self {
        Self { slab_heap }
    }

    /// Allocate a page-table page. Upstream returns `KVirtualAddress`
    /// pointing into the slab; ruzu returns the boxed page. `None` on
    /// slab exhaustion.
    pub fn allocate(&self) -> Option<Box<PageTablePage>> {
        self.slab_heap.allocate()
    }

    /// Free a page-table page back to the slab. Upstream's
    /// `void Free(T* t)`.
    pub fn free(&self, page: Box<PageTablePage>) {
        self.slab_heap.free(page);
    }

    /// Refcount accessor. Upstream's `RefCount GetRefCount(KVirtualAddress)`.
    pub fn get_ref_count(&self, addr: u64) -> RefCount {
        self.slab_heap.get_ref_count(addr)
    }

    /// Increment the refcount for `addr`. Upstream's
    /// `Open(KVirtualAddress, int count)`.
    pub fn open(&self, addr: u64, count: i32) {
        self.slab_heap.open(addr, count);
    }

    /// Decrement the refcount and return true if it hit zero. Upstream's
    /// `bool Close(KVirtualAddress, int count)`.
    pub fn close(&self, addr: u64, count: i32) -> bool {
        self.slab_heap.close(addr, count)
    }

    /// Membership test for the `FinalizeUpdate` debug assertion path —
    /// upstream's `IsInPageTableHeap(addr)` ensures only slab-owned
    /// addresses are returned via `Free`.
    pub fn is_in_page_table_heap(&self, addr: u64) -> bool {
        self.slab_heap.is_in_range(addr)
    }

    /// Total slab capacity (bytes). Mirrors upstream's `GetSize()` from
    /// the inherited `KDynamicResourceManager`.
    pub fn get_size(&self) -> usize {
        self.slab_heap.get_size()
    }

    /// Number of pre-allocated entries.
    pub fn get_count(&self) -> usize {
        self.slab_heap.get_capacity()
    }
}
