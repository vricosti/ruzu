//! Port of zuyu/src/core/hle/kernel/k_page_table_slab_heap.h
//! Status: Ported (page-allocator-backed; refcount table per entry).
//! Derniere synchro: 2026-05-04
//!
//! `KPageTableSlabHeap` exposes the upstream API where `Allocate`
//! returns a `KVirtualAddress` (not a typed pointer/handle) and
//! `Free(addr)` reclaims the entry. Internally the heap owns one
//! `Box<PageTablePage>` per slab slot, plus an `in_use` bitmap that
//! tracks which entries are currently checked out.
//!
//! Backed by `KDynamicPageManager` for page-level capacity tracking;
//! the page allocator is sized at boot and the slab fills from it.

use super::k_dynamic_page_manager::KDynamicPageManager;
use super::k_memory_block::PAGE_SIZE;
use std::sync::{Arc, Mutex};

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

struct Inner {
    /// All slab entries (always present). Indexed by `(addr - base) / PAGE_SIZE`.
    pages: Vec<Box<PageTablePage>>,
    /// Per-entry "checked out by `Allocate`" bitmap. False = entry is in
    /// the slab's free list and may be returned by the next `Allocate`.
    in_use: Vec<bool>,
    /// Per-entry refcount, indexed identically to `pages`.
    ref_counts: Vec<RefCount>,
    address: u64,
    size: usize,
    capacity: usize,
}

/// Slab heap of `PageTablePage` entries with companion refcount array.
///
/// Port of upstream `Kernel::KPageTableSlabHeap` — `KDynamicSlabHeap<PageTablePage,
/// ClearNode=true>` plus a refcount table indexed by entry position.
/// `Allocate` returns the entry's slab-region address (u64); `Free(addr)`
/// reclaims it. `Open(addr, count)` / `Close(addr, count)` adjust the
/// refcount; `Close` returns true when the count hits zero, signaling
/// the caller can `Free`.
pub struct KPageTableSlabHeap {
    page_allocator: Mutex<Option<Arc<Mutex<KDynamicPageManager>>>>,
    inner: Mutex<Inner>,
}

impl KPageTableSlabHeap {
    pub const PAGE_TABLE_SIZE: usize = PAGE_SIZE;

    pub fn new() -> Self {
        Self {
            page_allocator: Mutex::new(None),
            inner: Mutex::new(Inner {
                pages: Vec::new(),
                in_use: Vec::new(),
                ref_counts: Vec::new(),
                address: 0,
                size: 0,
                capacity: 0,
            }),
        }
    }

    /// Compute the bytes of refcount table needed to track `size` worth
    /// of page-table pages. Mirrors upstream's
    /// `CalculateReferenceCountSize(size_t size)` static helper used by
    /// the kernel boot init path to size the management region.
    pub fn calculate_reference_count_size(size: usize) -> usize {
        (size / PAGE_SIZE) * std::mem::size_of::<RefCount>()
    }

    /// Initialize the slab. The page allocator is sized in
    /// `num_pages * PAGE_SIZE`; the refcount table is sized to one
    /// `RefCount` slot per entry.
    ///
    /// Upstream:
    ///   `Initialize(KDynamicPageManager*, size_t object_count, RefCount* rc)`
    /// where `rc` is carved from the kernel management region. ruzu owns
    /// the refcount Vec inline since the slab is host-backed.
    pub fn initialize(&self, page_allocator: Arc<Mutex<KDynamicPageManager>>, num_pages: usize) {
        // Reserve `num_pages` from the page manager up-front so its
        // used-count reflects the slab's footprint. Upstream's
        // `KDynamicSlabHeap::Initialize` does the equivalent.
        let mut start_address: u64 = 0;
        {
            let mut pa = page_allocator.lock().unwrap();
            for i in 0..num_pages {
                match pa.allocate() {
                    Some(addr) => {
                        if i == 0 {
                            start_address = addr;
                        }
                    }
                    None => break,
                }
            }
        }
        let mut inner = self.inner.lock().unwrap();
        inner.pages = (0..num_pages)
            .map(|_| Box::new(PageTablePage::default()))
            .collect();
        inner.in_use = vec![false; num_pages];
        inner.ref_counts = vec![0; num_pages];
        inner.address = start_address;
        inner.size = num_pages * PAGE_SIZE;
        inner.capacity = num_pages;
        drop(inner);
        *self.page_allocator.lock().unwrap() = Some(page_allocator);
    }

    /// Allocate a page from the slab. Returns the entry's address (u64),
    /// or `None` on exhaustion. Mirrors upstream's
    ///   `KVirtualAddress Allocate()`
    /// which returns 0 on failure; ruzu returns `None` for the failure
    /// case and a non-zero address on success.
    pub fn allocate(&self) -> Option<u64> {
        let mut inner = self.inner.lock().unwrap();
        let cap = inner.capacity;
        let base = inner.address;
        for idx in 0..cap {
            if !inner.in_use[idx] {
                inner.in_use[idx] = true;
                return Some(base + (idx * PAGE_SIZE) as u64);
            }
        }
        None
    }

    /// Free a page-table page back to the slab. Upstream:
    ///   `void Free(KVirtualAddress addr)`.
    pub fn free(&self, addr: u64) {
        let mut inner = self.inner.lock().unwrap();
        let Some(idx) = self.addr_to_index_locked(&inner, addr) else {
            log::warn!(
                "KPageTableSlabHeap::free: addr {:#x} not in slab range",
                addr
            );
            return;
        };
        if !inner.in_use[idx] {
            log::warn!("KPageTableSlabHeap::free: double free of addr {:#x}", addr);
            return;
        }
        // Reset the entry contents so reused pages don't carry stale
        // state — equivalent to upstream's `ClearNode=true` on the
        // KDynamicSlabHeap template parameter.
        *inner.pages[idx] = PageTablePage::default();
        inner.in_use[idx] = false;
    }

    pub fn get_address(&self) -> u64 {
        self.inner.lock().unwrap().address
    }

    pub fn get_size(&self) -> usize {
        self.inner.lock().unwrap().size
    }

    pub fn get_capacity(&self) -> usize {
        self.inner.lock().unwrap().capacity
    }

    /// Address-range membership test. Returns true for any address that
    /// originally came from `Allocate`.
    pub fn is_in_range(&self, addr: u64) -> bool {
        let inner = self.inner.lock().unwrap();
        if inner.size == 0 {
            return false;
        }
        addr >= inner.address && addr < inner.address + inner.size as u64
    }

    /// Refcount accessor. Upstream: `GetRefCount(addr)` — asserts
    /// `IsInRange(addr)` and returns the entry's refcount.
    pub fn get_ref_count(&self, addr: u64) -> RefCount {
        let inner = self.inner.lock().unwrap();
        let Some(idx) = self.addr_to_index_locked(&inner, addr) else {
            return 0;
        };
        inner.ref_counts[idx]
    }

    /// Increment the refcount for `addr` by `count`.
    pub fn open(&self, addr: u64, count: i32) {
        let mut inner = self.inner.lock().unwrap();
        let Some(idx) = self.addr_to_index_locked(&inner, addr) else {
            return;
        };
        inner.ref_counts[idx] = inner.ref_counts[idx].wrapping_add(count as RefCount);
        debug_assert!(inner.ref_counts[idx] > 0);
    }

    /// Decrement the refcount for `addr` by `count`. Returns true when
    /// the refcount hits zero — caller is expected to free the page.
    pub fn close(&self, addr: u64, count: i32) -> bool {
        let mut inner = self.inner.lock().unwrap();
        let Some(idx) = self.addr_to_index_locked(&inner, addr) else {
            return false;
        };
        debug_assert!(inner.ref_counts[idx] >= count as RefCount);
        inner.ref_counts[idx] = inner.ref_counts[idx].wrapping_sub(count as RefCount);
        inner.ref_counts[idx] == 0
    }

    fn addr_to_index_locked(&self, inner: &Inner, addr: u64) -> Option<usize> {
        if inner.size == 0 {
            return None;
        }
        if addr < inner.address || addr >= inner.address + inner.size as u64 {
            return None;
        }
        Some(((addr - inner.address) / PAGE_SIZE as u64) as usize)
    }
}

impl Default for KPageTableSlabHeap {
    fn default() -> Self {
        Self::new()
    }
}
