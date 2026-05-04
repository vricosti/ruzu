//! Port of zuyu/src/core/hle/kernel/k_dynamic_slab_heap.h
//! Status: Ported (page-allocator-backed, lazy growth).
//! Derniere synchro: 2026-05-04
//!
//! Mirrors `Kernel::KDynamicSlabHeap<T, ClearNode>`. Carves typed entries
//! from pages drawn out of an attached `KDynamicPageManager`, with a
//! free-list of `Box<T>` per entry. Allocate pops; on exhaustion, pulls
//! one more page from the manager and refills the free list.
//!
//! Storage backing: pages reserved from the manager are accounted for
//! (the manager's used-count rises); the typed entries themselves are
//! `Box<T>` rather than carved-from-the-page-bytes raw pointers, which
//! keeps Rust's ownership rules clean. Functional behavior matches
//! upstream — allocation can fail when both the slab free list and the
//! page manager are exhausted, and capacity grows in PAGE_SIZE/sizeof(T)
//! increments.

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use super::k_dynamic_page_manager::KDynamicPageManager;
use super::k_memory_block::PAGE_SIZE;

/// `KDynamicSlabHeap<T>` — page-backed dynamically-expanding slab heap.
///
/// Mirrors upstream's `KDynamicSlabHeap<T, ClearNode>`. The `ClearNode`
/// template parameter is honored as a runtime flag (`clear_node`) which
/// resets entries on Free.
pub struct KDynamicSlabHeap<T: Default> {
    page_allocator: Mutex<Option<Arc<Mutex<KDynamicPageManager>>>>,
    free_list: Mutex<Vec<Box<T>>>,
    capacity: AtomicUsize,
    used: AtomicUsize,
    peak: AtomicUsize,
    address: AtomicUsize,
    size: AtomicUsize,
    clear_node: bool,
}

impl<T: Default> KDynamicSlabHeap<T> {
    /// Number of typed entries that fit in one PAGE-sized slab page.
    /// At least 1; if `sizeof(T) > PAGE_SIZE` we still allocate one
    /// entry per page (which would over-consume but never fail to make
    /// progress).
    pub const fn entries_per_page() -> usize {
        let s = std::mem::size_of::<T>();
        if s == 0 {
            // ZST: pretend 1 per page so capacity tracking is sane.
            1
        } else if s >= PAGE_SIZE {
            1
        } else {
            PAGE_SIZE / s
        }
    }

    pub fn new(clear_node: bool) -> Self {
        Self {
            page_allocator: Mutex::new(None),
            free_list: Mutex::new(Vec::new()),
            capacity: AtomicUsize::new(0),
            used: AtomicUsize::new(0),
            peak: AtomicUsize::new(0),
            address: AtomicUsize::new(0),
            size: AtomicUsize::new(0),
            clear_node,
        }
    }

    /// Attach a page manager and pre-reserve `num_pages` worth of entries.
    /// Mirrors upstream's
    /// `Initialize(KDynamicPageManager* page_allocator, size_t object_count)`
    /// where `object_count` is converted to `num_pages = ceil(object_count
    /// * sizeof(T) / PageSize)` internally.
    pub fn initialize_with_pages(
        &self,
        page_allocator: Arc<Mutex<KDynamicPageManager>>,
        num_pages: usize,
    ) {
        // Reserve the pages from the manager up-front so its used-count
        // reflects the slab's footprint.
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
        self.address.store(start_address as usize, Ordering::Relaxed);
        self.size
            .store(num_pages * PAGE_SIZE, Ordering::Relaxed);

        // Carve into typed entries and seed the free list.
        let entries_per_page = Self::entries_per_page();
        let total = num_pages * entries_per_page;
        let mut list = self.free_list.lock().unwrap();
        list.reserve(total);
        for _ in 0..total {
            list.push(Box::new(T::default()));
        }
        drop(list);
        self.capacity.store(total, Ordering::Relaxed);

        *self.page_allocator.lock().unwrap() = Some(page_allocator);
    }

    /// Convenience initializer when no page manager is attached — used
    /// by paths that just want a fixed-capacity slab without lazy growth.
    pub fn initialize_with_count(&self, num_objects: usize) {
        let mut list = self.free_list.lock().unwrap();
        list.reserve(num_objects);
        for _ in 0..num_objects {
            list.push(Box::new(T::default()));
        }
        drop(list);
        self.capacity.store(num_objects, Ordering::Relaxed);
    }

    pub fn get_address(&self) -> u64 {
        self.address.load(Ordering::Relaxed) as u64
    }
    pub fn get_size(&self) -> usize {
        self.size.load(Ordering::Relaxed)
    }
    pub fn get_used(&self) -> usize {
        self.used.load(Ordering::Relaxed)
    }
    pub fn get_peak(&self) -> usize {
        self.peak.load(Ordering::Relaxed)
    }
    pub fn get_count(&self) -> usize {
        self.capacity.load(Ordering::Relaxed)
    }

    pub fn is_in_range(&self, addr: u64) -> bool {
        let start = self.get_address();
        let end = start.wrapping_add(self.get_size() as u64);
        addr >= start && addr < end
    }

    /// Allocate one entry. On exhaustion, pull one more page from the
    /// attached page manager and refill the free list. Returns `None`
    /// only when both the free list and the page manager are dry.
    pub fn allocate(&self) -> Option<Box<T>> {
        // Fast path: free list non-empty.
        {
            let mut list = self.free_list.lock().unwrap();
            if let Some(item) = list.pop() {
                drop(list);
                self.bump_used();
                return Some(item);
            }
        }
        // Slow path: try to grow by one page.
        {
            let pa_opt = self.page_allocator.lock().unwrap();
            if let Some(pa) = pa_opt.as_ref() {
                let mut pa = pa.lock().unwrap();
                if pa.allocate().is_none() {
                    return None;
                }
            } else {
                return None;
            }
        }
        let entries_per_page = Self::entries_per_page();
        let mut list = self.free_list.lock().unwrap();
        for _ in 0..(entries_per_page - 1) {
            list.push(Box::new(T::default()));
        }
        let item = Box::new(T::default());
        drop(list);
        self.capacity
            .fetch_add(entries_per_page, Ordering::Relaxed);
        self.size
            .fetch_add(PAGE_SIZE, Ordering::Relaxed);
        self.bump_used();
        Some(item)
    }

    /// Return an entry to the free list. Resets `*item` per
    /// upstream's `ClearNode=true` template parameter when set.
    pub fn free(&self, mut item: Box<T>) {
        if self.clear_node {
            *item = T::default();
        }
        self.free_list.lock().unwrap().push(item);
        self.used.fetch_sub(1, Ordering::Relaxed);
    }

    fn bump_used(&self) {
        let new = self.used.fetch_add(1, Ordering::Relaxed) + 1;
        let mut peak = self.peak.load(Ordering::Relaxed);
        while peak < new {
            match self.peak.compare_exchange_weak(
                peak,
                new,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(actual) => peak = actual,
            }
        }
    }
}

impl<T: Default> Default for KDynamicSlabHeap<T> {
    fn default() -> Self {
        Self::new(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slab_with_count_pre_populates_free_list() {
        let heap: KDynamicSlabHeap<u64> = KDynamicSlabHeap::new(false);
        heap.initialize_with_count(4);
        assert_eq!(heap.get_count(), 4);
        let a = heap.allocate().expect("free list pre-populated");
        let _b = heap.allocate().expect("second allocation");
        heap.free(a);
        assert_eq!(heap.get_used(), 1);
    }

    #[test]
    fn slab_grows_via_page_manager() {
        // Reserve 1 page worth of entries up front; then exhaust and
        // ensure a second page can grow the slab.
        let pa = Arc::new(Mutex::new(KDynamicPageManager::new()));
        // Initialize with 4 pages of backing memory at fake addr 0x10000.
        pa.lock()
            .unwrap()
            .initialize(0x10000, 4 * PAGE_SIZE, PAGE_SIZE)
            .unwrap();
        let heap: KDynamicSlabHeap<u64> = KDynamicSlabHeap::new(false);
        heap.initialize_with_pages(Arc::clone(&pa), 1);
        let entries_per_page = KDynamicSlabHeap::<u64>::entries_per_page();
        // Drain initial page.
        let mut held = Vec::new();
        for _ in 0..entries_per_page {
            held.push(heap.allocate().unwrap());
        }
        // Next allocate should pull a new page.
        let _ = heap.allocate().expect("growth via page manager");
        assert!(heap.get_count() > entries_per_page);
    }
}
