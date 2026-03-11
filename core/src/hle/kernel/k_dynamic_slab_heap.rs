//! Port of zuyu/src/core/hle/kernel/k_dynamic_slab_heap.h
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! KDynamicSlabHeap — a slab heap that can dynamically expand by requesting
//! pages from a KDynamicPageManager.
//!
//! Mirrors `Kernel::KDynamicSlabHeap<T, ClearNode>`.
//! The page manager dependency is stubbed until KDynamicPageManager is ported.

use std::sync::atomic::{AtomicUsize, Ordering};

use super::k_slab_heap::KSlabHeapImpl;
use super::k_typed_address::KVirtualAddress;

/// KDynamicSlabHeap<T> — dynamically expandable slab heap.
///
/// Mirrors upstream `KDynamicSlabHeap<T, ClearNode>`.
/// The `ClearNode` template parameter controls whether allocated nodes have
/// their `next` pointer cleared.
pub struct KDynamicSlabHeap<T> {
    inner: KSlabHeapImpl,
    used: AtomicUsize,
    peak: AtomicUsize,
    count: AtomicUsize,
    address: KVirtualAddress,
    size: usize,
    clear_node: bool,
    /// Actual storage for dynamically allocated objects.
    storage: Vec<Option<T>>,
}

impl<T> KDynamicSlabHeap<T> {
    pub fn new(clear_node: bool) -> Self {
        Self {
            inner: KSlabHeapImpl::new(),
            used: AtomicUsize::new(0),
            peak: AtomicUsize::new(0),
            count: AtomicUsize::new(0),
            address: KVirtualAddress::new(0),
            size: 0,
            clear_node,
            storage: Vec::new(),
        }
    }

    pub fn get_address(&self) -> KVirtualAddress {
        self.address
    }

    pub fn get_size(&self) -> usize {
        self.size
    }

    pub fn get_used(&self) -> usize {
        self.used.load(Ordering::Relaxed)
    }

    pub fn get_peak(&self) -> usize {
        self.peak.load(Ordering::Relaxed)
    }

    pub fn get_count(&self) -> usize {
        self.count.load(Ordering::Relaxed)
    }

    /// Check if a virtual address falls within this heap's range.
    pub fn is_in_range(&self, addr: KVirtualAddress) -> bool {
        let start = self.address.get();
        let end = start + self.size as u64;
        addr.get() >= start && addr.get() < end
    }

    /// Initialize the dynamic slab heap.
    ///
    /// TODO: Requires KDynamicPageManager. Currently stubbed to initialize
    /// with a fixed number of objects.
    pub fn initialize_with_count(&mut self, num_objects: usize) {
        self.inner.initialize();
        self.storage.clear();
        self.storage.reserve(num_objects);
        for _ in 0..num_objects {
            self.storage.push(None);
        }
        self.count.store(num_objects, Ordering::Relaxed);
    }

    /// Allocate an object.
    ///
    /// TODO: The page_allocator parameter is stubbed until KDynamicPageManager is ported.
    pub fn allocate(&mut self) -> Option<usize>
    where
        T: Default,
    {
        let idx = self.inner.allocate();

        if let Some(idx) = idx {
            // Construct the object.
            self.storage[idx] = Some(T::default());

            // Update tracking.
            let used = self.used.fetch_add(1, Ordering::Relaxed) + 1;
            let mut peak = self.peak.load(Ordering::Relaxed);
            while peak < used {
                match self.peak.compare_exchange_weak(peak, used, Ordering::Relaxed, Ordering::Relaxed) {
                    Ok(_) => break,
                    Err(actual) => peak = actual,
                }
            }

            Some(idx)
        } else {
            None
        }
    }

    /// Free an object by index.
    pub fn free(&mut self, idx: usize) {
        self.storage[idx] = None;
        self.inner.free(idx);
        self.used.fetch_sub(1, Ordering::Relaxed);
    }

    /// Get a reference to the object at an index.
    pub fn get(&self, idx: usize) -> Option<&T> {
        self.storage.get(idx)?.as_ref()
    }

    /// Get a mutable reference to the object at an index.
    pub fn get_mut(&mut self, idx: usize) -> Option<&mut T> {
        self.storage.get_mut(idx)?.as_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dynamic_slab_heap_basic() {
        let mut heap: KDynamicSlabHeap<u64> = KDynamicSlabHeap::new(false);
        heap.initialize_with_count(4);
        assert_eq!(heap.get_count(), 4);
        assert_eq!(heap.get_used(), 0);
    }
}
