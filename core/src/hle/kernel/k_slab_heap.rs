//! Port of zuyu/src/core/hle/kernel/k_slab_heap.h
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! Template slab allocator for kernel objects.
//! Mirrors the C++ KSlabHeapImpl, KSlabHeapBase, and KSlabHeap classes.
//!
//! The C++ version uses raw pointers and placement new with a spinlock-protected
//! free list. In Rust, we use a Vec-backed pool with index-based free list.

use std::sync::Mutex;

/// Node in the free list. Mirrors `impl::KSlabHeapImpl::Node`.
struct Node {
    next: Option<usize>, // index into the pool, None = end of list
}

/// KSlabHeapImpl — the base slab allocator using a lock-protected free list.
/// Mirrors `Kernel::impl::KSlabHeapImpl`.
pub struct KSlabHeapImpl {
    head: Option<usize>,
    lock: Mutex<()>,
    /// Storage for the nodes (indices).
    nodes: Vec<Node>,
}

impl KSlabHeapImpl {
    pub fn new() -> Self {
        Self {
            head: None,
            lock: Mutex::new(()),
            nodes: Vec::new(),
        }
    }

    pub fn initialize(&mut self) {
        debug_assert!(self.head.is_none());
    }

    /// Allocate a slot from the free list. Returns the index, or None if empty.
    pub fn allocate(&mut self) -> Option<usize> {
        let _guard = self.lock.lock().unwrap();
        if let Some(idx) = self.head {
            self.head = self.nodes[idx].next;
            Some(idx)
        } else {
            None
        }
    }

    /// Return a slot to the free list by index.
    pub fn free(&mut self, idx: usize) {
        let _guard = self.lock.lock().unwrap();
        self.nodes[idx].next = self.head;
        self.head = Some(idx);
    }

    /// Get the current head index.
    pub fn get_head(&self) -> Option<usize> {
        self.head
    }
}

/// KSlabHeapBase — extends KSlabHeapImpl with object size tracking and range checks.
///
/// Mirrors `Kernel::KSlabHeapBase<SupportDynamicExpansion>`.
/// The `SupportDynamicExpansion` bool template parameter affects GetObjectIndex behavior.
pub struct KSlabHeapBase {
    pub inner: KSlabHeapImpl,
    obj_size: usize,
    capacity: usize,
    support_dynamic_expansion: bool,
}

impl KSlabHeapBase {
    pub fn new(support_dynamic_expansion: bool) -> Self {
        Self {
            inner: KSlabHeapImpl::new(),
            obj_size: 0,
            capacity: 0,
            support_dynamic_expansion,
        }
    }

    /// Initialize the slab heap with a given object size and capacity.
    /// Mirrors upstream `Initialize(size_t obj_size, void* memory, size_t memory_size)`.
    pub fn initialize(&mut self, obj_size: usize, num_objects: usize) {
        self.obj_size = obj_size;
        self.capacity = num_objects;

        // Initialize the inner allocator.
        self.inner.initialize();

        // Pre-allocate nodes and add them to the free list (in reverse order, matching upstream).
        self.inner.nodes.clear();
        self.inner.nodes.reserve(num_objects);
        for _ in 0..num_objects {
            self.inner.nodes.push(Node { next: None });
        }

        // Free in reverse order to match upstream behavior
        // (upstream iterates from end to start, freeing each).
        for i in (0..num_objects).rev() {
            self.inner.free(i);
        }
    }

    /// Check if an index is within the slab range.
    pub fn contains(&self, index: usize) -> bool {
        index < self.capacity
    }

    /// Get the number of objects in the slab.
    pub fn get_slab_heap_size(&self) -> usize {
        self.capacity
    }

    /// Get the object size.
    pub fn get_object_size(&self) -> usize {
        self.obj_size
    }

    /// Allocate a slot index.
    pub fn allocate(&mut self) -> Option<usize> {
        self.inner.allocate()
    }

    /// Free a slot index.
    pub fn free(&mut self, index: usize) {
        debug_assert!(self.contains(index));
        self.inner.free(index);
    }

    /// Get the object index. Returns usize::MAX if dynamic expansion is supported
    /// and the index is out of range.
    pub fn get_object_index(&self, index: usize) -> usize {
        if self.support_dynamic_expansion && !self.contains(index) {
            return usize::MAX;
        }
        index
    }

    /// Number of remaining free objects (debug only, returns 0 like upstream).
    pub fn get_num_remaining(&self) -> usize {
        0
    }
}

/// KSlabHeap<T> — typed slab heap that allocates objects of type T.
///
/// Mirrors `Kernel::KSlabHeap<T>`.
/// In Rust, we store objects in a Vec and manage allocation via the base slab heap indices.
pub struct KSlabHeap<T> {
    base: KSlabHeapBase,
    storage: Vec<Option<T>>,
}

impl<T> KSlabHeap<T> {
    pub fn new() -> Self {
        Self {
            base: KSlabHeapBase::new(false),
            storage: Vec::new(),
        }
    }

    /// Initialize with a given capacity.
    pub fn initialize(&mut self, num_objects: usize) {
        self.base.initialize(std::mem::size_of::<T>(), num_objects);
        self.storage.clear();
        self.storage.reserve(num_objects);
        for _ in 0..num_objects {
            self.storage.push(None);
        }
    }

    /// Allocate a default-constructed T. Returns a mutable reference and its index.
    pub fn allocate_default(&mut self) -> Option<(usize, &mut T)>
    where
        T: Default,
    {
        let idx = self.base.allocate()?;
        self.storage[idx] = Some(T::default());
        Some((idx, self.storage[idx].as_mut().unwrap()))
    }

    /// Allocate by placing an already-constructed T. Returns the index.
    pub fn allocate_with(&mut self, value: T) -> Option<usize> {
        let idx = self.base.allocate()?;
        self.storage[idx] = Some(value);
        Some(idx)
    }

    /// Free an object by index.
    pub fn free(&mut self, idx: usize) {
        self.storage[idx] = None;
        self.base.free(idx);
    }

    /// Get a reference to the object at an index.
    pub fn get(&self, idx: usize) -> Option<&T> {
        self.storage.get(idx)?.as_ref()
    }

    /// Get a mutable reference to the object at an index.
    pub fn get_mut(&mut self, idx: usize) -> Option<&mut T> {
        self.storage.get_mut(idx)?.as_mut()
    }

    /// Get the object index.
    pub fn get_object_index(&self, idx: usize) -> usize {
        self.base.get_object_index(idx)
    }

    /// Get the slab heap size (capacity).
    pub fn get_slab_heap_size(&self) -> usize {
        self.base.get_slab_heap_size()
    }

    /// Get the object size.
    pub fn get_object_size(&self) -> usize {
        self.base.get_object_size()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slab_heap_basic() {
        let mut heap: KSlabHeap<u64> = KSlabHeap::new();
        heap.initialize(4);

        let idx0 = heap.allocate_with(100).unwrap();
        let idx1 = heap.allocate_with(200).unwrap();
        assert_eq!(*heap.get(idx0).unwrap(), 100);
        assert_eq!(*heap.get(idx1).unwrap(), 200);

        heap.free(idx0);
        let idx2 = heap.allocate_with(300).unwrap();
        assert_eq!(*heap.get(idx2).unwrap(), 300);
    }

    #[test]
    fn test_slab_heap_exhaustion() {
        let mut heap: KSlabHeap<u32> = KSlabHeap::new();
        heap.initialize(2);

        assert!(heap.allocate_with(1).is_some());
        assert!(heap.allocate_with(2).is_some());
        assert!(heap.allocate_with(3).is_none()); // full
    }
}
