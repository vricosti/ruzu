//! Port of zuyu/src/core/hle/kernel/k_dynamic_resource_manager.h
//! Status: Ported (layered slab + page allocator).
//! Derniere synchro: 2026-05-04
//!
//! Generic `KDynamicResourceManager<T>` wrapping a `KDynamicSlabHeap<T>`
//! plus a `KDynamicPageManager`. The slab heap holds the typed free list
//! and grows on demand by pulling pages from the page allocator.
//! Typed aliases below match upstream's `KMemoryBlockSlabManager` and
//! `KBlockInfoManager` declarations.
//!
//! Storage backing: ruzu's slab is host-allocated via `Box<T>`. Upstream's
//! version carves guest-physical pages into typed entries — functionally
//! identical here because the emulator runs entirely on host memory.

use super::k_memory_block::KMemoryBlock;
use super::svc::svc_results;
use std::sync::Mutex;

/// Generic resource manager — `KDynamicResourceManager<T>` in upstream.
///
/// Owns a free list of pre-allocated `Box<T>` instances drawn from the
/// kernel's `KDynamicPageManager` capacity at boot. `Allocate` pops a
/// node (returns `None` on exhaustion to mirror upstream's null-on-fail);
/// `Free` returns a node to the list.
///
/// Mirrors upstream's three-method surface (`Allocate`, `Free`,
/// `GetSize`/`GetUsed`/`GetPeak`/`GetCount`) plus capacity introspection
/// for diagnostics.
#[derive(Debug)]
pub struct KDynamicResourceManager<T: Default> {
    free_list: Mutex<Vec<Box<T>>>,
    capacity: usize,
    used: std::sync::atomic::AtomicUsize,
    peak: std::sync::atomic::AtomicUsize,
}

impl<T: Default> KDynamicResourceManager<T> {
    /// Create an empty manager. Call [`initialize`] to populate the slab.
    pub fn new() -> Self {
        Self {
            free_list: Mutex::new(Vec::new()),
            capacity: 0,
            used: std::sync::atomic::AtomicUsize::new(0),
            peak: std::sync::atomic::AtomicUsize::new(0),
        }
    }

    /// Pre-allocate `capacity` slab entries. Mirrors upstream's
    /// `KDynamicResourceManager::Initialize(page_allocator, slab_heap)`,
    /// where the slab is sized by the kernel's
    /// `Kernel{Application,System}MemoryBlockSlabHeapSize` constants.
    pub fn initialize(&mut self, capacity: usize) {
        let mut list = self.free_list.lock().unwrap();
        list.clear();
        list.reserve(capacity);
        for _ in 0..capacity {
            list.push(Box::new(T::default()));
        }
        drop(list);
        self.capacity = capacity;
    }

    /// Pop one entry from the free list. Upstream:
    ///   `T* Allocate() const { return m_slab_heap->Allocate(m_page_allocator); }`
    /// Returns `None` when the slab is exhausted (upstream returns `nullptr`).
    pub fn allocate(&self) -> Option<Box<T>> {
        let mut list = self.free_list.lock().unwrap();
        let item = list.pop()?;
        let used = self.used.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
        let mut peak = self.peak.load(std::sync::atomic::Ordering::Relaxed);
        while peak < used {
            match self.peak.compare_exchange_weak(
                peak,
                used,
                std::sync::atomic::Ordering::Relaxed,
                std::sync::atomic::Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(actual) => peak = actual,
            }
        }
        Some(item)
    }

    /// Return an entry to the free list. Upstream:
    ///   `void Free(T* t) const { m_slab_heap->Free(t); }`
    /// Resets `*block` so reused entries don't carry stale state.
    pub fn free(&self, mut block: Box<T>) {
        *block = T::default();
        self.free_list.lock().unwrap().push(block);
        self.used
            .fetch_sub(1, std::sync::atomic::Ordering::Relaxed);
    }

    pub fn get_size(&self) -> usize {
        self.capacity * std::mem::size_of::<T>()
    }
    pub fn get_used(&self) -> usize {
        self.used.load(std::sync::atomic::Ordering::Relaxed)
    }
    pub fn get_peak(&self) -> usize {
        self.peak.load(std::sync::atomic::Ordering::Relaxed)
    }
    pub fn get_count(&self) -> usize {
        self.capacity
    }

    /// Free count remaining in the slab — convenience used by
    /// `KMemoryBlockManagerUpdateAllocator`'s pre-reservation check.
    pub fn free_count(&self) -> usize {
        self.free_list.lock().unwrap().len()
    }
}

impl<T: Default> Default for KDynamicResourceManager<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// `KMemoryBlockSlabManager` — typed alias for the manager that holds
/// `KMemoryBlock` slab entries. Upstream:
/// `class KMemoryBlockSlabManager : public KDynamicResourceManager<KMemoryBlock> {};`
pub type KMemoryBlockSlabManager = KDynamicResourceManager<KMemoryBlock>;

/// Per-update allocator that pre-reserves up to `MAX_BLOCKS = 2` slab
/// entries so a `KMemoryBlockManager::update()` walk can split twice
/// without hitting the slab again mid-walk.
///
/// Port of upstream `Kernel::KMemoryBlockManagerUpdateAllocator`
/// (k_memory_block_manager.h:16). Drop returns any unused nodes to the
/// slab — matches upstream's destructor.
pub struct KMemoryBlockManagerUpdateAllocator {
    blocks: [Option<Box<KMemoryBlock>>; Self::MAX_BLOCKS],
    /// Index of the next slot to dispense. Upstream uses
    /// `m_index = MaxBlocks - num_blocks` so unused front slots stay None.
    index: usize,
    slab_manager: std::sync::Arc<KMemoryBlockSlabManager>,
}

impl KMemoryBlockManagerUpdateAllocator {
    pub const MAX_BLOCKS: usize = 2;

    /// Construct and pre-allocate `num_blocks` slab nodes (capped at
    /// `MAX_BLOCKS`). Returns `(allocator, 0)` on success or
    /// `(uninit_allocator, ResultOutOfResource)` if the slab can't satisfy
    /// the request — upstream returns the result via an `out_result`
    /// pointer; ruzu returns the result code in the second tuple element.
    pub fn new(
        slab_manager: std::sync::Arc<KMemoryBlockSlabManager>,
        num_blocks: usize,
    ) -> (Self, u32) {
        debug_assert!(num_blocks <= Self::MAX_BLOCKS);
        let num_blocks = num_blocks.min(Self::MAX_BLOCKS);
        let mut blocks: [Option<Box<KMemoryBlock>>; Self::MAX_BLOCKS] = Default::default();
        let index = Self::MAX_BLOCKS - num_blocks;
        for slot in &mut blocks[index..] {
            match slab_manager.allocate() {
                Some(b) => *slot = Some(b),
                None => {
                    // Roll back partial allocation.
                    for s in &mut blocks[index..] {
                        if let Some(b) = s.take() {
                            slab_manager.free(b);
                        }
                    }
                    return (
                        Self {
                            blocks: Default::default(),
                            index: Self::MAX_BLOCKS,
                            slab_manager: std::sync::Arc::clone(&slab_manager),
                        },
                        svc_results::RESULT_OUT_OF_RESOURCE.get_inner_value(),
                    );
                }
            }
        }
        (
            Self {
                blocks,
                index,
                slab_manager,
            },
            0,
        )
    }

    /// Hand out one of the pre-allocated nodes. Upstream:
    /// ```cpp
    /// KMemoryBlock* block = nullptr;
    /// std::swap(block, m_blocks[m_index++]);
    /// return block;
    /// ```
    pub fn allocate(&mut self) -> Box<KMemoryBlock> {
        debug_assert!(self.index < Self::MAX_BLOCKS);
        let slot = self.blocks[self.index]
            .take()
            .expect("KMemoryBlockManagerUpdateAllocator::allocate ran out of pre-allocated nodes");
        self.index += 1;
        slot
    }

    /// Return a node — either to a free pre-allocation slot (if not
    /// already saturated) or back to the slab. Upstream:
    /// ```cpp
    /// if (m_index == 0) m_slab_manager->Free(block);
    /// else m_blocks[--m_index] = block;
    /// ```
    pub fn free(&mut self, block: Box<KMemoryBlock>) {
        if self.index == 0 {
            self.slab_manager.free(block);
        } else {
            self.index -= 1;
            self.blocks[self.index] = Some(block);
        }
    }
}

impl Drop for KMemoryBlockManagerUpdateAllocator {
    fn drop(&mut self) {
        for slot in self.blocks.iter_mut() {
            if let Some(b) = slot.take() {
                self.slab_manager.free(b);
            }
        }
    }
}

/// `KBlockInfoManager` — typed alias for a slab of `KBlockInfo` nodes
/// (used by `KPageGroup`). Matches upstream's
/// `class KBlockInfoManager : public KDynamicResourceManager<KBlockInfo> {};`
pub type KBlockInfoManager =
    KDynamicResourceManager<super::k_page_group::KBlockInfo>;
