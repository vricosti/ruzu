//! Port of zuyu/src/core/hle/kernel/k_dynamic_resource_manager.h
//! Status: Ported (layered slab + page allocator).
//! Derniere synchro: 2026-05-04
//!
//! Generic `KDynamicResourceManager<T>` wrapping a `KDynamicSlabHeap<T>`
//! plus a `KDynamicPageManager`. The slab heap holds the typed free list
//! and grows on demand by pulling pages from the page allocator.
//! Typed aliases below match upstream's `KMemoryBlockSlabManager` and
//! `KBlockInfoManager` declarations.

use super::k_dynamic_page_manager::KDynamicPageManager;
use super::k_dynamic_slab_heap::KDynamicSlabHeap;
use super::k_memory_block::KMemoryBlock;
use super::svc::svc_results;
use std::sync::{Arc, Mutex};

/// Generic resource manager — `KDynamicResourceManager<T>` in upstream.
///
/// Wraps a `KDynamicSlabHeap<T>` (typed free list) and a
/// `KDynamicPageManager` (page-level backing). Allocate forwards to the
/// slab heap, which lazily pulls pages from the page manager on
/// exhaustion. Free returns to the slab heap.
///
/// Mirrors upstream's three-method surface (`Allocate`, `Free`,
/// `GetSize`/`GetUsed`/`GetPeak`/`GetCount`) plus a capacity-tracking
/// `free_count()` accessor for diagnostics.
pub struct KDynamicResourceManager<T: Default> {
    slab_heap: Arc<KDynamicSlabHeap<T>>,
    page_allocator: Arc<Mutex<KDynamicPageManager>>,
    /// True if the page manager owns its backing region (created by
    /// `initialize`); false if both slab heap and page manager are
    /// `Default::default()` (caller will wire externally).
    initialized: bool,
}

impl<T: Default> KDynamicResourceManager<T> {
    /// Create an empty manager. Call [`initialize`] to populate it.
    pub fn new() -> Self {
        Self {
            slab_heap: Arc::new(KDynamicSlabHeap::new(true)),
            page_allocator: Arc::new(Mutex::new(KDynamicPageManager::new())),
            initialized: false,
        }
    }

    /// Pre-allocate `capacity` slab entries. The page manager is sized
    /// in PAGE_SIZE chunks computed from `capacity * sizeof(T)`.
    ///
    /// Mirrors upstream's
    /// `KDynamicResourceManager::Initialize(page_allocator, slab_heap)`
    /// where the page allocator is pre-sized by the kernel-init layer
    /// (`KernelApplicationMemoryBlockSlabHeapSize` /
    /// `KernelSystemMemoryBlockSlabHeapSize`) and the slab heap is then
    /// initialized to consume `object_count` worth of those pages.
    pub fn initialize(&mut self, capacity: usize) {
        let entries_per_page = KDynamicSlabHeap::<T>::entries_per_page();
        // Round up to whole pages so the slab gets at least `capacity`
        // entries and the page manager has a real backing region to
        // hand out.
        let num_pages = capacity.div_ceil(entries_per_page).max(1);
        let region_size = num_pages * super::k_memory_block::PAGE_SIZE;
        // Anchor the page manager at a synthetic base address well above
        // any legal guest VA so `is_in_range` queries against the slab
        // backing don't collide with mapped guest memory. Each
        // KDynamicResourceManager picks a unique base via a process-wide
        // counter so multiple resource managers don't share a range.
        use std::sync::atomic::{AtomicU64, Ordering};
        static NEXT_SLAB_BASE: AtomicU64 = AtomicU64::new(0xFFFF_F000_0000_0000);
        let base = NEXT_SLAB_BASE.fetch_add(
            (region_size as u64 + super::k_memory_block::PAGE_SIZE as u64) * 2,
            Ordering::Relaxed,
        );
        {
            let mut pa = self.page_allocator.lock().unwrap();
            pa.initialize(base, region_size, super::k_memory_block::PAGE_SIZE)
                .expect("KDynamicPageManager::initialize");
        }
        self.slab_heap
            .initialize_with_pages(Arc::clone(&self.page_allocator), num_pages);
        self.initialized = true;
    }

    /// Pop one entry from the slab. Upstream:
    ///   `T* Allocate() const { return m_slab_heap->Allocate(m_page_allocator); }`
    /// Returns `None` when both the slab and the page manager are
    /// exhausted (upstream returns `nullptr`).
    pub fn allocate(&self) -> Option<Box<T>> {
        self.slab_heap.allocate()
    }

    /// Return an entry to the slab. Upstream:
    ///   `void Free(T* t) const { m_slab_heap->Free(t); }`
    pub fn free(&self, block: Box<T>) {
        self.slab_heap.free(block);
    }

    /// Total slab capacity in bytes.
    pub fn get_size(&self) -> usize {
        self.slab_heap.get_size()
    }
    pub fn get_used(&self) -> usize {
        self.slab_heap.get_used()
    }
    pub fn get_peak(&self) -> usize {
        self.slab_heap.get_peak()
    }
    pub fn get_count(&self) -> usize {
        self.slab_heap.get_count()
    }

    /// Free count remaining in the slab.
    pub fn free_count(&self) -> usize {
        self.slab_heap.get_count() - self.slab_heap.get_used()
    }

    /// Membership test for a slab-managed address. Used by
    /// `KPageTableManager::IsInPageTableHeap` to validate pages handed
    /// to `Free` actually came from this slab.
    pub fn is_in_range(&self, addr: u64) -> bool {
        self.slab_heap.is_in_range(addr)
    }

    /// Slab-region base address — for upstream-shape callers that want
    /// to validate `addr in [base, base+size)`.
    pub fn get_address(&self) -> u64 {
        self.slab_heap.get_address()
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
pub type KBlockInfoManager = KDynamicResourceManager<super::k_page_group::KBlockInfo>;
