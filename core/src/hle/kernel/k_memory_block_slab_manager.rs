//! Port of zuyu/src/core/hle/kernel/k_memory_block_manager.h
//! (`KMemoryBlockManagerUpdateAllocator` + `KMemoryBlockSlabManager`).
//! Status: structural port — backed by Box<KMemoryBlock> free list rather
//! than the upstream KDynamicResourceManager<KMemoryBlock>.
//! Derniere synchro: 2026-05-04.
//!
//! Upstream's `KMemoryBlockSlabManager` is a typed wrapper around
//! `KDynamicResourceManager<KMemoryBlock>` that allocates `KMemoryBlock`
//! nodes from a guest-physical slab heap. ruzu's `KMemoryBlockManager`
//! stores blocks in a `BTreeMap`, so no guest pages back the nodes —
//! they are heap-allocated `Box<KMemoryBlock>`. The slab manager here
//! still tracks a free-list with a configurable capacity so allocation
//! can FAIL (returning `None`), matching the upstream contract that
//! `KMemoryBlockManagerUpdateAllocator::Initialize` may return
//! `ResultOutOfResource`.

use super::k_memory_block::KMemoryBlock;
use super::svc::svc_results;
use std::sync::{Arc, Mutex};

/// Slab manager for `KMemoryBlock` nodes.
///
/// Port of `Kernel::KMemoryBlockSlabManager`
/// (k_dynamic_resource_manager.h:56). Holds a free list of pre-allocated
/// `Box<KMemoryBlock>` instances; `Allocate` pops one (returning `None` if
/// the slab is exhausted), `Free` pushes one back.
///
/// Capacity is provided at construction; the slab pre-fills its free list
/// to that size at startup so callers see deterministic resource pressure
/// rather than relying on Rust's allocator.
#[derive(Debug)]
pub struct KMemoryBlockSlabManager {
    free_list: Vec<Box<KMemoryBlock>>,
}

impl KMemoryBlockSlabManager {
    /// Create a slab pre-filled with `capacity` free blocks. Mirrors the
    /// upstream init pattern where the kernel reserves a fixed-size heap
    /// of `KMemoryBlock` nodes at boot.
    pub fn new(capacity: usize) -> Self {
        let mut free_list = Vec::with_capacity(capacity);
        for _ in 0..capacity {
            free_list.push(Box::new(KMemoryBlock::new()));
        }
        Self { free_list }
    }

    /// Allocate one `KMemoryBlock` node from the slab. Returns `None` when
    /// the slab is exhausted (upstream returns `nullptr`).
    pub fn allocate(&mut self) -> Option<Box<KMemoryBlock>> {
        self.free_list.pop()
    }

    /// Return a node to the slab. Upstream: `Free(KMemoryBlock*)`.
    pub fn free(&mut self, mut block: Box<KMemoryBlock>) {
        // Reset block content so it doesn't leak old state across reuse.
        *block = KMemoryBlock::new();
        self.free_list.push(block);
    }

    pub fn free_count(&self) -> usize {
        self.free_list.len()
    }
}

/// Per-update allocator that pre-reserves up to two slab nodes so a
/// `KMemoryBlockManager::update()` walk can split twice without hitting the
/// slab again mid-walk.
///
/// Port of `Kernel::KMemoryBlockManagerUpdateAllocator`
/// (k_memory_block_manager.h:16). Upstream pre-allocates two
/// `KMemoryBlock*` slots, refusing the update altogether if the slab can't
/// satisfy the request (returns `ResultOutOfResource`).
///
/// Drop returns any unused nodes to the slab — matching upstream's
/// destructor.
pub struct KMemoryBlockManagerUpdateAllocator {
    blocks: [Option<Box<KMemoryBlock>>; Self::MAX_BLOCKS],
    /// Index of the next slot to dispense. Upstream uses
    /// `m_index = MaxBlocks - num_blocks` so unused front slots stay None.
    index: usize,
    slab_manager: Arc<Mutex<KMemoryBlockSlabManager>>,
}

impl KMemoryBlockManagerUpdateAllocator {
    pub const MAX_BLOCKS: usize = 2;

    /// Construct and pre-allocate `num_blocks` slab nodes (capped at
    /// `MAX_BLOCKS`). Returns `(allocator, 0)` on success or
    /// `(uninit_allocator, ResultOutOfResource)` when the slab can't
    /// satisfy the request — upstream returns the result via an `out_result`
    /// pointer; ruzu returns the result code in the second tuple element.
    pub fn new(
        slab_manager: Arc<Mutex<KMemoryBlockSlabManager>>,
        num_blocks: usize,
    ) -> (Self, u32) {
        debug_assert!(num_blocks <= Self::MAX_BLOCKS);
        let num_blocks = num_blocks.min(Self::MAX_BLOCKS);
        let mut blocks: [Option<Box<KMemoryBlock>>; Self::MAX_BLOCKS] = Default::default();
        let index = Self::MAX_BLOCKS - num_blocks;
        let mut sm = slab_manager.lock().unwrap();
        for slot in &mut blocks[index..] {
            match sm.allocate() {
                Some(b) => *slot = Some(b),
                None => {
                    // Roll back partial allocation.
                    for s in &mut blocks[index..] {
                        if let Some(b) = s.take() {
                            sm.free(b);
                        }
                    }
                    drop(sm);
                    return (
                        Self {
                            blocks: Default::default(),
                            index: Self::MAX_BLOCKS,
                            slab_manager,
                        },
                        svc_results::RESULT_OUT_OF_RESOURCE.get_inner_value(),
                    );
                }
            }
        }
        drop(sm);
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

    /// Return a node — either to a free pre-allocation slot (if we're not
    /// already saturated) or back to the slab. Upstream:
    /// ```cpp
    /// if (m_index == 0) m_slab_manager->Free(block);
    /// else m_blocks[--m_index] = block;
    /// ```
    pub fn free(&mut self, block: Box<KMemoryBlock>) {
        if self.index == 0 {
            self.slab_manager.lock().unwrap().free(block);
        } else {
            self.index -= 1;
            self.blocks[self.index] = Some(block);
        }
    }
}

impl Drop for KMemoryBlockManagerUpdateAllocator {
    fn drop(&mut self) {
        // Return any nodes still held to the slab. Upstream destructor:
        //   for (auto& b : m_blocks) if (b) m_slab_manager->Free(b);
        let mut sm = self.slab_manager.lock().unwrap();
        for slot in self.blocks.iter_mut() {
            if let Some(b) = slot.take() {
                sm.free(b);
            }
        }
    }
}
