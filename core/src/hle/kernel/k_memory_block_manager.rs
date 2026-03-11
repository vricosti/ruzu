//! Port of zuyu/src/core/hle/kernel/k_memory_block_manager.h and k_memory_block_manager.cpp
//! Status: Stubbed (core structure, depends on slab managers)
//! Derniere synchro: 2026-03-11

use super::k_memory_block::*;
use std::collections::BTreeMap;

// ---------------------------------------------------------------------------
// KMemoryBlockManager
// ---------------------------------------------------------------------------

/// Port of Kernel::KMemoryBlockManager.
///
/// Uses a BTreeMap<usize, KMemoryBlock> keyed by block address
/// to match the upstream intrusive red-black tree.
pub struct KMemoryBlockManager {
    memory_block_tree: BTreeMap<usize, KMemoryBlock>,
    m_start_address: usize,
    m_end_address: usize,
}

impl KMemoryBlockManager {
    pub fn new() -> Self {
        Self {
            memory_block_tree: BTreeMap::new(),
            m_start_address: 0,
            m_end_address: 0,
        }
    }

    /// Initialize the block manager with start/end addresses.
    /// Stubbed: does not use slab manager.
    pub fn initialize(&mut self, st: usize, nd: usize) -> Result<(), ()> {
        self.m_start_address = st;
        self.m_end_address = nd;
        debug_assert!(st % PAGE_SIZE == 0);
        debug_assert!(nd % PAGE_SIZE == 0);

        let mut start_block = KMemoryBlock::new();
        start_block.initialize(
            st,
            (nd - st) / PAGE_SIZE,
            KMemoryState::FREE,
            KMemoryPermission::NONE,
            KMemoryAttribute::NONE,
        );
        self.memory_block_tree.insert(st, start_block);
        Ok(())
    }

    pub fn find_block(&self, address: usize) -> Option<&KMemoryBlock> {
        // Find the block whose range contains address.
        for (_, block) in self.memory_block_tree.range(..=address).rev() {
            if block.get_address() <= address && address < block.get_end_address() {
                return Some(block);
            }
            break;
        }
        None
    }

    pub fn find_free_area(
        &self,
        region_start: usize,
        region_num_pages: usize,
        num_pages: usize,
        alignment: usize,
        offset: usize,
        guard_pages: usize,
    ) -> Option<usize> {
        if num_pages == 0 {
            return None;
        }

        let region_end = region_start + region_num_pages * PAGE_SIZE;
        let region_last = region_end - 1;

        for block in self.memory_block_tree.values() {
            let info = block.get_memory_info();
            if region_last < info.get_address() {
                break;
            }
            if info.m_state != KMemoryState::FREE {
                continue;
            }

            let mut area = if info.get_address() <= region_start {
                region_start
            } else {
                info.get_address()
            };
            area += guard_pages * PAGE_SIZE;

            let offset_area = common::alignment::align_down(area as u64, alignment as u64) as usize + offset;
            area = if area <= offset_area {
                offset_area
            } else {
                offset_area + alignment
            };

            let area_end = area + num_pages * PAGE_SIZE + guard_pages * PAGE_SIZE;
            let area_last = area_end - 1;

            if info.get_address() <= area
                && area < area_last
                && area_last <= region_last
                && area_last <= info.get_last_address()
            {
                return Some(area);
            }
        }

        None
    }

    pub fn iter(&self) -> impl Iterator<Item = &KMemoryBlock> {
        self.memory_block_tree.values()
    }

    /// Check that the block tree state is valid (debug).
    pub fn check_state(&self) -> bool {
        let blocks: Vec<&KMemoryBlock> = self.memory_block_tree.values().collect();
        if blocks.len() < 2 {
            return true;
        }
        for i in 0..blocks.len() - 1 {
            let prev = blocks[i];
            let cur = blocks[i + 1];
            if prev.can_merge_with(cur) {
                return false;
            }
            let prev_info = prev.get_memory_info();
            let cur_info = cur.get_memory_info();
            if prev_info.get_end_address() != cur_info.get_address() {
                return false;
            }
        }
        true
    }

    pub fn get_start_address(&self) -> usize {
        self.m_start_address
    }

    pub fn get_end_address(&self) -> usize {
        self.m_end_address
    }
}

impl Default for KMemoryBlockManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// KScopedMemoryBlockManagerAuditor
// ---------------------------------------------------------------------------

/// No-op auditor (debug tool in upstream).
pub struct KScopedMemoryBlockManagerAuditor;

impl KScopedMemoryBlockManagerAuditor {
    pub fn new(_manager: &KMemoryBlockManager) -> Self {
        Self
    }
}
