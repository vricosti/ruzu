//! Port of zuyu/src/core/hle/kernel/k_memory_block_manager.h and k_memory_block_manager.cpp
//! Status: Ported (full implementation)
//! Derniere synchro: 2026-03-13
//!
//! The KMemoryBlockManager manages a contiguous address space as a tree of
//! non-overlapping KMemoryBlock entries. Every byte in [start, end) belongs to
//! exactly one block. Operations (Update, UpdateLock, etc.) split blocks at
//! the boundaries of the affected range, apply the change, then coalesce
//! adjacent blocks that have become identical.
//!
//! Upstream uses an intrusive red-black tree; we use a BTreeMap<usize, KMemoryBlock>
//! keyed by block start address, which gives the same O(log N) guarantees.

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
    /// Creates a single Free block spanning the entire range.
    ///
    /// Upstream: KMemoryBlockManager::Initialize(start, end, slab_manager).
    /// We skip the slab manager — blocks are heap-allocated directly.
    pub fn initialize(&mut self, st: usize, nd: usize) -> Result<(), ()> {
        self.m_start_address = st;
        self.m_end_address = nd;
        debug_assert!(st % PAGE_SIZE == 0);
        debug_assert!(nd % PAGE_SIZE == 0);
        debug_assert!(st < nd);

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

    /// Finalize the manager: clear all blocks.
    ///
    /// Upstream: KMemoryBlockManager::Finalize(slab_manager, callback).
    pub fn finalize(&mut self) {
        self.memory_block_tree.clear();
    }

    /// Find the block containing the given address.
    ///
    /// Upstream: KMemoryBlockManager::FindBlock(address).
    pub fn find_block(&self, address: usize) -> Option<&KMemoryBlock> {
        // Find the last block with start_addr <= address.
        for (_, block) in self.memory_block_tree.range(..=address).rev() {
            if block.get_address() <= address && address < block.get_end_address() {
                return Some(block);
            }
            break;
        }
        None
    }

    /// Find the block containing the given address (mutable).
    pub fn find_block_mut(&mut self, address: usize) -> Option<&mut KMemoryBlock> {
        // Collect the key first to avoid double borrow.
        let key = {
            let mut found = None;
            for (&k, block) in self.memory_block_tree.range(..=address).rev() {
                if block.get_address() <= address && address < block.get_end_address() {
                    found = Some(k);
                }
                break;
            }
            found
        };
        key.and_then(move |k| self.memory_block_tree.get_mut(&k))
    }

    /// Update blocks in the range [address, address + num_pages * PAGE_SIZE) to
    /// have the given state, permission, and attribute.
    ///
    /// This is the core operation that splits blocks at boundaries, updates the
    /// affected range, and then coalesces compatible neighbors.
    ///
    /// Upstream: KMemoryBlockManager::Update(allocator, address, num_pages, state, perm, attr,
    ///           set_disable_attr, clear_disable_attr).
    pub fn update(
        &mut self,
        address: usize,
        num_pages: usize,
        state: KMemoryState,
        perm: KMemoryPermission,
        attr: KMemoryAttribute,
        set_disable_attr: KMemoryBlockDisableMergeAttribute,
        clear_disable_attr: KMemoryBlockDisableMergeAttribute,
    ) {
        let update_end = address + num_pages * PAGE_SIZE;
        debug_assert!(address % PAGE_SIZE == 0);
        debug_assert!(num_pages > 0);
        debug_assert!(update_end <= self.m_end_address);

        // Phase 1: Split block at start boundary if needed.
        self.split_at(address);

        // Phase 2: Split block at end boundary if needed.
        self.split_at(update_end);

        // Phase 3: Update all blocks in the range.
        let set_disable = !set_disable_attr.is_empty();
        let keys_in_range: Vec<usize> = self
            .memory_block_tree
            .range(address..update_end)
            .map(|(&k, _)| k)
            .collect();

        for key in keys_in_range {
            if let Some(block) = self.memory_block_tree.get_mut(&key) {
                block.update(
                    state,
                    perm,
                    attr,
                    set_disable,
                    set_disable_attr.bits(),
                    clear_disable_attr.bits(),
                );
            }
        }

        // Phase 4: Coalesce adjacent blocks that have become compatible.
        self.coalesce_for_update(address, num_pages);
    }

    /// Update blocks in the range only if they match the test properties.
    ///
    /// Upstream: KMemoryBlockManager::UpdateIfMatch.
    pub fn update_if_match(
        &mut self,
        address: usize,
        num_pages: usize,
        test_state: KMemoryState,
        test_perm: KMemoryPermission,
        test_attr: KMemoryAttribute,
        state: KMemoryState,
        perm: KMemoryPermission,
        attr: KMemoryAttribute,
        set_disable_attr: KMemoryBlockDisableMergeAttribute,
        clear_disable_attr: KMemoryBlockDisableMergeAttribute,
    ) {
        let update_end = address + num_pages * PAGE_SIZE;

        self.split_at(address);
        self.split_at(update_end);

        let set_disable = !set_disable_attr.is_empty();
        let keys_in_range: Vec<usize> = self
            .memory_block_tree
            .range(address..update_end)
            .map(|(&k, _)| k)
            .collect();

        for key in keys_in_range {
            if let Some(block) = self.memory_block_tree.get_mut(&key) {
                if block.has_properties(test_state, test_perm, test_attr) {
                    block.update(
                        state,
                        perm,
                        attr,
                        set_disable,
                        set_disable_attr.bits(),
                        clear_disable_attr.bits(),
                    );
                }
            }
        }

        self.coalesce_for_update(address, num_pages);
    }

    /// Update block attributes in the range.
    ///
    /// Upstream: KMemoryBlockManager::UpdateAttribute.
    pub fn update_attribute(
        &mut self,
        address: usize,
        num_pages: usize,
        mask: KMemoryAttribute,
        attr: KMemoryAttribute,
    ) {
        let update_end = address + num_pages * PAGE_SIZE;

        self.split_at(address);
        self.split_at(update_end);

        let keys_in_range: Vec<usize> = self
            .memory_block_tree
            .range(address..update_end)
            .map(|(&k, _)| k)
            .collect();

        for key in keys_in_range {
            if let Some(block) = self.memory_block_tree.get_mut(&key) {
                block.update_attribute(mask, attr);
            }
        }

        self.coalesce_for_update(address, num_pages);
    }

    /// Apply a lock function to blocks in the range.
    ///
    /// Upstream: KMemoryBlockManager::UpdateLock(allocator, address, num_pages, lock_func, perm).
    /// `lock_func` is a closure that takes (&mut KMemoryBlock, perm, is_first, is_last).
    pub fn update_lock<F>(
        &mut self,
        address: usize,
        num_pages: usize,
        perm: KMemoryPermission,
        mut lock_func: F,
    ) where
        F: FnMut(&mut KMemoryBlock, KMemoryPermission, bool, bool),
    {
        let update_end = address + num_pages * PAGE_SIZE;

        self.split_at(address);
        self.split_at(update_end);

        let keys_in_range: Vec<usize> = self
            .memory_block_tree
            .range(address..update_end)
            .map(|(&k, _)| k)
            .collect();

        let count = keys_in_range.len();
        for (i, key) in keys_in_range.iter().enumerate() {
            if let Some(block) = self.memory_block_tree.get_mut(key) {
                let is_first = i == 0;
                let is_last = i == count - 1;
                lock_func(block, perm, is_first, is_last);
            }
        }

        self.coalesce_for_update(address, num_pages);
    }

    /// Query the memory info for a given address.
    ///
    /// Upstream: KPageTableBase::QueryInfoImpl calls
    /// m_memory_block_manager.FindBlock(address)->GetMemoryInfo().
    pub fn query_info(&self, address: usize) -> Option<KMemoryInfo> {
        self.find_block(address).map(|block| block.get_memory_info())
    }

    /// Find free area in a region with alignment/guard constraints.
    ///
    /// Upstream: KMemoryBlockManager::FindFreeArea.
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

            let offset_area =
                common::alignment::align_down(area as u64, alignment as u64) as usize + offset;
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

    pub fn block_count(&self) -> usize {
        self.memory_block_tree.len()
    }

    /// Check that the block tree state is valid (debug).
    ///
    /// Upstream: KMemoryBlockManager::CheckState.
    pub fn check_state(&self) -> bool {
        let blocks: Vec<&KMemoryBlock> = self.memory_block_tree.values().collect();
        if blocks.is_empty() {
            return true;
        }

        // First block should start at m_start_address.
        if blocks[0].get_address() != self.m_start_address {
            return false;
        }

        // Last block should end at m_end_address.
        if blocks.last().unwrap().get_end_address() != self.m_end_address {
            return false;
        }

        for i in 0..blocks.len() - 1 {
            let prev = blocks[i];
            let cur = blocks[i + 1];
            // No adjacent mergeable blocks should exist.
            if prev.can_merge_with(cur) {
                return false;
            }
            // Blocks must be contiguous.
            if prev.get_end_address() != cur.get_address() {
                return false;
            }
        }

        // Validate invariants on individual blocks.
        for block in &blocks {
            let info = block.get_memory_info();
            if info.m_ipc_lock_count > 0 {
                if (info.m_attribute & KMemoryAttribute::IPC_LOCKED) != KMemoryAttribute::IPC_LOCKED
                {
                    return false;
                }
            }
            if info.m_device_use_count > 0 {
                if (info.m_attribute & KMemoryAttribute::DEVICE_SHARED)
                    != KMemoryAttribute::DEVICE_SHARED
                {
                    return false;
                }
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

    // -----------------------------------------------------------------------
    // Internal helpers
    // -----------------------------------------------------------------------

    /// Split the block at the given address.
    ///
    /// If `address` falls in the middle of an existing block, that block is
    /// split into two: [block_start, address) and [address, block_end).
    /// If `address` is already a block boundary, this is a no-op.
    fn split_at(&mut self, address: usize) {
        if address == self.m_start_address || address == self.m_end_address {
            return;
        }

        // Find the block containing `address`.
        let containing_key = {
            let mut found = None;
            for (&k, block) in self.memory_block_tree.range(..=address).rev() {
                if block.get_address() <= address && address < block.get_end_address() {
                    if block.get_address() == address {
                        // Already a boundary — no split needed.
                        return;
                    }
                    found = Some(k);
                }
                break;
            }
            found
        };

        if let Some(key) = containing_key {
            // Remove the block, split it, and reinsert both halves.
            let mut block = self.memory_block_tree.remove(&key).unwrap();
            let mut left_block = KMemoryBlock::new();
            block.split(&mut left_block, address);
            // left_block covers [original_start, address)
            // block now covers [address, original_end)
            self.memory_block_tree.insert(left_block.get_address(), left_block);
            self.memory_block_tree.insert(block.get_address(), block);
        }
    }

    /// Coalesce adjacent blocks that have become compatible after an update.
    ///
    /// Upstream: KMemoryBlockManager::CoalesceForUpdate.
    fn coalesce_for_update(&mut self, address: usize, num_pages: usize) {
        let update_end = address + num_pages * PAGE_SIZE;

        // Determine the range of blocks to consider for merging.
        // We need to include the block before `address` and the block at `update_end`.
        let start_key = {
            let mut sk = address;
            // Find the block that ends at `address` (the predecessor).
            for (&k, block) in self.memory_block_tree.range(..address).rev() {
                if block.get_end_address() == address {
                    sk = k;
                }
                break;
            }
            sk
        };

        // Collect all keys in the coalesce range.
        let end_key = {
            // Include the block at update_end (the successor) if it exists.
            let mut ek = update_end;
            if let Some((&k, _)) = self.memory_block_tree.range(update_end..).next() {
                if k == update_end {
                    // Include this block's end for coalescing.
                    ek = k + 1; // just past start to include it in range
                }
            }
            ek
        };

        // Collect keys for merging candidates (from start_key to end_key inclusive).
        let keys: Vec<usize> = self
            .memory_block_tree
            .range(start_key..=end_key)
            .map(|(&k, _)| k)
            .collect();

        if keys.len() < 2 {
            return;
        }

        // Attempt to merge consecutive blocks.
        let mut i = 0;
        while i < keys.len() - 1 {
            let cur_key = keys[i];
            let next_key = keys[i + 1];

            let can_merge = {
                let cur = self.memory_block_tree.get(&cur_key);
                let next = self.memory_block_tree.get(&next_key);
                match (cur, next) {
                    (Some(c), Some(n)) => c.can_merge_with(n),
                    _ => false,
                }
            };

            if can_merge {
                let next_block = self.memory_block_tree.remove(&next_key).unwrap();
                let cur_block = self.memory_block_tree.get_mut(&cur_key).unwrap();
                cur_block.add(&next_block);
                // Don't advance i — try to merge the extended block with the next one.
                // But we need to update our keys list.
                // Since we consumed next_key, skip it.
                i += 1; // advance past the merged key
                         // Try merging cur with the new next (if any).
                         // We handle this by just continuing the loop with updated i.
            } else {
                i += 1;
            }
        }
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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_initialize_creates_single_free_block() {
        let mut mgr = KMemoryBlockManager::new();
        mgr.initialize(0x200000, 0x400000).unwrap();
        assert_eq!(mgr.block_count(), 1);
        assert!(mgr.check_state());

        let block = mgr.find_block(0x200000).unwrap();
        assert_eq!(block.get_address(), 0x200000);
        assert_eq!(block.get_size(), 0x200000);
        assert_eq!(block.get_state(), KMemoryState::FREE);
    }

    #[test]
    fn test_update_splits_and_changes_state() {
        let mut mgr = KMemoryBlockManager::new();
        mgr.initialize(0x200000, 0x400000).unwrap();

        // Mark pages [0x210000, 0x220000) as Code/RX.
        mgr.update(
            0x210000,
            (0x220000 - 0x210000) / PAGE_SIZE,
            KMemoryState::CODE,
            KMemoryPermission::USER_READ_EXECUTE,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        // Should have 3 blocks: Free [200000, 210000), Code [210000, 220000), Free [220000, 400000)
        assert_eq!(mgr.block_count(), 3);
        assert!(mgr.check_state());

        let b1 = mgr.find_block(0x200000).unwrap();
        assert_eq!(b1.get_state(), KMemoryState::FREE);
        assert_eq!(b1.get_size(), 0x10000);

        let b2 = mgr.find_block(0x210000).unwrap();
        assert_eq!(b2.get_state(), KMemoryState::CODE);
        assert_eq!(b2.get_permission(), KMemoryPermission::USER_READ_EXECUTE);
        assert_eq!(b2.get_size(), 0x10000);

        let b3 = mgr.find_block(0x220000).unwrap();
        assert_eq!(b3.get_state(), KMemoryState::FREE);
    }

    #[test]
    fn test_update_at_start_boundary() {
        let mut mgr = KMemoryBlockManager::new();
        mgr.initialize(0x200000, 0x400000).unwrap();

        mgr.update(
            0x200000,
            0x10,
            KMemoryState::CODE,
            KMemoryPermission::USER_READ_EXECUTE,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        // 2 blocks: Code [200000, 210000), Free [210000, 400000)
        assert_eq!(mgr.block_count(), 2);
        assert!(mgr.check_state());
    }

    #[test]
    fn test_adjacent_same_state_coalesces() {
        let mut mgr = KMemoryBlockManager::new();
        mgr.initialize(0x200000, 0x400000).unwrap();

        // Mark [200000, 210000) as Code.
        mgr.update(
            0x200000,
            0x10,
            KMemoryState::CODE,
            KMemoryPermission::USER_READ_EXECUTE,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        // Mark [210000, 220000) as Code with same properties.
        mgr.update(
            0x210000,
            0x10,
            KMemoryState::CODE,
            KMemoryPermission::USER_READ_EXECUTE,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        // Should coalesce into 2 blocks: Code [200000, 220000), Free [220000, 400000).
        assert_eq!(mgr.block_count(), 2);
        assert!(mgr.check_state());

        let b = mgr.find_block(0x200000).unwrap();
        assert_eq!(b.get_size(), 0x20000);
    }

    #[test]
    fn test_query_info() {
        let mut mgr = KMemoryBlockManager::new();
        mgr.initialize(0x200000, 0x400000).unwrap();

        mgr.update(
            0x210000,
            0x10,
            KMemoryState::CODE,
            KMemoryPermission::USER_READ_EXECUTE,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        let info = mgr.query_info(0x215000).unwrap();
        assert_eq!(info.get_address(), 0x210000);
        assert_eq!(info.get_size(), 0x10000);
        assert_eq!(info.get_svc_state(), 0x03); // Code base state

        let info_free = mgr.query_info(0x300000).unwrap();
        assert_eq!(info_free.get_svc_state(), 0x00); // Free
    }

    #[test]
    fn test_multiple_segments() {
        let mut mgr = KMemoryBlockManager::new();
        // Simulating the memory layout of loaded NSOs.
        mgr.initialize(0x0, 0x1_0000_0000).unwrap(); // 4 GiB

        // rtld text: [200000, 20F000) — RX
        mgr.update(
            0x200000,
            (0x20F000 - 0x200000) / PAGE_SIZE,
            KMemoryState::CODE,
            KMemoryPermission::USER_READ_EXECUTE,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        // rtld rodata: [20F000, 210000) — R
        mgr.update(
            0x20F000,
            (0x210000 - 0x20F000) / PAGE_SIZE,
            KMemoryState::CODE,
            KMemoryPermission::USER_READ,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        // rtld data: [210000, 212000) — RW
        mgr.update(
            0x210000,
            (0x212000 - 0x210000) / PAGE_SIZE,
            KMemoryState::CODE_DATA,
            KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        // Stack: [2400000, 2500000) — RW
        mgr.update(
            0x2400000,
            (0x2500000 - 0x2400000) / PAGE_SIZE,
            KMemoryState::STACK,
            KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        assert!(mgr.check_state());

        // Query inside text.
        let info = mgr.query_info(0x205000).unwrap();
        assert_eq!(info.get_address(), 0x200000);
        assert_eq!(info.get_svc_state(), 0x03); // Code

        // Query rodata.
        let info = mgr.query_info(0x20F500).unwrap();
        assert_eq!(info.get_address(), 0x20F000);
        assert_eq!(info.get_svc_state(), 0x03); // Code

        // Query data.
        let info = mgr.query_info(0x211000).unwrap();
        assert_eq!(info.get_address(), 0x210000);
        assert_eq!(info.get_svc_state(), 0x04); // CodeData

        // Query free area between data and stack.
        let info = mgr.query_info(0x300000).unwrap();
        assert_eq!(info.get_svc_state(), 0x00); // Free

        // Query stack.
        let info = mgr.query_info(0x2450000).unwrap();
        assert_eq!(info.get_svc_state(), 0x0B); // Stack
    }
}
