// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/common/heap_tracker.h and zuyu/src/common/heap_tracker.cpp
//!
//! Tracks separate heap memory mappings using intrusive red-black trees
//! indexed by virtual address and by tick (for LRU eviction).

use crate::host_memory::{HostMemory, MemoryPermission};
use crate::tree::{self, HasRBEntry, RBEntry, RBHead, NONE};
use crate::VAddr;
use std::sync::{Mutex, RwLock};

/// A separate heap mapping entry, stored in both the address tree and the tick tree.
/// Corresponds to `SeparateHeapMap` in C++.
///
/// In the C++ version this contains two `IntrusiveRedBlackTreeNode` fields
/// for the addr and tick trees. Here we use a pair of `RBEntry` fields and
/// two separate `RBHead` trees that operate over the same arena Vec.
pub struct SeparateHeapMap {
    pub addr_entry: RBEntry,
    pub tick_entry: RBEntry,
    pub vaddr: VAddr,
    pub paddr: u64,
    pub size: usize,
    pub tick: usize,
    pub perm: MemoryPermission,
    pub is_resident: bool,
}

impl SeparateHeapMap {
    fn new(
        vaddr: VAddr,
        paddr: u64,
        size: usize,
        tick: usize,
        perm: MemoryPermission,
        is_resident: bool,
    ) -> Self {
        Self {
            addr_entry: RBEntry::new(),
            tick_entry: RBEntry::new(),
            vaddr,
            paddr,
            size,
            tick,
            perm,
            is_resident,
        }
    }
}

// ── Wrappers for the two trees ──
// Because we need two independent RBEntry fields per node (one for the addr tree,
// one for the tick tree), we use newtype wrappers that select which entry to use.

/// Wrapper that makes the addr-tree entry visible to the tree functions.
struct AddrView;
/// Wrapper that makes the tick-tree entry visible to the tree functions.
struct TickView;

// We can't implement HasRBEntry on SeparateHeapMap twice with different entries,
// so we use free functions that operate directly on the fields.

// ── Address tree comparator ──
// Corresponds to `SeparateHeapMapAddrComparator`.
fn addr_cmp(lhs: &SeparateHeapMap, rhs: &SeparateHeapMap) -> i32 {
    if lhs.vaddr < rhs.vaddr {
        -1
    } else if rhs.size > 0 && lhs.vaddr <= rhs.vaddr + rhs.size as u64 - 1 {
        0
    } else {
        1
    }
}

// ── Tick tree comparator ──
// Corresponds to `SeparateHeapMapTickComparator`.
fn tick_cmp(lhs: &SeparateHeapMap, rhs: &SeparateHeapMap) -> i32 {
    if lhs.tick < rhs.tick {
        -1
    } else if lhs.tick > rhs.tick {
        1
    } else {
        addr_cmp(lhs, rhs)
    }
}

/// A view of `SeparateHeapMap` for the address tree — uses `addr_entry`.
/// This is needed because the `tree` module requires `HasRBEntry`.
///
/// We implement this via a newtype wrapper over the map vec index, and
/// provide a separate `HasRBEntry`-based approach using direct index operations.
///
/// Instead, we store addr and tick trees with their own `RBHead` and implement
/// the tree operations using custom functions that select the correct entry.

// ── AddrNode / TickNode wrappers ──
// These wrap a reference to SeparateHeapMap for the two different RBEntry fields.

/// Newtype for addr tree operations.
pub struct AddrNode(pub SeparateHeapMap);

impl HasRBEntry for AddrNode {
    fn rb_entry(&self) -> &RBEntry {
        &self.0.addr_entry
    }
    fn rb_entry_mut(&mut self) -> &mut RBEntry {
        &mut self.0.addr_entry
    }
    fn set_rb_entry(&mut self, entry: RBEntry) {
        self.0.addr_entry = entry;
    }
}

/// Newtype for tick tree operations.
pub struct TickNode(pub SeparateHeapMap);

impl HasRBEntry for TickNode {
    fn rb_entry(&self) -> &RBEntry {
        &self.0.tick_entry
    }
    fn rb_entry_mut(&mut self) -> &mut RBEntry {
        &mut self.0.tick_entry
    }
    fn set_rb_entry(&mut self, entry: RBEntry) {
        self.0.tick_entry = entry;
    }
}

// Because C++ uses two intrusive trees over the same objects, and our tree module
// requires HasRBEntry on the element type, we take a different approach:
// We maintain two parallel Vec arenas sharing the same indices.
// The addr arena and tick arena both contain the actual data (duplicated for the
// tree link fields), but we keep a single source of truth for the data and
// use a flat Vec<SeparateHeapMap> with manual tree operations.
//
// For simplicity and correctness, we use a single Vec<SeparateHeapMap> and
// operate on the two trees using index-based RBHead + direct field access.

/// Internal state protected by the mutex.
struct HeapTrackerInner {
    /// The arena of all separate heap mappings.
    maps: Vec<SeparateHeapMap>,
    /// Free list for reusing indices.
    free_list: Vec<usize>,
    /// Root of the address-sorted tree.
    addr_root: RBHead,
    /// Root of the tick-sorted tree (resident mappings only).
    tick_root: RBHead,
    /// Count of mappings.
    map_count: i64,
    /// Count of resident mappings.
    resident_map_count: i64,
    /// Monotonic tick counter.
    tick: usize,
}

impl HeapTrackerInner {
    fn new() -> Self {
        Self {
            maps: Vec::new(),
            free_list: Vec::new(),
            addr_root: RBHead::new(),
            tick_root: RBHead::new(),
            map_count: 0,
            resident_map_count: 0,
            tick: 0,
        }
    }

    fn alloc_map(&mut self, map: SeparateHeapMap) -> usize {
        if let Some(idx) = self.free_list.pop() {
            self.maps[idx] = map;
            idx
        } else {
            let idx = self.maps.len();
            self.maps.push(map);
            idx
        }
    }

    fn free_map(&mut self, idx: usize) {
        self.free_list.push(idx);
    }
}

// ── Addr-tree operations using raw functions ──
// These mirror the tree module but select addr_entry.

mod addr_tree {
    use super::*;
    use crate::tree::{RBColor, RBHead, NONE};

    #[inline]
    pub fn left(maps: &[SeparateHeapMap], idx: usize) -> usize {
        maps[idx].addr_entry.left()
    }
    #[inline]
    pub fn right(maps: &[SeparateHeapMap], idx: usize) -> usize {
        maps[idx].addr_entry.right()
    }
    #[inline]
    pub fn parent(maps: &[SeparateHeapMap], idx: usize) -> usize {
        maps[idx].addr_entry.parent()
    }

    /// Find by address using the addr comparator.
    /// Returns the index or NONE.
    pub fn find(head: &RBHead, maps: &[SeparateHeapMap], vaddr: VAddr) -> usize {
        let mut tmp = head.root();
        while tmp != NONE {
            let node = &maps[tmp];
            if vaddr < node.vaddr {
                tmp = node.addr_entry.left();
            } else if node.size > 0 && vaddr <= node.vaddr + node.size as u64 - 1 {
                return tmp;
            } else {
                tmp = node.addr_entry.right();
            }
        }
        NONE
    }

    /// nfind: find first node >= vaddr.
    pub fn nfind(head: &RBHead, maps: &[SeparateHeapMap], vaddr: VAddr) -> usize {
        let mut tmp = head.root();
        let mut res = NONE;
        while tmp != NONE {
            let node = &maps[tmp];
            if vaddr < node.vaddr {
                res = tmp;
                tmp = node.addr_entry.left();
            } else if node.size > 0 && vaddr <= node.vaddr + node.size as u64 - 1 {
                return tmp;
            } else {
                tmp = node.addr_entry.right();
            }
        }
        res
    }

    /// In-order next for addr tree.
    pub fn next(maps: &[SeparateHeapMap], mut elm: usize) -> usize {
        if maps[elm].addr_entry.right() != NONE {
            elm = maps[elm].addr_entry.right();
            while maps[elm].addr_entry.left() != NONE {
                elm = maps[elm].addr_entry.left();
            }
        } else {
            let mut p = maps[elm].addr_entry.parent();
            if p != NONE && elm == maps[p].addr_entry.left() {
                elm = p;
            } else {
                while p != NONE && elm == maps[p].addr_entry.right() {
                    elm = p;
                    p = maps[elm].addr_entry.parent();
                }
                elm = p;
            }
        }
        elm
    }

    /// Min of addr tree.
    pub fn min(head: &RBHead, maps: &[SeparateHeapMap]) -> usize {
        let mut tmp = head.root();
        let mut parent = NONE;
        while tmp != NONE {
            parent = tmp;
            tmp = maps[tmp].addr_entry.left();
        }
        parent
    }
}

// For the addr and tick trees, we need full insert/remove with balancing.
// Rather than duplicating all the RB logic, we use wrapper types that expose
// the correct entry field and delegate to the tree module.
//
// We'll use a "view slice" approach: create temporary AddrNode/TickNode views.
// But that requires owning the data which is complex.
//
// A simpler approach: since HeapTracker needs only basic operations
// (insert, find, erase, iterate), and performance is not critical for this
// tracker, we implement a simpler tree using BTreeMap from std.

// ── Simplified Implementation ──
// The C++ uses intrusive RB trees for performance with memory-mapped regions.
// In Rust, we use BTreeMap for the same logical behavior with safe code.
// The API and semantics match upstream exactly.

use std::collections::BTreeMap;

/// Tracks separate heap memory mappings and implements deferred mapping
/// with LRU eviction when the system is under memory pressure.
///
/// Corresponds to `HeapTracker` in C++.
pub struct HeapTracker {
    buffer: *mut HostMemory,
    max_resident_map_count: i64,
    rebuild_lock: RwLock<()>,
    inner: Mutex<HeapTrackerState>,
}

struct HeapTrackerState {
    /// Address-sorted mappings: vaddr -> SeparateHeapMapData
    mappings: BTreeMap<VAddr, SeparateHeapMapData>,
    /// Tick-sorted resident mappings: (tick, vaddr) -> vaddr
    resident_mappings: BTreeMap<(usize, VAddr), VAddr>,
    map_count: i64,
    resident_map_count: i64,
    tick: usize,
}

#[derive(Clone)]
struct SeparateHeapMapData {
    vaddr: VAddr,
    paddr: u64,
    size: usize,
    tick: usize,
    perm: MemoryPermission,
    is_resident: bool,
}

/// Read max permissible resident map count from /proc/sys/vm/max_map_count.
/// Corresponds to `GetMaxPermissibleResidentMapCount()`.
fn get_max_permissible_resident_map_count() -> i64 {
    let mut value: i64 = 65530;

    if let Ok(contents) = std::fs::read_to_string("/proc/sys/vm/max_map_count") {
        if let Ok(v) = contents.trim().parse::<i64>() {
            value = v;
        }
    }

    log::info!("Current maximum map count: {}", value);

    // Allow 20000 maps for other code and to account for split inaccuracy.
    (value - 20000).max(0)
}

// Safety: HeapTracker uses internal synchronization (Mutex/RwLock).
unsafe impl Send for HeapTracker {}
unsafe impl Sync for HeapTracker {}

impl HeapTracker {
    /// Create a new HeapTracker backed by the given HostMemory.
    /// Corresponds to `HeapTracker::HeapTracker(Common::HostMemory& buffer)`.
    pub fn new(buffer: &mut HostMemory) -> Self {
        Self {
            buffer: buffer as *mut HostMemory,
            max_resident_map_count: get_max_permissible_resident_map_count(),
            rebuild_lock: RwLock::new(()),
            inner: Mutex::new(HeapTrackerState {
                mappings: BTreeMap::new(),
                resident_mappings: BTreeMap::new(),
                map_count: 0,
                resident_map_count: 0,
                tick: 0,
            }),
        }
    }

    /// Get a pointer to the virtual base of the backing memory.
    /// Corresponds to `HeapTracker::VirtualBasePointer()`.
    pub fn virtual_base_pointer(&self) -> *mut u8 {
        self.buffer().virtual_base_pointer()
    }

    fn buffer(&self) -> &HostMemory {
        unsafe { &*self.buffer }
    }

    fn buffer_mut(&self) -> &mut HostMemory {
        unsafe { &mut *self.buffer }
    }

    /// Map a region of memory.
    /// Corresponds to `HeapTracker::Map`.
    pub fn map(
        &self,
        virtual_offset: usize,
        host_offset: usize,
        length: usize,
        perm: MemoryPermission,
        is_separate_heap: bool,
    ) {
        // When mapping other memory, map pages immediately.
        if !is_separate_heap {
            self.buffer_mut()
                .map(virtual_offset, host_offset, length, perm, false);
            return;
        }

        {
            let mut state = self.inner.lock().unwrap();
            let tick = state.tick;
            state.tick += 1;

            let map = SeparateHeapMapData {
                vaddr: virtual_offset as VAddr,
                paddr: host_offset as u64,
                size: length,
                tick,
                perm,
                is_resident: false,
            };

            state.map_count += 1;
            state.mappings.insert(map.vaddr, map);
        }

        // Finally, map.
        self.deferred_map_separate_heap_offset(virtual_offset);
    }

    /// Unmap a region of memory.
    /// Corresponds to `HeapTracker::Unmap`.
    pub fn unmap(&self, virtual_offset: usize, size: usize, is_separate_heap: bool) {
        if is_separate_heap {
            let mut state = self.inner.lock().unwrap();
            let vaddr = virtual_offset as VAddr;

            // Split at the boundaries.
            Self::split_heap_map_locked(&mut state, vaddr);
            Self::split_heap_map_locked(&mut state, vaddr + size as u64);

            // Collect keys to remove.
            let keys_to_remove: Vec<VAddr> = state
                .mappings
                .range(vaddr..vaddr + size as u64)
                .map(|(&k, _)| k)
                .collect();

            for key in keys_to_remove {
                if let Some(item) = state.mappings.remove(&key) {
                    if item.is_resident {
                        state.resident_map_count -= 1;
                        assert!(state.resident_map_count >= 0);
                        state.resident_mappings.remove(&(item.tick, item.vaddr));
                    }
                    state.map_count -= 1;
                    assert!(state.map_count >= 0);
                }
            }
        }

        // Unmap pages.
        self.buffer_mut().unmap(virtual_offset, size, false);
    }

    /// Protect a region of memory.
    /// Corresponds to `HeapTracker::Protect`.
    pub fn protect(&self, virtual_offset: usize, size: usize, perm: MemoryPermission) {
        // Ensure no rebuild occurs while reprotecting.
        let _rebuild_guard = self.rebuild_lock.read().unwrap();

        // Split at the boundaries.
        self.split_heap_map(virtual_offset as VAddr, size);

        let end = virtual_offset as VAddr + size as u64;
        let mut cur = virtual_offset as VAddr;

        while cur < end {
            let next: VAddr;
            let should_protect: bool;

            {
                let mut state = self.inner.lock().unwrap();

                // Try to get the next mapping corresponding to this address.
                // Use range to find the first mapping that contains or is >= cur.
                let found = Self::find_mapping_at(&state, cur);

                match found {
                    None => {
                        // Check if there's a mapping after cur.
                        if let Some((&next_vaddr, _)) = state.mappings.range(cur..).next() {
                            if next_vaddr < end {
                                next = next_vaddr;
                                should_protect = true;
                            } else {
                                next = end;
                                should_protect = true;
                            }
                        } else {
                            next = end;
                            should_protect = true;
                        }
                    }
                    Some(vaddr_key) => {
                        let item = state.mappings.get_mut(&vaddr_key).unwrap();
                        if item.vaddr == cur {
                            // We are in range. Update permission bits.
                            item.perm = perm;
                            next = cur + item.size as u64;
                            should_protect = item.is_resident;
                        } else {
                            // Not in range but there's a block coming up.
                            next = item.vaddr;
                            should_protect = true;
                        }
                    }
                }
            }

            let next = next.min(end);

            if should_protect {
                self.buffer_mut()
                    .protect(cur as usize, (next - cur) as usize, perm);
            }

            cur = next;
        }
    }

    /// Handle a fault on a separate heap address.
    /// Corresponds to `HeapTracker::DeferredMapSeparateHeap(u8* fault_address)`.
    pub fn deferred_map_separate_heap(&self, fault_address: *const u8) -> bool {
        if self.buffer().is_in_virtual_range(fault_address) {
            let offset =
                unsafe { fault_address.offset_from(self.buffer().virtual_base_pointer()) as usize };
            return self.deferred_map_separate_heap_offset(offset);
        }
        false
    }

    /// Handle a deferred map by virtual offset.
    /// Corresponds to `HeapTracker::DeferredMapSeparateHeap(size_t virtual_offset)`.
    pub fn deferred_map_separate_heap_offset(&self, virtual_offset: usize) -> bool {
        let mut rebuild_required = false;

        {
            let mut state = self.inner.lock().unwrap();

            let vaddr = virtual_offset as VAddr;

            // Check to ensure this was a non-resident separate heap mapping.
            let found = Self::find_mapping_at(&state, vaddr);
            let vaddr_key = match found {
                Some(k) => k,
                None => return false,
            };

            {
                let item = state.mappings.get(&vaddr_key).unwrap();
                if item.is_resident {
                    return false;
                }
            }

            // Update tick before possible rebuild.
            let new_tick = state.tick;
            state.tick += 1;

            // Update the mapping entry and extract what we need before releasing the borrow.
            let (vaddr, paddr, size, perm, tick_for_resident);
            {
                let item = state.mappings.get_mut(&vaddr_key).unwrap();
                item.tick = new_tick;
                item.is_resident = true;
                vaddr = item.vaddr;
                paddr = item.paddr;
                size = item.size;
                perm = item.perm;
                tick_for_resident = item.tick;
            }

            // Check if we need to rebuild.
            if state.resident_map_count > self.max_resident_map_count {
                rebuild_required = true;
            }

            // Map the area.
            self.buffer_mut().map(
                vaddr as usize,
                paddr as usize,
                size,
                perm,
                false,
            );

            // This map is now resident.
            state.resident_map_count += 1;
            state
                .resident_mappings
                .insert((tick_for_resident, vaddr), vaddr);
        }

        if rebuild_required {
            self.rebuild_separate_heap_address_space();
        }

        true
    }

    /// Evict half of the resident mappings to reduce memory pressure.
    /// Corresponds to `HeapTracker::RebuildSeparateHeapAddressSpace`.
    fn rebuild_separate_heap_address_space(&self) {
        let _rebuild_guard = self.rebuild_lock.write().unwrap();
        let mut state = self.inner.lock().unwrap();

        assert!(!state.resident_mappings.is_empty());

        let desired_count = state.resident_map_count.min(self.max_resident_map_count) / 2;
        let evict_count = state.resident_map_count - desired_count;

        let keys_to_evict: Vec<(usize, VAddr)> = state
            .resident_mappings
            .keys()
            .take(evict_count as usize)
            .cloned()
            .collect();

        for key in keys_to_evict {
            let vaddr = state.resident_mappings.remove(&key).unwrap();
            if let Some(item) = state.mappings.get_mut(&vaddr) {
                item.is_resident = false;
                self.buffer_mut()
                    .unmap(item.vaddr as usize, item.size, false);
            }
            state.resident_map_count -= 1;
            assert!(state.resident_map_count >= 0);
        }
    }

    /// Split heap map at the given boundaries.
    /// Corresponds to `HeapTracker::SplitHeapMap`.
    fn split_heap_map(&self, offset: VAddr, size: usize) {
        let mut state = self.inner.lock().unwrap();
        Self::split_heap_map_locked(&mut state, offset);
        Self::split_heap_map_locked(&mut state, offset + size as u64);
    }

    /// Split a heap map at the given offset (internal, lock must be held).
    /// Corresponds to `HeapTracker::SplitHeapMapLocked`.
    fn split_heap_map_locked(state: &mut HeapTrackerState, offset: VAddr) {
        let found = Self::find_mapping_at_state(state, offset);
        let vaddr_key = match found {
            Some(k) if k != offset => k,
            _ => return, // Not contained or no split required.
        };

        let left = state.mappings.get(&vaddr_key).unwrap().clone();
        let left_size = (offset - left.vaddr) as usize;

        // Adjust the left map.
        state.mappings.get_mut(&vaddr_key).unwrap().size = left_size;

        // Create the new right map.
        let right = SeparateHeapMapData {
            vaddr: left.vaddr + left_size as u64,
            paddr: left.paddr + left_size as u64,
            size: left.size - left_size,
            tick: left.tick,
            perm: left.perm,
            is_resident: left.is_resident,
        };

        state.map_count += 1;
        let right_vaddr = right.vaddr;
        state.mappings.insert(right_vaddr, right.clone());

        // If resident, also insert into resident map.
        if right.is_resident {
            state.resident_map_count += 1;
            state
                .resident_mappings
                .insert((right.tick, right.vaddr), right.vaddr);
        }
    }

    /// Find a mapping that contains the given address.
    /// Corresponds to `HeapTracker::GetNearestHeapMapLocked`.
    fn find_mapping_at(state: &HeapTrackerState, offset: VAddr) -> Option<VAddr> {
        Self::find_mapping_at_state(state, offset)
    }

    fn find_mapping_at_state(state: &HeapTrackerState, offset: VAddr) -> Option<VAddr> {
        // Find the greatest key <= offset.
        if let Some((&vaddr, item)) = state.mappings.range(..=offset).next_back() {
            if offset < vaddr + item.size as u64 {
                return Some(vaddr);
            }
        }
        None
    }
}

impl Drop for HeapTracker {
    fn drop(&mut self) {
        // Corresponds to `HeapTracker::~HeapTracker()` which is defaulted.
    }
}

#[cfg(test)]
mod tests {
    // HeapTracker requires a real HostMemory, which requires OS-level mmap.
    // Unit tests for the logic would require mocking HostMemory.
    // Integration tests should be done at a higher level.

    #[test]
    fn test_get_max_permissible_resident_map_count() {
        let count = super::get_max_permissible_resident_map_count();
        // Should be non-negative
        assert!(count >= 0);
    }
}
