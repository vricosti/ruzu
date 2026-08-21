// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/common/heap_tracker.h and zuyu/src/common/heap_tracker.cpp
//!
//! Tracks separate heap memory mappings by virtual address and by tick for LRU eviction.

use crate::host_memory::{HostMemory, MemoryPermission};
use crate::VAddr;
use std::collections::BTreeMap;
use std::sync::{Mutex, RwLock};

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
        // `RUZU_TRACE_HEAP_TRACKER_MAP=1` logs every map call so we can
        // verify the fastmem-arena MAP_FIXED alias gets installed for a
        // given guest VA region. `RUZU_TRACE_HEAP_TRACKER_MAP=0xVADDR`
        // narrows to calls whose [virtual_offset, +length) covers VADDR.
        if let Ok(spec) = std::env::var("RUZU_TRACE_HEAP_TRACKER_MAP") {
            let log = if spec.trim() == "1" || spec.trim().is_empty() {
                true
            } else if let Ok(target) = u64::from_str_radix(spec.trim().trim_start_matches("0x"), 16)
            {
                let v = virtual_offset as u64;
                v <= target && target < v + length as u64
            } else {
                false
            };
            if log {
                eprintln!(
                    "[HEAP_TRACKER_MAP] vaddr=0x{:X} host_offset=0x{:X} length=0x{:X} is_separate_heap={}",
                    virtual_offset, host_offset, length, is_separate_heap
                );
            }
        }
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

            if let Ok(spec) = std::env::var("RUZU_TRACE_HEAP_TRACKER_MAP") {
                let log = if spec.trim() == "1" || spec.trim().is_empty() {
                    true
                } else if let Ok(target) =
                    u64::from_str_radix(spec.trim().trim_start_matches("0x"), 16)
                {
                    vaddr <= target && target < vaddr + size as u64
                } else {
                    false
                };
                if log {
                    eprintln!(
                        "[HEAP_TRACKER_DEFER_MAP] vaddr=0x{:X} paddr=0x{:X} size=0x{:X} (mmap_fixed alias now live)",
                        vaddr, paddr, size
                    );
                }
            }
            // Map the area.
            self.buffer_mut()
                .map(vaddr as usize, paddr as usize, size, perm, false);

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
