//! Port of zuyu/src/common/free_region_manager.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::collections::BTreeMap;
use std::sync::Mutex;

/// Manages free regions in an address space.
/// Replaces boost::icl::interval_set with a BTreeMap-based approach.
///
/// The manager tracks free (unallocated) regions as a set of non-overlapping intervals.
/// When freeing a block, adjacent regions are merged.
pub struct FreeRegionManager {
    mutex: Mutex<FreeRegionManagerInner>,
}

struct FreeRegionManagerInner {
    /// Map from region start address to region end address.
    /// Invariant: regions never overlap and are always maximally merged.
    free_regions: BTreeMap<usize, usize>,
}

impl FreeRegionManager {
    pub fn new() -> Self {
        Self {
            mutex: Mutex::new(FreeRegionManagerInner {
                free_regions: BTreeMap::new(),
            }),
        }
    }

    /// Initialize the address space by marking the entire range as free.
    pub fn set_address_space(&self, start: *mut u8, size: usize) {
        self.free_block(start, size);
    }

    /// Free a block and merge it with adjacent free regions.
    /// Returns the merged (start_ptr, size) after coalescing.
    pub fn free_block(&self, block_ptr: *mut u8, size: usize) -> (*mut u8, usize) {
        let mut inner = self.mutex.lock().unwrap();

        let mut start_address = block_ptr as usize;
        let mut end_address = start_address + size;

        // Check for adjacent regions by finding potential neighbors
        // Find any region that overlaps or is adjacent to [start_address-1, end_address+1]

        // Check left neighbor: find the region whose start is <= start_address
        // and might extend up to or past start_address
        let mut to_remove = Vec::new();

        // Look for regions that could be adjacent or overlapping
        // We need to find regions where:
        //   region.end >= start_address (could be adjacent on left)
        //   region.start <= end_address (could be adjacent on right)
        for (&region_start, &region_end) in inner.free_regions.iter() {
            // Check if this region overlaps or is adjacent
            if region_end >= start_address && region_start <= end_address {
                start_address = std::cmp::min(start_address, region_start);
                end_address = std::cmp::max(end_address, region_end);
                to_remove.push(region_start);
            }
        }

        for key in to_remove {
            inner.free_regions.remove(&key);
        }

        // Insert the merged region
        inner.free_regions.insert(start_address, end_address);

        let new_ptr = start_address as *mut u8;
        let new_size = end_address - start_address;
        (new_ptr, new_size)
    }

    /// Allocate (remove) a block from the free regions.
    pub fn allocate_block(&self, block_ptr: *mut u8, size: usize) {
        let mut inner = self.mutex.lock().unwrap();

        let address = block_ptr as usize;
        let end = address + size;

        // Find and split/remove any regions that overlap with [address, end)
        let mut to_remove = Vec::new();
        let mut to_add = Vec::new();

        for (&region_start, &region_end) in inner.free_regions.iter() {
            if region_start < end && region_end > address {
                // This region overlaps with our allocation
                to_remove.push(region_start);

                // Keep the part before our allocation
                if region_start < address {
                    to_add.push((region_start, address));
                }
                // Keep the part after our allocation
                if region_end > end {
                    to_add.push((end, region_end));
                }
            }
        }

        for key in to_remove {
            inner.free_regions.remove(&key);
        }

        for (start, end_addr) in to_add {
            inner.free_regions.insert(start, end_addr);
        }
    }
}

impl Default for FreeRegionManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_free_and_allocate() {
        let mgr = FreeRegionManager::new();

        // Set up a 1MB address space starting at some address
        let base = 0x10000usize;
        let size = 0x100000usize;
        mgr.set_address_space(base as *mut u8, size);

        // Allocate a block in the middle
        let alloc_addr = 0x20000usize;
        let alloc_size = 0x10000usize;
        mgr.allocate_block(alloc_addr as *mut u8, alloc_size);

        // Free the block back - should merge
        let (merged_ptr, merged_size) = mgr.free_block(alloc_addr as *mut u8, alloc_size);
        assert_eq!(merged_ptr as usize, base);
        assert_eq!(merged_size, size);
    }
}
