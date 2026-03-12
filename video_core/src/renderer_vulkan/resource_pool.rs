// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `vk_resource_pool.h` / `vk_resource_pool.cpp`.
//!
//! Generic pool of GPU resources protected by timeline tick fences.
//! Automatically grows when all resources are in use.

use std::sync::Arc;

use super::master_semaphore::MasterSemaphore;

// ---------------------------------------------------------------------------
// ResourcePool
// ---------------------------------------------------------------------------

/// Port of `ResourcePool` class.
///
/// Base type for managing a growable pool of GPU resources where each
/// resource slot is tagged with a timeline tick. When a slot's tick
/// has been completed by the GPU, the slot can be reused.
pub struct ResourcePool {
    /// Reference to the master semaphore for tick queries.
    master_semaphore: Option<Arc<MasterSemaphore>>,

    /// Number of new resources created on overflow.
    grow_step: usize,

    /// Hint iterator pointing to the likely next free resource.
    hint_iterator: usize,

    /// Timeline tick for each resource slot.
    ticks: Vec<u64>,
}

impl ResourcePool {
    /// Port of `ResourcePool::ResourcePool` (default).
    pub fn new_default() -> Self {
        ResourcePool {
            master_semaphore: None,
            grow_step: 0,
            hint_iterator: 0,
            ticks: Vec::new(),
        }
    }

    /// Port of `ResourcePool::ResourcePool(MasterSemaphore&, size_t)`.
    pub fn new(master_semaphore: Arc<MasterSemaphore>, grow_step: usize) -> Self {
        ResourcePool {
            master_semaphore: Some(master_semaphore),
            grow_step,
            hint_iterator: 0,
            ticks: Vec::new(),
        }
    }

    /// Port of `ResourcePool::CommitResource`.
    ///
    /// Finds and returns the index of a free resource slot, growing
    /// the pool if necessary. Calls `allocate_fn(begin, end)` when new
    /// resources must be created.
    pub fn commit_resource(&mut self, allocate_fn: &mut dyn FnMut(usize, usize)) -> usize {
        let ms = self
            .master_semaphore
            .as_ref()
            .expect("ResourcePool: master_semaphore not set");

        // Refresh semaphore to query updated results
        ms.refresh();
        let gpu_tick = ms.known_gpu_tick();

        // Search helper: finds a free slot in [begin..end)
        let search = |ticks: &mut [u64], begin: usize, end: usize, current_tick: u64| -> Option<usize> {
            for iterator in begin..end {
                if gpu_tick >= ticks[iterator] {
                    ticks[iterator] = current_tick;
                    return Some(iterator);
                }
            }
            None
        };

        let current_tick = ms.current_tick();
        let ticks_len = self.ticks.len();
        let hint = self.hint_iterator;

        // Try to find a free resource from the hinted position to the end.
        let found = search(&mut self.ticks, hint, ticks_len, current_tick);
        let found = match found {
            Some(idx) => idx,
            None => {
                // Search from beginning to the hinted position.
                match search(&mut self.ticks, 0, hint, current_tick) {
                    Some(idx) => idx,
                    None => {
                        // Both searches failed, the pool is full; handle it.
                        let free_resource = self.manage_overflow(allocate_fn);
                        self.ticks[free_resource] = current_tick;
                        free_resource
                    }
                }
            }
        };

        // Free iterator is hinted to the resource after the one that's been committed.
        self.hint_iterator = (found + 1) % self.ticks.len();
        found
    }

    // --- Private ---

    /// Port of `ResourcePool::ManageOverflow`.
    fn manage_overflow(&mut self, allocate_fn: &mut dyn FnMut(usize, usize)) -> usize {
        let old_capacity = self.ticks.len();
        self.grow(allocate_fn);

        // The last entry is guaranteed to be free, since it's the first element
        // of the freshly allocated resources.
        old_capacity
    }

    /// Port of `ResourcePool::Grow`.
    fn grow(&mut self, allocate_fn: &mut dyn FnMut(usize, usize)) {
        let old_capacity = self.ticks.len();
        self.ticks.resize(old_capacity + self.grow_step, 0);
        allocate_fn(old_capacity, old_capacity + self.grow_step);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resource_pool_default() {
        let pool = ResourcePool::new_default();
        assert_eq!(pool.grow_step, 0);
        assert_eq!(pool.hint_iterator, 0);
        assert!(pool.ticks.is_empty());
    }
}
