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

    /// Construct a pool whose tick source is supplied by its owner.
    ///
    /// The Rust scheduler currently owns the upstream `MasterSemaphore`
    /// equivalent directly. This keeps the upstream resource-pool algorithm
    /// while allowing that scheduler-owned timeline to drive reuse.
    pub fn new_with_external_ticks(grow_step: usize) -> Self {
        ResourcePool {
            master_semaphore: None,
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
        let ms = Arc::clone(
            self.master_semaphore
                .as_ref()
                .expect("ResourcePool: master_semaphore not set"),
        );
        let found = self
            .find_free(ms.known_gpu_tick(), ms.current_tick())
            .or_else(|| {
                ms.refresh();
                self.find_free(ms.known_gpu_tick(), ms.current_tick())
            });
        let found = found.unwrap_or_else(|| {
            let free_resource = self.manage_overflow(allocate_fn);
            self.ticks[free_resource] = ms.current_tick();
            free_resource
        });
        self.hint_iterator = (found + 1) % self.ticks.len();
        found
    }

    /// `CommitResource` using ticks supplied by the owning scheduler.
    pub fn commit_resource_with_ticks(
        &mut self,
        gpu_tick: u64,
        current_tick: u64,
        allocate_fn: &mut dyn FnMut(usize, usize),
    ) -> usize {
        let found = self.find_free(gpu_tick, current_tick).unwrap_or_else(|| {
            let free_resource = self.manage_overflow(allocate_fn);
            self.ticks[free_resource] = current_tick;
            free_resource
        });

        // Free iterator is hinted to the resource after the one that's been committed.
        self.hint_iterator = (found + 1) % self.ticks.len();
        found
    }

    /// Fallible Rust adaptation of `CommitResource`.
    ///
    /// Upstream propagates allocation failures through exceptions from the
    /// virtual `Allocate` call. Rust callers that allocate Vulkan resources
    /// need the same behavior through `Result`.
    pub fn try_commit_resource_with_ticks<E>(
        &mut self,
        gpu_tick: u64,
        current_tick: u64,
        allocate_fn: &mut dyn FnMut(usize, usize) -> Result<(), E>,
    ) -> Result<usize, E> {
        let search = |ticks: &mut [u64], begin: usize, end: usize| -> Option<usize> {
            for iterator in begin..end {
                if gpu_tick >= ticks[iterator] {
                    ticks[iterator] = current_tick;
                    return Some(iterator);
                }
            }
            None
        };

        let ticks_len = self.ticks.len();
        let hint = self.hint_iterator;
        let found =
            search(&mut self.ticks, hint, ticks_len).or_else(|| search(&mut self.ticks, 0, hint));
        let found = if let Some(found) = found {
            found
        } else {
            let old_capacity = self.ticks.len();
            let new_capacity = old_capacity + self.grow_step;
            allocate_fn(old_capacity, new_capacity)?;
            self.ticks.resize(new_capacity, 0);
            self.ticks[old_capacity] = current_tick;
            old_capacity
        };

        self.hint_iterator = (found + 1) % self.ticks.len();
        Ok(found)
    }

    // --- Private ---

    fn find_free(&mut self, gpu_tick: u64, current_tick: u64) -> Option<usize> {
        let search = |ticks: &mut [u64], begin: usize, end: usize| -> Option<usize> {
            for iterator in begin..end {
                if gpu_tick >= ticks[iterator] {
                    ticks[iterator] = current_tick;
                    return Some(iterator);
                }
            }
            None
        };
        let ticks_len = self.ticks.len();
        let hint = self.hint_iterator;
        search(&mut self.ticks, hint, ticks_len).or_else(|| search(&mut self.ticks, 0, hint))
    }

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

    #[test]
    fn external_ticks_reuse_only_completed_resources() {
        let mut pool = ResourcePool::new_with_external_ticks(2);
        let mut allocations = Vec::new();
        {
            let mut allocate = |begin, end| allocations.push((begin, end));
            assert_eq!(pool.commit_resource_with_ticks(0, 1, &mut allocate), 0);
            assert_eq!(pool.commit_resource_with_ticks(0, 1, &mut allocate), 1);
            assert_eq!(pool.commit_resource_with_ticks(0, 2, &mut allocate), 2);
            assert_eq!(pool.commit_resource_with_ticks(0, 2, &mut allocate), 3);
        }
        assert_eq!(allocations, [(0, 2), (2, 4)]);

        {
            let mut allocate = |begin, end| allocations.push((begin, end));
            assert_eq!(pool.commit_resource_with_ticks(1, 3, &mut allocate), 0);
        }
        assert_eq!(allocations, [(0, 2), (2, 4)]);
    }

    #[test]
    fn failed_growth_does_not_publish_resource_slots() {
        let mut pool = ResourcePool::new_with_external_ticks(2);
        let error = pool
            .try_commit_resource_with_ticks(0, 1, &mut |_, _| Err::<(), _>("allocation failed"))
            .unwrap_err();
        assert_eq!(error, "allocation failed");
        assert!(pool.ticks.is_empty());

        let index = pool
            .try_commit_resource_with_ticks(0, 1, &mut |_, _| Ok::<(), &str>(()))
            .unwrap();
        assert_eq!(index, 0);
        assert_eq!(pool.ticks.len(), 2);
    }
}
