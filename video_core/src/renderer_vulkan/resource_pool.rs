// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_resource_pool.h` / `vk_resource_pool.cpp`.
//!
//! Generic pool of GPU resources protected by timeline tick fences.
//! Automatically grows when all resources are in use.

// ---------------------------------------------------------------------------
// ResourcePool
// ---------------------------------------------------------------------------

/// Port of `ResourcePool` class.
///
/// Base type for managing a growable pool of GPU resources where each
/// resource slot is tagged with a timeline tick. When a slot's tick
/// has been completed by the GPU, the slot can be reused.
pub struct ResourcePool {
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
            grow_step: 0,
            hint_iterator: 0,
            ticks: Vec::new(),
        }
    }

    /// Port of `ResourcePool::ResourcePool(MasterSemaphore&, size_t)`.
    pub fn new(_grow_step: usize) -> Self {
        ResourcePool {
            grow_step: _grow_step,
            hint_iterator: 0,
            ticks: Vec::new(),
        }
    }

    /// Port of `ResourcePool::CommitResource`.
    ///
    /// Finds and returns the index of a free resource slot, growing
    /// the pool if necessary.
    pub fn commit_resource(&mut self, _allocate: &mut dyn FnMut(usize, usize)) -> usize {
        todo!("ResourcePool::commit_resource")
    }

    // --- Private ---

    /// Port of `ResourcePool::ManageOverflow`.
    fn manage_overflow(&mut self) -> usize {
        todo!("ResourcePool::manage_overflow")
    }

    /// Port of `ResourcePool::Grow`.
    fn grow(&mut self) {
        todo!("ResourcePool::grow")
    }
}
