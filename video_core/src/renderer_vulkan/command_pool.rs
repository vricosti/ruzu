// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_command_pool.h` / `vk_command_pool.cpp`.
//!
//! Command pool that extends `ResourcePool` to manage a growable set of
//! `VkCommandPool` / `VkCommandBuffer` pairs.

use ash::vk;

// ---------------------------------------------------------------------------
// CommandPool
// ---------------------------------------------------------------------------

/// Internal pool entry.
struct Pool {
    _handle: vk::CommandPool,
    _cmdbufs: Vec<vk::CommandBuffer>,
}

/// Port of `CommandPool` class.
///
/// Manages multiple `VkCommandPool` objects, each containing a batch of
/// pre-allocated command buffers. Extends `ResourcePool` for tick-based reuse.
pub struct CommandPool {
    pools: Vec<Pool>,
}

impl CommandPool {
    /// Port of `CommandPool::CommandPool`.
    pub fn new() -> Self {
        todo!("CommandPool::new")
    }

    /// Port of `CommandPool::Allocate`.
    pub fn allocate(&mut self, _begin: usize, _end: usize) {
        todo!("CommandPool::allocate")
    }

    /// Port of `CommandPool::Commit`.
    ///
    /// Returns the next available command buffer.
    pub fn commit(&mut self) -> vk::CommandBuffer {
        todo!("CommandPool::commit")
    }
}
