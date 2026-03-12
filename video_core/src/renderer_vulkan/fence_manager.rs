// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_fence_manager.h` / `vk_fence_manager.cpp`.
//!
//! Vulkan fence management using scheduler tick-based synchronization.

use std::sync::Arc;

// ---------------------------------------------------------------------------
// InnerFence
// ---------------------------------------------------------------------------

/// Port of `InnerFence` class.
///
/// A fence that tracks a scheduler tick for GPU completion.
pub struct InnerFence {
    is_stubbed: bool,
    wait_tick: u64,
}

impl InnerFence {
    /// Port of `InnerFence::InnerFence`.
    pub fn new(_is_stubbed: bool) -> Self {
        InnerFence {
            is_stubbed: _is_stubbed,
            wait_tick: 0,
        }
    }

    /// Port of `InnerFence::Queue`.
    pub fn queue(&mut self) {
        todo!("InnerFence::queue")
    }

    /// Port of `InnerFence::IsSignaled`.
    pub fn is_signaled(&self) -> bool {
        todo!("InnerFence::is_signaled")
    }

    /// Port of `InnerFence::Wait`.
    pub fn wait(&self) {
        todo!("InnerFence::wait")
    }
}

/// Port of `Fence` type alias (`std::shared_ptr<InnerFence>`).
pub type Fence = Arc<InnerFence>;

// ---------------------------------------------------------------------------
// FenceManager
// ---------------------------------------------------------------------------

/// Port of `FenceManager` class.
///
/// Extends `GenericFenceManager` (VideoCommon::FenceManager) with
/// Vulkan-specific fence creation and synchronization.
pub struct FenceManager {
    _private: (),
}

impl FenceManager {
    /// Port of `FenceManager::FenceManager`.
    pub fn new() -> Self {
        todo!("FenceManager::new")
    }

    /// Port of `FenceManager::CreateFence`.
    pub fn create_fence(&self, _is_stubbed: bool) -> Fence {
        todo!("FenceManager::create_fence")
    }

    /// Port of `FenceManager::QueueFence`.
    pub fn queue_fence(&mut self, _fence: &mut Fence) {
        todo!("FenceManager::queue_fence")
    }

    /// Port of `FenceManager::IsFenceSignaled`.
    pub fn is_fence_signaled(&self, _fence: &Fence) -> bool {
        todo!("FenceManager::is_fence_signaled")
    }

    /// Port of `FenceManager::WaitFence`.
    pub fn wait_fence(&self, _fence: &Fence) {
        todo!("FenceManager::wait_fence")
    }
}
