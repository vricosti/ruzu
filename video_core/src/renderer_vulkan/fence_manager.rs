// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

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
/// Upstream extends `VideoCommon::FenceBase`.
pub struct InnerFence {
    is_stubbed: bool,
    wait_tick: u64,
    // In the full implementation, this would hold an Arc to the Scheduler.
    // For now we store the tick and expose query methods.
}

impl InnerFence {
    /// Port of `InnerFence::InnerFence`.
    pub fn new(is_stubbed: bool) -> Self {
        InnerFence {
            is_stubbed,
            wait_tick: 0,
        }
    }

    /// Port of `InnerFence::Queue`.
    ///
    /// Records the current scheduler tick and triggers a flush.
    /// When no scheduler is wired, behaves as if immediately signaled.
    pub fn queue(&mut self, current_tick: u64) {
        if self.is_stubbed {
            return;
        }
        // Get the current tick so we can wait for it
        self.wait_tick = current_tick;
        // In upstream: scheduler.Flush();
        // TODO: Wire to scheduler when integration is complete
    }

    /// Port of `InnerFence::IsSignaled`.
    ///
    /// Returns true if the GPU has completed the tick this fence is waiting on.
    pub fn is_signaled(&self, known_gpu_tick: u64) -> bool {
        if self.is_stubbed {
            return true;
        }
        // In upstream: scheduler.IsFree(wait_tick)
        known_gpu_tick >= self.wait_tick
    }

    /// Port of `InnerFence::Wait`.
    ///
    /// Blocks until the GPU completes the tick this fence is waiting on.
    pub fn wait(&self, _known_gpu_tick: u64) {
        if self.is_stubbed {
            return;
        }
        // In upstream: scheduler.Wait(wait_tick);
        // TODO: Wire to scheduler when integration is complete
    }

    /// Returns the tick this fence is waiting for.
    pub fn wait_tick(&self) -> u64 {
        self.wait_tick
    }

    /// Returns whether this fence is a stub (always signaled).
    pub fn is_stubbed(&self) -> bool {
        self.is_stubbed
    }
}

/// Port of `Fence` type alias (`std::shared_ptr<InnerFence>`).
pub type Fence = Arc<std::sync::Mutex<InnerFence>>;

// ---------------------------------------------------------------------------
// FenceManager
// ---------------------------------------------------------------------------

/// Port of `FenceManager` class.
///
/// Extends `GenericFenceManager` (VideoCommon::FenceManager) with
/// Vulkan-specific fence creation and synchronization.
pub struct FenceManager {
    /// Current scheduler tick for queuing fences.
    /// In upstream this comes from `Scheduler& scheduler`.
    _current_tick: u64,
}

impl FenceManager {
    /// Port of `FenceManager::FenceManager`.
    pub fn new() -> Self {
        FenceManager { _current_tick: 0 }
    }

    /// Port of `FenceManager::CreateFence`.
    pub fn create_fence(&self, is_stubbed: bool) -> Fence {
        Arc::new(std::sync::Mutex::new(InnerFence::new(is_stubbed)))
    }

    /// Port of `FenceManager::QueueFence`.
    pub fn queue_fence(&mut self, fence: &Fence, current_tick: u64) {
        let mut inner = fence.lock().unwrap();
        inner.queue(current_tick);
    }

    /// Port of `FenceManager::IsFenceSignaled`.
    pub fn is_fence_signaled(&self, fence: &Fence, known_gpu_tick: u64) -> bool {
        let inner = fence.lock().unwrap();
        inner.is_signaled(known_gpu_tick)
    }

    /// Port of `FenceManager::WaitFence`.
    pub fn wait_fence(&self, fence: &Fence, known_gpu_tick: u64) {
        let inner = fence.lock().unwrap();
        inner.wait(known_gpu_tick);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stubbed_fence_is_always_signaled() {
        let fence = InnerFence::new(true);
        assert!(fence.is_signaled(0));
    }

    #[test]
    fn fence_not_signaled_before_tick() {
        let mut fence = InnerFence::new(false);
        fence.queue(10);
        assert!(!fence.is_signaled(5));
        assert!(fence.is_signaled(10));
        assert!(fence.is_signaled(15));
    }

    #[test]
    fn fence_manager_create_and_query() {
        let manager = FenceManager::new();
        let fence = manager.create_fence(false);
        {
            let mut inner = fence.lock().unwrap();
            inner.queue(42);
        }
        assert!(!manager.is_fence_signaled(&fence, 41));
        assert!(manager.is_fence_signaled(&fence, 42));
    }
}
