// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/fence_manager.h
//!
//! GPU fence/barrier management for synchronizing GPU and CPU operations.

use std::collections::VecDeque;
use std::sync::{Arc, Condvar, Mutex};

use crate::delayed_destruction_ring::DelayedDestructionRing;

/// Base trait for fence objects.
pub trait FenceBase {
    /// Returns true if this fence is stubbed (no actual GPU wait needed).
    fn is_stubbed(&self) -> bool;
}

/// Trait that fence manager implementations must provide.
pub trait FenceManagerBackend {
    /// The concrete fence type.
    type Fence: FenceBase + Send;

    /// Creates a fence. If `is_stubbed` is true, no backend fence is created.
    fn create_fence(&mut self, is_stubbed: bool) -> Self::Fence;

    /// Queues a fence into the backend if the fence is not stubbed.
    fn queue_fence(&mut self, fence: &Self::Fence);

    /// Returns true if the fence has been signaled by the host GPU.
    fn is_fence_signaled(&self, fence: &Self::Fence) -> bool;

    /// Waits until a fence has been signaled by the host GPU.
    fn wait_fence(&self, fence: &Self::Fence);
}

/// Generic fence manager that coordinates fence lifecycle, async flushes,
/// and deferred operations.
///
/// The upstream C++ uses CRTP templates; here we use a trait-based approach.
pub struct FenceManager<F: FenceBase + Send> {
    fences: VecDeque<F>,
    uncommitted_operations: VecDeque<Box<dyn FnOnce() + Send>>,
    pending_operations: VecDeque<VecDeque<Box<dyn FnOnce() + Send>>>,
    guard: Mutex<()>,
    ring_guard: Mutex<()>,
    cv: Condvar,
    delayed_destruction_ring: DelayedDestructionRing<F, 8>,
}

impl<F: FenceBase + Send> FenceManager<F> {
    pub fn new() -> Self {
        Self {
            fences: VecDeque::new(),
            uncommitted_operations: VecDeque::new(),
            pending_operations: VecDeque::new(),
            guard: Mutex::new(()),
            ring_guard: Mutex::new(()),
            cv: Condvar::new(),
            delayed_destruction_ring: DelayedDestructionRing::new(),
        }
    }

    /// Notify the fence manager about a new frame.
    pub fn tick_frame(&mut self) {
        let _lock = self.ring_guard.lock().unwrap();
        self.delayed_destruction_ring.tick();
    }

    /// Queue a sync operation to execute when the next fence is signaled.
    pub fn sync_operation(&mut self, func: Box<dyn FnOnce() + Send>) {
        self.uncommitted_operations.push_back(func);
    }

    /// Signal a fence with an associated callback.
    ///
    /// Port of `FenceManager::SignalFence()`.
    ///
    /// In the full implementation, this:
    /// 1. Tries to release pending fences
    /// 2. Commits async flushes from texture/buffer/query caches
    /// 3. Creates a new backend fence
    /// 4. Moves uncommitted operations into the pending queue
    /// 5. Queues the fence into the backend
    /// 6. Optionally delays the callback for high GPU accuracy
    pub fn signal_fence(&mut self, func: Box<dyn FnOnce() + Send>) {
        // Try to release any already-signaled fences first
        self.try_release_pending_fences(false);

        // Move uncommitted operations into a new pending batch
        let mut batch: VecDeque<Box<dyn FnOnce() + Send>> =
            std::mem::take(&mut self.uncommitted_operations);

        // In the non-delayed path, execute the callback immediately
        func();

        self.pending_operations.push_back(batch);

        // NOTE: In the full implementation, this would also:
        // - Call CommitAsyncFlushes() on texture/buffer/query caches
        // - Create a backend fence via CreateFence()
        // - Queue the fence via QueueFence()
        // - Optionally call rasterizer.FlushCommands()
        // - Call rasterizer.InvalidateGPUCache()
    }

    /// Signal a syncpoint increment.
    ///
    /// Port of `FenceManager::SignalSyncPoint()`.
    ///
    /// In the full implementation, this increments the guest syncpoint
    /// and creates a fence whose callback increments the host syncpoint.
    pub fn signal_sync_point(&mut self, value: u32) {
        // In the full implementation:
        // syncpoint_manager.IncrementGuest(value);
        // Then call signal_fence with a callback that does IncrementHost(value).
        log::debug!("SignalSyncPoint({})", value);
        let func = Box::new(move || {
            log::debug!("SyncPoint {} host increment", value);
        });
        self.signal_fence(func);
    }

    /// Wait for all pending fences to complete.
    ///
    /// Port of `FenceManager::WaitPendingFences()`.
    pub fn wait_pending_fences(&mut self, force: bool) {
        if !force {
            return;
        }
        // Force-wait: drain all pending fences
        self.try_release_pending_fences(true);
    }

    /// Signal ordering (accumulate flushes).
    ///
    /// Port of `FenceManager::SignalOrdering()`.
    ///
    /// In the full implementation, this tries to release pending fences and
    /// calls buffer_cache.AccumulateFlushes().
    pub fn signal_ordering(&mut self) {
        self.try_release_pending_fences(false);
        // In full implementation: buffer_cache.AccumulateFlushes()
    }

    /// Signal a reference (no-op fence).
    ///
    /// Port of `FenceManager::SignalReference()`.
    pub fn signal_reference(&mut self) {
        let do_nothing: Box<dyn FnOnce() + Send> = Box::new(|| {});
        self.signal_fence(do_nothing);
    }

    /// Try to release pending fences that have been signaled.
    ///
    /// Port of `FenceManager::TryReleasePendingFences()`.
    fn try_release_pending_fences(&mut self, force_wait: bool) {
        while !self.fences.is_empty() {
            // In full implementation: check IsFenceSignaled / WaitFence
            // For now, just drain all pending operations
            if !force_wait {
                // Without force, only process already-signaled fences
                // Since we don't have real fence checking yet, just return
                return;
            }
            self.fences.pop_front();

            if let Some(operations) = self.pending_operations.pop_front() {
                for op in operations {
                    op();
                }
            }
        }
    }
}
