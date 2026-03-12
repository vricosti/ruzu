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
    /// In the full port, this interacts with the rasterizer, texture cache,
    /// buffer cache, and query cache.
    pub fn signal_fence(&mut self, _func: Box<dyn FnOnce() + Send>) {
        todo!("signal_fence requires rasterizer and cache integration");
    }

    /// Signal a syncpoint increment.
    pub fn signal_sync_point(&mut self, _value: u32) {
        todo!("signal_sync_point requires syncpoint manager integration");
    }

    /// Wait for all pending fences to complete.
    pub fn wait_pending_fences(&mut self, _force: bool) {
        todo!("wait_pending_fences requires backend fence integration");
    }

    /// Signal ordering (accumulate flushes).
    pub fn signal_ordering(&mut self) {
        todo!("signal_ordering requires buffer cache integration");
    }

    /// Signal a reference (no-op fence).
    pub fn signal_reference(&mut self) {
        todo!("signal_reference requires signal_fence");
    }
}
