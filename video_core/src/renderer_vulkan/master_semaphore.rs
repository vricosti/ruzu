// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_master_semaphore.h` / `vk_master_semaphore.cpp`.
//!
//! Master timeline semaphore that tracks GPU tick progress and manages
//! fence-based submission synchronization.

use std::collections::VecDeque;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Condvar, Mutex};

use ash::vk;

// ---------------------------------------------------------------------------
// MasterSemaphore
// ---------------------------------------------------------------------------

/// Port of `MasterSemaphore` class.
///
/// Tracks the logical tick (CPU-side counter) and GPU tick (last known
/// completed work), using either timeline semaphores or fence fallback.
pub struct MasterSemaphore {
    /// Timeline semaphore handle (when supported).
    _semaphore: vk::Semaphore,

    /// Current known GPU tick.
    gpu_tick: AtomicU64,

    /// Current logical tick (CPU-side, monotonically increasing).
    current_tick: AtomicU64,

    /// Mutex for the wait queue.
    _wait_mutex: Mutex<()>,

    /// Mutex for the free fence queue.
    _free_mutex: Mutex<VecDeque<vk::Fence>>,

    /// Condition variable for free fences.
    _free_cv: Condvar,

    /// Condition variable for GPU wait.
    _wait_cv: Condvar,
}

impl MasterSemaphore {
    /// Port of `MasterSemaphore::MasterSemaphore`.
    pub fn new() -> Self {
        todo!("MasterSemaphore::new")
    }

    /// Returns the current logical tick.
    /// Port of `MasterSemaphore::CurrentTick`.
    pub fn current_tick(&self) -> u64 {
        self.current_tick.load(Ordering::Acquire)
    }

    /// Returns the last known GPU tick.
    /// Port of `MasterSemaphore::KnownGpuTick`.
    pub fn known_gpu_tick(&self) -> u64 {
        self.gpu_tick.load(Ordering::Acquire)
    }

    /// Returns true when a tick has been completed by the GPU.
    /// Port of `MasterSemaphore::IsFree`.
    pub fn is_free(&self, tick: u64) -> bool {
        self.known_gpu_tick() >= tick
    }

    /// Advance to the next logical tick and return the old one.
    /// Port of `MasterSemaphore::NextTick`.
    pub fn next_tick(&self) -> u64 {
        self.current_tick.fetch_add(1, Ordering::Release)
    }

    /// Refresh the known GPU tick.
    /// Port of `MasterSemaphore::Refresh`.
    pub fn refresh(&self) {
        todo!("MasterSemaphore::refresh")
    }

    /// Wait for a tick to be completed on the GPU.
    /// Port of `MasterSemaphore::Wait`.
    pub fn wait(&self, _tick: u64) {
        todo!("MasterSemaphore::wait")
    }

    /// Submit the device graphics queue, updating the tick.
    /// Port of `MasterSemaphore::SubmitQueue`.
    pub fn submit_queue(
        &self,
        _cmdbuf: vk::CommandBuffer,
        _upload_cmdbuf: vk::CommandBuffer,
        _signal_semaphore: vk::Semaphore,
        _wait_semaphore: vk::Semaphore,
        _host_tick: u64,
    ) -> vk::Result {
        todo!("MasterSemaphore::submit_queue")
    }

    // --- Private ---

    fn submit_queue_timeline(&self) -> vk::Result { todo!() }
    fn submit_queue_fence(&self) -> vk::Result { todo!() }
    fn wait_thread(&self) { todo!() }
    fn get_free_fence(&self) -> vk::Fence { todo!() }
}
