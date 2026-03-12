// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `vk_turbo_mode.h` / `vk_turbo_mode.cpp`.
//!
//! Keeps the GPU clocked at maximum frequency by submitting periodic
//! no-op work to prevent idle clock-down.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::time::{Duration, Instant};

use ash::vk;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Buffer size for the turbo mode compute shader workload (2 MiB).
///
/// Port of the `2_MiB` literal in `TurboMode::Run`.
const TURBO_BUFFER_SIZE: u64 = 2 * 1024 * 1024;

/// Maximum idle time before submitting a keep-alive dispatch.
///
/// Port of `std::chrono::milliseconds{100}` in `TurboMode::Run`.
const IDLE_TIMEOUT: Duration = Duration::from_millis(100);

// ---------------------------------------------------------------------------
// TurboMode
// ---------------------------------------------------------------------------

/// Port of `TurboMode` class.
///
/// On non-Android platforms, creates a secondary Vulkan device and
/// periodically submits trivial compute dispatches to prevent the GPU
/// from reducing its clock speed.
pub struct TurboMode {
    submission_time: Mutex<Instant>,
    submission_cv: Condvar,
    stop_flag: Arc<AtomicBool>,
    thread_handle: Option<std::thread::JoinHandle<()>>,
}

impl TurboMode {
    /// Port of `TurboMode::TurboMode`.
    ///
    /// In the full implementation, this would create a secondary Vulkan device,
    /// allocate a buffer, descriptor pool, compute pipeline, and command pool
    /// for submitting periodic keep-alive work. The thread periodically submits
    /// a trivial compute dispatch to prevent GPU clock-down.
    ///
    /// Currently creates the background thread with a simplified keep-alive loop
    /// that only tracks submission timing (actual GPU work submission requires
    /// the secondary device infrastructure).
    pub fn new(_instance: vk::Instance) -> Self {
        let submission_time = Mutex::new(Instant::now());
        let submission_cv = Condvar::new();
        let stop_flag = Arc::new(AtomicBool::new(false));

        // In upstream, the constructor creates:
        // - A secondary Device via CreateDevice()
        // - A MemoryAllocator for that device
        // - A 2MiB storage buffer
        // - A descriptor pool, set layout, descriptor set
        // - A compute shader module (from vulkan_turbo_mode_comp_spv)
        // - A pipeline layout and compute pipeline
        // - A fence, command pool, and command buffer
        //
        // Then spawns a thread that periodically:
        // 1. Resets the fence
        // 2. Updates the descriptor set
        // 3. Records a command buffer (fill buffer + bind + dispatch 64x64x1)
        // 4. Submits and waits on the fence
        //
        // TODO: Implement full secondary device creation and compute dispatch
        // when vulkan_common/vulkan_device infrastructure supports it.

        let stop = stop_flag.clone();
        let thread_handle = std::thread::Builder::new()
            .name("TurboMode".to_string())
            .spawn(move || {
                while !stop.load(Ordering::Relaxed) {
                    // In the full implementation, this would submit a compute
                    // dispatch to the secondary device, then wait for the
                    // condition variable with the idle timeout.
                    //
                    // For now, just sleep to avoid busy-waiting.
                    std::thread::sleep(IDLE_TIMEOUT);
                }
            })
            .expect("Failed to spawn TurboMode thread");

        TurboMode {
            submission_time,
            submission_cv,
            stop_flag,
            thread_handle: Some(thread_handle),
        }
    }

    /// Port of `TurboMode::QueueSubmitted`.
    ///
    /// Called after each GPU queue submission to reset the idle timer.
    pub fn queue_submitted(&self) {
        let mut time = self.submission_time.lock().unwrap();
        *time = Instant::now();
        self.submission_cv.notify_one();
    }
}

impl Drop for TurboMode {
    fn drop(&mut self) {
        self.stop_flag.store(true, Ordering::Release);
        self.submission_cv.notify_all();
        if let Some(handle) = self.thread_handle.take() {
            handle.join().ok();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn turbo_buffer_size() {
        assert_eq!(TURBO_BUFFER_SIZE, 2 * 1024 * 1024);
    }

    #[test]
    fn idle_timeout_is_100ms() {
        assert_eq!(IDLE_TIMEOUT, Duration::from_millis(100));
    }
}
