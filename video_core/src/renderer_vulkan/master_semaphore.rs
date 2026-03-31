// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `vk_master_semaphore.h` / `vk_master_semaphore.cpp`.
//!
//! Master timeline semaphore that tracks GPU tick progress and manages
//! fence-based submission synchronization.

use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Condvar, Mutex};

use ash::vk;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Number of pre-allocated fences for the fence fallback path.
///
/// Port of `FENCE_RESERVE_SIZE` from `vk_master_semaphore.cpp`.
const FENCE_RESERVE_SIZE: usize = 8;

/// Wait stage masks for queue submissions.
///
/// Port of `wait_stage_masks` from `vk_master_semaphore.cpp`.
const WAIT_STAGE_MASKS: [vk::PipelineStageFlags; 2] = [
    vk::PipelineStageFlags::ALL_COMMANDS,
    vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
];

// ---------------------------------------------------------------------------
// MasterSemaphore
// ---------------------------------------------------------------------------

/// Port of `MasterSemaphore` class.
///
/// Tracks the logical tick (CPU-side counter) and GPU tick (last known
/// completed work), using either timeline semaphores or fence fallback.
pub struct MasterSemaphore {
    device: ash::Device,

    /// Timeline semaphore handle (when supported, otherwise null).
    semaphore: vk::Semaphore,

    /// Whether the device supports timeline semaphores.
    has_timeline: bool,

    /// Current known GPU tick.
    gpu_tick: AtomicU64,

    /// Current logical tick (CPU-side, monotonically increasing).
    current_tick: AtomicU64,

    /// Mutex for the wait queue (fence fallback path).
    wait_mutex: Mutex<VecDeque<(u64, vk::Fence)>>,

    /// Mutex for the free fence queue (fence fallback path).
    free_mutex: Mutex<VecDeque<vk::Fence>>,

    /// Condition variable for free fences becoming available.
    free_cv: Condvar,

    /// Condition variable for GPU wait notification.
    wait_cv: Condvar,

    /// Stop flag for the wait thread.
    stop_requested: AtomicBool,

    /// Graphics queue for submissions.
    graphics_queue: vk::Queue,
}

// Safety: MasterSemaphore is designed to be shared across threads via Arc.
// The Vulkan handles are only used through thread-safe atomic operations
// and mutex-protected sections, matching the upstream C++ thread safety model.
unsafe impl Send for MasterSemaphore {}
unsafe impl Sync for MasterSemaphore {}

impl MasterSemaphore {
    /// Port of `MasterSemaphore::MasterSemaphore`.
    ///
    /// Creates a new master semaphore. If `has_timeline` is true, creates
    /// a timeline semaphore; otherwise sets up fence-based fallback.
    pub fn new(device: ash::Device, graphics_queue: vk::Queue, has_timeline: bool) -> Self {
        let semaphore = if has_timeline {
            let type_ci = vk::SemaphoreTypeCreateInfo::builder()
                .semaphore_type(vk::SemaphoreType::TIMELINE)
                .initial_value(0)
                .build();
            let ci = vk::SemaphoreCreateInfo::builder()
                .push_next(&mut vk::SemaphoreTypeCreateInfo { ..type_ci })
                .build();
            // Note: push_next requires mutable reference, use raw construction
            let mut type_ci = vk::SemaphoreTypeCreateInfo {
                s_type: vk::StructureType::SEMAPHORE_TYPE_CREATE_INFO,
                p_next: std::ptr::null(),
                semaphore_type: vk::SemaphoreType::TIMELINE,
                initial_value: 0,
            };
            let ci = vk::SemaphoreCreateInfo {
                s_type: vk::StructureType::SEMAPHORE_CREATE_INFO,
                p_next: &mut type_ci as *mut _ as *mut std::ffi::c_void,
                flags: vk::SemaphoreCreateFlags::empty(),
            };
            unsafe {
                device
                    .create_semaphore(&ci, None)
                    .expect("Failed to create timeline semaphore")
            }
        } else {
            vk::Semaphore::null()
        };

        let free_fences = if !has_timeline {
            let mut fences = VecDeque::with_capacity(FENCE_RESERVE_SIZE);
            let fence_ci = vk::FenceCreateInfo::builder().build();
            for _ in 0..FENCE_RESERVE_SIZE {
                let fence = unsafe {
                    device
                        .create_fence(&fence_ci, None)
                        .expect("Failed to create fence")
                };
                fences.push_back(fence);
            }
            fences
        } else {
            VecDeque::new()
        };

        MasterSemaphore {
            device,
            semaphore,
            has_timeline,
            gpu_tick: AtomicU64::new(0),
            current_tick: AtomicU64::new(1),
            wait_mutex: Mutex::new(VecDeque::new()),
            free_mutex: Mutex::new(free_fences),
            free_cv: Condvar::new(),
            wait_cv: Condvar::new(),
            stop_requested: AtomicBool::new(false),
            graphics_queue,
        }
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

    /// Refresh the known GPU tick from the timeline semaphore counter.
    /// Port of `MasterSemaphore::Refresh`.
    pub fn refresh(&self) {
        if self.semaphore == vk::Semaphore::null() {
            // If we don't support timeline semaphores, there's nothing to refresh
            return;
        }

        loop {
            let this_tick = self.gpu_tick.load(Ordering::Acquire);
            let counter = unsafe {
                self.device
                    .get_semaphore_counter_value(self.semaphore)
                    .unwrap_or(this_tick)
            };
            if counter < this_tick {
                return;
            }
            match self.gpu_tick.compare_exchange_weak(
                this_tick,
                counter,
                Ordering::Release,
                Ordering::Relaxed,
            ) {
                Ok(_) => return,
                Err(_) => continue,
            }
        }
    }

    /// Wait for a tick to be completed on the GPU.
    /// Port of `MasterSemaphore::Wait`.
    pub fn wait(&self, tick: u64) {
        if self.semaphore == vk::Semaphore::null() {
            // Fence fallback: wait for gpu_tick to reach the target
            let lock = self.free_mutex.lock().unwrap();
            let _guard = self
                .free_cv
                .wait_while(lock, |_| self.gpu_tick.load(Ordering::Relaxed) < tick)
                .unwrap();
            return;
        }

        // No need to wait if the GPU is ahead of the tick
        if self.is_free(tick) {
            return;
        }

        // Update the GPU tick and try again
        self.refresh();
        if self.is_free(tick) {
            return;
        }

        // Fallback to a regular timeline semaphore wait
        let semaphores = [self.semaphore];
        let values = [tick];
        let wait_info = vk::SemaphoreWaitInfo::builder()
            .semaphores(&semaphores)
            .values(&values)
            .build();

        loop {
            let result = unsafe { self.device.wait_semaphores(&wait_info, u64::MAX) };
            match result {
                Ok(_) => break,
                Err(vk::Result::TIMEOUT) => continue,
                Err(e) => {
                    log::error!("MasterSemaphore::wait failed: {:?}", e);
                    break;
                }
            }
        }

        self.refresh();
    }

    /// Submit the device graphics queue with timeline semaphore signaling.
    /// Port of `MasterSemaphore::SubmitQueue`.
    pub fn submit_queue(
        &self,
        cmdbuf: vk::CommandBuffer,
        upload_cmdbuf: vk::CommandBuffer,
        signal_semaphore: vk::Semaphore,
        wait_semaphore: vk::Semaphore,
        host_tick: u64,
    ) -> vk::Result {
        if self.has_timeline {
            self.submit_queue_timeline(
                cmdbuf,
                upload_cmdbuf,
                signal_semaphore,
                wait_semaphore,
                host_tick,
            )
        } else {
            self.submit_queue_fence(
                cmdbuf,
                upload_cmdbuf,
                signal_semaphore,
                wait_semaphore,
                host_tick,
            )
        }
    }

    /// Timeline semaphore submission path.
    /// Port of `MasterSemaphore::SubmitQueueTimeline`.
    fn submit_queue_timeline(
        &self,
        cmdbuf: vk::CommandBuffer,
        upload_cmdbuf: vk::CommandBuffer,
        signal_semaphore: vk::Semaphore,
        wait_semaphore: vk::Semaphore,
        host_tick: u64,
    ) -> vk::Result {
        let num_signal_semaphores = if signal_semaphore != vk::Semaphore::null() {
            2u32
        } else {
            1u32
        };
        let signal_values = [host_tick, 0u64];
        let signal_semaphores = [self.semaphore, signal_semaphore];
        let cmdbuffers = [upload_cmdbuf, cmdbuf];

        let num_wait_semaphores = if wait_semaphore != vk::Semaphore::null() {
            1u32
        } else {
            0u32
        };

        let timeline_si = vk::TimelineSemaphoreSubmitInfo {
            s_type: vk::StructureType::TIMELINE_SEMAPHORE_SUBMIT_INFO,
            p_next: std::ptr::null(),
            wait_semaphore_value_count: 0,
            p_wait_semaphore_values: std::ptr::null(),
            signal_semaphore_value_count: num_signal_semaphores,
            p_signal_semaphore_values: signal_values.as_ptr(),
        };

        let submit_info = vk::SubmitInfo {
            s_type: vk::StructureType::SUBMIT_INFO,
            p_next: &timeline_si as *const _ as *const std::ffi::c_void,
            wait_semaphore_count: num_wait_semaphores,
            p_wait_semaphores: &wait_semaphore,
            p_wait_dst_stage_mask: WAIT_STAGE_MASKS.as_ptr(),
            command_buffer_count: cmdbuffers.len() as u32,
            p_command_buffers: cmdbuffers.as_ptr(),
            signal_semaphore_count: num_signal_semaphores,
            p_signal_semaphores: signal_semaphores.as_ptr(),
        };

        unsafe {
            self.device
                .queue_submit(self.graphics_queue, &[submit_info], vk::Fence::null())
                .err()
                .unwrap_or(vk::Result::SUCCESS)
        }
    }

    /// Fence-based submission fallback path.
    /// Port of `MasterSemaphore::SubmitQueueFence`.
    fn submit_queue_fence(
        &self,
        cmdbuf: vk::CommandBuffer,
        upload_cmdbuf: vk::CommandBuffer,
        signal_semaphore: vk::Semaphore,
        wait_semaphore: vk::Semaphore,
        host_tick: u64,
    ) -> vk::Result {
        let num_signal_semaphores = if signal_semaphore != vk::Semaphore::null() {
            1u32
        } else {
            0u32
        };
        let num_wait_semaphores = if wait_semaphore != vk::Semaphore::null() {
            1u32
        } else {
            0u32
        };

        let cmdbuffers = [upload_cmdbuf, cmdbuf];

        let submit_info = vk::SubmitInfo {
            s_type: vk::StructureType::SUBMIT_INFO,
            p_next: std::ptr::null(),
            wait_semaphore_count: num_wait_semaphores,
            p_wait_semaphores: &wait_semaphore,
            p_wait_dst_stage_mask: WAIT_STAGE_MASKS.as_ptr(),
            command_buffer_count: cmdbuffers.len() as u32,
            p_command_buffers: cmdbuffers.as_ptr(),
            signal_semaphore_count: num_signal_semaphores,
            p_signal_semaphores: &signal_semaphore,
        };

        let fence = self.get_free_fence();
        let result = unsafe {
            self.device
                .queue_submit(self.graphics_queue, &[submit_info], fence)
                .err()
                .unwrap_or(vk::Result::SUCCESS)
        };

        if result == vk::Result::SUCCESS {
            let mut wait_queue = self.wait_mutex.lock().unwrap();
            wait_queue.push_back((host_tick, fence));
            self.wait_cv.notify_one();
        }

        result
    }

    /// Get a fence from the free pool, or create a new one.
    /// Port of `MasterSemaphore::GetFreeFence`.
    fn get_free_fence(&self) -> vk::Fence {
        let mut free_queue = self.free_mutex.lock().unwrap();
        if free_queue.is_empty() {
            let fence_ci = vk::FenceCreateInfo::builder().build();
            return unsafe {
                self.device
                    .create_fence(&fence_ci, None)
                    .expect("Failed to create fence")
            };
        }

        free_queue.pop_back().unwrap()
    }

    /// Process completed fences in the wait queue.
    /// Port of `MasterSemaphore::WaitThread`.
    ///
    /// Call this periodically or from a dedicated thread to drain completed fences.
    pub fn process_wait_queue(&self) {
        let mut wait_queue = self.wait_mutex.lock().unwrap();
        while let Some(&(host_tick, fence)) = wait_queue.front() {
            let result = unsafe { self.device.get_fence_status(fence) };
            match result {
                Ok(true) => {
                    // Fence is signaled
                    wait_queue.pop_front();

                    // Reset and return to free pool
                    unsafe {
                        self.device.reset_fences(&[fence]).ok();
                    }

                    {
                        let mut free_queue = self.free_mutex.lock().unwrap();
                        free_queue.push_front(fence);
                        self.gpu_tick.store(host_tick, Ordering::Release);
                    }
                    self.free_cv.notify_one();
                }
                _ => break, // Not yet signaled or error
            }
        }
    }
}

impl Drop for MasterSemaphore {
    fn drop(&mut self) {
        self.stop_requested.store(true, Ordering::Release);

        // Clean up timeline semaphore
        if self.semaphore != vk::Semaphore::null() {
            unsafe {
                self.device.destroy_semaphore(self.semaphore, None);
            }
        }

        // Clean up free fences
        let free_queue = self.free_mutex.lock().unwrap();
        for fence in free_queue.iter() {
            unsafe {
                self.device.destroy_fence(*fence, None);
            }
        }

        // Clean up wait queue fences
        let wait_queue = self.wait_mutex.lock().unwrap();
        for &(_, fence) in wait_queue.iter() {
            unsafe {
                self.device.destroy_fence(fence, None);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constants() {
        assert_eq!(FENCE_RESERVE_SIZE, 8);
        assert_eq!(WAIT_STAGE_MASKS.len(), 2);
    }
}
