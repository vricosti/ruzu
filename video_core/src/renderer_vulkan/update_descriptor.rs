// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `vk_update_descriptor.h` / `vk_update_descriptor.cpp`.
//!
//! Ring-buffered descriptor update queue. Uses a fixed-size payload buffer
//! that is partitioned into per-frame slices.

use super::scheduler::Scheduler;
use crate::vulkan_common::vulkan_device::{Device, DeviceReference};
use ash::vk;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Number of frames in flight for payload ring buffering.
///
/// Port of `UpdateDescriptorQueue::FRAMES_IN_FLIGHT`.
const FRAMES_IN_FLIGHT: usize = 8;

/// Per-frame guest-pipeline payload capacity.
///
/// Port of `UpdateDescriptorQueue::GUEST_FRAME_PAYLOAD_SIZE`.
pub const GUEST_FRAME_PAYLOAD_SIZE: usize = 0x80000;

/// Per-frame internal-compute-pass payload capacity.
///
/// Port of `UpdateDescriptorQueue::COMPUTE_FRAME_PAYLOAD_SIZE`.
pub const COMPUTE_FRAME_PAYLOAD_SIZE: usize = 0x20000;

// ---------------------------------------------------------------------------
// DescriptorUpdateEntry
// ---------------------------------------------------------------------------

/// A single descriptor update entry.
///
/// Upstream deliberately uses a C union because Vulkan descriptor update
/// templates consume this payload as raw bytes with
/// `sizeof(DescriptorUpdateEntry)` stride. A tagged Rust enum changes both the
/// size and field offsets and therefore cannot back the template path.
#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct DescriptorUpdateEmpty;

#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct DescriptorAddress {
    pub address: vk::DeviceAddress,
    pub range: vk::DeviceSize,
    pub format: vk::Format,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union DescriptorUpdateEntry {
    pub empty: DescriptorUpdateEmpty,
    pub image: vk::DescriptorImageInfo,
    pub buffer: vk::DescriptorBufferInfo,
    pub texel_buffer: vk::BufferView,
    pub address: DescriptorAddress,
}

impl Default for DescriptorUpdateEntry {
    fn default() -> Self {
        // Upstream initializes the union's `Empty empty{}` member. Zero the
        // complete storage as well so padding remains deterministic when the
        // payload is consumed as raw template data.
        unsafe { std::mem::zeroed() }
    }
}

// ---------------------------------------------------------------------------
// UpdateDescriptorQueue
// ---------------------------------------------------------------------------

/// Ring-buffered descriptor update queue.
///
/// Port of `UpdateDescriptorQueue` class.
///
/// Accumulates descriptor payload entries via `add_sampled_image`, `add_image`,
/// `add_buffer`, and `add_texel_buffer`. The caller uses `update_data()` to
/// retrieve the entries written since the last `acquire()` call. Vulkan
/// consumes that raw payload through a descriptor update template.
pub struct UpdateDescriptorQueue {
    device: DeviceReference,

    /// Current frame index in the ring buffer.
    frame_index: usize,

    /// Number of descriptor entries reserved for each frame.
    ///
    /// Port of upstream `frame_payload_size`.
    frame_payload_size: usize,

    /// Cursor into the payload for writing new entries.
    cursor: usize,

    /// Start of the current frame's payload slice.
    frame_start: usize,

    /// Start of the current upload batch (set by `acquire()`).
    upload_start: usize,

    /// Fixed-size ring buffer of descriptor entries.
    payload: Vec<DescriptorUpdateEntry>,
    supports_descriptor_buffer: bool,
    use_descriptor_buffer: bool,
}

impl UpdateDescriptorQueue {
    /// Port of `UpdateDescriptorQueue::UpdateDescriptorQueue`.
    pub fn new(
        device: &Device,
        frame_payload_size: usize,
        supports_descriptor_buffer: bool,
    ) -> Self {
        Self {
            device: DeviceReference::new(device),
            frame_index: 0,
            frame_payload_size,
            cursor: 0,
            frame_start: 0,
            upload_start: 0,
            payload: vec![DescriptorUpdateEntry::default(); frame_payload_size * FRAMES_IN_FLIGHT],
            supports_descriptor_buffer,
            use_descriptor_buffer: false,
        }
    }

    /// Advance to the next frame's payload slice.
    ///
    /// Port of `UpdateDescriptorQueue::TickFrame`.
    pub fn tick_frame(&mut self) {
        self.frame_index += 1;
        if self.frame_index >= FRAMES_IN_FLIGHT {
            self.frame_index = 0;
        }
        self.frame_start = self.frame_index * self.frame_payload_size;
        self.cursor = self.frame_start;
    }

    /// Begin a new batch of descriptor updates.
    ///
    /// Port of `UpdateDescriptorQueue::Acquire`.
    ///
    /// If the remaining space in the current frame is insufficient,
    /// waits for the scheduler worker before recycling the frame slice.
    pub fn acquire(
        &mut self,
        scheduler: &mut Scheduler,
        required_entries: usize,
        use_descriptor_buffer: bool,
    ) {
        self.use_descriptor_buffer = self.supports_descriptor_buffer && use_descriptor_buffer;
        self.acquire_with_wait(required_entries, || scheduler.wait_worker());
    }

    fn acquire_with_wait(&mut self, required_entries: usize, wait_worker: impl FnOnce()) {
        // Port of the function-local constant in
        // `UpdateDescriptorQueue::Acquire`.
        const DEFAULT_REQUIRED_ENTRIES: usize = 0x400;
        let reserve = if required_entries == 0 {
            DEFAULT_REQUIRED_ENTRIES
        } else {
            required_entries
        };
        assert!(
            reserve < self.frame_payload_size,
            "descriptor reservation {reserve} >= frame capacity {}",
            self.frame_payload_size
        );
        let used = self.cursor - self.frame_start;
        if used + reserve >= self.frame_payload_size {
            log::warn!(
                "Payload overflow (used={}, reserve={}, capacity={})",
                used,
                reserve,
                self.frame_payload_size
            );
            wait_worker();
            self.cursor = self.frame_start;
        }
        self.upload_start = self.cursor;
    }

    /// Returns the first entry written since the last `acquire()`.
    ///
    /// Port of `UpdateDescriptorQueue::UpdateData`.
    pub fn update_data(&self) -> *const DescriptorUpdateEntry {
        unsafe { self.payload.as_ptr().add(self.upload_start) }
    }

    /// Queue a combined image sampler descriptor entry.
    ///
    /// Port of `UpdateDescriptorQueue::AddSampledImage`.
    pub fn add_sampled_image(&mut self, image_view: vk::ImageView, sampler: vk::Sampler) {
        self.payload[self.cursor] = DescriptorUpdateEntry {
            image: vk::DescriptorImageInfo {
                sampler,
                image_view,
                image_layout: vk::ImageLayout::GENERAL,
            },
        };
        self.cursor += 1;
    }

    /// Queue a storage image descriptor entry.
    ///
    /// Port of `UpdateDescriptorQueue::AddImage`.
    pub fn add_image(&mut self, image_view: vk::ImageView) {
        self.payload[self.cursor] = DescriptorUpdateEntry {
            image: vk::DescriptorImageInfo {
                sampler: vk::Sampler::null(),
                image_view,
                image_layout: vk::ImageLayout::GENERAL,
            },
        };
        self.cursor += 1;
    }

    /// Queue a buffer descriptor entry.
    ///
    /// Port of `UpdateDescriptorQueue::AddBuffer`.
    pub fn add_buffer(&mut self, buffer: vk::Buffer, offset: vk::DeviceSize, size: vk::DeviceSize) {
        self.payload[self.cursor] = DescriptorUpdateEntry {
            buffer: vk::DescriptorBufferInfo {
                buffer,
                offset,
                range: size,
            },
        };
        self.cursor += 1;
    }

    pub fn add_buffer_with_address(
        &mut self,
        buffer: vk::Buffer,
        base_address: vk::DeviceAddress,
        offset: vk::DeviceSize,
        size: vk::DeviceSize,
    ) {
        if !self.use_descriptor_buffer {
            self.add_buffer(buffer, offset, size);
            return;
        }
        self.payload[self.cursor] = DescriptorUpdateEntry {
            address: DescriptorAddress {
                address: if base_address == 0 {
                    0
                } else {
                    base_address + offset
                },
                range: if base_address == 0 {
                    vk::WHOLE_SIZE
                } else {
                    size
                },
                format: vk::Format::UNDEFINED,
            },
        };
        self.cursor += 1;
    }

    /// Queue a texel buffer descriptor entry.
    ///
    /// Port of `UpdateDescriptorQueue::AddTexelBuffer`.
    pub fn add_texel_buffer(&mut self, texel_buffer: vk::BufferView) {
        self.payload[self.cursor] = DescriptorUpdateEntry { texel_buffer };
        self.cursor += 1;
    }

    pub fn add_texel_buffer_with_address(
        &mut self,
        texel_buffer: vk::BufferView,
        base_address: vk::DeviceAddress,
        offset: vk::DeviceSize,
        size: vk::DeviceSize,
        format: vk::Format,
    ) {
        if !self.use_descriptor_buffer {
            self.add_texel_buffer(texel_buffer);
            return;
        }
        self.payload[self.cursor] = DescriptorUpdateEntry {
            address: DescriptorAddress {
                address: if base_address == 0 {
                    0
                } else {
                    base_address + offset
                },
                range: if base_address == 0 {
                    vk::WHOLE_SIZE
                } else {
                    size
                },
                format,
            },
        };
        self.cursor += 1;
    }

    pub fn uses_descriptor_buffer(&self) -> bool {
        self.use_descriptor_buffer
    }

    /// Returns the number of entries written since the last `acquire()`.
    pub fn pending_count(&self) -> usize {
        self.cursor - self.upload_start
    }
}

/// Type alias matching upstream.
pub type GuestDescriptorQueue = UpdateDescriptorQueue;

/// Type alias matching upstream.
pub type ComputePassDescriptorQueue = UpdateDescriptorQueue;

#[cfg(test)]
mod tests {
    use super::*;

    fn test_queue() -> UpdateDescriptorQueue {
        UpdateDescriptorQueue {
            device: DeviceReference::dangling_for_test(),
            frame_index: 0,
            frame_payload_size: COMPUTE_FRAME_PAYLOAD_SIZE,
            cursor: 0,
            frame_start: 0,
            upload_start: 0,
            payload: vec![
                DescriptorUpdateEntry::default();
                COMPUTE_FRAME_PAYLOAD_SIZE * FRAMES_IN_FLIGHT
            ],
            supports_descriptor_buffer: false,
            use_descriptor_buffer: false,
        }
    }

    #[test]
    fn constants_match_upstream() {
        assert_eq!(FRAMES_IN_FLIGHT, 8);
        assert_eq!(GUEST_FRAME_PAYLOAD_SIZE, 0x80000);
        assert_eq!(COMPUTE_FRAME_PAYLOAD_SIZE, 0x20000);
        let largest_member = std::mem::size_of::<vk::DescriptorImageInfo>()
            .max(std::mem::size_of::<vk::DescriptorBufferInfo>())
            .max(std::mem::size_of::<vk::BufferView>())
            .max(std::mem::size_of::<DescriptorAddress>());
        assert_eq!(std::mem::size_of::<DescriptorUpdateEntry>(), largest_member);
        assert_eq!(
            std::mem::align_of::<DescriptorUpdateEntry>(),
            std::mem::align_of::<DescriptorAddress>()
        );
    }

    #[test]
    fn basic_acquire_and_add() {
        let mut queue = test_queue();
        queue.acquire_with_wait(0, || panic!("worker wait is not needed"));

        queue.add_buffer(vk::Buffer::null(), 0, 256);
        queue.add_sampled_image(vk::ImageView::null(), vk::Sampler::null());

        assert_eq!(queue.pending_count(), 2);
        let data = queue.update_data();
        unsafe {
            assert_eq!((*data).buffer.range, 256);
            assert_eq!((*data.add(1)).image.image_layout, vk::ImageLayout::GENERAL);
        }
    }

    #[test]
    fn tick_frame_advances_ring() {
        let mut queue = test_queue();
        assert_eq!(queue.frame_start, 0);

        queue.tick_frame();
        assert_eq!(queue.frame_start, COMPUTE_FRAME_PAYLOAD_SIZE);
        assert_eq!(queue.cursor, COMPUTE_FRAME_PAYLOAD_SIZE);

        // Wrap around
        for _ in 0..FRAMES_IN_FLIGHT {
            queue.tick_frame();
        }
        assert_eq!(queue.frame_start, COMPUTE_FRAME_PAYLOAD_SIZE); // wrapped back to 1
    }

    #[test]
    fn acquire_waits_for_worker_before_recycling_overflowed_frame_slice() {
        let mut queue = test_queue();
        queue.cursor = COMPUTE_FRAME_PAYLOAD_SIZE - 0x400;
        let mut waits = 0;

        queue.acquire_with_wait(0, || waits += 1);

        assert_eq!(waits, 1);
        assert_eq!(queue.cursor, queue.frame_start);
        assert_eq!(queue.upload_start, queue.frame_start);
    }

    #[test]
    fn acquire_reserves_the_caller_requested_descriptor_count() {
        let mut queue = test_queue();
        queue.cursor = COMPUTE_FRAME_PAYLOAD_SIZE - 32;
        let mut waits = 0;

        queue.acquire_with_wait(64, || waits += 1);

        assert_eq!(waits, 1);
        assert_eq!(queue.cursor, queue.frame_start);
        assert_eq!(queue.upload_start, queue.frame_start);
    }

    #[test]
    fn descriptor_buffer_addresses_match_upstream_null_and_offset_rules() {
        let mut queue = test_queue();
        queue.supports_descriptor_buffer = true;
        queue.acquire_with_wait(2, || panic!("worker wait is not needed"));
        queue.use_descriptor_buffer = true;

        queue.add_buffer_with_address(vk::Buffer::null(), 0, 17, 23);
        queue.add_buffer_with_address(vk::Buffer::null(), 0x1000, 0x40, 0x80);

        let data = queue.update_data();
        unsafe {
            assert_eq!((*data).address.address, 0);
            assert_eq!((*data).address.range, vk::WHOLE_SIZE);
            assert_eq!((*data.add(1)).address.address, 0x1040);
            assert_eq!((*data.add(1)).address.range, 0x80);
        }
    }
}
