// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `vk_update_descriptor.h` / `vk_update_descriptor.cpp`.
//!
//! Ring-buffered descriptor update queue. Uses a fixed-size payload buffer
//! that is partitioned into per-frame slices.

use ash::vk;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Number of frames in flight for payload ring buffering.
///
/// Port of `UpdateDescriptorQueue::FRAMES_IN_FLIGHT`.
const FRAMES_IN_FLIGHT: usize = 8;

/// Per-frame payload capacity (number of descriptor entries).
///
/// Port of `UpdateDescriptorQueue::FRAME_PAYLOAD_SIZE`.
const FRAME_PAYLOAD_SIZE: usize = 0x20000;

/// Total payload capacity across all frames.
///
/// Port of `UpdateDescriptorQueue::PAYLOAD_SIZE`.
const PAYLOAD_SIZE: usize = FRAME_PAYLOAD_SIZE * FRAMES_IN_FLIGHT;

/// Minimum number of entries required before an overflow check triggers.
///
/// Port of `MIN_ENTRIES` from `Acquire()`.
const MIN_ENTRIES: usize = 0x400;

// ---------------------------------------------------------------------------
// DescriptorUpdateEntry
// ---------------------------------------------------------------------------

/// A single descriptor update entry, holding one of the possible descriptor
/// info types.
///
/// Port of `DescriptorUpdateEntry` union.
#[derive(Clone, Copy)]
pub enum DescriptorUpdateEntry {
    Image(vk::DescriptorImageInfo),
    Buffer(vk::DescriptorBufferInfo),
    TexelBuffer(vk::BufferView),
    Empty,
}

impl Default for DescriptorUpdateEntry {
    fn default() -> Self {
        DescriptorUpdateEntry::Empty
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
/// retrieve the entries written since the last `acquire()` call, then uses
/// those entries to build `vkUpdateDescriptorSets` write descriptors.
pub struct UpdateDescriptorQueue {
    /// Current frame index in the ring buffer.
    frame_index: usize,

    /// Cursor into the payload for writing new entries.
    cursor: usize,

    /// Start of the current frame's payload slice.
    frame_start: usize,

    /// Start of the current upload batch (set by `acquire()`).
    upload_start: usize,

    /// Fixed-size ring buffer of descriptor entries.
    payload: Vec<DescriptorUpdateEntry>,
}

impl UpdateDescriptorQueue {
    /// Port of `UpdateDescriptorQueue::UpdateDescriptorQueue`.
    pub fn new() -> Self {
        Self {
            frame_index: 0,
            cursor: 0,
            frame_start: 0,
            upload_start: 0,
            payload: vec![DescriptorUpdateEntry::Empty; PAYLOAD_SIZE],
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
        self.frame_start = self.frame_index * FRAME_PAYLOAD_SIZE;
        self.cursor = self.frame_start;
    }

    /// Begin a new batch of descriptor updates.
    ///
    /// Port of `UpdateDescriptorQueue::Acquire`.
    ///
    /// If the remaining space in the current frame is insufficient,
    /// resets the cursor to the start of the frame (the caller is expected
    /// to have waited for the scheduler's worker thread first).
    pub fn acquire(&mut self) {
        if (self.cursor - self.frame_start) + MIN_ENTRIES >= FRAME_PAYLOAD_SIZE {
            log::warn!("UpdateDescriptorQueue: payload overflow, resetting cursor");
            self.cursor = self.frame_start;
        }
        self.upload_start = self.cursor;
    }

    /// Returns a slice of entries written since the last `acquire()`.
    ///
    /// Port of `UpdateDescriptorQueue::UpdateData`.
    pub fn update_data(&self) -> &[DescriptorUpdateEntry] {
        &self.payload[self.upload_start..self.cursor]
    }

    /// Queue a combined image sampler descriptor entry.
    ///
    /// Port of `UpdateDescriptorQueue::AddSampledImage`.
    pub fn add_sampled_image(&mut self, image_view: vk::ImageView, sampler: vk::Sampler) {
        self.payload[self.cursor] = DescriptorUpdateEntry::Image(vk::DescriptorImageInfo {
            sampler,
            image_view,
            image_layout: vk::ImageLayout::GENERAL,
        });
        self.cursor += 1;
    }

    /// Queue a storage image descriptor entry.
    ///
    /// Port of `UpdateDescriptorQueue::AddImage`.
    pub fn add_image(&mut self, image_view: vk::ImageView) {
        self.payload[self.cursor] = DescriptorUpdateEntry::Image(vk::DescriptorImageInfo {
            sampler: vk::Sampler::null(),
            image_view,
            image_layout: vk::ImageLayout::GENERAL,
        });
        self.cursor += 1;
    }

    /// Queue a buffer descriptor entry.
    ///
    /// Port of `UpdateDescriptorQueue::AddBuffer`.
    pub fn add_buffer(&mut self, buffer: vk::Buffer, offset: vk::DeviceSize, size: vk::DeviceSize) {
        self.payload[self.cursor] = DescriptorUpdateEntry::Buffer(vk::DescriptorBufferInfo {
            buffer,
            offset,
            range: size,
        });
        self.cursor += 1;
    }

    /// Queue a texel buffer descriptor entry.
    ///
    /// Port of `UpdateDescriptorQueue::AddTexelBuffer`.
    pub fn add_texel_buffer(&mut self, texel_buffer: vk::BufferView) {
        self.payload[self.cursor] = DescriptorUpdateEntry::TexelBuffer(texel_buffer);
        self.cursor += 1;
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

    #[test]
    fn constants_match_upstream() {
        assert_eq!(FRAMES_IN_FLIGHT, 8);
        assert_eq!(FRAME_PAYLOAD_SIZE, 0x20000);
        assert_eq!(PAYLOAD_SIZE, FRAME_PAYLOAD_SIZE * FRAMES_IN_FLIGHT);
        assert_eq!(MIN_ENTRIES, 0x400);
    }

    #[test]
    fn basic_acquire_and_add() {
        let mut queue = UpdateDescriptorQueue::new();
        queue.acquire();

        queue.add_buffer(vk::Buffer::null(), 0, 256);
        queue.add_sampled_image(vk::ImageView::null(), vk::Sampler::null());

        assert_eq!(queue.pending_count(), 2);
        let data = queue.update_data();
        assert_eq!(data.len(), 2);
        assert!(matches!(data[0], DescriptorUpdateEntry::Buffer(_)));
        assert!(matches!(data[1], DescriptorUpdateEntry::Image(_)));
    }

    #[test]
    fn tick_frame_advances_ring() {
        let mut queue = UpdateDescriptorQueue::new();
        assert_eq!(queue.frame_start, 0);

        queue.tick_frame();
        assert_eq!(queue.frame_start, FRAME_PAYLOAD_SIZE);
        assert_eq!(queue.cursor, FRAME_PAYLOAD_SIZE);

        // Wrap around
        for _ in 0..FRAMES_IN_FLIGHT {
            queue.tick_frame();
        }
        assert_eq!(queue.frame_start, FRAME_PAYLOAD_SIZE); // wrapped back to 1
    }
}
