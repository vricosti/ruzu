// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Ring-buffered descriptor update queue.
//!
//! Ref: zuyu `vk_update_descriptor.h` — batches vkUpdateDescriptorSets calls
//! to reduce driver overhead.

use ash::vk;
use log::trace;

/// Ring-buffered descriptor update queue.
///
/// Ref: zuyu UpdateDescriptorQueue — accumulates descriptor writes and flushes
/// them in a single `vkUpdateDescriptorSets` call for efficiency.
pub struct UpdateDescriptorQueue {
    device: ash::Device,
    /// Pending descriptor writes to flush.
    writes: Vec<vk::WriteDescriptorSet>,
    /// Buffer info storage (writes reference indices into this).
    buffer_infos: Vec<vk::DescriptorBufferInfo>,
    /// Image info storage (writes reference indices into this).
    image_infos: Vec<vk::DescriptorImageInfo>,
}

impl UpdateDescriptorQueue {
    pub fn new(device: ash::Device) -> Self {
        Self {
            device,
            writes: Vec::with_capacity(32),
            buffer_infos: Vec::with_capacity(32),
            image_infos: Vec::with_capacity(32),
        }
    }

    /// Queue a uniform/storage buffer descriptor write.
    pub fn add_buffer(
        &mut self,
        set: vk::DescriptorSet,
        binding: u32,
        descriptor_type: vk::DescriptorType,
        buffer: vk::Buffer,
        offset: vk::DeviceSize,
        range: vk::DeviceSize,
    ) {
        let buf_idx = self.buffer_infos.len();
        self.buffer_infos.push(vk::DescriptorBufferInfo {
            buffer,
            offset,
            range,
        });

        self.writes.push(
            vk::WriteDescriptorSet::builder()
                .dst_set(set)
                .dst_binding(binding)
                .dst_array_element(0)
                .descriptor_type(descriptor_type)
                .buffer_info(std::slice::from_ref(&self.buffer_infos[buf_idx]))
                .build(),
        );
    }

    /// Queue a combined image sampler descriptor write.
    pub fn add_image(
        &mut self,
        set: vk::DescriptorSet,
        binding: u32,
        image_view: vk::ImageView,
        sampler: vk::Sampler,
        layout: vk::ImageLayout,
    ) {
        let img_idx = self.image_infos.len();
        self.image_infos.push(vk::DescriptorImageInfo {
            sampler,
            image_view,
            image_layout: layout,
        });

        self.writes.push(
            vk::WriteDescriptorSet::builder()
                .dst_set(set)
                .dst_binding(binding)
                .dst_array_element(0)
                .descriptor_type(vk::DescriptorType::COMBINED_IMAGE_SAMPLER)
                .image_info(std::slice::from_ref(&self.image_infos[img_idx]))
                .build(),
        );
    }

    /// Flush all queued descriptor writes to the device.
    pub fn flush(&mut self) {
        if self.writes.is_empty() {
            return;
        }

        trace!(
            "UpdateDescriptorQueue: flushing {} writes",
            self.writes.len()
        );

        // We need to rebuild the writes with correct pointers since the Vecs
        // may have been reallocated. Each write stores the index we need.
        // Since the writes already have correct slice references from construction,
        // and we haven't reallocated (we use with_capacity), they should be valid.
        // However, to be safe, rebuild the pointer references.
        let mut buf_idx = 0;
        let mut img_idx = 0;

        // Collect into temporary vec with stable references
        let mut final_writes: Vec<vk::WriteDescriptorSet> =
            Vec::with_capacity(self.writes.len());

        for write in &self.writes {
            let mut w = *write;
            if write.descriptor_type == vk::DescriptorType::COMBINED_IMAGE_SAMPLER {
                w.p_image_info = &self.image_infos[img_idx];
                w.descriptor_count = 1;
                img_idx += 1;
            } else {
                w.p_buffer_info = &self.buffer_infos[buf_idx];
                w.descriptor_count = 1;
                buf_idx += 1;
            }
            final_writes.push(w);
        }

        unsafe {
            self.device.update_descriptor_sets(&final_writes, &[]);
        }

        self.reset();
    }

    /// Clear all queued writes.
    pub fn reset(&mut self) {
        self.writes.clear();
        self.buffer_infos.clear();
        self.image_infos.clear();
    }
}
