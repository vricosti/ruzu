// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Pool of staging buffers for CPU↔GPU data transfer.
//!
//! Ref: zuyu `vk_staging_buffer_pool.h` — stream buffer + size-class caching
//! for efficient CPU→GPU and GPU→CPU transfers.

use ash::vk;
use log::trace;

/// A staging buffer allocation (host-visible, for CPU↔GPU transfers).
pub struct StagingBuffer {
    pub buffer: vk::Buffer,
    pub memory: vk::DeviceMemory,
    pub mapped: *mut u8,
    pub offset: vk::DeviceSize,
    pub size: vk::DeviceSize,
}

// Raw pointer is only used for mapped memory
unsafe impl Send for StagingBuffer {}

/// Pool of staging buffers for CPU↔GPU data transfer.
///
/// Ref: zuyu StagingBufferPool — uses a large stream buffer for small
/// allocations and a free list for larger ones.
pub struct StagingBufferPool {
    device: ash::Device,
    instance: ash::Instance,
    physical_device: vk::PhysicalDevice,

    /// Stream buffer for small allocations (1 MB, reused per frame).
    stream_buffer: Option<StagingBuffer>,
    stream_offset: vk::DeviceSize,
    stream_capacity: vk::DeviceSize,

    /// Free list of previously allocated buffers.
    free_buffers: Vec<StagingBuffer>,
}

impl StagingBufferPool {
    /// Default stream buffer size (1 MB).
    const STREAM_BUFFER_SIZE: vk::DeviceSize = 1024 * 1024;

    pub fn new(
        device: ash::Device,
        instance: ash::Instance,
        physical_device: vk::PhysicalDevice,
    ) -> Self {
        Self {
            device,
            instance,
            physical_device,
            stream_buffer: None,
            stream_offset: 0,
            stream_capacity: Self::STREAM_BUFFER_SIZE,
            free_buffers: Vec::new(),
        }
    }

    /// Request a staging buffer for CPU→GPU upload.
    pub fn request_upload_buffer(&mut self, size: vk::DeviceSize) -> Option<StagingBuffer> {
        self.request_buffer(size, vk::BufferUsageFlags::TRANSFER_SRC)
    }

    /// Request a staging buffer for GPU→CPU readback.
    pub fn request_download_buffer(&mut self, size: vk::DeviceSize) -> Option<StagingBuffer> {
        self.request_buffer(size, vk::BufferUsageFlags::TRANSFER_DST)
    }

    /// Return a buffer to the free list.
    pub fn release(&mut self, buffer: StagingBuffer) {
        self.free_buffers.push(buffer);
    }

    /// Reset stream buffer offset for a new frame.
    pub fn new_frame(&mut self) {
        self.stream_offset = 0;
    }

    fn request_buffer(
        &mut self,
        size: vk::DeviceSize,
        usage: vk::BufferUsageFlags,
    ) -> Option<StagingBuffer> {
        // For small allocations, try the stream buffer
        if size <= self.stream_capacity / 4 {
            if let Some(buf) = self.try_stream_allocate(size) {
                return Some(buf);
            }
        }

        // Allocate a new dedicated buffer
        self.allocate_buffer(size, usage)
    }

    fn try_stream_allocate(&mut self, size: vk::DeviceSize) -> Option<StagingBuffer> {
        // Initialize stream buffer if needed
        if self.stream_buffer.is_none() {
            let buf = self.allocate_buffer(
                self.stream_capacity,
                vk::BufferUsageFlags::TRANSFER_SRC | vk::BufferUsageFlags::TRANSFER_DST,
            )?;
            self.stream_buffer = Some(buf);
            self.stream_offset = 0;
        }

        let stream = self.stream_buffer.as_ref()?;

        // Align offset to 256 bytes
        let aligned_offset = (self.stream_offset + 255) & !255;
        if aligned_offset + size > self.stream_capacity {
            return None;
        }

        let result = StagingBuffer {
            buffer: stream.buffer,
            memory: stream.memory,
            mapped: unsafe { stream.mapped.add(aligned_offset as usize) },
            offset: aligned_offset,
            size,
        };

        self.stream_offset = aligned_offset + size;
        Some(result)
    }

    fn allocate_buffer(
        &self,
        size: vk::DeviceSize,
        usage: vk::BufferUsageFlags,
    ) -> Option<StagingBuffer> {
        let buf_info = vk::BufferCreateInfo::builder()
            .size(size)
            .usage(usage)
            .sharing_mode(vk::SharingMode::EXCLUSIVE)
            .build();

        let buffer = unsafe { self.device.create_buffer(&buf_info, None).ok()? };

        let mem_reqs = unsafe { self.device.get_buffer_memory_requirements(buffer) };
        let mem_type = find_host_visible_memory(
            &self.instance,
            self.physical_device,
            mem_reqs.memory_type_bits,
        )?;

        let alloc_info = vk::MemoryAllocateInfo::builder()
            .allocation_size(mem_reqs.size)
            .memory_type_index(mem_type)
            .build();
        let memory = unsafe { self.device.allocate_memory(&alloc_info, None).ok()? };
        unsafe {
            self.device.bind_buffer_memory(buffer, memory, 0).ok()?;
        }

        let mapped = unsafe {
            self.device
                .map_memory(memory, 0, size, vk::MemoryMapFlags::empty())
                .ok()? as *mut u8
        };

        trace!("StagingBufferPool: allocated {} bytes", size);

        Some(StagingBuffer {
            buffer,
            memory,
            mapped,
            offset: 0,
            size,
        })
    }
}

impl Drop for StagingBufferPool {
    fn drop(&mut self) {
        unsafe {
            // Free stream buffer
            if let Some(buf) = self.stream_buffer.take() {
                self.device.unmap_memory(buf.memory);
                self.device.destroy_buffer(buf.buffer, None);
                self.device.free_memory(buf.memory, None);
            }
            // Free pooled buffers
            for buf in self.free_buffers.drain(..) {
                self.device.unmap_memory(buf.memory);
                self.device.destroy_buffer(buf.buffer, None);
                self.device.free_memory(buf.memory, None);
            }
        }
    }
}

fn find_host_visible_memory(
    instance: &ash::Instance,
    physical_device: vk::PhysicalDevice,
    type_filter: u32,
) -> Option<u32> {
    let mem_props = unsafe { instance.get_physical_device_memory_properties(physical_device) };
    let required = vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT;
    for i in 0..mem_props.memory_type_count {
        if (type_filter & (1 << i)) != 0
            && mem_props.memory_types[i as usize]
                .property_flags
                .contains(required)
        {
            return Some(i);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_staging_buffer_send() {
        // Verify StagingBuffer is Send
        fn assert_send<T: Send>() {}
        assert_send::<StagingBuffer>();
    }
}
