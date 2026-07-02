// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Pool of staging buffers for CPU↔GPU data transfer.
//!
//! Ref: zuyu `vk_staging_buffer_pool.h` — stream buffer + size-class caching
//! for efficient CPU→GPU and GPU→CPU transfers.

use std::ptr::NonNull;

use ash::vk;
use log::trace;

use super::scheduler::Scheduler;

/// A staging buffer allocation (host-visible, for CPU↔GPU transfers).
#[derive(Clone, Copy)]
pub struct StagingBuffer {
    pub buffer: vk::Buffer,
    pub memory: vk::DeviceMemory,
    pub mapped: *mut u8,
    pub offset: vk::DeviceSize,
    pub size: vk::DeviceSize,
    pub usage: StagingBufferUsage,
    pub index: u64,
    pub log2_level: u32,
    pub tick: u64,
    pub deferred: bool,
}

// Raw pointer is only used for mapped memory
unsafe impl Send for StagingBuffer {}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StagingBufferUsage {
    Upload,
    Download,
}

/// Pool of staging buffers for CPU↔GPU data transfer.
///
/// Ref: zuyu StagingBufferPool — uses a large stream buffer for small
/// allocations and a free list for larger ones.
pub struct StagingBufferPool {
    device: ash::Device,
    instance: ash::Instance,
    physical_device: vk::PhysicalDevice,
    scheduler: NonNull<Scheduler>,

    /// Stream buffer for small allocations (1 MB, reused per frame).
    stream_buffer: Option<StagingBuffer>,
    stream_offset: vk::DeviceSize,
    stream_capacity: vk::DeviceSize,

    /// Dedicated staging buffers owned by the pool, matching upstream cached
    /// staging entries. Returned `StagingBuffer`s are references to these
    /// entries, not owners.
    buffers: Vec<StagingBuffer>,
    unique_ids: u64,
    current_delete_level: u32,
}

impl StagingBufferPool {
    /// Default stream buffer size (1 MB).
    const STREAM_BUFFER_SIZE: vk::DeviceSize = 1024 * 1024;

    pub fn new(
        device: ash::Device,
        instance: ash::Instance,
        physical_device: vk::PhysicalDevice,
        scheduler: &mut Scheduler,
    ) -> Self {
        Self {
            device,
            instance,
            physical_device,
            scheduler: NonNull::from(scheduler),
            stream_buffer: None,
            stream_offset: 0,
            stream_capacity: Self::STREAM_BUFFER_SIZE,
            buffers: Vec::new(),
            unique_ids: 1,
            current_delete_level: 0,
        }
    }

    /// Request a staging buffer for CPU→GPU upload.
    pub fn request_upload_buffer(&mut self, size: vk::DeviceSize) -> Option<StagingBuffer> {
        self.request_buffer(size, StagingBufferUsage::Upload, false)
    }

    /// Request a staging buffer for GPU→CPU readback.
    pub fn request_download_buffer(
        &mut self,
        size: vk::DeviceSize,
        deferred: bool,
    ) -> Option<StagingBuffer> {
        self.request_buffer(size, StagingBufferUsage::Download, deferred)
    }

    /// Port of upstream `StagingBufferPool::FreeDeferred`.
    pub fn free_deferred(&mut self, buffer: &mut StagingBuffer) {
        let current_tick = self.scheduler().pending_tick();
        let Some(entry) = self.buffers.iter_mut().find(|entry| {
            entry.index == buffer.index
                && entry.usage == buffer.usage
                && entry.log2_level == buffer.log2_level
        }) else {
            debug_assert!(
                buffer.index == 0,
                "deferred staging buffer missing from Vulkan staging pool"
            );
            return;
        };
        debug_assert!(entry.deferred);
        entry.tick = current_tick;
        entry.deferred = false;
        entry.offset = 0;
        buffer.tick = entry.tick;
        buffer.deferred = false;
    }

    /// Reset stream buffer offset for a new frame.
    pub fn new_frame(&mut self) {
        self.current_delete_level = (self.current_delete_level + 1) % vk::DeviceSize::BITS;
        self.release_level(self.current_delete_level);
        self.stream_offset = 0;
    }

    fn scheduler(&self) -> &Scheduler {
        // SAFETY: `StagingBufferPool` is constructed after the boxed
        // rasterizer scheduler and is dropped before it. The box keeps a stable
        // address for the scheduler.
        unsafe { self.scheduler.as_ref() }
    }

    fn request_buffer(
        &mut self,
        size: vk::DeviceSize,
        usage: StagingBufferUsage,
        deferred: bool,
    ) -> Option<StagingBuffer> {
        // Upstream only uses the stream buffer for non-deferred uploads.
        if !deferred && usage == StagingBufferUsage::Upload && size <= self.stream_capacity / 4 {
            if let Some(buf) = self.try_stream_allocate(size) {
                return Some(buf);
            }
        }

        if let Some(buffer) = self.try_get_reserved_buffer(size, usage, deferred) {
            return Some(buffer);
        }
        self.create_staging_buffer(size, usage, deferred)
    }

    fn try_stream_allocate(&mut self, size: vk::DeviceSize) -> Option<StagingBuffer> {
        // Initialize stream buffer if needed
        if self.stream_buffer.is_none() {
            let buf = self.allocate_buffer(self.stream_capacity)?;
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
            usage: StagingBufferUsage::Upload,
            index: 0,
            log2_level: 0,
            tick: self.scheduler().pending_tick(),
            deferred: false,
        };

        self.stream_offset = aligned_offset + size;
        Some(result)
    }

    fn try_get_reserved_buffer(
        &mut self,
        size: vk::DeviceSize,
        usage: StagingBufferUsage,
        deferred: bool,
    ) -> Option<StagingBuffer> {
        let log2_level = log2_ceil(size.max(1));
        let current_tick = self.scheduler().pending_tick();
        let free_index = self.buffers.iter().position(|entry| {
            entry.usage == usage
                && entry.log2_level == log2_level
                && !entry.deferred
                && entry.size >= size
                && self.scheduler().is_free(entry.tick)
        })?;
        let entry = &mut self.buffers[free_index];
        entry.tick = if deferred { u64::MAX } else { current_tick };
        entry.deferred = deferred;
        let mut handle = *entry;
        handle.deferred = deferred;
        Some(handle)
    }

    fn create_staging_buffer(
        &mut self,
        size: vk::DeviceSize,
        usage: StagingBufferUsage,
        deferred: bool,
    ) -> Option<StagingBuffer> {
        let log2_level = log2_ceil(size.max(1));
        let allocation_size = 1u64.checked_shl(log2_level).unwrap_or(size.max(1));
        let mut buffer = self.allocate_buffer(allocation_size)?;
        buffer.usage = usage;
        buffer.index = self.unique_ids;
        buffer.log2_level = log2_level;
        buffer.tick = if deferred {
            u64::MAX
        } else {
            self.scheduler().pending_tick()
        };
        buffer.deferred = deferred;
        self.unique_ids = self.unique_ids.wrapping_add(1);
        self.buffers.push(buffer);
        Some(buffer)
    }

    fn release_level(&mut self, log2_level: u32) {
        const DELETIONS_PER_TICK: usize = 16;

        let mut deleted = 0usize;
        let mut index = 0usize;
        while index < self.buffers.len() && deleted < DELETIONS_PER_TICK {
            let entry = self.buffers[index];
            if entry.log2_level == log2_level
                && !entry.deferred
                && entry.index != 0
                && self.scheduler().is_free(entry.tick)
            {
                let entry = self.buffers.swap_remove(index);
                unsafe {
                    self.device.unmap_memory(entry.memory);
                    self.device.destroy_buffer(entry.buffer, None);
                    self.device.free_memory(entry.memory, None);
                }
                deleted += 1;
                continue;
            }
            index += 1;
        }
    }

    fn allocate_buffer(&self, size: vk::DeviceSize) -> Option<StagingBuffer> {
        let buf_info = vk::BufferCreateInfo::builder()
            .size(size)
            .usage(
                vk::BufferUsageFlags::TRANSFER_SRC
                    | vk::BufferUsageFlags::TRANSFER_DST
                    | vk::BufferUsageFlags::UNIFORM_BUFFER
                    | vk::BufferUsageFlags::STORAGE_BUFFER
                    | vk::BufferUsageFlags::INDEX_BUFFER
                    | vk::BufferUsageFlags::VERTEX_BUFFER,
            )
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
            usage: StagingBufferUsage::Upload,
            index: 0,
            log2_level: log2_ceil(size.max(1)),
            tick: self.scheduler().pending_tick(),
            deferred: false,
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
            for buf in self.buffers.drain(..) {
                self.device.unmap_memory(buf.memory);
                self.device.destroy_buffer(buf.buffer, None);
                self.device.free_memory(buf.memory, None);
            }
        }
    }
}

fn log2_ceil(value: vk::DeviceSize) -> u32 {
    if value <= 1 {
        0
    } else {
        vk::DeviceSize::BITS - (value - 1).leading_zeros()
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

    #[test]
    fn log2_ceil_matches_staging_size_classes() {
        assert_eq!(log2_ceil(1), 0);
        assert_eq!(log2_ceil(2), 1);
        assert_eq!(log2_ceil(3), 2);
        assert_eq!(log2_ceil(4), 2);
        assert_eq!(log2_ceil(5), 3);
        assert_eq!(log2_ceil(1024), 10);
        assert_eq!(log2_ceil(1025), 11);
    }
}
