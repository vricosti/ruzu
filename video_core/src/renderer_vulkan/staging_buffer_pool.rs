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

    /// Stream buffer for small per-draw allocations (uniforms, index data).
    ///
    /// Port of upstream's 128MiB stream buffer split into `NUM_SYNCS`
    /// regions, each stamped with the tick of the submission that may read
    /// it (vk_staging_buffer_pool.cpp GetStreamBuffer). A region is only
    /// reused once `Scheduler::known_gpu_tick` passes its stamp — the old
    /// per-frame `stream_offset = 0` reset recycled mappings while earlier
    /// (possibly not even submitted) work still read them.
    stream_buffer: Option<StagingBuffer>,
    stream_capacity: vk::DeviceSize,
    stream_iterator: vk::DeviceSize,
    stream_used_iterator: vk::DeviceSize,
    stream_free_iterator: vk::DeviceSize,
    stream_sync_ticks: [u64; Self::NUM_SYNCS],

    /// Dedicated staging buffers owned by the pool, matching upstream cached
    /// staging entries. Returned `StagingBuffer`s are references to these
    /// entries, not owners.
    buffers: Vec<StagingBuffer>,
    unique_ids: u64,
    current_delete_level: u32,
}

impl StagingBufferPool {
    /// Stream buffer size, port of upstream `MAX_STREAM_BUFFER_SIZE` (128MiB).
    const STREAM_BUFFER_SIZE: vk::DeviceSize = 128 * 1024 * 1024;
    /// Port of upstream `StagingBufferPool::NUM_SYNCS`.
    const NUM_SYNCS: usize = 16;
    /// Port of upstream `MAX_ALIGNMENT`.
    const MAX_ALIGNMENT: vk::DeviceSize = 256;

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
            stream_capacity: Self::STREAM_BUFFER_SIZE,
            stream_iterator: 0,
            stream_used_iterator: 0,
            stream_free_iterator: 0,
            stream_sync_ticks: [0; Self::NUM_SYNCS],
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

    /// Per-frame housekeeping (upstream `TickFrame`): rotate the deletion
    /// level. The stream buffer is NOT reset here — its regions retire
    /// individually against the GPU timeline (see `try_stream_allocate`).
    pub fn new_frame(&mut self) {
        self.current_delete_level = (self.current_delete_level + 1) % vk::DeviceSize::BITS;
        self.release_level(self.current_delete_level);
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
        // Upstream only uses the stream buffer for non-deferred uploads that
        // fit in one region (Request: `size <= region_size`).
        if !deferred
            && usage == StagingBufferUsage::Upload
            && size <= self.stream_capacity / Self::NUM_SYNCS as vk::DeviceSize
        {
            if let Some(buf) = self.try_stream_allocate(size) {
                return Some(buf);
            }
        }

        if let Some(buffer) = self.try_get_reserved_buffer(size, usage, deferred) {
            return Some(buffer);
        }
        self.create_staging_buffer(size, usage, deferred)
    }

    /// Port of upstream `StagingBufferPool::GetStreamBuffer`
    /// (vk_staging_buffer_pool.cpp:105-139). Returns `None` (upstream falls
    /// back to `GetStagingBuffer`) instead of waiting when the required
    /// regions are still referenced by in-flight GPU work.
    fn try_stream_allocate(&mut self, size: vk::DeviceSize) -> Option<StagingBuffer> {
        // Initialize stream buffer if needed
        if self.stream_buffer.is_none() {
            let buf = self.allocate_buffer(self.stream_capacity)?;
            self.stream_buffer = Some(buf);
            self.stream_iterator = 0;
            self.stream_used_iterator = 0;
            self.stream_free_iterator = 0;
            self.stream_sync_ticks = [0; Self::NUM_SYNCS];
        }

        let region_size = self.stream_capacity / Self::NUM_SYNCS as vk::DeviceSize;
        let region = |offset: vk::DeviceSize| (offset / region_size) as usize;

        if self.are_stream_regions_active(
            region(self.stream_free_iterator) + 1,
            (region(self.stream_iterator + size) + 1).min(Self::NUM_SYNCS),
        ) {
            // Avoid waiting for the previous usages to be free.
            return None;
        }

        let current_tick = self.scheduler().pending_tick();
        for tick in &mut self.stream_sync_ticks
            [region(self.stream_used_iterator)..region(self.stream_iterator)]
        {
            *tick = current_tick;
        }
        self.stream_used_iterator = self.stream_iterator;
        self.stream_free_iterator = self.stream_free_iterator.max(self.stream_iterator + size);

        if self.stream_iterator + size >= self.stream_capacity {
            for tick in
                &mut self.stream_sync_ticks[region(self.stream_used_iterator)..Self::NUM_SYNCS]
            {
                *tick = current_tick;
            }
            self.stream_used_iterator = 0;
            self.stream_iterator = 0;
            self.stream_free_iterator = size;

            if self.are_stream_regions_active(0, region(size) + 1) {
                // Avoid waiting for the previous usages to be free.
                return None;
            }
        }

        let offset = self.stream_iterator;
        self.stream_iterator =
            (self.stream_iterator + size + Self::MAX_ALIGNMENT - 1) & !(Self::MAX_ALIGNMENT - 1);

        let stream = self.stream_buffer.as_ref()?;
        Some(StagingBuffer {
            buffer: stream.buffer,
            memory: stream.memory,
            mapped: unsafe { stream.mapped.add(offset as usize) },
            offset,
            size,
            usage: StagingBufferUsage::Upload,
            index: 0,
            log2_level: 0,
            tick: current_tick,
            deferred: false,
        })
    }

    /// Port of upstream `StagingBufferPool::AreRegionsActive`.
    fn are_stream_regions_active(&self, region_begin: usize, region_end: usize) -> bool {
        let gpu_tick = self.scheduler().known_gpu_tick();
        self.stream_sync_ticks[region_begin..region_end]
            .iter()
            .any(|&sync_tick| gpu_tick < sync_tick)
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
