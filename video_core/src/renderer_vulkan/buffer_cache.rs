// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GPU buffer cache for vertex, index, uniform, and storage data.
//!
//! Ref: zuyu `vk_buffer_cache.h` — caches VkBuffer objects by GPU VA range
//! to avoid redundant uploads of unchanged data.

use std::collections::HashMap;

use ash::vk;
use log::{debug, trace};

use super::staging_buffer_pool::StagingBufferPool;

/// A cached GPU buffer backed by VkBuffer + VkDeviceMemory.
pub struct CachedBuffer {
    pub buffer: vk::Buffer,
    pub memory: vk::DeviceMemory,
    pub size: vk::DeviceSize,
}

/// Manages GPU buffers for vertex, index, uniform, and storage data.
///
/// Ref: zuyu BufferCacheRuntime — caches GPU-local buffers by GPU VA,
/// uploads data from guest memory via the staging buffer pool.
pub struct BufferCache {
    device: ash::Device,
    instance: ash::Instance,
    physical_device: vk::PhysicalDevice,

    /// Cached buffers by GPU VA.
    cache: HashMap<u64, CachedBuffer>,

    /// Null buffer for unbound vertex/index slots.
    null_buffer: vk::Buffer,
    null_memory: vk::DeviceMemory,
}

impl BufferCache {
    pub fn new(
        device: ash::Device,
        instance: ash::Instance,
        physical_device: vk::PhysicalDevice,
    ) -> Result<Self, vk::Result> {
        // Create a small null buffer (256 bytes of zeros)
        let null_size = 256u64;
        let buf_info = vk::BufferCreateInfo::builder()
            .size(null_size)
            .usage(
                vk::BufferUsageFlags::VERTEX_BUFFER
                    | vk::BufferUsageFlags::INDEX_BUFFER
                    | vk::BufferUsageFlags::UNIFORM_BUFFER
                    | vk::BufferUsageFlags::TRANSFER_DST,
            )
            .sharing_mode(vk::SharingMode::EXCLUSIVE)
            .build();
        let null_buffer = unsafe { device.create_buffer(&buf_info, None)? };

        let mem_reqs = unsafe { device.get_buffer_memory_requirements(null_buffer) };
        let mem_type = find_device_local_memory(&instance, physical_device, mem_reqs.memory_type_bits)
            .unwrap_or(0);
        let alloc_info = vk::MemoryAllocateInfo::builder()
            .allocation_size(mem_reqs.size)
            .memory_type_index(mem_type)
            .build();
        let null_memory = unsafe { device.allocate_memory(&alloc_info, None)? };
        unsafe { device.bind_buffer_memory(null_buffer, null_memory, 0)? };

        Ok(Self {
            device,
            instance,
            physical_device,
            cache: HashMap::new(),
            null_buffer,
            null_memory,
        })
    }

    /// Get or upload a GPU buffer from guest memory.
    ///
    /// Returns (VkBuffer, offset) suitable for binding.
    pub fn get_or_upload(
        &mut self,
        gpu_va: u64,
        size: vk::DeviceSize,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        staging_pool: &mut StagingBufferPool,
        cmd: vk::CommandBuffer,
    ) -> (vk::Buffer, vk::DeviceSize) {
        if size == 0 || gpu_va == 0 {
            return (self.null_buffer, 0);
        }

        // Check cache
        if let Some(cached) = self.cache.get(&gpu_va) {
            if cached.size >= size {
                return (cached.buffer, 0);
            }
            // Buffer too small — will be recreated
        }

        // Create GPU-local buffer
        let buf_info = vk::BufferCreateInfo::builder()
            .size(size)
            .usage(
                vk::BufferUsageFlags::VERTEX_BUFFER
                    | vk::BufferUsageFlags::INDEX_BUFFER
                    | vk::BufferUsageFlags::UNIFORM_BUFFER
                    | vk::BufferUsageFlags::STORAGE_BUFFER
                    | vk::BufferUsageFlags::TRANSFER_DST,
            )
            .sharing_mode(vk::SharingMode::EXCLUSIVE)
            .build();

        let buffer = match unsafe { self.device.create_buffer(&buf_info, None) } {
            Ok(b) => b,
            Err(_) => return (self.null_buffer, 0),
        };

        let mem_reqs = unsafe { self.device.get_buffer_memory_requirements(buffer) };
        let mem_type =
            find_device_local_memory(&self.instance, self.physical_device, mem_reqs.memory_type_bits)
                .unwrap_or(0);

        let alloc_info = vk::MemoryAllocateInfo::builder()
            .allocation_size(mem_reqs.size)
            .memory_type_index(mem_type)
            .build();
        let memory = match unsafe { self.device.allocate_memory(&alloc_info, None) } {
            Ok(m) => m,
            Err(_) => {
                unsafe { self.device.destroy_buffer(buffer, None) };
                return (self.null_buffer, 0);
            }
        };
        unsafe {
            if self.device.bind_buffer_memory(buffer, memory, 0).is_err() {
                self.device.destroy_buffer(buffer, None);
                self.device.free_memory(memory, None);
                return (self.null_buffer, 0);
            }
        }

        // Upload data via staging buffer
        if let Some(staging) = staging_pool.request_upload_buffer(size) {
            let mut host_data = vec![0u8; size as usize];
            read_gpu(gpu_va, &mut host_data);
            unsafe {
                std::ptr::copy_nonoverlapping(
                    host_data.as_ptr(),
                    staging.mapped,
                    size as usize,
                );
            }

            // Record copy command
            let copy_region = vk::BufferCopy {
                src_offset: staging.offset,
                dst_offset: 0,
                size,
            };
            unsafe {
                self.device
                    .cmd_copy_buffer(cmd, staging.buffer, buffer, &[copy_region]);
            }
        }

        trace!(
            "BufferCache: uploaded {} bytes from GPU VA 0x{:016X}",
            size,
            gpu_va
        );

        // Remove old entry if exists
        if let Some(old) = self.cache.remove(&gpu_va) {
            unsafe {
                self.device.destroy_buffer(old.buffer, None);
                self.device.free_memory(old.memory, None);
            }
        }

        self.cache.insert(
            gpu_va,
            CachedBuffer {
                buffer,
                memory,
                size,
            },
        );

        (buffer, 0)
    }

    /// Bind a vertex buffer at the given binding index.
    pub fn bind_vertex_buffer(
        &mut self,
        cmd: vk::CommandBuffer,
        binding: u32,
        gpu_va: u64,
        size: vk::DeviceSize,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        staging_pool: &mut StagingBufferPool,
        upload_cmd: vk::CommandBuffer,
    ) {
        let (buffer, offset) = self.get_or_upload(gpu_va, size, read_gpu, staging_pool, upload_cmd);
        unsafe {
            self.device
                .cmd_bind_vertex_buffers(cmd, binding, &[buffer], &[offset]);
        }
    }

    /// Bind an index buffer.
    pub fn bind_index_buffer(
        &mut self,
        cmd: vk::CommandBuffer,
        gpu_va: u64,
        size: vk::DeviceSize,
        index_type: vk::IndexType,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        staging_pool: &mut StagingBufferPool,
        upload_cmd: vk::CommandBuffer,
    ) {
        let (buffer, offset) = self.get_or_upload(gpu_va, size, read_gpu, staging_pool, upload_cmd);
        unsafe {
            self.device
                .cmd_bind_index_buffer(cmd, buffer, offset, index_type);
        }
    }

    /// Invalidate a cached buffer range (mark as stale).
    pub fn invalidate(&mut self, gpu_va: u64) {
        if let Some(old) = self.cache.remove(&gpu_va) {
            debug!("BufferCache: invalidated buffer at GPU VA 0x{:016X}", gpu_va);
            unsafe {
                self.device.destroy_buffer(old.buffer, None);
                self.device.free_memory(old.memory, None);
            }
        }
    }

    /// Get the null buffer handle.
    pub fn null_buffer(&self) -> vk::Buffer {
        self.null_buffer
    }
}

impl Drop for BufferCache {
    fn drop(&mut self) {
        unsafe {
            for (_, cached) in self.cache.drain() {
                self.device.destroy_buffer(cached.buffer, None);
                self.device.free_memory(cached.memory, None);
            }
            self.device.destroy_buffer(self.null_buffer, None);
            self.device.free_memory(self.null_memory, None);
        }
    }
}

fn find_device_local_memory(
    instance: &ash::Instance,
    physical_device: vk::PhysicalDevice,
    type_filter: u32,
) -> Option<u32> {
    let mem_props = unsafe { instance.get_physical_device_memory_properties(physical_device) };
    for i in 0..mem_props.memory_type_count {
        if (type_filter & (1 << i)) != 0
            && mem_props.memory_types[i as usize]
                .property_flags
                .contains(vk::MemoryPropertyFlags::DEVICE_LOCAL)
        {
            return Some(i);
        }
    }
    None
}
