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
use crate::control::channel_state::ChannelState;
use crate::control::channel_state_cache::{ChannelInfo, ChannelSetupCaches};
use crate::engines::maxwell_3d::{IndexFormat, PrimitiveTopology};

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
    channel_caches: ChannelSetupCaches<ChannelInfo>,

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
        let mem_type =
            find_device_local_memory(&instance, physical_device, mem_reqs.memory_type_bits)
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
            channel_caches: ChannelSetupCaches::new(),
            cache: HashMap::new(),
            null_buffer,
            null_memory,
        })
    }

    /// Port of the Vulkan buffer-cache owner `CreateChannel` edge.
    pub fn create_channel(&mut self, channel: &ChannelState) {
        self.channel_caches.create_channel(channel);
    }

    /// Port of the Vulkan buffer-cache owner `BindToChannel` edge.
    pub fn bind_to_channel(&mut self, channel_id: i32) {
        self.channel_caches.bind_to_channel(channel_id);
    }

    /// Port of the Vulkan buffer-cache owner `EraseChannel` edge.
    pub fn erase_channel(&mut self, channel_id: i32) {
        self.channel_caches.erase_channel(channel_id);
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
        let mem_type = find_device_local_memory(
            &self.instance,
            self.physical_device,
            mem_reqs.memory_type_bits,
        )
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
                std::ptr::copy_nonoverlapping(host_data.as_ptr(), staging.mapped, size as usize);
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
                post_copy_barrier(&self.device, cmd);
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

    fn get_or_upload_bytes(
        &mut self,
        cache_key: u64,
        data: &[u8],
        staging_pool: &mut StagingBufferPool,
        cmd: vk::CommandBuffer,
    ) -> (vk::Buffer, vk::DeviceSize) {
        let size = data.len() as vk::DeviceSize;
        if size == 0 {
            return (self.null_buffer, 0);
        }
        if let Some(cached) = self.cache.get(&cache_key) {
            if cached.size >= size {
                return (cached.buffer, 0);
            }
        }

        let buf_info = vk::BufferCreateInfo::builder()
            .size(size)
            .usage(vk::BufferUsageFlags::INDEX_BUFFER | vk::BufferUsageFlags::TRANSFER_DST)
            .sharing_mode(vk::SharingMode::EXCLUSIVE)
            .build();
        let buffer = match unsafe { self.device.create_buffer(&buf_info, None) } {
            Ok(buffer) => buffer,
            Err(_) => return (self.null_buffer, 0),
        };

        let mem_reqs = unsafe { self.device.get_buffer_memory_requirements(buffer) };
        let mem_type = find_device_local_memory(
            &self.instance,
            self.physical_device,
            mem_reqs.memory_type_bits,
        )
        .unwrap_or(0);
        let alloc_info = vk::MemoryAllocateInfo::builder()
            .allocation_size(mem_reqs.size)
            .memory_type_index(mem_type)
            .build();
        let memory = match unsafe { self.device.allocate_memory(&alloc_info, None) } {
            Ok(memory) => memory,
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

        if let Some(staging) = staging_pool.request_upload_buffer(size) {
            unsafe {
                std::ptr::copy_nonoverlapping(data.as_ptr(), staging.mapped, data.len());
                let copy_region = vk::BufferCopy {
                    src_offset: staging.offset,
                    dst_offset: 0,
                    size,
                };
                self.device
                    .cmd_copy_buffer(cmd, staging.buffer, buffer, &[copy_region]);
                post_copy_barrier(&self.device, cmd);
            }
        }

        if let Some(old) = self.cache.remove(&cache_key) {
            unsafe {
                self.device.destroy_buffer(old.buffer, None);
                self.device.free_memory(old.memory, None);
            }
        }
        self.cache.insert(
            cache_key,
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

    /// Port-facing equivalent of `BufferCacheRuntime::BindQuadIndexBuffer`.
    pub fn bind_quad_index_buffer(
        &mut self,
        cmd: vk::CommandBuffer,
        topology: PrimitiveTopology,
        first: u32,
        count: u32,
        staging_pool: &mut StagingBufferPool,
        upload_cmd: vk::CommandBuffer,
    ) {
        let indices = make_quad_array_indices(topology, first, count);
        let cache_key = quad_index_cache_key(topology, first, count, 0, 0, false);
        let data = indices_as_bytes(&indices);
        let (buffer, offset) = self.get_or_upload_bytes(cache_key, data, staging_pool, upload_cmd);
        unsafe {
            self.device
                .cmd_bind_index_buffer(cmd, buffer, offset, vk::IndexType::UINT32);
        }
    }

    /// Port-facing equivalent of `BufferCacheRuntime::BindIndexBuffer` for quad topologies.
    pub fn bind_quad_indexed_buffer(
        &mut self,
        cmd: vk::CommandBuffer,
        topology: PrimitiveTopology,
        index_format: IndexFormat,
        base_vertex: i32,
        first_index: u32,
        count: u32,
        gpu_va: u64,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        staging_pool: &mut StagingBufferPool,
        upload_cmd: vk::CommandBuffer,
    ) {
        let indices = make_quad_indexed_indices(
            topology,
            index_format,
            base_vertex,
            first_index,
            count,
            gpu_va,
            read_gpu,
        );
        let cache_key = quad_index_cache_key(
            topology,
            first_index,
            count,
            gpu_va,
            base_vertex as u32,
            true,
        );
        let data = indices_as_bytes(&indices);
        let (buffer, offset) = self.get_or_upload_bytes(cache_key, data, staging_pool, upload_cmd);
        unsafe {
            self.device
                .cmd_bind_index_buffer(cmd, buffer, offset, vk::IndexType::UINT32);
        }
    }

    /// Invalidate a cached buffer range (mark as stale).
    pub fn invalidate(&mut self, gpu_va: u64) {
        if let Some(old) = self.cache.remove(&gpu_va) {
            debug!(
                "BufferCache: invalidated buffer at GPU VA 0x{:016X}",
                gpu_va
            );
            unsafe {
                self.device.destroy_buffer(old.buffer, None);
                self.device.free_memory(old.memory, None);
            }
        }
    }

    /// Notify the cache that guest memory was written and drop stale GPU buffers.
    ///
    /// Upstream receives a translated CPU address here. This reduced Vulkan cache
    /// is keyed by GPU VA, so invalidate overlapping cached GPU ranges instead.
    pub fn write_memory(&mut self, gpu_va: u64, size: u64) {
        let end = gpu_va.saturating_add(size);
        let stale: Vec<u64> = self
            .cache
            .iter()
            .filter_map(|(&base, cached)| {
                let cached_end = base.saturating_add(cached.size);
                (base < end && gpu_va < cached_end).then_some(base)
            })
            .collect();

        for base in stale {
            self.invalidate(base);
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

fn quad_count(topology: PrimitiveTopology, count: u32) -> u32 {
    match topology {
        PrimitiveTopology::Quads => count / 4,
        PrimitiveTopology::QuadStrip => count.saturating_sub(2) / 2,
        _ => 0,
    }
}

fn make_quad_array_indices(topology: PrimitiveTopology, first: u32, count: u32) -> Vec<u32> {
    let quads = quad_count(topology, count);
    let mut indices = Vec::with_capacity((quads * 6) as usize);
    for quad in 0..quads {
        match topology {
            PrimitiveTopology::Quads => {
                let base = first.wrapping_add(quad * 4);
                indices.extend_from_slice(&[
                    base,
                    base.wrapping_add(1),
                    base.wrapping_add(2),
                    base,
                    base.wrapping_add(2),
                    base.wrapping_add(3),
                ]);
            }
            PrimitiveTopology::QuadStrip => {
                let base = first.wrapping_add(quad * 2);
                indices.extend_from_slice(&[
                    base,
                    base.wrapping_add(3),
                    base.wrapping_add(1),
                    base,
                    base.wrapping_add(2),
                    base.wrapping_add(3),
                ]);
            }
            _ => {}
        }
    }
    indices
}

fn make_quad_indexed_indices(
    topology: PrimitiveTopology,
    index_format: IndexFormat,
    base_vertex: i32,
    first_index: u32,
    count: u32,
    gpu_va: u64,
    read_gpu: &dyn Fn(u64, &mut [u8]),
) -> Vec<u32> {
    let index_size = index_size(index_format);
    let mut source = vec![0u8; (first_index.saturating_add(count) as usize) * index_size];
    read_gpu(gpu_va, &mut source);

    let quads = quad_count(topology, count);
    let mut indices = Vec::with_capacity((quads * 6) as usize);
    for quad in 0..quads {
        let offsets = match topology {
            PrimitiveTopology::Quads => [
                first_index + quad * 4,
                first_index + quad * 4 + 1,
                first_index + quad * 4 + 2,
                first_index + quad * 4,
                first_index + quad * 4 + 2,
                first_index + quad * 4 + 3,
            ],
            PrimitiveTopology::QuadStrip => [
                first_index + quad * 2,
                first_index + quad * 2 + 3,
                first_index + quad * 2 + 1,
                first_index + quad * 2,
                first_index + quad * 2 + 2,
                first_index + quad * 2 + 3,
            ],
            _ => [0; 6],
        };
        for offset in offsets {
            indices
                .push(read_index(&source, index_format, offset).wrapping_add(base_vertex as u32));
        }
    }
    indices
}

fn index_size(index_format: IndexFormat) -> usize {
    match index_format {
        IndexFormat::UnsignedByte => 1,
        IndexFormat::UnsignedShort => 2,
        IndexFormat::UnsignedInt => 4,
    }
}

fn read_index(source: &[u8], index_format: IndexFormat, index: u32) -> u32 {
    let offset = index as usize * index_size(index_format);
    match index_format {
        IndexFormat::UnsignedByte => source.get(offset).copied().unwrap_or(0) as u32,
        IndexFormat::UnsignedShort => {
            let bytes = [
                source.get(offset).copied().unwrap_or(0),
                source.get(offset + 1).copied().unwrap_or(0),
            ];
            u16::from_le_bytes(bytes) as u32
        }
        IndexFormat::UnsignedInt => {
            let bytes = [
                source.get(offset).copied().unwrap_or(0),
                source.get(offset + 1).copied().unwrap_or(0),
                source.get(offset + 2).copied().unwrap_or(0),
                source.get(offset + 3).copied().unwrap_or(0),
            ];
            u32::from_le_bytes(bytes)
        }
    }
}

fn indices_as_bytes(indices: &[u32]) -> &[u8] {
    unsafe {
        std::slice::from_raw_parts(
            indices.as_ptr().cast::<u8>(),
            std::mem::size_of_val(indices),
        )
    }
}

fn quad_index_cache_key(
    topology: PrimitiveTopology,
    first: u32,
    count: u32,
    gpu_va: u64,
    base_vertex: u32,
    indexed: bool,
) -> u64 {
    let mut key = 0xC0DE_0000_0000_0000u64;
    key ^= (topology as u64) << 56;
    key ^= (first as u64) << 32;
    key ^= count as u64;
    key ^= gpu_va.rotate_left(17);
    key ^= (base_vertex as u64).rotate_left(7);
    key ^ if indexed { 1u64 << 55 } else { 0 }
}

/// Port of upstream `BufferCacheRuntime::PostCopyBarrier`.
fn post_copy_barrier(device: &ash::Device, cmd: vk::CommandBuffer) {
    let write_barrier = vk::MemoryBarrier::builder()
        .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
        .dst_access_mask(vk::AccessFlags::MEMORY_READ | vk::AccessFlags::MEMORY_WRITE)
        .build();
    unsafe {
        device.cmd_pipeline_barrier(
            cmd,
            vk::PipelineStageFlags::TRANSFER,
            vk::PipelineStageFlags::ALL_COMMANDS,
            vk::DependencyFlags::empty(),
            std::slice::from_ref(&write_barrier),
            &[],
            &[],
        );
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
