// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GPU buffer cache for vertex, index, uniform, and storage data.
//!
//! Ref: zuyu `vk_buffer_cache.h` — caches VkBuffer objects by GPU VA range
//! to avoid redundant uploads of unchanged data.

use std::collections::HashMap;
use std::ptr::NonNull;

use ash::vk;
use ash::vk::Handle;
use common::slot_vector::SlotVector;
use log::{debug, trace};
use smallvec::SmallVec;

use super::compute_pass::{QuadIndexedPass, Uint8Pass};
use super::descriptor_pool::DescriptorPool;
use super::scheduler::Scheduler;
use super::staging_buffer_pool::StagingBufferPool;
use super::update_descriptor::{ComputePassDescriptorQueue, UpdateDescriptorQueue};
use crate::buffer_cache::buffer_base::BufferBase;
use crate::buffer_cache::buffer_cache::BufferCache as CommonBufferCache;
use crate::buffer_cache::buffer_cache_base::{
    self as base, BufferCopy, BufferId, HostBindings, StagingBufferRef, NULL_BUFFER_ID,
};
use crate::buffer_cache::word_manager::DeviceTracker;
use crate::control::channel_state::ChannelState;
use crate::control::channel_state_cache::{ChannelInfo, ChannelSetupCaches};
use crate::engines::maxwell_3d::{IndexFormat, PrimitiveTopology};
use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
use crate::surface::{PixelFormat, MAX_DEPTH_STENCIL_FORMAT};
use crate::texture_cache::texture_cache_base::TICKS_TO_DESTROY;

/// Cached Vulkan buffer view for texture/image buffer descriptors.
pub struct CachedBufferView {
    pub offset: u32,
    pub size: u32,
    pub format: u32,
    pub view: vk::BufferView,
}

/// A cached GPU buffer backed by VkBuffer + VkDeviceMemory.
pub struct CachedBuffer {
    pub buffer: vk::Buffer,
    pub memory: vk::DeviceMemory,
    pub size: vk::DeviceSize,
    pub views: Vec<CachedBufferView>,
}

/// A replaced/invalidated buffer awaiting delayed destruction.
///
/// Commands referencing the buffer may already be recorded into the
/// scheduler's pending command buffer; MoltenVK encodes them at
/// vkQueueSubmit, so destroying immediately is a use-after-free.
struct SentencedBuffer {
    retire_tick: u64,
    buffer: CachedBuffer,
}

/// Port of upstream's anonymous `QuadIndexBuffer` hierarchy state.
struct QuadIndexBuffer {
    gpu_handle: u32,
    index_type: vk::IndexType,
    num_indices: u32,
}

impl Default for QuadIndexBuffer {
    fn default() -> Self {
        Self {
            gpu_handle: 0,
            index_type: vk::IndexType::UINT16,
            num_indices: 0,
        }
    }
}

fn index_type_from_num_elements(
    num_elements: u32,
    index_type_uint8_supported: bool,
) -> vk::IndexType {
    if num_elements <= 0xff && index_type_uint8_supported {
        vk::IndexType::UINT8_EXT
    } else if num_elements <= 0xffff {
        vk::IndexType::UINT16
    } else {
        vk::IndexType::UINT32
    }
}

fn bytes_per_index(index_type: vk::IndexType) -> usize {
    match index_type {
        vk::IndexType::UINT8_EXT => 1,
        vk::IndexType::UINT16 => 2,
        vk::IndexType::UINT32 => 4,
        _ => unreachable!("invalid Vulkan index type"),
    }
}

fn quad_count_for_topology(topology: PrimitiveTopology, num_indices: u32) -> u32 {
    match topology {
        PrimitiveTopology::Quads => num_indices / 4,
        PrimitiveTopology::QuadStrip => {
            if num_indices >= 4 {
                (num_indices - 2) / 2
            } else {
                0
            }
        }
        _ => unreachable!("invalid quad topology"),
    }
}

fn append_quad_index(bytes: &mut Vec<u8>, index_type: vk::IndexType, index: u32) {
    match index_type {
        vk::IndexType::UINT8_EXT => bytes.push(index as u8),
        vk::IndexType::UINT16 => bytes.extend_from_slice(&(index as u16).to_le_bytes()),
        vk::IndexType::UINT32 => bytes.extend_from_slice(&index.to_le_bytes()),
        _ => unreachable!("invalid Vulkan index type"),
    }
}

fn make_quad_lut(
    topology: PrimitiveTopology,
    num_indices: u32,
    index_type: vk::IndexType,
) -> Vec<u8> {
    let num_quads = quad_count_for_topology(topology, num_indices);
    let mut bytes = Vec::with_capacity(num_quads as usize * 6 * 4 * bytes_per_index(index_type));
    for first in 0u32..4 {
        for quad in 0..num_quads {
            let offsets = match topology {
                PrimitiveTopology::Quads => [0, 1, 2, 0, 2, 3]
                    .map(|index| first.wrapping_add(index).wrapping_add(quad.wrapping_mul(4))),
                PrimitiveTopology::QuadStrip => [0, 3, 1, 0, 2, 3]
                    .map(|index| first.wrapping_add(index).wrapping_add(quad.wrapping_mul(2))),
                _ => unreachable!("invalid quad topology"),
            };
            for index in offsets {
                append_quad_index(&mut bytes, index_type, index);
            }
        }
    }
    bytes
}

/// Buffer cache parameters matching upstream `Vulkan::BufferCacheParams`.
pub struct BufferCacheParams;

impl BufferCacheParams {
    pub const IS_OPENGL: bool = false;
    pub const HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS: bool = false;
    pub const HAS_FULL_INDEX_AND_PRIMITIVE_SUPPORT: bool = false;
    pub const NEEDS_BIND_UNIFORM_INDEX: bool = false;
    pub const NEEDS_BIND_STORAGE_INDEX: bool = false;
    pub const USE_MEMORY_MAPS: bool = true;
    pub const SEPARATE_IMAGE_BUFFER_BINDINGS: bool = false;
    pub const USE_MEMORY_MAPS_FOR_UPLOADS: bool = true;
}

impl base::BufferCacheParams for BufferCacheParams {
    const IS_OPENGL: bool = Self::IS_OPENGL;
    const HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS: bool =
        Self::HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS;
    const HAS_FULL_INDEX_AND_PRIMITIVE_SUPPORT: bool = Self::HAS_FULL_INDEX_AND_PRIMITIVE_SUPPORT;
    const NEEDS_BIND_UNIFORM_INDEX: bool = Self::NEEDS_BIND_UNIFORM_INDEX;
    const NEEDS_BIND_STORAGE_INDEX: bool = Self::NEEDS_BIND_STORAGE_INDEX;
    const USE_MEMORY_MAPS: bool = Self::USE_MEMORY_MAPS;
    const SEPARATE_IMAGE_BUFFER_BINDINGS: bool = Self::SEPARATE_IMAGE_BUFFER_BINDINGS;
    const USE_MEMORY_MAPS_FOR_UPLOADS: bool = Self::USE_MEMORY_MAPS_FOR_UPLOADS;
}

fn common_buffer_usage_flags() -> vk::BufferUsageFlags {
    vk::BufferUsageFlags::TRANSFER_SRC
        | vk::BufferUsageFlags::TRANSFER_DST
        | vk::BufferUsageFlags::UNIFORM_TEXEL_BUFFER
        | vk::BufferUsageFlags::STORAGE_TEXEL_BUFFER
        | vk::BufferUsageFlags::UNIFORM_BUFFER
        | vk::BufferUsageFlags::STORAGE_BUFFER
        | vk::BufferUsageFlags::INDEX_BUFFER
        | vk::BufferUsageFlags::VERTEX_BUFFER
        | vk::BufferUsageFlags::INDIRECT_BUFFER
}

pub type VulkanCommonBufferCache = CommonBufferCache<BufferCacheParams, MaxwellDeviceMemoryManager>;

impl DeviceTracker for MaxwellDeviceMemoryManager {
    fn update_pages_cached_count(&self, addr: u64, size: u64, delta: i32) {
        MaxwellDeviceMemoryManager::update_pages_cached_count(self, addr, size as usize, delta);
    }
}

/// Vulkan implementation of upstream `BufferCacheRuntime`.
///
/// This is the runtime service owner used by the common `BufferCache<P>` port:
/// scheduler-recorded copies/clears, staging allocation, and backend buffer
/// materialization. The existing `BufferCache` below is still the legacy direct
/// rasterizer cache and will be retired once the rasterizer is moved onto the
/// common cache.
pub struct BufferCacheRuntime {
    device: ash::Device,
    instance: ash::Instance,
    physical_device: vk::PhysicalDevice,
    scheduler: NonNull<Scheduler>,
    staging_pool: NonNull<StagingBufferPool>,
    guest_descriptor_queue: NonNull<UpdateDescriptorQueue>,
    uint8_pass: Option<Uint8Pass>,
    quad_index_pass: QuadIndexedPass,
    quad_array_index_buffer: QuadIndexBuffer,
    quad_strip_index_buffer: QuadIndexBuffer,
    index_type_uint8_supported: bool,
    buffers: HashMap<u32, CachedBuffer>,
    staging_refs: HashMap<usize, super::staging_buffer_pool::StagingBuffer>,
    next_handle: u32,
    null_buffer: vk::Buffer,
    null_memory: vk::DeviceMemory,
    null_buffer_size: vk::DeviceSize,
    has_null_descriptor: bool,
    extended_dynamic_state_supported: bool,
    transform_feedback: Option<vk::ExtTransformFeedbackFn>,
    max_vertex_input_bindings: u32,
}

impl BufferCacheRuntime {
    pub fn new(
        device: ash::Device,
        instance: ash::Instance,
        physical_device: vk::PhysicalDevice,
        scheduler: &mut Scheduler,
        staging_pool: &mut StagingBufferPool,
        guest_descriptor_queue: &mut UpdateDescriptorQueue,
        compute_pass_descriptor_queue: &mut ComputePassDescriptorQueue,
        descriptor_pool: &DescriptorPool,
        driver_id: vk::DriverId,
        index_type_uint8_supported: bool,
        has_null_descriptor: bool,
        extended_dynamic_state_supported: bool,
        transform_feedback_supported: bool,
        max_vertex_input_bindings: u32,
    ) -> Result<Self, vk::Result> {
        let quad_index_pass = QuadIndexedPass::new(
            &device,
            scheduler,
            descriptor_pool,
            staging_pool,
            compute_pass_descriptor_queue,
        )?;
        let uint8_pass = if driver_id != vk::DriverId::QUALCOMM_PROPRIETARY {
            Some(Uint8Pass::new(
                &device,
                scheduler,
                descriptor_pool,
                staging_pool,
                compute_pass_descriptor_queue,
            )?)
        } else {
            None
        };
        let transform_feedback = transform_feedback_supported.then(|| {
            vk::ExtTransformFeedbackFn::load(|name| unsafe {
                std::mem::transmute(instance.get_device_proc_addr(device.handle(), name.as_ptr()))
            })
        });
        Ok(Self {
            device,
            instance,
            physical_device,
            scheduler: NonNull::from(scheduler),
            staging_pool: NonNull::from(staging_pool),
            guest_descriptor_queue: NonNull::from(guest_descriptor_queue),
            uint8_pass,
            quad_index_pass,
            quad_array_index_buffer: QuadIndexBuffer::default(),
            quad_strip_index_buffer: QuadIndexBuffer::default(),
            index_type_uint8_supported,
            buffers: HashMap::new(),
            staging_refs: HashMap::new(),
            next_handle: 1,
            null_buffer: vk::Buffer::null(),
            null_memory: vk::DeviceMemory::null(),
            null_buffer_size: 4,
            has_null_descriptor,
            extended_dynamic_state_supported,
            transform_feedback,
            max_vertex_input_bindings,
        })
    }

    fn scheduler(&mut self) -> &mut Scheduler {
        // SAFETY: the runtime is constructed from boxed rasterizer services.
        // Their addresses remain stable and they outlive the runtime.
        unsafe { self.scheduler.as_mut() }
    }

    fn staging_pool(&mut self) -> &mut StagingBufferPool {
        // SAFETY: see `scheduler`.
        unsafe { self.staging_pool.as_mut() }
    }

    fn guest_descriptor_queue(&mut self) -> &mut UpdateDescriptorQueue {
        // SAFETY: see `scheduler`.
        unsafe { self.guest_descriptor_queue.as_mut() }
    }

    fn update_quad_index_buffer(&mut self, topology: PrimitiveTopology, num_indices: u32) {
        let (current_num_indices, old_handle) = match topology {
            PrimitiveTopology::Quads => (
                self.quad_array_index_buffer.num_indices,
                self.quad_array_index_buffer.gpu_handle,
            ),
            PrimitiveTopology::QuadStrip => (
                self.quad_strip_index_buffer.num_indices,
                self.quad_strip_index_buffer.gpu_handle,
            ),
            _ => unreachable!("invalid quad topology"),
        };
        if num_indices <= current_num_indices {
            return;
        }

        self.scheduler().finish();
        if old_handle != 0 {
            if let Some(old) = self.buffers.remove(&old_handle) {
                unsafe {
                    for view in old.views {
                        self.device.destroy_buffer_view(view.view, None);
                    }
                    self.device.destroy_buffer(old.buffer, None);
                    self.device.free_memory(old.memory, None);
                }
            }
        }

        let index_type = index_type_from_num_elements(num_indices, self.index_type_uint8_supported);
        let data = make_quad_lut(topology, num_indices, index_type);
        let size = data.len() as vk::DeviceSize;
        let cached = self
            .create_gpu_buffer(
                size,
                vk::BufferUsageFlags::INDEX_BUFFER | vk::BufferUsageFlags::TRANSFER_DST,
            )
            .expect("quad index buffer allocation failed");
        let staging = self
            .staging_pool()
            .request_upload_buffer(size)
            .expect("quad index upload staging allocation failed");
        unsafe {
            std::slice::from_raw_parts_mut(staging.mapped, data.len()).copy_from_slice(&data);
        }

        let device = self.device.clone();
        let src_buffer = staging.buffer;
        let src_offset = staging.offset;
        let dst_buffer = cached.buffer;
        self.scheduler().request_outside_renderpass();
        self.scheduler().record(move |cmdbuf| unsafe {
            let copy = vk::BufferCopy {
                src_offset,
                dst_offset: 0,
                size,
            };
            let barrier = vk::BufferMemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .dst_access_mask(vk::AccessFlags::INDEX_READ)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .buffer(dst_buffer)
                .offset(0)
                .size(size)
                .build();
            device.cmd_copy_buffer(cmdbuf, src_buffer, dst_buffer, &[copy]);
            device.cmd_pipeline_barrier(
                cmdbuf,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::VERTEX_INPUT,
                vk::DependencyFlags::empty(),
                &[],
                std::slice::from_ref(&barrier),
                &[],
            );
        });

        let gpu_handle = self.allocate_handle();
        self.buffers.insert(gpu_handle, cached);
        let state = match topology {
            PrimitiveTopology::Quads => &mut self.quad_array_index_buffer,
            PrimitiveTopology::QuadStrip => &mut self.quad_strip_index_buffer,
            _ => unreachable!("invalid quad topology"),
        };
        state.gpu_handle = gpu_handle;
        state.index_type = index_type;
        state.num_indices = num_indices;
    }

    /// Port of upstream `BufferCacheRuntime::ReserveNullBuffer`.
    fn reserve_null_buffer(&mut self) {
        if self.null_buffer != vk::Buffer::null() {
            return;
        }
        let (buffer, memory, size) =
            create_runtime_null_buffer(&self.device, &self.instance, self.physical_device);
        if buffer == vk::Buffer::null() {
            return;
        }
        self.null_buffer = buffer;
        self.null_memory = memory;
        self.null_buffer_size = size;

        let device = self.device.clone();
        self.scheduler().request_outside_renderpass();
        self.scheduler().record(move |cmdbuf| unsafe {
            device.cmd_fill_buffer(cmdbuf, buffer, 0, vk::WHOLE_SIZE, 0);
        });
    }

    fn allocate_handle(&mut self) -> u32 {
        let handle = self.next_handle;
        self.next_handle = self.next_handle.wrapping_add(1).max(1);
        handle
    }

    fn create_gpu_buffer(
        &self,
        size: vk::DeviceSize,
        usage: vk::BufferUsageFlags,
    ) -> Option<CachedBuffer> {
        let buffer_info = vk::BufferCreateInfo::builder()
            .size(size.max(1))
            .usage(usage)
            .sharing_mode(vk::SharingMode::EXCLUSIVE)
            .build();
        let buffer = unsafe { self.device.create_buffer(&buffer_info, None).ok()? };
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
                return None;
            }
        };
        unsafe {
            if self.device.bind_buffer_memory(buffer, memory, 0).is_err() {
                self.device.destroy_buffer(buffer, None);
                self.device.free_memory(memory, None);
                return None;
            }
        }
        Some(CachedBuffer {
            buffer,
            memory,
            size: size.max(1),
            views: Vec::new(),
        })
    }

    fn resolve_buffer(&self, gpu_handle: u32) -> vk::Buffer {
        if gpu_handle == 0 {
            return vk::Buffer::null();
        }
        self.buffers
            .get(&gpu_handle)
            .map(|buffer| buffer.buffer)
            .unwrap_or(vk::Buffer::null())
    }

    fn staging_ref_from_map(
        &mut self,
        staging: super::staging_buffer_pool::StagingBuffer,
    ) -> StagingBufferRef {
        let handle = self.allocate_handle();
        self.buffers.insert(
            handle,
            CachedBuffer {
                buffer: staging.buffer,
                memory: vk::DeviceMemory::null(),
                size: staging.size,
                views: Vec::new(),
            },
        );
        self.staging_refs.insert(staging.index as usize, staging);
        unsafe {
            StagingBufferRef::from_mapped_backend(
                NULL_BUFFER_ID,
                handle,
                staging.offset,
                staging.index as usize,
                staging.mapped,
                staging.size as usize,
                std::ptr::null_mut(),
            )
        }
    }

    fn make_buffer_copies(copies: &[BufferCopy]) -> Vec<vk::BufferCopy> {
        copies
            .iter()
            .map(|copy| vk::BufferCopy {
                src_offset: copy.src_offset,
                dst_offset: copy.dst_offset,
                size: copy.size,
            })
            .collect()
    }

    fn pixel_format_from_raw(format: u32) -> Option<PixelFormat> {
        if format >= MAX_DEPTH_STENCIL_FORMAT {
            return None;
        }
        // SAFETY: PixelFormat is repr(u32), contiguous up to MaxDepthStencilFormat,
        // and the guard above excludes sentinel/invalid values.
        Some(unsafe { std::mem::transmute::<u32, PixelFormat>(format) })
    }

    fn is_buffer_format_supported(&self, format: vk::Format) -> bool {
        let required = vk::FormatFeatureFlags::STORAGE_TEXEL_BUFFER
            | vk::FormatFeatureFlags::UNIFORM_TEXEL_BUFFER;
        let props = unsafe {
            self.instance
                .get_physical_device_format_properties(self.physical_device, format)
        };
        (props.buffer_features & required) == required
    }

    fn supported_buffer_format(&self, wanted_format: vk::Format) -> vk::Format {
        if self.is_buffer_format_supported(wanted_format) {
            return wanted_format;
        }
        if let Some(alternatives) =
            crate::vulkan_common::vulkan_device::format_alternatives(wanted_format)
        {
            for &format in alternatives {
                if self.is_buffer_format_supported(format) {
                    return format;
                }
            }
        }
        wanted_format
    }

    fn create_buffer_view(
        &self,
        buffer: vk::Buffer,
        offset: u32,
        size: u32,
        format: u32,
    ) -> vk::BufferView {
        let Some(pixel_format) = Self::pixel_format_from_raw(format) else {
            return vk::BufferView::null();
        };
        if buffer == vk::Buffer::null() || size == 0 {
            return vk::BufferView::null();
        }
        let format_info = super::maxwell_to_vk::surface_format(pixel_format);
        let format = self.supported_buffer_format(format_info.format);
        let info = vk::BufferViewCreateInfo::builder()
            .buffer(buffer)
            .format(format)
            .offset(offset as vk::DeviceSize)
            .range(size as vk::DeviceSize)
            .build();
        unsafe {
            self.device
                .create_buffer_view(&info, None)
                .unwrap_or(vk::BufferView::null())
        }
    }

    fn buffer_view(
        &mut self,
        gpu_handle: u32,
        offset: u32,
        size: u32,
        format: u32,
    ) -> vk::BufferView {
        let Some(buffer) = self.buffers.get(&gpu_handle) else {
            return vk::BufferView::null();
        };
        if let Some(view) = buffer
            .views
            .iter()
            .find(|view| view.offset == offset && view.size == size && view.format == format)
        {
            return view.view;
        }
        let raw_buffer = buffer.buffer;
        let view = self.create_buffer_view(raw_buffer, offset, size, format);
        if view == vk::BufferView::null() {
            return view;
        }
        if let Some(buffer) = self.buffers.get_mut(&gpu_handle) {
            buffer.views.push(CachedBufferView {
                offset,
                size,
                format,
                view,
            });
        }
        view
    }

    fn bind_buffer_descriptor(&mut self, gpu_handle: u32, offset: u32, size: u32) {
        // Upstream binds a reserved null buffer for unbound uniform/storage
        // descriptors. `resolve_buffer` keeps returning the null sentinel so
        // copy/clear paths still skip missing handles; the substitution is done
        // here, locally to descriptor binding.
        let resolved = self.resolve_buffer(gpu_handle);
        let (buffer, offset, size) = if resolved == vk::Buffer::null() && !self.has_null_descriptor
        {
            self.reserve_null_buffer();
            (self.null_buffer, 0, self.null_buffer_size as u32)
        } else {
            (resolved, offset, size)
        };
        self.guest_descriptor_queue().add_buffer(
            buffer,
            offset as vk::DeviceSize,
            size.max(1) as vk::DeviceSize,
        );
    }
}

impl base::BufferCacheRuntime for BufferCacheRuntime {
    fn initialize_backend_buffer(&mut self, buffer: &mut BufferBase) {
        if buffer.gpu_handle != 0 || buffer.size_bytes() == 0 {
            return;
        }
        let Some(gpu_buffer) = self.create_gpu_buffer(
            buffer.size_bytes() as vk::DeviceSize,
            common_buffer_usage_flags(),
        ) else {
            return;
        };
        let handle = self.allocate_handle();
        self.buffers.insert(handle, gpu_buffer);
        buffer.gpu_handle = handle;
    }

    fn tick_frame(&mut self) {}

    fn current_tick(&self) -> u64 {
        unsafe { self.scheduler.as_ref() }.current_tick()
    }

    fn known_gpu_tick(&self) -> u64 {
        unsafe { self.scheduler.as_ref() }.known_gpu_tick()
    }

    fn wait(&mut self, tick: u64) {
        self.scheduler().wait(tick);
    }

    fn can_report_memory_usage(&self) -> bool {
        false
    }

    fn get_device_local_memory(&self) -> u64 {
        0
    }

    fn get_device_memory_usage(&self) -> u64 {
        self.buffers
            .values()
            .filter(|buffer| buffer.memory != vk::DeviceMemory::null())
            .map(|buffer| buffer.size)
            .sum()
    }

    fn get_storage_buffer_alignment(&self) -> u32 {
        let properties = unsafe {
            self.instance
                .get_physical_device_properties(self.physical_device)
        };
        properties.limits.min_storage_buffer_offset_alignment.max(1) as u32
    }

    fn resolve_backend_buffer_raw(&self, gpu_handle: u32) -> u64 {
        self.resolve_buffer(gpu_handle).as_raw()
    }

    fn finish(&mut self) {
        self.scheduler().finish();
    }

    fn upload_staging_buffer(&mut self, size: u64) -> StagingBufferRef {
        let staging = self
            .staging_pool()
            .request_upload_buffer(size as vk::DeviceSize)
            .expect("Vulkan upload staging allocation failed");
        self.staging_ref_from_map(staging)
    }

    fn download_staging_buffer(&mut self, size: u64, deferred: bool) -> StagingBufferRef {
        let staging = self
            .staging_pool()
            .request_download_buffer(size as vk::DeviceSize, deferred)
            .expect("Vulkan download staging allocation failed");
        self.staging_ref_from_map(staging)
    }

    fn free_deferred_staging_buffer(&mut self, buffer: &mut StagingBufferRef) {
        if let Some(mut staging) = self.staging_refs.remove(&buffer.index) {
            self.staging_pool().free_deferred(&mut staging);
            self.staging_refs.insert(buffer.index, staging);
        }
    }

    fn can_reorder_upload(&self, _buffer_id: BufferId, _copies: &[BufferCopy]) -> bool {
        false
    }

    fn pre_copy_barrier(&mut self) {
        let device = self.device.clone();
        self.scheduler().request_outside_renderpass();
        self.scheduler().record(move |cmdbuf| {
            let read_barrier = vk::MemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::MEMORY_WRITE)
                .dst_access_mask(vk::AccessFlags::TRANSFER_READ | vk::AccessFlags::TRANSFER_WRITE)
                .build();
            unsafe {
                device.cmd_pipeline_barrier(
                    cmdbuf,
                    vk::PipelineStageFlags::ALL_COMMANDS,
                    vk::PipelineStageFlags::TRANSFER,
                    vk::DependencyFlags::empty(),
                    std::slice::from_ref(&read_barrier),
                    &[],
                    &[],
                );
            }
        });
    }

    fn post_copy_barrier(&mut self) {
        let device = self.device.clone();
        self.scheduler().request_outside_renderpass();
        self.scheduler().record(move |cmdbuf| {
            let write_barrier = vk::MemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .dst_access_mask(vk::AccessFlags::MEMORY_READ | vk::AccessFlags::MEMORY_WRITE)
                .build();
            unsafe {
                device.cmd_pipeline_barrier(
                    cmdbuf,
                    vk::PipelineStageFlags::TRANSFER,
                    vk::PipelineStageFlags::ALL_COMMANDS,
                    vk::DependencyFlags::empty(),
                    std::slice::from_ref(&write_barrier),
                    &[],
                    &[],
                );
            }
        });
    }

    fn copy_buffer(
        &mut self,
        _dst: BufferId,
        dst_gpu_handle: u32,
        _src: BufferId,
        src_gpu_handle: u32,
        copies: &[BufferCopy],
        barrier: bool,
        _can_reorder_upload: bool,
    ) {
        if copies.is_empty() {
            return;
        }
        let dst_buffer = self.resolve_buffer(dst_gpu_handle);
        let src_buffer = self.resolve_buffer(src_gpu_handle);
        if dst_buffer == vk::Buffer::null() || src_buffer == vk::Buffer::null() {
            return;
        }
        let vk_copies = Self::make_buffer_copies(copies);
        let device = self.device.clone();
        self.scheduler().request_outside_renderpass();
        self.scheduler().record(move |cmdbuf| {
            let read_barrier = vk::MemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::MEMORY_WRITE)
                .dst_access_mask(vk::AccessFlags::TRANSFER_READ | vk::AccessFlags::TRANSFER_WRITE)
                .build();
            let write_barrier = vk::MemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .dst_access_mask(vk::AccessFlags::MEMORY_READ | vk::AccessFlags::MEMORY_WRITE)
                .build();
            unsafe {
                if barrier {
                    device.cmd_pipeline_barrier(
                        cmdbuf,
                        vk::PipelineStageFlags::ALL_COMMANDS,
                        vk::PipelineStageFlags::TRANSFER,
                        vk::DependencyFlags::empty(),
                        std::slice::from_ref(&read_barrier),
                        &[],
                        &[],
                    );
                }
                device.cmd_copy_buffer(cmdbuf, src_buffer, dst_buffer, &vk_copies);
                if barrier {
                    device.cmd_pipeline_barrier(
                        cmdbuf,
                        vk::PipelineStageFlags::TRANSFER,
                        vk::PipelineStageFlags::ALL_COMMANDS,
                        vk::DependencyFlags::empty(),
                        std::slice::from_ref(&write_barrier),
                        &[],
                        &[],
                    );
                }
            }
        });
    }

    fn clear_buffer(
        &mut self,
        _buffer: BufferId,
        gpu_handle: u32,
        offset: u32,
        size: u64,
        value: u32,
    ) {
        if size == 0 {
            return;
        }
        let dest_buffer = self.resolve_buffer(gpu_handle);
        if dest_buffer == vk::Buffer::null() {
            return;
        }
        let device = self.device.clone();
        self.scheduler().request_outside_renderpass();
        self.scheduler().record(move |cmdbuf| {
            let read_barrier = vk::MemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::MEMORY_WRITE)
                .dst_access_mask(vk::AccessFlags::TRANSFER_READ | vk::AccessFlags::TRANSFER_WRITE)
                .build();
            let write_barrier = vk::MemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .dst_access_mask(vk::AccessFlags::MEMORY_READ | vk::AccessFlags::MEMORY_WRITE)
                .build();
            unsafe {
                device.cmd_pipeline_barrier(
                    cmdbuf,
                    vk::PipelineStageFlags::ALL_COMMANDS,
                    vk::PipelineStageFlags::TRANSFER,
                    vk::DependencyFlags::empty(),
                    std::slice::from_ref(&read_barrier),
                    &[],
                    &[],
                );
                device.cmd_fill_buffer(cmdbuf, dest_buffer, offset as u64, size, value);
                device.cmd_pipeline_barrier(
                    cmdbuf,
                    vk::PipelineStageFlags::TRANSFER,
                    vk::PipelineStageFlags::ALL_COMMANDS,
                    vk::DependencyFlags::empty(),
                    std::slice::from_ref(&write_barrier),
                    &[],
                    &[],
                );
            }
        });
    }

    fn bind_index_buffer(
        &mut self,
        topology: PrimitiveTopology,
        index_format: IndexFormat,
        base_vertex: u32,
        num_indices: u32,
        buffer: &mut BufferBase,
        offset: u32,
        _size: u32,
    ) {
        let mut buffer = self.resolve_buffer(buffer.gpu_handle);
        let mut vk_offset = u64::from(offset);
        let mut index_type = match index_format {
            IndexFormat::UnsignedByte => vk::IndexType::UINT8_EXT,
            IndexFormat::UnsignedShort => vk::IndexType::UINT16,
            IndexFormat::UnsignedInt => vk::IndexType::UINT32,
        };
        if matches!(
            topology,
            PrimitiveTopology::Quads | PrimitiveTopology::QuadStrip
        ) {
            index_type = vk::IndexType::UINT32;
            (buffer, vk_offset) = self.quad_index_pass.assemble(
                index_format,
                num_indices,
                base_vertex,
                buffer,
                offset,
                topology == PrimitiveTopology::QuadStrip,
            );
        } else if index_type == vk::IndexType::UINT8_EXT && !self.index_type_uint8_supported {
            index_type = vk::IndexType::UINT16;
            if let Some(uint8_pass) = &mut self.uint8_pass {
                (buffer, vk_offset) = uint8_pass.assemble(num_indices, buffer, offset);
            }
        }
        if buffer == vk::Buffer::null() {
            self.reserve_null_buffer();
            buffer = self.null_buffer;
        }
        let device = self.device.clone();
        self.scheduler().record(move |cmdbuf| unsafe {
            device.cmd_bind_index_buffer(cmdbuf, buffer, vk_offset, index_type);
        });
    }

    fn bind_quad_index_buffer(&mut self, topology: PrimitiveTopology, first: u32, count: u32) {
        if count == 0 {
            self.reserve_null_buffer();
            let buffer = self.null_buffer;
            let device = self.device.clone();
            self.scheduler().record(move |cmdbuf| unsafe {
                device.cmd_bind_index_buffer(cmdbuf, buffer, 0, vk::IndexType::UINT32);
            });
            return;
        }

        self.update_quad_index_buffer(topology, first.wrapping_add(count));
        let state = match topology {
            PrimitiveTopology::Quads => &self.quad_array_index_buffer,
            PrimitiveTopology::QuadStrip => &self.quad_strip_index_buffer,
            _ => return,
        };
        let sub_first_offset =
            u64::from(first % 4) * u64::from(quad_count_for_topology(topology, state.num_indices));
        let offset = (sub_first_offset + u64::from(quad_count_for_topology(topology, first)))
            * 6
            * bytes_per_index(state.index_type) as u64;
        let buffer = self.resolve_buffer(state.gpu_handle);
        let index_type = state.index_type;
        let device = self.device.clone();
        self.scheduler().record(move |cmdbuf| unsafe {
            device.cmd_bind_index_buffer(cmdbuf, buffer, offset, index_type);
        });
    }

    fn bind_vertex_buffers(
        &mut self,
        bindings: &HostBindings,
        buffers: &mut common::slot_vector::SlotVector<BufferBase>,
    ) {
        let binding_count = vertex_binding_count(
            bindings.min_index,
            bindings.max_index,
            self.max_vertex_input_bindings,
        ) as usize;
        if binding_count == 0 {
            return;
        }
        let mut vk_buffers = SmallVec::<[vk::Buffer; 32]>::new();
        let mut offsets = SmallVec::<[u64; 32]>::new();
        let mut sizes = SmallVec::<[u64; 32]>::new();
        let mut strides = SmallVec::<[u64; 32]>::new();
        for index in 0..binding_count {
            let buffer_id = bindings.buffer_ids[index];
            let buffer = if buffer_id.is_valid() {
                self.resolve_buffer(buffers[buffer_id].gpu_handle)
            } else {
                vk::Buffer::null()
            };
            if buffer == vk::Buffer::null() && !self.has_null_descriptor {
                self.reserve_null_buffer();
            }
            let (buffer, offset, size) = prepare_vertex_binding(
                buffer,
                bindings.offsets[index],
                bindings.sizes[index],
                self.has_null_descriptor,
                self.null_buffer,
                self.null_buffer_size,
            );
            vk_buffers.push(buffer);
            offsets.push(offset);
            sizes.push(size);
            strides.push(bindings.strides[index]);
        }
        let first_binding = bindings.min_index;
        let dynamic_stride = self.extended_dynamic_state_supported;
        let device = self.device.clone();
        self.scheduler().record(move |cmdbuf| unsafe {
            if dynamic_stride {
                device.cmd_bind_vertex_buffers2(
                    cmdbuf,
                    first_binding,
                    &vk_buffers,
                    &offsets,
                    Some(&sizes),
                    Some(&strides),
                );
            } else {
                device.cmd_bind_vertex_buffers(cmdbuf, first_binding, &vk_buffers, &offsets);
            }
        });
    }

    fn bind_uniform_buffer(
        &mut self,
        _stage: usize,
        _binding_index: u32,
        _buffer: BufferId,
        gpu_handle: u32,
        offset: u32,
        size: u32,
    ) {
        self.bind_buffer_descriptor(gpu_handle, offset, size);
    }

    fn bind_storage_buffer(
        &mut self,
        _stage: usize,
        _binding_index: u32,
        buffer: &mut BufferBase,
        offset: u32,
        size: u32,
        _is_written: bool,
    ) {
        self.bind_buffer_descriptor(buffer.gpu_handle, offset, size);
    }

    fn bind_texture_buffer(
        &mut self,
        _buffer: BufferId,
        gpu_handle: u32,
        offset: u32,
        size: u32,
        format: u32,
    ) {
        let view = self.buffer_view(gpu_handle, offset, size, format);
        self.guest_descriptor_queue().add_texel_buffer(view);
    }

    fn bind_image_buffer(
        &mut self,
        _buffer: BufferId,
        gpu_handle: u32,
        offset: u32,
        size: u32,
        format: u32,
    ) {
        let view = self.buffer_view(gpu_handle, offset, size, format);
        self.guest_descriptor_queue().add_texel_buffer(view);
    }

    fn bind_transform_feedback_buffers(
        &mut self,
        bindings: &HostBindings,
        buffers: &mut SlotVector<BufferBase>,
    ) {
        let Some(transform_feedback) = self.transform_feedback.clone() else {
            return;
        };
        let buffer_handles: Vec<vk::Buffer> = bindings
            .buffer_ids
            .iter()
            .map(|&buffer_id| self.resolve_buffer(buffers[buffer_id].gpu_handle))
            .collect();
        let offsets: Vec<vk::DeviceSize> = bindings.offsets.iter().copied().collect();
        let sizes: Vec<vk::DeviceSize> = bindings.sizes.iter().copied().collect();
        self.scheduler().record(move |command_buffer| unsafe {
            (transform_feedback.cmd_bind_transform_feedback_buffers_ext)(
                command_buffer,
                0,
                buffer_handles.len() as u32,
                buffer_handles.as_ptr(),
                offsets.as_ptr(),
                sizes.as_ptr(),
            );
        });
    }

    fn bind_compute_uniform_buffer(
        &mut self,
        _binding_index: u32,
        buffer: BufferId,
        gpu_handle: u32,
        offset: u32,
        size: u32,
    ) {
        let _ = buffer;
        self.bind_buffer_descriptor(gpu_handle, offset, size);
    }

    fn bind_compute_storage_buffer(
        &mut self,
        _binding_index: u32,
        buffer: &mut BufferBase,
        offset: u32,
        size: u32,
        _is_written: bool,
    ) {
        // Vulkan shares the same descriptor-buffer path as graphics storage
        // buffers (only NEEDS_BIND_STORAGE_INDEX backends route here at all).
        self.bind_buffer_descriptor(buffer.gpu_handle, offset, size);
    }

    fn with_mapped_uniform_buffer(
        &mut self,
        _stage: usize,
        _binding_index: u32,
        size: u32,
        write: &mut dyn FnMut(&mut [u8]),
    ) -> bool {
        let staging = self
            .staging_pool()
            .request_upload_buffer(size as vk::DeviceSize);
        let Some(staging) = staging else {
            return false;
        };
        unsafe {
            let span = std::slice::from_raw_parts_mut(staging.mapped, size as usize);
            write(span);
        }
        self.guest_descriptor_queue().add_buffer(
            staging.buffer,
            staging.offset,
            size as vk::DeviceSize,
        );
        true
    }
}

impl Drop for BufferCacheRuntime {
    fn drop(&mut self) {
        unsafe {
            for (_, buffer) in self.buffers.drain() {
                for view in buffer.views {
                    self.device.destroy_buffer_view(view.view, None);
                }
                if buffer.memory == vk::DeviceMemory::null() {
                    continue;
                }
                self.device.destroy_buffer(buffer.buffer, None);
                self.device.free_memory(buffer.memory, None);
            }
            if self.null_buffer != vk::Buffer::null() {
                self.device.destroy_buffer(self.null_buffer, None);
            }
            if self.null_memory != vk::DeviceMemory::null() {
                self.device.free_memory(self.null_memory, None);
            }
        }
    }
}

fn create_runtime_null_buffer(
    device: &ash::Device,
    instance: &ash::Instance,
    physical_device: vk::PhysicalDevice,
) -> (vk::Buffer, vk::DeviceMemory, vk::DeviceSize) {
    let size = 4;
    let info = vk::BufferCreateInfo::builder()
        .size(size)
        .usage(runtime_null_buffer_usage_flags())
        .sharing_mode(vk::SharingMode::EXCLUSIVE)
        .build();
    let Ok(buffer) = (unsafe { device.create_buffer(&info, None) }) else {
        return (vk::Buffer::null(), vk::DeviceMemory::null(), size);
    };
    let requirements = unsafe { device.get_buffer_memory_requirements(buffer) };
    let Some(memory_type) =
        find_device_local_memory(instance, physical_device, requirements.memory_type_bits)
    else {
        unsafe {
            device.destroy_buffer(buffer, None);
        }
        return (vk::Buffer::null(), vk::DeviceMemory::null(), size);
    };
    let alloc = vk::MemoryAllocateInfo::builder()
        .allocation_size(requirements.size)
        .memory_type_index(memory_type)
        .build();
    let Ok(memory) = (unsafe { device.allocate_memory(&alloc, None) }) else {
        unsafe {
            device.destroy_buffer(buffer, None);
        }
        return (vk::Buffer::null(), vk::DeviceMemory::null(), size);
    };
    if unsafe { device.bind_buffer_memory(buffer, memory, 0) }.is_err() {
        unsafe {
            device.destroy_buffer(buffer, None);
            device.free_memory(memory, None);
        }
        return (vk::Buffer::null(), vk::DeviceMemory::null(), size);
    }
    (buffer, memory, size)
}

fn runtime_null_buffer_usage_flags() -> vk::BufferUsageFlags {
    vk::BufferUsageFlags::VERTEX_BUFFER
        | vk::BufferUsageFlags::INDEX_BUFFER
        | vk::BufferUsageFlags::TRANSFER_DST
        | vk::BufferUsageFlags::INDIRECT_BUFFER
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
    /// Delayed-destruction ring for replaced/invalidated buffers, mirroring
    /// the texture cache's sentenced resources (upstream
    /// `DelayedDestructionRing` with `TICKS_TO_DESTROY`).
    sentenced: Vec<SentencedBuffer>,

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
            sentenced: Vec::new(),
            null_buffer,
            null_memory,
        })
    }

    /// Queue a no-longer-cached buffer for destruction once the GPU can no
    /// longer reference it. Recorded-but-unsubmitted commands (vertex/index
    /// binds, staging copies) keep the handle alive until the scheduler has
    /// advanced past the retire tick.
    fn sentence(&mut self, buffer: CachedBuffer) {
        // The retire tick is finalized at the next `tick_frame`: this cache
        // has no scheduler handle, so the pending submission tick is stamped
        // there. `u64::MAX` marks a not-yet-finalized entry.
        self.sentenced.push(SentencedBuffer {
            retire_tick: u64::MAX,
            buffer,
        });
    }

    /// Advance the delayed-destruction ring. Called once per frame by the
    /// rasterizer with the GPU-completed tick (`Scheduler::known_gpu_tick`),
    /// like `TextureCache::tick_frame`. Destruction only happens once the GPU
    /// has passed the submission that could last reference a buffer.
    pub fn tick_frame(&mut self, gpu_tick: u64, retire_tick: u64) {
        let mut index = 0;
        while index < self.sentenced.len() {
            let retire_tick = self.sentenced[index].retire_tick;
            if retire_tick != u64::MAX && retire_tick <= gpu_tick {
                let sentenced = self.sentenced.swap_remove(index);
                unsafe {
                    self.device.destroy_buffer(sentenced.buffer.buffer, None);
                    self.device.free_memory(sentenced.buffer.memory, None);
                }
            } else {
                index += 1;
            }
        }
        // Finalize buffers sentenced during this frame: their last possible
        // use is covered by the pending submission after this frame boundary.
        for sentenced in &mut self.sentenced {
            if sentenced.retire_tick == u64::MAX {
                sentenced.retire_tick = retire_tick;
            }
        }
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
        scheduler: &mut Scheduler,
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

            let copy_region = vk::BufferCopy {
                src_offset: staging.offset,
                dst_offset: 0,
                size,
            };
            let device = self.device.clone();
            scheduler.request_outside_renderpass();
            scheduler.record_with_upload(move |_cmdbuf, upload_cmdbuf| unsafe {
                device.cmd_copy_buffer(upload_cmdbuf, staging.buffer, buffer, &[copy_region]);
            });
        }

        trace!(
            "BufferCache: uploaded {} bytes from GPU VA 0x{:016X}",
            size,
            gpu_va
        );

        // Remove old entry if exists. The old buffer may still be referenced
        // by commands recorded this frame — defer its destruction.
        if let Some(old) = self.cache.remove(&gpu_va) {
            self.sentence(old);
        }

        self.cache.insert(
            gpu_va,
            CachedBuffer {
                buffer,
                memory,
                size,
                views: Vec::new(),
            },
        );

        (buffer, 0)
    }

    /// Upload a short-lived uniform buffer even when the same GPU VA was seen
    /// before. Guest constant buffers are commonly allocated from ring buffers
    /// and rewritten in place; caching solely by address would bind stale
    /// constants after the ring wraps.
    pub fn get_or_upload_fresh(
        &mut self,
        gpu_va: u64,
        size: vk::DeviceSize,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        staging_pool: &mut StagingBufferPool,
        scheduler: &mut Scheduler,
    ) -> (vk::Buffer, vk::DeviceSize) {
        if let Some(old) = self.cache.remove(&gpu_va) {
            self.sentence(old);
        }
        self.get_or_upload(gpu_va, size, read_gpu, staging_pool, scheduler)
    }

    /// Bind a short-lived uniform buffer directly from the mapped staging
    /// stream, matching upstream `BufferCacheRuntime::BindMappedUniformBuffer`.
    ///
    /// Uniform buffers rewritten by the guest every draw should not allocate a
    /// device-local VkBuffer and VkDeviceMemory on the hot path.
    pub fn bind_mapped_uniform_buffer(
        &mut self,
        gpu_va: u64,
        size: vk::DeviceSize,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        staging_pool: &mut StagingBufferPool,
    ) -> Option<(vk::Buffer, vk::DeviceSize)> {
        if size == 0 || gpu_va == 0 {
            return Some((self.null_buffer, 0));
        }
        let staging = staging_pool.request_upload_buffer(size)?;
        unsafe {
            let span = std::slice::from_raw_parts_mut(staging.mapped, size as usize);
            read_gpu(gpu_va, span);
        }
        Some((staging.buffer, staging.offset))
    }

    /// Invalidate a cached buffer range (mark as stale).
    ///
    /// The buffer may still be bound or targeted by copies in the pending
    /// command buffer (e.g. `write_memory` runs mid-frame from
    /// `accelerate_inline_to_memory`), so destruction is deferred until the
    /// scheduler tick retires.
    pub fn invalidate(&mut self, gpu_va: u64) {
        if let Some(old) = self.cache.remove(&gpu_va) {
            debug!(
                "BufferCache: invalidated buffer at GPU VA 0x{:016X}",
                gpu_va
            );
            self.sentence(old);
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
                ranges_overlap(base, cached_end, gpu_va, end).then_some(base)
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

fn ranges_overlap(lhs_begin: u64, lhs_end: u64, rhs_begin: u64, rhs_end: u64) -> bool {
    lhs_begin < rhs_end && rhs_begin < lhs_end
}

impl Drop for BufferCache {
    fn drop(&mut self) {
        unsafe {
            for (_, cached) in self.cache.drain() {
                self.device.destroy_buffer(cached.buffer, None);
                self.device.free_memory(cached.memory, None);
            }
            for sentenced in self.sentenced.drain(..) {
                self.device.destroy_buffer(sentenced.buffer.buffer, None);
                self.device.free_memory(sentenced.buffer.memory, None);
            }
            self.device.destroy_buffer(self.null_buffer, None);
            self.device.free_memory(self.null_memory, None);
        }
    }
}

fn vertex_binding_count(min_index: u32, max_index: u32, device_max: u32) -> u32 {
    let min_binding = min_index.min(device_max);
    let max_binding = max_index.min(device_max);
    max_binding.saturating_sub(min_binding)
}

/// Port of the null-handle branch in upstream
/// `BufferCacheRuntime::BindVertexBuffers`.
fn prepare_vertex_binding(
    buffer: vk::Buffer,
    offset: vk::DeviceSize,
    size: vk::DeviceSize,
    has_null_descriptor: bool,
    null_buffer: vk::Buffer,
    null_buffer_size: vk::DeviceSize,
) -> (vk::Buffer, vk::DeviceSize, vk::DeviceSize) {
    if buffer != vk::Buffer::null() {
        return (buffer, offset, size);
    }
    if has_null_descriptor {
        return (vk::Buffer::null(), 0, vk::WHOLE_SIZE);
    }
    (null_buffer, 0, null_buffer_size)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vertex_binding_count_is_capped_to_the_device_limit() {
        assert_eq!(vertex_binding_count(0, 32, 16), 16);
        assert_eq!(vertex_binding_count(12, 20, 16), 4);
        assert_eq!(vertex_binding_count(16, 32, 16), 0);
    }

    #[test]
    fn runtime_null_buffer_base_usage_matches_upstream() {
        assert_eq!(
            runtime_null_buffer_usage_flags(),
            vk::BufferUsageFlags::VERTEX_BUFFER
                | vk::BufferUsageFlags::INDEX_BUFFER
                | vk::BufferUsageFlags::TRANSFER_DST
                | vk::BufferUsageFlags::INDIRECT_BUFFER
        );
    }

    #[test]
    fn null_vertex_binding_preserves_upstream_null_descriptor_path() {
        let fallback = vk::Buffer::from_raw(0x1234);
        assert_eq!(
            prepare_vertex_binding(vk::Buffer::null(), 91, 73, true, fallback, 4),
            (vk::Buffer::null(), 0, vk::WHOLE_SIZE)
        );
    }

    #[test]
    fn null_vertex_binding_fallback_is_zero_buffer_bounded() {
        let fallback = vk::Buffer::from_raw(0x1234);
        assert_eq!(
            prepare_vertex_binding(vk::Buffer::null(), 91, 73, false, fallback, 4),
            (fallback, 0, 4)
        );
    }

    #[test]
    fn buffer_cache_params_match_upstream_vulkan() {
        assert!(!BufferCacheParams::IS_OPENGL);
        assert!(!BufferCacheParams::HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS);
        assert!(!BufferCacheParams::HAS_FULL_INDEX_AND_PRIMITIVE_SUPPORT);
        assert!(!BufferCacheParams::NEEDS_BIND_UNIFORM_INDEX);
        assert!(!BufferCacheParams::NEEDS_BIND_STORAGE_INDEX);
        assert!(BufferCacheParams::USE_MEMORY_MAPS);
        assert!(!BufferCacheParams::SEPARATE_IMAGE_BUFFER_BINDINGS);
        assert!(BufferCacheParams::USE_MEMORY_MAPS_FOR_UPLOADS);
    }

    #[test]
    fn common_buffers_support_texel_buffer_views() {
        let usage = common_buffer_usage_flags();
        assert!(usage.contains(vk::BufferUsageFlags::UNIFORM_TEXEL_BUFFER));
        assert!(usage.contains(vk::BufferUsageFlags::STORAGE_TEXEL_BUFFER));
    }

    #[test]
    fn quad_lut_matches_upstream_swizzles() {
        let quads = make_quad_lut(PrimitiveTopology::Quads, 4, vk::IndexType::UINT8_EXT);
        assert_eq!(&quads[..6], &[0, 1, 2, 0, 2, 3]);
        assert_eq!(&quads[6..12], &[1, 2, 3, 1, 3, 4]);

        let strip = make_quad_lut(PrimitiveTopology::QuadStrip, 4, vk::IndexType::UINT8_EXT);
        assert_eq!(&strip[..6], &[0, 3, 1, 0, 2, 3]);
        assert_eq!(&strip[6..12], &[1, 4, 2, 1, 3, 4]);
    }

    #[test]
    fn quad_lut_index_type_uses_upstream_boundaries() {
        assert_eq!(
            index_type_from_num_elements(0xff, true),
            vk::IndexType::UINT8_EXT
        );
        assert_eq!(
            index_type_from_num_elements(0x100, true),
            vk::IndexType::UINT16
        );
        assert_eq!(
            index_type_from_num_elements(0xff, false),
            vk::IndexType::UINT16
        );
        assert_eq!(
            index_type_from_num_elements(0xffff, true),
            vk::IndexType::UINT16
        );
        assert_eq!(
            index_type_from_num_elements(0x1_0000, true),
            vk::IndexType::UINT32
        );
    }

    #[test]
    fn direct_cache_write_overlap_uses_half_open_ranges() {
        assert!(ranges_overlap(0x1000, 0x2000, 0x1800, 0x2800));
        assert!(ranges_overlap(0x1000, 0x2000, 0x0800, 0x1800));
        assert!(!ranges_overlap(0x1000, 0x2000, 0x2000, 0x3000));
        assert!(!ranges_overlap(0x1000, 0x2000, 0x0000, 0x1000));
    }
}
