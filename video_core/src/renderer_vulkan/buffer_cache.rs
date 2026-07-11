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
use log::{debug, trace};

use super::scheduler::Scheduler;
use super::staging_buffer_pool::StagingBufferPool;
use super::update_descriptor::UpdateDescriptorQueue;
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
    buffers: HashMap<u32, CachedBuffer>,
    staging_refs: HashMap<usize, super::staging_buffer_pool::StagingBuffer>,
    next_handle: u32,
    null_buffer: vk::Buffer,
    null_memory: vk::DeviceMemory,
    null_buffer_size: vk::DeviceSize,
    extended_dynamic_state_supported: bool,
}

impl BufferCacheRuntime {
    pub fn new(
        device: ash::Device,
        instance: ash::Instance,
        physical_device: vk::PhysicalDevice,
        scheduler: &mut Scheduler,
        staging_pool: &mut StagingBufferPool,
        guest_descriptor_queue: &mut UpdateDescriptorQueue,
        extended_dynamic_state_supported: bool,
    ) -> Self {
        let (null_buffer, null_memory, null_buffer_size) =
            create_runtime_null_buffer(&device, &instance, physical_device);
        Self {
            device,
            instance,
            physical_device,
            scheduler: NonNull::from(scheduler),
            staging_pool: NonNull::from(staging_pool),
            guest_descriptor_queue: NonNull::from(guest_descriptor_queue),
            buffers: HashMap::new(),
            staging_refs: HashMap::new(),
            next_handle: 1,
            null_buffer,
            null_memory,
            null_buffer_size,
            extended_dynamic_state_supported,
        }
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
        let (buffer, offset, size) = if resolved == vk::Buffer::null() {
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
            vk::BufferUsageFlags::TRANSFER_SRC
                | vk::BufferUsageFlags::TRANSFER_DST
                | vk::BufferUsageFlags::UNIFORM_BUFFER
                | vk::BufferUsageFlags::STORAGE_BUFFER
                | vk::BufferUsageFlags::INDEX_BUFFER
                | vk::BufferUsageFlags::VERTEX_BUFFER,
        ) else {
            return;
        };
        let handle = self.allocate_handle();
        self.buffers.insert(handle, gpu_buffer);
        buffer.gpu_handle = handle;
    }

    fn tick_frame(&mut self) {}

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
        _base_vertex: u32,
        _num_indices: u32,
        _buffer: BufferId,
        gpu_handle: u32,
        offset: u32,
        _size: u32,
    ) {
        debug_assert!(!matches!(
            topology,
            PrimitiveTopology::Quads | PrimitiveTopology::QuadStrip
        ));
        let mut buffer = self.resolve_buffer(gpu_handle);
        if buffer == vk::Buffer::null() {
            buffer = self.null_buffer;
        }
        let index_type = match index_format {
            IndexFormat::UnsignedByte => vk::IndexType::UINT8_EXT,
            IndexFormat::UnsignedShort => vk::IndexType::UINT16,
            IndexFormat::UnsignedInt => vk::IndexType::UINT32,
        };
        let device = self.device.clone();
        self.scheduler().record(move |cmdbuf| unsafe {
            device.cmd_bind_index_buffer(cmdbuf, buffer, offset as u64, index_type);
        });
    }

    fn bind_vertex_buffers(
        &mut self,
        bindings: &HostBindings,
        buffers: &mut common::slot_vector::SlotVector<BufferBase>,
    ) {
        let binding_count = bindings.max_index.saturating_sub(bindings.min_index) as usize;
        if binding_count == 0 {
            return;
        }
        let mut vk_buffers = Vec::with_capacity(binding_count);
        let mut offsets = Vec::with_capacity(binding_count);
        let mut sizes = Vec::with_capacity(binding_count);
        let mut strides = Vec::with_capacity(binding_count);
        for index in 0..binding_count {
            let buffer_id = bindings.buffer_ids[index];
            let mut buffer = if buffer_id.is_valid() {
                self.resolve_buffer(buffers[buffer_id].gpu_handle)
            } else {
                vk::Buffer::null()
            };
            let is_null = buffer == vk::Buffer::null();
            if is_null {
                buffer = self.null_buffer;
            }
            vk_buffers.push(buffer);
            offsets.push(if is_null { 0 } else { bindings.offsets[index] });
            sizes.push(if is_null {
                vk::WHOLE_SIZE
            } else {
                bindings.sizes[index]
            });
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
        stage: usize,
        binding_index: u32,
        buffer: &mut BufferBase,
        offset: u32,
        size: u32,
        is_written: bool,
    ) {
        if std::env::var_os("RUZU_TRACE_SSBO_BIND").is_some() {
            log::info!(
                "[VK_SSBO_DESCRIPTOR] stage={} index={} gpu_handle={} vk_buffer=0x{:X} offset=0x{:X} size=0x{:X} written={}",
                stage,
                binding_index,
                buffer.gpu_handle,
                self.resolve_buffer(buffer.gpu_handle).as_raw(),
                offset,
                size,
                is_written,
            );
        }
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

    fn bind_transform_feedback_buffers(&mut self, _bindings: &HostBindings) {
        // Upstream `BindTransformFeedbackBuffers` returns early unless
        // VK_EXT_transform_feedback is supported (already logged in the
        // rasterizer). That extension is not available on the MoltenVK target,
        // so this is a no-op — matching upstream's behaviour there. Recording
        // vkCmdBindTransformFeedbackBuffersEXT would additionally require the
        // per-binding VkBuffer handles, which this signature does not carry
        // (HostBindings holds BufferIds, resolved by the caller); wire both when
        // a transform-feedback-capable device is targeted.
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
    let size = 256;
    let info = vk::BufferCreateInfo::builder()
        .size(size)
        .usage(
            vk::BufferUsageFlags::VERTEX_BUFFER
                | vk::BufferUsageFlags::INDEX_BUFFER
                | vk::BufferUsageFlags::UNIFORM_BUFFER
                | vk::BufferUsageFlags::STORAGE_BUFFER
                | vk::BufferUsageFlags::UNIFORM_TEXEL_BUFFER
                | vk::BufferUsageFlags::STORAGE_TEXEL_BUFFER
                | vk::BufferUsageFlags::TRANSFER_DST
                | vk::BufferUsageFlags::INDIRECT_BUFFER,
        )
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
    /// Scheduler tick observed at the last `tick_frame`.
    current_tick: u64,

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
            current_tick: 0,
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
    pub fn tick_frame(&mut self, gpu_tick: u64) {
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
        // use is covered by the next submission after this frame boundary.
        let finalize_tick = self.current_tick.saturating_add(1);
        for sentenced in &mut self.sentenced {
            if sentenced.retire_tick == u64::MAX {
                sentenced.retire_tick = finalize_tick;
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
        cmd: vk::CommandBuffer,
    ) -> (vk::Buffer, vk::DeviceSize) {
        if let Some(old) = self.cache.remove(&gpu_va) {
            self.sentence(old);
        }
        self.get_or_upload(gpu_va, size, read_gpu, staging_pool, cmd)
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
            self.sentence(old);
        }
        self.cache.insert(
            cache_key,
            CachedBuffer {
                buffer,
                memory,
                size,
                views: Vec::new(),
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
        stride: vk::DeviceSize,
        use_dynamic_stride: bool,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        staging_pool: &mut StagingBufferPool,
        upload_cmd: vk::CommandBuffer,
    ) {
        let (buffer, offset) = self.get_or_upload(gpu_va, size, read_gpu, staging_pool, upload_cmd);
        unsafe {
            if use_dynamic_stride {
                self.device.cmd_bind_vertex_buffers2(
                    cmd,
                    binding,
                    &[buffer],
                    &[offset],
                    Some(&[size]),
                    Some(&[stride]),
                );
            } else {
                self.device
                    .cmd_bind_vertex_buffers(cmd, binding, &[buffer], &[offset]);
            }
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

#[cfg(test)]
mod tests {
    use super::*;

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
    fn direct_cache_write_overlap_uses_half_open_ranges() {
        assert!(ranges_overlap(0x1000, 0x2000, 0x1800, 0x2800));
        assert!(ranges_overlap(0x1000, 0x2000, 0x0800, 0x1800));
        assert!(!ranges_overlap(0x1000, 0x2000, 0x2000, 0x3000));
        assert!(!ranges_overlap(0x1000, 0x2000, 0x0000, 0x1000));
    }
}
