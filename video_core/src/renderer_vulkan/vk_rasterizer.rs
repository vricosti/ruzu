// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Vulkan rasterizer port matching zuyu `vk_rasterizer.h/.cpp`.
//!
//! Ref: zuyu `vk_rasterizer.h/.cpp` — central orchestrator that coordinates
//! shader compilation, pipeline caching, buffer/texture management, command
//! batching, and GPU state tracking for efficient Vulkan rendering.
//!
//! # Components
//!
//! - [`Scheduler`] — command chunk batching + submission
//! - [`StateTracker`] — dirty flags for selective state updates
//! - [`FixedPipelineState`] — hashable pipeline key
//! - [`GraphicsPipelineCache`] — compiled VkPipeline caching
//! - [`RenderPassCache`] — format-keyed VkRenderPass caching
//! - [`StagingBufferPool`] — CPU↔GPU transfer buffer pooling
//! - [`DescriptorPool`] — banked descriptor set allocation
//! - [`UpdateDescriptorQueue`] — ring-buffered descriptor updates
//! - [`BufferCache`] — vertex/index/uniform buffer management
//! - [`TextureCache`] — image/view/sampler/framebuffer management

use crate::query_cache::types::{QueryPropertiesFlags, QueryType};

use std::ptr::NonNull;
use std::sync::{Arc, OnceLock};

use ash::vk;
use ash::vk::Handle;
use log::{debug, info, warn};
use thiserror::Error;

use crate::buffer_cache::buffer_cache_base::{
    DeviceMemoryAccess, GpuMemoryAccess, ObtainBufferOperation, ObtainBufferSynchronize,
};
use crate::control::channel_state_cache::{ChannelCacheAccessor, ChannelInfo, ChannelSetupCaches};
use crate::engines::kepler_compute::DispatchCall;
use crate::engines::maxwell_3d::{
    BlendEquation, BlendFactor, ComparisonOp, CullFace, DrawCall, FrontFace, PrimitiveTopology,
    NUM_VIEWPORTS,
};
use crate::engines::maxwell_dma::dma;
use crate::engines::Framebuffer;
use crate::fence_manager::FenceManager as GenericFenceManager;
use crate::framebuffer_config::FramebufferConfig;
use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
use crate::host1x::syncpoint_manager::SyncpointManager;
use crate::rasterizer_interface::{RasterizerDownloadArea, RasterizerInterface};
use crate::texture_cache::texture_cache_base::{DescriptorSyncRegs, ImageViewInOut};
use crate::texture_cache::types::{SamplerId, NULL_IMAGE_ID, NULL_IMAGE_VIEW_ID};
use crate::textures::texture::texture_pair;
use crate::vulkan_common::vulkan_memory_allocator::MemoryAllocator;
use shader_recompiler::host_translate_info::HostTranslateInfo;
use shader_recompiler::shader_info::{num_descriptors, Info as ShaderInfo};
use shader_recompiler::{PipelineCache as ShaderPipelineCache, Profile};

use super::{blit_image, blit_screen, maxwell_to_vk};

use super::pipeline_helper::{
    RescalingPushConstant, NUM_TEXTURE_AND_IMAGE_SCALING_WORDS, RENDERAREA_LAYOUT_OFFSET,
    RESCALING_LAYOUT_DOWN_FACTOR_OFFSET, RESCALING_LAYOUT_WORDS_OFFSET,
};

// Rust counterpart of upstream `std::scoped_lock{buffer_cache.mutex,
// texture_cache.mutex}`. `parking_lot::ReentrantMutex` does not provide a
// multi-lock scoped helper, so retry both orders to avoid ABBA deadlocks.
macro_rules! lock_two_reentrant_mutexes {
    ($first_mutex:expr, $second_mutex:expr, $first_guard:ident, $second_guard:ident) => {
        let $first_guard;
        let $second_guard;
        loop {
            let first_candidate = unsafe { (*$first_mutex).lock() };
            if let Some(second_candidate) = unsafe { (*$second_mutex).try_lock() } {
                $first_guard = first_candidate;
                $second_guard = second_candidate;
                break;
            }
            drop(first_candidate);
            std::thread::yield_now();

            let second_candidate = unsafe { (*$second_mutex).lock() };
            if let Some(first_candidate) = unsafe { (*$first_mutex).try_lock() } {
                $first_guard = first_candidate;
                $second_guard = second_candidate;
                break;
            }
            drop(second_candidate);
            std::thread::yield_now();
        }
    };
}

struct GpuTickGuard(Option<crate::renderer_base::GpuTickCallback>);

impl Drop for GpuTickGuard {
    fn drop(&mut self) {
        if let Some(callback) = self.0.as_ref() {
            callback();
        }
    }
}

fn bytes_of<T: Sized>(value: &T) -> &[u8] {
    unsafe {
        std::slice::from_raw_parts((value as *const T).cast::<u8>(), std::mem::size_of::<T>())
    }
}

#[derive(Clone, Copy, Debug)]
struct DrawParams {
    base_instance: u32,
    num_instances: u32,
    base_vertex: i32,
    num_vertices: u32,
    first_index: u32,
    is_indexed: bool,
}

struct PreparedGraphicsDescriptors {
    views: [ImageViewInOut; super::graphics_pipeline::MAX_IMAGE_ELEMENTS],
    samplers: [SamplerId; super::graphics_pipeline::MAX_IMAGE_ELEMENTS],
    view_count: usize,
    descriptor_data: Option<DescriptorData>,
    rescaling_data: [u32; NUM_TEXTURE_AND_IMAGE_SCALING_WORDS],
}

#[derive(Clone, Copy)]
struct DescriptorData(*const DescriptorUpdateEntry);

// The queue owns one fixed allocation for its whole lifetime. Its eight-frame
// ring and `Acquire` worker wait prevent a payload slice from being recycled
// while a recorded Vulkan command still consumes it, matching upstream's raw
// `const DescriptorUpdateEntry*` capture in `ConfigureDraw`.
unsafe impl Send for DescriptorData {}

impl Default for PreparedGraphicsDescriptors {
    fn default() -> Self {
        Self {
            views: [ImageViewInOut::default(); super::graphics_pipeline::MAX_IMAGE_ELEMENTS],
            samplers: [SamplerId::default(); super::graphics_pipeline::MAX_IMAGE_ELEMENTS],
            view_count: 0,
            descriptor_data: None,
            rescaling_data: [0; NUM_TEXTURE_AND_IMAGE_SCALING_WORDS],
        }
    }
}

fn vulkan_draw_trace_enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("RUZU_VK_TRACE_DRAWS").is_some())
}

fn parse_vulkan_sync_draw_interval(value: Option<&str>) -> Option<u32> {
    value?.parse::<u32>().ok().filter(|interval| *interval != 0)
}

fn vulkan_sync_draw_interval() -> Option<u32> {
    static INTERVAL: OnceLock<Option<u32>> = OnceLock::new();
    *INTERVAL.get_or_init(|| {
        std::env::var("RUZU_VK_SYNC_DRAW_INTERVAL")
            .ok()
            .as_deref()
            .and_then(|value| parse_vulkan_sync_draw_interval(Some(value)))
    })
}

fn format_vulkan_draw_trace(
    tick: u64,
    draw_counter: u32,
    draw: &DrawCall,
    params: DrawParams,
    unique_hashes: &[u64; crate::shader_cache::NUM_PROGRAMS],
) -> String {
    let shaders = unique_hashes
        .iter()
        .enumerate()
        .filter(|(_, hash)| **hash != 0)
        .map(|(stage, hash)| format!("{stage}:0x{hash:016X}"))
        .collect::<Vec<_>>()
        .join(",");
    let vertex_streams = draw
        .vertex_streams
        .iter()
        .filter(|stream| stream.enabled)
        .map(|stream| {
            format!(
                "{}:0x{:X}/{}x{}",
                stream.index, stream.address, stream.stride, stream.frequency
            )
        })
        .collect::<Vec<_>>()
        .join(",");
    let constant_buffers = draw
        .cb_bindings
        .iter()
        .enumerate()
        .flat_map(|(stage, bindings)| {
            bindings
                .iter()
                .enumerate()
                .filter(|(_, binding)| binding.enabled)
                .map(move |(index, binding)| {
                    format!(
                        "{stage}:{index}=0x{:X}+0x{:X}",
                        binding.address, binding.size
                    )
                })
        })
        .collect::<Vec<_>>()
        .join(",");
    format!(
        "[VK_DRAW_TRACE] tick={tick} draw={draw_counter} shaders=[{shaders}] topology={:?} indexed={} vertices={} instances={} first_index={} base_vertex={} base_instance={} index=0x{:X}..0x{:X}/{} streams=[{vertex_streams}] cbufs=[{constant_buffers}]",
        draw.topology,
        params.is_indexed,
        params.num_vertices,
        params.num_instances,
        params.first_index,
        params.base_vertex,
        params.base_instance,
        draw.index_buffer_addr,
        draw.index_buffer_addr_end,
        draw.index_buffer_count,
    )
}

fn format_descriptor_buffer_infos(buffer_infos: &[vk::DescriptorBufferInfo]) -> String {
    buffer_infos
        .iter()
        .map(|info| {
            format!(
                "0x{:X}@0x{:X}+0x{:X}",
                info.buffer.as_raw(),
                info.offset,
                info.range
            )
        })
        .collect::<Vec<_>>()
        .join(",")
}

fn format_descriptor_image_infos(image_infos: &[vk::DescriptorImageInfo]) -> String {
    image_infos
        .iter()
        .map(|info| {
            format!(
                "s=0x{:X}/v=0x{:X}/l={:?}",
                info.sampler.as_raw(),
                info.image_view.as_raw(),
                info.image_layout
            )
        })
        .collect::<Vec<_>>()
        .join(",")
}

fn make_draw_params(draw: &DrawCall) -> DrawParams {
    let mut params = DrawParams {
        base_instance: draw.base_instance,
        num_instances: draw.instance_count,
        base_vertex: if draw.indexed {
            draw.base_vertex
        } else {
            draw.vertex_first as i32
        },
        num_vertices: if draw.indexed {
            draw.index_buffer_count
        } else {
            draw.vertex_count
        },
        first_index: if draw.indexed {
            draw.index_buffer_first
        } else {
            0
        },
        is_indexed: draw.indexed,
    };

    match draw.topology {
        PrimitiveTopology::Quads => {
            params.num_vertices = (params.num_vertices / 4) * 6;
            params.base_vertex = 0;
            params.is_indexed = true;
        }
        PrimitiveTopology::QuadStrip => {
            params.num_vertices = params.num_vertices.wrapping_sub(2) / 2 * 6;
            params.base_vertex = 0;
            params.is_indexed = true;
        }
        _ => {}
    }

    params
}

/// Port of the color clear-value conversion in `RasterizerVulkan::Clear`.
fn make_color_clear_value(format: crate::surface::PixelFormat, color: [f32; 4]) -> vk::ClearValue {
    if !crate::surface::is_pixel_format_integer(format) {
        return vk::ClearValue {
            color: vk::ClearColorValue { float32: color },
        };
    }
    let int_size = crate::surface::pixel_component_size_bits_integer(format);
    if !crate::surface::is_pixel_format_signed_integer(format) {
        let scale = ((int_size as u64) << 1) as f32;
        return vk::ClearValue {
            color: vk::ClearColorValue {
                uint32: color.map(|component| (scale * component) as u32),
            },
        };
    }
    let scale = (((int_size - 1) as i64) << 1) as f32;
    vk::ClearValue {
        color: vk::ClearColorValue {
            int32: color.map(|component| (scale * (component - 0.5)) as i32),
        },
    }
}

use super::fence_manager::{Fence as VkFence, FenceManager as VkFenceBackend};

/// Port of `GetViewportState` from the anonymous namespace in
/// `vk_rasterizer.cpp`.
fn get_viewport_state(
    translate_x: f32,
    scale_x: f32,
    translate_y: f32,
    scale_y: f32,
    translate_z: f32,
    scale_z: f32,
    scale: f32,
    depth_minus_one_to_one: bool,
    lower_left: bool,
    y_negate: bool,
    surface_clip_height: f32,
    clamp_depth: bool,
) -> vk::Viewport {
    let conv = |value: f32| -> f32 {
        let new_value = value * scale;
        if scale < 1.0 {
            new_value.abs().round().copysign(value)
        } else {
            new_value
        }
    };

    let x = conv(translate_x - scale_x);
    let width = conv(scale_x * 2.0);
    let mut y = conv(translate_y - scale_y);
    let mut height = conv(scale_y * 2.0);
    if lower_left {
        y += conv(surface_clip_height);
        height = -height;
    }
    if y_negate {
        y += height;
        height = -height;
    }

    let reduce_z = if depth_minus_one_to_one { 1.0 } else { 0.0 };
    let mut min_depth = translate_z - scale_z * reduce_z;
    let mut max_depth = translate_z + scale_z;
    if clamp_depth {
        min_depth = min_depth.clamp(0.0, 1.0);
        max_depth = max_depth.clamp(0.0, 1.0);
    }
    vk::Viewport {
        x,
        y,
        width: if width != 0.0 { width } else { 1.0 },
        height: if height != 0.0 { height } else { 1.0 },
        min_depth,
        max_depth,
    }
}

fn viewport_state(
    draw: &DrawCall,
    index: usize,
    scale: f32,
    depth_range_unrestricted: bool,
    nv_viewport_swizzle: bool,
) -> vk::Viewport {
    let src = draw.viewport_transforms[index];
    get_viewport_state(
        src.translate_x,
        src.scale_x,
        src.translate_y,
        src.scale_y,
        src.translate_z,
        src.scale_z,
        scale,
        draw.depth_stencil.depth_mode == crate::engines::maxwell_3d::DepthMode::MinusOneToOne,
        draw.window_origin_lower_left,
        !nv_viewport_swizzle && ((src.swizzle >> 4) & 0x7) == 3,
        draw.surface_clip.height as f32,
        !depth_range_unrestricted,
    )
}

fn scissor_state(draw: &DrawCall, index: usize) -> vk::Rect2D {
    let src = draw.scissors[index];
    let clip_height = draw.surface_clip.height as i32;
    let mut min_y = if draw.window_origin_lower_left {
        clip_height - src.max_y as i32
    } else {
        src.min_y as i32
    };
    let mut max_y = if draw.window_origin_lower_left {
        clip_height - src.min_y as i32
    } else {
        src.max_y as i32
    };
    min_y = min_y.max(0);
    max_y = max_y.max(0);

    if src.enabled {
        vk::Rect2D {
            offset: vk::Offset2D {
                x: src.min_x as i32,
                y: min_y,
            },
            extent: vk::Extent2D {
                width: src.max_x.wrapping_sub(src.min_x),
                height: max_y.wrapping_sub(min_y) as u32,
            },
        }
    } else {
        vk::Rect2D {
            offset: vk::Offset2D { x: 0, y: 0 },
            extent: vk::Extent2D {
                width: i32::MAX as u32,
                height: i32::MAX as u32,
            },
        }
    }
}
use super::blit_image::BlitImageHelper;
use super::buffer_cache::{
    BufferCache as DirectBufferCache, BufferCacheRuntime, VulkanCommonBufferCache,
};
use super::descriptor_pool::DescriptorPool;
use super::graphics_pipeline::GraphicsDescriptorBinding;
use super::pipeline_cache::PipelineCache as VulkanPipelineCache;
use super::query_cache::QueryCache as VulkanQueryCache;
use super::render_pass_cache::RenderPassCache;
use super::scheduler::Scheduler;
use super::staging_buffer_pool::StagingBufferPool;
use super::state_tracker::StateTracker;
use super::texture_cache::TextureCache;
use super::update_descriptor::{DescriptorUpdateEntry, UpdateDescriptorQueue};

#[derive(Debug, Error)]
pub enum RendererError {
    #[error("Vulkan initialization failed: {0}")]
    InitFailed(String),
    #[error("No suitable GPU found")]
    NoSuitableDevice,
    #[error("Surface creation failed: {0}")]
    SurfaceFailed(String),
    #[error("Shader compilation failed: {0}")]
    ShaderCompilationFailed(String),
    #[error("Pipeline creation failed: {0}")]
    PipelineCreationFailed(String),
    #[error("Vulkan error: {0}")]
    VulkanError(vk::Result),
}

impl From<vk::Result> for RendererError {
    fn from(e: vk::Result) -> Self {
        RendererError::VulkanError(e)
    }
}

struct GpuMemoryAccessAdapter {
    mm: Arc<parking_lot::Mutex<crate::memory_manager::MemoryManager>>,
}

impl GpuMemoryAccess for GpuMemoryAccessAdapter {
    fn gpu_to_cpu_address(&self, gpu_addr: u64) -> Option<u64> {
        self.mm.lock().gpu_to_cpu_address(gpu_addr)
    }

    fn read_u64(&self, gpu_addr: u64) -> Option<u64> {
        let mut buf = [0u8; 8];
        self.mm.lock().read_block(gpu_addr, &mut buf);
        Some(u64::from_le_bytes(buf))
    }

    fn read_u32(&self, gpu_addr: u64) -> Option<u32> {
        let mut buf = [0u8; 4];
        self.mm.lock().read_block(gpu_addr, &mut buf);
        Some(u32::from_le_bytes(buf))
    }

    fn is_within_gpu_address_range(&self, gpu_addr: u64) -> bool {
        self.mm.lock().is_within_gpu_address_range(gpu_addr)
    }

    fn max_continuous_range(&self, gpu_addr: u64, size: u64) -> u64 {
        self.mm.lock().max_continuous_range(gpu_addr, size)
    }

    fn get_memory_layout_size(&self, gpu_addr: u64) -> u64 {
        self.mm.lock().get_memory_layout_size(gpu_addr)
    }
}

struct DeviceMemoryAccessAdapter {
    device_memory: Arc<MaxwellDeviceMemoryManager>,
}

fn is_geometry_dirty_flag(index: usize) -> bool {
    index == crate::dirty_flags::flags::INDEX_BUFFER as usize
        || index == crate::dirty_flags::flags::VERTEX_BUFFERS as usize
        || (crate::dirty_flags::flags::VERTEX_BUFFER0 as usize
            ..=crate::dirty_flags::flags::VERTEX_BUFFER31 as usize)
            .contains(&index)
}

impl DeviceMemoryAccess for DeviceMemoryAccessAdapter {
    fn get_pointer(&self, device_addr: u64) -> Option<*const u8> {
        let pointer = self.device_memory.get_pointer(device_addr);
        (!pointer.is_null()).then_some(pointer)
    }

    fn read_block_unsafe(&self, device_addr: u64, dst: &mut [u8]) {
        self.device_memory.smmu_read_block_unsafe(device_addr, dst);
    }

    fn write_block_unsafe(&self, device_addr: u64, src: &[u8]) {
        self.device_memory.smmu_write_block_unsafe(device_addr, src);
    }
}

/// Central Vulkan rendering orchestrator.
///
/// Ref: zuyu RasterizerVulkan — coordinates all rendering sub-components:
/// shader compilation, pipeline caching, buffer management, dynamic state
/// tracking, and command batching for efficient GPU rendering.
pub struct RasterizerVulkan {
    device: ash::Device,
    instance: ash::Instance,
    physical_device: vk::PhysicalDevice,
    syncpoints: Arc<SyncpointManager>,
    /// Shared owner counterpart of upstream
    /// `Tegra::MaxwellDeviceMemoryManager& device_memory`.
    device_memory: Arc<MaxwellDeviceMemoryManager>,
    channel_caches: ChannelSetupCaches<ChannelInfo>,

    // Sub-components (matching zuyu's architecture)
    /// Non-owning counterpart of upstream `Scheduler& scheduler`.
    ///
    /// `RendererVulkan` owns the single boxed scheduler and outlives this
    /// rasterizer. The stable pointer preserves upstream ownership without a
    /// self-referential Rust struct.
    scheduler: OwnerReference<Scheduler>,
    memory_allocator: NonNull<MemoryAllocator>,
    /// Non-owning counterpart of upstream `StateTracker& state_tracker`.
    state_tracker: OwnerReference<StateTracker>,
    staging_pool: Box<StagingBufferPool>,
    // Boxed like `scheduler`/`staging_pool`/`render_pass_cache`: sub-components
    // capture `NonNull` pointers to these during construction (BlitImageHelper
    // and TextureCache point at the descriptor pool and the descriptor queues,
    // TextureCache at the blit helper). A by-value field would move when the
    // constructor returns `Self`, leaving those pointers dangling on the old
    // stack frame — observed as an UpdateDescriptorQueue whose `acquire()`
    // clamped the real instance while `add_buffer` grew a stale cursor until
    descriptor_pool: Box<DescriptorPool>,
    desc_queue: Box<UpdateDescriptorQueue>,
    compute_pass_desc_queue: Box<UpdateDescriptorQueue>,
    blit_image: Box<BlitImageHelper>,
    fallback_uniform_buffer: vk::Buffer,
    fallback_uniform_memory: vk::DeviceMemory,
    fallback_uniform_mapped: *mut u8,
    fallback_sampler: vk::Sampler,
    render_pass_cache: Box<RenderPassCache>,
    shader_cache: crate::shader_cache::ShaderCache,
    pipeline_cache: VulkanPipelineCache,
    buffer_cache: DirectBufferCache,
    common_buffer_cache: VulkanCommonBufferCache,
    texture_cache: TextureCache,
    query_cache: VulkanQueryCache,
    fence_manager: GenericFenceManager<VkFence>,
    fence_backend: VkFenceBackend,
    wfi_event: vk::Event,

    // Default render pass for the offscreen framebuffer
    default_render_pass: vk::RenderPass,

    // Offscreen framebuffer resources
    offscreen_image: vk::Image,
    offscreen_memory: vk::DeviceMemory,
    offscreen_view: vk::ImageView,
    offscreen_fb: vk::Framebuffer,
    depth_image: vk::Image,
    depth_memory: vk::DeviceMemory,
    depth_view: vk::ImageView,
    fb_width: u32,
    fb_height: u32,

    // Readback buffer (GPU→CPU pixel transfer)
    readback_buffer: vk::Buffer,
    readback_memory: vk::DeviceMemory,
    readback_mapped: *mut u8,
    readback_size: u64,

    // Upstream FlushWork checks every eighth operation and flushes at 4096.
    draw_counter: u32,
    /// Monotonic draw sequence used only by env-gated diagnostics.
    draw_sequence: u64,
    /// Draws dropped because pipeline compilation failed (diagnostic).
    draw_skipped_pipeline: u64,
    /// Draws redirected to the offscreen framebuffer because no guest
    /// render-target framebuffer could be resolved (diagnostic).
    draw_offscreen_fallback: u64,
    has_null_descriptor: bool,
    extended_dynamic_state_supported: bool,
    extended_dynamic_state2_supported: bool,
    extended_dynamic_state2_extra_supported: bool,
    extended_dynamic_state3_blending_supported: bool,
    extended_dynamic_state3_enables_supported: bool,
    vertex_input_dynamic_state_supported: bool,
    must_emulate_scaled_formats: bool,
    depth_bounds_supported: bool,
    depth_range_unrestricted: bool,
    nv_viewport_swizzle: bool,
    extended_dynamic_state2: Option<ash::extensions::ext::ExtendedDynamicState2>,
    extended_dynamic_state3: Option<ash::extensions::ext::ExtendedDynamicState3>,
    vertex_input_dynamic_state: Option<vk::ExtVertexInputDynamicStateFn>,
    draw_indirect_count: Option<ash::extensions::khr::DrawIndirectCount>,
    push_descriptor: Option<ash::extensions::khr::PushDescriptor>,
    max_viewports: u32,
    max_vertex_input_attributes: u32,
    max_vertex_input_bindings: u32,

    // Channel-bound GPU memory manager, matching upstream rasterizer access to
    // the active channel's Tegra::MemoryManager.
    channel_memory_manager: Option<Arc<parking_lot::Mutex<crate::memory_manager::MemoryManager>>>,
    /// Rust owner bridge for upstream `Tegra::GPU& gpu` / `gpu.TickWork()`.
    gpu_tick_callback: Option<crate::renderer_base::GpuTickCallback>,
    /// Rust owner bridge for upstream `Tegra::GPU& gpu` /
    /// `gpu.InvalidateGPUCache()`.
    invalidate_gpu_cache_callback: Option<crate::renderer_base::InvalidateGpuCacheCallback>,
}

// Raw pointers are only used for mapped memory
unsafe impl Send for RasterizerVulkan {}

/// Stable, non-owning Rust representation of an upstream C++ reference member.
///
/// The owner boxes the referenced value and is declared after the borrower so
/// Rust drops the borrower first.
struct OwnerReference<T> {
    pointer: NonNull<T>,
}

impl<T> OwnerReference<T> {
    fn new(value: &mut T) -> Self {
        Self {
            pointer: NonNull::from(value),
        }
    }
}

impl<T> std::ops::Deref for OwnerReference<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.pointer.as_ref() }
    }
}

impl<T> std::ops::DerefMut for OwnerReference<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.pointer.as_mut() }
    }
}

#[cfg(test)]
mod owner_reference_tests {
    use super::OwnerReference;

    #[test]
    fn references_renderer_owned_stable_storage() {
        let mut owner = Box::new(0x1234_u64);
        let owner_address = std::ptr::from_ref(owner.as_ref());
        let mut reference = OwnerReference::new(owner.as_mut());

        assert_eq!(reference.pointer.as_ptr(), owner_address.cast_mut());
        *reference = 0x5678;
        assert_eq!(*owner, 0x5678);
    }
}

impl RasterizerVulkan {
    /// Low-bit mask used by upstream to check every eighth operation.
    const DISPATCH_THRESHOLD: u32 = 7;
    /// Hard flush threshold — full GPU submit every N draws.
    const FLUSH_THRESHOLD: u32 = 4096;

    /// Create a new RasterizerVulkan.
    ///
    /// Takes Vulkan handles from the VulkanPresenter so they share the same
    /// device and queue.
    pub fn new(
        shader_notify: crate::shader_notify::ShaderNotifyHandle,
        instance: ash::Instance,
        physical_device: vk::PhysicalDevice,
        device: ash::Device,
        driver_id: vk::DriverId,
        has_broken_parallel_shader_compiling: bool,
        cant_blit_msaa: bool,
        width: u32,
        height: u32,
        profile: Profile,
        host_info: HostTranslateInfo,
        depth_bounds_supported: bool,
        depth_range_unrestricted: bool,
        nv_viewport_swizzle: bool,
        index_type_uint8_supported: bool,
        has_null_descriptor: bool,
        extended_dynamic_state_supported: bool,
        transform_feedback_supported: bool,
        host_query_reset_supported: bool,
        extended_dynamic_state2_supported: bool,
        extended_dynamic_state2_extra_supported: bool,
        extended_dynamic_state3_blending_supported: bool,
        extended_dynamic_state3_enables_supported: bool,
        vertex_input_dynamic_state_supported: bool,
        topology_list_primitive_restart_supported: bool,
        patch_list_primitive_restart_supported: bool,
        must_emulate_scaled_formats: bool,
        must_emulate_bgr565: bool,
        ext_4444_formats_supported: bool,
        shader_stencil_export_supported: bool,
        image_format_list_supported: bool,
        optimal_astc_supported: bool,
        custom_border_color_supported: bool,
        sampler_filter_minmax_supported: bool,
        max_viewports: u32,
        max_vertex_input_attributes: u32,
        max_vertex_input_bindings: u32,
        vertex_attribute_divisor_supported: bool,
        provoking_vertex_supported: bool,
        draw_indirect_count_supported: bool,
        push_descriptor_supported: bool,
        max_push_descriptors: u32,
        syncpoints: Arc<SyncpointManager>,
        device_memory: Arc<MaxwellDeviceMemoryManager>,
        memory_allocator: &mut MemoryAllocator,
        state_tracker: &mut StateTracker,
        scheduler: &mut Scheduler,
    ) -> Result<Self, RendererError> {
        info!(
            "RasterizerVulkan: initializing {}x{} renderer",
            width, height
        );

        // Create staging buffer pool
        let mut staging_pool = Box::new(StagingBufferPool::new(
            device.clone(),
            instance.clone(),
            physical_device,
            scheduler,
        ));

        // Create descriptor pool. Boxed (with the descriptor queues and the
        // blit helper below) so the `NonNull` pointers captured by
        // sub-components stay valid when the constructed `Self` is moved.
        let mut descriptor_pool = Box::new(DescriptorPool::new(device.clone(), 64));

        // Create descriptor update queue
        let mut desc_queue = Box::new(UpdateDescriptorQueue::new(scheduler));
        let mut compute_pass_desc_queue = Box::new(UpdateDescriptorQueue::new(scheduler));
        let mut blit_image = Box::new(BlitImageHelper::new(
            device.clone(),
            scheduler,
            descriptor_pool.as_mut(),
            shader_stencil_export_supported,
        ));

        let (fallback_uniform_buffer, fallback_uniform_memory, fallback_uniform_mapped) =
            create_host_buffer(
                &instance,
                physical_device,
                &device,
                0x10000,
                vk::BufferUsageFlags::UNIFORM_BUFFER,
            )?;
        unsafe {
            // Upstream's physical null-buffer fallback is deterministically
            // zero-filled. Do the same for this legacy rasterizer fallback.
            std::ptr::write_bytes(fallback_uniform_mapped, 0, 0x10000);
        }
        let fallback_sampler = create_fallback_sampler(&device)?;

        // Create render pass cache
        let mut render_pass_cache = Box::new(RenderPassCache::new(
            device.clone(),
            instance.clone(),
            physical_device,
        ));

        // Create shader recompiler pipeline cache
        let shader_cache = ShaderPipelineCache::new(profile.clone());
        let must_emulate_scaled_formats =
            must_emulate_scaled_formats || !profile.support_scaled_attributes;

        // Create pipeline cache owner
        let use_asynchronous_shaders = *common::settings::values()
            .use_asynchronous_shaders
            .get_value();
        let use_vulkan_pipeline_cache = *common::settings::values()
            .use_vulkan_driver_pipeline_cache
            .get_value();
        let pipeline_cache = VulkanPipelineCache::new(
            device.clone(),
            descriptor_pool.as_mut(),
            shader_notify,
            use_asynchronous_shaders,
            use_vulkan_pipeline_cache,
            has_broken_parallel_shader_compiling,
            shader_cache,
            profile,
            host_info,
            render_pass_cache.as_mut(),
            extended_dynamic_state_supported,
            extended_dynamic_state2_supported,
            extended_dynamic_state2_extra_supported,
            extended_dynamic_state3_blending_supported,
            extended_dynamic_state3_enables_supported,
            vertex_input_dynamic_state_supported,
            must_emulate_scaled_formats,
            topology_list_primitive_restart_supported,
            patch_list_primitive_restart_supported,
            max_viewports,
            max_vertex_input_bindings,
            vertex_attribute_divisor_supported,
            provoking_vertex_supported,
            push_descriptor_supported,
            max_push_descriptors,
        );

        // Create buffer cache
        let buffer_cache =
            DirectBufferCache::new(device.clone(), instance.clone(), physical_device)
                .map_err(|e| RendererError::InitFailed(format!("buffer cache: {:?}", e)))?;
        // Upstream BufferCacheParams uses
        // MemoryTrackerBase<Tegra::MaxwellDeviceMemoryManager>. Keep the
        // tracker connected to the shared device-memory manager so tracked
        // pages are protected and later CPU writes reach OnCPUWrite.
        let mut common_buffer_cache = VulkanCommonBufferCache::new(device_memory.as_ref());
        let buffer_runtime = BufferCacheRuntime::new(
            device.clone(),
            instance.clone(),
            physical_device,
            scheduler,
            staging_pool.as_mut(),
            desc_queue.as_mut(),
            compute_pass_desc_queue.as_mut(),
            descriptor_pool.as_ref(),
            driver_id,
            index_type_uint8_supported,
            has_null_descriptor,
            extended_dynamic_state_supported,
            transform_feedback_supported,
            max_vertex_input_bindings,
        )
        .map_err(|e| RendererError::InitFailed(format!("buffer cache runtime: {:?}", e)))?;
        common_buffer_cache.set_runtime(Box::new(buffer_runtime));
        common_buffer_cache.set_device_memory(Box::new(DeviceMemoryAccessAdapter {
            device_memory: Arc::clone(&device_memory),
        }));

        // Create texture cache
        let shader_cache = crate::shader_cache::ShaderCache::new(Arc::clone(&device_memory));

        let texture_cache = TextureCache::new(
            device.clone(),
            instance.clone(),
            physical_device,
            Arc::clone(&device_memory),
            scheduler,
            &mut *memory_allocator,
            staging_pool.as_mut(),
            blit_image.as_mut(),
            render_pass_cache.as_mut(),
            descriptor_pool.as_mut(),
            compute_pass_desc_queue.as_mut(),
            cant_blit_msaa,
            image_format_list_supported,
            optimal_astc_supported,
            must_emulate_bgr565,
            ext_4444_formats_supported,
            custom_border_color_supported,
            sampler_filter_minmax_supported,
            has_null_descriptor,
        )
        .map_err(|e| RendererError::InitFailed(format!("texture cache: {:?}", e)))?;

        // Create query cache
        let query_cache = VulkanQueryCache::new(
            &instance,
            device.clone(),
            scheduler,
            memory_allocator,
            transform_feedback_supported,
            host_query_reset_supported,
        )
        .map_err(|e| RendererError::InitFailed(format!("query cache: {e:?}")))?;

        let wfi_event_info = vk::EventCreateInfo::default();
        let wfi_event = unsafe {
            device
                .create_event(&wfi_event_info, None)
                .map_err(|e| RendererError::InitFailed(format!("wfi event: {:?}", e)))?
        };

        // Create default render pass
        let default_render_pass = create_default_render_pass(&device)?;

        // Create offscreen framebuffer resources
        let (offscreen_image, offscreen_memory, offscreen_view) =
            create_color_attachment(&instance, physical_device, &device, width, height)?;
        let (depth_image, depth_memory, depth_view) =
            create_depth_attachment(&instance, physical_device, &device, width, height)?;

        let offscreen_fb = create_framebuffer(
            &device,
            default_render_pass,
            offscreen_view,
            depth_view,
            width,
            height,
        )?;

        // Create readback buffer
        let readback_size = (width * height * 4) as u64;
        let (readback_buffer, readback_memory, readback_mapped) = create_host_buffer(
            &instance,
            physical_device,
            &device,
            readback_size,
            vk::BufferUsageFlags::TRANSFER_DST,
        )?;
        let draw_indirect_count = draw_indirect_count_supported
            .then(|| ash::extensions::khr::DrawIndirectCount::new(&instance, &device));
        let push_descriptor = push_descriptor_supported
            .then(|| ash::extensions::khr::PushDescriptor::new(&instance, &device));
        let extended_dynamic_state2 = extended_dynamic_state2_extra_supported
            .then(|| ash::extensions::ext::ExtendedDynamicState2::new(&instance, &device));
        let extended_dynamic_state3 = (extended_dynamic_state3_blending_supported
            || extended_dynamic_state3_enables_supported)
            .then(|| ash::extensions::ext::ExtendedDynamicState3::new(&instance, &device));
        let vertex_input_dynamic_state = vertex_input_dynamic_state_supported.then(|| {
            vk::ExtVertexInputDynamicStateFn::load(|name| unsafe {
                std::mem::transmute(instance.get_device_proc_addr(device.handle(), name.as_ptr()))
            })
        });

        let fence_wait_handle = scheduler.wait_handle();
        Ok(Self {
            device,
            instance,
            physical_device,
            syncpoints,
            device_memory,
            channel_caches: ChannelSetupCaches::new(),
            scheduler: OwnerReference::new(scheduler),
            memory_allocator: NonNull::from(&mut *memory_allocator),
            state_tracker: OwnerReference::new(state_tracker),
            staging_pool,
            descriptor_pool,
            desc_queue,
            compute_pass_desc_queue,
            blit_image,
            fallback_uniform_buffer,
            fallback_uniform_memory,
            fallback_uniform_mapped,
            fallback_sampler,
            render_pass_cache,
            shader_cache,
            pipeline_cache,
            buffer_cache,
            common_buffer_cache,
            texture_cache,
            query_cache,
            fence_manager: GenericFenceManager::new(true),
            fence_backend: VkFenceBackend::new(fence_wait_handle),
            wfi_event,
            default_render_pass,
            offscreen_image,
            offscreen_memory,
            offscreen_view,
            offscreen_fb,
            depth_image,
            depth_memory,
            depth_view,
            fb_width: width,
            fb_height: height,
            readback_buffer,
            readback_memory,
            readback_mapped,
            readback_size,
            draw_counter: 0,
            draw_sequence: 0,
            draw_skipped_pipeline: 0,
            draw_offscreen_fallback: 0,
            has_null_descriptor,
            extended_dynamic_state_supported,
            extended_dynamic_state2_supported,
            extended_dynamic_state2_extra_supported,
            extended_dynamic_state3_blending_supported,
            extended_dynamic_state3_enables_supported,
            vertex_input_dynamic_state_supported,
            must_emulate_scaled_formats,
            depth_bounds_supported,
            depth_range_unrestricted,
            nv_viewport_swizzle,
            extended_dynamic_state2,
            extended_dynamic_state3,
            vertex_input_dynamic_state,
            draw_indirect_count,
            push_descriptor,
            max_viewports: max_viewports.min(NUM_VIEWPORTS as u32).max(1),
            max_vertex_input_attributes,
            max_vertex_input_bindings,
            channel_memory_manager: None,
            gpu_tick_callback: None,
            invalidate_gpu_cache_callback: None,
        })
    }

    /// Wire the GPU tick source into the Vulkan query-cache owner.
    ///
    /// Port of the Vulkan rasterizer-side query-cache wiring edge. The active
    /// runtime Vulkan owner still lacks the full upstream `RendererBase`
    /// plumbing, but the query-cache ownership belongs here rather than in a
    /// local query shortcut.
    pub fn set_gpu_ticks_getter(&mut self, getter: crate::renderer_base::GpuTicksGetter) {
        self.query_cache.set_gpu_ticks_getter(getter);
    }

    pub fn set_guest_memory_writer(&mut self, writer: crate::renderer_base::GuestMemoryWriter) {
        self.texture_cache.set_guest_memory_writer(writer);
    }

    pub fn set_gpu_tick_callback(&mut self, callback: crate::renderer_base::GpuTickCallback) {
        self.gpu_tick_callback = Some(callback);
    }

    pub fn set_invalidate_gpu_cache_callback(
        &mut self,
        callback: crate::renderer_base::InvalidateGpuCacheCallback,
    ) {
        self.invalidate_gpu_cache_callback = Some(callback);
    }

    /// Main draw entry point — process a single draw call.
    ///
    /// Ref: zuyu RasterizerVulkan::Draw() — compiles/caches pipeline,
    /// updates dynamic state via dirty flags, binds resources, records draw.
    fn draw_prepared(
        &mut self,
        draw: &DrawCall,
        zpass_pixel_count_enabled: bool,
        indirect_params: Option<crate::engines::draw_manager::IndirectParams>,
        dirty_flags: &mut [bool; 256],
        engine_dirty_flags: Option<std::ptr::NonNull<[bool; 256]>>,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
    ) {
        // 1. Periodic flush
        self.flush_work();

        // 2. Compile or lookup cached pipeline
        let known_gpu_tick = self.scheduler.known_gpu_tick();
        let pending_tick = self.scheduler.pending_tick();
        let pipeline_result = self
            .pipeline_cache
            .current_graphics_pipeline_with_shared_cache(draw, &mut self.shader_cache);
        let (
            pipeline_waiter,
            pipeline_layout,
            descriptor_set_layout,
            descriptor_update_template,
            uses_push_descriptor,
            descriptor_set,
            descriptor_bindings,
            stage_infos,
            enabled_uniform_buffer_masks,
            uniform_buffer_sizes,
            uses_render_area,
            _uses_rescaling_uniform,
            unique_hashes,
        ) = match pipeline_result {
            Some((gp, _fixed_state)) => {
                let descriptor_set = if gp.uses_push_descriptor {
                    None
                } else {
                    match gp.commit_descriptor_set(known_gpu_tick, pending_tick) {
                        Ok(set) => set,
                        Err(error) => {
                            warn!(
                                "RasterizerVulkan: failed to commit graphics descriptor set: {error:?}"
                            );
                            return;
                        }
                    }
                };
                (
                    gp.build_waiter(),
                    gp.pipeline_layout,
                    gp.descriptor_set_layout,
                    gp.descriptor_update_template,
                    gp.uses_push_descriptor,
                    descriptor_set,
                    Arc::clone(&gp.descriptor_bindings),
                    Arc::clone(&gp.stage_infos),
                    gp.enabled_uniform_buffer_masks,
                    gp.uniform_buffer_sizes,
                    gp.uses_render_area,
                    gp.uses_rescaling_uniform,
                    gp.key().unique_hashes,
                )
            }
            None => {
                self.draw_skipped_pipeline = self.draw_skipped_pipeline.wrapping_add(1);
                // A skipped draw leaves the previous frame's pixels in place;
                // with LOAD attachments this accumulates visibly (e.g. the
                if self.draw_skipped_pipeline <= 16 || self.draw_skipped_pipeline.is_power_of_two()
                {
                    log::warn!(
                        "[DRAW_SKIP] #{} pipeline compilation failed (draw={} rt0=0x{:X} fmt={} topology={:?} indexed={})",
                        self.draw_skipped_pipeline,
                        self.draw_counter,
                        draw.render_targets[0].address,
                        draw.render_targets[0].format,
                        draw.topology,
                        draw.indexed,
                    );
                }
                return;
            }
        };
        let trace_draw = vulkan_draw_trace_enabled();
        let clear_values = [
            vk::ClearValue {
                color: vk::ClearColorValue {
                    float32: [0.0, 0.0, 0.0, 1.0],
                },
            },
            vk::ClearValue {
                depth_stencil: vk::ClearDepthStencilValue {
                    depth: 1.0,
                    stencil: 0,
                },
            },
        ];
        let draw_params = make_draw_params(draw);
        // Serialize every common-buffer-cache access on this draw
        // (uniform/storage descriptor binding AND the geometry binding below)
        // against concurrent CPU-write invalidation. A guest write on another
        // core reaches `BufferCache::on_cpu_write` -> `delete_buffer` ->
        // `slot_buffers.take()`, which frees the very slots this path reads via
        // `slot_buffers[buffer_id]` (unguarded in release, where SlotVector's
        // validate_index is a debug_assert). Without this lock the GPU thread
        // can index a slot the CPU thread just freed -> SlotVector panic /
        // use-after-free. The mutexes are reentrant, so the texture lock taken
        // inside `bind_graphics_descriptors` is fine. Matches the locking the
        // async-flush paths already use for these two caches.
        let bc_draw_texture_mutex: *const _ = &self.texture_cache.base.mutex;
        let bc_draw_buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
        lock_two_reentrant_mutexes!(
            bc_draw_buffer_mutex,
            bc_draw_texture_mutex,
            _bc_draw_buffer_guard,
            _bc_draw_texture_guard
        );
        // Upstream `GraphicsPipeline::ConfigureImpl` updates all buffer
        // bindings once, then binds geometry before the per-stage buffers.
        let prepared_descriptors = self.bind_graphics_descriptors(
            descriptor_set_layout,
            descriptor_bindings.as_slice(),
            stage_infos.as_ref(),
            &enabled_uniform_buffer_masks,
            &uniform_buffer_sizes,
            draw,
            draw.indexed,
            read_gpu,
            read_gpu_unsafe,
        );
        let Some(prepared_descriptors) = prepared_descriptors else {
            warn!("RasterizerVulkan: draw skipped because descriptor preparation failed");
            return;
        };
        if !uses_push_descriptor
            && required_descriptor_set_missing(
                descriptor_set_layout,
                descriptor_bindings.len(),
                descriptor_set,
            )
        {
            warn!("RasterizerVulkan: draw skipped because required descriptors are incomplete");
            return;
        }

        // Upstream GraphicsPipeline::ConfigureImpl resolves image views before
        // UpdateRenderTargets. Resolving a view may join/delete cached images
        // and dirty the render-target bindings, so taking the framebuffer
        // snapshot any earlier can leave this draw targeting a stale image.
        let target_fb = self
            .texture_cache
            .update_render_targets_and_get_rt0_framebuffer(
                &crate::engines::draw_manager::Maxwell3DRenderTargets {
                    rt_control: draw.rt_control,
                    render_targets: draw.render_targets,
                    zeta: draw.zeta,
                    anti_alias_samples_mode: 0,
                    surface_clip: draw.surface_clip,
                },
                dirty_flags,
                read_gpu_unsafe,
                false,
                None,
            );
        if self
            .texture_cache
            .base
            .check_feedback_loop(&prepared_descriptors.views)
        {
            self.texture_cache.barrier_feedback_loop();
        }
        let target_has_depth = target_fb.as_ref().is_some_and(|target| target.has_depth);
        let target_num_color = target_fb
            .as_ref()
            .map(|target| target.num_color)
            .unwrap_or(1);
        let render_pass = target_fb
            .as_ref()
            .map(|target| target.render_pass)
            .unwrap_or(self.default_render_pass);
        let (framebuffer, extent, rp_images, rp_image_ranges) = if let Some(target) = target_fb {
            self.texture_cache
                .prepare_render_targets_for_render(&target.image_ids);
            (
                target.framebuffer,
                target.extent,
                target.images,
                target.image_ranges,
            )
        } else {
            self.draw_offscreen_fallback = self.draw_offscreen_fallback.wrapping_add(1);
            if self.draw_offscreen_fallback <= 16 || self.draw_offscreen_fallback.is_power_of_two()
            {
                log::warn!(
                    "[DRAW_OFFSCREEN] #{} no guest framebuffer resolved (draw={} rt0=0x{:X} fmt={})",
                    self.draw_offscreen_fallback,
                    self.draw_counter,
                    draw.render_targets[0].address,
                    draw.render_targets[0].format,
                );
            }
            (
                self.offscreen_fb,
                vk::Extent2D {
                    width: self.fb_width,
                    height: self.fb_height,
                },
                Vec::new(),
                Vec::new(),
            )
        };
        let render_area = vk::Rect2D {
            offset: vk::Offset2D { x: 0, y: 0 },
            extent,
        };
        let indirect_binding = indirect_params.map(|params| {
            let (buffer_id, offset) = self.common_buffer_cache.get_draw_indirect_buffer();
            let buffer = vk::Buffer::from_raw(
                self.common_buffer_cache
                    .resolve_backend_buffer_raw(buffer_id),
            );
            let count = params.include_count.then(|| {
                let (count_buffer_id, count_offset) =
                    self.common_buffer_cache.get_draw_indirect_count();
                (
                    vk::Buffer::from_raw(
                        self.common_buffer_cache
                            .resolve_backend_buffer_raw(count_buffer_id),
                    ),
                    count_offset,
                )
            });
            (params, buffer, offset, count)
        });
        // The guards (`bc_draw_buffer_guard`/`bc_draw_texture_guard`) are held
        // through the rest of this function, i.e. across texture
        // materialization and the draw emission below, matching upstream
        // `RasterizerVulkan::PrepareDraw` which keeps
        // `scoped_lock{buffer_cache.mutex, texture_cache.mutex}` around
        // Configure AND draw_func (vk_rasterizer.cpp:223-233). Releasing after
        // binding would still let a concurrent CPU-write free a slot the draw
        // depends on before it is recorded. RAII drops them on every exit,
        // including the early-return trace-stub path.

        // Upstream `GraphicsPipeline::ConfigureImpl` resolves and snapshots
        // every guest resource before `ConfigureDraw` records the wait for an
        // asynchronously-built pipeline. Waiting before descriptor/geometry
        // preparation lets the guest recycle a uniform-buffer ring while the
        // occurrence 4 and its short-lived transition effects disappear.
        let Some(pipeline) = pipeline_waiter.pipeline_handle() else {
            self.draw_skipped_pipeline = self.draw_skipped_pipeline.wrapping_add(1);
            if self.draw_skipped_pipeline <= 16 || self.draw_skipped_pipeline.is_power_of_two() {
                log::warn!(
                    "[DRAW_SKIP] #{} pipeline build failed asynchronously after resource preparation (draw={} rt0=0x{:X} fmt={} topology={:?} indexed={})",
                    self.draw_skipped_pipeline,
                    self.draw_counter,
                    draw.render_targets[0].address,
                    draw.render_targets[0].format,
                    draw.topology,
                    draw.indexed,
                );
            }
            return;
        };
        // Build clear values indexed by attachment: one per colour attachment
        // (ignored by the LOAD colour attachments, but the array must be long
        // enough to index the CLEAR depth attachment at index `num_color`),
        // then the depth clear value when a depth attachment is bound.
        let render_pass_clears: Vec<vk::ClearValue> = if render_pass == self.default_render_pass {
            clear_values.to_vec()
        } else {
            let mut clears = vec![clear_values[0]; target_num_color.max(1) as usize];
            if target_has_depth {
                clears.push(clear_values[1]);
            }
            clears
        };
        self.scheduler.request_renderpass(
            framebuffer,
            render_pass,
            render_area,
            &render_pass_clears,
            &rp_images,
            &rp_image_ranges,
        );
        self.push_graphics_push_constants(
            pipeline,
            pipeline_layout,
            descriptor_set_layout,
            descriptor_update_template,
            uses_push_descriptor,
            descriptor_set,
            prepared_descriptors.descriptor_data,
            prepared_descriptors.rescaling_data,
            draw,
            uses_render_area,
        );

        // `bind_graphics_descriptors` has just run the common buffer cache's
        // `UpdateGraphicsBuffers`, which consumes these live Maxwell dirty
        // flags before Eden calls `UpdateDynamicStates`.
        dirty_flags[crate::dirty_flags::flags::INDEX_BUFFER as usize] = false;
        dirty_flags[crate::dirty_flags::flags::VERTEX_BUFFERS as usize] = false;
        for index in
            crate::dirty_flags::flags::VERTEX_BUFFER0..=crate::dirty_flags::flags::VERTEX_BUFFER31
        {
            dirty_flags[index as usize] = false;
        }

        // 6. Update dynamic states via dirty flags. Upstream requests the
        // render pass in `GraphicsPipeline::ConfigureDraw` before
        // `RasterizerVulkan::UpdateDynamicStates`.
        self.update_dynamic_states(draw, dirty_flags, engine_dirty_flags);

        self.query_cache.notify_segment(true);
        self.query_cache
            .handle_transform_feedback(&mut self.scheduler);
        self.query_cache.counter_enable(
            &mut self.scheduler,
            QueryType::ZPassPixelCount64 as u32,
            zpass_pixel_count_enabled,
        );

        // 7. Issue draw call
        if trace_draw {
            info!(
                "{}",
                format_vulkan_draw_trace(
                    self.scheduler.pending_tick(),
                    self.draw_counter,
                    draw,
                    draw_params,
                    &unique_hashes,
                )
            );
        }
        if let Some((params, buffer, offset, count)) = indirect_binding {
            if buffer == vk::Buffer::null() {
                warn!("RasterizerVulkan::draw_indirect skipped: missing indirect buffer");
                return;
            }
            if params.is_byte_count {
                let Some(transform_feedback) = self.query_cache.transform_feedback_dispatch()
                else {
                    warn!("RasterizerVulkan::draw_indirect byte-count path requires VK_EXT_transform_feedback");
                    return;
                };
                self.scheduler.record(move |cmdbuf| unsafe {
                    (transform_feedback.cmd_draw_indirect_byte_count_ext)(
                        cmdbuf,
                        1,
                        0,
                        buffer,
                        offset as vk::DeviceSize,
                        0,
                        params.stride as u32,
                    );
                });
                return;
            }
            if let Some((count_buffer, count_offset)) = count {
                if count_buffer == vk::Buffer::null() {
                    warn!("RasterizerVulkan::draw_indirect skipped: missing count buffer");
                    return;
                }
                let Some(draw_indirect_count) = self.draw_indirect_count.clone() else {
                    warn!("RasterizerVulkan::draw_indirect skipped: VK_KHR_draw_indirect_count is unavailable");
                    return;
                };
                self.scheduler.record(move |cmdbuf| unsafe {
                    if params.is_indexed {
                        draw_indirect_count.cmd_draw_indexed_indirect_count(
                            cmdbuf,
                            buffer,
                            offset as vk::DeviceSize,
                            count_buffer,
                            count_offset as vk::DeviceSize,
                            params.max_draw_counts as u32,
                            params.stride as u32,
                        );
                    } else {
                        draw_indirect_count.cmd_draw_indirect_count(
                            cmdbuf,
                            buffer,
                            offset as vk::DeviceSize,
                            count_buffer,
                            count_offset as vk::DeviceSize,
                            params.max_draw_counts as u32,
                            params.stride as u32,
                        );
                    }
                });
            } else {
                let device = self.device.clone();
                self.scheduler.record(move |cmdbuf| unsafe {
                    if params.is_indexed {
                        device.cmd_draw_indexed_indirect(
                            cmdbuf,
                            buffer,
                            offset as vk::DeviceSize,
                            params.max_draw_counts as u32,
                            params.stride as u32,
                        );
                    } else {
                        device.cmd_draw_indirect(
                            cmdbuf,
                            buffer,
                            offset as vk::DeviceSize,
                            params.max_draw_counts as u32,
                            params.stride as u32,
                        );
                    }
                });
            }
        } else if draw_params.is_indexed {
            let device = self.device.clone();
            self.scheduler.record(move |cmdbuf| unsafe {
                device.cmd_draw_indexed(
                    cmdbuf,
                    draw_params.num_vertices,
                    draw_params.num_instances,
                    draw_params.first_index,
                    draw_params.base_vertex,
                    draw_params.base_instance,
                );
            });
        } else {
            let device = self.device.clone();
            self.scheduler.record(move |cmdbuf| unsafe {
                device.cmd_draw(
                    cmdbuf,
                    draw_params.num_vertices,
                    draw_params.num_instances,
                    draw_params.base_vertex as u32,
                    draw_params.base_instance,
                );
            });
        }
        if let Some(interval) = vulkan_sync_draw_interval() {
            if self.draw_counter % interval == 0 {
                info!(
                    "[VK_DRAW_SYNC] tick={} draw={} interval={interval}",
                    self.scheduler.pending_tick(),
                    self.draw_counter,
                );
                self.finish();
            }
        }
    }

    fn push_graphics_push_constants(
        &mut self,
        pipeline: vk::Pipeline,
        pipeline_layout: vk::PipelineLayout,
        descriptor_set_layout: vk::DescriptorSetLayout,
        descriptor_update_template: vk::DescriptorUpdateTemplate,
        uses_push_descriptor: bool,
        descriptor_set: Option<vk::DescriptorSet>,
        descriptor_data: Option<DescriptorData>,
        rescaling_data: [u32; NUM_TEXTURE_AND_IMAGE_SCALING_WORDS],
        draw: &DrawCall,
        uses_render_area: bool,
    ) {
        let render_area = [
            draw.surface_clip.width as f32,
            draw.surface_clip.height as f32,
            0.0,
            0.0,
        ];
        let is_rescaling = self.texture_cache.base.is_rescaling;
        let update_rescaling = self.scheduler.update_rescaling(is_rescaling);
        let scale_down_factor = if is_rescaling {
            common::settings::values().resolution_info.down_factor
        } else {
            1.0
        };
        let bind_pipeline = self.scheduler.update_graphics_pipeline(pipeline);
        let device = self.device.clone();
        let push_descriptor = self.push_descriptor.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            if bind_pipeline {
                device.cmd_bind_pipeline(cmdbuf, vk::PipelineBindPoint::GRAPHICS, pipeline);
            }
            device.cmd_push_constants(
                cmdbuf,
                pipeline_layout,
                vk::ShaderStageFlags::ALL_GRAPHICS,
                RESCALING_LAYOUT_WORDS_OFFSET,
                bytes_of(&rescaling_data),
            );

            if update_rescaling {
                device.cmd_push_constants(
                    cmdbuf,
                    pipeline_layout,
                    vk::ShaderStageFlags::ALL_GRAPHICS,
                    RESCALING_LAYOUT_DOWN_FACTOR_OFFSET,
                    bytes_of(&scale_down_factor),
                );
            }

            if uses_render_area {
                device.cmd_push_constants(
                    cmdbuf,
                    pipeline_layout,
                    vk::ShaderStageFlags::ALL_GRAPHICS,
                    RENDERAREA_LAYOUT_OFFSET,
                    bytes_of(&render_area),
                );
            }
            if descriptor_set_layout == vk::DescriptorSetLayout::null() {
                return;
            }
            let descriptor_data = descriptor_data
                .expect("graphics descriptor layout requires descriptor update payload")
                .0
                .cast::<std::ffi::c_void>();
            if uses_push_descriptor {
                push_descriptor
                    .as_ref()
                    .expect("push-descriptor pipeline requires VK_KHR_push_descriptor")
                    .cmd_push_descriptor_set_with_template(
                        cmdbuf,
                        descriptor_update_template,
                        pipeline_layout,
                        0,
                        descriptor_data,
                    );
            } else if let Some(descriptor_set) = descriptor_set {
                device.update_descriptor_set_with_template(
                    descriptor_set,
                    descriptor_update_template,
                    descriptor_data,
                );
                device.cmd_bind_descriptor_sets(
                    cmdbuf,
                    vk::PipelineBindPoint::GRAPHICS,
                    pipeline_layout,
                    0,
                    &[descriptor_set],
                    &[],
                );
            }
        });
    }

    /// Port of upstream `RasterizerVulkan::FlushWork`.
    fn flush_work(&mut self) {
        self.draw_counter = self.draw_counter.wrapping_add(1);
        if self.draw_counter & Self::DISPATCH_THRESHOLD != Self::DISPATCH_THRESHOLD {
            return;
        }
        if self.draw_counter < Self::FLUSH_THRESHOLD {
            self.scheduler.dispatch_work();
            return;
        }
        self.scheduler.flush();
        self.draw_counter = 0;
        self.state_tracker.invalidate_command_buffer_state();
        self.staging_pool.new_frame();
    }

    /// Submit and wait for all GPU work to complete.
    pub fn finish(&mut self) {
        // End render pass and submit
        self.scheduler.finish();
        self.draw_counter = 0;
        self.state_tracker.invalidate_command_buffer_state();
        self.staging_pool.new_frame();
    }

    fn should_wait_async_flushes(&self) -> bool {
        let cache_wait = unsafe {
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            lock_two_reentrant_mutexes!(buffer_mutex, texture_mutex, _buffer_guard, _texture_guard);
            self.texture_cache.should_wait_async_flushes()
                || self.common_buffer_cache.should_wait_async_flushes()
        };
        cache_wait || self.query_cache.should_wait_async_flushes()
    }

    fn should_flush_async(&self) -> bool {
        let cache_flush = unsafe {
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            lock_two_reentrant_mutexes!(buffer_mutex, texture_mutex, _buffer_guard, _texture_guard);
            self.texture_cache.has_uncommitted_flushes()
                || self.common_buffer_cache.has_uncommitted_flushes()
        };
        cache_flush || self.query_cache.has_uncommitted_flushes()
    }

    fn pop_async_flushes(&mut self) {
        unsafe {
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            lock_two_reentrant_mutexes!(buffer_mutex, texture_mutex, _buffer_guard, _texture_guard);
            self.texture_cache.pop_async_flushes();
            self.common_buffer_cache.pop_async_flushes();
        }
        self.query_cache.pop_async_flushes();
    }

    fn commit_async_flushes(&mut self) {
        unsafe {
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            lock_two_reentrant_mutexes!(buffer_mutex, texture_mutex, _buffer_guard, _texture_guard);
            self.texture_cache.commit_async_flushes();
            self.common_buffer_cache.commit_async_flushes();
        }
        self.query_cache.commit_async_flushes();
    }

    fn queue_fence(&mut self, fence: &mut VkFence) {
        let is_stubbed = fence.lock().unwrap().is_stubbed();
        let tick = if is_stubbed {
            0
        } else {
            self.scheduler.flush()
        };
        self.fence_backend.queue_fence(fence, tick);
    }

    fn is_fence_signaled(&self, fence: &VkFence) -> bool {
        let wait_tick = fence.lock().unwrap().wait_tick();
        self.scheduler.is_free(wait_tick)
    }

    fn wait_fence(&mut self, fence: &VkFence) {
        let wait_tick = fence.lock().unwrap().wait_tick();
        self.scheduler.wait(wait_tick);
    }

    /// Read back the offscreen framebuffer as RGBA8 pixels.
    pub fn read_framebuffer(&mut self) -> Vec<u8> {
        self.texture_cache.transition_layout(
            self.offscreen_image,
            vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
            vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
            vk::ImageAspectFlags::COLOR,
        );

        // Copy to readback buffer
        let region = vk::BufferImageCopy::builder()
            .buffer_offset(0)
            .buffer_row_length(0)
            .buffer_image_height(0)
            .image_subresource(vk::ImageSubresourceLayers {
                aspect_mask: vk::ImageAspectFlags::COLOR,
                mip_level: 0,
                base_array_layer: 0,
                layer_count: 1,
            })
            .image_offset(vk::Offset3D { x: 0, y: 0, z: 0 })
            .image_extent(vk::Extent3D {
                width: self.fb_width,
                height: self.fb_height,
                depth: 1,
            })
            .build();
        let device = self.device.clone();
        let offscreen_image = self.offscreen_image;
        let readback_buffer = self.readback_buffer;
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_copy_image_to_buffer(
                cmdbuf,
                offscreen_image,
                vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                readback_buffer,
                &[region],
            );
        });

        self.texture_cache.transition_layout(
            self.offscreen_image,
            vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
            vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
            vk::ImageAspectFlags::COLOR,
        );

        // Submit and wait
        self.scheduler.finish();

        // Read pixels
        let pixel_count = (self.fb_width * self.fb_height * 4) as usize;
        let mut pixels = vec![0u8; pixel_count];
        unsafe {
            std::ptr::copy_nonoverlapping(self.readback_mapped, pixels.as_mut_ptr(), pixel_count);
        }
        pixels
    }

    /// Render all draw calls and return the framebuffer result.
    ///
    /// This is the main entry point called from GpuContext::flush().
    pub fn render_draw_calls(
        &mut self,
        draws: &[DrawCall],
        read_gpu: &dyn Fn(u64, &mut [u8]),
        base_framebuffer: Option<Framebuffer>,
    ) -> Option<Framebuffer> {
        if draws.is_empty() {
            return base_framebuffer;
        }

        let (fb_width, fb_height, gpu_va) = if let Some(ref fb) = base_framebuffer {
            (fb.width, fb.height, fb.gpu_va)
        } else {
            let rt = &draws[0].render_targets[0];
            let w = if rt.width > 0 { rt.width } else { 1280 };
            let h = if rt.height > 0 { rt.height } else { 720 };
            (w, h, rt.address)
        };

        if fb_width == 0 || fb_height == 0 {
            return None;
        }

        // Resize offscreen framebuffer if needed
        if fb_width != self.fb_width || fb_height != self.fb_height {
            if let Err(e) = self.resize_framebuffer(fb_width, fb_height) {
                warn!("RasterizerVulkan: failed to resize framebuffer: {}", e);
                return base_framebuffer;
            }
        }

        // Process each draw call individually (per-draw dispatch like zuyu)
        let read_gpu_unsafe = |gpu_va: u64, output: &mut [u8]| {
            read_gpu(gpu_va, output);
            true
        };
        for draw in draws {
            // Legacy batch path: no live engine to propagate consumed dirty
            // flags back to, so consume a per-draw copy.
            let mut dirty_flags = draw.dirty_flags;
            self.draw_prepared(
                draw,
                false,
                None,
                &mut dirty_flags,
                None,
                read_gpu,
                &read_gpu_unsafe,
            );
        }

        // Read back rendered pixels
        let pixels = self.read_framebuffer();

        Some(Framebuffer {
            gpu_va,
            width: fb_width,
            height: fb_height,
            pixels,
        })
    }

    // ── Dynamic state update methods ──────────────────────────────────────

    fn update_dynamic_states(
        &mut self,
        draw: &DrawCall,
        dirty_flags: &mut [bool; 256],
        engine_dirty_flags: Option<NonNull<[bool; 256]>>,
    ) {
        self.update_viewports(draw);
        self.update_scissors(draw);
        self.update_depth_bias(draw);
        self.update_blend_constants(draw);
        self.update_depth_bounds(draw);
        self.update_stencil_faces(draw);
        self.update_line_width(draw);
        if self.extended_dynamic_state_supported {
            self.update_cull_mode(draw);
            self.update_depth_compare_op(draw);
            self.update_front_face(draw);
            self.update_stencil_op(draw);

            if self.state_tracker.touch_state_enable() {
                self.update_depth_bounds_test_enable(draw);
                self.update_depth_test_enable(draw);
                self.update_depth_write_enable(draw);
                self.update_stencil_test_enable(draw);
                if self.extended_dynamic_state2_supported {
                    self.update_primitive_restart_enable(draw);
                    self.update_rasterizer_discard_enable(draw);
                    self.update_depth_bias_enable(draw);
                }
                if self.extended_dynamic_state3_enables_supported {
                    self.update_logic_op_enable(draw);
                    self.update_depth_clamp_enable(draw);
                }
            }
            if self.extended_dynamic_state2_extra_supported {
                self.update_logic_op(draw);
            }
            if self.extended_dynamic_state3_blending_supported {
                self.update_blending(draw);
            }
        }
        if self.vertex_input_dynamic_state_supported {
            self.update_vertex_input(draw, dirty_flags, engine_dirty_flags);
        }
    }

    fn update_logic_op_enable(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_logic_op_enable() {
            return;
        }
        let enabled = draw.logic_op.enabled;
        let extension = self
            .extended_dynamic_state3
            .as_ref()
            .expect("dynamic state 3 loader missing")
            .clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            extension.cmd_set_logic_op_enable(cmdbuf, enabled);
        });
    }

    fn update_depth_clamp_enable(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_depth_clamp_enable() {
            return;
        }
        let enabled = draw.depth_clamp_enabled;
        let extension = self
            .extended_dynamic_state3
            .as_ref()
            .expect("dynamic state 3 loader missing")
            .clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            extension.cmd_set_depth_clamp_enable(cmdbuf, enabled);
        });
    }

    fn update_logic_op(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_logic_op() {
            return;
        }
        let raw = draw.logic_op.op;
        let op = if (0x1500..0x1510).contains(&raw) {
            vk::LogicOp::from_raw((raw - 0x1500) as i32)
        } else {
            vk::LogicOp::NO_OP
        };
        let extension = self
            .extended_dynamic_state2
            .as_ref()
            .expect("dynamic state 2 loader missing")
            .clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            extension.cmd_set_logic_op(cmdbuf, op);
        });
    }

    fn update_blending(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_blending() {
            return;
        }
        let extension = self
            .extended_dynamic_state3
            .as_ref()
            .expect("dynamic state 3 loader missing")
            .clone();

        if self.state_tracker.touch_color_mask() {
            let masks = std::array::from_fn::<_, 8, _>(|index| {
                let mask = draw.color_masks[index];
                let mut flags = vk::ColorComponentFlags::empty();
                if mask.r {
                    flags |= vk::ColorComponentFlags::R;
                }
                if mask.g {
                    flags |= vk::ColorComponentFlags::G;
                }
                if mask.b {
                    flags |= vk::ColorComponentFlags::B;
                }
                if mask.a {
                    flags |= vk::ColorComponentFlags::A;
                }
                flags
            });
            let extension = extension.clone();
            self.scheduler.record(move |cmdbuf| unsafe {
                extension.cmd_set_color_write_mask(cmdbuf, 0, &masks);
            });
        }

        if self.state_tracker.touch_blend_enable() {
            let enables = std::array::from_fn::<_, 8, _>(|index| draw.blend[index].enabled.into());
            let extension = extension.clone();
            self.scheduler.record(move |cmdbuf| unsafe {
                extension.cmd_set_color_blend_enable(cmdbuf, 0, &enables);
            });
        }

        if self.state_tracker.touch_blend_equations() {
            let equations = std::array::from_fn::<_, 8, _>(|index| {
                let blend = draw.blend[index];
                vk::ColorBlendEquationEXT {
                    src_color_blend_factor: maxwell_to_vk::blend_factor(blend.color_src),
                    dst_color_blend_factor: maxwell_to_vk::blend_factor(blend.color_dst),
                    color_blend_op: maxwell_to_vk::blend_equation(blend.color_op),
                    src_alpha_blend_factor: maxwell_to_vk::blend_factor(blend.alpha_src),
                    dst_alpha_blend_factor: maxwell_to_vk::blend_factor(blend.alpha_dst),
                    alpha_blend_op: maxwell_to_vk::blend_equation(blend.alpha_op),
                }
            });
            self.scheduler.record(move |cmdbuf| unsafe {
                extension.cmd_set_color_blend_equation(cmdbuf, 0, &equations);
            });
        }
    }

    fn update_vertex_input(
        &mut self,
        draw: &DrawCall,
        dirty_flags: &mut [bool; 256],
        engine_dirty_flags: Option<NonNull<[bool; 256]>>,
    ) {
        use super::state_tracker::dirty;

        let vertex_input_dirty = dirty_flags[dirty::VERTEX_INPUT as usize];
        let vertex_buffers_dirty = dirty_flags[crate::dirty_flags::flags::VERTEX_BUFFERS as usize];
        if !vertex_input_dirty && !vertex_buffers_dirty {
            return;
        }
        dirty_flags[dirty::VERTEX_INPUT as usize] = false;

        let mut bindings = Vec::with_capacity(32);
        let mut attributes = Vec::with_capacity(32);
        let max_attributes = 32usize.min(self.max_vertex_input_attributes as usize);
        let max_bindings = 32usize.min(self.max_vertex_input_bindings as usize);

        for index in 0..max_attributes {
            let attribute = draw.vertex_attribs[index];
            let binding = attribute.buffer_index as usize;
            if attribute.constant || binding >= max_bindings {
                continue;
            }
            attributes.push((
                index as u32,
                binding as u32,
                maxwell_to_vk::vertex_format(
                    self.must_emulate_scaled_formats,
                    attribute.attrib_type,
                    attribute.size,
                ),
                attribute.offset,
            ));
        }

        for binding in 0..max_bindings {
            let stream = draw.vertex_streams[binding];
            let is_instanced = draw.vertex_stream_instances[binding] != 0;
            bindings.push((
                binding as u32,
                stream.stride,
                is_instanced,
                if is_instanced { stream.frequency } else { 1 },
            ));
        }

        for index in 0..32 {
            dirty_flags[dirty::VERTEX_ATTRIBUTE_0 as usize + index] = false;
            dirty_flags[dirty::VERTEX_BINDING_0 as usize + index] = false;
        }
        if let Some(mut flags) = engine_dirty_flags {
            unsafe {
                flags.as_mut()[dirty::VERTEX_INPUT as usize..=dirty::VERTEX_BINDING_31 as usize]
                    .copy_from_slice(
                        &dirty_flags
                            [dirty::VERTEX_INPUT as usize..=dirty::VERTEX_BINDING_31 as usize],
                    );
            }
        }

        let extension = self
            .vertex_input_dynamic_state
            .as_ref()
            .expect("vertex input dynamic state loader missing")
            .clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            let binding_descriptions = bindings
                .into_iter()
                .map(|(binding, stride, is_instanced, divisor)| {
                    vk::VertexInputBindingDescription2EXT::builder()
                        .binding(binding)
                        .stride(stride)
                        .input_rate(if is_instanced {
                            vk::VertexInputRate::INSTANCE
                        } else {
                            vk::VertexInputRate::VERTEX
                        })
                        .divisor(divisor)
                        .build()
                })
                .collect::<Vec<_>>();
            let attribute_descriptions = attributes
                .into_iter()
                .map(|(location, binding, format, offset)| {
                    vk::VertexInputAttributeDescription2EXT::builder()
                        .location(location)
                        .binding(binding)
                        .format(format)
                        .offset(offset)
                        .build()
                })
                .collect::<Vec<_>>();
            (extension.cmd_set_vertex_input_ext)(
                cmdbuf,
                binding_descriptions.len() as u32,
                binding_descriptions.as_ptr(),
                attribute_descriptions.len() as u32,
                attribute_descriptions.as_ptr(),
            );
        });
    }

    fn update_primitive_restart_enable(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_primitive_restart_enable() {
            return;
        }
        let enabled = draw.primitive_restart.enabled;
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_primitive_restart_enable(cmdbuf, enabled);
        });
    }

    fn update_cull_mode(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_cull_mode() {
            return;
        }
        let cull_mode = if draw.rasterizer.cull_enable {
            maxwell_to_vk::cull_face(draw.rasterizer.cull_face)
        } else {
            vk::CullModeFlags::NONE
        };
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_cull_mode(cmdbuf, cull_mode);
        });
    }

    fn update_depth_bounds_test_enable(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_depth_bounds_test_enable() {
            return;
        }
        let mut enabled = draw.depth_bounds_enable;
        if enabled && !self.depth_bounds_supported {
            warn!("Depth bounds is enabled but not supported");
            enabled = false;
        }
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_depth_bounds_test_enable(cmdbuf, enabled);
        });
    }

    fn update_depth_test_enable(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_depth_test_enable() {
            return;
        }
        let enabled = draw.depth_stencil.depth_test_enable;
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_depth_test_enable(cmdbuf, enabled);
        });
    }

    fn update_depth_write_enable(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_depth_write_enable() {
            return;
        }
        let enabled = draw.depth_stencil.depth_write_enable;
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_depth_write_enable(cmdbuf, enabled);
        });
    }

    fn update_stencil_test_enable(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_stencil_test_enable() {
            return;
        }
        let enabled = draw.depth_stencil.stencil_enable;
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_stencil_test_enable(cmdbuf, enabled);
        });
    }

    fn update_rasterizer_discard_enable(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_rasterizer_discard_enable() {
            return;
        }
        let enabled = !draw.rasterize_enable;
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_rasterizer_discard_enable(cmdbuf, enabled);
        });
    }

    fn update_depth_bias_enable(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_depth_bias_enable() {
            return;
        }
        let enabled = match draw.topology {
            PrimitiveTopology::Points => draw.rasterizer.polygon_offset_point_enable,
            PrimitiveTopology::Lines
            | PrimitiveTopology::LineLoop
            | PrimitiveTopology::LineStrip
            | PrimitiveTopology::LinesAdjacency
            | PrimitiveTopology::LineStripAdjacency => draw.rasterizer.polygon_offset_line_enable,
            PrimitiveTopology::Triangles
            | PrimitiveTopology::TriangleStrip
            | PrimitiveTopology::TriangleFan
            | PrimitiveTopology::Quads
            | PrimitiveTopology::QuadStrip
            | PrimitiveTopology::Polygon
            | PrimitiveTopology::TrianglesAdjacency
            | PrimitiveTopology::TriangleStripAdjacency
            | PrimitiveTopology::Patches => draw.rasterizer.polygon_offset_fill_enable,
        };
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_depth_bias_enable(cmdbuf, enabled);
        });
    }

    fn update_depth_compare_op(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_depth_compare_op() {
            return;
        }
        let op = maxwell_to_vk::comparison_op(draw.depth_stencil.depth_func);
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_depth_compare_op(cmdbuf, op);
        });
    }

    fn update_front_face(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_front_face() {
            return;
        }
        let mut front_face = maxwell_to_vk::front_face(draw.rasterizer.front_face);
        if draw.window_origin_flip_y {
            front_face = if front_face == vk::FrontFace::CLOCKWISE {
                vk::FrontFace::COUNTER_CLOCKWISE
            } else {
                vk::FrontFace::CLOCKWISE
            };
        }
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_front_face(cmdbuf, front_face);
        });
    }

    fn update_stencil_op(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_stencil_op() {
            return;
        }
        let front = draw.depth_stencil.front;
        let back = draw.depth_stencil.back;
        let two_side = draw.depth_stencil.stencil_two_side;
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            if two_side {
                device.cmd_set_stencil_op(
                    cmdbuf,
                    vk::StencilFaceFlags::FRONT,
                    maxwell_to_vk::stencil_op(front.fail_op),
                    maxwell_to_vk::stencil_op(front.zpass_op),
                    maxwell_to_vk::stencil_op(front.zfail_op),
                    maxwell_to_vk::comparison_op(front.func),
                );
                device.cmd_set_stencil_op(
                    cmdbuf,
                    vk::StencilFaceFlags::BACK,
                    maxwell_to_vk::stencil_op(back.fail_op),
                    maxwell_to_vk::stencil_op(back.zpass_op),
                    maxwell_to_vk::stencil_op(back.zfail_op),
                    maxwell_to_vk::comparison_op(back.func),
                );
            } else {
                device.cmd_set_stencil_op(
                    cmdbuf,
                    vk::StencilFaceFlags::FRONT_AND_BACK,
                    maxwell_to_vk::stencil_op(front.fail_op),
                    maxwell_to_vk::stencil_op(front.zpass_op),
                    maxwell_to_vk::stencil_op(front.zfail_op),
                    maxwell_to_vk::comparison_op(front.func),
                );
            }
        });
    }

    fn update_viewports(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_viewports() {
            return;
        }
        let viewports = if !draw.viewport_scale_offset_enabled {
            vec![vk::Viewport {
                x: draw.surface_clip.x as f32,
                y: draw.surface_clip.y as f32,
                width: (draw.surface_clip.width as f32).max(1.0),
                height: (draw.surface_clip.height as f32).max(1.0),
                min_depth: 0.0,
                max_depth: 1.0,
            }]
        } else {
            let scale = if self.texture_cache.base.is_rescaling {
                common::settings::values().resolution_info.up_factor
            } else {
                1.0
            };
            std::array::from_fn::<_, { NUM_VIEWPORTS }, _>(|index| {
                viewport_state(
                    draw,
                    index,
                    scale,
                    self.depth_range_unrestricted,
                    self.nv_viewport_swizzle,
                )
            })[..self.max_viewports as usize]
                .to_vec()
        };
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_viewport(cmdbuf, 0, &viewports);
        });
    }

    fn update_scissors(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_scissors() {
            return;
        }
        let scissor = if !draw.viewport_scale_offset_enabled {
            vk::Rect2D {
                offset: vk::Offset2D {
                    x: draw.surface_clip.x as i32,
                    y: draw.surface_clip.y as i32,
                },
                extent: vk::Extent2D {
                    width: draw.surface_clip.width.max(1),
                    height: draw.surface_clip.height.max(1),
                },
            }
        } else if draw.scissors[0].enabled {
            scissor_state(draw, 0)
        } else {
            scissor_state(draw, 0)
        };
        let scissors = if draw.viewport_scale_offset_enabled {
            std::array::from_fn::<_, { NUM_VIEWPORTS }, _>(|index| scissor_state(draw, index))
                [..self.max_viewports as usize]
                .to_vec()
        } else {
            vec![scissor]
        };
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_scissor(cmdbuf, 0, &scissors);
        });
    }

    fn update_depth_bias(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_depth_bias() {
            return;
        }
        let constant = draw.rasterizer.depth_bias;
        let clamp = draw.rasterizer.depth_bias_clamp;
        let slope = draw.rasterizer.slope_scale_depth_bias;
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_depth_bias(cmdbuf, constant, clamp, slope);
        });
    }

    fn update_blend_constants(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_blend_constants() {
            return;
        }
        let blend_constants = [
            draw.blend_color.r,
            draw.blend_color.g,
            draw.blend_color.b,
            draw.blend_color.a,
        ];
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_blend_constants(cmdbuf, &blend_constants);
        });
    }

    fn record_stencil_reference(&mut self, face: vk::StencilFaceFlags, value: u32) {
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_stencil_reference(cmdbuf, face, value);
        });
    }

    fn record_stencil_write_mask(&mut self, face: vk::StencilFaceFlags, value: u32) {
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_stencil_write_mask(cmdbuf, face, value);
        });
    }

    fn record_stencil_compare_mask(&mut self, face: vk::StencilFaceFlags, value: u32) {
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_stencil_compare_mask(cmdbuf, face, value);
        });
    }

    fn update_stencil_faces(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_stencil_properties() {
            return;
        }
        let mut update_references = self.state_tracker.touch_stencil_reference();
        let mut update_write_mask = self.state_tracker.touch_stencil_write_mask();
        let mut update_compare_masks = self.state_tracker.touch_stencil_compare();

        if self
            .state_tracker
            .touch_stencil_side(draw.depth_stencil.stencil_two_side)
        {
            update_references = true;
            update_write_mask = true;
            update_compare_masks = true;
        }

        let front = draw.depth_stencil.front;
        let back = draw.depth_stencil.back;

        if update_references {
            if draw.depth_stencil.stencil_two_side && front.ref_value != back.ref_value {
                if self
                    .state_tracker
                    .check_stencil_reference_front(front.ref_value)
                {
                    self.record_stencil_reference(vk::StencilFaceFlags::FRONT, front.ref_value);
                }
                if self
                    .state_tracker
                    .check_stencil_reference_back(back.ref_value)
                {
                    self.record_stencil_reference(vk::StencilFaceFlags::BACK, back.ref_value);
                }
            } else if self
                .state_tracker
                .check_stencil_reference_front(front.ref_value)
            {
                self.record_stencil_reference(
                    vk::StencilFaceFlags::FRONT_AND_BACK,
                    front.ref_value,
                );
            }
        }

        if update_write_mask {
            if draw.depth_stencil.stencil_two_side && front.write_mask != back.write_mask {
                if self
                    .state_tracker
                    .check_stencil_write_mask_front(front.write_mask)
                {
                    self.record_stencil_write_mask(vk::StencilFaceFlags::FRONT, front.write_mask);
                }
                if self
                    .state_tracker
                    .check_stencil_write_mask_back(back.write_mask)
                {
                    self.record_stencil_write_mask(vk::StencilFaceFlags::BACK, back.write_mask);
                }
            } else if self
                .state_tracker
                .check_stencil_write_mask_front(front.write_mask)
            {
                self.record_stencil_write_mask(
                    vk::StencilFaceFlags::FRONT_AND_BACK,
                    front.write_mask,
                );
            }
        }

        if update_compare_masks {
            if draw.depth_stencil.stencil_two_side && front.func_mask != back.func_mask {
                if self
                    .state_tracker
                    .check_stencil_compare_mask_front(front.func_mask)
                {
                    self.record_stencil_compare_mask(vk::StencilFaceFlags::FRONT, front.func_mask);
                }
                if self
                    .state_tracker
                    .check_stencil_compare_mask_back(back.func_mask)
                {
                    self.record_stencil_compare_mask(vk::StencilFaceFlags::BACK, back.func_mask);
                }
            } else if self
                .state_tracker
                .check_stencil_compare_mask_front(front.func_mask)
            {
                self.record_stencil_compare_mask(
                    vk::StencilFaceFlags::FRONT_AND_BACK,
                    front.func_mask,
                );
            }
        }

        self.state_tracker.clear_stencil_reset();
    }

    fn update_depth_bounds(&mut self, _draw: &DrawCall) {
        if !self.state_tracker.touch_depth_bounds() {
            return;
        }
        // Depth bounds test not currently used, but set safe defaults
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_depth_bounds(cmdbuf, 0.0, 1.0);
        });
    }

    fn update_line_width(&mut self, draw: &DrawCall) {
        if !self.state_tracker.touch_line_width() {
            return;
        }
        let width = draw.rasterizer.line_width_smooth.max(1.0);
        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_line_width(cmdbuf, width);
        });
    }

    fn bind_graphics_texture_buffer_view(
        &mut self,
        stage: usize,
        index: usize,
        view: ImageViewInOut,
        is_written: bool,
        is_image: bool,
    ) {
        let (gpu_addr, size, format) = if view.id.is_valid() && view.id != NULL_IMAGE_VIEW_ID {
            let base = &self.texture_cache.base.slot_image_views[view.id];
            (
                base.gpu_addr,
                crate::surface::bytes_per_block(base.format).saturating_mul(base.size.width),
                base.format as u32,
            )
        } else {
            (0, 0, crate::surface::PixelFormat::Invalid as u32)
        };
        self.common_buffer_cache.bind_graphics_texture_buffer(
            stage, index, gpu_addr, size, format, is_written, is_image,
        );
    }

    fn bind_graphics_descriptors(
        &mut self,
        descriptor_set_layout: vk::DescriptorSetLayout,
        descriptor_bindings: &[GraphicsDescriptorBinding],
        stage_infos: &[Option<ShaderInfo>; 5],
        enabled_uniform_buffer_masks: &[u32; crate::buffer_cache::buffer_cache_base::NUM_STAGES
             as usize],
        uniform_buffer_sizes: &crate::buffer_cache::buffer_cache_base::UniformBufferSizes,
        draw: &DrawCall,
        is_indexed: bool,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
    ) -> Option<PreparedGraphicsDescriptors> {
        let mut prepared = PreparedGraphicsDescriptors::default();
        self.common_buffer_cache
            .set_uniform_buffers_state(enabled_uniform_buffer_masks, uniform_buffer_sizes);

        if descriptor_set_layout == vk::DescriptorSetLayout::null()
            || descriptor_bindings.is_empty()
        {
            self.common_buffer_cache.update_graphics_buffers(is_indexed);
            self.common_buffer_cache
                .bind_host_geometry_buffers(is_indexed);
            return Some(prepared);
        }

        self.texture_cache
            .base
            .synchronize_graphics_descriptors(DescriptorSyncRegs {
                sampler_binding_via_header: matches!(
                    draw.sampler_binding,
                    crate::engines::maxwell_3d::SamplerBinding::ViaHeaderBinding
                ),
                tex_header_addr: draw.tex_header_pool_addr,
                tex_header_limit: draw.tex_header_pool_limit,
                tex_sampler_addr: draw.tex_sampler_pool_addr,
                tex_sampler_limit: draw.tex_sampler_pool_limit,
            });

        let via_header_index = matches!(
            draw.sampler_binding,
            crate::engines::maxwell_3d::SamplerBinding::ViaHeaderBinding
        );
        let read_u32 = |addr: u64| -> u32 {
            let mut bytes = [0u8; 4];
            read_gpu(addr, &mut bytes);
            u32::from_le_bytes(bytes)
        };
        let read_stage_handle = |stage: usize,
                                 cbuf_index: u32,
                                 cbuf_offset: u32,
                                 size_shift: u32,
                                 element: u32,
                                 has_secondary: bool,
                                 shift_left: u32,
                                 secondary_cbuf_index: u32,
                                 secondary_cbuf_offset: u32,
                                 secondary_shift_left: u32|
         -> Option<(u32, u32)> {
            let index_offset = element.checked_shl(size_shift)?;
            let cbuf = draw.cb_bindings[stage].get(cbuf_index as usize)?;
            if !cbuf.enabled {
                log::error!(
                    "shader descriptor references disabled CBUF {} in stage {}",
                    cbuf_index,
                    stage
                );
                return None;
            }
            let addr = cbuf
                .address
                .wrapping_add(cbuf_offset.checked_add(index_offset)? as u64);
            if !has_secondary {
                return Some(texture_pair(read_u32(addr), via_header_index));
            }
            let secondary = draw.cb_bindings[stage].get(secondary_cbuf_index as usize)?;
            if !secondary.enabled {
                log::error!(
                    "shader descriptor references disabled secondary CBUF {} in stage {}",
                    secondary_cbuf_index,
                    stage
                );
                return None;
            }
            let secondary_addr = secondary
                .address
                .wrapping_add(secondary_cbuf_offset.checked_add(index_offset)? as u64);
            Some(texture_pair(
                (read_u32(addr) << shift_left) | (read_u32(secondary_addr) << secondary_shift_left),
                via_header_index,
            ))
        };

        let required_views = stage_infos
            .iter()
            .flatten()
            .map(|info| {
                num_descriptors(&info.texture_buffer_descriptors)
                    + num_descriptors(&info.image_buffer_descriptors)
                    + num_descriptors(&info.texture_descriptors)
                    + num_descriptors(&info.image_descriptors)
            })
            .sum::<u32>() as usize;
        let required_samplers = stage_infos
            .iter()
            .flatten()
            .map(|info| num_descriptors(&info.texture_descriptors))
            .sum::<u32>() as usize;
        if required_views > super::graphics_pipeline::MAX_IMAGE_ELEMENTS
            || required_samplers > super::graphics_pipeline::MAX_IMAGE_ELEMENTS
        {
            log::error!(
                "graphics pipeline descriptor arrays exceed MAX_IMAGE_ELEMENTS: views={} samplers={} max={}",
                required_views,
                required_samplers,
                super::graphics_pipeline::MAX_IMAGE_ELEMENTS
            );
            return None;
        }

        let mut sampler_count = 0usize;
        let mut view_count = 0usize;
        for (stage, info) in stage_infos.iter().enumerate() {
            let Some(info) = info else {
                continue;
            };
            self.common_buffer_cache
                .unbind_graphics_storage_buffers(stage);
            for (ssbo_index, desc) in info.storage_buffers_descriptors.iter().enumerate() {
                if desc.count != 1 {
                    log::error!(
                        "storage buffer descriptor count is {}, expected 1",
                        desc.count
                    );
                }
                self.common_buffer_cache.bind_graphics_storage_buffer(
                    stage,
                    ssbo_index,
                    desc.cbuf_index,
                    desc.cbuf_offset,
                    desc.is_written,
                );
            }

            let mut add_view = |tic_id: u32, blacklist: bool| {
                prepared.views[view_count] = ImageViewInOut {
                    index: tic_id,
                    blacklist,
                    id: NULL_IMAGE_VIEW_ID,
                };
                view_count += 1;
            };
            for desc in &info.texture_buffer_descriptors {
                for element in 0..desc.count {
                    let (tic_id, _) = read_stage_handle(
                        stage,
                        desc.cbuf_index,
                        desc.cbuf_offset,
                        desc.size_shift,
                        element,
                        desc.has_secondary,
                        desc.shift_left,
                        desc.secondary_cbuf_index,
                        desc.secondary_cbuf_offset,
                        desc.secondary_shift_left,
                    )
                    .unwrap_or_default();
                    add_view(tic_id, false);
                }
            }
            for desc in &info.image_buffer_descriptors {
                for element in 0..desc.count {
                    let (tic_id, _) = read_stage_handle(
                        stage,
                        desc.cbuf_index,
                        desc.cbuf_offset,
                        desc.size_shift,
                        element,
                        false,
                        0,
                        0,
                        0,
                        0,
                    )
                    .unwrap_or_default();
                    add_view(tic_id, false);
                }
            }
            for desc in &info.texture_descriptors {
                for element in 0..desc.count {
                    let (tic_id, tsc_id) = read_stage_handle(
                        stage,
                        desc.cbuf_index,
                        desc.cbuf_offset,
                        desc.size_shift,
                        element,
                        desc.has_secondary,
                        desc.shift_left,
                        desc.secondary_cbuf_index,
                        desc.secondary_cbuf_offset,
                        desc.secondary_shift_left,
                    )
                    .unwrap_or_default();
                    add_view(tic_id, false);
                    prepared.samplers[sampler_count] =
                        self.texture_cache.base.get_graphics_sampler_id(tsc_id);
                    sampler_count += 1;
                }
            }
            for desc in &info.image_descriptors {
                for element in 0..desc.count {
                    let (tic_id, _) = read_stage_handle(
                        stage,
                        desc.cbuf_index,
                        desc.cbuf_offset,
                        desc.size_shift,
                        element,
                        false,
                        0,
                        0,
                        0,
                        0,
                    )
                    .unwrap_or_default();
                    add_view(tic_id, desc.is_written);
                }
            }
        }
        prepared.view_count = view_count;
        let has_images = stage_infos
            .iter()
            .flatten()
            .any(|info| !info.image_descriptors.is_empty());
        self.texture_cache
            .fill_graphics_image_views(&mut prepared.views[..view_count], has_images);

        let mut view_cursor = 0usize;
        for (stage, info) in stage_infos.iter().enumerate() {
            let Some(info) = info else {
                continue;
            };
            self.common_buffer_cache
                .unbind_graphics_texture_buffers(stage);
            let mut binding_index = 0usize;
            for desc in &info.texture_buffer_descriptors {
                for _ in 0..desc.count {
                    self.bind_graphics_texture_buffer_view(
                        stage,
                        binding_index,
                        prepared.views[view_cursor],
                        false,
                        false,
                    );
                    binding_index += 1;
                    view_cursor += 1;
                }
            }
            for desc in &info.image_buffer_descriptors {
                for _ in 0..desc.count {
                    self.bind_graphics_texture_buffer_view(
                        stage,
                        binding_index,
                        prepared.views[view_cursor],
                        desc.is_written,
                        true,
                    );
                    binding_index += 1;
                    view_cursor += 1;
                }
            }
            view_cursor += num_descriptors(&info.texture_descriptors) as usize;
            view_cursor += num_descriptors(&info.image_descriptors) as usize;
        }

        for stage in 0..enabled_uniform_buffer_masks.len() {
            let mut bits = enabled_uniform_buffer_masks[stage];
            let mut index = 0u32;
            while bits != 0 {
                let skip = bits.trailing_zeros();
                index += skip;
                bits >>= skip;
                let binding = draw.cb_bindings[stage][index as usize];
                if binding.enabled && binding.address != 0 && binding.size != 0 {
                    self.common_buffer_cache.bind_graphics_uniform_buffer(
                        stage,
                        index,
                        binding.address,
                        binding.size,
                    );
                } else {
                    self.common_buffer_cache
                        .disable_graphics_uniform_buffer(stage, index);
                }
                index += 1;
                bits >>= 1;
            }
        }

        self.common_buffer_cache.update_graphics_buffers(is_indexed);
        self.common_buffer_cache
            .bind_host_geometry_buffers(is_indexed);
        self.desc_queue.acquire();

        let mut rescaling = RescalingPushConstant::new();
        let mut sampler_cursor = 0usize;
        view_cursor = 0;
        for (stage, info) in stage_infos.iter().enumerate() {
            let Some(info) = info else {
                continue;
            };
            self.common_buffer_cache.bind_host_stage_buffers(stage);
            view_cursor += num_descriptors(&info.texture_buffer_descriptors) as usize;
            view_cursor += num_descriptors(&info.image_buffer_descriptors) as usize;
            for desc in &info.texture_descriptors {
                for _ in 0..desc.count {
                    let view_id = prepared.views[view_cursor].id;
                    let image_view = self
                        .texture_cache
                        .materialize_sampled_image_view(view_id, desc.texture_type, read_gpu_unsafe)
                        .unwrap_or_else(|| {
                            self.texture_cache.null_image_view_handle(desc.texture_type)
                        });
                    let supports_anisotropy = view_id.is_valid()
                        && view_id != NULL_IMAGE_VIEW_ID
                        && self.texture_cache.base.slot_image_views[view_id].supports_anisotropy();
                    let sampler = self
                        .texture_cache
                        .sampler(prepared.samplers[sampler_cursor])
                        .map(|sampler| {
                            if sampler.has_added_anisotropy() && !supports_anisotropy {
                                sampler.handle_with_default_anisotropy()
                            } else {
                                sampler.handle()
                            }
                        })
                        .unwrap_or(self.fallback_sampler);
                    self.desc_queue.add_sampled_image(image_view, sampler);
                    rescaling
                        .push_texture(self.texture_cache.base.is_rescaling_image_view(view_id));
                    view_cursor += 1;
                    sampler_cursor += 1;
                }
            }
            for desc in &info.image_descriptors {
                for _ in 0..desc.count {
                    let view_id = prepared.views[view_cursor].id;
                    let image_view = if view_id.is_valid() && view_id != NULL_IMAGE_VIEW_ID {
                        let _ = self.texture_cache.materialize_sampled_image_view(
                            view_id,
                            desc.texture_type,
                            read_gpu_unsafe,
                        );
                        if desc.is_written {
                            let image_id =
                                self.texture_cache.base.slot_image_views[view_id].image_id;
                            if image_id.is_valid() && image_id != NULL_IMAGE_ID {
                                self.texture_cache.base.mark_modification_by_id(image_id);
                            }
                        }
                        self.texture_cache
                            .image_view_storage_view(view_id, desc.texture_type, desc.format)
                            .or_else(|| {
                                self.texture_cache
                                    .null_storage_image_view(desc.texture_type, desc.format)
                            })
                            .unwrap_or(vk::ImageView::null())
                    } else {
                        self.texture_cache
                            .null_storage_image_view(desc.texture_type, desc.format)
                            .unwrap_or(vk::ImageView::null())
                    };
                    self.desc_queue.add_image(image_view);
                    rescaling.push_image(self.texture_cache.base.is_rescaling_image_view(view_id));
                    view_cursor += 1;
                }
            }
        }
        let expected_descriptor_count = descriptor_bindings
            .iter()
            .map(|binding| binding.descriptor_count as usize)
            .sum::<usize>();
        if self.desc_queue.pending_count() != expected_descriptor_count {
            log::error!(
                "descriptor payload/layout mismatch: queued={} expected={}",
                self.desc_queue.pending_count(),
                expected_descriptor_count
            );
            return None;
        }
        prepared.descriptor_data = Some(DescriptorData(self.desc_queue.update_data()));
        prepared.rescaling_data = *rescaling.data();
        Some(prepared)
    }

    // ── Framebuffer resize ────────────────────────────────────────────────

    fn resize_framebuffer(&mut self, new_width: u32, new_height: u32) -> Result<(), RendererError> {
        unsafe {
            self.device.device_wait_idle().ok();
        }

        // Destroy old resources
        unsafe {
            self.device.destroy_framebuffer(self.offscreen_fb, None);
            self.device.destroy_image_view(self.offscreen_view, None);
            self.device.destroy_image(self.offscreen_image, None);
            self.device.free_memory(self.offscreen_memory, None);
            self.device.destroy_image_view(self.depth_view, None);
            self.device.destroy_image(self.depth_image, None);
            self.device.free_memory(self.depth_memory, None);
            self.device.unmap_memory(self.readback_memory);
            self.device.destroy_buffer(self.readback_buffer, None);
            self.device.free_memory(self.readback_memory, None);
        }

        // Create new resources
        let (oi, om, ov) = create_color_attachment(
            &self.instance,
            self.physical_device,
            &self.device,
            new_width,
            new_height,
        )?;
        let (di, dm, dv) = create_depth_attachment(
            &self.instance,
            self.physical_device,
            &self.device,
            new_width,
            new_height,
        )?;
        let fb = create_framebuffer(
            &self.device,
            self.default_render_pass,
            ov,
            dv,
            new_width,
            new_height,
        )?;

        let readback_size = (new_width * new_height * 4) as u64;
        let (rb, rm, rp) = create_host_buffer(
            &self.instance,
            self.physical_device,
            &self.device,
            readback_size,
            vk::BufferUsageFlags::TRANSFER_DST,
        )?;

        self.offscreen_image = oi;
        self.offscreen_memory = om;
        self.offscreen_view = ov;
        self.depth_image = di;
        self.depth_memory = dm;
        self.depth_view = dv;
        self.offscreen_fb = fb;
        self.readback_buffer = rb;
        self.readback_memory = rm;
        self.readback_mapped = rp;
        self.readback_size = readback_size;
        self.fb_width = new_width;
        self.fb_height = new_height;

        info!(
            "RasterizerVulkan: resized framebuffer to {}x{}",
            new_width, new_height
        );
        Ok(())
    }

    /// Port-facing entry point for upstream `RasterizerVulkan::AccelerateDisplay`.
    ///
    /// The texture-cache lookup body is still unported in this active rasterizer
    /// owner, so callers fall back to the raw framebuffer upload path.
    pub fn accelerate_display(
        &mut self,
        config: &FramebufferConfig,
        framebuffer_addr: u64,
        _pixel_stride: u32,
    ) -> Option<blit_screen::FramebufferTextureInfo> {
        if framebuffer_addr == 0 {
            return None;
        }
        let texture_cache: *mut TextureCache = &mut self.texture_cache;
        // Upstream keeps TextureCache::mutex locked for the complete
        // AccelerateDisplay operation. Releasing it after the lookup lets the
        // GPU thread delete and recycle the returned ImageId before its image
        // handle/layout is consumed.
        let _texture_lock = unsafe { (*texture_cache).base.mutex.lock() };
        let framebuffer_view =
            unsafe { (*texture_cache).try_find_framebuffer_image_view(config, framebuffer_addr) };
        let Some(framebuffer_view) = framebuffer_view else {
            return None;
        };
        let image_id = framebuffer_view.common.view.image_id;
        self.query_cache.notify_segment(false);
        unsafe {
            (*texture_cache).prepare_framebuffer_for_present(image_id);
        }
        let resolution = common::settings::values().resolution_info.clone();
        let scaled_width = if framebuffer_view.common.scaled {
            resolution.scale_up_u32(framebuffer_view.width)
        } else {
            framebuffer_view.width
        };
        let scaled_height = if framebuffer_view.common.scaled {
            resolution.scale_up_u32(framebuffer_view.height)
        } else {
            framebuffer_view.height
        };
        Some(blit_screen::FramebufferTextureInfo {
            image: framebuffer_view.image,
            image_view: framebuffer_view.image_view,
            width: framebuffer_view.width,
            height: framebuffer_view.height,
            scaled_width,
            scaled_height,
        })
    }
}

impl RasterizerInterface for RasterizerVulkan {
    fn load_disk_resources(
        &mut self,
        title_id: u64,
        stop_loading: crate::rasterizer_interface::DiskResourceLoadStop,
        callback: crate::rasterizer_interface::DiskResourceLoadCallback,
    ) {
        let shader_dir =
            common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::ShaderDir);
        self.pipeline_cache
            .load_disk_resources(title_id, &shader_dir, stop_loading, callback);
    }

    fn draw(
        &mut self,
        mut draw_view: crate::engines::draw_manager::Maxwell3DDrawView<'_>,
        instance_count: u32,
    ) {
        let _gpu_tick_guard = GpuTickGuard(self.gpu_tick_callback.clone());
        // Upstream `RasterizerVulkan::PrepareDraw` flushes cached GPU-memory
        // writes before descriptors/textures are consumed by the draw.
        if let Some(mm) = self.channel_memory_manager.as_ref().cloned() {
            mm.lock().flush_caching();
        }
        let draw_indexed = draw_view.is_indexed();
        self.draw_sequence = self.draw_sequence.wrapping_add(1);
        debug!(
            "RasterizerVulkan::draw indexed={} instances={}",
            draw_indexed, instance_count
        );
        let Some(memory_manager) = self.channel_memory_manager.as_ref().cloned() else {
            warn!("RasterizerVulkan::draw skipped: no bound channel memory manager");
            return;
        };
        let engine_dirty_flags = draw_view.dirty_flags_ptr();
        let zpass_pixel_count_enabled = draw_view.zpass_pixel_count_enabled();
        let draw_call = draw_view.draw_call_snapshot(instance_count);
        let read_gpu = |gpu_va: u64, output: &mut [u8]| {
            memory_manager.lock().read_block(gpu_va, output);
        };
        let memory_manager_unsafe = Arc::clone(&memory_manager);
        let read_gpu_unsafe = |gpu_va: u64, output: &mut [u8]| {
            memory_manager_unsafe
                .lock()
                .read_block_unsafe(gpu_va, output)
        };
        let mut dirty_flags = draw_call.dirty_flags;
        self.draw_prepared(
            &draw_call,
            zpass_pixel_count_enabled,
            None,
            &mut dirty_flags,
            engine_dirty_flags,
            &read_gpu,
            &read_gpu_unsafe,
        );
        // Upstream backends consume `maxwell3d->dirty.flags` in place (e.g.
        // TextureCache::UpdateRenderTargets clears Dirty::RenderTargets on the
        // live engine). The snapshot in `draw_call` is a copy, so propagate
        // the flags the backend consumed back to the engine — otherwise every
        // draw re-runs the full render-target resolution.
        for (index, dirty) in dirty_flags.iter().enumerate() {
            // The common buffer cache consumes geometry flags directly through
            // the channel-bound Maxwell3D. Replaying the pre-draw snapshot here
            // would erase an invalidation raised during the draw's FlushWork.
            if engine_dirty_flags.is_some() && is_geometry_dirty_flag(index) {
                continue;
            }
            if !dirty && draw_call.dirty_flags[index] {
                draw_view.clear_dirty_flag(index as u8);
            }
        }
    }

    fn draw_indirect(
        &mut self,
        mut indirect_view: crate::engines::draw_manager::Maxwell3DIndirectView<'_>,
    ) {
        let _gpu_tick_guard = GpuTickGuard(self.gpu_tick_callback.clone());
        if let Some(mm) = self.channel_memory_manager.as_ref().cloned() {
            mm.lock().flush_caching();
        }
        let params = *indirect_view.params();
        let Some(memory_manager) = self.channel_memory_manager.as_ref().cloned() else {
            warn!("RasterizerVulkan::draw_indirect skipped: no bound channel memory manager");
            return;
        };

        self.draw_sequence = self.draw_sequence.wrapping_add(1);
        let engine_dirty_flags = indirect_view.dirty_flags_ptr();
        let zpass_pixel_count_enabled = indirect_view.draw_view_mut().zpass_pixel_count_enabled();
        let draw_call = indirect_view.draw_call_snapshot();
        let read_gpu = |gpu_va: u64, output: &mut [u8]| {
            memory_manager.lock().read_block(gpu_va, output);
        };
        let memory_manager_unsafe = Arc::clone(&memory_manager);
        let read_gpu_unsafe = |gpu_va: u64, output: &mut [u8]| {
            memory_manager_unsafe
                .lock()
                .read_block_unsafe(gpu_va, output)
        };
        let cache_params = crate::buffer_cache::buffer_cache_base::DrawIndirectParams {
            indirect_start_address: params.indirect_start_address,
            count_start_address: params.count_start_address,
            buffer_size: params.buffer_size as u64,
            max_draw_counts: params.max_draw_counts as u32,
            stride: params.stride as u32,
            include_count: params.include_count,
        };
        self.common_buffer_cache
            .set_draw_indirect(Some(cache_params));
        let mut dirty_flags = draw_call.dirty_flags;
        self.draw_prepared(
            &draw_call,
            zpass_pixel_count_enabled,
            Some(params),
            &mut dirty_flags,
            engine_dirty_flags,
            &read_gpu,
            &read_gpu_unsafe,
        );
        self.common_buffer_cache.set_draw_indirect(None);

        for (index, dirty) in dirty_flags.iter().enumerate() {
            if engine_dirty_flags.is_some() && is_geometry_dirty_flag(index) {
                continue;
            }
            if !dirty && draw_call.dirty_flags[index] {
                indirect_view.clear_dirty_flag(index as u8);
            }
        }
    }

    fn draw_texture(
        &mut self,
        mut draw_texture_view: crate::engines::draw_manager::Maxwell3DDrawTextureView<'_>,
    ) {
        let _gpu_tick_guard = GpuTickGuard(self.gpu_tick_callback.clone());
        self.flush_work();

        let draw_texture_state = draw_texture_view.draw_texture_state();
        let dynamic_state = draw_texture_view.draw_call_snapshot();
        let render_targets = draw_texture_view.render_targets();
        let descriptor_sync_regs = draw_texture_view.descriptor_sync_regs();
        let mut dirty_flags = draw_texture_view.dirty_flags();
        let original_dirty_flags = dirty_flags;
        let engine_dirty_flags = draw_texture_view.dirty_flags_ptr();

        let Some(memory_manager) = self.channel_memory_manager.as_ref().cloned() else {
            log::warn!("RasterizerVulkan::draw_texture skipped: no bound channel memory manager");
            return;
        };
        let read_gpu_unsafe = |gpu_va: u64, output: &mut [u8]| {
            memory_manager.lock().read_block_unsafe(gpu_va, output)
        };

        // Upstream keeps TextureCache::mutex locked from descriptor
        // synchronization through the BlitImageHelper call.
        let texture_cache_mutex: *const _ = &self.texture_cache.base.mutex;
        let _texture_cache_guard = unsafe { (*texture_cache_mutex).lock() };
        self.texture_cache
            .base
            .synchronize_graphics_descriptors(descriptor_sync_regs);
        let Some(framebuffer) = self
            .texture_cache
            .update_render_targets_and_get_rt0_framebuffer(
                &render_targets,
                &mut dirty_flags,
                &read_gpu_unsafe,
                false,
                None,
            )
        else {
            log::warn!("RasterizerVulkan::draw_texture skipped: no framebuffer");
            return;
        };
        self.update_dynamic_states(&dynamic_state, &mut dirty_flags, engine_dirty_flags);
        self.query_cache.notify_segment(true);
        for (index, dirty) in dirty_flags.iter().enumerate() {
            if !dirty && original_dirty_flags[index] {
                draw_texture_view.clear_dirty_flag(index as u8);
            }
        }
        self.query_cache.counter_enable(
            &mut self.scheduler,
            QueryType::ZPassPixelCount64 as u32,
            draw_texture_view.zpass_pixel_count_enabled(),
        );

        let sampler_id = self
            .texture_cache
            .base
            .get_graphics_sampler_id(draw_texture_state.src_sampler);
        let Some(sampler) = self.texture_cache.sampler_handle(sampler_id) else {
            log::warn!(
                "RasterizerVulkan::draw_texture skipped: invalid sampler {}",
                draw_texture_state.src_sampler
            );
            return;
        };
        let Some(texture) = self
            .texture_cache
            .draw_texture_source(draw_texture_state.src_texture, &read_gpu_unsafe)
        else {
            log::warn!(
                "RasterizerVulkan::draw_texture skipped: invalid texture {}",
                draw_texture_state.src_texture
            );
            return;
        };
        self.texture_cache
            .prepare_render_targets_for_render(&framebuffer.image_ids);

        let cache_is_rescaling = self.texture_cache.base.is_rescaling;
        let src_rescaling = cache_is_rescaling && texture.is_rescaled;
        let dst_rescaling = cache_is_rescaling && framebuffer.is_rescaled;
        let resolution = common::settings::values().resolution_info.clone();
        let scale_src = |value: f32| {
            let value = value as i32;
            if src_rescaling {
                resolution.scale_up_i32(value)
            } else {
                value
            }
        };
        let scale_dst = |value: f32| {
            let value = value as i32;
            if dst_rescaling {
                resolution.scale_up_i32(value)
            } else {
                value
            }
        };
        let dst_region = blit_image::Region2D {
            start: blit_image::Offset2D {
                x: scale_dst(draw_texture_state.dst_x0),
                y: scale_dst(draw_texture_state.dst_y0),
            },
            end: blit_image::Offset2D {
                x: scale_dst(draw_texture_state.dst_x1),
                y: scale_dst(draw_texture_state.dst_y1),
            },
        };
        let src_region = blit_image::Region2D {
            start: blit_image::Offset2D {
                x: scale_src(draw_texture_state.src_x0),
                y: scale_src(draw_texture_state.src_y0),
            },
            end: blit_image::Offset2D {
                x: scale_src(draw_texture_state.src_x1),
                y: scale_src(draw_texture_state.src_y1),
            },
        };
        let mut src_size = texture.size;
        if src_rescaling {
            src_size.width = resolution.scale_up_u32(src_size.width);
            src_size.height = resolution.scale_up_u32(src_size.height);
        }

        self.blit_image.blit_color_with_sampler(
            framebuffer.blit_framebuffer_info(),
            texture.image_view,
            texture.image,
            sampler,
            &dst_region,
            &src_region,
            &src_size,
        );
    }

    fn clear(
        &mut self,
        mut clear_view: crate::engines::draw_manager::Maxwell3DClearView<'_>,
        layer_count: u32,
    ) {
        let _gpu_tick_guard = GpuTickGuard(self.gpu_tick_callback.clone());
        // Preserve upstream ordering: submit pending work before flushing the
        // channel GPU-memory cache.
        self.flush_work();
        if let Some(mm) = self.channel_memory_manager.as_ref().cloned() {
            mm.lock().flush_caching();
        }

        let clear_state = clear_view.clear_state();
        let use_depth = clear_state.flags & (1 << 0) != 0;
        let use_stencil = clear_state.flags & (1 << 1) != 0;
        let use_r = clear_state.flags & (1 << 2) != 0;
        let use_g = clear_state.flags & (1 << 3) != 0;
        let use_b = clear_state.flags & (1 << 4) != 0;
        let use_a = clear_state.flags & (1 << 5) != 0;
        let use_color = use_r || use_g || use_b || use_a;
        if !use_color && !use_depth && !use_stencil {
            return;
        }

        let render_targets = clear_view.render_targets();
        let mut dirty_flags = clear_view.dirty_flags();
        let Some(memory_manager) = self.channel_memory_manager.as_ref().cloned() else {
            warn!("RasterizerVulkan::clear skipped: no bound channel memory manager");
            return;
        };
        let read_gpu_unsafe = |gpu_va: u64, output: &mut [u8]| {
            memory_manager.lock().read_block_unsafe(gpu_va, output)
        };
        let clear_scissor = clear_view.use_scissor().then(|| {
            let scissor = clear_view.scissor(0);
            (scissor.min_x, scissor.min_y, scissor.max_x, scissor.max_y)
        });
        let original_flags = dirty_flags;
        // Upstream holds texture_cache.mutex from UpdateRenderTargets through
        // the clear command. CPU invalidation may otherwise erase a slot while
        // alias synchronization is iterating slot_images.
        let texture_cache_mutex: *const _ = &self.texture_cache.base.mutex;
        let _texture_cache_guard = unsafe { (*texture_cache_mutex).lock() };
        let target = self
            .texture_cache
            .update_render_targets_and_get_rt0_framebuffer(
                &render_targets,
                &mut dirty_flags,
                &read_gpu_unsafe,
                true,
                clear_scissor,
            );
        // Same live-flag propagation as the draw path: the snapshot copy must
        // not swallow the flags consumed by UpdateRenderTargets.
        for (index, dirty) in dirty_flags.iter().enumerate() {
            if !dirty && original_flags[index] {
                clear_view.clear_dirty_flag(index as u8);
            }
        }
        let Some(target) = target else {
            return;
        };
        let framebuffer = target.framebuffer;
        let render_area = target.extent;
        self.texture_cache
            .prepare_render_targets_for_render(&target.image_ids);
        let clear_values = [
            vk::ClearValue {
                color: vk::ClearColorValue {
                    float32: clear_state.color,
                },
            },
            vk::ClearValue {
                depth_stencil: vk::ClearDepthStencilValue {
                    depth: clear_state.depth,
                    stencil: clear_state.stencil as u32,
                },
            },
        ];
        let mut render_pass_clears = vec![clear_values[0]; target.num_color.max(1) as usize];
        if target.has_depth {
            render_pass_clears.push(clear_values[1]);
        }
        self.scheduler.request_outside_renderpass();
        self.scheduler.request_renderpass(
            framebuffer,
            target.render_pass,
            vk::Rect2D {
                offset: vk::Offset2D { x: 0, y: 0 },
                extent: render_area,
            },
            &render_pass_clears,
            &target.images,
            &target.image_ranges,
        );

        let clear_rect_2d = if clear_view.use_scissor() {
            let scissor = clear_view.scissor(0);
            let offset_x = scissor.min_x.min(render_area.width) as i32;
            let offset_y = scissor.min_y.min(render_area.height) as i32;
            let max_x = scissor.max_x.min(render_area.width);
            let max_y = scissor.max_y.min(render_area.height);
            vk::Rect2D {
                offset: vk::Offset2D {
                    x: offset_x,
                    y: offset_y,
                },
                extent: vk::Extent2D {
                    width: max_x.saturating_sub(offset_x as u32),
                    height: max_y.saturating_sub(offset_y as u32),
                },
            }
        } else {
            vk::Rect2D {
                offset: vk::Offset2D { x: 0, y: 0 },
                extent: render_area,
            }
        };
        if clear_rect_2d.extent.width == 0 || clear_rect_2d.extent.height == 0 {
            return;
        }

        let clear_rect = vk::ClearRect {
            rect: clear_rect_2d,
            base_array_layer: ((clear_state.flags >> 10) & 0xFFFF),
            layer_count,
        };
        let color_attachment = ((clear_state.flags >> 6) & 0xF) as usize;
        let mut attachments = Vec::with_capacity(2);
        if use_color && target.has_aspect_color_bit(color_attachment) {
            let format = crate::surface::pixel_format_from_render_target_format(
                render_targets.render_targets[color_attachment].format,
            );
            let clear_value = make_color_clear_value(format, clear_state.color);
            if use_r && use_g && use_b && use_a {
                attachments.push(vk::ClearAttachment {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    color_attachment: color_attachment as u32,
                    clear_value,
                });
            } else {
                let color_mask = u8::from(use_r)
                    | (u8::from(use_g) << 1)
                    | (u8::from(use_b) << 2)
                    | (u8::from(use_a) << 3);
                let dst_region = blit_image::Region2D {
                    start: blit_image::Offset2D {
                        x: clear_rect.rect.offset.x,
                        y: clear_rect.rect.offset.y,
                    },
                    end: blit_image::Offset2D {
                        x: clear_rect.rect.offset.x + clear_rect.rect.extent.width as i32,
                        y: clear_rect.rect.offset.y + clear_rect.rect.extent.height as i32,
                    },
                };
                self.blit_image.clear_color(
                    target.blit_framebuffer_info(),
                    color_mask,
                    clear_state.color,
                    &dst_region,
                );
            }
        }
        let mut depth_stencil_aspects = vk::ImageAspectFlags::empty();
        if target.has_depth && use_depth {
            depth_stencil_aspects |= vk::ImageAspectFlags::DEPTH;
        }
        if target.has_stencil && use_stencil {
            depth_stencil_aspects |= vk::ImageAspectFlags::STENCIL;
        }
        if !depth_stencil_aspects.is_empty() {
            attachments.push(vk::ClearAttachment {
                aspect_mask: depth_stencil_aspects,
                color_attachment: 0,
                clear_value: vk::ClearValue {
                    depth_stencil: vk::ClearDepthStencilValue {
                        depth: clear_state.depth,
                        stencil: clear_state.stencil as u32,
                    },
                },
            });
        }
        if attachments.is_empty() {
            return;
        }

        let device = self.device.clone();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_clear_attachments(cmdbuf, &attachments, &[clear_rect]);
        });
    }

    fn dispatch_compute(&mut self) {
        let _gpu_tick_guard = GpuTickGuard(self.gpu_tick_callback.clone());
        debug!("RasterizerVulkan::dispatch_compute");
    }

    fn dispatch_compute_with_call(&mut self, dispatch: &DispatchCall) {
        let _gpu_tick_guard = GpuTickGuard(self.gpu_tick_callback.clone());
        self.flush_work();
        if let Some(mm) = self.channel_memory_manager.as_ref().cloned() {
            mm.lock().flush_caching();
        }

        let Some(current_pipeline) = self
            .pipeline_cache
            .current_compute_pipeline_with_shared_cache(&mut self.shader_cache)
        else {
            return;
        };

        if current_pipeline.requires_descriptor_binding {
            return;
        }

        if dispatch.indirect_compute_address.is_some() {
            return;
        }

        let device = self.device.clone();
        let pipeline = current_pipeline.pipeline;
        let dim = [
            dispatch.qmd.grid_dim_x,
            dispatch.qmd.grid_dim_y,
            dispatch.qmd.grid_dim_z,
        ];
        self.scheduler.request_outside_renderpass();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_bind_pipeline(cmdbuf, vk::PipelineBindPoint::COMPUTE, pipeline);
            device.cmd_dispatch(cmdbuf, dim[0], dim[1], dim[2]);
        });
    }

    fn reset_counter(&mut self, query_type: u32) {
        if query_type != QueryType::ZPassPixelCount64 as u32 {
            debug!(
                "RasterizerVulkan::reset_counter unimplemented counter reset={}",
                query_type
            );
            return;
        }
        self.query_cache
            .reset_counter(&mut self.scheduler, query_type);
    }

    fn query(
        &mut self,
        gpu_addr: u64,
        query_type: u32,
        flags: QueryPropertiesFlags,
        payload: u32,
        subreport: u32,
    ) {
        let this = self as *mut Self;
        self.query_cache.query(
            &mut self.scheduler,
            gpu_addr,
            query_type,
            flags,
            payload,
            subreport,
            move |func| unsafe { (*this).signal_fence(func) },
            move |func| unsafe { (*this).sync_operation(func) },
        );
    }

    fn bind_graphics_uniform_buffer(
        &mut self,
        _stage: usize,
        _index: u32,
        _gpu_addr: u64,
        _size: u32,
    ) {
    }

    fn disable_graphics_uniform_buffer(&mut self, _stage: usize, _index: u32) {}

    fn signal_fence(&mut self, func: Box<dyn FnOnce() + Send>) {
        let this = self as *mut Self;
        let this_for_pop = this as usize;
        self.fence_manager.signal_fence(
            func,
            move |is_stubbed| unsafe { (*this).fence_backend.create_fence(is_stubbed) },
            move |fence| unsafe { (*this).queue_fence(fence) },
            move || unsafe { (*this).should_wait_async_flushes() },
            move |fence| unsafe { (*this).is_fence_signaled(fence) },
            move || unsafe { (*(this_for_pop as *mut Self)).pop_async_flushes() },
            move || unsafe { (*this).draw_counter != 0 || (*this).should_flush_async() },
            move || unsafe { (*this).commit_async_flushes() },
            move || unsafe { (*this).flush_commands() },
            move || unsafe { (*this).invalidate_gpu_cache() },
        );
    }

    fn sync_operation(&mut self, func: Box<dyn FnOnce() + Send>) {
        self.fence_manager.sync_operation(func);
    }

    fn signal_sync_point(&mut self, id: u32) {
        let this = self as *mut Self;
        let this_for_pop = this as usize;
        let syncpoints = Arc::clone(&self.syncpoints);
        self.fence_manager.signal_sync_point(
            id,
            {
                let syncpoints = Arc::clone(&syncpoints);
                move |value| syncpoints.increment_guest(value)
            },
            move |value| syncpoints.increment_host(value),
            move |is_stubbed| unsafe { (*this).fence_backend.create_fence(is_stubbed) },
            move |fence| unsafe { (*this).queue_fence(fence) },
            move || unsafe { (*this).should_wait_async_flushes() },
            move |fence| unsafe { (*this).is_fence_signaled(fence) },
            move || unsafe { (*(this_for_pop as *mut Self)).pop_async_flushes() },
            move || unsafe { (*this).draw_counter != 0 || (*this).should_flush_async() },
            move || unsafe { (*this).commit_async_flushes() },
            move || unsafe { (*this).flush_commands() },
            move || unsafe { (*this).invalidate_gpu_cache() },
        );
    }

    fn signal_reference(&mut self) {
        let this = self as *mut Self;
        let this_for_pop = this as usize;
        self.fence_manager.signal_reference(
            move |is_stubbed| unsafe { (*this).fence_backend.create_fence(is_stubbed) },
            move |fence| unsafe { (*this).queue_fence(fence) },
            move || unsafe { (*this).should_wait_async_flushes() },
            move |fence| unsafe { (*this).is_fence_signaled(fence) },
            move || unsafe { (*(this_for_pop as *mut Self)).pop_async_flushes() },
            move || unsafe { (*this).draw_counter != 0 || (*this).should_flush_async() },
            move || unsafe { (*this).commit_async_flushes() },
            move || unsafe { (*this).flush_commands() },
            move || unsafe { (*this).invalidate_gpu_cache() },
        );
    }

    fn release_fences(&mut self, force: bool) {
        let this = self as *mut Self;
        let this_for_pop = this as usize;
        self.fence_manager.wait_pending_fences(
            force,
            move |is_stubbed| unsafe { (*this).fence_backend.create_fence(is_stubbed) },
            move |fence| unsafe { (*this).queue_fence(fence) },
            move || unsafe { (*this).should_wait_async_flushes() },
            move |fence| unsafe { (*this).is_fence_signaled(fence) },
            move |fence| unsafe { (*this).wait_fence(fence) },
            move || unsafe { (*(this_for_pop as *mut Self)).pop_async_flushes() },
            move || unsafe { (*this).draw_counter != 0 || (*this).should_flush_async() },
            move || unsafe { (*this).commit_async_flushes() },
            move || unsafe { (*this).flush_commands() },
            move || unsafe { (*this).invalidate_gpu_cache() },
        );
    }

    fn flush_all(&mut self) {
        self.scheduler.flush();
    }

    fn flush_region(&mut self, addr: u64, size: u64) {
        if addr == 0 || size == 0 {
            return;
        }
        unsafe {
            let _lo_tex = common::lock_order::guard("texture_cache");
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            self.texture_cache.download_memory(addr, size as usize);
        }
        unsafe {
            let _lo_buf = common::lock_order::guard("buffer_cache");
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.common_buffer_cache.download_memory(addr, size);
        }
        self.query_cache.flush_region(addr, size as usize);
    }

    fn must_flush_region(&self, addr: u64, size: u64) -> bool {
        {
            let _lo_buf = common::lock_order::guard("buffer_cache");
            let _buffer_guard = self.common_buffer_cache.mutex.lock();
            if self
                .common_buffer_cache
                .is_region_gpu_modified(addr, size as usize)
            {
                return true;
            }
        }
        if !common::settings::is_gpu_level_high(&common::settings::values()) {
            return false;
        }
        let _lo_tex = common::lock_order::guard("texture_cache");
        let _texture_guard = self.texture_cache.base.mutex.lock();
        self.texture_cache
            .base
            .is_region_gpu_modified(addr, size as usize)
    }

    fn get_flush_area(&self, addr: u64, size: u64) -> RasterizerDownloadArea {
        const PAGE: u64 = 4096;
        RasterizerDownloadArea {
            start_address: addr & !(PAGE - 1),
            end_address: (addr + size + PAGE - 1) & !(PAGE - 1),
            preemptive: true,
        }
    }

    fn invalidate_region(&mut self, addr: u64, size: u64) {
        if addr == 0 || size == 0 {
            return;
        }
        unsafe {
            let _lo_tex = common::lock_order::guard("texture_cache");
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            self.texture_cache.base.write_memory(addr, size as usize);
        }
        unsafe {
            let _lo_buf = common::lock_order::guard("buffer_cache");
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.common_buffer_cache.write_memory(addr, size);
        }
        // Geometry is still bound through the reduced GPU-VA keyed cache.
        // Keep it coherent until BindHostGeometryBuffers owns this path.
        self.buffer_cache.write_memory(addr, size);
        self.query_cache.invalidate_region(addr, size as usize);
        self.shader_cache.invalidate_region(addr, size as usize);
    }

    fn on_cache_invalidation(&mut self, addr: u64, size: u64) {
        if addr == 0 || size == 0 {
            return;
        }
        unsafe {
            let _lo_tex = common::lock_order::guard("texture_cache");
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            self.texture_cache.base.write_memory(addr, size as usize);
        }
        unsafe {
            let _lo_buf = common::lock_order::guard("buffer_cache");
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.common_buffer_cache.write_memory(addr, size);
        }
        self.buffer_cache.write_memory(addr, size);
        self.shader_cache.on_cache_invalidation(addr, size as usize);
    }

    fn on_cpu_write(&mut self, addr: u64, size: u64) -> bool {
        if addr == 0 || size == 0 {
            return false;
        }
        let buffer_handled = unsafe {
            let _lo_buf = common::lock_order::guard("buffer_cache");
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.common_buffer_cache.on_cpu_write(addr, size)
        };
        self.buffer_cache.write_memory(addr, size);
        if buffer_handled {
            return true;
        }
        unsafe {
            let _lo_tex = common::lock_order::guard("texture_cache");
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            self.texture_cache.base.write_memory(addr, size as usize);
        }
        self.shader_cache.invalidate_region(addr, size as usize);
        false
    }

    fn invalidate_gpu_cache(&mut self) {
        if let Some(callback) = &self.invalidate_gpu_cache_callback {
            callback();
        }
    }

    fn unmap_memory(&mut self, addr: u64, size: u64) {
        if addr == 0 || size == 0 {
            return;
        }
        unsafe {
            let _lo_tex = common::lock_order::guard("texture_cache");
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            self.texture_cache.base.unmap_memory(addr, size as usize);
        }
        unsafe {
            let _lo_buf = common::lock_order::guard("buffer_cache");
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.common_buffer_cache.write_memory(addr, size);
        }
        self.buffer_cache.write_memory(addr, size);
        self.shader_cache.on_cache_invalidation(addr, size as usize);
    }

    fn modify_gpu_memory(&mut self, _as_id: usize, _addr: u64, _size: u64) {}

    fn flush_and_invalidate_region(&mut self, addr: u64, size: u64) {
        self.invalidate_region(addr, size);
    }

    fn wait_for_idle(&mut self) {
        let flags = vk::PipelineStageFlags::DRAW_INDIRECT
            | vk::PipelineStageFlags::VERTEX_INPUT
            | vk::PipelineStageFlags::VERTEX_SHADER
            | vk::PipelineStageFlags::TESSELLATION_CONTROL_SHADER
            | vk::PipelineStageFlags::TESSELLATION_EVALUATION_SHADER
            | vk::PipelineStageFlags::GEOMETRY_SHADER
            | vk::PipelineStageFlags::FRAGMENT_SHADER
            | vk::PipelineStageFlags::COMPUTE_SHADER
            | vk::PipelineStageFlags::TRANSFER;

        self.query_cache.notify_wfi();

        let device = self.device.clone();
        let event = self.wfi_event;
        self.scheduler.request_outside_renderpass();
        self.scheduler.record(move |cmdbuf| unsafe {
            device.cmd_set_event(cmdbuf, event, flags);
            device.cmd_wait_events(
                cmdbuf,
                &[event],
                flags,
                vk::PipelineStageFlags::TOP_OF_PIPE,
                &[],
                &[],
                &[],
            );
        });
        let this = self as *mut Self;
        let this_for_pop = this as usize;
        self.fence_manager.signal_ordering(
            move || unsafe { (*this).should_wait_async_flushes() },
            move |fence| unsafe { (*this).is_fence_signaled(fence) },
            move || unsafe { (*(this_for_pop as *mut Self)).pop_async_flushes() },
            move || unsafe { (*this).commit_async_flushes() },
        );
    }

    fn fragment_barrier(&mut self) {
        // Upstream `RasterizerVulkan::FragmentBarrier` ends the active render
        // pass. `Scheduler::request_outside_renderpass` emits the attachment
        // write barrier needed before a later texture read.
        self.scheduler.request_outside_renderpass();
    }

    fn tiled_cache_barrier(&mut self) {}

    fn flush_commands(&mut self) {
        self.scheduler.flush();
    }

    fn tick_frame(&mut self) {
        self.draw_counter = 0;
        // Upstream `RasterizerVulkan::TickFrame` rotates both descriptor
        // queues to the next per-frame payload slice before anything else
        // (vk_rasterizer.cpp:765-766). Without this the ring never advances
        // and in-flight frames overwrite each other's descriptor payload.
        self.desc_queue.tick_frame();
        self.compute_pass_desc_queue.tick_frame();
        self.state_tracker.invalidate_command_buffer_state();
        self.staging_pool.new_frame();
        // Retire delayed-destruction rings against GPU completion, not the
        // submission counter (pipelined submissions run ahead of the GPU).
        let known_gpu_tick = self.scheduler.known_gpu_tick();
        unsafe {
            let _lo_tex = common::lock_order::guard("texture_cache");
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            self.texture_cache.tick_frame(known_gpu_tick);
        }
        self.buffer_cache
            .tick_frame(known_gpu_tick, self.scheduler.pending_tick());
        unsafe {
            let _lo_buf = common::lock_order::guard("buffer_cache");
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.common_buffer_cache.tick_frame();
        }
    }

    fn initialize_channel(&mut self, channel: &mut crate::control::channel_state::ChannelState) {
        self.channel_caches.create_channel(channel);
        self.texture_cache.create_channel(channel);
        self.buffer_cache.create_channel(channel);
        self.common_buffer_cache.create_channel(channel);
        self.shader_cache.create_channel(channel);
        self.pipeline_cache.create_channel(channel);
        self.query_cache.create_channel(channel);
        self.state_tracker.setup_tables(channel);
    }

    fn bind_channel(&mut self, channel: &mut crate::control::channel_state::ChannelState) {
        self.channel_caches.bind_to_channel(channel.bind_id);
        self.texture_cache.bind_to_channel(channel.bind_id);
        self.buffer_cache.bind_to_channel(channel.bind_id);
        self.common_buffer_cache.bind_to_channel(channel.bind_id);
        self.shader_cache.bind_to_channel(channel.bind_id);
        self.pipeline_cache.bind_to_channel(channel.bind_id);
        self.query_cache.bind_to_channel(channel.bind_id);
        self.state_tracker.change_channel(channel);
        self.state_tracker.invalidate_state();
        self.channel_memory_manager = self
            .channel_caches
            .current_channel_state()
            .and_then(ChannelCacheAccessor::gpu_memory_arc);
        if let Some(mm) = self.channel_memory_manager.as_ref() {
            self.common_buffer_cache
                .set_gpu_memory(Box::new(GpuMemoryAccessAdapter { mm: Arc::clone(mm) }));
        }
    }

    fn release_channel(&mut self, channel_id: i32) {
        self.state_tracker.release_channel(channel_id);
        self.channel_caches.erase_channel(channel_id);
        self.texture_cache.erase_channel(channel_id);
        self.buffer_cache.erase_channel(channel_id);
        self.common_buffer_cache.erase_channel(channel_id);
        self.shader_cache.erase_channel(channel_id);
        self.pipeline_cache.erase_channel(channel_id);
        self.query_cache.erase_channel(channel_id);
        self.channel_memory_manager = None;
    }

    fn accelerate_surface_copy(
        &mut self,
        src: &crate::engines::fermi_2d::Surface,
        dst: &crate::engines::fermi_2d::Surface,
        copy_config: &crate::engines::fermi_2d::Config,
    ) -> bool {
        let Some(mm) = self.channel_memory_manager.as_ref().cloned() else {
            return false;
        };
        let texture_cache: *mut TextureCache = &mut self.texture_cache;
        unsafe {
            let _texture_lock = (*texture_cache).base.mutex.lock();
            (*texture_cache).blit_image(
                dst,
                src,
                copy_config,
                |gpu_addr| mm.lock().gpu_to_cpu_address(gpu_addr),
                |gpu_addr, out| {
                    let guard = mm.lock();
                    guard.read_block(gpu_addr, out);
                    true
                },
            )
        }
    }

    fn accelerate_dma_buffer_copy(
        &mut self,
        src_address: u64,
        dest_address: u64,
        amount: u64,
    ) -> bool {
        unsafe {
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.common_buffer_cache
                .dma_copy(src_address, dest_address, amount)
        }
    }

    fn accelerate_dma_buffer_clear(&mut self, dst_address: u64, amount: u64, value: u32) -> bool {
        unsafe {
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.common_buffer_cache
                .dma_clear(dst_address, amount, value)
        }
    }

    fn accelerate_dma_image_to_buffer(
        &mut self,
        copy_info: &dma::ImageCopy,
        src: &dma::ImageOperand,
        dst: &dma::BufferOperand,
    ) -> bool {
        let Some(mm) = self.channel_memory_manager.as_ref().cloned() else {
            return false;
        };
        unsafe {
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            lock_two_reentrant_mutexes!(buffer_mutex, texture_mutex, _buffer_guard, _texture_guard);

            let image_id = self.texture_cache.base.dma_image_id(src, false);
            if image_id == NULL_IMAGE_ID {
                return false;
            }

            let buffer_size = dst.pitch.saturating_mul(dst.height);
            let (buffer_id, offset) = self.common_buffer_cache.obtain_buffer(
                dst.address,
                buffer_size,
                ObtainBufferSynchronize::FullSynchronize,
                ObtainBufferOperation::MarkAsWritten,
            );
            let raw_buffer = self
                .common_buffer_cache
                .resolve_backend_buffer_raw(buffer_id);
            let buffer = vk::Buffer::from_raw(raw_buffer);
            if buffer == vk::Buffer::null() {
                return false;
            }

            let read_gpu_unsafe =
                |gpu_addr: u64, out: &mut [u8]| mm.lock().read_block_unsafe(gpu_addr, out);
            self.texture_cache.dma_buffer_image_copy(
                copy_info,
                dst,
                src,
                image_id,
                buffer,
                offset as vk::DeviceSize,
                false,
                &read_gpu_unsafe,
            )
        }
    }

    fn accelerate_dma_buffer_to_image(
        &mut self,
        copy_info: &dma::ImageCopy,
        src: &dma::BufferOperand,
        dst: &dma::ImageOperand,
    ) -> bool {
        let Some(mm) = self.channel_memory_manager.as_ref().cloned() else {
            return false;
        };
        unsafe {
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            lock_two_reentrant_mutexes!(buffer_mutex, texture_mutex, _buffer_guard, _texture_guard);

            let image_id = self.texture_cache.base.dma_image_id(dst, true);
            if image_id == NULL_IMAGE_ID {
                return false;
            }

            let buffer_size = src.pitch.saturating_mul(src.height);
            let (buffer_id, offset) = self.common_buffer_cache.obtain_buffer(
                src.address,
                buffer_size,
                ObtainBufferSynchronize::FullSynchronize,
                ObtainBufferOperation::DoNothing,
            );
            let raw_buffer = self
                .common_buffer_cache
                .resolve_backend_buffer_raw(buffer_id);
            let buffer = vk::Buffer::from_raw(raw_buffer);
            if buffer == vk::Buffer::null() {
                return false;
            }

            let read_gpu_unsafe =
                |gpu_addr: u64, out: &mut [u8]| mm.lock().read_block_unsafe(gpu_addr, out);
            self.texture_cache.dma_buffer_image_copy(
                copy_info,
                src,
                dst,
                image_id,
                buffer,
                offset as vk::DeviceSize,
                true,
                &read_gpu_unsafe,
            )
        }
    }

    fn accelerate_inline_to_memory(&mut self, address: u64, copy_size: usize, memory: &[u8]) {
        let copy_size = copy_size.min(memory.len());
        if copy_size == 0 {
            return;
        }

        let Some(mm) = self.channel_memory_manager.as_ref().cloned() else {
            return;
        };

        let mm = mm.lock();
        let cpu_addr = mm.gpu_to_cpu_address(address);
        let input = &memory[..copy_size];
        if cpu_addr.is_none() {
            mm.write_block(address, input);
            return;
        }
        mm.write_block_unsafe(address, input);
        drop(mm);

        let cpu_addr = cpu_addr.unwrap();
        unsafe {
            let _lo_buf = common::lock_order::guard("buffer_cache");
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            if !self
                .common_buffer_cache
                .inline_memory(cpu_addr, copy_size, input)
            {
                self.common_buffer_cache
                    .write_memory(cpu_addr, copy_size as u64);
            }
        }
        unsafe {
            let _lo_tex = common::lock_order::guard("texture_cache");
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            self.texture_cache.base.write_memory(cpu_addr, copy_size);
        }
        self.shader_cache.invalidate_region(cpu_addr, copy_size);
        self.query_cache.invalidate_region(cpu_addr, copy_size);
    }
}

impl Drop for RasterizerVulkan {
    fn drop(&mut self) {
        unsafe {
            self.device.device_wait_idle().ok();

            self.device.unmap_memory(self.readback_memory);
            self.device.destroy_buffer(self.readback_buffer, None);
            self.device.free_memory(self.readback_memory, None);

            self.device.destroy_sampler(self.fallback_sampler, None);
            self.device.unmap_memory(self.fallback_uniform_memory);
            self.device
                .destroy_buffer(self.fallback_uniform_buffer, None);
            self.device.free_memory(self.fallback_uniform_memory, None);

            self.device.destroy_framebuffer(self.offscreen_fb, None);
            self.device.destroy_image_view(self.offscreen_view, None);
            self.device.destroy_image(self.offscreen_image, None);
            self.device.free_memory(self.offscreen_memory, None);

            self.device.destroy_image_view(self.depth_view, None);
            self.device.destroy_image(self.depth_image, None);
            self.device.free_memory(self.depth_memory, None);

            self.device.destroy_event(self.wfi_event, None);
            self.device
                .destroy_render_pass(self.default_render_pass, None);
        }
    }
}

// ── State mapping helpers (reused from old renderer.rs) ────────────────────

pub(crate) fn map_topology(topo: PrimitiveTopology) -> vk::PrimitiveTopology {
    match topo {
        PrimitiveTopology::Points => vk::PrimitiveTopology::POINT_LIST,
        PrimitiveTopology::Lines => vk::PrimitiveTopology::LINE_LIST,
        PrimitiveTopology::LineStrip => vk::PrimitiveTopology::LINE_STRIP,
        PrimitiveTopology::Triangles => vk::PrimitiveTopology::TRIANGLE_LIST,
        PrimitiveTopology::TriangleStrip => vk::PrimitiveTopology::TRIANGLE_STRIP,
        PrimitiveTopology::TriangleFan => vk::PrimitiveTopology::TRIANGLE_FAN,
        _ => vk::PrimitiveTopology::TRIANGLE_LIST,
    }
}

pub(crate) fn map_cull_mode(
    rasterizer: &crate::engines::maxwell_3d::RasterizerInfo,
) -> vk::CullModeFlags {
    if !rasterizer.cull_enable {
        return vk::CullModeFlags::NONE;
    }
    match rasterizer.cull_face {
        CullFace::Front => vk::CullModeFlags::FRONT,
        CullFace::Back => vk::CullModeFlags::BACK,
        CullFace::FrontAndBack => vk::CullModeFlags::FRONT_AND_BACK,
    }
}

pub(crate) fn map_front_face(ff: FrontFace) -> vk::FrontFace {
    match ff {
        FrontFace::CW => vk::FrontFace::CLOCKWISE,
        FrontFace::CCW => vk::FrontFace::COUNTER_CLOCKWISE,
    }
}

pub(crate) fn map_compare_op(op: ComparisonOp) -> vk::CompareOp {
    match op {
        ComparisonOp::Never => vk::CompareOp::NEVER,
        ComparisonOp::Less => vk::CompareOp::LESS,
        ComparisonOp::Equal => vk::CompareOp::EQUAL,
        ComparisonOp::LessEqual => vk::CompareOp::LESS_OR_EQUAL,
        ComparisonOp::Greater => vk::CompareOp::GREATER,
        ComparisonOp::NotEqual => vk::CompareOp::NOT_EQUAL,
        ComparisonOp::GreaterEqual => vk::CompareOp::GREATER_OR_EQUAL,
        ComparisonOp::Always => vk::CompareOp::ALWAYS,
    }
}

pub(crate) fn map_blend_factor(factor: BlendFactor) -> vk::BlendFactor {
    match factor {
        BlendFactor::Zero => vk::BlendFactor::ZERO,
        BlendFactor::One => vk::BlendFactor::ONE,
        BlendFactor::SrcColor => vk::BlendFactor::SRC_COLOR,
        BlendFactor::OneMinusSrcColor => vk::BlendFactor::ONE_MINUS_SRC_COLOR,
        BlendFactor::SrcAlpha => vk::BlendFactor::SRC_ALPHA,
        BlendFactor::OneMinusSrcAlpha => vk::BlendFactor::ONE_MINUS_SRC_ALPHA,
        BlendFactor::DstAlpha => vk::BlendFactor::DST_ALPHA,
        BlendFactor::OneMinusDstAlpha => vk::BlendFactor::ONE_MINUS_DST_ALPHA,
        BlendFactor::DstColor => vk::BlendFactor::DST_COLOR,
        BlendFactor::OneMinusDstColor => vk::BlendFactor::ONE_MINUS_DST_COLOR,
        BlendFactor::SrcAlphaSaturate => vk::BlendFactor::SRC_ALPHA_SATURATE,
        BlendFactor::ConstantColor => vk::BlendFactor::CONSTANT_COLOR,
        BlendFactor::OneMinusConstantColor => vk::BlendFactor::ONE_MINUS_CONSTANT_COLOR,
        BlendFactor::ConstantAlpha => vk::BlendFactor::CONSTANT_ALPHA,
        BlendFactor::OneMinusConstantAlpha => vk::BlendFactor::ONE_MINUS_CONSTANT_ALPHA,
        BlendFactor::Src1Color => vk::BlendFactor::SRC1_COLOR,
        BlendFactor::OneMinusSrc1Color => vk::BlendFactor::ONE_MINUS_SRC1_COLOR,
        BlendFactor::Src1Alpha => vk::BlendFactor::SRC1_ALPHA,
        BlendFactor::OneMinusSrc1Alpha => vk::BlendFactor::ONE_MINUS_SRC1_ALPHA,
    }
}

pub(crate) fn map_blend_equation(eq: BlendEquation) -> vk::BlendOp {
    match eq {
        BlendEquation::Add => vk::BlendOp::ADD,
        BlendEquation::Subtract => vk::BlendOp::SUBTRACT,
        BlendEquation::ReverseSubtract => vk::BlendOp::REVERSE_SUBTRACT,
        BlendEquation::Min => vk::BlendOp::MIN,
        BlendEquation::Max => vk::BlendOp::MAX,
    }
}

// ── Vulkan resource creation helpers ───────────────────────────────────────

fn find_memory_type(
    instance: &ash::Instance,
    physical_device: vk::PhysicalDevice,
    type_filter: u32,
    properties: vk::MemoryPropertyFlags,
) -> Option<u32> {
    let mem_props = unsafe { instance.get_physical_device_memory_properties(physical_device) };
    for i in 0..mem_props.memory_type_count {
        if (type_filter & (1 << i)) != 0
            && mem_props.memory_types[i as usize]
                .property_flags
                .contains(properties)
        {
            return Some(i);
        }
    }
    None
}

fn create_default_render_pass(device: &ash::Device) -> Result<vk::RenderPass, RendererError> {
    let attachments = [
        // Color attachment (RGBA8)
        vk::AttachmentDescription::builder()
            .format(vk::Format::R8G8B8A8_UNORM)
            .samples(vk::SampleCountFlags::TYPE_1)
            .load_op(vk::AttachmentLoadOp::CLEAR)
            .store_op(vk::AttachmentStoreOp::STORE)
            .stencil_load_op(vk::AttachmentLoadOp::DONT_CARE)
            .stencil_store_op(vk::AttachmentStoreOp::DONT_CARE)
            .initial_layout(vk::ImageLayout::UNDEFINED)
            .final_layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)
            .build(),
        // Depth attachment
        vk::AttachmentDescription::builder()
            .format(vk::Format::D32_SFLOAT)
            .samples(vk::SampleCountFlags::TYPE_1)
            .load_op(vk::AttachmentLoadOp::CLEAR)
            .store_op(vk::AttachmentStoreOp::DONT_CARE)
            .stencil_load_op(vk::AttachmentLoadOp::DONT_CARE)
            .stencil_store_op(vk::AttachmentStoreOp::DONT_CARE)
            .initial_layout(vk::ImageLayout::UNDEFINED)
            .final_layout(vk::ImageLayout::DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
            .build(),
    ];

    let color_ref = [vk::AttachmentReference {
        attachment: 0,
        layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
    }];
    let depth_ref = vk::AttachmentReference {
        attachment: 1,
        layout: vk::ImageLayout::DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
    };

    let subpass = vk::SubpassDescription::builder()
        .pipeline_bind_point(vk::PipelineBindPoint::GRAPHICS)
        .color_attachments(&color_ref)
        .depth_stencil_attachment(&depth_ref)
        .build();

    let dependency = vk::SubpassDependency::builder()
        .src_subpass(vk::SUBPASS_EXTERNAL)
        .dst_subpass(0)
        .src_stage_mask(
            vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT
                | vk::PipelineStageFlags::EARLY_FRAGMENT_TESTS,
        )
        .dst_stage_mask(
            vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT
                | vk::PipelineStageFlags::EARLY_FRAGMENT_TESTS,
        )
        .src_access_mask(vk::AccessFlags::empty())
        .dst_access_mask(
            vk::AccessFlags::COLOR_ATTACHMENT_WRITE
                | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE,
        )
        .build();

    let render_pass_info = vk::RenderPassCreateInfo::builder()
        .attachments(&attachments)
        .subpasses(std::slice::from_ref(&subpass))
        .dependencies(std::slice::from_ref(&dependency))
        .build();

    unsafe {
        device
            .create_render_pass(&render_pass_info, None)
            .map_err(|e| RendererError::InitFailed(format!("render pass: {:?}", e)))
    }
}

fn create_color_attachment(
    instance: &ash::Instance,
    physical_device: vk::PhysicalDevice,
    device: &ash::Device,
    width: u32,
    height: u32,
) -> Result<(vk::Image, vk::DeviceMemory, vk::ImageView), RendererError> {
    let image_info = vk::ImageCreateInfo::builder()
        .image_type(vk::ImageType::TYPE_2D)
        .format(vk::Format::R8G8B8A8_UNORM)
        .extent(vk::Extent3D {
            width,
            height,
            depth: 1,
        })
        .mip_levels(1)
        .array_layers(1)
        .samples(vk::SampleCountFlags::TYPE_1)
        .tiling(vk::ImageTiling::OPTIMAL)
        .usage(vk::ImageUsageFlags::COLOR_ATTACHMENT | vk::ImageUsageFlags::TRANSFER_SRC)
        .sharing_mode(vk::SharingMode::EXCLUSIVE)
        .build();

    let image = unsafe {
        device
            .create_image(&image_info, None)
            .map_err(|e| RendererError::InitFailed(format!("color image: {:?}", e)))?
    };

    let mem_reqs = unsafe { device.get_image_memory_requirements(image) };
    let mem_type = find_memory_type(
        instance,
        physical_device,
        mem_reqs.memory_type_bits,
        vk::MemoryPropertyFlags::DEVICE_LOCAL,
    )
    .ok_or_else(|| RendererError::InitFailed("no device-local memory".into()))?;

    let alloc_info = vk::MemoryAllocateInfo::builder()
        .allocation_size(mem_reqs.size)
        .memory_type_index(mem_type)
        .build();
    let memory = unsafe {
        device
            .allocate_memory(&alloc_info, None)
            .map_err(|e| RendererError::InitFailed(format!("color memory: {:?}", e)))?
    };
    unsafe {
        device
            .bind_image_memory(image, memory, 0)
            .map_err(|e| RendererError::InitFailed(format!("bind color: {:?}", e)))?;
    }

    let view_info = vk::ImageViewCreateInfo::builder()
        .image(image)
        .view_type(vk::ImageViewType::TYPE_2D)
        .format(vk::Format::R8G8B8A8_UNORM)
        .subresource_range(vk::ImageSubresourceRange {
            aspect_mask: vk::ImageAspectFlags::COLOR,
            base_mip_level: 0,
            level_count: 1,
            base_array_layer: 0,
            layer_count: 1,
        })
        .build();
    let view = unsafe {
        device
            .create_image_view(&view_info, None)
            .map_err(|e| RendererError::InitFailed(format!("color view: {:?}", e)))?
    };

    Ok((image, memory, view))
}

fn create_depth_attachment(
    instance: &ash::Instance,
    physical_device: vk::PhysicalDevice,
    device: &ash::Device,
    width: u32,
    height: u32,
) -> Result<(vk::Image, vk::DeviceMemory, vk::ImageView), RendererError> {
    let image_info = vk::ImageCreateInfo::builder()
        .image_type(vk::ImageType::TYPE_2D)
        .format(vk::Format::D32_SFLOAT)
        .extent(vk::Extent3D {
            width,
            height,
            depth: 1,
        })
        .mip_levels(1)
        .array_layers(1)
        .samples(vk::SampleCountFlags::TYPE_1)
        .tiling(vk::ImageTiling::OPTIMAL)
        .usage(vk::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT)
        .sharing_mode(vk::SharingMode::EXCLUSIVE)
        .build();

    let image = unsafe {
        device
            .create_image(&image_info, None)
            .map_err(|e| RendererError::InitFailed(format!("depth image: {:?}", e)))?
    };

    let mem_reqs = unsafe { device.get_image_memory_requirements(image) };
    let mem_type = find_memory_type(
        instance,
        physical_device,
        mem_reqs.memory_type_bits,
        vk::MemoryPropertyFlags::DEVICE_LOCAL,
    )
    .ok_or_else(|| RendererError::InitFailed("no device-local memory for depth".into()))?;

    let alloc_info = vk::MemoryAllocateInfo::builder()
        .allocation_size(mem_reqs.size)
        .memory_type_index(mem_type)
        .build();
    let memory = unsafe {
        device
            .allocate_memory(&alloc_info, None)
            .map_err(|e| RendererError::InitFailed(format!("depth memory: {:?}", e)))?
    };
    unsafe {
        device
            .bind_image_memory(image, memory, 0)
            .map_err(|e| RendererError::InitFailed(format!("bind depth: {:?}", e)))?;
    }

    let view_info = vk::ImageViewCreateInfo::builder()
        .image(image)
        .view_type(vk::ImageViewType::TYPE_2D)
        .format(vk::Format::D32_SFLOAT)
        .subresource_range(vk::ImageSubresourceRange {
            aspect_mask: vk::ImageAspectFlags::DEPTH,
            base_mip_level: 0,
            level_count: 1,
            base_array_layer: 0,
            layer_count: 1,
        })
        .build();
    let view = unsafe {
        device
            .create_image_view(&view_info, None)
            .map_err(|e| RendererError::InitFailed(format!("depth view: {:?}", e)))?
    };

    Ok((image, memory, view))
}

fn create_framebuffer(
    device: &ash::Device,
    render_pass: vk::RenderPass,
    color_view: vk::ImageView,
    depth_view: vk::ImageView,
    width: u32,
    height: u32,
) -> Result<vk::Framebuffer, RendererError> {
    let attachments = [color_view, depth_view];
    let fb_info = vk::FramebufferCreateInfo::builder()
        .render_pass(render_pass)
        .attachments(&attachments)
        .width(width)
        .height(height)
        .layers(1)
        .build();
    unsafe {
        device
            .create_framebuffer(&fb_info, None)
            .map_err(|e| RendererError::InitFailed(format!("framebuffer: {:?}", e)))
    }
}

fn create_fallback_sampler(device: &ash::Device) -> Result<vk::Sampler, RendererError> {
    let sampler_info = vk::SamplerCreateInfo::builder()
        .mag_filter(vk::Filter::NEAREST)
        .min_filter(vk::Filter::NEAREST)
        .mipmap_mode(vk::SamplerMipmapMode::NEAREST)
        .address_mode_u(vk::SamplerAddressMode::CLAMP_TO_EDGE)
        .address_mode_v(vk::SamplerAddressMode::CLAMP_TO_EDGE)
        .address_mode_w(vk::SamplerAddressMode::CLAMP_TO_EDGE)
        .min_lod(0.0)
        .max_lod(0.0)
        .build();

    unsafe {
        device
            .create_sampler(&sampler_info, None)
            .map_err(|e| RendererError::InitFailed(format!("fallback sampler: {:?}", e)))
    }
}

fn null_buffer_descriptor(
    has_null_descriptor: bool,
    fallback_buffer: vk::Buffer,
) -> (vk::Buffer, vk::DeviceSize, vk::DeviceSize) {
    if has_null_descriptor {
        // Keep the non-zero range used by BufferCacheRuntime; the buffer
        // handle itself is ignored by VK_EXT_robustness2 null descriptors.
        (vk::Buffer::null(), 0, 1)
    } else {
        (fallback_buffer, 0, 0x10000)
    }
}

fn required_descriptor_set_missing(
    layout: vk::DescriptorSetLayout,
    binding_count: usize,
    descriptor_set: Option<vk::DescriptorSet>,
) -> bool {
    layout != vk::DescriptorSetLayout::null() && binding_count != 0 && descriptor_set.is_none()
}

fn create_host_buffer(
    instance: &ash::Instance,
    physical_device: vk::PhysicalDevice,
    device: &ash::Device,
    size: u64,
    usage: vk::BufferUsageFlags,
) -> Result<(vk::Buffer, vk::DeviceMemory, *mut u8), RendererError> {
    let buf_info = vk::BufferCreateInfo::builder()
        .size(size)
        .usage(usage)
        .sharing_mode(vk::SharingMode::EXCLUSIVE)
        .build();

    let buffer = unsafe {
        device
            .create_buffer(&buf_info, None)
            .map_err(|e| RendererError::InitFailed(format!("buffer: {:?}", e)))?
    };

    let mem_reqs = unsafe { device.get_buffer_memory_requirements(buffer) };
    let mem_type = find_memory_type(
        instance,
        physical_device,
        mem_reqs.memory_type_bits,
        vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
    )
    .ok_or_else(|| RendererError::InitFailed("no host-visible memory".into()))?;

    let alloc_info = vk::MemoryAllocateInfo::builder()
        .allocation_size(mem_reqs.size)
        .memory_type_index(mem_type)
        .build();
    let memory = unsafe {
        device
            .allocate_memory(&alloc_info, None)
            .map_err(|e| RendererError::InitFailed(format!("buffer memory: {:?}", e)))?
    };
    unsafe {
        device
            .bind_buffer_memory(buffer, memory, 0)
            .map_err(|e| RendererError::InitFailed(format!("bind buffer: {:?}", e)))?;
    }

    let mapped = unsafe {
        device
            .map_memory(memory, 0, size, vk::MemoryMapFlags::empty())
            .map_err(|e| RendererError::InitFailed(format!("map buffer: {:?}", e)))?
            as *mut u8
    };

    Ok((buffer, memory, mapped))
}

// ── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map_topology() {
        assert_eq!(
            map_topology(PrimitiveTopology::Triangles),
            vk::PrimitiveTopology::TRIANGLE_LIST
        );
        assert_eq!(
            map_topology(PrimitiveTopology::Points),
            vk::PrimitiveTopology::POINT_LIST
        );
        assert_eq!(
            map_topology(PrimitiveTopology::TriangleStrip),
            vk::PrimitiveTopology::TRIANGLE_STRIP
        );
    }

    #[test]
    fn missing_buffer_uses_null_descriptor_when_supported() {
        let fallback = vk::Buffer::from_raw(0x1234);
        assert_eq!(
            null_buffer_descriptor(true, fallback),
            (vk::Buffer::null(), 0, 1)
        );
        assert_eq!(
            null_buffer_descriptor(false, fallback),
            (fallback, 0, 0x10000)
        );
    }

    #[test]
    fn descriptor_trace_preserves_handles_offsets_and_ranges() {
        let infos = [
            vk::DescriptorBufferInfo {
                buffer: vk::Buffer::from_raw(0x1234),
                offset: 0x80,
                range: 0x400,
            },
            vk::DescriptorBufferInfo {
                buffer: vk::Buffer::null(),
                offset: 0,
                range: 1,
            },
        ];
        assert_eq!(
            format_descriptor_buffer_infos(&infos),
            "0x1234@0x80+0x400,0x0@0x0+0x1"
        );
    }

    #[test]
    fn sync_draw_interval_is_opt_in_and_rejects_zero_or_invalid_values() {
        assert_eq!(parse_vulkan_sync_draw_interval(None), None);
        assert_eq!(parse_vulkan_sync_draw_interval(Some("")), None);
        assert_eq!(parse_vulkan_sync_draw_interval(Some("invalid")), None);
        assert_eq!(parse_vulkan_sync_draw_interval(Some("0")), None);
        assert_eq!(parse_vulkan_sync_draw_interval(Some("1")), Some(1));
        assert_eq!(parse_vulkan_sync_draw_interval(Some("32")), Some(32));
    }

    #[test]
    fn incomplete_required_descriptor_set_skips_draw() {
        let layout = vk::DescriptorSetLayout::from_raw(0x1234);
        let set = vk::DescriptorSet::from_raw(0x5678);
        assert!(required_descriptor_set_missing(layout, 1, None));
        assert!(!required_descriptor_set_missing(layout, 1, Some(set)));
        assert!(!required_descriptor_set_missing(
            vk::DescriptorSetLayout::null(),
            1,
            None
        ));
        assert!(!required_descriptor_set_missing(layout, 0, None));
    }

    #[test]
    fn graphics_descriptors_use_vulkan_texture_cache_view_wrapper() {
        let source = include_str!("vk_rasterizer.rs");
        let function = source
            .split("fn bind_graphics_descriptors")
            .nth(1)
            .expect("bind_graphics_descriptors must exist")
            .split("// ── Framebuffer resize")
            .next()
            .expect("bind_graphics_descriptors boundary must exist");

        assert!(function.contains("self.texture_cache\n            .fill_graphics_image_views"));
        assert!(!function.contains(
            "self.texture_cache\n            .base\n            .fill_graphics_image_views"
        ));
        assert!(function.contains("null_image_view_handle"));
        assert!(function.contains("null_storage_image_view"));
        assert!(
            !function.contains("self.offscreen_view"),
            "the framebuffer attachment is not a legal sampled/storage fallback"
        );
    }

    #[test]
    fn graphics_views_are_resolved_before_render_targets() {
        let source = include_str!("vk_rasterizer.rs");
        let function = source
            .split("fn draw_prepared")
            .nth(1)
            .expect("draw_prepared must exist")
            .split("pub fn read_framebuffer")
            .next()
            .expect("draw_prepared boundary must exist");
        let bind = function
            .find("self.bind_graphics_descriptors")
            .expect("graphics views must be resolved");
        let update = function
            .find(".update_render_targets_and_get_rt0_framebuffer")
            .expect("render targets must be updated");
        let feedback = function
            .find(".check_feedback_loop(&prepared_descriptors.views)")
            .expect("feedback loops must be checked");

        assert!(
            bind < update,
            "image views must be resolved before render targets"
        );
        assert!(
            update < feedback,
            "feedback loops require current render targets"
        );
    }

    #[test]
    fn prepare_draw_orders_query_segments_like_upstream() {
        let source = include_str!("vk_rasterizer.rs");
        let function = source
            .split("fn draw_prepared")
            .nth(1)
            .expect("draw_prepared must exist")
            .split("fn push_graphics_push_constants")
            .next()
            .expect("draw_prepared boundary must exist");
        let clear_geometry = function
            .find("dirty_flags[crate::dirty_flags::flags::VERTEX_BUFFERS")
            .expect("buffer-cache geometry dirty state must be consumed");
        let dynamic = function
            .find("self.update_dynamic_states")
            .expect("dynamic state update must exist");
        let segment = function
            .find("self.query_cache.notify_segment(true)")
            .expect("draw segment notification must exist");
        let transform_feedback = function
            .find(".handle_transform_feedback")
            .expect("transform-feedback handling must exist");
        let zpass = function
            .find("QueryType::ZPassPixelCount64")
            .expect("ZPass enable must exist");

        assert!(clear_geometry < dynamic);
        assert!(dynamic < segment);
        assert!(segment < transform_feedback);
        assert!(transform_feedback < zpass);
    }

    #[test]
    fn dynamic_vertex_input_rebuilds_the_complete_description() {
        let source = include_str!("vk_rasterizer.rs");
        let function = source
            .split("fn update_vertex_input")
            .nth(1)
            .expect("update_vertex_input must exist")
            .split("fn update_primitive_restart_enable")
            .next()
            .expect("update_vertex_input boundary must exist");

        assert!(function.contains("for index in 0..max_attributes"));
        assert!(function.contains("for binding in 0..max_bindings"));
        assert!(function.contains("attribute.constant || binding >= max_bindings"));
        assert!(function.contains("VERTEX_BUFFERS"));
        assert!(!function.contains("highest_dirty_attr"));
    }

    #[test]
    fn test_map_compare_op() {
        assert_eq!(map_compare_op(ComparisonOp::Less), vk::CompareOp::LESS);
        assert_eq!(map_compare_op(ComparisonOp::Always), vk::CompareOp::ALWAYS);
        assert_eq!(map_compare_op(ComparisonOp::Never), vk::CompareOp::NEVER);
    }

    #[test]
    fn test_map_blend_factor() {
        assert_eq!(map_blend_factor(BlendFactor::One), vk::BlendFactor::ONE);
        assert_eq!(
            map_blend_factor(BlendFactor::SrcAlpha),
            vk::BlendFactor::SRC_ALPHA
        );
    }

    #[test]
    fn test_map_blend_equation() {
        assert_eq!(map_blend_equation(BlendEquation::Add), vk::BlendOp::ADD);
        assert_eq!(map_blend_equation(BlendEquation::Min), vk::BlendOp::MIN);
    }

    #[test]
    fn test_map_front_face() {
        assert_eq!(map_front_face(FrontFace::CW), vk::FrontFace::CLOCKWISE);
        assert_eq!(
            map_front_face(FrontFace::CCW),
            vk::FrontFace::COUNTER_CLOCKWISE
        );
    }

    #[test]
    fn viewport_identity_scale() {
        let viewport = get_viewport_state(
            320.0, 320.0, 240.0, 240.0, 0.5, 0.5, 1.0, false, false, false, 480.0, false,
        );
        assert_eq!(viewport.x, 0.0);
        assert_eq!(viewport.width, 640.0);
        assert_eq!(viewport.y, 0.0);
        assert_eq!(viewport.height, 480.0);
    }

    #[test]
    fn viewport_rescaling_matches_upstream_factor_and_rounding() {
        let upscaled = get_viewport_state(
            321.0, 319.0, 241.0, 239.0, 0.5, 0.5, 1.5, false, false, false, 480.0, false,
        );
        assert_eq!(upscaled.x, 3.0);
        assert_eq!(upscaled.width, 957.0);
        assert_eq!(upscaled.y, 3.0);
        assert_eq!(upscaled.height, 717.0);

        let downscaled = get_viewport_state(
            318.0, 320.0, 238.0, 240.0, 0.5, 0.5, 0.75, false, false, false, 480.0, false,
        );
        assert_eq!(downscaled.x, -2.0);
        assert_eq!(downscaled.width, 480.0);
        assert_eq!(downscaled.y, -2.0);
        assert_eq!(downscaled.height, 360.0);
    }

    #[test]
    fn viewport_depth_range_matches_extension_support() {
        let clamped = get_viewport_state(
            0.0, 1.0, 0.0, 1.0, 2.0, 2.0, 1.0, false, false, false, 1.0, true,
        );
        assert_eq!(clamped.min_depth, 1.0);
        assert_eq!(clamped.max_depth, 1.0);

        let unrestricted = get_viewport_state(
            0.0, 1.0, 0.0, 1.0, 2.0, 2.0, 1.0, false, false, false, 1.0, false,
        );
        assert_eq!(unrestricted.min_depth, 2.0);
        assert_eq!(unrestricted.max_depth, 4.0);
    }

    #[test]
    fn geometry_dirty_range_matches_upstream_common_flags() {
        assert!(is_geometry_dirty_flag(
            crate::dirty_flags::flags::INDEX_BUFFER as usize
        ));
        assert!(is_geometry_dirty_flag(
            crate::dirty_flags::flags::VERTEX_BUFFERS as usize
        ));
        assert!(is_geometry_dirty_flag(
            crate::dirty_flags::flags::VERTEX_BUFFER31 as usize
        ));
        assert!(!is_geometry_dirty_flag(
            crate::dirty_flags::flags::RENDER_TARGETS as usize
        ));
    }

    #[test]
    fn runtime_has_no_graphics_engine_snapshot_adapter() {
        let source = include_str!("vk_rasterizer.rs");
        let runtime = source.split("#[cfg(test)]").next().unwrap_or(source);
        assert!(!runtime.contains("VulkanDrawStateEngineAdapter"));
        assert!(!runtime.contains("set_engine_state(Box::new"));
    }

    #[test]
    fn color_clear_value_matches_upstream_format_conversion() {
        let float_value = make_color_clear_value(
            crate::surface::PixelFormat::B10G11R11Float,
            [0.25, 0.5, 0.75, 1.0],
        );
        let uint_value =
            make_color_clear_value(crate::surface::PixelFormat::R8Uint, [0.25, 0.5, 0.75, 1.0]);
        let sint_value =
            make_color_clear_value(crate::surface::PixelFormat::R8Sint, [0.0, 0.5, 1.0, 0.25]);

        unsafe {
            assert_eq!(float_value.color.float32, [0.25, 0.5, 0.75, 1.0]);
            assert_eq!(uint_value.color.uint32, [4, 8, 12, 16]);
            assert_eq!(sint_value.color.int32, [-7, 0, 7, -3]);
        }
    }
}
