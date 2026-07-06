// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Vulkan GPU renderer — multi-component architecture matching zuyu.
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

pub mod blit_image;
pub mod blit_screen;
pub mod buffer_cache;
pub mod buffer_cache_base;
pub mod command_pool;
pub mod compute_pass;
pub mod compute_pipeline;
pub mod descriptor_pool;
pub mod fence_manager;
pub mod fixed_pipeline_state;
pub mod graphics_pipeline;
pub mod master_semaphore;
pub mod maxwell_to_vk;
pub mod pipeline_cache;
pub mod pipeline_helper;
pub mod pipeline_statistics;
pub mod present;
pub mod present_manager;
pub mod query_cache;
pub mod render_pass_cache;
pub mod renderer_vulkan;
pub mod resource_pool;
pub mod scheduler;
pub mod shader_util;
pub mod staging_buffer_pool;
pub mod state_tracker;
pub mod swapchain;
pub mod texture_cache;
pub mod texture_cache_base;
pub mod turbo_mode;
pub mod update_descriptor;
pub mod vk_rasterizer;

use std::ptr::NonNull;
use std::sync::Arc;

use ash::vk;
use ash::vk::Handle;
use log::{debug, info, trace, warn};
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
use crate::texture_cache::types::{NULL_IMAGE_ID, NULL_IMAGE_VIEW_ID};
use crate::textures::texture::texture_pair;
use crate::vulkan_common::vulkan_memory_allocator::MemoryAllocator;
use shader_recompiler::shader_info::Info as ShaderInfo;
use shader_recompiler::{PipelineCache as ShaderPipelineCache, Profile};

use self::pipeline_helper::{
    RescalingPushConstant, RENDERAREA_LAYOUT_OFFSET, RESCALING_LAYOUT_DOWN_FACTOR_OFFSET,
    RESCALING_LAYOUT_WORDS_OFFSET,
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

fn make_draw_params(draw: &DrawCall) -> DrawParams {
    let mut params = DrawParams {
        base_instance: draw.base_instance,
        num_instances: draw.instance_count.max(1),
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
            params.first_index = 0;
            params.is_indexed = true;
        }
        PrimitiveTopology::QuadStrip => {
            params.num_vertices = params.num_vertices.saturating_sub(2) / 2 * 6;
            params.base_vertex = 0;
            params.first_index = 0;
            params.is_indexed = true;
        }
        _ => {}
    }

    params
}

fn should_skip_debug_draw(rt0_addr: u64, draw_counter: u32) -> bool {
    let Some(target_rt) = std::env::var("RUZU_SKIP_VK_DRAW_RT")
        .ok()
        .and_then(|value| u64::from_str_radix(value.trim_start_matches("0x"), 16).ok())
    else {
        return false;
    };
    if target_rt != rt0_addr {
        return false;
    }
    if std::env::var("RUZU_SKIP_VK_DRAW_GE")
        .ok()
        .and_then(|value| value.parse::<u32>().ok())
        .is_some_and(|target_draw| draw_counter >= target_draw)
    {
        return true;
    }
    std::env::var("RUZU_SKIP_VK_DRAW")
        .ok()
        .and_then(|value| value.parse::<u32>().ok())
        .is_some_and(|target_draw| target_draw == draw_counter)
}

use self::fence_manager::{Fence as VkFence, FenceManager as VkFenceBackend};

fn viewport_state(draw: &DrawCall, index: usize) -> vk::Viewport {
    let src = draw.viewport_transforms[index];
    vk_rasterizer::get_viewport_state(
        src.translate_x,
        src.scale_x,
        src.translate_y,
        src.scale_y,
        src.translate_z,
        src.scale_z,
        1.0,
        draw.depth_stencil.depth_mode == crate::engines::maxwell_3d::DepthMode::MinusOneToOne,
        draw.window_origin_lower_left,
        ((src.swizzle >> 4) & 0x7) == 3,
        draw.surface_clip.height as f32,
        true,
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
                width: src.max_x.saturating_sub(src.min_x).max(1),
                height: (max_y - min_y).max(1) as u32,
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
use blit_image::BlitImageHelper;
use buffer_cache::{
    BufferCache as DirectBufferCache, BufferCacheRuntime, VulkanCommonBufferCache,
    VULKAN_DEVICE_TRACKER,
};
use descriptor_pool::{DescriptorBankInfo, DescriptorPool};
use graphics_pipeline::GraphicsDescriptorBinding;
use pipeline_cache::PipelineCache as VulkanPipelineCache;
use query_cache::QueryCache as VulkanQueryCache;
use render_pass_cache::RenderPassCache;
use scheduler::Scheduler;
use staging_buffer_pool::StagingBufferPool;
use state_tracker::StateTracker;
use texture_cache::TextureCache;
use update_descriptor::{DescriptorUpdateEntry, UpdateDescriptorQueue};

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
    channel_caches: ChannelSetupCaches<ChannelInfo>,

    // Sub-components (matching zuyu's architecture)
    scheduler: Box<Scheduler>,
    memory_allocator: NonNull<MemoryAllocator>,
    state_tracker: Box<StateTracker>,
    staging_pool: Box<StagingBufferPool>,
    // Boxed like `scheduler`/`staging_pool`/`render_pass_cache`: sub-components
    // capture `NonNull` pointers to these during construction (BlitImageHelper
    // and TextureCache point at the descriptor pool and the descriptor queues,
    // TextureCache at the blit helper). A by-value field would move when the
    // constructor returns `Self`, leaving those pointers dangling on the old
    // stack frame — observed as an UpdateDescriptorQueue whose `acquire()`
    // clamped the real instance while `add_buffer` grew a stale cursor until
    // the payload overflowed (~80s into MK8D).
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

    // Draw counter for periodic flush (zuyu: 7 draws → dispatch, 4096 → flush)
    draw_counter: u32,
    /// Draws dropped because pipeline compilation failed (diagnostic).
    draw_skipped_pipeline: u64,
    /// Draws redirected to the offscreen framebuffer because no guest
    /// render-target framebuffer could be resolved (diagnostic).
    draw_offscreen_fallback: u64,
    extended_dynamic_state_supported: bool,
    extended_dynamic_state2_supported: bool,
    max_viewports: u32,

    // Channel-bound GPU memory manager, matching upstream rasterizer access to
    // the active channel's Tegra::MemoryManager.
    channel_memory_manager: Option<Arc<parking_lot::Mutex<crate::memory_manager::MemoryManager>>>,
}

// Raw pointers are only used for mapped memory
unsafe impl Send for RasterizerVulkan {}

impl RasterizerVulkan {
    /// Flush threshold — dispatch work every N draws.
    const DISPATCH_THRESHOLD: u32 = 7;
    /// Hard flush threshold — full GPU submit every N draws.
    const FLUSH_THRESHOLD: u32 = 4096;

    /// Create a new RasterizerVulkan.
    ///
    /// Takes Vulkan handles from the VulkanPresenter so they share the same
    /// device and queue.
    pub fn new(
        instance: ash::Instance,
        physical_device: vk::PhysicalDevice,
        device: ash::Device,
        graphics_queue: vk::Queue,
        queue_family_index: u32,
        width: u32,
        height: u32,
        supported_spirv_version: u32,
        extended_dynamic_state_supported: bool,
        extended_dynamic_state2_supported: bool,
        topology_list_primitive_restart_supported: bool,
        patch_list_primitive_restart_supported: bool,
        must_emulate_scaled_formats: bool,
        shader_stencil_export_supported: bool,
        timeline_semaphore_supported: bool,
        custom_border_color_supported: bool,
        max_viewports: u32,
        syncpoints: Arc<SyncpointManager>,
        device_memory: Arc<MaxwellDeviceMemoryManager>,
        memory_allocator: &mut MemoryAllocator,
    ) -> Result<Self, RendererError> {
        info!(
            "RasterizerVulkan: initializing {}x{} renderer",
            width, height
        );

        // Create command pool
        let pool_info = vk::CommandPoolCreateInfo::builder()
            .queue_family_index(queue_family_index)
            .flags(vk::CommandPoolCreateFlags::RESET_COMMAND_BUFFER)
            .build();
        let command_pool = unsafe {
            device
                .create_command_pool(&pool_info, None)
                .map_err(|e| RendererError::InitFailed(format!("command pool: {:?}", e)))?
        };

        // Create state tracker
        let mut state_tracker = Box::new(StateTracker::new());

        // Create scheduler
        let mut scheduler = Box::new(
            Scheduler::new(
                device.clone(),
                graphics_queue,
                command_pool,
                timeline_semaphore_supported,
            )
            .map_err(|e| RendererError::InitFailed(format!("scheduler: {:?}", e)))?,
        );
        scheduler.set_state_tracker(NonNull::from(state_tracker.as_mut()));

        // Create staging buffer pool
        let mut staging_pool = Box::new(StagingBufferPool::new(
            device.clone(),
            instance.clone(),
            physical_device,
            scheduler.as_mut(),
        ));

        // Create descriptor pool. Boxed (with the descriptor queues and the
        // blit helper below) so the `NonNull` pointers captured by
        // sub-components stay valid when the constructed `Self` is moved.
        let mut descriptor_pool = Box::new(DescriptorPool::new(device.clone(), 64));

        // Create descriptor update queue
        let mut desc_queue = Box::new(UpdateDescriptorQueue::new());
        let mut compute_pass_desc_queue = Box::new(UpdateDescriptorQueue::new());
        let mut blit_image = Box::new(BlitImageHelper::new(
            device.clone(),
            &mut scheduler,
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
        let fallback_sampler = create_fallback_sampler(&device)?;

        // Create render pass cache
        let mut render_pass_cache = Box::new(RenderPassCache::new(device.clone()));

        // Create shader recompiler pipeline cache
        let profile = Profile {
            supported_spirv: supported_spirv_version,
            unified_descriptor_binding: true,
            ..Profile::default()
        };
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
            use_asynchronous_shaders,
            use_vulkan_pipeline_cache,
            shader_cache,
            profile,
            render_pass_cache.as_mut(),
            extended_dynamic_state_supported,
            extended_dynamic_state2_supported,
            // `extendedDynamicState2LogicOp` is not yet exposed by the Rust
            // device wrapper. Keep the feature false so pipeline keys and disk
            // cache filtering match the actually enabled command set.
            false,
            // Extended dynamic state 3 and vertex-input dynamic state are not
            // enabled by this simplified Vulkan init path yet.
            false,
            false,
            false,
            must_emulate_scaled_formats,
            topology_list_primitive_restart_supported,
            patch_list_primitive_restart_supported,
            max_viewports,
        );

        // Create buffer cache
        let buffer_cache =
            DirectBufferCache::new(device.clone(), instance.clone(), physical_device)
                .map_err(|e| RendererError::InitFailed(format!("buffer cache: {:?}", e)))?;
        let mut common_buffer_cache = VulkanCommonBufferCache::new(&VULKAN_DEVICE_TRACKER);
        let buffer_runtime = BufferCacheRuntime::new(
            device.clone(),
            instance.clone(),
            physical_device,
            scheduler.as_mut(),
            staging_pool.as_mut(),
            desc_queue.as_mut(),
        );
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
            device_memory,
            scheduler.as_mut(),
            &mut *memory_allocator,
            staging_pool.as_mut(),
            blit_image.as_mut(),
            render_pass_cache.as_mut(),
            descriptor_pool.as_mut(),
            compute_pass_desc_queue.as_mut(),
            custom_border_color_supported,
        );

        // Create query cache
        let query_cache = VulkanQueryCache::new();

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

        Ok(Self {
            device,
            instance,
            physical_device,
            syncpoints,
            channel_caches: ChannelSetupCaches::new(),
            scheduler,
            memory_allocator: NonNull::from(&mut *memory_allocator),
            state_tracker,
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
            fence_backend: VkFenceBackend::new(),
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
            draw_skipped_pipeline: 0,
            draw_offscreen_fallback: 0,
            extended_dynamic_state_supported,
            extended_dynamic_state2_supported,
            max_viewports: max_viewports.min(NUM_VIEWPORTS as u32).max(1),
            channel_memory_manager: None,
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

    /// Main draw entry point — process a single draw call.
    ///
    /// Ref: zuyu RasterizerVulkan::Draw() — compiles/caches pipeline,
    /// updates dynamic state via dirty flags, binds resources, records draw.
    pub fn draw(
        &mut self,
        draw: &DrawCall,
        dirty_flags: &mut [bool; 256],
        read_gpu: &dyn Fn(u64, &mut [u8]),
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
    ) {
        // 1. Periodic flush
        self.flush_work();

        // 2. Update render targets before compiling the pipeline. Upstream
        // requests the render pass from the framebuffer selected by
        // TextureCache::UpdateRenderTargets, so the pipeline must be built
        // against that same render pass.
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
        let target_has_depth = target_fb.as_ref().is_some_and(|target| target.has_depth);
        let target_num_color = target_fb
            .as_ref()
            .map(|target| target.num_color)
            .unwrap_or(1);
        let render_pass = target_fb
            .as_ref()
            .map(|target| target.render_pass)
            .unwrap_or(self.default_render_pass);

        // 3. Compile or lookup cached pipeline
        let pipeline_result = self
            .pipeline_cache
            .current_graphics_pipeline_with_shared_cache(draw, render_pass, &mut self.shader_cache);
        let (
            pipeline,
            pipeline_layout,
            descriptor_set_layout,
            descriptor_bindings,
            descriptor_bank_info,
            stage_infos,
            enabled_uniform_buffer_masks,
            uniform_buffer_sizes,
            uses_render_area,
            uses_rescaling_uniform,
        ) = match pipeline_result {
            Some((gp, _fixed_state)) => {
                let Some(pipeline) = gp.pipeline_handle() else {
                    self.draw_skipped_pipeline = self.draw_skipped_pipeline.wrapping_add(1);
                    if self.draw_skipped_pipeline <= 16
                        || self.draw_skipped_pipeline.is_power_of_two()
                    {
                        log::warn!(
                            "[DRAW_SKIP] #{} pipeline build failed asynchronously (draw={} rt0=0x{:X} fmt={} topology={:?} indexed={})",
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
                (
                    pipeline,
                    gp.pipeline_layout,
                    gp.descriptor_set_layout,
                    gp.descriptor_bindings.clone(),
                    gp.descriptor_bank_info,
                    gp.stage_infos.clone(),
                    gp.enabled_uniform_buffer_masks,
                    gp.uniform_buffer_sizes,
                    gp.uses_render_area,
                    gp.uses_rescaling_uniform,
                )
            }
            None => {
                self.draw_skipped_pipeline = self.draw_skipped_pipeline.wrapping_add(1);
                // A skipped draw leaves the previous frame's pixels in place;
                // with LOAD attachments this accumulates visibly (e.g. the
                // MK8D title fog washing to white). Surface the loss.
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

        // 4. Ensure we're inside a render pass
        let cmd = self.scheduler.command_buffer();
        let (framebuffer, extent, rp_images, rp_image_ranges) = if let Some(target) = target_fb {
            self.texture_cache
                .prepare_render_targets_for_render(&target.image_ids, cmd);
            (
                target.framebuffer,
                target.extent,
                target.images,
                target.image_ranges,
            )
        } else {
            self.draw_offscreen_fallback = self.draw_offscreen_fallback.wrapping_add(1);
            // The draw executes but lands in the internal offscreen
            // framebuffer instead of the guest render target — the guest RT
            // keeps its stale contents, which reads as a lost draw.
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
        let render_area = vk::Rect2D {
            offset: vk::Offset2D { x: 0, y: 0 },
            extent,
        };

        // 4. Bind pipeline
        unsafe {
            self.device
                .cmd_bind_pipeline(cmd, vk::PipelineBindPoint::GRAPHICS, pipeline);
        }
        self.push_graphics_push_constants(
            cmd,
            pipeline_layout,
            draw,
            uses_render_area,
            uses_rescaling_uniform,
        );
        let draw_params = make_draw_params(draw);
        self.bind_graphics_descriptors(
            cmd,
            pipeline_layout,
            descriptor_set_layout,
            &descriptor_bindings,
            &descriptor_bank_info,
            &stage_infos,
            &enabled_uniform_buffer_masks,
            &uniform_buffer_sizes,
            draw,
            read_gpu,
            read_gpu_unsafe,
        );

        // 5. Bind vertex/index buffers
        self.bind_vertex_buffers(cmd, draw, read_gpu);
        if draw_params.is_indexed {
            self.bind_index_buffer(cmd, draw, draw_params, read_gpu);
        }

        // Texture/buffer materialization can enqueue upload copies and
        // barriers through `Scheduler::record_with_upload`. Upstream records
        // the draw itself through the scheduler, preserving command order in
        // the chunk. This reduced backend emits vkCmdDraw directly, so flush
        // the preparation chunk before entering the render pass for the draw.
        self.scheduler.dispatch_work();

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

        // 6. Update dynamic states via dirty flags. Upstream requests the
        // render pass in `GraphicsPipeline::ConfigureDraw` before
        // `RasterizerVulkan::UpdateDynamicStates`.
        self.update_dynamic_states(cmd, draw);

        if std::env::var_os("RUZU_TRACE_VK_DRAW").is_some() {
            let rt0 = draw.render_targets[0];
            if std::env::var_os("RUZU_TRACE_VK_SHADER_INFO").is_some() {
                let vs_info = stage_infos
                    .get(1)
                    .and_then(|info| info.as_ref())
                    .or_else(|| stage_infos.get(0).and_then(|info| info.as_ref()));
                let fs_info = stage_infos.get(4).and_then(|info| info.as_ref());
                let vs_stage = draw.shader_stages[1];
                let fs_stage = draw.shader_stages[5];
                log::info!(
                    "[VK_SHADER_INFO] draw={} rt0_addr=0x{:X} base=0x{:X} vs_enabled={} vs_offset=0x{:X} vs_addr=0x{:X} fs_enabled={} fs_offset=0x{:X} fs_addr=0x{:X} vs_cbufs={} vs_textures={} vs_ssbos={} fs_cbufs={} fs_textures={} fs_ssbos={} fs_stores={} fs_uses_demote={} fs_uses_render_area={} fs_uses_rescaling={}",
                    self.draw_counter,
                    rt0.address,
                    draw.program_base_address,
                    vs_stage.enabled,
                    vs_stage.offset,
                    draw.program_base_address + vs_stage.offset as u64,
                    fs_stage.enabled,
                    fs_stage.offset,
                    draw.program_base_address + fs_stage.offset as u64,
                    vs_info.map(|info| info.constant_buffer_descriptors.len()).unwrap_or(0),
                    vs_info.map(|info| info.texture_descriptors.len()).unwrap_or(0),
                    vs_info.map(|info| info.storage_buffers_descriptors.len()).unwrap_or(0),
                    fs_info.map(|info| info.constant_buffer_descriptors.len()).unwrap_or(0),
                    fs_info.map(|info| info.texture_descriptors.len()).unwrap_or(0),
                    fs_info.map(|info| info.storage_buffers_descriptors.len()).unwrap_or(0),
                    fs_info.map(|info| info.stores_frag_color.iter().filter(|&&v| v).count()).unwrap_or(0),
                    fs_info.map(|info| info.uses_demote_to_helper_invocation).unwrap_or(false),
                    fs_info.map(|info| info.uses_render_area).unwrap_or(false),
                    fs_info.map(|info| info.uses_rescaling_uniform).unwrap_or(false),
                );
            }
            log::info!(
                "[VK_DRAW] draw={} vp=({:.0},{:.0} {:.0}x{:.0}) topology={:?} guest_indexed={} vk_indexed={} vertices={} instances={} first_index={} base_vertex={} base_instance={} vertex_first={} vertex_count={} index_first={} index_count={} rt_count={} rt0_addr=0x{:X} rt0={}x{} fmt={} zeta_enable={} zeta_addr=0x{:X} zeta={}x{} zeta_fmt={} depth_mode={:?} depth_test={} depth_write={} depth_func={:?} stencil={} prim_restart={} rasterize={} cull_enable={} cull_face={:?} front_face={:?} window_flip_y={} uses_render_area={} uses_rescaling_uniform={} logic_op={} blend0={} color=({:?},{:?},{:?}) alpha=({:?},{:?},{:?}) mask={:?}",
                self.draw_counter,
                {
                    let vp = viewport_state(draw, 0);
                    vp.x
                },
                viewport_state(draw, 0).y,
                viewport_state(draw, 0).width,
                viewport_state(draw, 0).height,
                draw.topology,
                draw.indexed,
                draw_params.is_indexed,
                draw_params.num_vertices,
                draw_params.num_instances,
                draw_params.first_index,
                draw_params.base_vertex,
                draw_params.base_instance,
                draw.vertex_first,
                draw.vertex_count,
                draw.index_buffer_first,
                draw.index_buffer_count,
                draw.rt_control.count,
                rt0.address,
                rt0.width,
                rt0.height,
                rt0.format,
                draw.zeta.enabled,
                draw.zeta.address,
                draw.zeta.width,
                draw.zeta.height,
                draw.zeta.format,
                draw.depth_stencil.depth_mode,
                draw.depth_stencil.depth_test_enable,
                draw.depth_stencil.depth_write_enable,
                draw.depth_stencil.depth_func,
                draw.depth_stencil.stencil_enable,
                draw.primitive_restart.enabled,
                draw.rasterize_enable,
                draw.rasterizer.cull_enable,
                draw.rasterizer.cull_face,
                draw.rasterizer.front_face,
                draw.window_origin_flip_y,
                uses_render_area,
                uses_rescaling_uniform,
                draw.logic_op.enabled,
                draw.blend[0].enabled,
                draw.blend[0].color_src,
                draw.blend[0].color_dst,
                draw.blend[0].color_op,
                draw.blend[0].alpha_src,
                draw.blend[0].alpha_dst,
                draw.blend[0].alpha_op,
                draw.color_masks[0],
            );
        }

        // 7. Issue draw call
        if should_skip_debug_draw(draw.render_targets[0].address, self.draw_counter) {
            log::warn!(
                "[VK_DRAW_SKIP_DEBUG] draw={} rt0_addr=0x{:X}",
                self.draw_counter,
                draw.render_targets[0].address
            );
            self.texture_cache.dump_image_if_requested();
            self.draw_counter += 1;
            self.state_tracker.invalidate_state();
            return;
        }
        if draw_params.is_indexed {
            unsafe {
                self.device.cmd_draw_indexed(
                    cmd,
                    draw_params.num_vertices,
                    draw_params.num_instances,
                    draw_params.first_index,
                    draw_params.base_vertex,
                    draw_params.base_instance,
                );
            }
        } else {
            unsafe {
                self.device.cmd_draw(
                    cmd,
                    draw_params.num_vertices,
                    draw_params.num_instances,
                    draw_params.base_vertex as u32,
                    draw_params.base_instance,
                );
            }
        }

        self.texture_cache.dump_image_if_requested();
        self.draw_counter += 1;
        // Mark state dirty for next draw (conservative — real tracking per-register later)
        self.state_tracker.invalidate_state();
    }

    fn push_graphics_push_constants(
        &self,
        cmd: vk::CommandBuffer,
        pipeline_layout: vk::PipelineLayout,
        draw: &DrawCall,
        uses_render_area: bool,
        uses_rescaling_uniform: bool,
    ) {
        let rescaling = RescalingPushConstant::new();
        unsafe {
            self.device.cmd_push_constants(
                cmd,
                pipeline_layout,
                vk::ShaderStageFlags::ALL_GRAPHICS,
                RESCALING_LAYOUT_WORDS_OFFSET,
                bytes_of(rescaling.data()),
            );
        }

        if uses_rescaling_uniform {
            let scale_down_factor = 1.0f32;
            unsafe {
                self.device.cmd_push_constants(
                    cmd,
                    pipeline_layout,
                    vk::ShaderStageFlags::ALL_GRAPHICS,
                    RESCALING_LAYOUT_DOWN_FACTOR_OFFSET,
                    bytes_of(&scale_down_factor),
                );
            }
        }

        if uses_render_area {
            let render_area = [
                draw.surface_clip.width as f32,
                draw.surface_clip.height as f32,
                0.0,
                0.0,
            ];
            unsafe {
                self.device.cmd_push_constants(
                    cmd,
                    pipeline_layout,
                    vk::ShaderStageFlags::ALL_GRAPHICS,
                    RENDERAREA_LAYOUT_OFFSET,
                    bytes_of(&render_area),
                );
            }
        }
    }

    /// Clear framebuffer.
    ///
    /// Upstream uses vkCmdClearAttachments with scissor rects, render target
    /// format introspection, and blit_image fallback for partial color masks.
    /// Full implementation requires: TextureCache (UpdateRenderTargets, GetFramebuffer,
    /// IsRescaling), QueryCache (NotifySegment, CounterEnable), Settings (resolution_info),
    /// Scheduler (RequestRenderpass, Record), and BlitImage (ClearColor).
    /// These subsystems are not yet wired in the Vulkan renderer.
    pub fn clear(&mut self, _draw: &DrawCall) {
        trace!("RasterizerVulkan: clear (stub — requires TextureCache/Scheduler/BlitImage integration)");
    }

    /// Periodic work dispatch (zuyu: 7 draws → dispatch, 4096 → flush).
    fn flush_work(&mut self) {
        if self.draw_counter > 0 && self.draw_counter % Self::DISPATCH_THRESHOLD == 0 {
            self.scheduler.dispatch_work();
        }
        if self.draw_counter >= Self::FLUSH_THRESHOLD {
            let tick = self.scheduler.flush();
            self.draw_counter = 0;
            self.state_tracker.invalidate_command_buffer_state();
            self.staging_pool.new_frame();
            // The freshly submitted work still references this frame's
            // descriptor sets; retire the pools against the submission tick
            // instead of resetting them under the GPU.
            let scheduler = &self.scheduler;
            self.descriptor_pool
                .end_frame(tick, &|t| scheduler.is_free(t));
        }
    }

    /// Submit and wait for all GPU work to complete.
    pub fn finish(&mut self) {
        // End render pass and submit
        self.scheduler.finish();
        self.draw_counter = 0;
        self.state_tracker.invalidate_command_buffer_state();
        self.staging_pool.new_frame();
        self.descriptor_pool.reset_pools();
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
        let cmd = self.scheduler.command_buffer();

        // Transition offscreen image for transfer
        self.texture_cache.transition_layout(
            cmd,
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
        unsafe {
            self.device.cmd_copy_image_to_buffer(
                cmd,
                self.offscreen_image,
                vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                self.readback_buffer,
                &[region],
            );
        }

        // Transition back
        self.texture_cache.transition_layout(
            cmd,
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
            self.draw(draw, &mut dirty_flags, read_gpu, &read_gpu_unsafe);
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

    fn update_dynamic_states(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        self.update_viewports(cmd, draw);
        self.update_scissors(cmd, draw);
        self.update_depth_bias(cmd, draw);
        self.update_blend_constants(cmd, draw);
        self.update_depth_bounds(cmd, draw);
        self.update_stencil_faces(cmd, draw);
        self.update_line_width(cmd, draw);
        if self.extended_dynamic_state_supported {
            self.update_cull_mode(cmd, draw);
            self.update_depth_compare_op(cmd, draw);
            self.update_front_face(cmd, draw);
            self.update_stencil_op(cmd, draw);

            if self.state_tracker.touch_state_enable() {
                self.update_depth_bounds_test_enable(cmd, draw);
                self.update_depth_test_enable(cmd, draw);
                self.update_depth_write_enable(cmd, draw);
                self.update_stencil_test_enable(cmd, draw);
                if self.extended_dynamic_state2_supported {
                    self.update_primitive_restart_enable(cmd, draw);
                    self.update_rasterizer_discard_enable(cmd, draw);
                    self.update_depth_bias_enable(cmd, draw);
                }
            }
        }
    }

    fn update_primitive_restart_enable(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_primitive_restart_enable() {
            return;
        }
        unsafe {
            self.device
                .cmd_set_primitive_restart_enable(cmd, draw.primitive_restart.enabled);
        }
    }

    fn update_cull_mode(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_cull_mode() {
            return;
        }
        let cull_mode = if draw.rasterizer.cull_enable {
            maxwell_to_vk::cull_face(draw.rasterizer.cull_face)
        } else {
            vk::CullModeFlags::NONE
        };
        unsafe {
            self.device.cmd_set_cull_mode(cmd, cull_mode);
        }
    }

    fn update_depth_bounds_test_enable(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_depth_bounds_test_enable() {
            return;
        }
        unsafe {
            self.device
                .cmd_set_depth_bounds_test_enable(cmd, draw.depth_bounds_enable);
        }
    }

    fn update_depth_test_enable(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_depth_test_enable() {
            return;
        }
        unsafe {
            self.device
                .cmd_set_depth_test_enable(cmd, draw.depth_stencil.depth_test_enable);
        }
    }

    fn update_depth_write_enable(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_depth_write_enable() {
            return;
        }
        unsafe {
            self.device
                .cmd_set_depth_write_enable(cmd, draw.depth_stencil.depth_write_enable);
        }
    }

    fn update_stencil_test_enable(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_stencil_test_enable() {
            return;
        }
        unsafe {
            self.device
                .cmd_set_stencil_test_enable(cmd, draw.depth_stencil.stencil_enable);
        }
    }

    fn update_rasterizer_discard_enable(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_rasterizer_discard_enable() {
            return;
        }
        unsafe {
            self.device
                .cmd_set_rasterizer_discard_enable(cmd, !draw.rasterize_enable);
        }
    }

    fn update_depth_bias_enable(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
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
        unsafe {
            self.device.cmd_set_depth_bias_enable(cmd, enabled);
        }
    }

    fn update_depth_compare_op(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_depth_compare_op() {
            return;
        }
        unsafe {
            self.device.cmd_set_depth_compare_op(
                cmd,
                maxwell_to_vk::comparison_op(draw.depth_stencil.depth_func),
            );
        }
    }

    fn update_front_face(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
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
        unsafe {
            self.device.cmd_set_front_face(cmd, front_face);
        }
    }

    fn update_stencil_op(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_stencil_op() {
            return;
        }
        let front = &draw.depth_stencil.front;
        unsafe {
            if draw.depth_stencil.stencil_two_side {
                let back = &draw.depth_stencil.back;
                self.device.cmd_set_stencil_op(
                    cmd,
                    vk::StencilFaceFlags::FRONT,
                    maxwell_to_vk::stencil_op(front.fail_op),
                    maxwell_to_vk::stencil_op(front.zpass_op),
                    maxwell_to_vk::stencil_op(front.zfail_op),
                    maxwell_to_vk::comparison_op(front.func),
                );
                self.device.cmd_set_stencil_op(
                    cmd,
                    vk::StencilFaceFlags::BACK,
                    maxwell_to_vk::stencil_op(back.fail_op),
                    maxwell_to_vk::stencil_op(back.zpass_op),
                    maxwell_to_vk::stencil_op(back.zfail_op),
                    maxwell_to_vk::comparison_op(back.func),
                );
            } else {
                self.device.cmd_set_stencil_op(
                    cmd,
                    vk::StencilFaceFlags::FRONT_AND_BACK,
                    maxwell_to_vk::stencil_op(front.fail_op),
                    maxwell_to_vk::stencil_op(front.zpass_op),
                    maxwell_to_vk::stencil_op(front.zfail_op),
                    maxwell_to_vk::comparison_op(front.func),
                );
            }
        }
    }

    fn update_viewports(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_viewports() {
            return;
        }
        let viewport = if !draw.viewport_scale_offset_enabled {
            vk::Viewport {
                x: draw.surface_clip.x as f32,
                y: draw.surface_clip.y as f32,
                width: (draw.surface_clip.width as f32).max(1.0),
                height: (draw.surface_clip.height as f32).max(1.0),
                min_depth: 0.0,
                max_depth: 1.0,
            }
        } else {
            viewport_state(draw, 0)
        };
        unsafe {
            if draw.viewport_scale_offset_enabled {
                let viewports = std::array::from_fn::<_, { NUM_VIEWPORTS }, _>(|index| {
                    viewport_state(draw, index)
                });
                self.device
                    .cmd_set_viewport(cmd, 0, &viewports[..self.max_viewports as usize]);
            } else {
                self.device.cmd_set_viewport(cmd, 0, &[viewport]);
            }
        }
        if std::env::var_os("RUZU_TRACE_VK_DYNAMIC_STATE").is_some() {
            log::info!(
                "[VK_DYNAMIC] viewport scale_offset={} x={} y={} w={} h={} min_depth={} max_depth={}",
                draw.viewport_scale_offset_enabled,
                viewport.x,
                viewport.y,
                viewport.width,
                viewport.height,
                viewport.min_depth,
                viewport.max_depth,
            );
        }
    }

    fn update_scissors(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
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
        unsafe {
            if draw.viewport_scale_offset_enabled {
                let scissors = std::array::from_fn::<_, { NUM_VIEWPORTS }, _>(|index| {
                    scissor_state(draw, index)
                });
                self.device
                    .cmd_set_scissor(cmd, 0, &scissors[..self.max_viewports as usize]);
            } else {
                self.device.cmd_set_scissor(cmd, 0, &[scissor]);
            }
        }
        if std::env::var_os("RUZU_TRACE_VK_DYNAMIC_STATE").is_some() {
            log::info!(
                "[VK_DYNAMIC] scissor scale_offset={} x={} y={} w={} h={}",
                draw.viewport_scale_offset_enabled,
                scissor.offset.x,
                scissor.offset.y,
                scissor.extent.width,
                scissor.extent.height,
            );
        }
    }

    fn update_depth_bias(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_depth_bias() {
            return;
        }
        unsafe {
            self.device.cmd_set_depth_bias(
                cmd,
                draw.rasterizer.depth_bias,
                draw.rasterizer.depth_bias_clamp,
                draw.rasterizer.slope_scale_depth_bias,
            );
        }
    }

    fn update_blend_constants(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_blend_constants() {
            return;
        }
        unsafe {
            self.device.cmd_set_blend_constants(
                cmd,
                &[
                    draw.blend_color.r,
                    draw.blend_color.g,
                    draw.blend_color.b,
                    draw.blend_color.a,
                ],
            );
        }
    }

    fn update_stencil_faces(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
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

        let front = &draw.depth_stencil.front;
        let back = &draw.depth_stencil.back;

        if update_references {
            unsafe {
                if draw.depth_stencil.stencil_two_side && front.ref_value != back.ref_value {
                    if self
                        .state_tracker
                        .check_stencil_reference_front(front.ref_value)
                    {
                        self.device.cmd_set_stencil_reference(
                            cmd,
                            vk::StencilFaceFlags::FRONT,
                            front.ref_value,
                        );
                    }
                    if self
                        .state_tracker
                        .check_stencil_reference_back(back.ref_value)
                    {
                        self.device.cmd_set_stencil_reference(
                            cmd,
                            vk::StencilFaceFlags::BACK,
                            back.ref_value,
                        );
                    }
                } else if self
                    .state_tracker
                    .check_stencil_reference_front(front.ref_value)
                {
                    self.device.cmd_set_stencil_reference(
                        cmd,
                        vk::StencilFaceFlags::FRONT_AND_BACK,
                        front.ref_value,
                    );
                }
            }
        }

        if update_write_mask {
            unsafe {
                if draw.depth_stencil.stencil_two_side && front.write_mask != back.write_mask {
                    if self
                        .state_tracker
                        .check_stencil_write_mask_front(front.write_mask)
                    {
                        self.device.cmd_set_stencil_write_mask(
                            cmd,
                            vk::StencilFaceFlags::FRONT,
                            front.write_mask,
                        );
                    }
                    if self
                        .state_tracker
                        .check_stencil_write_mask_back(back.write_mask)
                    {
                        self.device.cmd_set_stencil_write_mask(
                            cmd,
                            vk::StencilFaceFlags::BACK,
                            back.write_mask,
                        );
                    }
                } else if self
                    .state_tracker
                    .check_stencil_write_mask_front(front.write_mask)
                {
                    self.device.cmd_set_stencil_write_mask(
                        cmd,
                        vk::StencilFaceFlags::FRONT_AND_BACK,
                        front.write_mask,
                    );
                }
            }
        }

        if update_compare_masks {
            unsafe {
                if draw.depth_stencil.stencil_two_side && front.func_mask != back.func_mask {
                    if self
                        .state_tracker
                        .check_stencil_compare_mask_front(front.func_mask)
                    {
                        self.device.cmd_set_stencil_compare_mask(
                            cmd,
                            vk::StencilFaceFlags::FRONT,
                            front.func_mask,
                        );
                    }
                    if self
                        .state_tracker
                        .check_stencil_compare_mask_back(back.func_mask)
                    {
                        self.device.cmd_set_stencil_compare_mask(
                            cmd,
                            vk::StencilFaceFlags::BACK,
                            back.func_mask,
                        );
                    }
                } else if self
                    .state_tracker
                    .check_stencil_compare_mask_front(front.func_mask)
                {
                    self.device.cmd_set_stencil_compare_mask(
                        cmd,
                        vk::StencilFaceFlags::FRONT_AND_BACK,
                        front.func_mask,
                    );
                }
            }
        }

        self.state_tracker.clear_stencil_reset();
    }

    fn update_depth_bounds(&mut self, cmd: vk::CommandBuffer, _draw: &DrawCall) {
        if !self.state_tracker.touch_depth_bounds() {
            return;
        }
        // Depth bounds test not currently used, but set safe defaults
        unsafe {
            self.device.cmd_set_depth_bounds(cmd, 0.0, 1.0);
        }
    }

    fn update_line_width(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_line_width() {
            return;
        }
        unsafe {
            self.device
                .cmd_set_line_width(cmd, draw.rasterizer.line_width_smooth.max(1.0));
        }
    }

    // ── Buffer binding ────────────────────────────────────────────────────

    fn bind_vertex_buffers(
        &mut self,
        cmd: vk::CommandBuffer,
        draw: &DrawCall,
        read_gpu: &dyn Fn(u64, &mut [u8]),
    ) {
        let upload_cmd = self.scheduler.upload_command_buffer();
        for stream in &draw.vertex_streams {
            if !stream.enabled || stream.address == 0 {
                continue;
            }
            let limit = draw
                .vertex_stream_limits
                .get(stream.index as usize)
                .map(|limit| limit.address)
                .unwrap_or(0);
            let fallback_count = draw.vertex_first.saturating_add(draw.vertex_count).max(
                draw.index_buffer_first
                    .saturating_add(draw.index_buffer_count),
            );
            let fallback_size = (stream.stride as u64).saturating_mul(fallback_count as u64);
            let size = if limit >= stream.address {
                limit.saturating_sub(stream.address).saturating_add(1)
            } else {
                fallback_size
            };
            if size == 0 {
                continue;
            }
            let trace_vertex_input = std::env::var_os("RUZU_TRACE_VK_VERTEX_INPUT").is_some()
                && (self.draw_counter <= 3
                    || draw
                        .render_targets
                        .first()
                        .is_some_and(|rt| rt.width == 1920 && rt.height == 1080));
            if trace_vertex_input {
                let mut bytes = vec![0u8; size.min(128) as usize];
                read_gpu(stream.address, &mut bytes);
                log::info!(
                    "[VK_VERTEX_BUFFER] draw={} rt0=0x{:X} binding={} addr=0x{:X} stride={} frequency={} size={} first={} count={} bytes={:02X?}",
                    self.draw_counter,
                    draw.render_targets[0].address,
                    stream.index,
                    stream.address,
                    stream.stride,
                    stream.frequency,
                    size,
                    draw.vertex_first,
                    draw.vertex_count,
                    bytes
                );
            }
            self.buffer_cache.bind_vertex_buffer(
                cmd,
                stream.index,
                stream.address,
                size,
                stream.stride as vk::DeviceSize,
                self.extended_dynamic_state_supported,
                read_gpu,
                &mut self.staging_pool,
                upload_cmd,
            );
        }
    }

    fn bind_index_buffer(
        &mut self,
        cmd: vk::CommandBuffer,
        draw: &DrawCall,
        draw_params: DrawParams,
        read_gpu: &dyn Fn(u64, &mut [u8]),
    ) {
        let upload_cmd = self.scheduler.upload_command_buffer();
        if matches!(
            draw.topology,
            PrimitiveTopology::Quads | PrimitiveTopology::QuadStrip
        ) {
            if draw.indexed {
                self.buffer_cache.bind_quad_indexed_buffer(
                    cmd,
                    draw.topology,
                    draw.index_format,
                    draw.base_vertex,
                    draw.index_buffer_first,
                    draw.index_buffer_count,
                    draw.index_buffer_addr,
                    read_gpu,
                    &mut self.staging_pool,
                    upload_cmd,
                );
            } else {
                self.buffer_cache.bind_quad_index_buffer(
                    cmd,
                    draw.topology,
                    draw.vertex_first,
                    draw.vertex_count,
                    &mut self.staging_pool,
                    upload_cmd,
                );
            }
            return;
        }

        if draw.index_buffer_addr == 0 {
            return;
        }
        let index_size = match draw.index_format {
            crate::engines::maxwell_3d::IndexFormat::UnsignedByte => 1,
            crate::engines::maxwell_3d::IndexFormat::UnsignedShort => 2,
            crate::engines::maxwell_3d::IndexFormat::UnsignedInt => 4,
        };
        let size = (index_size
            * draw_params
                .first_index
                .saturating_add(draw_params.num_vertices)) as u64;
        let index_type = match draw.index_format {
            crate::engines::maxwell_3d::IndexFormat::UnsignedByte => vk::IndexType::UINT8_EXT,
            crate::engines::maxwell_3d::IndexFormat::UnsignedShort => vk::IndexType::UINT16,
            crate::engines::maxwell_3d::IndexFormat::UnsignedInt => vk::IndexType::UINT32,
        };
        let trace_vertex_input = std::env::var_os("RUZU_TRACE_VK_VERTEX_INPUT").is_some()
            && (self.draw_counter <= 3
                || draw
                    .render_targets
                    .first()
                    .is_some_and(|rt| rt.width == 1920 && rt.height == 1080));
        if trace_vertex_input {
            let mut bytes = vec![0u8; size.min(128) as usize];
            read_gpu(draw.index_buffer_addr, &mut bytes);
            log::info!(
                "[VK_INDEX_BUFFER] draw={} rt0=0x{:X} addr=0x{:X} format={:?} size={} first_index={} count={} bytes={:02X?}",
                self.draw_counter,
                draw.render_targets[0].address,
                draw.index_buffer_addr,
                draw.index_format,
                size,
                draw_params.first_index,
                draw_params.num_vertices,
                bytes,
            );
        }
        self.buffer_cache.bind_index_buffer(
            cmd,
            draw.index_buffer_addr,
            size,
            index_type,
            read_gpu,
            &mut self.staging_pool,
            upload_cmd,
        );
    }

    fn bind_graphics_descriptors(
        &mut self,
        cmd: vk::CommandBuffer,
        pipeline_layout: vk::PipelineLayout,
        descriptor_set_layout: vk::DescriptorSetLayout,
        descriptor_bindings: &[GraphicsDescriptorBinding],
        descriptor_bank_info: &DescriptorBankInfo,
        stage_infos: &[Option<ShaderInfo>; 5],
        enabled_uniform_buffer_masks: &[u32; crate::buffer_cache::buffer_cache_base::NUM_STAGES
             as usize],
        uniform_buffer_sizes: &crate::buffer_cache::buffer_cache_base::UniformBufferSizes,
        draw: &DrawCall,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
    ) {
        if descriptor_set_layout == vk::DescriptorSetLayout::null()
            || descriptor_bindings.is_empty()
        {
            return;
        }

        let texture_cache: *mut TextureCache = &mut self.texture_cache;
        unsafe {
            let _texture_lock = (*texture_cache).base.mutex.lock();
            (*texture_cache)
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
        }

        self.desc_queue.acquire();
        let descriptor_set = match self
            .descriptor_pool
            .allocate(descriptor_set_layout, descriptor_bank_info)
        {
            Ok(set) => set,
            Err(err) => {
                warn!("RasterizerVulkan: failed to allocate graphics descriptor set: {err:?}");
                return;
            }
        };

        let descriptor_count = descriptor_bindings
            .iter()
            .map(|binding| binding.descriptor_count as usize)
            .sum();
        let mut buffer_infos = Vec::with_capacity(descriptor_count);
        let mut image_infos = Vec::with_capacity(descriptor_count);
        let mut writes = Vec::with_capacity(descriptor_bindings.len());
        let upload_cmd = self.scheduler.upload_command_buffer();
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
         -> Option<u32> {
            let index_offset = element.checked_shl(size_shift.min(31))?;
            let cbuf = draw
                .cb_bindings
                .get(stage)
                .and_then(|stage| stage.get(cbuf_index as usize))?;
            if !cbuf.enabled || cbuf.address == 0 {
                return None;
            }
            let offset = cbuf_offset.checked_add(index_offset)?;
            if cbuf.size as u32 <= offset {
                return None;
            }
            let addr = cbuf.address.wrapping_add(offset as u64);
            if !has_secondary {
                return Some(read_u32(addr));
            }
            let secondary_cbuf = draw
                .cb_bindings
                .get(stage)
                .and_then(|stage| stage.get(secondary_cbuf_index as usize))?;
            if !secondary_cbuf.enabled || secondary_cbuf.address == 0 {
                return None;
            }
            let secondary_offset = secondary_cbuf_offset.checked_add(index_offset)?;
            if secondary_cbuf.size as u32 <= secondary_offset {
                return None;
            }
            let secondary_addr = secondary_cbuf.address.wrapping_add(secondary_offset as u64);
            Some(
                (read_u32(addr) << shift_left) | (read_u32(secondary_addr) << secondary_shift_left),
            )
        };
        let mut sampled_views = Vec::new();
        let mut sampled_samplers = Vec::new();
        let has_uniform_buffer_descriptors = descriptor_bindings
            .iter()
            .any(|binding| binding.descriptor_type == vk::DescriptorType::UNIFORM_BUFFER);
        for (stage, info) in stage_infos.iter().enumerate() {
            let Some(info) = info else {
                continue;
            };
            for desc in &info.texture_descriptors {
                for element in 0..desc.count {
                    let raw = read_stage_handle(
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
                    .unwrap_or(0);
                    let (tic_id, tsc_id) = texture_pair(raw, via_header_index);
                    if std::env::var_os("RUZU_TRACE_VK_TEXTURE_HANDLE").is_some() {
                        log::warn!(
                            "[VK_TEXTURE_HANDLE] stage={} elem={} type={:?} cbuf={} offset=0x{:X} raw=0x{:X} tic={} tsc={} via_header={}",
                            stage,
                            element,
                            desc.texture_type,
                            desc.cbuf_index,
                            desc.cbuf_offset.wrapping_add(element << desc.size_shift.min(31)),
                            raw,
                            tic_id,
                            tsc_id,
                            via_header_index,
                        );
                    }
                    let sampler_id = unsafe {
                        let _texture_lock = (*texture_cache).base.mutex.lock();
                        (*texture_cache).base.get_graphics_sampler_id(tsc_id)
                    };
                    sampled_views.push(ImageViewInOut {
                        index: tic_id,
                        blacklist: false,
                        id: NULL_IMAGE_VIEW_ID,
                    });
                    sampled_samplers.push(sampler_id);
                }
            }
        }
        unsafe {
            let _texture_lock = (*texture_cache).base.mutex.lock();
            (*texture_cache)
                .base
                .fill_graphics_image_views(&mut sampled_views, false);
        }
        let requires_feedback_barrier = unsafe {
            let _texture_lock = (*texture_cache).base.mutex.lock();
            (*texture_cache).base.check_feedback_loop(&sampled_views)
        };
        if requires_feedback_barrier {
            unsafe {
                (*texture_cache).barrier_feedback_loop();
            }
        }
        let mut common_uniform_buffer_infos = Vec::new();
        if has_uniform_buffer_descriptors {
            self.common_buffer_cache
                .set_uniform_buffers_state(enabled_uniform_buffer_masks, uniform_buffer_sizes);
            for stage in 0..enabled_uniform_buffer_masks
                .len()
                .min(draw.cb_bindings.len())
            {
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
            self.desc_queue.acquire();
            self.common_buffer_cache.update_graphics_buffers(false);
            for stage in 0..enabled_uniform_buffer_masks.len() {
                self.common_buffer_cache.bind_host_stage_buffers(stage);
            }
            common_uniform_buffer_infos.extend(self.desc_queue.update_data().iter().filter_map(
                |entry| match entry {
                    DescriptorUpdateEntry::Buffer(info) => Some(*info),
                    _ => None,
                },
            ));
        }
        let mut sampled_cursor = 0usize;
        let mut common_uniform_cursor = 0usize;

        for binding in descriptor_bindings {
            match binding.descriptor_type {
                vk::DescriptorType::UNIFORM_BUFFER | vk::DescriptorType::STORAGE_BUFFER => {
                    let start = buffer_infos.len();
                    for element in 0..binding.descriptor_count {
                        if binding.descriptor_type == vk::DescriptorType::UNIFORM_BUFFER {
                            if let Some(info) = common_uniform_buffer_infos
                                .get(common_uniform_cursor)
                                .copied()
                            {
                                common_uniform_cursor += 1;
                                buffer_infos.push(info);
                                continue;
                            }
                        }
                        let descriptor_info = binding
                            .uniform_stage
                            .zip(binding.uniform_index)
                            .and_then(|(stage, index)| {
                                let stage = stage as usize;
                                let index = index.saturating_add(element) as usize;
                                draw.cb_bindings
                                    .get(stage)
                                    .and_then(|stage_bindings| stage_bindings.get(index))
                            })
                            .and_then(|cbuf| {
                                if cbuf.enabled && cbuf.address != 0 && cbuf.size != 0 {
                                    Some((cbuf.address, cbuf.size as vk::DeviceSize))
                                } else {
                                    None
                                }
                            });
                        let (buffer, offset, range) = if let Some((gpu_va, size)) = descriptor_info
                        {
                            let size = size.min(0x10000).max(4);
                            let trace_cbuf_rt = std::env::var("RUZU_TRACE_VK_CBUF_RT")
                                .ok()
                                .and_then(|value| {
                                    u64::from_str_radix(value.trim_start_matches("0x"), 16).ok()
                                });
                            if std::env::var_os("RUZU_TRACE_VK_CBUF").is_some()
                                && trace_cbuf_rt
                                    .is_none_or(|target| target == draw.render_targets[0].address)
                            {
                                let stage = binding.uniform_stage.unwrap_or(u32::MAX);
                                let index = binding
                                    .uniform_index
                                    .unwrap_or(u32::MAX)
                                    .saturating_add(element);
                                let mut bytes = vec![0u8; size.min(0x80) as usize];
                                read_gpu(gpu_va, &mut bytes);
                                let mut floats = Vec::with_capacity(bytes.len() / 4);
                                for chunk in bytes.chunks_exact(4) {
                                    floats.push(f32::from_le_bytes([
                                        chunk[0], chunk[1], chunk[2], chunk[3],
                                    ]));
                                }
                                log::info!(
                                    "[VK_CBUF] draw={} rt0_addr=0x{:X} binding={} stage={} index={} addr=0x{:X} size={} f32={:?}",
                                    self.draw_counter,
                                    draw.render_targets[0].address,
                                    binding.binding,
                                    stage,
                                    index,
                                    gpu_va,
                                    size,
                                    floats
                                );
                            }
                            let (buffer, offset) =
                                if binding.descriptor_type == vk::DescriptorType::UNIFORM_BUFFER {
                                    self.buffer_cache
                                        .bind_mapped_uniform_buffer(
                                            gpu_va,
                                            size,
                                            read_gpu,
                                            &mut self.staging_pool,
                                        )
                                        .unwrap_or_else(|| {
                                            self.buffer_cache.get_or_upload_fresh(
                                                gpu_va,
                                                size,
                                                read_gpu,
                                                &mut self.staging_pool,
                                                upload_cmd,
                                            )
                                        })
                                } else {
                                    self.buffer_cache.get_or_upload(
                                        gpu_va,
                                        size,
                                        read_gpu,
                                        &mut self.staging_pool,
                                        upload_cmd,
                                    )
                                };
                            (buffer, offset, size)
                        } else {
                            (self.fallback_uniform_buffer, 0, 0x10000)
                        };
                        buffer_infos.push(vk::DescriptorBufferInfo {
                            buffer,
                            offset,
                            range,
                        });
                    }
                    writes.push(
                        vk::WriteDescriptorSet::builder()
                            .dst_set(descriptor_set)
                            .dst_binding(binding.binding)
                            .descriptor_type(binding.descriptor_type)
                            .buffer_info(&buffer_infos[start..])
                            .build(),
                    );
                }
                vk::DescriptorType::COMBINED_IMAGE_SAMPLER | vk::DescriptorType::STORAGE_IMAGE => {
                    let start = image_infos.len();
                    if binding.descriptor_type == vk::DescriptorType::COMBINED_IMAGE_SAMPLER {
                        for element in 0..binding.descriptor_count as usize {
                            let sampled_index = sampled_cursor + element;
                            let view_id = sampled_views
                                .get(sampled_index)
                                .map(|view| view.id)
                                .unwrap_or(NULL_IMAGE_VIEW_ID);
                            let image_view = self
                                .texture_cache
                                .materialize_sampled_image_view(
                                    view_id,
                                    binding
                                        .texture
                                        .map(|texture| texture.texture_type)
                                        .unwrap_or(
                                            shader_recompiler::shader_info::TextureType::Color2D,
                                        ),
                                    read_gpu_unsafe,
                                    cmd,
                                )
                                .unwrap_or(self.offscreen_view);
                            let sampler_id = sampled_samplers
                                .get(sampled_index)
                                .copied()
                                .unwrap_or_default();
                            let sampler = self
                                .texture_cache
                                .sampler_handle(sampler_id)
                                .unwrap_or(self.fallback_sampler);
                            let trace_texture_state =
                                std::env::var_os("RUZU_TRACE_VK_TEXTURE_STATE").is_some();
                            let trace_texture_rt = std::env::var("RUZU_TRACE_VK_TEXTURE_STATE_RT")
                                .ok()
                                .and_then(|value| {
                                    u64::from_str_radix(value.trim_start_matches("0x"), 16).ok()
                                });
                            if trace_texture_state
                                && trace_texture_rt
                                    .is_none_or(|target| target == draw.render_targets[0].address)
                            {
                                if view_id.is_valid() && view_id != NULL_IMAGE_VIEW_ID {
                                    let view =
                                        self.texture_cache.base.slot_image_views.get(view_id);
                                    let image =
                                        self.texture_cache.base.slot_images.get(view.image_id);
                                    log::warn!(
                                        "[VK_TEXTURE_STATE] draw={} rt0_addr=0x{:X} binding={} elem={} sampled_index={} view_id={} sampler_id={} image_id={} view_gpu=0x{:X} view_fmt={:?} view_type={:?} view_range={:?} view_swizzle={:?} view_size={:?} view_flags={:?} image_type={:?} image_fmt={:?} image_size={:?} image_gpu=0x{:X} image_cpu=0x{:X} guest_size={} unswizzled_size={} image_flags={:?}",
                                        self.draw_counter,
                                        draw.render_targets[0].address,
                                        binding.binding,
                                        element,
                                        sampled_index,
                                        view_id.index,
                                        sampler_id.index,
                                        view.image_id.index,
                                        view.gpu_addr,
                                        view.format,
                                        view.view_type,
                                        view.range,
                                        view.swizzle,
                                        view.size,
                                        view.flags,
                                        image.info.image_type,
                                        image.info.format,
                                        image.info.size,
                                        image.gpu_addr,
                                        image.cpu_addr,
                                        image.guest_size_bytes,
                                        image.unswizzled_size_bytes,
                                        image.flags,
                                    );
                                } else {
                                    log::warn!(
                                        "[VK_TEXTURE_STATE] draw={} rt0_addr=0x{:X} binding={} elem={} sampled_index={} view_id={} sampler_id={} null_view",
                                        self.draw_counter,
                                        draw.render_targets[0].address,
                                        binding.binding,
                                        element,
                                        sampled_index,
                                        view_id.index,
                                        sampler_id.index,
                                    );
                                }
                                if sampler_id.is_valid() {
                                    let tsc = self.texture_cache.base.slot_samplers.get(sampler_id);
                                    log::warn!(
                                        "[VK_TEXTURE_SAMPLER] draw={} rt0_addr=0x{:X} binding={} elem={} sampler_id={} raw={:016X?} wrap=({},{},{}) filter=({},{},{}) depth_cmp={} depth_func={} lod=({:.3},{:.3}) bias={:.3}",
                                        self.draw_counter,
                                        draw.render_targets[0].address,
                                        binding.binding,
                                        element,
                                        sampler_id.index,
                                        tsc.raw,
                                        tsc.wrap_u(),
                                        tsc.wrap_v(),
                                        tsc.wrap_p(),
                                        tsc.mag_filter(),
                                        tsc.min_filter(),
                                        tsc.mipmap_filter(),
                                        tsc.depth_compare_enabled(),
                                        tsc.depth_compare_func(),
                                        tsc.min_lod(),
                                        tsc.max_lod(),
                                        tsc.lod_bias(),
                                    );
                                }
                            }
                            if std::env::var_os("RUZU_TRACE_VK_TEXTURE_BIND").is_some() {
                                log::warn!(
                                    "[VK_TEXTURE_BIND] binding={} elem={} view_id={} sampler_id={} vk_view=0x{:X} fallback_view={} fallback_sampler={}",
                                    binding.binding,
                                    element,
                                    view_id.index,
                                    sampler_id.index,
                                    image_view.as_raw(),
                                    image_view == self.offscreen_view,
                                    sampler == self.fallback_sampler,
                                );
                            }
                            image_infos.push(vk::DescriptorImageInfo {
                                sampler,
                                image_view,
                                image_layout: vk::ImageLayout::GENERAL,
                            });
                        }
                        sampled_cursor =
                            sampled_cursor.saturating_add(binding.descriptor_count as usize);
                    } else {
                        for _ in 0..binding.descriptor_count {
                            image_infos.push(vk::DescriptorImageInfo {
                                sampler: vk::Sampler::null(),
                                image_view: self.offscreen_view,
                                image_layout: vk::ImageLayout::GENERAL,
                            });
                        }
                    }
                    writes.push(
                        vk::WriteDescriptorSet::builder()
                            .dst_set(descriptor_set)
                            .dst_binding(binding.binding)
                            .descriptor_type(binding.descriptor_type)
                            .image_info(&image_infos[start..])
                            .build(),
                    );
                }
                _ => {
                    warn!(
                        "RasterizerVulkan: unsupported graphics descriptor type {:?}",
                        binding.descriptor_type
                    );
                }
            }
        }

        unsafe {
            self.device.update_descriptor_sets(&writes, &[]);
            self.device.cmd_bind_descriptor_sets(
                cmd,
                vk::PipelineBindPoint::GRAPHICS,
                pipeline_layout,
                0,
                &[descriptor_set],
                &[],
            );
        }
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
        if std::env::var_os("RUZU_DISABLE_VK_ACCELERATE_DISPLAY").is_some() {
            if std::env::var_os("RUZU_TRACE_VK_PRESENT").is_some() {
                log::info!(
                    "[VK_PRESENT] AccelerateDisplay disabled addr=0x{:X} {}x{} stride={}",
                    framebuffer_addr,
                    config.width,
                    config.height,
                    config.stride,
                );
            }
            return None;
        }
        if framebuffer_addr == 0 {
            return None;
        }
        let texture_cache: *mut TextureCache = &mut self.texture_cache;
        let framebuffer_view = unsafe {
            let _texture_lock = (*texture_cache).base.mutex.lock();
            (*texture_cache).try_find_framebuffer_image_view(config, framebuffer_addr)
        };
        let Some(framebuffer_view) = framebuffer_view else {
            if std::env::var_os("RUZU_TRACE_VK_PRESENT").is_some() {
                log::info!(
                    "[VK_PRESENT] AccelerateDisplay miss addr=0x{:X} {}x{} stride={}",
                    framebuffer_addr,
                    config.width,
                    config.height,
                    config.stride,
                );
            }
            return None;
        };
        let cmd = self.scheduler.command_buffer();
        self.texture_cache
            .prepare_framebuffer_for_present(framebuffer_addr, cmd);
        if std::env::var_os("RUZU_TRACE_VK_PRESENT").is_some() {
            log::info!(
                "[VK_PRESENT] AccelerateDisplay hit addr=0x{:X} view={} view_fmt={:?} view_swizzle={:?} image=0x{:X} image_view=0x{:X} size={}x{} scaled={}",
                framebuffer_addr,
                framebuffer_view.common.view_id.index,
                framebuffer_view.common.view.format,
                framebuffer_view.common.view.swizzle,
                framebuffer_view.image.as_raw(),
                framebuffer_view.image_view.as_raw(),
                framebuffer_view.width,
                framebuffer_view.height,
                framebuffer_view.common.scaled,
            );
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
    fn load_disk_resources(&mut self, title_id: u64) {
        let shader_dir =
            common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::ShaderDir);
        self.pipeline_cache
            .load_disk_resources(title_id, &shader_dir);
    }

    fn draw(
        &mut self,
        mut draw_view: crate::engines::draw_manager::Maxwell3DDrawView<'_>,
        instance_count: u32,
    ) {
        // Upstream `RasterizerVulkan::PrepareDraw` flushes cached GPU-memory
        // writes before descriptors/textures are consumed by the draw.
        if let Some(mm) = self.channel_memory_manager.as_ref().cloned() {
            mm.lock().flush_caching();
        }
        let draw_state = draw_view.draw_state();
        self.draw_counter = self.draw_counter.wrapping_add(1);
        if std::env::var_os("RUZU_TRACE_VK_DRAW_STUB").is_some() {
            let render_targets = draw_view.render_targets();
            let rt0 = render_targets.render_targets[0];
            log::info!(
                "[VK_DRAW_STUB] #{} indexed={} instances={} topology={:?} vb_first={} vb_count={} ib_first={} ib_count={} rt0=0x{:X} {}x{} fmt=0x{:X}",
                self.draw_counter,
                draw_state.draw_indexed,
                instance_count,
                draw_state.topology,
                draw_state.vertex_buffer.first,
                draw_state.vertex_buffer.count,
                draw_state.index_buffer.first,
                draw_state.index_buffer.count,
                rt0.address,
                rt0.width,
                rt0.height,
                rt0.format,
            );
        }
        debug!(
            "RasterizerVulkan::draw indexed={} instances={}",
            draw_state.draw_indexed, instance_count
        );
        let Some(memory_manager) = self.channel_memory_manager.as_ref().cloned() else {
            warn!("RasterizerVulkan::draw skipped: no bound channel memory manager");
            return;
        };
        let draw_call = draw_view.draw_call_snapshot(draw_state.draw_indexed, instance_count);
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
        self.draw(&draw_call, &mut dirty_flags, &read_gpu, &read_gpu_unsafe);
        // Upstream backends consume `maxwell3d->dirty.flags` in place (e.g.
        // TextureCache::UpdateRenderTargets clears Dirty::RenderTargets on the
        // live engine). The snapshot in `draw_call` is a copy, so propagate
        // the flags the backend consumed back to the engine — otherwise every
        // draw re-runs the full render-target resolution.
        for (index, dirty) in dirty_flags.iter().enumerate() {
            if !dirty && draw_call.dirty_flags[index] {
                draw_view.clear_dirty_flag(index as u8);
            }
        }
    }

    fn draw_texture(&mut self) {
        debug!("RasterizerVulkan::draw_texture");
    }

    fn clear(
        &mut self,
        mut clear_view: crate::engines::draw_manager::Maxwell3DClearView<'_>,
        layer_count: u32,
    ) {
        // Upstream `RasterizerVulkan::Clear` starts with
        // `gpu_memory->FlushCaching()`.
        if let Some(mm) = self.channel_memory_manager.as_ref().cloned() {
            mm.lock().flush_caching();
        }
        self.flush_work();

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
        let cmd = self.scheduler.command_buffer();
        self.texture_cache
            .prepare_render_targets_for_render(&target.image_ids, cmd);
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
        if std::env::var_os("RUZU_TRACE_VK_CLEAR").is_some() {
            let rt_index = (clear_state.flags >> 6) & 0xF;
            log::info!(
                "[VK_CLEAR] flags=0x{:X} rt={} color={} depth={} stencil={} rgba=({:.4},{:.4},{:.4},{:.4}) ds=({:.4},{}) rect=({},{} {}x{}) target_rt0=0x{:X}",
                clear_state.flags,
                rt_index,
                use_color,
                use_depth,
                use_stencil,
                clear_state.color[0],
                clear_state.color[1],
                clear_state.color[2],
                clear_state.color[3],
                clear_state.depth,
                clear_state.stencil,
                clear_rect.rect.offset.x,
                clear_rect.rect.offset.y,
                clear_rect.rect.extent.width,
                clear_rect.rect.extent.height,
                render_targets.render_targets[0].address,
            );
        }
        let mut attachments = Vec::with_capacity(2);
        if use_color {
            if (clear_state.flags >> 6) & 0xF != 0 {
                log::debug!(
                    "RasterizerVulkan::clear skipping non-RT0 color attachment flags=0x{:X}",
                    clear_state.flags
                );
            } else {
                attachments.push(vk::ClearAttachment {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    color_attachment: 0,
                    clear_value: vk::ClearValue {
                        color: vk::ClearColorValue {
                            float32: clear_state.color,
                        },
                    },
                });
            }
        }
        let mut depth_stencil_aspects = vk::ImageAspectFlags::empty();
        if target.has_depth && use_depth {
            depth_stencil_aspects |= vk::ImageAspectFlags::DEPTH;
        }
        if target.has_depth && use_stencil {
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

        let cmd = self.scheduler.command_buffer();
        unsafe {
            self.device
                .cmd_clear_attachments(cmd, &attachments, &[clear_rect]);
        }
        self.scheduler.request_outside_renderpass();
        if use_r
            && use_g
            && use_b
            && use_a
            && (clear_state.flags >> 6) & 0xF == 0
            && !clear_view.use_scissor()
        {
            if let Some(rt_image) = self.texture_cache.rt0_image_info(&render_targets) {
                let cmd = self.scheduler.command_buffer();
                let to_transfer = vk::ImageMemoryBarrier::builder()
                    .src_access_mask(vk::AccessFlags::COLOR_ATTACHMENT_WRITE)
                    .dst_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                    .old_layout(vk::ImageLayout::GENERAL)
                    .new_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                    .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .image(rt_image.image)
                    .subresource_range(vk::ImageSubresourceRange {
                        aspect_mask: vk::ImageAspectFlags::COLOR,
                        base_mip_level: 0,
                        level_count: 1,
                        base_array_layer: 0,
                        layer_count: 1,
                    })
                    .build();
                let to_color_attachment = vk::ImageMemoryBarrier::builder()
                    .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                    .dst_access_mask(vk::AccessFlags::COLOR_ATTACHMENT_WRITE)
                    .old_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                    .new_layout(vk::ImageLayout::GENERAL)
                    .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .image(rt_image.image)
                    .subresource_range(vk::ImageSubresourceRange {
                        aspect_mask: vk::ImageAspectFlags::COLOR,
                        base_mip_level: 0,
                        level_count: 1,
                        base_array_layer: 0,
                        layer_count: 1,
                    })
                    .build();
                let range = vk::ImageSubresourceRange {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    base_mip_level: 0,
                    level_count: 1,
                    base_array_layer: 0,
                    layer_count: 1,
                };
                unsafe {
                    self.device.cmd_pipeline_barrier(
                        cmd,
                        vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                        vk::PipelineStageFlags::TRANSFER,
                        vk::DependencyFlags::empty(),
                        &[],
                        &[],
                        &[to_transfer],
                    );
                    self.device.cmd_clear_color_image(
                        cmd,
                        rt_image.image,
                        vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                        &vk::ClearColorValue {
                            float32: clear_state.color,
                        },
                        &[range],
                    );
                    self.device.cmd_pipeline_barrier(
                        cmd,
                        vk::PipelineStageFlags::TRANSFER,
                        vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                        vk::DependencyFlags::empty(),
                        &[],
                        &[],
                        &[to_color_attachment],
                    );
                }
                if std::env::var_os("RUZU_TRACE_VK_CLEAR").is_some() {
                    log::info!(
                        "[VK_CLEAR_IMAGE] color={:?} image=0x{:X} size={}x{}",
                        clear_state.color,
                        rt_image.image.as_raw(),
                        rt_image.width,
                        rt_image.height,
                    );
                }
            }
        }
        if std::env::var_os("RUZU_TRACE_VK_CLEAR").is_some() {
            log::info!(
                "[VK_CLEAR] flags=0x{:X} color={:?} depth={} stencil={} area={}x{} rect=({}, {}) {}x{} scissor={} layers={}",
                clear_state.flags,
                clear_state.color,
                clear_state.depth,
                clear_state.stencil,
                render_area.width,
                render_area.height,
                clear_rect.rect.offset.x,
                clear_rect.rect.offset.y,
                clear_rect.rect.extent.width,
                clear_rect.rect.extent.height,
                clear_view.use_scissor(),
                layer_count,
            );
        }
    }

    fn dispatch_compute(&mut self) {
        debug!("RasterizerVulkan::dispatch_compute");
    }

    fn dispatch_compute_with_call(&mut self, dispatch: &DispatchCall) {
        self.flush_work();
        if let Some(mm) = self.channel_memory_manager.as_ref().cloned() {
            mm.lock().flush_caching();
        }

        let Some(current_pipeline) = self
            .pipeline_cache
            .current_compute_pipeline_with_shared_cache(&mut self.shader_cache)
        else {
            if std::env::var_os("RUZU_TRACE_COMPUTE").is_some() {
                log::warn!(
                    "[VK_COMPUTE] skipped: no pipeline grid=({},{},{}) block=({},{},{}) qmd=0x{:X} code=0x{:X}",
                    dispatch.qmd.grid_dim_x,
                    dispatch.qmd.grid_dim_y,
                    dispatch.qmd.grid_dim_z,
                    dispatch.qmd.block_dim_x,
                    dispatch.qmd.block_dim_y,
                    dispatch.qmd.block_dim_z,
                    dispatch.qmd_address,
                    dispatch.code_address,
                );
            }
            return;
        };

        if current_pipeline.requires_descriptor_binding {
            if std::env::var_os("RUZU_TRACE_COMPUTE").is_some() {
                log::warn!(
                    "[VK_COMPUTE] skipped dispatch until ComputePipeline::Configure is ported: grid=({},{},{}) block=({},{},{}) qmd=0x{:X} code=0x{:X} indirect={:?}",
                    dispatch.qmd.grid_dim_x,
                    dispatch.qmd.grid_dim_y,
                    dispatch.qmd.grid_dim_z,
                    dispatch.qmd.block_dim_x,
                    dispatch.qmd.block_dim_y,
                    dispatch.qmd.block_dim_z,
                    dispatch.qmd_address,
                    dispatch.code_address,
                    dispatch.indirect_compute_address,
                );
            }
            return;
        }

        if let Some(indirect_address) = dispatch.indirect_compute_address {
            if std::env::var_os("RUZU_TRACE_COMPUTE").is_some() {
                log::warn!(
                    "[VK_COMPUTE] indirect dispatch skipped until Vulkan compute indirect buffer binding is ported: addr=0x{:X}",
                    indirect_address
                );
            }
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
        if std::env::var_os("RUZU_TRACE_COMPUTE").is_some() {
            log::info!(
                "[VK_COMPUTE] dispatch grid=({},{},{}) descriptor_free=true",
                dim[0],
                dim[1],
                dim[2],
            );
        }
    }

    fn reset_counter(&mut self, query_type: u32) {
        if query_type != QueryType::ZPassPixelCount64 as u32 {
            debug!(
                "RasterizerVulkan::reset_counter unimplemented counter reset={}",
                query_type
            );
            return;
        }
        self.query_cache.reset_counter(query_type);
    }

    fn query(
        &mut self,
        gpu_addr: u64,
        _query_type: u32,
        flags: QueryPropertiesFlags,
        payload: u32,
        _subreport: u32,
    ) {
        let this = self as *mut Self;
        self.query_cache.query(
            gpu_addr,
            flags,
            payload,
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

    fn must_flush_region(&self, _addr: u64, _size: u64) -> bool {
        false
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
        self.shader_cache.invalidate_region(addr, size as usize);
        self.query_cache.invalidate_region(addr, size as usize);
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
        self.shader_cache.on_cache_invalidation(addr, size as usize);
    }

    fn on_cpu_write(&mut self, addr: u64, size: u64) -> bool {
        if addr == 0 || size == 0 {
            return false;
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

    fn invalidate_gpu_cache(&mut self) {}

    fn unmap_memory(&mut self, _addr: u64, _size: u64) {}

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
        self.scheduler.dispatch_work();
    }

    fn tiled_cache_barrier(&mut self) {
        self.scheduler.dispatch_work();
    }

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
        {
            let tick = self.scheduler.current_tick();
            let scheduler = &self.scheduler;
            self.descriptor_pool
                .end_frame(tick, &|t| scheduler.is_free(t));
        }
        // Retire delayed-destruction rings against GPU completion, not the
        // submission counter (pipelined submissions run ahead of the GPU).
        let known_gpu_tick = self.scheduler.known_gpu_tick();
        self.texture_cache.tick_frame(known_gpu_tick);
        self.buffer_cache.tick_frame(known_gpu_tick);
        unsafe {
            let _lo_buf = common::lock_order::guard("buffer_cache");
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.common_buffer_cache.tick_frame();
        }
    }

    fn initialize_channel(&mut self, channel: &crate::control::channel_state::ChannelState) {
        self.channel_caches.create_channel(channel);
        self.texture_cache.create_channel(channel);
        self.buffer_cache.create_channel(channel);
        self.shader_cache.create_channel(channel);
        self.pipeline_cache.create_channel(channel);
        self.query_cache.create_channel(channel);
        self.state_tracker.setup_tables(channel);
    }

    fn bind_channel(&mut self, channel: &crate::control::channel_state::ChannelState) {
        self.channel_caches.bind_to_channel(channel.bind_id);
        self.texture_cache.bind_to_channel(channel.bind_id);
        self.buffer_cache.bind_to_channel(channel.bind_id);
        self.shader_cache.bind_to_channel(channel.bind_id);
        self.pipeline_cache.bind_to_channel(channel.bind_id);
        self.query_cache.bind_to_channel(channel.bind_id);
        self.state_tracker.change_channel(channel);
        self.state_tracker.invalidate_state();
        self.channel_memory_manager = self
            .channel_caches
            .current_channel_state()
            .and_then(ChannelCacheAccessor::gpu_memory_arc);
        if self.common_buffer_cache.channel_state.is_none() {
            self.common_buffer_cache.channel_state = Some(Box::default());
        }
        if let Some(mm) = self.channel_memory_manager.as_ref() {
            self.common_buffer_cache
                .set_gpu_memory(Box::new(GpuMemoryAccessAdapter { mm: Arc::clone(mm) }));
        }
    }

    fn release_channel(&mut self, channel_id: i32) {
        self.channel_caches.erase_channel(channel_id);
        self.texture_cache.erase_channel(channel_id);
        self.buffer_cache.erase_channel(channel_id);
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

    fn accelerate_dma_image_to_buffer(
        &mut self,
        copy_info: &dma::ImageCopy,
        src: &dma::ImageOperand,
        dst: &dma::BufferOperand,
    ) -> bool {
        let trace_dma = std::env::var_os("RUZU_TRACE_DMA_IMAGE").is_some();
        let Some(mm) = self.channel_memory_manager.as_ref().cloned() else {
            if trace_dma {
                info!(
                    "[DMA_IMAGE] vk image->buffer missing channel memory src=0x{:X} dst=0x{:X} len={}x{}",
                    src.address, dst.address, copy_info.length_x, copy_info.length_y
                );
            }
            return false;
        };
        unsafe {
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            lock_two_reentrant_mutexes!(buffer_mutex, texture_mutex, _buffer_guard, _texture_guard);

            let image_id = self.texture_cache.base.dma_image_id(src, false);
            if image_id == NULL_IMAGE_ID {
                if trace_dma {
                    info!(
                        "[DMA_IMAGE] vk image->buffer no image src=0x{:X} dst=0x{:X} len={}x{}",
                        src.address, dst.address, copy_info.length_x, copy_info.length_y
                    );
                }
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
                if trace_dma {
                    info!(
                        "[DMA_IMAGE] vk image->buffer null buffer image_id={} buffer_id={} src=0x{:X} dst=0x{:X}",
                        image_id.index, buffer_id.index, src.address, dst.address
                    );
                }
                return false;
            }

            let read_gpu_unsafe =
                |gpu_addr: u64, out: &mut [u8]| mm.lock().read_block_unsafe(gpu_addr, out);
            let copied = self.texture_cache.dma_buffer_image_copy(
                copy_info,
                dst,
                src,
                image_id,
                buffer,
                offset as vk::DeviceSize,
                false,
                &read_gpu_unsafe,
            );
            if trace_dma {
                info!(
                    "[DMA_IMAGE] vk image->buffer image_id={} buffer_id={} raw_buffer=0x{:X} offset={} size={} copied={}",
                    image_id.index,
                    buffer_id.index,
                    raw_buffer,
                    offset,
                    buffer_size,
                    copied
                );
            }
            copied
        }
    }

    fn accelerate_dma_buffer_to_image(
        &mut self,
        copy_info: &dma::ImageCopy,
        src: &dma::BufferOperand,
        dst: &dma::ImageOperand,
    ) -> bool {
        let trace_dma = std::env::var_os("RUZU_TRACE_DMA_IMAGE").is_some();
        let Some(mm) = self.channel_memory_manager.as_ref().cloned() else {
            if trace_dma {
                info!(
                    "[DMA_IMAGE] vk buffer->image missing channel memory src=0x{:X} dst=0x{:X} len={}x{}",
                    src.address, dst.address, copy_info.length_x, copy_info.length_y
                );
            }
            return false;
        };
        unsafe {
            let buffer_mutex: *const _ = &self.common_buffer_cache.mutex;
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            lock_two_reentrant_mutexes!(buffer_mutex, texture_mutex, _buffer_guard, _texture_guard);

            let image_id = self.texture_cache.base.dma_image_id(dst, true);
            if image_id == NULL_IMAGE_ID {
                if trace_dma {
                    info!(
                        "[DMA_IMAGE] vk buffer->image no image src=0x{:X} dst=0x{:X} len={}x{}",
                        src.address, dst.address, copy_info.length_x, copy_info.length_y
                    );
                }
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
                if trace_dma {
                    info!(
                        "[DMA_IMAGE] vk buffer->image null buffer image_id={} buffer_id={} src=0x{:X} dst=0x{:X}",
                        image_id.index, buffer_id.index, src.address, dst.address
                    );
                }
                return false;
            }

            let read_gpu_unsafe =
                |gpu_addr: u64, out: &mut [u8]| mm.lock().read_block_unsafe(gpu_addr, out);
            let copied = self.texture_cache.dma_buffer_image_copy(
                copy_info,
                src,
                dst,
                image_id,
                buffer,
                offset as vk::DeviceSize,
                true,
                &read_gpu_unsafe,
            );
            if trace_dma {
                info!(
                    "[DMA_IMAGE] vk buffer->image image_id={} buffer_id={} raw_buffer=0x{:X} offset={} size={} copied={}",
                    image_id.index,
                    buffer_id.index,
                    raw_buffer,
                    offset,
                    buffer_size,
                    copied
                );
            }
            copied
        }
    }

    fn accelerate_inline_to_memory(&mut self, address: u64, copy_size: usize, memory: &[u8]) {
        let copy_size = copy_size.min(memory.len());
        if copy_size == 0 {
            return;
        }

        if std::env::var_os("RUZU_TRACE_INLINE_TO_MEMORY").is_some() {
            info!(
                "RasterizerVulkan::accelerate_inline_to_memory enter gpu=0x{:X} size={} has_mm={}",
                address,
                copy_size,
                self.channel_memory_manager.is_some()
            );
        }

        let Some(mm) = self.channel_memory_manager.as_ref().cloned() else {
            if std::env::var_os("RUZU_TRACE_INLINE_TO_MEMORY").is_some() {
                info!(
                    "RasterizerVulkan::accelerate_inline_to_memory missing_channel_memory_manager gpu=0x{:X} size={}",
                    address, copy_size
                );
            }
            return;
        };

        let mm = mm.lock();
        let cpu_addr = mm.gpu_to_cpu_address(address);
        let input = &memory[..copy_size];
        if cpu_addr.is_none() {
            mm.write_block(address, input);
            if std::env::var_os("RUZU_TRACE_INLINE_TO_MEMORY").is_some() {
                info!(
                    "RasterizerVulkan::accelerate_inline_to_memory fallback_write_block gpu=0x{:X} size={}",
                    address, copy_size
                );
            }
            return;
        }
        mm.write_block_unsafe(address, input);
        drop(mm);

        let cpu_addr = cpu_addr.unwrap();
        self.buffer_cache.write_memory(address, copy_size as u64);
        self.texture_cache.base.write_memory(cpu_addr, copy_size);
        self.shader_cache.invalidate_region(cpu_addr, copy_size);
        self.query_cache.invalidate_region(cpu_addr, copy_size);

        if std::env::var_os("RUZU_TRACE_INLINE_TO_MEMORY").is_some() {
            info!(
                "RasterizerVulkan::accelerate_inline_to_memory complete gpu=0x{:X} cpu=0x{:X} size={} first=0x{:02X}",
                address,
                cpu_addr,
                copy_size,
                input.first().copied().unwrap_or(0)
            );
        }
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
}
