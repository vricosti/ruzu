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

use std::sync::Arc;

use ash::vk;
use ash::vk::Handle;
use log::{debug, info, trace, warn};
use thiserror::Error;

use crate::control::channel_state_cache::{ChannelCacheAccessor, ChannelInfo, ChannelSetupCaches};
use crate::engines::maxwell_3d::{
    BlendEquation, BlendFactor, ComparisonOp, CullFace, DrawCall, FrontFace, PrimitiveTopology,
    NUM_VIEWPORTS,
};
use crate::engines::Framebuffer;
use crate::fence_manager::FenceManager as GenericFenceManager;
use crate::framebuffer_config::FramebufferConfig;
use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
use crate::host1x::syncpoint_manager::SyncpointManager;
use crate::rasterizer_interface::{RasterizerDownloadArea, RasterizerInterface};
use crate::texture_cache::texture_cache_base::{DescriptorSyncRegs, ImageViewInOut};
use crate::texture_cache::types::NULL_IMAGE_VIEW_ID;
use crate::textures::texture::texture_pair;
use shader_recompiler::shader_info::Info as ShaderInfo;
use shader_recompiler::{PipelineCache as ShaderPipelineCache, Profile};

use self::pipeline_helper::{
    RescalingPushConstant, RENDERAREA_LAYOUT_OFFSET, RESCALING_LAYOUT_DOWN_FACTOR_OFFSET,
    RESCALING_LAYOUT_WORDS_OFFSET,
};

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
use buffer_cache::BufferCache;
use descriptor_pool::{DescriptorBankInfo, DescriptorPool};
use graphics_pipeline::GraphicsDescriptorBinding;
use pipeline_cache::PipelineCache as VulkanPipelineCache;
use query_cache::QueryCache as VulkanQueryCache;
use render_pass_cache::RenderPassCache;
use scheduler::Scheduler;
use staging_buffer_pool::StagingBufferPool;
use state_tracker::StateTracker;
use texture_cache::TextureCache;
use update_descriptor::UpdateDescriptorQueue;

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
    scheduler: Scheduler,
    state_tracker: StateTracker,
    staging_pool: StagingBufferPool,
    descriptor_pool: DescriptorPool,
    desc_queue: UpdateDescriptorQueue,
    fallback_uniform_buffer: vk::Buffer,
    fallback_uniform_memory: vk::DeviceMemory,
    fallback_uniform_mapped: *mut u8,
    fallback_sampler: vk::Sampler,
    render_pass_cache: RenderPassCache,
    shader_cache: crate::shader_cache::ShaderCache,
    pipeline_cache: VulkanPipelineCache,
    buffer_cache: BufferCache,
    texture_cache: TextureCache,
    query_cache: VulkanQueryCache,
    fence_manager: GenericFenceManager<VkFence>,
    fence_backend: VkFenceBackend,

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
        max_viewports: u32,
        syncpoints: Arc<SyncpointManager>,
        device_memory: Arc<MaxwellDeviceMemoryManager>,
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

        // Create scheduler
        let scheduler = Scheduler::new(device.clone(), graphics_queue, command_pool)
            .map_err(|e| RendererError::InitFailed(format!("scheduler: {:?}", e)))?;

        // Create state tracker
        let state_tracker = StateTracker::new();

        // Create staging buffer pool
        let staging_pool =
            StagingBufferPool::new(device.clone(), instance.clone(), physical_device);

        // Create descriptor pool
        let descriptor_pool = DescriptorPool::new(device.clone(), 64);

        // Create descriptor update queue
        let desc_queue = UpdateDescriptorQueue::new();

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
        let render_pass_cache = RenderPassCache::new(device.clone());

        // Create shader recompiler pipeline cache
        let profile = Profile {
            supported_spirv: supported_spirv_version,
            unified_descriptor_binding: true,
            ..Profile::default()
        };
        let shader_cache = ShaderPipelineCache::new(profile.clone());

        // Create pipeline cache owner
        let pipeline_cache = VulkanPipelineCache::new(
            device.clone(),
            false,
            false,
            shader_cache,
            profile,
            extended_dynamic_state_supported,
            extended_dynamic_state2_supported,
            topology_list_primitive_restart_supported,
            patch_list_primitive_restart_supported,
            max_viewports,
        );

        // Create buffer cache
        let buffer_cache = BufferCache::new(device.clone(), instance.clone(), physical_device)
            .map_err(|e| RendererError::InitFailed(format!("buffer cache: {:?}", e)))?;

        // Create texture cache
        let shader_cache = crate::shader_cache::ShaderCache::new(Arc::clone(&device_memory));

        let texture_cache = TextureCache::new(
            device.clone(),
            instance.clone(),
            physical_device,
            device_memory,
        );

        // Create query cache
        let query_cache = VulkanQueryCache::new();

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
            state_tracker,
            staging_pool,
            descriptor_pool,
            desc_queue,
            fallback_uniform_buffer,
            fallback_uniform_memory,
            fallback_uniform_mapped,
            fallback_sampler,
            render_pass_cache,
            shader_cache,
            pipeline_cache,
            buffer_cache,
            texture_cache,
            query_cache,
            fence_manager: GenericFenceManager::new(true),
            fence_backend: VkFenceBackend::new(),
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

    /// Main draw entry point — process a single draw call.
    ///
    /// Ref: zuyu RasterizerVulkan::Draw() — compiles/caches pipeline,
    /// updates dynamic state via dirty flags, binds resources, records draw.
    pub fn draw(
        &mut self,
        draw: &DrawCall,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
    ) {
        // 1. Periodic flush
        self.flush_work();

        // 2. Compile or lookup cached pipeline
        let pipeline_result = self
            .pipeline_cache
            .current_graphics_pipeline_with_shared_cache(
                draw,
                self.default_render_pass,
                &mut self.shader_cache,
            );
        let (
            pipeline,
            pipeline_layout,
            descriptor_set_layout,
            descriptor_bindings,
            descriptor_bank_info,
            stage_infos,
            uses_render_area,
            uses_rescaling_uniform,
        ) = match pipeline_result {
            Some((gp, _fixed_state)) => (
                gp.pipeline,
                gp.pipeline_layout,
                gp.descriptor_set_layout,
                gp.descriptor_bindings.clone(),
                gp.descriptor_bank_info,
                gp.stage_infos.clone(),
                gp.uses_render_area,
                gp.uses_rescaling_uniform,
            ),
            None => {
                debug!("RasterizerVulkan: skipping draw (pipeline compilation failed)");
                return;
            }
        };

        // 3. Ensure we're inside a render pass
        let target_fb = self
            .texture_cache
            .update_render_targets_and_get_rt0_framebuffer(
                &crate::engines::draw_manager::Maxwell3DRenderTargets {
                    rt_control: draw.rt_control,
                    render_targets: draw.render_targets,
                    zeta: Default::default(),
                    anti_alias_samples_mode: 0,
                    surface_clip: draw.surface_clip,
                },
                self.default_render_pass,
                self.depth_view,
            );
        let cmd = self.scheduler.command_buffer();
        let (framebuffer, extent) = if let Some(target) = target_fb {
            self.texture_cache
                .prepare_render_target_for_render(target.cpu_addr, cmd);
            (target.framebuffer, target.extent)
        } else {
            (
                self.offscreen_fb,
                vk::Extent2D {
                    width: self.fb_width,
                    height: self.fb_height,
                },
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
            draw,
            read_gpu,
            read_gpu_unsafe,
        );

        // 5. Update dynamic states via dirty flags
        self.update_dynamic_states(cmd, draw);

        // 6. Bind vertex/index buffers
        self.bind_vertex_buffers(cmd, draw, read_gpu);
        if draw_params.is_indexed {
            self.bind_index_buffer(cmd, draw, draw_params, read_gpu);
        }

        self.scheduler.request_renderpass(
            framebuffer,
            self.default_render_pass,
            render_area,
            &clear_values,
        );

        if std::env::var_os("RUZU_TRACE_VK_DRAW").is_some() {
            let rt0 = draw.render_targets[0];
            log::info!(
                "[VK_DRAW] draw={} topology={:?} guest_indexed={} vk_indexed={} vertices={} instances={} first_index={} base_vertex={} base_instance={} vertex_first={} vertex_count={} index_first={} index_count={} rt0_addr=0x{:X} rt0={}x{} fmt={} prim_restart={} rasterize={} uses_render_area={} uses_rescaling_uniform={} blend0={} color=({:?},{:?},{:?}) alpha=({:?},{:?},{:?}) mask={:?}",
                self.draw_counter,
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
                rt0.address,
                rt0.width,
                rt0.height,
                rt0.format,
                draw.primitive_restart.enabled,
                draw.rasterize_enable,
                uses_render_area,
                uses_rescaling_uniform,
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
            self.scheduler.flush();
            self.draw_counter = 0;
            self.state_tracker.invalidate_command_buffer_state();
            self.staging_pool.new_frame();
            self.descriptor_pool.reset_pools();
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
        self.query_cache.should_wait_async_flushes()
    }

    fn should_flush_async(&self) -> bool {
        self.query_cache.has_uncommitted_flushes()
    }

    fn pop_async_flushes(&mut self) {
        self.query_cache.pop_async_flushes();
    }

    fn commit_async_flushes(&mut self) {
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
            self.draw(draw, read_gpu, &read_gpu_unsafe);
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

    fn update_depth_bounds_test_enable(&mut self, cmd: vk::CommandBuffer, _draw: &DrawCall) {
        if !self.state_tracker.touch_depth_bounds_test_enable() {
            return;
        }
        // ruzu does not expose regs.depth_bounds_enable in DrawCall yet.
        unsafe {
            self.device.cmd_set_depth_bounds_test_enable(cmd, false);
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
            if std::env::var_os("RUZU_TRACE_VK_VERTEX_INPUT").is_some() && self.draw_counter <= 3 {
                let mut bytes = vec![0u8; size.min(128) as usize];
                read_gpu(stream.address, &mut bytes);
                log::info!(
                    "[VK_VERTEX_BUFFER] draw={} binding={} addr=0x{:X} stride={} frequency={} size={} first={} count={} bytes={:02X?}",
                    self.draw_counter,
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
        let mut sampled_cursor = 0usize;

        for binding in descriptor_bindings {
            match binding.descriptor_type {
                vk::DescriptorType::UNIFORM_BUFFER | vk::DescriptorType::STORAGE_BUFFER => {
                    let start = buffer_infos.len();
                    for element in 0..binding.descriptor_count {
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
                            if std::env::var_os("RUZU_TRACE_VK_CBUF").is_some()
                                && self.draw_counter <= 3
                            {
                                let stage = binding.uniform_stage.unwrap_or(u32::MAX);
                                let index = binding
                                    .uniform_index
                                    .unwrap_or(u32::MAX)
                                    .saturating_add(element);
                                let mut bytes = vec![0u8; size.min(0x40) as usize];
                                read_gpu(gpu_va, &mut bytes);
                                let mut floats = Vec::with_capacity(bytes.len() / 4);
                                for chunk in bytes.chunks_exact(4) {
                                    floats.push(f32::from_le_bytes([
                                        chunk[0], chunk[1], chunk[2], chunk[3],
                                    ]));
                                }
                                log::info!(
                                    "[VK_CBUF] draw={} binding={} stage={} index={} addr=0x{:X} size={} f32={:?}",
                                    self.draw_counter,
                                    binding.binding,
                                    stage,
                                    index,
                                    gpu_va,
                                    size,
                                    floats
                                );
                            }
                            let (buffer, offset) = self.buffer_cache.get_or_upload(
                                gpu_va,
                                size,
                                read_gpu,
                                &mut self.staging_pool,
                                upload_cmd,
                            );
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
                                    &mut self.staging_pool,
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
                            if std::env::var_os("RUZU_TRACE_VK_TEXTURE_STATE").is_some() {
                                if view_id.is_valid() && view_id != NULL_IMAGE_VIEW_ID {
                                    let view =
                                        self.texture_cache.base.slot_image_views.get(view_id);
                                    let image =
                                        self.texture_cache.base.slot_images.get(view.image_id);
                                    log::warn!(
                                        "[VK_TEXTURE_STATE] binding={} elem={} sampled_index={} view_id={} sampler_id={} image_id={} view_gpu=0x{:X} view_fmt={:?} view_type={:?} view_range={:?} view_swizzle={:?} view_size={:?} view_flags={:?} image_type={:?} image_fmt={:?} image_size={:?} image_gpu=0x{:X} image_cpu=0x{:X} guest_size={} unswizzled_size={} image_flags={:?}",
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
                                        "[VK_TEXTURE_STATE] binding={} elem={} sampled_index={} view_id={} sampler_id={} null_view",
                                        binding.binding,
                                        element,
                                        sampled_index,
                                        view_id.index,
                                        sampler_id.index,
                                    );
                                }
                                if sampler_id.is_valid()
                                    && sampler_id != crate::texture_cache::types::NULL_SAMPLER_ID
                                {
                                    let tsc = self.texture_cache.base.slot_samplers.get(sampler_id);
                                    log::warn!(
                                        "[VK_TEXTURE_SAMPLER] binding={} elem={} sampler_id={} raw={:016X?} wrap=({},{},{}) filter=({},{},{}) depth_cmp={} depth_func={} lod=({:.3},{:.3}) bias={:.3}",
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
                "[VK_PRESENT] AccelerateDisplay hit addr=0x{:X} view={} image=0x{:X} image_view=0x{:X} size={}x{} scaled={}",
                framebuffer_addr,
                framebuffer_view.common.view_id.index,
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
    fn draw(
        &mut self,
        draw_view: crate::engines::draw_manager::Maxwell3DDrawView<'_>,
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
        self.draw(&draw_call, &read_gpu, &read_gpu_unsafe);
    }

    fn draw_texture(&mut self) {
        debug!("RasterizerVulkan::draw_texture");
    }

    fn clear(
        &mut self,
        clear_view: crate::engines::draw_manager::Maxwell3DClearView<'_>,
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
        let Some(target) = self
            .texture_cache
            .update_render_targets_and_get_rt0_framebuffer(
                &render_targets,
                self.default_render_pass,
                self.depth_view,
            )
        else {
            return;
        };
        let framebuffer = target.framebuffer;
        let render_area = target.extent;
        let cmd = self.scheduler.command_buffer();
        self.texture_cache
            .prepare_render_target_for_render(target.cpu_addr, cmd);
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
        self.scheduler.request_outside_renderpass();
        self.scheduler.request_renderpass(
            framebuffer,
            self.default_render_pass,
            vk::Rect2D {
                offset: vk::Offset2D { x: 0, y: 0 },
                extent: render_area,
            },
            &clear_values,
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
        if use_depth {
            depth_stencil_aspects |= vk::ImageAspectFlags::DEPTH;
        }
        if use_stencil {
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
                    .old_layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)
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
                    .new_layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)
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
        self.finish();
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
        self.state_tracker.invalidate_command_buffer_state();
        self.staging_pool.new_frame();
        self.descriptor_pool.reset_pools();
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
        _src: &crate::engines::fermi_2d::Surface,
        _dst: &crate::engines::fermi_2d::Surface,
        _copy_config: &crate::engines::fermi_2d::Config,
    ) -> bool {
        false
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
