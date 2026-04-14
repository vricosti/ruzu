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

use crate::query_cache::types::QueryPropertiesFlags;

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
use log::{debug, info, trace, warn};
use thiserror::Error;

use crate::engines::maxwell_3d::{
    BlendEquation, BlendFactor, ComparisonOp, CullFace, DrawCall, FrontFace, PrimitiveTopology,
};
use crate::engines::Framebuffer;
use crate::rasterizer_interface::{RasterizerDownloadArea, RasterizerInterface};
use crate::syncpoint::SyncpointManager;
use shader_recompiler::{PipelineCache, Profile};

use buffer_cache::BufferCache;
use descriptor_pool::DescriptorPool;
use graphics_pipeline::GraphicsPipelineCache;
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

    // Sub-components (matching zuyu's architecture)
    scheduler: Scheduler,
    state_tracker: StateTracker,
    staging_pool: StagingBufferPool,
    descriptor_pool: DescriptorPool,
    desc_queue: UpdateDescriptorQueue,
    render_pass_cache: RenderPassCache,
    pipeline_cache: GraphicsPipelineCache,
    buffer_cache: BufferCache,
    texture_cache: TextureCache,

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
        syncpoints: Arc<SyncpointManager>,
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

        // Create render pass cache
        let render_pass_cache = RenderPassCache::new(device.clone());

        // Create shader recompiler pipeline cache
        let profile = Profile::default();
        let shader_cache = PipelineCache::new(profile);

        // Create graphics pipeline cache
        let pipeline_cache = GraphicsPipelineCache::new(device.clone(), shader_cache);

        // Create buffer cache
        let buffer_cache = BufferCache::new(device.clone(), instance.clone(), physical_device)
            .map_err(|e| RendererError::InitFailed(format!("buffer cache: {:?}", e)))?;

        // Create texture cache
        let texture_cache = TextureCache::new(device.clone(), instance.clone(), physical_device);

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
            scheduler,
            state_tracker,
            staging_pool,
            descriptor_pool,
            desc_queue,
            render_pass_cache,
            pipeline_cache,
            buffer_cache,
            texture_cache,
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
        })
    }

    /// Main draw entry point — process a single draw call.
    ///
    /// Ref: zuyu RasterizerVulkan::Draw() — compiles/caches pipeline,
    /// updates dynamic state via dirty flags, binds resources, records draw.
    pub fn draw(&mut self, draw: &DrawCall, read_gpu: &dyn Fn(u64, &mut [u8])) {
        // 1. Periodic flush
        self.flush_work();

        // 2. Compile or lookup cached pipeline
        let pipeline_result =
            self.pipeline_cache
                .get_or_compile(draw, self.default_render_pass, read_gpu);
        let (pipeline, pipeline_layout) = match pipeline_result {
            Some((gp, _fixed_state)) => (gp.pipeline, gp.pipeline_layout),
            None => {
                debug!("RasterizerVulkan: skipping draw (pipeline compilation failed)");
                return;
            }
        };

        // 3. Ensure we're inside a render pass
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
        self.scheduler.request_renderpass(
            self.offscreen_fb,
            self.default_render_pass,
            vk::Rect2D {
                offset: vk::Offset2D { x: 0, y: 0 },
                extent: vk::Extent2D {
                    width: self.fb_width,
                    height: self.fb_height,
                },
            },
            &clear_values,
        );

        let cmd = self.scheduler.command_buffer();

        // 4. Bind pipeline
        unsafe {
            self.device
                .cmd_bind_pipeline(cmd, vk::PipelineBindPoint::GRAPHICS, pipeline);
        }

        // 5. Update dynamic states via dirty flags
        self.update_dynamic_states(cmd, draw);

        // 6. Bind vertex/index buffers
        self.bind_vertex_buffers(cmd, draw, read_gpu);
        if draw.indexed {
            self.bind_index_buffer(cmd, draw, read_gpu);
        }

        // 7. Issue draw call
        if draw.indexed {
            unsafe {
                self.device.cmd_draw_indexed(
                    cmd,
                    draw.index_buffer_count,
                    draw.instance_count.max(1),
                    draw.index_buffer_first,
                    draw.base_vertex,
                    draw.base_instance,
                );
            }
        } else {
            unsafe {
                self.device.cmd_draw(
                    cmd,
                    draw.vertex_count,
                    draw.instance_count.max(1),
                    draw.vertex_first,
                    draw.base_instance,
                );
            }
        }

        self.draw_counter += 1;
        // Mark state dirty for next draw (conservative — real tracking per-register later)
        self.state_tracker.invalidate_state();
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
        for draw in draws {
            self.draw(draw, read_gpu);
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
        self.update_line_width(cmd, draw);
    }

    fn update_viewports(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_viewports() {
            return;
        }
        let viewport = vk::Viewport {
            x: draw.viewports[0].x,
            y: draw.viewports[0].y,
            width: draw.viewports[0].width.max(1.0),
            height: draw.viewports[0].height.max(1.0),
            min_depth: draw.viewports[0].depth_near,
            max_depth: draw.viewports[0].depth_far,
        };
        unsafe {
            self.device.cmd_set_viewport(cmd, 0, &[viewport]);
        }
    }

    fn update_scissors(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_scissors() {
            return;
        }
        let scissor = if draw.scissors[0].enabled {
            vk::Rect2D {
                offset: vk::Offset2D {
                    x: draw.scissors[0].min_x as i32,
                    y: draw.scissors[0].min_y as i32,
                },
                extent: vk::Extent2D {
                    width: draw.scissors[0]
                        .max_x
                        .saturating_sub(draw.scissors[0].min_x),
                    height: draw.scissors[0]
                        .max_y
                        .saturating_sub(draw.scissors[0].min_y),
                },
            }
        } else {
            vk::Rect2D {
                offset: vk::Offset2D { x: 0, y: 0 },
                extent: vk::Extent2D {
                    width: self.fb_width,
                    height: self.fb_height,
                },
            }
        };
        unsafe {
            self.device.cmd_set_scissor(cmd, 0, &[scissor]);
        }
    }

    fn update_depth_bias(&mut self, cmd: vk::CommandBuffer, draw: &DrawCall) {
        if !self.state_tracker.touch_depth_bias() {
            return;
        }
        if draw.rasterizer.depth_bias != 0.0 {
            unsafe {
                self.device.cmd_set_depth_bias(
                    cmd,
                    draw.rasterizer.depth_bias,
                    draw.rasterizer.depth_bias_clamp,
                    draw.rasterizer.slope_scale_depth_bias,
                );
            }
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
            // Estimate buffer size from stride × vertex_count
            let size =
                (stream.stride as u64) * (draw.vertex_count.max(draw.index_buffer_count) as u64);
            if size == 0 {
                continue;
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
        read_gpu: &dyn Fn(u64, &mut [u8]),
    ) {
        if draw.index_buffer_addr == 0 {
            return;
        }
        let index_size = match draw.index_format {
            crate::engines::maxwell_3d::IndexFormat::UnsignedByte => 1,
            crate::engines::maxwell_3d::IndexFormat::UnsignedShort => 2,
            crate::engines::maxwell_3d::IndexFormat::UnsignedInt => 4,
        };
        let size = (index_size * draw.index_buffer_count) as u64;
        let index_type = match draw.index_format {
            crate::engines::maxwell_3d::IndexFormat::UnsignedByte => vk::IndexType::UINT8_EXT,
            crate::engines::maxwell_3d::IndexFormat::UnsignedShort => vk::IndexType::UINT16,
            crate::engines::maxwell_3d::IndexFormat::UnsignedInt => vk::IndexType::UINT32,
        };
        let upload_cmd = self.scheduler.upload_command_buffer();
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
}

impl RasterizerInterface for RasterizerVulkan {
    fn draw(&mut self, draw_state: &crate::engines::draw_manager::DrawState, instance_count: u32) {
        debug!(
            "RasterizerVulkan::draw indexed={} instances={}",
            draw_state.draw_indexed, instance_count
        );
        // Actual draw dispatch is done through render_draw_calls() which
        // calls self.draw(draw_call, read_gpu) with full DrawCall context.
        // This trait method is for the generic interface.
    }

    fn draw_texture(&mut self) {
        debug!("RasterizerVulkan::draw_texture");
    }

    fn clear(&mut self, layer_count: u32) {
        trace!("RasterizerVulkan::clear layers={}", layer_count);
    }

    fn dispatch_compute(&mut self) {
        debug!("RasterizerVulkan::dispatch_compute");
    }

    fn reset_counter(&mut self, _query_type: u32) {}

    fn query(
        &mut self,
        gpu_addr: u64,
        _query_type: u32,
        flags: QueryPropertiesFlags,
        gpu_ticks: u64,
        payload: u32,
        _subreport: u32,
        gpu_write: Arc<dyn Fn(u64, &[u8]) + Send + Sync>,
    ) {
        let has_timeout = flags.contains(QueryPropertiesFlags::HAS_TIMEOUT);
        if has_timeout {
            gpu_write(gpu_addr + 8, &gpu_ticks.to_le_bytes());
            gpu_write(gpu_addr, &(payload as u64).to_le_bytes());
        } else {
            gpu_write(gpu_addr, &payload.to_le_bytes());
        }
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
        self.finish();
        func();
    }

    fn sync_operation(&mut self, func: Box<dyn FnOnce() + Send>) {
        func();
    }

    fn signal_sync_point(&mut self, id: u32) {
        self.syncpoints.increment(id);
    }

    fn signal_reference(&mut self) {}

    fn release_fences(&mut self, _force: bool) {}

    fn flush_all(&mut self) {
        self.scheduler.flush();
    }

    fn flush_region(&mut self, _addr: u64, _size: u64) {}

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

    fn invalidate_region(&mut self, _addr: u64, _size: u64) {}

    fn on_cache_invalidation(&mut self, _addr: u64, _size: u64) {}

    fn on_cpu_write(&mut self, _addr: u64, _size: u64) -> bool {
        false
    }

    fn invalidate_gpu_cache(&mut self) {}

    fn unmap_memory(&mut self, _addr: u64, _size: u64) {}

    fn modify_gpu_memory(&mut self, _as_id: usize, _addr: u64, _size: u64) {}

    fn flush_and_invalidate_region(&mut self, _addr: u64, _size: u64) {}

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

    fn accelerate_surface_copy(&mut self) -> bool {
        false
    }

    fn accelerate_inline_to_memory(&mut self, _address: u64, _copy_size: usize, _memory: &[u8]) {}
}

impl Drop for RasterizerVulkan {
    fn drop(&mut self) {
        unsafe {
            self.device.device_wait_idle().ok();

            self.device.unmap_memory(self.readback_memory);
            self.device.destroy_buffer(self.readback_buffer, None);
            self.device.free_memory(self.readback_memory, None);

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
