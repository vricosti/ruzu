// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_rasterizer.h` / `vk_rasterizer.cpp`.
//!
//! Central rasterizer orchestrator for the Vulkan backend. Coordinates
//! shader compilation, pipeline caching, buffer/texture management,
//! command batching, and GPU state tracking.

use ash::vk;

// ---------------------------------------------------------------------------
// Constants (from vk_rasterizer.h private section)
// ---------------------------------------------------------------------------

const MAX_TEXTURES: usize = 192;
const MAX_IMAGES: usize = 48;
const MAX_IMAGE_VIEWS: usize = MAX_TEXTURES + MAX_IMAGES;

/// Default buffer size used as fallback.
const DEFAULT_BUFFER_SIZE: vk::DeviceSize = 4 * std::mem::size_of::<f32>() as vk::DeviceSize;

// ---------------------------------------------------------------------------
// DrawParams (from vk_rasterizer.cpp anonymous namespace)
// ---------------------------------------------------------------------------

/// Port of `DrawParams` struct from the anonymous namespace.
///
/// Encapsulates all parameters needed for a draw call.
#[derive(Debug, Clone, Copy, Default)]
struct DrawParams {
    base_instance: u32,
    num_instances: u32,
    base_vertex: u32,
    num_vertices: u32,
    first_index: u32,
    is_indexed: bool,
}

// ---------------------------------------------------------------------------
// AccelerateDMA
// ---------------------------------------------------------------------------

/// Port of `AccelerateDMA` class.
///
/// Implements `AccelerateDMAInterface` to accelerate DMA copies via
/// the buffer cache and texture cache. References buffer_cache,
/// texture_cache, and scheduler from the parent rasterizer.
pub struct AccelerateDma {
    // In upstream, holds references to BufferCache, TextureCache, Scheduler
    _private: (),
}

impl AccelerateDma {
    /// Port of `AccelerateDMA::AccelerateDMA`.
    pub fn new() -> Self {
        AccelerateDma { _private: () }
    }

    /// Port of `AccelerateDMA::BufferCopy`.
    ///
    /// Attempts to accelerate a buffer-to-buffer DMA copy through the
    /// buffer cache. Returns true if the copy was handled.
    pub fn buffer_copy(&mut self, _start_address: u64, _end_address: u64, _amount: u64) -> bool {
        // Full implementation delegates to buffer_cache.DMACopy
        false
    }

    /// Port of `AccelerateDMA::BufferClear`.
    ///
    /// Attempts to accelerate a DMA buffer clear through the buffer cache.
    /// Returns true if the clear was handled.
    pub fn buffer_clear(&mut self, _src_address: u64, _amount: u64, _value: u32) -> bool {
        // Full implementation delegates to buffer_cache.DMAClear
        false
    }

    /// Port of `AccelerateDMA::ImageToBuffer`.
    ///
    /// Attempts to accelerate an image-to-buffer DMA copy through the
    /// texture cache. Returns true if the copy was handled.
    pub fn image_to_buffer(&mut self) -> bool {
        // Full implementation calls DmaBufferImageCopy<false>
        false
    }

    /// Port of `AccelerateDMA::BufferToImage`.
    ///
    /// Attempts to accelerate a buffer-to-image DMA copy through the
    /// texture cache. Returns true if the copy was handled.
    pub fn buffer_to_image(&mut self) -> bool {
        // Full implementation calls DmaBufferImageCopy<true>
        false
    }
}

// ---------------------------------------------------------------------------
// RasterizerVulkan
// ---------------------------------------------------------------------------

/// Port of `RasterizerVulkan` class.
///
/// Implements `RasterizerInterface` and `ChannelSetupCaches<ChannelInfo>`.
///
/// This is the central orchestrator for the Vulkan rendering backend.
/// It coordinates shader compilation, pipeline caching, buffer/texture
/// management, command batching, and GPU state tracking.
pub struct RasterizerVulkan {
    /// Port of `draw_counter` — incremented each draw for flush heuristics.
    draw_counter: u32,

    /// Port of `wfi_event` — VkEvent used for WaitForIdle synchronization.
    wfi_event: vk::Event,

    /// Port of `image_view_indices` — static vector of image view indices.
    image_view_indices: Vec<u32>,

    /// Port of `sampler_handles` — static vector of sampler handles.
    sampler_handles: Vec<vk::Sampler>,

    /// The DMA accelerator instance.
    accelerate_dma: AccelerateDma,
}

/// How many draws before FlushWork triggers a scheduler Flush.
/// Port of the threshold used in `FlushWork`.
const DRAWS_TO_FLUSH: u32 = 4096;

impl RasterizerVulkan {
    /// Port of `RasterizerVulkan::RasterizerVulkan`.
    ///
    /// In the full implementation, this constructor initializes:
    /// - staging_pool, descriptor_pool, guest_descriptor_queue
    /// - compute_pass_descriptor_queue, blit_image, render_pass_cache
    /// - texture_cache_runtime, texture_cache, buffer_cache_runtime
    /// - buffer_cache, query_cache_runtime, query_cache
    /// - pipeline_cache, accelerate_dma, fence_manager, wfi_event
    /// - Registers query_cache with scheduler
    pub fn new() -> Self {
        RasterizerVulkan {
            draw_counter: 0,
            wfi_event: vk::Event::null(),
            image_view_indices: Vec::with_capacity(MAX_IMAGE_VIEWS),
            sampler_handles: Vec::with_capacity(MAX_TEXTURES),
            accelerate_dma: AccelerateDma::new(),
        }
    }

    /// Port of `RasterizerVulkan::Draw`.
    ///
    /// Records a draw call: prepares state via PrepareDraw, then records
    /// either DrawIndexed or Draw based on the draw state.
    pub fn draw(&mut self, _is_indexed: bool, _instance_count: u32) {
        // PrepareDraw(is_indexed, || {
        //     let draw_state = maxwell3d.draw_manager.get_draw_state();
        //     let draw_params = make_draw_params(draw_state, instance_count, is_indexed);
        //     scheduler.record(|cmdbuf| {
        //         if draw_params.is_indexed {
        //             cmdbuf.draw_indexed(...)
        //         } else {
        //             cmdbuf.draw(...)
        //         }
        //     });
        // });
    }

    /// Port of `RasterizerVulkan::DrawIndirect`.
    pub fn draw_indirect(&mut self) {
        // Sets draw indirect buffer, then PrepareDraw with indirect dispatch
    }

    /// Port of `RasterizerVulkan::DrawTexture`.
    pub fn draw_texture(&mut self) {
        // Draws a textured quad using the blit image helper
    }

    /// Port of `RasterizerVulkan::Clear`.
    pub fn clear(&mut self, _layer_count: u32) {
        // Records a clear operation via scheduler
    }

    /// Port of `RasterizerVulkan::DispatchCompute`.
    pub fn dispatch_compute(&mut self) {
        // Configures and dispatches the current compute pipeline
    }

    /// Port of `RasterizerVulkan::ResetCounter`.
    pub fn reset_counter(&mut self, _query_type: u32) {
        // Delegates to query_cache.ResetCounter
    }

    /// Port of `RasterizerVulkan::Query`.
    pub fn query(
        &mut self,
        _gpu_addr: u64,
        _query_type: u32,
        _flags: crate::query_cache::types::QueryPropertiesFlags,
        _payload: u32,
        _subreport: u32,
    ) {
        // Delegates to query_cache.Query
    }

    /// Port of `RasterizerVulkan::BindGraphicsUniformBuffer`.
    pub fn bind_graphics_uniform_buffer(&mut self, _stage: usize, _index: u32, _gpu_addr: u64, _size: u32) {
        // Delegates to buffer_cache.BindGraphicsUniformBuffer
    }

    /// Port of `RasterizerVulkan::DisableGraphicsUniformBuffer`.
    pub fn disable_graphics_uniform_buffer(&mut self, _stage: usize, _index: u32) {
        // Delegates to buffer_cache.DisableGraphicsUniformBuffer
    }

    /// Port of `RasterizerVulkan::FlushAll`.
    pub fn flush_all(&mut self) {
        // No-op in upstream
    }

    /// Port of `RasterizerVulkan::FlushRegion`.
    pub fn flush_region(&mut self, _addr: u64, _size: u64) {
        // Delegates to query_cache, buffer_cache, texture_cache
    }

    /// Port of `RasterizerVulkan::MustFlushRegion`.
    pub fn must_flush_region(&self, _addr: u64, _size: u64) -> bool {
        false
    }

    /// Port of `RasterizerVulkan::GetFlushArea`.
    pub fn get_flush_area(&self, addr: u64, size: u64) -> (u64, u64) {
        // Returns the area that needs flushing
        (addr, size)
    }

    /// Port of `RasterizerVulkan::InvalidateRegion`.
    pub fn invalidate_region(&mut self, _addr: u64, _size: u64) {
        // Delegates to query_cache, buffer_cache, texture_cache, pipeline_cache
    }

    /// Port of `RasterizerVulkan::InnerInvalidation`.
    pub fn inner_invalidation(&mut self, _sequences: &[(u64, usize)]) {
        // Batch invalidation of multiple memory regions
    }

    /// Port of `RasterizerVulkan::OnCacheInvalidation`.
    pub fn on_cache_invalidation(&mut self, _addr: u64, _size: u64) {
        // Delegates to buffer_cache, texture_cache, pipeline_cache
    }

    /// Port of `RasterizerVulkan::OnCPUWrite`.
    pub fn on_cpu_write(&mut self, _addr: u64, _size: u64) -> bool {
        // Delegates to buffer_cache.OnCPUWrite, texture_cache.WriteMemory
        false
    }

    /// Port of `RasterizerVulkan::InvalidateGPUCache`.
    pub fn invalidate_gpu_cache(&mut self) {
        // Flushes all shader caches
    }

    /// Port of `RasterizerVulkan::UnmapMemory`.
    pub fn unmap_memory(&mut self, _addr: u64, _size: u64) {
        // Unmaps memory from buffer_cache, texture_cache
    }

    /// Port of `RasterizerVulkan::ModifyGPUMemory`.
    pub fn modify_gpu_memory(&mut self, _as_id: usize, _addr: u64, _size: u64) {
        // Notifies texture_cache and buffer_cache of GPU memory modifications
    }

    /// Port of `RasterizerVulkan::SignalFence`.
    pub fn signal_fence(&mut self, _func: Box<dyn FnOnce() + Send>) {
        // Delegates to fence_manager.SignalFence
    }

    /// Port of `RasterizerVulkan::SyncOperation`.
    pub fn sync_operation(&mut self, _func: Box<dyn FnOnce() + Send>) {
        // Delegates to fence_manager.SyncOperation
    }

    /// Port of `RasterizerVulkan::SignalSyncPoint`.
    pub fn signal_sync_point(&mut self, _value: u32) {
        // Delegates to fence_manager.SignalSyncPoint
    }

    /// Port of `RasterizerVulkan::SignalReference`.
    pub fn signal_reference(&mut self) {
        // Delegates to fence_manager.SignalOrdering
    }

    /// Port of `RasterizerVulkan::ReleaseFences`.
    pub fn release_fences(&mut self, _force: bool) {
        // Delegates to fence_manager.WaitPendingFences
    }

    /// Port of `RasterizerVulkan::FlushAndInvalidateRegion`.
    pub fn flush_and_invalidate_region(&mut self, addr: u64, size: u64) {
        if size == 0 {
            return;
        }
        self.flush_region(addr, size);
        self.invalidate_region(addr, size);
    }

    /// Port of `RasterizerVulkan::WaitForIdle`.
    pub fn wait_for_idle(&mut self) {
        // Records a set/reset event pair via scheduler, then submits and waits
    }

    /// Port of `RasterizerVulkan::FragmentBarrier`.
    pub fn fragment_barrier(&mut self) {
        // Inserts fragment shader barrier via scheduler
    }

    /// Port of `RasterizerVulkan::TiledCacheBarrier`.
    pub fn tiled_cache_barrier(&mut self) {
        // Inserts tiled cache barrier via scheduler
    }

    /// Port of `RasterizerVulkan::FlushCommands`.
    pub fn flush_commands(&mut self) {
        // Flushes the scheduler command buffer
    }

    /// Port of `RasterizerVulkan::TickFrame`.
    pub fn tick_frame(&mut self) {
        // Ticks buffer_cache, texture_cache, query_cache, staging_pool
        self.draw_counter = 0;
    }

    /// Port of `RasterizerVulkan::AccelerateConditionalRendering`.
    pub fn accelerate_conditional_rendering(&mut self) -> bool {
        // Delegates to query_cache.AccelerateHostConditionalRendering
        false
    }

    /// Port of `RasterizerVulkan::AccelerateSurfaceCopy`.
    pub fn accelerate_surface_copy(&mut self) -> bool {
        // Delegates to blit_image via texture_cache
        false
    }

    /// Port of `RasterizerVulkan::AccelerateInlineToMemory`.
    pub fn accelerate_inline_to_memory(&mut self, _address: u64, _copy_size: usize, _memory: &[u8]) {
        // Delegates to buffer_cache.InlineMemory
    }

    /// Port of `RasterizerVulkan::LoadDiskResources`.
    pub fn load_disk_resources(&mut self, _title_id: u64) {
        // Delegates to pipeline_cache.LoadDiskResources
    }

    /// Port of `RasterizerVulkan::InitializeChannel`.
    pub fn initialize_channel(&mut self) {
        // Base class ChannelSetupCaches handles channel initialization
    }

    /// Port of `RasterizerVulkan::BindChannel`.
    pub fn bind_channel(&mut self) {
        // Base class ChannelSetupCaches handles channel binding
    }

    /// Port of `RasterizerVulkan::ReleaseChannel`.
    pub fn release_channel(&mut self, _channel_id: i32) {
        // Releases channel resources from all caches
    }

    /// Port of `RasterizerVulkan::AccelerateDisplay`.
    pub fn accelerate_display(&mut self, _framebuffer_addr: u64, _pixel_stride: u32) -> Option<FramebufferTextureInfo> {
        // Attempts to use the texture cache to directly present an existing
        // GPU texture instead of copying data to CPU and back
        None
    }

    // --- Private helpers ---

    /// Port of `RasterizerVulkan::FlushWork`.
    ///
    /// Checks if enough draws have accumulated to warrant flushing the
    /// command buffer to prevent growing it indefinitely.
    fn flush_work(&mut self) {
        self.draw_counter += 1;
        if self.draw_counter >= DRAWS_TO_FLUSH {
            self.draw_counter = 0;
            // scheduler.flush();
        }
    }

    /// Port of `RasterizerVulkan::UpdateDynamicStates`.
    ///
    /// Checks dirty flags and updates only the Vulkan dynamic states that
    /// have changed since the last draw.
    fn update_dynamic_states(&mut self) {
        // Checks state_tracker dirty flags and calls individual update methods
    }

    /// Port of `RasterizerVulkan::HandleTransformFeedback`.
    fn handle_transform_feedback(&mut self) {
        // Handles transform feedback buffer binding
    }

    fn update_viewports_state(&mut self) {}
    fn update_scissors_state(&mut self) {}
    fn update_depth_bias(&mut self) {}
    fn update_blend_constants(&mut self) {}
    fn update_depth_bounds(&mut self) {}
    fn update_stencil_faces(&mut self) {}
    fn update_line_width(&mut self) {}
    fn update_cull_mode(&mut self) {}
    fn update_depth_bounds_test_enable(&mut self) {}
    fn update_depth_test_enable(&mut self) {}
    fn update_depth_write_enable(&mut self) {}
    fn update_depth_compare_op(&mut self) {}
    fn update_primitive_restart_enable(&mut self) {}
    fn update_rasterizer_discard_enable(&mut self) {}
    fn update_depth_bias_enable(&mut self) {}
    fn update_logic_op_enable(&mut self) {}
    fn update_depth_clamp_enable(&mut self) {}
    fn update_front_face(&mut self) {}
    fn update_stencil_op(&mut self) {}
    fn update_stencil_test_enable(&mut self) {}
    fn update_logic_op(&mut self) {}
    fn update_blending(&mut self) {}
    fn update_vertex_input(&mut self) {}
}

/// Port of `FramebufferTextureInfo` struct from `vk_blit_screen.h`,
/// referenced by the rasterizer's `AccelerateDisplay`.
#[derive(Debug, Clone, Copy, Default)]
pub struct FramebufferTextureInfo {
    pub image: vk::Image,
    pub image_view: vk::ImageView,
    pub width: u32,
    pub height: u32,
    pub scaled_width: u32,
    pub scaled_height: u32,
}

/// Port of `GetViewportState` helper from anonymous namespace.
///
/// Computes a VkViewport from Maxwell viewport transform registers.
pub fn get_viewport_state(
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
            let sign = value.is_sign_negative();
            let new_value = new_value.abs().round();
            if sign { -new_value } else { new_value }
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

/// Port of `MakeDrawParams` helper from anonymous namespace.
///
/// Constructs draw parameters from the maxwell draw state, handling
/// quad topology conversion to triangles.
fn make_draw_params(
    base_instance: u32,
    num_instances: u32,
    base_vertex: u32,
    num_vertices: u32,
    first_index: u32,
    is_indexed: bool,
    is_quads: bool,
    is_quad_strip: bool,
) -> DrawParams {
    let mut params = DrawParams {
        base_instance,
        num_instances,
        base_vertex,
        num_vertices,
        first_index,
        is_indexed,
    };

    // 6 triangle vertices per quad, base vertex is part of the index
    if is_quads {
        params.num_vertices = (params.num_vertices / 4) * 6;
        params.base_vertex = 0;
        params.is_indexed = true;
    } else if is_quad_strip {
        params.num_vertices = (params.num_vertices.saturating_sub(2)) / 2 * 6;
        params.base_vertex = 0;
        params.is_indexed = true;
    }

    params
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constants() {
        assert_eq!(MAX_TEXTURES, 192);
        assert_eq!(MAX_IMAGES, 48);
        assert_eq!(MAX_IMAGE_VIEWS, 240);
        assert_eq!(DEFAULT_BUFFER_SIZE, 16);
    }

    #[test]
    fn draw_params_quads() {
        let params = make_draw_params(0, 1, 10, 8, 0, true, true, false);
        assert_eq!(params.num_vertices, 12); // 8/4 * 6
        assert_eq!(params.base_vertex, 0);
        assert!(params.is_indexed);
    }

    #[test]
    fn draw_params_quad_strip() {
        let params = make_draw_params(0, 1, 0, 10, 0, false, false, true);
        assert_eq!(params.num_vertices, 24); // (10-2)/2 * 6
        assert_eq!(params.base_vertex, 0);
        assert!(params.is_indexed);
    }

    #[test]
    fn draw_params_triangles() {
        let params = make_draw_params(0, 1, 5, 100, 0, true, false, false);
        assert_eq!(params.num_vertices, 100);
        assert_eq!(params.base_vertex, 5);
        assert!(params.is_indexed);
    }

    #[test]
    fn viewport_identity_scale() {
        let vp = get_viewport_state(320.0, 320.0, 240.0, 240.0, 0.5, 0.5, 1.0, false, false, false, 480.0, false);
        assert_eq!(vp.x, 0.0);
        assert_eq!(vp.width, 640.0);
        assert_eq!(vp.y, 0.0);
        assert_eq!(vp.height, 480.0);
    }

    #[test]
    fn flush_work_counter() {
        let mut rasterizer = RasterizerVulkan::new();
        for _ in 0..DRAWS_TO_FLUSH {
            rasterizer.flush_work();
        }
        assert_eq!(rasterizer.draw_counter, 0);
    }
}
