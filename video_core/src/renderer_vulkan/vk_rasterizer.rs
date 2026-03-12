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
// AccelerateDMA
// ---------------------------------------------------------------------------

/// Port of `AccelerateDMA` class.
///
/// Implements `AccelerateDMAInterface` to accelerate DMA copies via
/// the buffer cache and texture cache.
pub struct AccelerateDma {
    // In upstream these are references; here they'd be shared references or indices.
    _private: (),
}

impl AccelerateDma {
    /// Port of `AccelerateDMA::AccelerateDMA`.
    pub fn new() -> Self {
        todo!("AccelerateDma::new")
    }

    /// Port of `AccelerateDMA::BufferCopy`.
    pub fn buffer_copy(&mut self, _start_address: u64, _end_address: u64, _amount: u64) -> bool {
        todo!("AccelerateDma::buffer_copy")
    }

    /// Port of `AccelerateDMA::BufferClear`.
    pub fn buffer_clear(&mut self, _src_address: u64, _amount: u64, _value: u32) -> bool {
        todo!("AccelerateDma::buffer_clear")
    }

    /// Port of `AccelerateDMA::ImageToBuffer`.
    pub fn image_to_buffer(&mut self) -> bool {
        todo!("AccelerateDma::image_to_buffer")
    }

    /// Port of `AccelerateDMA::BufferToImage`.
    pub fn buffer_to_image(&mut self) -> bool {
        todo!("AccelerateDma::buffer_to_image")
    }
}

// ---------------------------------------------------------------------------
// RasterizerVulkan
// ---------------------------------------------------------------------------

/// Port of `RasterizerVulkan` class.
///
/// Implements `RasterizerInterface` and `ChannelSetupCaches<ChannelInfo>`.
pub struct RasterizerVulkan {
    /// Port of `draw_counter`.
    draw_counter: u32,
    // Full field set requires downstream types (StagingBufferPool, DescriptorPool, etc.)
    _private: (),
}

impl RasterizerVulkan {
    /// Port of `RasterizerVulkan::RasterizerVulkan`.
    pub fn new() -> Self {
        todo!("RasterizerVulkan::new")
    }

    /// Port of `RasterizerVulkan::Draw`.
    pub fn draw(&mut self, _is_indexed: bool, _instance_count: u32) {
        todo!("RasterizerVulkan::draw")
    }

    /// Port of `RasterizerVulkan::DrawIndirect`.
    pub fn draw_indirect(&mut self) {
        todo!("RasterizerVulkan::draw_indirect")
    }

    /// Port of `RasterizerVulkan::DrawTexture`.
    pub fn draw_texture(&mut self) {
        todo!("RasterizerVulkan::draw_texture")
    }

    /// Port of `RasterizerVulkan::Clear`.
    pub fn clear(&mut self, _layer_count: u32) {
        todo!("RasterizerVulkan::clear")
    }

    /// Port of `RasterizerVulkan::DispatchCompute`.
    pub fn dispatch_compute(&mut self) {
        todo!("RasterizerVulkan::dispatch_compute")
    }

    /// Port of `RasterizerVulkan::ResetCounter`.
    pub fn reset_counter(&mut self, _query_type: u32) {
        todo!("RasterizerVulkan::reset_counter")
    }

    /// Port of `RasterizerVulkan::Query`.
    pub fn query(&mut self, _gpu_addr: u64, _query_type: u32, _flags: u32, _payload: u32, _subreport: u32) {
        todo!("RasterizerVulkan::query")
    }

    /// Port of `RasterizerVulkan::BindGraphicsUniformBuffer`.
    pub fn bind_graphics_uniform_buffer(&mut self, _stage: usize, _index: u32, _gpu_addr: u64, _size: u32) {
        todo!("RasterizerVulkan::bind_graphics_uniform_buffer")
    }

    /// Port of `RasterizerVulkan::DisableGraphicsUniformBuffer`.
    pub fn disable_graphics_uniform_buffer(&mut self, _stage: usize, _index: u32) {
        todo!("RasterizerVulkan::disable_graphics_uniform_buffer")
    }

    /// Port of `RasterizerVulkan::FlushAll`.
    pub fn flush_all(&mut self) {
        todo!("RasterizerVulkan::flush_all")
    }

    /// Port of `RasterizerVulkan::FlushRegion`.
    pub fn flush_region(&mut self, _addr: u64, _size: u64) {
        todo!("RasterizerVulkan::flush_region")
    }

    /// Port of `RasterizerVulkan::MustFlushRegion`.
    pub fn must_flush_region(&self, _addr: u64, _size: u64) -> bool {
        todo!("RasterizerVulkan::must_flush_region")
    }

    /// Port of `RasterizerVulkan::GetFlushArea`.
    pub fn get_flush_area(&self, _addr: u64, _size: u64) {
        todo!("RasterizerVulkan::get_flush_area")
    }

    /// Port of `RasterizerVulkan::InvalidateRegion`.
    pub fn invalidate_region(&mut self, _addr: u64, _size: u64) {
        todo!("RasterizerVulkan::invalidate_region")
    }

    /// Port of `RasterizerVulkan::InnerInvalidation`.
    pub fn inner_invalidation(&mut self, _sequences: &[(u64, usize)]) {
        todo!("RasterizerVulkan::inner_invalidation")
    }

    /// Port of `RasterizerVulkan::OnCacheInvalidation`.
    pub fn on_cache_invalidation(&mut self, _addr: u64, _size: u64) {
        todo!("RasterizerVulkan::on_cache_invalidation")
    }

    /// Port of `RasterizerVulkan::OnCPUWrite`.
    pub fn on_cpu_write(&mut self, _addr: u64, _size: u64) -> bool {
        todo!("RasterizerVulkan::on_cpu_write")
    }

    /// Port of `RasterizerVulkan::InvalidateGPUCache`.
    pub fn invalidate_gpu_cache(&mut self) {
        todo!("RasterizerVulkan::invalidate_gpu_cache")
    }

    /// Port of `RasterizerVulkan::UnmapMemory`.
    pub fn unmap_memory(&mut self, _addr: u64, _size: u64) {
        todo!("RasterizerVulkan::unmap_memory")
    }

    /// Port of `RasterizerVulkan::ModifyGPUMemory`.
    pub fn modify_gpu_memory(&mut self, _as_id: usize, _addr: u64, _size: u64) {
        todo!("RasterizerVulkan::modify_gpu_memory")
    }

    /// Port of `RasterizerVulkan::SignalFence`.
    pub fn signal_fence(&mut self, _func: Box<dyn FnOnce()>) {
        todo!("RasterizerVulkan::signal_fence")
    }

    /// Port of `RasterizerVulkan::SyncOperation`.
    pub fn sync_operation(&mut self, _func: Box<dyn FnOnce()>) {
        todo!("RasterizerVulkan::sync_operation")
    }

    /// Port of `RasterizerVulkan::SignalSyncPoint`.
    pub fn signal_sync_point(&mut self, _value: u32) {
        todo!("RasterizerVulkan::signal_sync_point")
    }

    /// Port of `RasterizerVulkan::SignalReference`.
    pub fn signal_reference(&mut self) {
        todo!("RasterizerVulkan::signal_reference")
    }

    /// Port of `RasterizerVulkan::ReleaseFences`.
    pub fn release_fences(&mut self, _force: bool) {
        todo!("RasterizerVulkan::release_fences")
    }

    /// Port of `RasterizerVulkan::FlushAndInvalidateRegion`.
    pub fn flush_and_invalidate_region(&mut self, _addr: u64, _size: u64) {
        todo!("RasterizerVulkan::flush_and_invalidate_region")
    }

    /// Port of `RasterizerVulkan::WaitForIdle`.
    pub fn wait_for_idle(&mut self) {
        todo!("RasterizerVulkan::wait_for_idle")
    }

    /// Port of `RasterizerVulkan::FragmentBarrier`.
    pub fn fragment_barrier(&mut self) {
        todo!("RasterizerVulkan::fragment_barrier")
    }

    /// Port of `RasterizerVulkan::TiledCacheBarrier`.
    pub fn tiled_cache_barrier(&mut self) {
        todo!("RasterizerVulkan::tiled_cache_barrier")
    }

    /// Port of `RasterizerVulkan::FlushCommands`.
    pub fn flush_commands(&mut self) {
        todo!("RasterizerVulkan::flush_commands")
    }

    /// Port of `RasterizerVulkan::TickFrame`.
    pub fn tick_frame(&mut self) {
        todo!("RasterizerVulkan::tick_frame")
    }

    /// Port of `RasterizerVulkan::AccelerateConditionalRendering`.
    pub fn accelerate_conditional_rendering(&mut self) -> bool {
        todo!("RasterizerVulkan::accelerate_conditional_rendering")
    }

    /// Port of `RasterizerVulkan::AccelerateSurfaceCopy`.
    pub fn accelerate_surface_copy(&mut self) -> bool {
        todo!("RasterizerVulkan::accelerate_surface_copy")
    }

    /// Port of `RasterizerVulkan::AccelerateInlineToMemory`.
    pub fn accelerate_inline_to_memory(&mut self, _address: u64, _copy_size: usize, _memory: &[u8]) {
        todo!("RasterizerVulkan::accelerate_inline_to_memory")
    }

    /// Port of `RasterizerVulkan::LoadDiskResources`.
    pub fn load_disk_resources(&mut self, _title_id: u64) {
        todo!("RasterizerVulkan::load_disk_resources")
    }

    /// Port of `RasterizerVulkan::InitializeChannel`.
    pub fn initialize_channel(&mut self) {
        todo!("RasterizerVulkan::initialize_channel")
    }

    /// Port of `RasterizerVulkan::BindChannel`.
    pub fn bind_channel(&mut self) {
        todo!("RasterizerVulkan::bind_channel")
    }

    /// Port of `RasterizerVulkan::ReleaseChannel`.
    pub fn release_channel(&mut self, _channel_id: i32) {
        todo!("RasterizerVulkan::release_channel")
    }

    /// Port of `RasterizerVulkan::AccelerateDisplay`.
    pub fn accelerate_display(&mut self, _framebuffer_addr: u64, _pixel_stride: u32) -> Option<FramebufferTextureInfo> {
        todo!("RasterizerVulkan::accelerate_display")
    }

    // --- Private helpers ---

    /// Port of `RasterizerVulkan::FlushWork`.
    fn flush_work(&mut self) {
        todo!("RasterizerVulkan::flush_work")
    }

    /// Port of `RasterizerVulkan::UpdateDynamicStates`.
    fn update_dynamic_states(&mut self) {
        todo!("RasterizerVulkan::update_dynamic_states")
    }

    /// Port of `RasterizerVulkan::HandleTransformFeedback`.
    fn handle_transform_feedback(&mut self) {
        todo!("RasterizerVulkan::handle_transform_feedback")
    }

    fn update_viewports_state(&mut self) { todo!() }
    fn update_scissors_state(&mut self) { todo!() }
    fn update_depth_bias(&mut self) { todo!() }
    fn update_blend_constants(&mut self) { todo!() }
    fn update_depth_bounds(&mut self) { todo!() }
    fn update_stencil_faces(&mut self) { todo!() }
    fn update_line_width(&mut self) { todo!() }
    fn update_cull_mode(&mut self) { todo!() }
    fn update_depth_bounds_test_enable(&mut self) { todo!() }
    fn update_depth_test_enable(&mut self) { todo!() }
    fn update_depth_write_enable(&mut self) { todo!() }
    fn update_depth_compare_op(&mut self) { todo!() }
    fn update_primitive_restart_enable(&mut self) { todo!() }
    fn update_rasterizer_discard_enable(&mut self) { todo!() }
    fn update_depth_bias_enable(&mut self) { todo!() }
    fn update_logic_op_enable(&mut self) { todo!() }
    fn update_depth_clamp_enable(&mut self) { todo!() }
    fn update_front_face(&mut self) { todo!() }
    fn update_stencil_op(&mut self) { todo!() }
    fn update_stencil_test_enable(&mut self) { todo!() }
    fn update_logic_op(&mut self) { todo!() }
    fn update_blending(&mut self) { todo!() }
    fn update_vertex_input(&mut self) { todo!() }
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
