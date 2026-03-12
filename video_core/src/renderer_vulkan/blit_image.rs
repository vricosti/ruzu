// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `blit_image.h` / `blit_image.cpp`.
//!
//! Helper that blits, converts, and clears images using fullscreen-triangle
//! fragment shaders. Manages pipelines for color blits, depth/stencil blits,
//! format conversions, and color/stencil clears.

use ash::vk;

// ---------------------------------------------------------------------------
// Pipeline key types
// ---------------------------------------------------------------------------

/// Port of `BlitImagePipelineKey`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlitImagePipelineKey {
    pub renderpass: vk::RenderPass,
    pub operation: u32, // Fermi2D::Operation
}

/// Port of `BlitDepthStencilPipelineKey`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlitDepthStencilPipelineKey {
    pub renderpass: vk::RenderPass,
    pub depth_clear: bool,
    pub stencil_mask: u8,
    pub stencil_compare_mask: u32,
    pub stencil_ref: u32,
}

// ---------------------------------------------------------------------------
// Region / Extent helpers (matching upstream using statements)
// ---------------------------------------------------------------------------

/// 2D offset used for blit regions.
#[derive(Debug, Clone, Copy, Default)]
pub struct Offset2D {
    pub x: i32,
    pub y: i32,
}

/// 2D region defined by two corners.
#[derive(Debug, Clone, Copy, Default)]
pub struct Region2D {
    pub start: Offset2D,
    pub end: Offset2D,
}

/// 3D extent.
#[derive(Debug, Clone, Copy, Default)]
pub struct Extent3D {
    pub width: u32,
    pub height: u32,
    pub depth: u32,
}

// ---------------------------------------------------------------------------
// BlitImageHelper
// ---------------------------------------------------------------------------

/// Port of `BlitImageHelper` class.
///
/// Provides GPU-accelerated blit, conversion, and clear operations via
/// fullscreen-triangle shaders and cached pipelines.
pub struct BlitImageHelper {
    // Descriptor layouts and allocators
    _one_texture_set_layout: vk::DescriptorSetLayout,
    _two_textures_set_layout: vk::DescriptorSetLayout,

    // Pipeline layouts
    _one_texture_pipeline_layout: vk::PipelineLayout,
    _two_textures_pipeline_layout: vk::PipelineLayout,
    _clear_color_pipeline_layout: vk::PipelineLayout,

    // Cached pipeline vectors
    blit_color_keys: Vec<BlitImagePipelineKey>,
    blit_depth_stencil_keys: Vec<BlitImagePipelineKey>,
    clear_color_keys: Vec<BlitImagePipelineKey>,
    clear_stencil_keys: Vec<BlitDepthStencilPipelineKey>,

    // Samplers
    _linear_sampler: vk::Sampler,
    _nearest_sampler: vk::Sampler,
}

impl BlitImageHelper {
    /// Port of `BlitImageHelper::BlitImageHelper`.
    pub fn new() -> Self {
        todo!("BlitImageHelper::new")
    }

    /// Port of `BlitImageHelper::BlitColor` (sampled blit variant).
    pub fn blit_color(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_image_view: vk::ImageView,
        _dst_region: &Region2D,
        _src_region: &Region2D,
        _filter: u32,
        _operation: u32,
    ) {
        todo!("BlitImageHelper::blit_color")
    }

    /// Port of `BlitImageHelper::BlitColor` (explicit image + sampler variant).
    pub fn blit_color_with_sampler(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_image_view: vk::ImageView,
        _src_image: vk::Image,
        _src_sampler: vk::Sampler,
        _dst_region: &Region2D,
        _src_region: &Region2D,
        _src_size: &Extent3D,
    ) {
        todo!("BlitImageHelper::blit_color_with_sampler")
    }

    /// Port of `BlitImageHelper::BlitDepthStencil`.
    pub fn blit_depth_stencil(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_depth_view: vk::ImageView,
        _src_stencil_view: vk::ImageView,
        _dst_region: &Region2D,
        _src_region: &Region2D,
        _filter: u32,
        _operation: u32,
    ) {
        todo!("BlitImageHelper::blit_depth_stencil")
    }

    /// Port of `BlitImageHelper::ConvertD32ToR32`.
    pub fn convert_d32_to_r32(&mut self) { todo!() }

    /// Port of `BlitImageHelper::ConvertR32ToD32`.
    pub fn convert_r32_to_d32(&mut self) { todo!() }

    /// Port of `BlitImageHelper::ConvertD16ToR16`.
    pub fn convert_d16_to_r16(&mut self) { todo!() }

    /// Port of `BlitImageHelper::ConvertR16ToD16`.
    pub fn convert_r16_to_d16(&mut self) { todo!() }

    /// Port of `BlitImageHelper::ConvertABGR8ToD24S8`.
    pub fn convert_abgr8_to_d24s8(&mut self) { todo!() }

    /// Port of `BlitImageHelper::ConvertABGR8ToD32F`.
    pub fn convert_abgr8_to_d32f(&mut self) { todo!() }

    /// Port of `BlitImageHelper::ConvertD32FToABGR8`.
    pub fn convert_d32f_to_abgr8(&mut self) { todo!() }

    /// Port of `BlitImageHelper::ConvertD24S8ToABGR8`.
    pub fn convert_d24s8_to_abgr8(&mut self) { todo!() }

    /// Port of `BlitImageHelper::ConvertS8D24ToABGR8`.
    pub fn convert_s8d24_to_abgr8(&mut self) { todo!() }

    /// Port of `BlitImageHelper::ClearColor`.
    pub fn clear_color(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _color_mask: u8,
        _clear_color: [f32; 4],
        _dst_region: &Region2D,
    ) {
        todo!("BlitImageHelper::clear_color")
    }

    /// Port of `BlitImageHelper::ClearDepthStencil`.
    pub fn clear_depth_stencil(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _depth_clear: bool,
        _clear_depth: f32,
        _stencil_mask: u8,
        _stencil_ref: u32,
        _stencil_compare_mask: u32,
        _dst_region: &Region2D,
    ) {
        todo!("BlitImageHelper::clear_depth_stencil")
    }

    // --- Private helpers ---

    fn convert(&mut self, _pipeline: vk::Pipeline) { todo!() }
    fn convert_depth_stencil(&mut self, _pipeline: vk::Pipeline) { todo!() }
    fn find_or_emplace_color_pipeline(&mut self, _key: &BlitImagePipelineKey) -> vk::Pipeline { todo!() }
    fn find_or_emplace_depth_stencil_pipeline(&mut self, _key: &BlitImagePipelineKey) -> vk::Pipeline { todo!() }
    fn find_or_emplace_clear_color_pipeline(&mut self, _key: &BlitImagePipelineKey) -> vk::Pipeline { todo!() }
    fn find_or_emplace_clear_stencil_pipeline(&mut self, _key: &BlitDepthStencilPipelineKey) -> vk::Pipeline { todo!() }
    fn convert_pipeline(&mut self, _renderpass: vk::RenderPass, _is_target_depth: bool) { todo!() }
    fn convert_depth_to_color_pipeline(&mut self, _renderpass: vk::RenderPass) { todo!() }
    fn convert_color_to_depth_pipeline(&mut self, _renderpass: vk::RenderPass) { todo!() }
}
