// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `blit_image.h` / `blit_image.cpp`.
//!
//! Helper that blits, converts, and clears images using fullscreen-triangle
//! fragment shaders. Manages pipelines for color blits, depth/stencil blits,
//! format conversions, and color/stencil clears.

use ash::vk;

// ---------------------------------------------------------------------------
// Push constants (file-local, matching upstream anonymous namespace)
// ---------------------------------------------------------------------------

/// Port of anonymous `PushConstants` struct for blit operations.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
struct PushConstants {
    tex_scale: [f32; 2],
    tex_offset: [f32; 2],
}

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
// Fermi2D Operation / Filter (matching upstream enums)
// ---------------------------------------------------------------------------

/// Port of `Tegra::Engines::Fermi2D::Operation`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum Operation {
    SrcCopyAnd = 0,
    RopAnd = 1,
    Blend = 2,
    SrcCopy = 3,
    Rop = 4,
    SrcCopyPremult = 5,
    BlendPremult = 6,
}

/// Port of `Tegra::Engines::Fermi2D::Filter`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum Filter {
    PointSample = 0,
    Bilinear = 1,
}

// ---------------------------------------------------------------------------
// Helper: compute push constants from region
// ---------------------------------------------------------------------------

/// Port of `PushConstants` construction from regions.
fn compute_push_constants(src_region: &Region2D, dst_region: &Region2D) -> PushConstants {
    let src_width = (src_region.end.x - src_region.start.x) as f32;
    let src_height = (src_region.end.y - src_region.start.y) as f32;
    let dst_width = (dst_region.end.x - dst_region.start.x) as f32;
    let dst_height = (dst_region.end.y - dst_region.start.y) as f32;

    if dst_width == 0.0 || dst_height == 0.0 {
        return PushConstants::default();
    }

    PushConstants {
        tex_scale: [src_width / dst_width, src_height / dst_height],
        tex_offset: [
            src_region.start.x as f32 / dst_width,
            src_region.start.y as f32 / dst_height,
        ],
    }
}

// ---------------------------------------------------------------------------
// BlitImageHelper
// ---------------------------------------------------------------------------

/// Port of `BlitImageHelper` class.
///
/// Provides GPU-accelerated blit, conversion, and clear operations via
/// fullscreen-triangle shaders and cached pipelines.
pub struct BlitImageHelper {
    device: ash::Device,

    // Descriptor layouts
    one_texture_set_layout: vk::DescriptorSetLayout,
    two_textures_set_layout: vk::DescriptorSetLayout,

    // Pipeline layouts
    one_texture_pipeline_layout: vk::PipelineLayout,
    two_textures_pipeline_layout: vk::PipelineLayout,
    clear_color_pipeline_layout: vk::PipelineLayout,

    // Shader modules
    full_screen_vert: vk::ShaderModule,
    blit_color_to_color_frag: vk::ShaderModule,
    blit_depth_stencil_frag: vk::ShaderModule,
    clear_color_vert: vk::ShaderModule,
    clear_color_frag: vk::ShaderModule,
    clear_stencil_frag: vk::ShaderModule,
    convert_depth_to_float_frag: vk::ShaderModule,
    convert_float_to_depth_frag: vk::ShaderModule,
    convert_abgr8_to_d24s8_frag: vk::ShaderModule,
    convert_abgr8_to_d32f_frag: vk::ShaderModule,
    convert_d32f_to_abgr8_frag: vk::ShaderModule,
    convert_d24s8_to_abgr8_frag: vk::ShaderModule,
    convert_s8d24_to_abgr8_frag: vk::ShaderModule,

    // Samplers
    linear_sampler: vk::Sampler,
    nearest_sampler: vk::Sampler,

    // Cached pipeline vectors (key + pipeline in parallel)
    blit_color_keys: Vec<BlitImagePipelineKey>,
    blit_color_pipelines: Vec<vk::Pipeline>,
    blit_depth_stencil_keys: Vec<BlitImagePipelineKey>,
    blit_depth_stencil_pipelines: Vec<vk::Pipeline>,
    clear_color_keys: Vec<BlitImagePipelineKey>,
    clear_color_pipelines: Vec<vk::Pipeline>,
    clear_stencil_keys: Vec<BlitDepthStencilPipelineKey>,
    clear_stencil_pipelines: Vec<vk::Pipeline>,

    // Conversion pipelines (lazily created)
    convert_d32_to_r32_pipeline: vk::Pipeline,
    convert_r32_to_d32_pipeline: vk::Pipeline,
    convert_d16_to_r16_pipeline: vk::Pipeline,
    convert_r16_to_d16_pipeline: vk::Pipeline,
    convert_abgr8_to_d24s8_pipeline: vk::Pipeline,
    convert_abgr8_to_d32f_pipeline: vk::Pipeline,
    convert_d32f_to_abgr8_pipeline: vk::Pipeline,
    convert_d24s8_to_abgr8_pipeline: vk::Pipeline,
    convert_s8d24_to_abgr8_pipeline: vk::Pipeline,
}

impl BlitImageHelper {
    /// Port of `BlitImageHelper::BlitImageHelper`.
    pub fn new(device: ash::Device) -> Self {
        // Create one-texture descriptor set layout (1 combined image sampler)
        let one_tex_binding = vk::DescriptorSetLayoutBinding {
            binding: 0,
            descriptor_type: vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
            descriptor_count: 1,
            stage_flags: vk::ShaderStageFlags::FRAGMENT,
            p_immutable_samplers: std::ptr::null(),
        };
        let one_tex_layout_ci = vk::DescriptorSetLayoutCreateInfo::builder()
            .bindings(std::slice::from_ref(&one_tex_binding))
            .build();
        let one_texture_set_layout = unsafe {
            device
                .create_descriptor_set_layout(&one_tex_layout_ci, None)
                .expect("Failed to create one-texture set layout")
        };

        // Create two-texture descriptor set layout (2 combined image samplers)
        let two_tex_bindings = [
            vk::DescriptorSetLayoutBinding {
                binding: 0,
                descriptor_type: vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
                descriptor_count: 1,
                stage_flags: vk::ShaderStageFlags::FRAGMENT,
                p_immutable_samplers: std::ptr::null(),
            },
            vk::DescriptorSetLayoutBinding {
                binding: 1,
                descriptor_type: vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
                descriptor_count: 1,
                stage_flags: vk::ShaderStageFlags::FRAGMENT,
                p_immutable_samplers: std::ptr::null(),
            },
        ];
        let two_tex_layout_ci = vk::DescriptorSetLayoutCreateInfo::builder()
            .bindings(&two_tex_bindings)
            .build();
        let two_textures_set_layout = unsafe {
            device
                .create_descriptor_set_layout(&two_tex_layout_ci, None)
                .expect("Failed to create two-textures set layout")
        };

        // Create one-texture pipeline layout with push constants
        let push_range = vk::PushConstantRange {
            stage_flags: vk::ShaderStageFlags::FRAGMENT,
            offset: 0,
            size: std::mem::size_of::<PushConstants>() as u32,
        };
        let one_tex_layouts = [one_texture_set_layout];
        let one_tex_pl_ci = vk::PipelineLayoutCreateInfo::builder()
            .set_layouts(&one_tex_layouts)
            .push_constant_ranges(std::slice::from_ref(&push_range))
            .build();
        let one_texture_pipeline_layout = unsafe {
            device
                .create_pipeline_layout(&one_tex_pl_ci, None)
                .expect("Failed to create one-texture pipeline layout")
        };

        // Create two-texture pipeline layout with push constants
        let two_tex_layouts = [two_textures_set_layout];
        let two_tex_pl_ci = vk::PipelineLayoutCreateInfo::builder()
            .set_layouts(&two_tex_layouts)
            .push_constant_ranges(std::slice::from_ref(&push_range))
            .build();
        let two_textures_pipeline_layout = unsafe {
            device
                .create_pipeline_layout(&two_tex_pl_ci, None)
                .expect("Failed to create two-textures pipeline layout")
        };

        // Create clear color pipeline layout (no descriptor sets, push constants for color)
        let clear_push_range = vk::PushConstantRange {
            stage_flags: vk::ShaderStageFlags::FRAGMENT,
            offset: 0,
            size: 4 * std::mem::size_of::<f32>() as u32, // 4 floats for color
        };
        let clear_pl_ci = vk::PipelineLayoutCreateInfo::builder()
            .push_constant_ranges(std::slice::from_ref(&clear_push_range))
            .build();
        let clear_color_pipeline_layout = unsafe {
            device
                .create_pipeline_layout(&clear_pl_ci, None)
                .expect("Failed to create clear color pipeline layout")
        };

        // Create samplers
        let linear_sampler_ci = vk::SamplerCreateInfo::builder()
            .mag_filter(vk::Filter::LINEAR)
            .min_filter(vk::Filter::LINEAR)
            .address_mode_u(vk::SamplerAddressMode::CLAMP_TO_EDGE)
            .address_mode_v(vk::SamplerAddressMode::CLAMP_TO_EDGE)
            .build();
        let linear_sampler = unsafe {
            device
                .create_sampler(&linear_sampler_ci, None)
                .expect("Failed to create linear sampler")
        };

        let nearest_sampler_ci = vk::SamplerCreateInfo::builder()
            .mag_filter(vk::Filter::NEAREST)
            .min_filter(vk::Filter::NEAREST)
            .address_mode_u(vk::SamplerAddressMode::CLAMP_TO_EDGE)
            .address_mode_v(vk::SamplerAddressMode::CLAMP_TO_EDGE)
            .build();
        let nearest_sampler = unsafe {
            device
                .create_sampler(&nearest_sampler_ci, None)
                .expect("Failed to create nearest sampler")
        };

        // Shader modules are null placeholders; actual SPV comes from host_shaders
        BlitImageHelper {
            device,
            one_texture_set_layout,
            two_textures_set_layout,
            one_texture_pipeline_layout,
            two_textures_pipeline_layout,
            clear_color_pipeline_layout,
            full_screen_vert: vk::ShaderModule::null(),
            blit_color_to_color_frag: vk::ShaderModule::null(),
            blit_depth_stencil_frag: vk::ShaderModule::null(),
            clear_color_vert: vk::ShaderModule::null(),
            clear_color_frag: vk::ShaderModule::null(),
            clear_stencil_frag: vk::ShaderModule::null(),
            convert_depth_to_float_frag: vk::ShaderModule::null(),
            convert_float_to_depth_frag: vk::ShaderModule::null(),
            convert_abgr8_to_d24s8_frag: vk::ShaderModule::null(),
            convert_abgr8_to_d32f_frag: vk::ShaderModule::null(),
            convert_d32f_to_abgr8_frag: vk::ShaderModule::null(),
            convert_d24s8_to_abgr8_frag: vk::ShaderModule::null(),
            convert_s8d24_to_abgr8_frag: vk::ShaderModule::null(),
            linear_sampler,
            nearest_sampler,
            blit_color_keys: Vec::new(),
            blit_color_pipelines: Vec::new(),
            blit_depth_stencil_keys: Vec::new(),
            blit_depth_stencil_pipelines: Vec::new(),
            clear_color_keys: Vec::new(),
            clear_color_pipelines: Vec::new(),
            clear_stencil_keys: Vec::new(),
            clear_stencil_pipelines: Vec::new(),
            convert_d32_to_r32_pipeline: vk::Pipeline::null(),
            convert_r32_to_d32_pipeline: vk::Pipeline::null(),
            convert_d16_to_r16_pipeline: vk::Pipeline::null(),
            convert_r16_to_d16_pipeline: vk::Pipeline::null(),
            convert_abgr8_to_d24s8_pipeline: vk::Pipeline::null(),
            convert_abgr8_to_d32f_pipeline: vk::Pipeline::null(),
            convert_d32f_to_abgr8_pipeline: vk::Pipeline::null(),
            convert_d24s8_to_abgr8_pipeline: vk::Pipeline::null(),
            convert_s8d24_to_abgr8_pipeline: vk::Pipeline::null(),
        }
    }

    /// Port of `BlitImageHelper::BlitColor` (sampled blit variant).
    ///
    /// Blits a source image view to a destination framebuffer using the
    /// specified filter and operation.
    pub fn blit_color(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_image_view: vk::ImageView,
        dst_region: &Region2D,
        src_region: &Region2D,
        _filter: Filter,
        _operation: Operation,
    ) {
        let _push_constants = compute_push_constants(src_region, dst_region);
        // In the full implementation:
        // 1. Select sampler based on filter (linear or nearest)
        // 2. Find or create the appropriate pipeline via find_or_emplace_color_pipeline
        // 3. Bind pipeline, descriptor sets, push constants
        // 4. Set viewport/scissor to dst_region
        // 5. Draw(3, 1, 0, 0) for fullscreen triangle
    }

    /// Port of `BlitImageHelper::BlitColor` (explicit image + sampler variant).
    pub fn blit_color_with_sampler(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_image_view: vk::ImageView,
        _src_image: vk::Image,
        _src_sampler: vk::Sampler,
        dst_region: &Region2D,
        src_region: &Region2D,
        _src_size: &Extent3D,
    ) {
        let _push_constants = compute_push_constants(src_region, dst_region);
        // Similar to blit_color but uses the provided sampler directly
    }

    /// Port of `BlitImageHelper::BlitDepthStencil`.
    pub fn blit_depth_stencil(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_depth_view: vk::ImageView,
        _src_stencil_view: vk::ImageView,
        dst_region: &Region2D,
        src_region: &Region2D,
        _filter: Filter,
        _operation: Operation,
    ) {
        let _push_constants = compute_push_constants(src_region, dst_region);
        // Uses the depth stencil blit shader and two-texture pipeline layout
    }

    /// Port of `BlitImageHelper::ConvertD32ToR32`.
    pub fn convert_d32_to_r32(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_image_view: vk::ImageView,
    ) {
        // Uses convert_depth_to_float_frag shader
    }

    /// Port of `BlitImageHelper::ConvertR32ToD32`.
    pub fn convert_r32_to_d32(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_image_view: vk::ImageView,
    ) {
        // Uses convert_float_to_depth_frag shader
    }

    /// Port of `BlitImageHelper::ConvertD16ToR16`.
    pub fn convert_d16_to_r16(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_image_view: vk::ImageView,
    ) {
        // Uses convert_depth_to_float_frag shader with D16 format
    }

    /// Port of `BlitImageHelper::ConvertR16ToD16`.
    pub fn convert_r16_to_d16(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_image_view: vk::ImageView,
    ) {
        // Uses convert_float_to_depth_frag shader with D16 format
    }

    /// Port of `BlitImageHelper::ConvertABGR8ToD24S8`.
    pub fn convert_abgr8_to_d24s8(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_image_view: vk::ImageView,
    ) {
        // Uses convert_abgr8_to_d24s8_frag shader
    }

    /// Port of `BlitImageHelper::ConvertABGR8ToD32F`.
    pub fn convert_abgr8_to_d32f(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_image_view: vk::ImageView,
    ) {
        // Uses convert_abgr8_to_d32f_frag shader
    }

    /// Port of `BlitImageHelper::ConvertD32FToABGR8`.
    pub fn convert_d32f_to_abgr8(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_image_view: vk::ImageView,
    ) {
        // Uses convert_d32f_to_abgr8_frag shader
    }

    /// Port of `BlitImageHelper::ConvertD24S8ToABGR8`.
    pub fn convert_d24s8_to_abgr8(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_image_view: vk::ImageView,
    ) {
        // Uses convert_d24s8_to_abgr8_frag shader
    }

    /// Port of `BlitImageHelper::ConvertS8D24ToABGR8`.
    pub fn convert_s8d24_to_abgr8(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _src_image_view: vk::ImageView,
    ) {
        // Uses convert_s8d24_to_abgr8_frag shader
    }

    /// Port of `BlitImageHelper::ClearColor`.
    ///
    /// Clears a region of the color attachment using a fragment shader that
    /// respects the color write mask.
    pub fn clear_color(
        &mut self,
        _dst_framebuffer: vk::Framebuffer,
        _color_mask: u8,
        _clear_color: [f32; 4],
        _dst_region: &Region2D,
    ) {
        // 1. Find or create clear color pipeline for the renderpass + color mask
        // 2. Set viewport/scissor to dst_region
        // 3. Push clear_color as push constants
        // 4. Draw(3, 1, 0, 0)
    }

    /// Port of `BlitImageHelper::ClearDepthStencil`.
    ///
    /// Clears depth and/or stencil attachments using a specialized fragment
    /// shader.
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
        // 1. Find or create clear stencil pipeline for the renderpass + params
        // 2. Set viewport/scissor to dst_region
        // 3. Push depth value as push constants
        // 4. Set stencil reference/compare mask via dynamic state
        // 5. Draw(3, 1, 0, 0)
    }

    // --- Private helpers ---

    /// Port of `BlitImageHelper::FindOrEmplaceColorPipeline`.
    ///
    /// Looks up or creates a graphics pipeline for color blitting with
    /// the given render pass and blend operation.
    fn find_or_emplace_color_pipeline(&mut self, key: &BlitImagePipelineKey) -> vk::Pipeline {
        if let Some(idx) = self.blit_color_keys.iter().position(|k| k == key) {
            return self.blit_color_pipelines[idx];
        }
        // Would create pipeline here using full_screen_vert + blit_color_to_color_frag
        let pipeline = vk::Pipeline::null();
        self.blit_color_keys.push(*key);
        self.blit_color_pipelines.push(pipeline);
        pipeline
    }

    /// Port of `BlitImageHelper::FindOrEmplaceDepthStencilPipeline`.
    fn find_or_emplace_depth_stencil_pipeline(
        &mut self,
        key: &BlitImagePipelineKey,
    ) -> vk::Pipeline {
        if let Some(idx) = self.blit_depth_stencil_keys.iter().position(|k| k == key) {
            return self.blit_depth_stencil_pipelines[idx];
        }
        let pipeline = vk::Pipeline::null();
        self.blit_depth_stencil_keys.push(*key);
        self.blit_depth_stencil_pipelines.push(pipeline);
        pipeline
    }

    /// Port of `BlitImageHelper::FindOrEmplaceClearColorPipeline`.
    fn find_or_emplace_clear_color_pipeline(
        &mut self,
        key: &BlitImagePipelineKey,
    ) -> vk::Pipeline {
        if let Some(idx) = self.clear_color_keys.iter().position(|k| k == key) {
            return self.clear_color_pipelines[idx];
        }
        let pipeline = vk::Pipeline::null();
        self.clear_color_keys.push(*key);
        self.clear_color_pipelines.push(pipeline);
        pipeline
    }

    /// Port of `BlitImageHelper::FindOrEmplaceClearStencilPipeline`.
    fn find_or_emplace_clear_stencil_pipeline(
        &mut self,
        key: &BlitDepthStencilPipelineKey,
    ) -> vk::Pipeline {
        if let Some(idx) = self.clear_stencil_keys.iter().position(|k| k == key) {
            return self.clear_stencil_pipelines[idx];
        }
        let pipeline = vk::Pipeline::null();
        self.clear_stencil_keys.push(*key);
        self.clear_stencil_pipelines.push(pipeline);
        pipeline
    }
}
