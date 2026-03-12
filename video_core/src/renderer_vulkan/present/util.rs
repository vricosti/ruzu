// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/util.h` / `present/util.cpp`.
//!
//! Utility functions for creating wrapped Vulkan objects used during
//! frame presentation: buffers, images, image views, render passes,
//! framebuffers, samplers, shaders, descriptor pools/sets/layouts,
//! pipeline layouts, and pipelines.

use ash::vk;

/// Port of `CreateWrappedBuffer`.
pub fn create_wrapped_buffer(
    _size: vk::DeviceSize,
    _usage: u32,
) -> vk::Buffer {
    todo!("create_wrapped_buffer")
}

/// Port of `CreateWrappedImage`.
pub fn create_wrapped_image(
    _dimensions: vk::Extent2D,
    _format: vk::Format,
) -> vk::Image {
    todo!("create_wrapped_image")
}

/// Port of `TransitionImageLayout`.
pub fn transition_image_layout(
    _cmdbuf: vk::CommandBuffer,
    _image: vk::Image,
    _target_layout: vk::ImageLayout,
    _source_layout: vk::ImageLayout,
) {
    todo!("transition_image_layout")
}

/// Port of `UploadImage`.
pub fn upload_image(
    _image: vk::Image,
    _dimensions: vk::Extent2D,
    _format: vk::Format,
    _initial_contents: &[u8],
) {
    todo!("upload_image")
}

/// Port of `DownloadColorImage`.
pub fn download_color_image(
    _cmdbuf: vk::CommandBuffer,
    _image: vk::Image,
    _buffer: vk::Buffer,
    _extent: vk::Extent3D,
) {
    todo!("download_color_image")
}

/// Port of `ClearColorImage`.
pub fn clear_color_image(
    _cmdbuf: vk::CommandBuffer,
    _image: vk::Image,
) {
    todo!("clear_color_image")
}

/// Port of `CreateWrappedImageView`.
pub fn create_wrapped_image_view(
    _image: vk::Image,
    _format: vk::Format,
) -> vk::ImageView {
    todo!("create_wrapped_image_view")
}

/// Port of `CreateWrappedRenderPass`.
pub fn create_wrapped_render_pass(
    _format: vk::Format,
    _initial_layout: vk::ImageLayout,
) -> vk::RenderPass {
    todo!("create_wrapped_render_pass")
}

/// Port of `CreateWrappedFramebuffer`.
pub fn create_wrapped_framebuffer(
    _render_pass: vk::RenderPass,
    _dest_image: vk::ImageView,
    _extent: vk::Extent2D,
) -> vk::Framebuffer {
    todo!("create_wrapped_framebuffer")
}

/// Port of `CreateWrappedSampler`.
pub fn create_wrapped_sampler(
    _filter: vk::Filter,
) -> vk::Sampler {
    todo!("create_wrapped_sampler")
}

/// Port of `CreateWrappedShaderModule`.
pub fn create_wrapped_shader_module(
    _code: &[u32],
) -> vk::ShaderModule {
    todo!("create_wrapped_shader_module")
}

/// Port of `CreateWrappedDescriptorPool`.
pub fn create_wrapped_descriptor_pool(
    _max_descriptors: usize,
    _max_sets: usize,
    _types: &[vk::DescriptorType],
) -> vk::DescriptorPool {
    todo!("create_wrapped_descriptor_pool")
}

/// Port of `CreateWrappedDescriptorSetLayout`.
pub fn create_wrapped_descriptor_set_layout(
    _types: &[vk::DescriptorType],
) -> vk::DescriptorSetLayout {
    todo!("create_wrapped_descriptor_set_layout")
}

/// Port of `CreateWrappedDescriptorSets`.
pub fn create_wrapped_descriptor_sets(
    _pool: vk::DescriptorPool,
    _layouts: &[vk::DescriptorSetLayout],
) -> Vec<vk::DescriptorSet> {
    todo!("create_wrapped_descriptor_sets")
}

/// Port of `CreateWrappedPipelineLayout`.
pub fn create_wrapped_pipeline_layout(
    _layout: vk::DescriptorSetLayout,
) -> vk::PipelineLayout {
    todo!("create_wrapped_pipeline_layout")
}

/// Port of `CreateWrappedPipeline`.
pub fn create_wrapped_pipeline(
    _renderpass: vk::RenderPass,
    _layout: vk::PipelineLayout,
    _vert_shader: vk::ShaderModule,
    _frag_shader: vk::ShaderModule,
) -> vk::Pipeline {
    todo!("create_wrapped_pipeline")
}

/// Port of `CreateWrappedPremultipliedBlendingPipeline`.
pub fn create_wrapped_premultiplied_blending_pipeline(
    _renderpass: vk::RenderPass,
    _layout: vk::PipelineLayout,
    _vert_shader: vk::ShaderModule,
    _frag_shader: vk::ShaderModule,
) -> vk::Pipeline {
    todo!("create_wrapped_premultiplied_blending_pipeline")
}

/// Port of `CreateWrappedCoverageBlendingPipeline`.
pub fn create_wrapped_coverage_blending_pipeline(
    _renderpass: vk::RenderPass,
    _layout: vk::PipelineLayout,
    _vert_shader: vk::ShaderModule,
    _frag_shader: vk::ShaderModule,
) -> vk::Pipeline {
    todo!("create_wrapped_coverage_blending_pipeline")
}

/// Port of `CreateWriteDescriptorSet`.
pub fn create_write_descriptor_set(
    _sampler: vk::Sampler,
    _view: vk::ImageView,
    _set: vk::DescriptorSet,
    _binding: u32,
) -> vk::WriteDescriptorSet {
    todo!("create_write_descriptor_set")
}

/// Port of `CreateBilinearSampler`.
pub fn create_bilinear_sampler() -> vk::Sampler {
    todo!("create_bilinear_sampler")
}

/// Port of `CreateNearestNeighborSampler`.
pub fn create_nearest_neighbor_sampler() -> vk::Sampler {
    todo!("create_nearest_neighbor_sampler")
}

/// Port of `BeginRenderPass`.
pub fn begin_render_pass(
    _cmdbuf: vk::CommandBuffer,
    _render_pass: vk::RenderPass,
    _framebuffer: vk::Framebuffer,
    _extent: vk::Extent2D,
) {
    todo!("begin_render_pass")
}
