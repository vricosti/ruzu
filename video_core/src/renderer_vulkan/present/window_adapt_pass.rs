// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/window_adapt_pass.h` / `present/window_adapt_pass.cpp`.
//!
//! Final presentation pass that composites layers into the destination frame
//! using opaque, premultiplied, or coverage blending pipelines.

use ash::vk;

// ---------------------------------------------------------------------------
// WindowAdaptPass
// ---------------------------------------------------------------------------

/// Port of `WindowAdaptPass` class.
///
/// Owns the render pass, pipelines (opaque, premultiplied, coverage),
/// descriptor set layout, pipeline layout, sampler, and shaders for
/// compositing presentation layers into the swapchain frame.
pub struct WindowAdaptPass {
    _descriptor_set_layout: vk::DescriptorSetLayout,
    _pipeline_layout: vk::PipelineLayout,
    _sampler: vk::Sampler,
    _vertex_shader: vk::ShaderModule,
    _fragment_shader: vk::ShaderModule,
    _render_pass: vk::RenderPass,
    _opaque_pipeline: vk::Pipeline,
    _premultiplied_pipeline: vk::Pipeline,
    _coverage_pipeline: vk::Pipeline,
}

impl WindowAdaptPass {
    /// Port of `WindowAdaptPass::WindowAdaptPass`.
    pub fn new(
        _frame_format: vk::Format,
        _sampler: vk::Sampler,
        _fragment_shader: vk::ShaderModule,
    ) -> Self {
        todo!("WindowAdaptPass::new")
    }

    /// Port of `WindowAdaptPass::Draw`.
    pub fn draw(
        &mut self,
        _image_index: usize,
        _dst: &mut super::super::present_manager::Frame,
    ) {
        todo!("WindowAdaptPass::draw")
    }

    /// Port of `WindowAdaptPass::GetDescriptorSetLayout`.
    pub fn get_descriptor_set_layout(&self) -> vk::DescriptorSetLayout {
        self._descriptor_set_layout
    }

    /// Port of `WindowAdaptPass::GetRenderPass`.
    pub fn get_render_pass(&self) -> vk::RenderPass {
        self._render_pass
    }

    // --- Private ---
    fn create_descriptor_set_layout(&mut self) { todo!() }
    fn create_pipeline_layout(&mut self) { todo!() }
    fn create_vertex_shader(&mut self) { todo!() }
    fn create_render_pass(&mut self, _frame_format: vk::Format) { todo!() }
    fn create_pipelines(&mut self) { todo!() }
}
