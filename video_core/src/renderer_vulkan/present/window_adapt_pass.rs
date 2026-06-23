// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/window_adapt_pass.h` / `present/window_adapt_pass.cpp`.
//!
//! Final presentation pass that composites layers into the destination frame
//! using opaque, premultiplied, or coverage blending pipelines.

use ash::vk;

use crate::host_shaders::spirv_shaders::VULKAN_PRESENT_VERT_SPV;
use crate::renderer_vulkan::scheduler::Scheduler;
use crate::renderer_vulkan::shader_util::build_shader;

use super::present_push_constants::PresentPushConstants;
use super::util;

// ---------------------------------------------------------------------------
// WindowAdaptPass
// ---------------------------------------------------------------------------

/// Port of `WindowAdaptPass` class.
///
/// Owns the render pass, pipelines (opaque, premultiplied, coverage),
/// descriptor set layout, pipeline layout, sampler, and shaders for
/// compositing presentation layers into the swapchain frame.
pub struct WindowAdaptPass {
    device: ash::Device,
    descriptor_set_layout: vk::DescriptorSetLayout,
    pipeline_layout: vk::PipelineLayout,
    sampler: vk::Sampler,
    vertex_shader: vk::ShaderModule,
    fragment_shader: vk::ShaderModule,
    render_pass: vk::RenderPass,
    opaque_pipeline: vk::Pipeline,
    premultiplied_pipeline: vk::Pipeline,
    coverage_pipeline: vk::Pipeline,
}

impl WindowAdaptPass {
    /// Port of `WindowAdaptPass::WindowAdaptPass`.
    pub fn new(
        device: ash::Device,
        frame_format: vk::Format,
        sampler: vk::Sampler,
        fragment_shader: vk::ShaderModule,
    ) -> Self {
        // Create descriptor set layout: 1 combined image sampler
        let descriptor_set_layout = util::create_wrapped_descriptor_set_layout(
            &device,
            &[vk::DescriptorType::COMBINED_IMAGE_SAMPLER],
        );

        // Create pipeline layout with push constants for vertex stage
        let push_constant_range = vk::PushConstantRange {
            stage_flags: vk::ShaderStageFlags::VERTEX,
            offset: 0,
            size: std::mem::size_of::<PresentPushConstants>() as u32,
        };
        let set_layouts = [descriptor_set_layout];
        let pipeline_layout_ci = vk::PipelineLayoutCreateInfo::builder()
            .set_layouts(&set_layouts)
            .push_constant_ranges(std::slice::from_ref(&push_constant_range))
            .build();
        let pipeline_layout = unsafe {
            device
                .create_pipeline_layout(&pipeline_layout_ci, None)
                .expect("Failed to create WindowAdaptPass pipeline layout")
        };

        let vertex_shader = build_shader(&device, VULKAN_PRESENT_VERT_SPV)
            .expect("Failed to build vulkan_present.vert");

        // Create render pass
        let render_pass =
            util::create_wrapped_render_pass(&device, frame_format, vk::ImageLayout::UNDEFINED);

        // Create pipelines
        let opaque_pipeline = util::create_wrapped_pipeline(
            &device,
            render_pass,
            pipeline_layout,
            vertex_shader,
            fragment_shader,
        );
        let premultiplied_pipeline = util::create_wrapped_premultiplied_blending_pipeline(
            &device,
            render_pass,
            pipeline_layout,
            vertex_shader,
            fragment_shader,
        );
        let coverage_pipeline = util::create_wrapped_coverage_blending_pipeline(
            &device,
            render_pass,
            pipeline_layout,
            vertex_shader,
            fragment_shader,
        );

        WindowAdaptPass {
            device,
            descriptor_set_layout,
            pipeline_layout,
            sampler,
            vertex_shader,
            fragment_shader,
            render_pass,
            opaque_pipeline,
            premultiplied_pipeline,
            coverage_pipeline,
        }
    }

    /// Port of `WindowAdaptPass::GetDescriptorSetLayout`.
    pub fn get_descriptor_set_layout(&self) -> vk::DescriptorSetLayout {
        self.descriptor_set_layout
    }

    /// Port of `WindowAdaptPass::GetRenderPass`.
    pub fn get_render_pass(&self) -> vk::RenderPass {
        self.render_pass
    }

    /// Port access to the sampler owned by `WindowAdaptPass`.
    pub fn get_sampler(&self) -> vk::Sampler {
        self.sampler
    }

    /// Port of `WindowAdaptPass::Draw`.
    ///
    /// Composites one or more layers into the destination frame. Each layer
    /// has its own push constants, descriptor set, and blending mode.
    pub fn draw(
        &self,
        scheduler: &mut Scheduler,
        push_constants_list: &[PresentPushConstants],
        descriptor_sets: &[vk::DescriptorSet],
        blend_modes: &[BlendMode],
        dst_framebuffer: vk::Framebuffer,
        render_area: vk::Extent2D,
        bg_color: [f32; 4],
    ) {
        let layer_count = push_constants_list.len();

        let graphics_pipelines: Vec<vk::Pipeline> = blend_modes
            .iter()
            .map(|mode| match mode {
                BlendMode::Opaque => self.opaque_pipeline,
                BlendMode::Premultiplied => self.premultiplied_pipeline,
                BlendMode::Coverage => self.coverage_pipeline,
            })
            .collect();
        let device = self.device.clone();
        let render_pass = self.render_pass;
        let pipeline_layout = self.pipeline_layout;
        let push_constants = push_constants_list.to_vec();
        let descriptor_sets = descriptor_sets.to_vec();

        scheduler.record(move |cmdbuf| unsafe {
            util::begin_render_pass(&device, cmdbuf, render_pass, dst_framebuffer, render_area);

            let clear_attachment = vk::ClearAttachment {
                aspect_mask: vk::ImageAspectFlags::COLOR,
                color_attachment: 0,
                clear_value: vk::ClearValue {
                    color: vk::ClearColorValue { float32: bg_color },
                },
            };
            let clear_rect = vk::ClearRect {
                rect: vk::Rect2D {
                    offset: vk::Offset2D { x: 0, y: 0 },
                    extent: render_area,
                },
                base_array_layer: 0,
                layer_count: 1,
            };
            device.cmd_clear_attachments(cmdbuf, &[clear_attachment], &[clear_rect]);

            for i in 0..layer_count {
                device.cmd_bind_pipeline(
                    cmdbuf,
                    vk::PipelineBindPoint::GRAPHICS,
                    graphics_pipelines[i],
                );

                let constants_bytes: &[u8] = std::slice::from_raw_parts(
                    &push_constants[i] as *const PresentPushConstants as *const u8,
                    std::mem::size_of::<PresentPushConstants>(),
                );
                device.cmd_push_constants(
                    cmdbuf,
                    pipeline_layout,
                    vk::ShaderStageFlags::VERTEX,
                    0,
                    constants_bytes,
                );

                device.cmd_bind_descriptor_sets(
                    cmdbuf,
                    vk::PipelineBindPoint::GRAPHICS,
                    pipeline_layout,
                    0,
                    &[descriptor_sets[i]],
                    &[],
                );

                device.cmd_draw(cmdbuf, 4, 1, 0, 0);
            }

            device.cmd_end_render_pass(cmdbuf);
        });
    }
}

impl Drop for WindowAdaptPass {
    fn drop(&mut self) {
        unsafe {
            if self.coverage_pipeline != vk::Pipeline::null() {
                self.device.destroy_pipeline(self.coverage_pipeline, None);
                self.coverage_pipeline = vk::Pipeline::null();
            }
            if self.premultiplied_pipeline != vk::Pipeline::null() {
                self.device
                    .destroy_pipeline(self.premultiplied_pipeline, None);
                self.premultiplied_pipeline = vk::Pipeline::null();
            }
            if self.opaque_pipeline != vk::Pipeline::null() {
                self.device.destroy_pipeline(self.opaque_pipeline, None);
                self.opaque_pipeline = vk::Pipeline::null();
            }
            if self.render_pass != vk::RenderPass::null() {
                self.device.destroy_render_pass(self.render_pass, None);
                self.render_pass = vk::RenderPass::null();
            }
            if self.fragment_shader != vk::ShaderModule::null() {
                self.device
                    .destroy_shader_module(self.fragment_shader, None);
                self.fragment_shader = vk::ShaderModule::null();
            }
            if self.vertex_shader != vk::ShaderModule::null() {
                self.device.destroy_shader_module(self.vertex_shader, None);
                self.vertex_shader = vk::ShaderModule::null();
            }
            if self.sampler != vk::Sampler::null() {
                self.device.destroy_sampler(self.sampler, None);
                self.sampler = vk::Sampler::null();
            }
            if self.pipeline_layout != vk::PipelineLayout::null() {
                self.device
                    .destroy_pipeline_layout(self.pipeline_layout, None);
                self.pipeline_layout = vk::PipelineLayout::null();
            }
            if self.descriptor_set_layout != vk::DescriptorSetLayout::null() {
                self.device
                    .destroy_descriptor_set_layout(self.descriptor_set_layout, None);
                self.descriptor_set_layout = vk::DescriptorSetLayout::null();
            }
        }
    }
}

/// Blend mode for a presentation layer, matching upstream `Tegra::BlendMode`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlendMode {
    Opaque,
    Premultiplied,
    Coverage,
}
