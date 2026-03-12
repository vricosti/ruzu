// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/fxaa.h` / `present/fxaa.cpp`.
//!
//! Fast Approximate Anti-Aliasing (FXAA) post-processing pass.

use ash::vk;

use super::anti_alias_pass::AntiAliasPass;
use super::util;

// ---------------------------------------------------------------------------
// Per-image dynamic resources
// ---------------------------------------------------------------------------

/// Port of `FXAA::Image` inner struct.
struct FxaaImage {
    descriptor_sets: Vec<vk::DescriptorSet>,
    framebuffer: vk::Framebuffer,
    image: vk::Image,
    image_view: vk::ImageView,
}

// ---------------------------------------------------------------------------
// FXAA
// ---------------------------------------------------------------------------

/// Port of `FXAA` class.
///
/// Single-pass FXAA anti-aliasing using a fullscreen fragment shader.
pub struct Fxaa {
    device: ash::Device,
    extent: vk::Extent2D,
    image_count: u32,
    images_ready: bool,
    dynamic_images: Vec<FxaaImage>,

    vertex_shader: vk::ShaderModule,
    fragment_shader: vk::ShaderModule,
    descriptor_pool: vk::DescriptorPool,
    descriptor_set_layout: vk::DescriptorSetLayout,
    pipeline_layout: vk::PipelineLayout,
    pipeline: vk::Pipeline,
    renderpass: vk::RenderPass,
    sampler: vk::Sampler,
}

impl Fxaa {
    /// Port of `FXAA::FXAA`.
    pub fn new(
        device: ash::Device,
        allocator: &vk::DeviceMemory,
        extent: vk::Extent2D,
        image_count: usize,
    ) -> Self {
        let image_count_u32 = image_count as u32;

        // Create images
        let mut dynamic_images = Vec::with_capacity(image_count);
        for _i in 0..image_count {
            let image = util::create_wrapped_image(
                &device,
                allocator,
                extent,
                vk::Format::R16G16B16A16_SFLOAT,
            );
            let image_view = util::create_wrapped_image_view(
                &device,
                image,
                vk::Format::R16G16B16A16_SFLOAT,
            );
            dynamic_images.push(FxaaImage {
                descriptor_sets: Vec::new(),
                framebuffer: vk::Framebuffer::null(),
                image,
                image_view,
            });
        }

        // Create render pass
        let renderpass = util::create_wrapped_render_pass(
            &device,
            vk::Format::R16G16B16A16_SFLOAT,
            vk::ImageLayout::UNDEFINED,
        );

        // Create framebuffers
        for img in &mut dynamic_images {
            img.framebuffer = util::create_wrapped_framebuffer(
                &device,
                renderpass,
                img.image_view,
                extent,
            );
        }

        // Create sampler
        let sampler = util::create_wrapped_sampler(&device, vk::Filter::LINEAR);

        // Create shaders - placeholder empty SPIR-V (actual shader data would come from host_shaders)
        let vertex_shader = vk::ShaderModule::null();
        let fragment_shader = vk::ShaderModule::null();

        // Create descriptor pool: 2 descriptors, 1 descriptor set per image
        let descriptor_pool = util::create_wrapped_descriptor_pool(
            &device,
            2 * image_count_u32,
            image_count_u32,
            &[vk::DescriptorType::COMBINED_IMAGE_SAMPLER],
        );

        // Create descriptor set layout
        let descriptor_set_layout = util::create_wrapped_descriptor_set_layout(
            &device,
            &[
                vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
                vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
            ],
        );

        // Create descriptor sets
        for img in &mut dynamic_images {
            img.descriptor_sets = util::create_wrapped_descriptor_sets(
                &device,
                descriptor_pool,
                &[descriptor_set_layout],
            );
        }

        // Create pipeline layout
        let pipeline_layout = util::create_wrapped_pipeline_layout(
            &device,
            descriptor_set_layout,
        );

        // Create pipeline
        let pipeline = util::create_wrapped_pipeline(
            &device,
            renderpass,
            pipeline_layout,
            vertex_shader,
            fragment_shader,
        );

        Fxaa {
            device,
            extent,
            image_count: image_count_u32,
            images_ready: false,
            dynamic_images,
            vertex_shader,
            fragment_shader,
            descriptor_pool,
            descriptor_set_layout,
            pipeline_layout,
            pipeline,
            renderpass,
            sampler,
        }
    }

    /// Port of `FXAA::UpdateDescriptorSets`.
    fn update_descriptor_sets(&self, image_view: vk::ImageView, image_index: usize) {
        let image = &self.dynamic_images[image_index];
        let mut image_infos = Vec::with_capacity(2);
        let mut updates = Vec::new();

        updates.push(util::create_write_descriptor_set(
            &mut image_infos,
            self.sampler,
            image_view,
            image.descriptor_sets[0],
            0,
        ));
        updates.push(util::create_write_descriptor_set(
            &mut image_infos,
            self.sampler,
            image_view,
            image.descriptor_sets[0],
            1,
        ));

        unsafe {
            self.device.update_descriptor_sets(&updates, &[]);
        }
    }

    /// Port of `FXAA::UploadImages`.
    fn upload_images(&mut self, cmdbuf: vk::CommandBuffer) {
        if self.images_ready {
            return;
        }

        for img in &self.dynamic_images {
            util::clear_color_image(&self.device, cmdbuf, img.image);
        }

        self.images_ready = true;
    }
}

impl AntiAliasPass for Fxaa {
    /// Port of `FXAA::Draw`.
    ///
    /// Records commands to apply FXAA to the input image and swaps the
    /// image/view pointers to the output.
    fn draw(
        &mut self,
        image_index: usize,
        inout_image: &mut vk::Image,
        inout_image_view: &mut vk::ImageView,
    ) {
        let image = &self.dynamic_images[image_index];
        let _input_image = *inout_image;
        let _output_image = image.image;
        let _descriptor_set = image.descriptor_sets[0];
        let _framebuffer = image.framebuffer;
        let _renderpass = self.renderpass;
        let _pipeline = self.pipeline;
        let _layout = self.pipeline_layout;
        let _extent = self.extent;

        self.update_descriptor_sets(*inout_image_view, image_index);

        // NOTE: actual command recording requires a scheduler/command buffer
        // which will be connected when the scheduler is fully ported.
        // The draw would:
        // 1. TransitionImageLayout(cmdbuf, input_image, VK_IMAGE_LAYOUT_GENERAL)
        // 2. TransitionImageLayout(cmdbuf, output_image, VK_IMAGE_LAYOUT_GENERAL)
        // 3. BeginRenderPass(cmdbuf, renderpass, framebuffer, extent)
        // 4. BindPipeline(GRAPHICS, pipeline)
        // 5. BindDescriptorSets(GRAPHICS, layout, 0, descriptor_set)
        // 6. Draw(3, 1, 0, 0)
        // 7. EndRenderPass()
        // 8. TransitionImageLayout(cmdbuf, output_image, VK_IMAGE_LAYOUT_GENERAL)

        *inout_image = image.image;
        *inout_image_view = image.image_view;
    }
}
