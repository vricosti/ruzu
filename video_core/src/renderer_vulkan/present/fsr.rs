// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/fsr.h` / `present/fsr.cpp`.
//!
//! AMD FidelityFX Super Resolution (FSR) 1.0 upscaling pass.

use ash::vk;

use super::util;

// ---------------------------------------------------------------------------
// FSR stage enum
// ---------------------------------------------------------------------------

/// Port of `FSR::FsrStage` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum FsrStage {
    Easu = 0,
    Rcas = 1,
}

/// Total number of FSR stages.
pub const MAX_FSR_STAGE: usize = 2;

/// Port of FSR push constants (4x4 u32 array).
type PushConstants = [u32; 4 * 4];

// ---------------------------------------------------------------------------
// Per-image dynamic resources
// ---------------------------------------------------------------------------

/// Port of `FSR::Images` inner struct.
pub struct FsrImages {
    pub descriptor_sets: Vec<vk::DescriptorSet>,
    pub images: [vk::Image; MAX_FSR_STAGE],
    pub image_views: [vk::ImageView; MAX_FSR_STAGE],
    pub framebuffers: [vk::Framebuffer; MAX_FSR_STAGE],
}

// ---------------------------------------------------------------------------
// FSR
// ---------------------------------------------------------------------------

/// Port of `FSR` class.
///
/// Two-pass (EASU + RCAS) FidelityFX Super Resolution upscaling.
pub struct Fsr {
    device: ash::Device,
    image_count: usize,
    extent: vk::Extent2D,
    images_ready: bool,
    dynamic_images: Vec<FsrImages>,

    descriptor_pool: vk::DescriptorPool,
    descriptor_set_layout: vk::DescriptorSetLayout,
    pipeline_layout: vk::PipelineLayout,
    vert_shader: vk::ShaderModule,
    easu_shader: vk::ShaderModule,
    rcas_shader: vk::ShaderModule,
    easu_pipeline: vk::Pipeline,
    rcas_pipeline: vk::Pipeline,
    renderpass: vk::RenderPass,
    sampler: vk::Sampler,
}

impl Fsr {
    /// Port of `FSR::FSR`.
    pub fn new(
        device: ash::Device,
        allocator: &vk::DeviceMemory,
        image_count: usize,
        extent: vk::Extent2D,
    ) -> Self {
        // Create images
        let mut dynamic_images = Vec::with_capacity(image_count);
        for _i in 0..image_count {
            let easu_image = util::create_wrapped_image(
                &device,
                allocator,
                extent,
                vk::Format::R16G16B16A16_SFLOAT,
            );
            let rcas_image = util::create_wrapped_image(
                &device,
                allocator,
                extent,
                vk::Format::R16G16B16A16_SFLOAT,
            );
            let easu_view = util::create_wrapped_image_view(
                &device,
                easu_image,
                vk::Format::R16G16B16A16_SFLOAT,
            );
            let rcas_view = util::create_wrapped_image_view(
                &device,
                rcas_image,
                vk::Format::R16G16B16A16_SFLOAT,
            );
            dynamic_images.push(FsrImages {
                descriptor_sets: Vec::new(),
                images: [easu_image, rcas_image],
                image_views: [easu_view, rcas_view],
                framebuffers: [vk::Framebuffer::null(); MAX_FSR_STAGE],
            });
        }

        // Create render pass
        let renderpass = util::create_wrapped_render_pass(
            &device,
            vk::Format::R16G16B16A16_SFLOAT,
            vk::ImageLayout::UNDEFINED,
        );

        // Create framebuffers
        for imgs in &mut dynamic_images {
            imgs.framebuffers[FsrStage::Easu as usize] = util::create_wrapped_framebuffer(
                &device,
                renderpass,
                imgs.image_views[FsrStage::Easu as usize],
                extent,
            );
            imgs.framebuffers[FsrStage::Rcas as usize] = util::create_wrapped_framebuffer(
                &device,
                renderpass,
                imgs.image_views[FsrStage::Rcas as usize],
                extent,
            );
        }

        // Create sampler (bilinear)
        let sampler = util::create_bilinear_sampler(&device);

        // Shaders would come from host_shaders; null placeholders for now
        let vert_shader = vk::ShaderModule::null();
        let easu_shader = vk::ShaderModule::null();
        let rcas_shader = vk::ShaderModule::null();

        // Descriptor pool: 2 descriptors, 2 sets per image
        let image_count_u32 = image_count as u32;
        let descriptor_pool = util::create_wrapped_descriptor_pool(
            &device,
            2 * image_count_u32,
            2 * image_count_u32,
            &[vk::DescriptorType::COMBINED_IMAGE_SAMPLER],
        );

        // Descriptor set layout: 1 combined image sampler
        let descriptor_set_layout = util::create_wrapped_descriptor_set_layout(
            &device,
            &[vk::DescriptorType::COMBINED_IMAGE_SAMPLER],
        );

        // Create descriptor sets (2 per image, one for EASU and one for RCAS)
        let layouts = vec![descriptor_set_layout; MAX_FSR_STAGE];
        for imgs in &mut dynamic_images {
            imgs.descriptor_sets =
                util::create_wrapped_descriptor_sets(&device, descriptor_pool, &layouts);
        }

        // Pipeline layout with push constants
        let push_constant_range = vk::PushConstantRange {
            stage_flags: vk::ShaderStageFlags::FRAGMENT,
            offset: 0,
            size: std::mem::size_of::<PushConstants>() as u32,
        };
        let set_layouts = [descriptor_set_layout];
        let pipeline_layout_ci = vk::PipelineLayoutCreateInfo::builder()
            .set_layouts(&set_layouts)
            .push_constant_ranges(std::slice::from_ref(&push_constant_range))
            .build();
        let pipeline_layout = unsafe {
            device
                .create_pipeline_layout(&pipeline_layout_ci, None)
                .expect("Failed to create FSR pipeline layout")
        };

        // Create pipelines
        let easu_pipeline = util::create_wrapped_pipeline(
            &device,
            renderpass,
            pipeline_layout,
            vert_shader,
            easu_shader,
        );
        let rcas_pipeline = util::create_wrapped_pipeline(
            &device,
            renderpass,
            pipeline_layout,
            vert_shader,
            rcas_shader,
        );

        Fsr {
            device,
            image_count,
            extent,
            images_ready: false,
            dynamic_images,
            descriptor_pool,
            descriptor_set_layout,
            pipeline_layout,
            vert_shader,
            easu_shader,
            rcas_shader,
            easu_pipeline,
            rcas_pipeline,
            renderpass,
            sampler,
        }
    }

    /// Port of `FSR::UpdateDescriptorSets`.
    fn update_descriptor_sets(&self, image_view: vk::ImageView, image_index: usize) {
        let images = &self.dynamic_images[image_index];
        let mut image_infos = Vec::with_capacity(2);
        let mut updates = Vec::new();

        // EASU reads from the source image
        updates.push(util::create_write_descriptor_set(
            &mut image_infos,
            self.sampler,
            image_view,
            images.descriptor_sets[FsrStage::Easu as usize],
            0,
        ));
        // RCAS reads from EASU output
        updates.push(util::create_write_descriptor_set(
            &mut image_infos,
            self.sampler,
            images.image_views[FsrStage::Easu as usize],
            images.descriptor_sets[FsrStage::Rcas as usize],
            0,
        ));

        unsafe {
            self.device.update_descriptor_sets(&updates, &[]);
        }
    }

    /// Port of `FSR::UploadImages`.
    fn upload_images(&mut self, cmdbuf: vk::CommandBuffer) {
        if self.images_ready {
            return;
        }

        for imgs in &self.dynamic_images {
            util::clear_color_image(&self.device, cmdbuf, imgs.images[FsrStage::Easu as usize]);
            util::clear_color_image(&self.device, cmdbuf, imgs.images[FsrStage::Rcas as usize]);
        }

        self.images_ready = true;
    }

    /// Port of `FSR::Draw`.
    ///
    /// Applies EASU upscaling followed by RCAS sharpening, returning the
    /// output image view.
    pub fn draw(
        &mut self,
        image_index: usize,
        _source_image: vk::Image,
        source_image_view: vk::ImageView,
        input_image_extent: vk::Extent2D,
        crop_rect: [f32; 4], // [left, top, right, bottom]
    ) -> vk::ImageView {
        let images = &self.dynamic_images[image_index];

        let _easu_image = images.images[FsrStage::Easu as usize];
        let _rcas_image = images.images[FsrStage::Rcas as usize];
        let _easu_descriptor_set = images.descriptor_sets[FsrStage::Easu as usize];
        let _rcas_descriptor_set = images.descriptor_sets[FsrStage::Rcas as usize];
        let _easu_framebuffer = images.framebuffers[FsrStage::Easu as usize];
        let _rcas_framebuffer = images.framebuffers[FsrStage::Rcas as usize];
        let _easu_pipeline = self.easu_pipeline;
        let _rcas_pipeline = self.rcas_pipeline;
        let _pipeline_layout = self.pipeline_layout;
        let _renderpass = self.renderpass;
        let _extent = self.extent;

        // Compute push constants
        let input_image_width = input_image_extent.width as f32;
        let input_image_height = input_image_extent.height as f32;
        let _output_image_width = self.extent.width as f32;
        let _output_image_height = self.extent.height as f32;
        let _viewport_width = (crop_rect[2] - crop_rect[0]) * input_image_width;
        let _viewport_x = crop_rect[0] * input_image_width;
        let _viewport_height = (crop_rect[3] - crop_rect[1]) * input_image_height;
        let _viewport_y = crop_rect[1] * input_image_height;

        let _easu_con: PushConstants = [0u32; 16];
        let _rcas_con: PushConstants = [0u32; 16];
        // FsrEasuConOffset and FsrRcasCon would populate these;
        // they depend on the FSR algorithm implementation in fsr.rs (top-level)

        self.update_descriptor_sets(source_image_view, image_index);

        // NOTE: actual command recording requires a scheduler/command buffer.
        // The draw sequence would be:
        // 1. TransitionImageLayout for source, easu, rcas
        // 2. EASU pass: bind pipeline, descriptor sets, push constants, draw(3,1,0,0)
        // 3. RCAS pass: bind pipeline, descriptor sets, push constants, draw(3,1,0,0)

        images.image_views[FsrStage::Rcas as usize]
    }
}
