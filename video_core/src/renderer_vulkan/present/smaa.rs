// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/smaa.h` / `present/smaa.cpp`.
//!
//! Subpixel Morphological Anti-Aliasing (SMAA) post-processing pass.

use ash::vk;

use super::anti_alias_pass::AntiAliasPass;
use super::util;

// ---------------------------------------------------------------------------
// Enums
// ---------------------------------------------------------------------------

/// Port of `SMAA::SMAAStage` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum SmaaStage {
    EdgeDetection = 0,
    BlendingWeightCalculation = 1,
    NeighborhoodBlending = 2,
}

pub const MAX_SMAA_STAGE: usize = 3;

/// Port of `SMAA::StaticImageType` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum StaticImageType {
    Area = 0,
    Search = 1,
}

pub const MAX_STATIC_IMAGE: usize = 2;

/// Port of `SMAA::DynamicImageType` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum DynamicImageType {
    Blend = 0,
    Edges = 1,
    Output = 2,
}

pub const MAX_DYNAMIC_IMAGE: usize = 3;

// ---------------------------------------------------------------------------
// Per-image dynamic resources
// ---------------------------------------------------------------------------

/// Port of `SMAA::Images` inner struct.
pub struct SmaaImages {
    pub descriptor_sets: Vec<vk::DescriptorSet>,
    pub images: [vk::Image; MAX_DYNAMIC_IMAGE],
    pub image_views: [vk::ImageView; MAX_DYNAMIC_IMAGE],
    pub framebuffers: [vk::Framebuffer; MAX_SMAA_STAGE],
}

// ---------------------------------------------------------------------------
// SMAA
// ---------------------------------------------------------------------------

/// Port of `SMAA` class.
///
/// Three-stage SMAA anti-aliasing: edge detection, blending weight
/// calculation, and neighborhood blending.
pub struct Smaa {
    device: ash::Device,
    extent: vk::Extent2D,
    image_count: u32,
    images_ready: bool,
    dynamic_images: Vec<SmaaImages>,
    static_images: [vk::Image; MAX_STATIC_IMAGE],
    static_image_views: [vk::ImageView; MAX_STATIC_IMAGE],

    descriptor_pool: vk::DescriptorPool,
    descriptor_set_layouts: [vk::DescriptorSetLayout; MAX_SMAA_STAGE],
    pipeline_layouts: [vk::PipelineLayout; MAX_SMAA_STAGE],
    vertex_shaders: [vk::ShaderModule; MAX_SMAA_STAGE],
    fragment_shaders: [vk::ShaderModule; MAX_SMAA_STAGE],
    pipelines: [vk::Pipeline; MAX_SMAA_STAGE],
    renderpasses: [vk::RenderPass; MAX_SMAA_STAGE],
    sampler: vk::Sampler,
}

impl Smaa {
    /// Port of `SMAA::SMAA`.
    pub fn new(
        device: ash::Device,
        allocator: &vk::DeviceMemory,
        extent: vk::Extent2D,
        image_count: usize,
    ) -> Self {
        let image_count_u32 = image_count as u32;

        // Create static images (area + search textures)
        // Area texture dimensions from smaa_area_tex.h: AREATEX_WIDTH=160, AREATEX_HEIGHT=560
        let area_extent = vk::Extent2D { width: 160, height: 560 };
        // Search texture dimensions from smaa_search_tex.h: SEARCHTEX_WIDTH=64, SEARCHTEX_HEIGHT=16
        let search_extent = vk::Extent2D { width: 64, height: 16 };

        let area_image = util::create_wrapped_image(
            &device, allocator, area_extent, vk::Format::R8G8_UNORM,
        );
        let search_image = util::create_wrapped_image(
            &device, allocator, search_extent, vk::Format::R8_UNORM,
        );
        let area_view = util::create_wrapped_image_view(
            &device, area_image, vk::Format::R8G8_UNORM,
        );
        let search_view = util::create_wrapped_image_view(
            &device, search_image, vk::Format::R8_UNORM,
        );

        let static_images = [area_image, search_image];
        let static_image_views = [area_view, search_view];

        // Create dynamic images
        let mut dynamic_images = Vec::with_capacity(image_count);
        for _i in 0..image_count {
            let blend_image = util::create_wrapped_image(
                &device, allocator, extent, vk::Format::R16G16B16A16_SFLOAT,
            );
            let edges_image = util::create_wrapped_image(
                &device, allocator, extent, vk::Format::R16G16_SFLOAT,
            );
            let output_image = util::create_wrapped_image(
                &device, allocator, extent, vk::Format::R16G16B16A16_SFLOAT,
            );
            let blend_view = util::create_wrapped_image_view(
                &device, blend_image, vk::Format::R16G16B16A16_SFLOAT,
            );
            let edges_view = util::create_wrapped_image_view(
                &device, edges_image, vk::Format::R16G16_SFLOAT,
            );
            let output_view = util::create_wrapped_image_view(
                &device, output_image, vk::Format::R16G16B16A16_SFLOAT,
            );
            dynamic_images.push(SmaaImages {
                descriptor_sets: Vec::new(),
                images: [blend_image, edges_image, output_image],
                image_views: [blend_view, edges_view, output_view],
                framebuffers: [vk::Framebuffer::null(); MAX_SMAA_STAGE],
            });
        }

        // Create render passes
        let renderpass_edges = util::create_wrapped_render_pass(
            &device, vk::Format::R16G16_SFLOAT, vk::ImageLayout::UNDEFINED,
        );
        let renderpass_blend = util::create_wrapped_render_pass(
            &device, vk::Format::R16G16B16A16_SFLOAT, vk::ImageLayout::UNDEFINED,
        );
        let renderpass_neighborhood = util::create_wrapped_render_pass(
            &device, vk::Format::R16G16B16A16_SFLOAT, vk::ImageLayout::UNDEFINED,
        );
        let renderpasses = [renderpass_edges, renderpass_blend, renderpass_neighborhood];

        // Create framebuffers
        for imgs in &mut dynamic_images {
            imgs.framebuffers[SmaaStage::EdgeDetection as usize] =
                util::create_wrapped_framebuffer(
                    &device,
                    renderpasses[SmaaStage::EdgeDetection as usize],
                    imgs.image_views[DynamicImageType::Edges as usize],
                    extent,
                );
            imgs.framebuffers[SmaaStage::BlendingWeightCalculation as usize] =
                util::create_wrapped_framebuffer(
                    &device,
                    renderpasses[SmaaStage::BlendingWeightCalculation as usize],
                    imgs.image_views[DynamicImageType::Blend as usize],
                    extent,
                );
            imgs.framebuffers[SmaaStage::NeighborhoodBlending as usize] =
                util::create_wrapped_framebuffer(
                    &device,
                    renderpasses[SmaaStage::NeighborhoodBlending as usize],
                    imgs.image_views[DynamicImageType::Output as usize],
                    extent,
                );
        }

        // Create sampler
        let sampler = util::create_wrapped_sampler(&device, vk::Filter::LINEAR);

        // Shaders (null placeholders - actual SPV data from host_shaders)
        let vertex_shaders = [vk::ShaderModule::null(); MAX_SMAA_STAGE];
        let fragment_shaders = [vk::ShaderModule::null(); MAX_SMAA_STAGE];

        // Descriptor pool: 6 descriptors, 3 descriptor sets per image
        let descriptor_pool = util::create_wrapped_descriptor_pool(
            &device,
            6 * image_count_u32,
            3 * image_count_u32,
            &[vk::DescriptorType::COMBINED_IMAGE_SAMPLER],
        );

        // Descriptor set layouts
        // Edge detection: 1 sampler
        let edge_layout = util::create_wrapped_descriptor_set_layout(
            &device,
            &[vk::DescriptorType::COMBINED_IMAGE_SAMPLER],
        );
        // Blending weight: 3 samplers (edges, area, search)
        let blend_layout = util::create_wrapped_descriptor_set_layout(
            &device,
            &[
                vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
                vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
                vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
            ],
        );
        // Neighborhood blending: 2 samplers (source, blend)
        let neighborhood_layout = util::create_wrapped_descriptor_set_layout(
            &device,
            &[
                vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
                vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
            ],
        );
        let descriptor_set_layouts = [edge_layout, blend_layout, neighborhood_layout];

        // Create descriptor sets
        let ds_layouts: Vec<vk::DescriptorSetLayout> =
            descriptor_set_layouts.iter().copied().collect();
        for imgs in &mut dynamic_images {
            imgs.descriptor_sets = util::create_wrapped_descriptor_sets(
                &device,
                descriptor_pool,
                &ds_layouts,
            );
        }

        // Create pipeline layouts
        let mut pipeline_layouts = [vk::PipelineLayout::null(); MAX_SMAA_STAGE];
        for i in 0..MAX_SMAA_STAGE {
            pipeline_layouts[i] = util::create_wrapped_pipeline_layout(
                &device,
                descriptor_set_layouts[i],
            );
        }

        // Create pipelines
        let mut pipelines = [vk::Pipeline::null(); MAX_SMAA_STAGE];
        for i in 0..MAX_SMAA_STAGE {
            pipelines[i] = util::create_wrapped_pipeline(
                &device,
                renderpasses[i],
                pipeline_layouts[i],
                vertex_shaders[i],
                fragment_shaders[i],
            );
        }

        Smaa {
            device,
            extent,
            image_count: image_count_u32,
            images_ready: false,
            dynamic_images,
            static_images,
            static_image_views,
            descriptor_pool,
            descriptor_set_layouts,
            pipeline_layouts,
            vertex_shaders,
            fragment_shaders,
            pipelines,
            renderpasses,
            sampler,
        }
    }

    /// Port of `SMAA::UpdateDescriptorSets`.
    fn update_descriptor_sets(&self, image_view: vk::ImageView, image_index: usize) {
        let images = &self.dynamic_images[image_index];
        let mut image_infos = Vec::with_capacity(6);
        let mut updates = Vec::new();

        // Edge detection: source image
        updates.push(util::create_write_descriptor_set(
            &mut image_infos,
            self.sampler,
            image_view,
            images.descriptor_sets[SmaaStage::EdgeDetection as usize],
            0,
        ));

        // Blending weight calculation: edges, area, search
        updates.push(util::create_write_descriptor_set(
            &mut image_infos,
            self.sampler,
            images.image_views[DynamicImageType::Edges as usize],
            images.descriptor_sets[SmaaStage::BlendingWeightCalculation as usize],
            0,
        ));
        updates.push(util::create_write_descriptor_set(
            &mut image_infos,
            self.sampler,
            self.static_image_views[StaticImageType::Area as usize],
            images.descriptor_sets[SmaaStage::BlendingWeightCalculation as usize],
            1,
        ));
        updates.push(util::create_write_descriptor_set(
            &mut image_infos,
            self.sampler,
            self.static_image_views[StaticImageType::Search as usize],
            images.descriptor_sets[SmaaStage::BlendingWeightCalculation as usize],
            2,
        ));

        // Neighborhood blending: source, blend
        updates.push(util::create_write_descriptor_set(
            &mut image_infos,
            self.sampler,
            image_view,
            images.descriptor_sets[SmaaStage::NeighborhoodBlending as usize],
            0,
        ));
        updates.push(util::create_write_descriptor_set(
            &mut image_infos,
            self.sampler,
            images.image_views[DynamicImageType::Blend as usize],
            images.descriptor_sets[SmaaStage::NeighborhoodBlending as usize],
            1,
        ));

        unsafe {
            self.device.update_descriptor_sets(&updates, &[]);
        }
    }

    /// Port of `SMAA::UploadImages`.
    fn upload_images(&mut self, cmdbuf: vk::CommandBuffer) {
        if self.images_ready {
            return;
        }

        // Static images would be uploaded via staging buffer here (area/search textures).
        // For now, clear dynamic images.
        for imgs in &self.dynamic_images {
            for i in 0..MAX_DYNAMIC_IMAGE {
                util::clear_color_image(&self.device, cmdbuf, imgs.images[i]);
            }
        }

        self.images_ready = true;
    }
}

impl AntiAliasPass for Smaa {
    /// Port of `SMAA::Draw`.
    ///
    /// Records three-pass SMAA: edge detection, blending weight calculation,
    /// and neighborhood blending. Swaps the image/view pointers to the output.
    fn draw(
        &mut self,
        image_index: usize,
        inout_image: &mut vk::Image,
        inout_image_view: &mut vk::ImageView,
    ) {
        let images = &self.dynamic_images[image_index];

        let _input_image = *inout_image;
        let _output_image = images.images[DynamicImageType::Output as usize];
        let _edges_image = images.images[DynamicImageType::Edges as usize];
        let _blend_image = images.images[DynamicImageType::Blend as usize];

        let _edge_detection_ds = images.descriptor_sets[SmaaStage::EdgeDetection as usize];
        let _blending_weight_ds =
            images.descriptor_sets[SmaaStage::BlendingWeightCalculation as usize];
        let _neighborhood_ds = images.descriptor_sets[SmaaStage::NeighborhoodBlending as usize];

        let _edge_fb = images.framebuffers[SmaaStage::EdgeDetection as usize];
        let _blend_fb = images.framebuffers[SmaaStage::BlendingWeightCalculation as usize];
        let _neighborhood_fb = images.framebuffers[SmaaStage::NeighborhoodBlending as usize];

        self.update_descriptor_sets(*inout_image_view, image_index);

        // NOTE: actual command recording requires a scheduler/command buffer.
        // The three-pass sequence would be:
        // Pass 1: Edge detection
        //   - Transition input + edges to GENERAL
        //   - BeginRenderPass(edge), BindPipeline, BindDescriptorSets, Draw(3,1,0,0), EndRenderPass
        // Pass 2: Blending weight calculation
        //   - Transition edges + blend to GENERAL
        //   - BeginRenderPass(blend), BindPipeline, BindDescriptorSets, Draw(3,1,0,0), EndRenderPass
        // Pass 3: Neighborhood blending
        //   - Transition blend + output to GENERAL
        //   - BeginRenderPass(neighborhood), BindPipeline, BindDescriptorSets, Draw(3,1,0,0), EndRenderPass
        //   - Transition output to GENERAL

        *inout_image = images.images[DynamicImageType::Output as usize];
        *inout_image_view = images.image_views[DynamicImageType::Output as usize];
    }
}
