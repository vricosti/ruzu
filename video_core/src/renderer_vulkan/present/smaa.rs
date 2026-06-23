// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/smaa.h` / `present/smaa.cpp`.
//!
//! Subpixel Morphological Anti-Aliasing (SMAA) post-processing pass.

use ash::vk;

use super::anti_alias_pass::AntiAliasPass;
use super::util;
use crate::host_shaders::spirv_shaders::{
    SMAA_BLENDING_WEIGHT_CALCULATION_FRAG_SPV, SMAA_BLENDING_WEIGHT_CALCULATION_VERT_SPV,
    SMAA_EDGE_DETECTION_FRAG_SPV, SMAA_EDGE_DETECTION_VERT_SPV,
    SMAA_NEIGHBORHOOD_BLENDING_FRAG_SPV, SMAA_NEIGHBORHOOD_BLENDING_VERT_SPV,
};
use crate::renderer_vulkan::scheduler::Scheduler;
use crate::renderer_vulkan::shader_util::build_shader;
use crate::smaa_area_tex::{AREATEX_HEIGHT, AREATEX_WIDTH, AREA_TEX_BYTES};
use crate::smaa_search_tex::{SEARCHTEX_HEIGHT, SEARCHTEX_WIDTH, SEARCH_TEX_BYTES};
use crate::vulkan_common::vulkan_memory_allocator::{MappedBuffer, MemoryAllocator, MemoryUsage};

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
    area_upload_buffer: MappedBuffer,
    search_upload_buffer: MappedBuffer,

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
    fn create_static_upload_buffer(allocator: &MemoryAllocator, bytes: &[u8]) -> MappedBuffer {
        let ci = vk::BufferCreateInfo::builder()
            .size(bytes.len() as vk::DeviceSize)
            .usage(vk::BufferUsageFlags::TRANSFER_SRC)
            .sharing_mode(vk::SharingMode::EXCLUSIVE)
            .build();
        let mut buffer = allocator
            .create_mapped_buffer(&ci, MemoryUsage::Upload)
            .expect("Failed to create SMAA static texture upload buffer");
        buffer.mapped_slice_mut()[..bytes.len()].copy_from_slice(bytes);
        buffer.flush();
        buffer
    }

    fn upload_static_image(
        device: &ash::Device,
        cmdbuf: vk::CommandBuffer,
        buffer: vk::Buffer,
        image: vk::Image,
        extent: vk::Extent2D,
    ) {
        let range = vk::ImageSubresourceRange {
            aspect_mask: vk::ImageAspectFlags::COLOR,
            base_mip_level: 0,
            level_count: 1,
            base_array_layer: 0,
            layer_count: 1,
        };
        let upload_barrier = vk::ImageMemoryBarrier::builder()
            .src_access_mask(vk::AccessFlags::empty())
            .dst_access_mask(vk::AccessFlags::TRANSFER_WRITE)
            .old_layout(vk::ImageLayout::UNDEFINED)
            .new_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
            .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
            .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
            .image(image)
            .subresource_range(range)
            .build();
        let shader_read_barrier = vk::ImageMemoryBarrier::builder()
            .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
            .dst_access_mask(vk::AccessFlags::SHADER_READ)
            .old_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
            .new_layout(vk::ImageLayout::GENERAL)
            .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
            .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
            .image(image)
            .subresource_range(range)
            .build();
        let copy = vk::BufferImageCopy::builder()
            .buffer_offset(0)
            .buffer_row_length(0)
            .buffer_image_height(0)
            .image_subresource(vk::ImageSubresourceLayers {
                aspect_mask: vk::ImageAspectFlags::COLOR,
                mip_level: 0,
                base_array_layer: 0,
                layer_count: 1,
            })
            .image_offset(vk::Offset3D { x: 0, y: 0, z: 0 })
            .image_extent(vk::Extent3D {
                width: extent.width,
                height: extent.height,
                depth: 1,
            })
            .build();

        unsafe {
            device.cmd_pipeline_barrier(
                cmdbuf,
                vk::PipelineStageFlags::TOP_OF_PIPE,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &[upload_barrier],
            );
            device.cmd_copy_buffer_to_image(
                cmdbuf,
                buffer,
                image,
                vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                &[copy],
            );
            device.cmd_pipeline_barrier(
                cmdbuf,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::FRAGMENT_SHADER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &[shader_read_barrier],
            );
        }
    }

    /// Port of `SMAA::SMAA`.
    pub fn new(
        device: ash::Device,
        allocator: &MemoryAllocator,
        extent: vk::Extent2D,
        image_count: usize,
    ) -> Self {
        let image_count_u32 = image_count as u32;

        // Create static images (area + search textures)
        let area_extent = vk::Extent2D {
            width: AREATEX_WIDTH,
            height: AREATEX_HEIGHT,
        };
        let search_extent = vk::Extent2D {
            width: SEARCHTEX_WIDTH,
            height: SEARCHTEX_HEIGHT,
        };

        let area_image =
            util::create_wrapped_image(&device, allocator, area_extent, vk::Format::R8G8_UNORM);
        let search_image =
            util::create_wrapped_image(&device, allocator, search_extent, vk::Format::R8_UNORM);
        let area_view =
            util::create_wrapped_image_view(&device, area_image, vk::Format::R8G8_UNORM);
        let search_view =
            util::create_wrapped_image_view(&device, search_image, vk::Format::R8_UNORM);

        let static_images = [area_image, search_image];
        let static_image_views = [area_view, search_view];
        let area_upload_buffer = Self::create_static_upload_buffer(allocator, AREA_TEX_BYTES);
        let search_upload_buffer = Self::create_static_upload_buffer(allocator, SEARCH_TEX_BYTES);

        // Create dynamic images
        let mut dynamic_images = Vec::with_capacity(image_count);
        for _i in 0..image_count {
            let blend_image = util::create_wrapped_image(
                &device,
                allocator,
                extent,
                vk::Format::R16G16B16A16_SFLOAT,
            );
            let edges_image =
                util::create_wrapped_image(&device, allocator, extent, vk::Format::R16G16_SFLOAT);
            let output_image = util::create_wrapped_image(
                &device,
                allocator,
                extent,
                vk::Format::R16G16B16A16_SFLOAT,
            );
            let blend_view = util::create_wrapped_image_view(
                &device,
                blend_image,
                vk::Format::R16G16B16A16_SFLOAT,
            );
            let edges_view =
                util::create_wrapped_image_view(&device, edges_image, vk::Format::R16G16_SFLOAT);
            let output_view = util::create_wrapped_image_view(
                &device,
                output_image,
                vk::Format::R16G16B16A16_SFLOAT,
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
            &device,
            vk::Format::R16G16_SFLOAT,
            vk::ImageLayout::UNDEFINED,
        );
        let renderpass_blend = util::create_wrapped_render_pass(
            &device,
            vk::Format::R16G16B16A16_SFLOAT,
            vk::ImageLayout::UNDEFINED,
        );
        let renderpass_neighborhood = util::create_wrapped_render_pass(
            &device,
            vk::Format::R16G16B16A16_SFLOAT,
            vk::ImageLayout::UNDEFINED,
        );
        let renderpasses = [renderpass_edges, renderpass_blend, renderpass_neighborhood];

        // Create framebuffers
        for imgs in &mut dynamic_images {
            imgs.framebuffers[SmaaStage::EdgeDetection as usize] = util::create_wrapped_framebuffer(
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

        let vertex_shaders = [
            build_shader(&device, SMAA_EDGE_DETECTION_VERT_SPV)
                .expect("Failed to build smaa_edge_detection.vert"),
            build_shader(&device, SMAA_BLENDING_WEIGHT_CALCULATION_VERT_SPV)
                .expect("Failed to build smaa_blending_weight_calculation.vert"),
            build_shader(&device, SMAA_NEIGHBORHOOD_BLENDING_VERT_SPV)
                .expect("Failed to build smaa_neighborhood_blending.vert"),
        ];
        let fragment_shaders = [
            build_shader(&device, SMAA_EDGE_DETECTION_FRAG_SPV)
                .expect("Failed to build smaa_edge_detection.frag"),
            build_shader(&device, SMAA_BLENDING_WEIGHT_CALCULATION_FRAG_SPV)
                .expect("Failed to build smaa_blending_weight_calculation.frag"),
            build_shader(&device, SMAA_NEIGHBORHOOD_BLENDING_FRAG_SPV)
                .expect("Failed to build smaa_neighborhood_blending.frag"),
        ];

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
            imgs.descriptor_sets =
                util::create_wrapped_descriptor_sets(&device, descriptor_pool, &ds_layouts);
        }

        // Create pipeline layouts
        let mut pipeline_layouts = [vk::PipelineLayout::null(); MAX_SMAA_STAGE];
        for i in 0..MAX_SMAA_STAGE {
            pipeline_layouts[i] =
                util::create_wrapped_pipeline_layout(&device, descriptor_set_layouts[i]);
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
            area_upload_buffer,
            search_upload_buffer,
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
    fn upload_images(&mut self, scheduler: &mut Scheduler) {
        if self.images_ready {
            return;
        }

        let device = self.device.clone();
        let area_buffer = self.area_upload_buffer.buffer();
        let search_buffer = self.search_upload_buffer.buffer();
        let area_image = self.static_images[StaticImageType::Area as usize];
        let search_image = self.static_images[StaticImageType::Search as usize];
        let dynamic_images: Vec<[vk::Image; MAX_DYNAMIC_IMAGE]> = self
            .dynamic_images
            .iter()
            .map(|images| images.images)
            .collect();

        scheduler.record(move |cmdbuf| {
            Self::upload_static_image(
                &device,
                cmdbuf,
                area_buffer,
                area_image,
                vk::Extent2D {
                    width: AREATEX_WIDTH,
                    height: AREATEX_HEIGHT,
                },
            );
            Self::upload_static_image(
                &device,
                cmdbuf,
                search_buffer,
                search_image,
                vk::Extent2D {
                    width: SEARCHTEX_WIDTH,
                    height: SEARCHTEX_HEIGHT,
                },
            );
            for images in dynamic_images {
                for image in images {
                    util::clear_color_image(&device, cmdbuf, image);
                }
            }
        });
        scheduler.finish();

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
        scheduler: &mut Scheduler,
        image_index: usize,
        inout_image: &mut vk::Image,
        inout_image_view: &mut vk::ImageView,
    ) {
        let images = &self.dynamic_images[image_index];

        let input_image = *inout_image;
        let output_image = images.images[DynamicImageType::Output as usize];
        let output_image_view = images.image_views[DynamicImageType::Output as usize];
        let edges_image = images.images[DynamicImageType::Edges as usize];
        let blend_image = images.images[DynamicImageType::Blend as usize];

        let edge_detection_ds = images.descriptor_sets[SmaaStage::EdgeDetection as usize];
        let blending_weight_ds =
            images.descriptor_sets[SmaaStage::BlendingWeightCalculation as usize];
        let neighborhood_ds = images.descriptor_sets[SmaaStage::NeighborhoodBlending as usize];

        let edge_fb = images.framebuffers[SmaaStage::EdgeDetection as usize];
        let blend_fb = images.framebuffers[SmaaStage::BlendingWeightCalculation as usize];
        let neighborhood_fb = images.framebuffers[SmaaStage::NeighborhoodBlending as usize];
        let renderpasses = self.renderpasses;
        let pipelines = self.pipelines;
        let pipeline_layouts = self.pipeline_layouts;
        let extent = self.extent;

        self.upload_images(scheduler);
        self.update_descriptor_sets(*inout_image_view, image_index);

        scheduler.request_outside_renderpass();
        let device = self.device.clone();
        scheduler.record(move |cmdbuf| unsafe {
            util::transition_image_layout(
                &device,
                cmdbuf,
                input_image,
                vk::ImageLayout::GENERAL,
                vk::ImageLayout::GENERAL,
            );
            util::transition_image_layout(
                &device,
                cmdbuf,
                edges_image,
                vk::ImageLayout::GENERAL,
                vk::ImageLayout::GENERAL,
            );
            util::begin_render_pass(
                &device,
                cmdbuf,
                renderpasses[SmaaStage::EdgeDetection as usize],
                edge_fb,
                extent,
            );
            device.cmd_bind_pipeline(
                cmdbuf,
                vk::PipelineBindPoint::GRAPHICS,
                pipelines[SmaaStage::EdgeDetection as usize],
            );
            device.cmd_bind_descriptor_sets(
                cmdbuf,
                vk::PipelineBindPoint::GRAPHICS,
                pipeline_layouts[SmaaStage::EdgeDetection as usize],
                0,
                &[edge_detection_ds],
                &[],
            );
            device.cmd_draw(cmdbuf, 3, 1, 0, 0);
            device.cmd_end_render_pass(cmdbuf);

            util::transition_image_layout(
                &device,
                cmdbuf,
                edges_image,
                vk::ImageLayout::GENERAL,
                vk::ImageLayout::GENERAL,
            );
            util::transition_image_layout(
                &device,
                cmdbuf,
                blend_image,
                vk::ImageLayout::GENERAL,
                vk::ImageLayout::GENERAL,
            );
            util::begin_render_pass(
                &device,
                cmdbuf,
                renderpasses[SmaaStage::BlendingWeightCalculation as usize],
                blend_fb,
                extent,
            );
            device.cmd_bind_pipeline(
                cmdbuf,
                vk::PipelineBindPoint::GRAPHICS,
                pipelines[SmaaStage::BlendingWeightCalculation as usize],
            );
            device.cmd_bind_descriptor_sets(
                cmdbuf,
                vk::PipelineBindPoint::GRAPHICS,
                pipeline_layouts[SmaaStage::BlendingWeightCalculation as usize],
                0,
                &[blending_weight_ds],
                &[],
            );
            device.cmd_draw(cmdbuf, 3, 1, 0, 0);
            device.cmd_end_render_pass(cmdbuf);

            util::transition_image_layout(
                &device,
                cmdbuf,
                blend_image,
                vk::ImageLayout::GENERAL,
                vk::ImageLayout::GENERAL,
            );
            util::transition_image_layout(
                &device,
                cmdbuf,
                output_image,
                vk::ImageLayout::GENERAL,
                vk::ImageLayout::GENERAL,
            );
            util::begin_render_pass(
                &device,
                cmdbuf,
                renderpasses[SmaaStage::NeighborhoodBlending as usize],
                neighborhood_fb,
                extent,
            );
            device.cmd_bind_pipeline(
                cmdbuf,
                vk::PipelineBindPoint::GRAPHICS,
                pipelines[SmaaStage::NeighborhoodBlending as usize],
            );
            device.cmd_bind_descriptor_sets(
                cmdbuf,
                vk::PipelineBindPoint::GRAPHICS,
                pipeline_layouts[SmaaStage::NeighborhoodBlending as usize],
                0,
                &[neighborhood_ds],
                &[],
            );
            device.cmd_draw(cmdbuf, 3, 1, 0, 0);
            device.cmd_end_render_pass(cmdbuf);
            util::transition_image_layout(
                &device,
                cmdbuf,
                output_image,
                vk::ImageLayout::GENERAL,
                vk::ImageLayout::GENERAL,
            );
        });

        *inout_image = output_image;
        *inout_image_view = output_image_view;
    }
}

impl Drop for Smaa {
    fn drop(&mut self) {
        unsafe {
            for images in &mut self.dynamic_images {
                for framebuffer in &mut images.framebuffers {
                    if *framebuffer != vk::Framebuffer::null() {
                        self.device.destroy_framebuffer(*framebuffer, None);
                        *framebuffer = vk::Framebuffer::null();
                    }
                }
                for image_view in &mut images.image_views {
                    if *image_view != vk::ImageView::null() {
                        self.device.destroy_image_view(*image_view, None);
                        *image_view = vk::ImageView::null();
                    }
                }
            }
            for image_view in &mut self.static_image_views {
                if *image_view != vk::ImageView::null() {
                    self.device.destroy_image_view(*image_view, None);
                    *image_view = vk::ImageView::null();
                }
            }
            if self.sampler != vk::Sampler::null() {
                self.device.destroy_sampler(self.sampler, None);
                self.sampler = vk::Sampler::null();
            }
            for renderpass in &mut self.renderpasses {
                if *renderpass != vk::RenderPass::null() {
                    self.device.destroy_render_pass(*renderpass, None);
                    *renderpass = vk::RenderPass::null();
                }
            }
            for pipeline in &mut self.pipelines {
                if *pipeline != vk::Pipeline::null() {
                    self.device.destroy_pipeline(*pipeline, None);
                    *pipeline = vk::Pipeline::null();
                }
            }
            for layout in &mut self.pipeline_layouts {
                if *layout != vk::PipelineLayout::null() {
                    self.device.destroy_pipeline_layout(*layout, None);
                    *layout = vk::PipelineLayout::null();
                }
            }
            for layout in &mut self.descriptor_set_layouts {
                if *layout != vk::DescriptorSetLayout::null() {
                    self.device.destroy_descriptor_set_layout(*layout, None);
                    *layout = vk::DescriptorSetLayout::null();
                }
            }
            if self.descriptor_pool != vk::DescriptorPool::null() {
                self.device
                    .destroy_descriptor_pool(self.descriptor_pool, None);
                self.descriptor_pool = vk::DescriptorPool::null();
            }
            for shader in &mut self.fragment_shaders {
                if *shader != vk::ShaderModule::null() {
                    self.device.destroy_shader_module(*shader, None);
                    *shader = vk::ShaderModule::null();
                }
            }
            for shader in &mut self.vertex_shaders {
                if *shader != vk::ShaderModule::null() {
                    self.device.destroy_shader_module(*shader, None);
                    *shader = vk::ShaderModule::null();
                }
            }
        }
    }
}
