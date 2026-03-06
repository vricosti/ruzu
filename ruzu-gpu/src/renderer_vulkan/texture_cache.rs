// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GPU texture cache — images, views, samplers, framebuffers.
//!
//! Ref: zuyu `vk_texture_cache.h` — caches VkImage/VkImageView/VkSampler
//! objects and VkFramebuffer objects by render target configuration.

use std::collections::HashMap;

use ash::vk;
use log::trace;

/// A cached GPU texture (VkImage + VkImageView + VkSampler).
pub struct CachedTexture {
    pub image: vk::Image,
    pub memory: vk::DeviceMemory,
    pub view: vk::ImageView,
    pub sampler: vk::Sampler,
    pub format: vk::Format,
    pub width: u32,
    pub height: u32,
    pub layout: vk::ImageLayout,
}

/// A cached framebuffer (VkFramebuffer + associated views).
pub struct CachedFramebuffer {
    pub framebuffer: vk::Framebuffer,
    pub render_pass: vk::RenderPass,
    pub color_views: Vec<vk::ImageView>,
    pub depth_view: Option<vk::ImageView>,
    pub extent: vk::Extent2D,
}

/// Key for framebuffer lookup.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FramebufferKey {
    color_formats: [vk::Format; 8],
    num_colors: u8,
    depth_format: vk::Format,
    width: u32,
    height: u32,
}

/// Manages GPU textures (images, views, samplers, framebuffers).
///
/// Ref: zuyu TextureCacheRuntime — caches textures by TIC index and
/// framebuffers by render target configuration.
pub struct TextureCache {
    device: ash::Device,
    instance: ash::Instance,
    physical_device: vk::PhysicalDevice,

    /// Cached textures by TIC (texture image control) index.
    textures: HashMap<u32, CachedTexture>,

    /// Cached framebuffers by render target config key.
    framebuffers: HashMap<FramebufferKey, CachedFramebuffer>,
}

impl TextureCache {
    pub fn new(
        device: ash::Device,
        instance: ash::Instance,
        physical_device: vk::PhysicalDevice,
    ) -> Self {
        Self {
            device,
            instance,
            physical_device,
            textures: HashMap::new(),
            framebuffers: HashMap::new(),
        }
    }

    /// Get or create a framebuffer for the given render target configuration.
    pub fn get_or_create_framebuffer(
        &mut self,
        render_pass: vk::RenderPass,
        color_views: &[vk::ImageView],
        depth_view: Option<vk::ImageView>,
        width: u32,
        height: u32,
    ) -> Result<vk::Framebuffer, vk::Result> {
        // Build attachment list
        let mut attachments: Vec<vk::ImageView> = color_views.to_vec();
        if let Some(dv) = depth_view {
            attachments.push(dv);
        }

        let fb_info = vk::FramebufferCreateInfo::builder()
            .render_pass(render_pass)
            .attachments(&attachments)
            .width(width)
            .height(height)
            .layers(1)
            .build();

        unsafe { self.device.create_framebuffer(&fb_info, None) }
    }

    /// Create a VkImage + VkImageView for a color or depth attachment.
    pub fn create_attachment(
        &self,
        format: vk::Format,
        usage: vk::ImageUsageFlags,
        aspect: vk::ImageAspectFlags,
        width: u32,
        height: u32,
    ) -> Result<(vk::Image, vk::DeviceMemory, vk::ImageView), vk::Result> {
        let image_info = vk::ImageCreateInfo::builder()
            .image_type(vk::ImageType::TYPE_2D)
            .format(format)
            .extent(vk::Extent3D {
                width,
                height,
                depth: 1,
            })
            .mip_levels(1)
            .array_layers(1)
            .samples(vk::SampleCountFlags::TYPE_1)
            .tiling(vk::ImageTiling::OPTIMAL)
            .usage(usage)
            .sharing_mode(vk::SharingMode::EXCLUSIVE)
            .build();

        let image = unsafe { self.device.create_image(&image_info, None)? };

        let mem_reqs = unsafe { self.device.get_image_memory_requirements(image) };
        let mem_type = find_device_local_memory(
            &self.instance,
            self.physical_device,
            mem_reqs.memory_type_bits,
        )
        .unwrap_or(0);

        let alloc_info = vk::MemoryAllocateInfo::builder()
            .allocation_size(mem_reqs.size)
            .memory_type_index(mem_type)
            .build();
        let memory = unsafe { self.device.allocate_memory(&alloc_info, None)? };
        unsafe {
            self.device.bind_image_memory(image, memory, 0)?;
        }

        let view_info = vk::ImageViewCreateInfo::builder()
            .image(image)
            .view_type(vk::ImageViewType::TYPE_2D)
            .format(format)
            .subresource_range(vk::ImageSubresourceRange {
                aspect_mask: aspect,
                base_mip_level: 0,
                level_count: 1,
                base_array_layer: 0,
                layer_count: 1,
            })
            .build();
        let view = unsafe { self.device.create_image_view(&view_info, None)? };

        trace!(
            "TextureCache: created {:?} attachment {}x{}",
            format,
            width,
            height
        );

        Ok((image, memory, view))
    }

    /// Record an image layout transition.
    pub fn transition_layout(
        &self,
        cmd: vk::CommandBuffer,
        image: vk::Image,
        old_layout: vk::ImageLayout,
        new_layout: vk::ImageLayout,
        aspect: vk::ImageAspectFlags,
    ) {
        let (src_access, src_stage, dst_access, dst_stage) = match (old_layout, new_layout) {
            (vk::ImageLayout::UNDEFINED, vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL) => (
                vk::AccessFlags::empty(),
                vk::PipelineStageFlags::TOP_OF_PIPE,
                vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
                vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            ),
            (
                vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
                vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
            ) => (
                vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
                vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                vk::AccessFlags::TRANSFER_READ,
                vk::PipelineStageFlags::TRANSFER,
            ),
            (vk::ImageLayout::UNDEFINED, vk::ImageLayout::DEPTH_STENCIL_ATTACHMENT_OPTIMAL) => (
                vk::AccessFlags::empty(),
                vk::PipelineStageFlags::TOP_OF_PIPE,
                vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE,
                vk::PipelineStageFlags::EARLY_FRAGMENT_TESTS,
            ),
            (
                vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
            ) => (
                vk::AccessFlags::TRANSFER_READ,
                vk::PipelineStageFlags::TRANSFER,
                vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
                vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            ),
            _ => (
                vk::AccessFlags::empty(),
                vk::PipelineStageFlags::TOP_OF_PIPE,
                vk::AccessFlags::empty(),
                vk::PipelineStageFlags::BOTTOM_OF_PIPE,
            ),
        };

        let barrier = vk::ImageMemoryBarrier::builder()
            .old_layout(old_layout)
            .new_layout(new_layout)
            .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
            .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
            .image(image)
            .subresource_range(vk::ImageSubresourceRange {
                aspect_mask: aspect,
                base_mip_level: 0,
                level_count: 1,
                base_array_layer: 0,
                layer_count: 1,
            })
            .src_access_mask(src_access)
            .dst_access_mask(dst_access)
            .build();

        unsafe {
            self.device.cmd_pipeline_barrier(
                cmd,
                src_stage,
                dst_stage,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &[barrier],
            );
        }
    }

    /// Destroy a specific attachment (image + memory + view).
    pub fn destroy_attachment(
        &self,
        image: vk::Image,
        memory: vk::DeviceMemory,
        view: vk::ImageView,
    ) {
        unsafe {
            self.device.destroy_image_view(view, None);
            self.device.destroy_image(image, None);
            self.device.free_memory(memory, None);
        }
    }
}

impl Drop for TextureCache {
    fn drop(&mut self) {
        unsafe {
            for (_, tex) in self.textures.drain() {
                self.device.destroy_sampler(tex.sampler, None);
                self.device.destroy_image_view(tex.view, None);
                self.device.destroy_image(tex.image, None);
                self.device.free_memory(tex.memory, None);
            }
            for (_, fb) in self.framebuffers.drain() {
                self.device.destroy_framebuffer(fb.framebuffer, None);
            }
        }
    }
}

fn find_device_local_memory(
    instance: &ash::Instance,
    physical_device: vk::PhysicalDevice,
    type_filter: u32,
) -> Option<u32> {
    let mem_props = unsafe { instance.get_physical_device_memory_properties(physical_device) };
    for i in 0..mem_props.memory_type_count {
        if (type_filter & (1 << i)) != 0
            && mem_props.memory_types[i as usize]
                .property_flags
                .contains(vk::MemoryPropertyFlags::DEVICE_LOCAL)
        {
            return Some(i);
        }
    }
    None
}
