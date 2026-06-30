// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GPU texture cache — images, views, samplers, framebuffers.
//!
//! Ref: zuyu `vk_texture_cache.h` — caches VkImage/VkImageView/VkSampler
//! objects and VkFramebuffer objects by render target configuration.

use std::collections::HashMap;
use std::sync::Arc;

use ash::vk;
use ash::vk::Handle;
use log::trace;

use crate::control::channel_state::ChannelState;
use crate::control::channel_state_cache::{ChannelInfo, ChannelSetupCaches};
use crate::engines::maxwell_3d::RenderTargetInfo;
use crate::framebuffer_config::FramebufferConfig;
use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
use crate::surface::PixelFormat;
use crate::texture_cache::image_base::ImageFlagBits;
use crate::texture_cache::texture_cache_base::{
    FramebufferImageView, TextureCacheBase as CommonTextureCache,
};
use crate::texture_cache::types::{
    BufferImageCopy, ImageType, ImageViewId, NULL_IMAGE_ID, NULL_IMAGE_VIEW_ID,
};
use crate::texture_cache::util::unswizzle_image;

use super::staging_buffer_pool::StagingBufferPool;

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

/// Backend-owned image corresponding to a common texture-cache render target.
pub struct CachedRenderTarget {
    pub cpu_addr: u64,
    pub image: vk::Image,
    pub memory: vk::DeviceMemory,
    pub view: vk::ImageView,
    pub framebuffer: vk::Framebuffer,
    pub extent: vk::Extent2D,
    pub width: u32,
    pub height: u32,
    pub layout: vk::ImageLayout,
}

#[derive(Debug, Clone, Copy)]
pub struct RenderTargetFramebuffer {
    pub framebuffer: vk::Framebuffer,
    pub cpu_addr: u64,
    pub extent: vk::Extent2D,
}

#[derive(Debug, Clone, Copy)]
pub struct RenderTargetImageInfo {
    pub image: vk::Image,
    pub width: u32,
    pub height: u32,
}

pub struct FramebufferImageViewVulkan {
    pub common: FramebufferImageView,
    pub image: vk::Image,
    pub image_view: vk::ImageView,
    pub width: u32,
    pub height: u32,
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
    pub base: CommonTextureCache,
    device: ash::Device,
    instance: ash::Instance,
    physical_device: vk::PhysicalDevice,
    channel_caches: ChannelSetupCaches<ChannelInfo>,

    /// Cached textures by TIC (texture image control) index.
    textures: HashMap<u32, CachedTexture>,

    /// Cached framebuffers by render target config key.
    framebuffers: HashMap<FramebufferKey, CachedFramebuffer>,

    /// Minimal active Vulkan render-target images keyed by translated CPU address.
    render_targets: HashMap<u64, CachedRenderTarget>,
}

impl TextureCache {
    pub fn new(
        device: ash::Device,
        instance: ash::Instance,
        physical_device: vk::PhysicalDevice,
        device_memory: Arc<MaxwellDeviceMemoryManager>,
    ) -> Self {
        Self {
            base: CommonTextureCache::new(device_memory),
            device,
            instance,
            physical_device,
            channel_caches: ChannelSetupCaches::new(),
            textures: HashMap::new(),
            framebuffers: HashMap::new(),
            render_targets: HashMap::new(),
        }
    }

    /// Port of the Vulkan texture-cache owner `CreateChannel` edge.
    pub fn create_channel(&mut self, channel: &ChannelState) {
        self.channel_caches.create_channel(channel);
        self.base.create_channel(channel);
    }

    /// Port of the Vulkan texture-cache owner `BindToChannel` edge.
    pub fn bind_to_channel(&mut self, channel_id: i32) {
        self.channel_caches.bind_to_channel(channel_id);
        self.base.bind_to_channel(channel_id);
    }

    /// Port of the Vulkan texture-cache owner `EraseChannel` edge.
    pub fn erase_channel(&mut self, channel_id: i32) {
        self.channel_caches.erase_channel(channel_id);
        self.base.erase_channel(channel_id);
    }

    /// Register the current draw render targets in the common cache and
    /// return a backend framebuffer for RT0 when it can be presented later.
    pub fn update_render_targets_and_get_rt0_framebuffer(
        &mut self,
        render_targets: &crate::engines::draw_manager::Maxwell3DRenderTargets,
        render_pass: vk::RenderPass,
        depth_view: vk::ImageView,
    ) -> Option<RenderTargetFramebuffer> {
        let gpu_memory = self.base.channel_gpu_memory.as_ref().cloned()?;
        self.base
            .update_render_targets_from_snapshot(render_targets, |gpu_addr, guest_size| {
                let gpu_memory = gpu_memory.lock();
                gpu_memory
                    .gpu_to_cpu_address(gpu_addr)
                    .or_else(|| gpu_memory.gpu_to_cpu_address_range(gpu_addr, guest_size))
            });

        let rt0 = render_targets.render_targets[0];
        if rt0.address == 0 || rt0.width == 0 || rt0.height == 0 || rt0.format == 0 {
            return None;
        }
        let gpu_memory = self.base.channel_gpu_memory.as_ref()?.lock();
        let cpu_addr = gpu_memory.gpu_to_cpu_address(rt0.address).or_else(|| {
            let approx_size = (rt0.width as u64)
                .saturating_mul(rt0.height as u64)
                .saturating_mul(4);
            gpu_memory.gpu_to_cpu_address_range(rt0.address, approx_size)
        })?;
        drop(gpu_memory);

        let render_area = vk::Extent2D {
            width: self
                .base
                .render_targets
                .size
                .width
                .max(1)
                .min(rt0.width.max(1)),
            height: self
                .base
                .render_targets
                .size
                .height
                .max(1)
                .min(rt0.height.max(1)),
        };

        if !self.render_targets.contains_key(&cpu_addr) {
            let target = self
                .create_render_target(cpu_addr, &rt0, render_area, render_pass, depth_view)
                .ok()?;
            self.render_targets.insert(cpu_addr, target);
        }
        self.render_targets
            .get(&cpu_addr)
            .map(|target| RenderTargetFramebuffer {
                framebuffer: target.framebuffer,
                cpu_addr,
                extent: target.extent,
            })
    }

    pub fn prepare_render_target_for_render(&mut self, cpu_addr: u64, cmd: vk::CommandBuffer) {
        let Some((image, old_layout)) = self
            .render_targets
            .get(&cpu_addr)
            .map(|target| (target.image, target.layout))
        else {
            return;
        };
        if old_layout == vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL {
            return;
        }
        self.transition_layout(
            cmd,
            image,
            old_layout,
            vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
            vk::ImageAspectFlags::COLOR,
        );
        if let Some(target) = self.render_targets.get_mut(&cpu_addr) {
            target.layout = vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL;
        }
    }

    pub fn prepare_framebuffer_for_present(&mut self, cpu_addr: u64, cmd: vk::CommandBuffer) {
        let (image, old_layout) = self
            .render_targets
            .get(&cpu_addr)
            .map(|target| (target.image, target.layout))
            .unwrap_or((vk::Image::null(), vk::ImageLayout::UNDEFINED));
        if image == vk::Image::null() {
            return;
        }
        if old_layout != vk::ImageLayout::GENERAL {
            self.transition_layout(
                cmd,
                image,
                old_layout,
                vk::ImageLayout::GENERAL,
                vk::ImageAspectFlags::COLOR,
            );
            if let Some(target) = self.render_targets.get_mut(&cpu_addr) {
                target.layout = vk::ImageLayout::GENERAL;
            }
        }
    }

    pub fn rt0_image_info(
        &self,
        render_targets: &crate::engines::draw_manager::Maxwell3DRenderTargets,
    ) -> Option<RenderTargetImageInfo> {
        let rt0 = render_targets.render_targets[0];
        if rt0.address == 0 {
            return None;
        }
        let gpu_memory = self.base.channel_gpu_memory.as_ref()?.lock();
        let cpu_addr = gpu_memory.gpu_to_cpu_address(rt0.address).or_else(|| {
            let approx_size = (rt0.width as u64)
                .saturating_mul(rt0.height as u64)
                .saturating_mul(4);
            gpu_memory.gpu_to_cpu_address_range(rt0.address, approx_size)
        })?;
        drop(gpu_memory);

        let target = self.render_targets.get(&cpu_addr)?;
        Some(RenderTargetImageInfo {
            image: target.image,
            width: target.width,
            height: target.height,
        })
    }

    /// Port-facing subset of upstream `TextureCache<P>::TryFindFramebufferImageView`.
    pub fn try_find_framebuffer_image_view(
        &mut self,
        config: &FramebufferConfig,
        cpu_addr: u64,
    ) -> Option<FramebufferImageViewVulkan> {
        let framebuffer_view = self
            .base
            .try_find_framebuffer_image_view(config, cpu_addr)?;
        let target = self.render_targets.get(&cpu_addr)?;
        Some(FramebufferImageViewVulkan {
            common: framebuffer_view,
            image: target.image,
            image_view: target.view,
            width: target.width,
            height: target.height,
        })
    }

    /// Resolve a common texture-cache `ImageViewId` to a Vulkan image view
    /// when the backend image is already materialized by this partial Vulkan
    /// cache. This currently covers render-target-backed images.
    pub fn image_view_handle(&self, view_id: ImageViewId) -> Option<vk::ImageView> {
        if !view_id.is_valid() || view_id == NULL_IMAGE_VIEW_ID {
            return None;
        }
        let view = self.base.slot_image_views.get(view_id);
        if !view.image_id.is_valid() || view.image_id == NULL_IMAGE_ID {
            return None;
        }
        let image = self.base.slot_images.get(view.image_id);
        self.render_targets
            .get(&image.cpu_addr)
            .map(|target| target.view)
    }

    /// Materialize a common `ImageViewId` into a Vulkan image/view when the
    /// partial backend has not already created one. This is the Vulkan-owned
    /// half of upstream `ImageView::Handle(desc.type)` and intentionally stays
    /// in the backend wrapper.
    pub fn materialize_sampled_image_view(
        &mut self,
        view_id: ImageViewId,
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
        staging_pool: &mut StagingBufferPool,
        cmd: vk::CommandBuffer,
    ) -> Option<vk::ImageView> {
        if let Some(view) = self.image_view_handle(view_id) {
            return Some(view);
        }
        if !view_id.is_valid() || view_id == NULL_IMAGE_VIEW_ID {
            return None;
        }

        let view_base = self.base.slot_image_views.get(view_id).clone();
        if !view_base.image_id.is_valid() || view_base.image_id == NULL_IMAGE_ID {
            return None;
        }
        let image_base = self.base.slot_images.get(view_base.image_id).clone();
        if let Some(cached) = self.textures.get(&view_id.index) {
            let view = cached.view;
            let image = cached.image;
            if image_base.flags.contains(ImageFlagBits::CPU_MODIFIED) {
                self.upload_sampled_image_contents(
                    view_id,
                    image,
                    &view_base,
                    &image_base,
                    read_gpu_unsafe,
                    staging_pool,
                    cmd,
                    true,
                );
                self.base.slot_images.get_mut(view_base.image_id).flags &=
                    !ImageFlagBits::CPU_MODIFIED;
            }
            return Some(view);
        }

        if !matches!(
            image_base.info.image_type,
            ImageType::E2D | ImageType::Linear
        ) {
            log::warn!(
                "TextureCacheVulkan: sampled image type {:?} is not materialized yet",
                image_base.info.image_type
            );
            return None;
        }
        let format = vk_format_from_pixel_format(view_base.format)?;
        let width = view_base.size.width.max(1);
        let height = view_base.size.height.max(1);

        let (image, memory, view) = self
            .create_sampled_image(format, width, height)
            .inspect_err(|err| {
                log::warn!(
                    "TextureCacheVulkan: failed to create sampled image {:?} {}x{}: {:?}",
                    format,
                    width,
                    height,
                    err
                );
            })
            .ok()?;

        self.upload_sampled_image_contents(
            view_id,
            image,
            &view_base,
            &image_base,
            read_gpu_unsafe,
            staging_pool,
            cmd,
            false,
        );
        self.base.slot_images.get_mut(view_base.image_id).flags &= !ImageFlagBits::CPU_MODIFIED;

        self.textures.insert(
            view_id.index,
            CachedTexture {
                image,
                memory,
                view,
                sampler: vk::Sampler::null(),
                format,
                width,
                height,
                layout: vk::ImageLayout::GENERAL,
            },
        );
        Some(view)
    }

    fn upload_sampled_image_contents(
        &mut self,
        view_id: ImageViewId,
        image: vk::Image,
        view_base: &crate::texture_cache::image_view_base::ImageViewBase,
        image_base: &crate::texture_cache::image_base::ImageBase,
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
        staging_pool: &mut StagingBufferPool,
        cmd: vk::CommandBuffer,
        is_initialized: bool,
    ) {
        let guest_size = image_base.guest_size_bytes as usize;
        let staging_size = image_base.unswizzled_size_bytes as usize;
        let Some(staging) = staging_pool.request_upload_buffer(staging_size as vk::DeviceSize)
        else {
            return;
        };
        let mut guest = vec![0u8; guest_size];
        if !read_gpu_unsafe(image_base.gpu_addr, &mut guest) {
            log::warn!(
                "TextureCacheVulkan: unsafe texture upload read missed gpu=0x{:X} size={}",
                image_base.gpu_addr,
                guest.len()
            );
            return;
        }
        let mut linear = vec![0u8; staging_size];
        let copies = unswizzle_image(
            &(),
            image_base.gpu_addr,
            &image_base.info,
            &guest,
            &mut linear,
        );
        unsafe {
            std::ptr::copy_nonoverlapping(linear.as_ptr(), staging.mapped, linear.len());
        }
        if let Some(dir) = std::env::var_os("RUZU_DUMP_VK_TEXTURE_UPLOAD_DIR") {
            let width = view_base.size.width as usize;
            let height = view_base.size.height as usize;
            if width != 0 && height != 0 && linear.len() >= width * height * 4 {
                let path = std::path::PathBuf::from(dir).join(format!(
                    "upload_view{}_image{}_{}x{}_{:?}.ppm",
                    view_id.index, view_base.image_id.index, width, height, view_base.format
                ));
                if let Some(parent) = path.parent() {
                    let _ = std::fs::create_dir_all(parent);
                }
                let mut ppm = Vec::with_capacity(32 + width * height * 3);
                ppm.extend_from_slice(format!("P6\n{} {}\n255\n", width, height).as_bytes());
                for px in linear.chunks_exact(4).take(width * height) {
                    // A8B8G8R8_UNORM_PACK32 is laid out as RGBA bytes on little-endian hosts.
                    ppm.extend_from_slice(&px[..3]);
                }
                if let Err(err) = std::fs::write(&path, ppm) {
                    log::warn!(
                        "TextureCacheVulkan: failed to dump upload texture {}: {}",
                        path.display(),
                        err
                    );
                } else {
                    log::info!("[VK_TEXTURE_UPLOAD_DUMP] {}", path.display());
                }
            }
        }
        if std::env::var_os("RUZU_TRACE_VK_TEXTURE_UPLOAD").is_some() {
            let guest_nonzero = guest.iter().any(|&byte| byte != 0);
            let linear_nonzero = linear.iter().any(|&byte| byte != 0);
            let guest_crc = guest.iter().take(4096).fold(0u64, |acc, &byte| {
                acc.wrapping_mul(16777619).wrapping_add(byte as u64)
            });
            let linear_crc = linear.iter().take(4096).fold(0u64, |acc, &byte| {
                acc.wrapping_mul(16777619).wrapping_add(byte as u64)
            });
            let first_copy = copies.first().copied();
            log::warn!(
                "[VK_TEXTURE_UPLOAD] view_id={} image_id={} gpu=0x{:X} cpu=0x{:X} initialized={} type={:?} fmt={:?} size={}x{} guest={} unswizzled={} copies={} cpu_modified={} guest_nonzero={} linear_nonzero={} guest_crc=0x{:X} linear_crc=0x{:X} first_copy={:?}",
                view_id.index,
                view_base.image_id.index,
                image_base.gpu_addr,
                image_base.cpu_addr,
                is_initialized,
                image_base.info.image_type,
                view_base.format,
                view_base.size.width,
                view_base.size.height,
                guest_size,
                staging_size,
                copies.len(),
                image_base.flags.contains(ImageFlagBits::CPU_MODIFIED),
                guest_nonzero,
                linear_nonzero,
                guest_crc,
                linear_crc,
                first_copy,
            );
        }
        self.upload_sampled_image(
            cmd,
            image,
            staging.buffer,
            staging.offset,
            &copies,
            is_initialized,
        );
    }

    fn create_render_target(
        &mut self,
        cpu_addr: u64,
        rt: &RenderTargetInfo,
        render_area: vk::Extent2D,
        render_pass: vk::RenderPass,
        depth_view: vk::ImageView,
    ) -> Result<CachedRenderTarget, vk::Result> {
        let (image, memory, view) = self.create_attachment(
            vk::Format::R8G8B8A8_UNORM,
            vk::ImageUsageFlags::COLOR_ATTACHMENT
                | vk::ImageUsageFlags::SAMPLED
                | vk::ImageUsageFlags::TRANSFER_DST
                | vk::ImageUsageFlags::TRANSFER_SRC,
            vk::ImageAspectFlags::COLOR,
            rt.width.max(1),
            rt.height.max(1),
        )?;
        let framebuffer = self.get_or_create_framebuffer(
            render_pass,
            &[view],
            Some(depth_view),
            render_area.width,
            render_area.height,
        )?;
        Ok(CachedRenderTarget {
            cpu_addr,
            image,
            memory,
            view,
            framebuffer,
            extent: render_area,
            width: rt.width.max(1),
            height: rt.height.max(1),
            layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
        })
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

    fn create_sampled_image(
        &self,
        format: vk::Format,
        width: u32,
        height: u32,
    ) -> Result<(vk::Image, vk::DeviceMemory, vk::ImageView), vk::Result> {
        self.create_attachment(
            format,
            vk::ImageUsageFlags::SAMPLED | vk::ImageUsageFlags::TRANSFER_DST,
            vk::ImageAspectFlags::COLOR,
            width,
            height,
        )
    }

    fn upload_sampled_image(
        &self,
        cmd: vk::CommandBuffer,
        image: vk::Image,
        staging_buffer: vk::Buffer,
        staging_offset: vk::DeviceSize,
        copies: &[BufferImageCopy],
        is_initialized: bool,
    ) {
        if copies.is_empty() {
            return;
        }
        unsafe {
            let upload_barrier = vk::ImageMemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::empty())
                .dst_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .old_layout(if is_initialized {
                    vk::ImageLayout::GENERAL
                } else {
                    vk::ImageLayout::UNDEFINED
                })
                .new_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(image)
                .subresource_range(vk::ImageSubresourceRange {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    base_mip_level: 0,
                    level_count: 1,
                    base_array_layer: 0,
                    layer_count: 1,
                })
                .build();
            self.device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &[upload_barrier],
            );

            let vk_copies: Vec<_> = copies
                .iter()
                .map(|copy| {
                    vk::BufferImageCopy::builder()
                        .buffer_offset(staging_offset + copy.buffer_offset as vk::DeviceSize)
                        .buffer_row_length(copy.buffer_row_length)
                        .buffer_image_height(copy.buffer_image_height)
                        .image_subresource(vk::ImageSubresourceLayers {
                            aspect_mask: vk::ImageAspectFlags::COLOR,
                            mip_level: copy.image_subresource.base_level as u32,
                            base_array_layer: copy.image_subresource.base_layer as u32,
                            layer_count: copy.image_subresource.num_layers as u32,
                        })
                        .image_offset(vk::Offset3D {
                            x: copy.image_offset.x,
                            y: copy.image_offset.y,
                            z: copy.image_offset.z,
                        })
                        .image_extent(vk::Extent3D {
                            width: copy.image_extent.width,
                            height: copy.image_extent.height,
                            depth: copy.image_extent.depth,
                        })
                        .build()
                })
                .collect();
            self.device.cmd_copy_buffer_to_image(
                cmd,
                staging_buffer,
                image,
                vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                &vk_copies,
            );

            let shader_barrier = vk::ImageMemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .dst_access_mask(vk::AccessFlags::SHADER_READ)
                .old_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .new_layout(vk::ImageLayout::GENERAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(image)
                .subresource_range(vk::ImageSubresourceRange {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    base_mip_level: 0,
                    level_count: 1,
                    base_array_layer: 0,
                    layer_count: 1,
                })
                .build();
            self.device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &[shader_barrier],
            );
        }
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
            (vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL, vk::ImageLayout::TRANSFER_SRC_OPTIMAL) => (
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
            (vk::ImageLayout::TRANSFER_SRC_OPTIMAL, vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL) => (
                vk::AccessFlags::TRANSFER_READ,
                vk::PipelineStageFlags::TRANSFER,
                vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
                vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            ),
            (vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL, vk::ImageLayout::GENERAL) => (
                vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
                vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                vk::AccessFlags::SHADER_READ,
                vk::PipelineStageFlags::FRAGMENT_SHADER,
            ),
            (vk::ImageLayout::GENERAL, vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL) => (
                vk::AccessFlags::SHADER_READ,
                vk::PipelineStageFlags::FRAGMENT_SHADER,
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
            for (_, target) in self.render_targets.drain() {
                self.device.destroy_framebuffer(target.framebuffer, None);
                self.device.destroy_image_view(target.view, None);
                self.device.destroy_image(target.image, None);
                self.device.free_memory(target.memory, None);
            }
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

fn vk_format_from_pixel_format(format: PixelFormat) -> Option<vk::Format> {
    match format {
        PixelFormat::A8B8G8R8Unorm => Some(vk::Format::A8B8G8R8_UNORM_PACK32),
        PixelFormat::A8B8G8R8Srgb => Some(vk::Format::A8B8G8R8_SRGB_PACK32),
        PixelFormat::B8G8R8A8Unorm => Some(vk::Format::B8G8R8A8_UNORM),
        PixelFormat::B8G8R8A8Srgb => Some(vk::Format::B8G8R8A8_SRGB),
        PixelFormat::R8Unorm => Some(vk::Format::R8_UNORM),
        PixelFormat::R8G8Unorm => Some(vk::Format::R8G8_UNORM),
        _ => {
            log::warn!(
                "TextureCacheVulkan: unsupported sampled pixel format {:?}",
                format
            );
            None
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
