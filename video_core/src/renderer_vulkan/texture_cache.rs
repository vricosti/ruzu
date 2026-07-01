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
use crate::surface::{PixelFormat, SurfaceType};
use crate::texture_cache::image_base::ImageFlagBits;
use crate::texture_cache::texture_cache_base::{
    FramebufferImageView, TextureCacheBase as CommonTextureCache,
};
use crate::texture_cache::types::{
    BufferImageCopy, ImageType, ImageViewId, SamplerId, SubresourceRange, NULL_IMAGE_ID,
    NULL_IMAGE_VIEW_ID,
};
use crate::texture_cache::util::unswizzle_image;
use crate::textures::texture::{TextureFilter, TextureMipmapFilter, TscEntry, WrapMode};
use shader_recompiler::shader_info::TextureType;

use super::maxwell_to_vk;
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
    /// Depth attachment sized to match this render target. A shared,
    /// window-sized depth buffer would clamp the framebuffer (and therefore
    /// all rendering) to the smallest attachment, truncating draws to targets
    /// larger than the initial window. See `create_render_target`.
    pub depth_image: vk::Image,
    pub depth_memory: vk::DeviceMemory,
    pub depth_view: vk::ImageView,
    pub framebuffer: vk::Framebuffer,
    pub extent: vk::Extent2D,
    pub width: u32,
    pub height: u32,
    pub layout: vk::ImageLayout,
}

/// Backend-owned sampler corresponding to an upstream `TSCEntry`.
pub struct CachedSampler {
    pub sampler: vk::Sampler,
}

/// Backend-owned Vulkan view corresponding to an upstream `ImageView`.
pub struct CachedRenderTargetView {
    pub view: vk::ImageView,
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

    /// Vulkan views materialized from common-cache render-target `ImageViewId`s.
    render_target_views: HashMap<ImageViewId, CachedRenderTargetView>,

    /// Vulkan samplers materialized from common-cache `SamplerId`s.
    samplers: HashMap<SamplerId, CachedSampler>,
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
            render_target_views: HashMap::new(),
            samplers: HashMap::new(),
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
        if std::env::var_os("RUZU_TRACE_VK_RT_FRAMEBUFFER").is_some() {
            log::info!(
                "[VK_RT_FRAMEBUFFER] rt0_gpu=0x{:X} cpu=0x{:X} rt={}x{} surface_clip={}x{} base_size={}x{} render_area={}x{}",
                rt0.address,
                cpu_addr,
                rt0.width,
                rt0.height,
                render_targets.surface_clip.width,
                render_targets.surface_clip.height,
                self.base.render_targets.size.width,
                self.base.render_targets.size.height,
                render_area.width,
                render_area.height,
            );
        }

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
        let target_image = target.image;
        let fallback_view = target.view;
        let image_view = self
            .materialize_render_target_color2d_view(
                framebuffer_view.view_id,
                target_image,
                &framebuffer_view.view,
            )
            .unwrap_or(fallback_view);
        Some(FramebufferImageViewVulkan {
            width: framebuffer_view.view.size.width,
            height: framebuffer_view.view.size.height,
            common: framebuffer_view,
            image: target_image,
            image_view,
        })
    }

    /// Resolve a common texture-cache `ImageViewId` to a Vulkan image view
    /// when the backend image is already materialized by this partial Vulkan
    /// cache. This currently covers render-target-backed images.
    pub fn image_view_handle(
        &mut self,
        view_id: ImageViewId,
        texture_type: TextureType,
    ) -> Option<vk::ImageView> {
        if !view_id.is_valid() || view_id == NULL_IMAGE_VIEW_ID {
            return None;
        }
        let view_base = self.base.slot_image_views.get(view_id).clone();
        if !view_base.image_id.is_valid() || view_base.image_id == NULL_IMAGE_ID {
            return None;
        }
        let image_base = self.base.slot_images.get(view_base.image_id).clone();
        if let Some(target) = self.render_targets.get(&image_base.cpu_addr) {
            let target_image = target.image;
            let target_view = target.view;
            return match texture_type {
                TextureType::Color2D | TextureType::Color2DRect => self
                    .materialize_render_target_color2d_view(view_id, target_image, &view_base)
                    .or(Some(target_view)),
                TextureType::ColorArray2D => Some(target_view),
                _ => Some(target_view),
            };
        }
        None
    }

    /// Port-facing subset of upstream `ImageView::Handle(TextureType::Color2D)`.
    fn materialize_render_target_color2d_view(
        &mut self,
        view_id: ImageViewId,
        image: vk::Image,
        view_base: &crate::texture_cache::image_view_base::ImageViewBase,
    ) -> Option<vk::ImageView> {
        if let Some(cached) = self.render_target_views.get(&view_id) {
            return Some(cached.view);
        }
        let format = pixel_format_to_vk(view_base.format);
        let aspect_mask = image_view_aspect_mask(view_base);
        let components = image_view_components(view_base);
        let subresource_range =
            color2d_subresource_range(aspect_mask, view_base.range, view_base.flags);

        let mut usage_info = vk::ImageViewUsageCreateInfo::builder()
            .usage(
                vk::ImageUsageFlags::SAMPLED
                    | vk::ImageUsageFlags::COLOR_ATTACHMENT
                    | vk::ImageUsageFlags::TRANSFER_DST
                    | vk::ImageUsageFlags::TRANSFER_SRC,
            )
            .build();
        let view_info = vk::ImageViewCreateInfo::builder()
            .push_next(&mut usage_info)
            .image(image)
            .view_type(vk::ImageViewType::TYPE_2D)
            .format(format)
            .components(components)
            .subresource_range(subresource_range)
            .build();
        let view = unsafe { self.device.create_image_view(&view_info, None) }
            .inspect_err(|err| {
                log::warn!(
                    "TextureCacheVulkan: failed to create framebuffer Color2D view id={} image=0x{:X} format={:?} range={:?}: {:?}",
                    view_id.index,
                    image.as_raw(),
                    format,
                    subresource_range,
                    err
                );
            })
            .ok()?;
        if std::env::var_os("RUZU_TRACE_PRESENT_IMG").is_some() {
            log::warn!(
                "[PRESENT_VIEW] view_id={} image=0x{:X} view=0x{:X} fmt={:?} base_mip={} levels={} base_layer={} layers={} swizzle=({:?},{:?},{:?},{:?})",
                view_id.index,
                image.as_raw(),
                view.as_raw(),
                format,
                subresource_range.base_mip_level,
                subresource_range.level_count,
                subresource_range.base_array_layer,
                subresource_range.layer_count,
                components.r,
                components.g,
                components.b,
                components.a,
            );
        }
        self.render_target_views
            .insert(view_id, CachedRenderTargetView { view });
        Some(view)
    }

    /// Port-facing subset of upstream `TextureCache::GetSampler(id).Handle()`.
    pub fn sampler_handle(&mut self, sampler_id: SamplerId) -> Option<vk::Sampler> {
        use crate::texture_cache::types::NULL_SAMPLER_ID;
        if !sampler_id.is_valid() || sampler_id == NULL_SAMPLER_ID {
            return None;
        }
        if let Some(sampler) = self.samplers.get(&sampler_id) {
            return Some(sampler.sampler);
        }
        let tsc = *self.base.slot_samplers.get(sampler_id);
        let sampler = self.create_sampler_from_tsc(&tsc).ok()?;
        self.samplers.insert(sampler_id, CachedSampler { sampler });
        Some(sampler)
    }

    /// Materialize a common `ImageViewId` into a Vulkan image/view when the
    /// partial backend has not already created one. This is the Vulkan-owned
    /// half of upstream `ImageView::Handle(desc.type)` and intentionally stays
    /// in the backend wrapper.
    pub fn materialize_sampled_image_view(
        &mut self,
        view_id: ImageViewId,
        texture_type: TextureType,
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
        staging_pool: &mut StagingBufferPool,
        cmd: vk::CommandBuffer,
    ) -> Option<vk::ImageView> {
        if let Some(view) = self.image_view_handle(view_id, texture_type) {
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
        if self.textures.get(&view_id.index).is_some_and(|cached| {
            cached.format != vk_format_from_pixel_format(view_base.format).unwrap_or(cached.format)
                || cached.width != view_base.size.width.max(1)
                || cached.height != view_base.size.height.max(1)
        }) {
            if let Some(old) = self.textures.remove(&view_id.index) {
                if std::env::var_os("RUZU_TRACE_VK_TEXTURE_UPLOAD").is_some() {
                    log::warn!(
                        "[VK_TEXTURE_CACHE_RECREATE] view_id={} old={:?} {}x{} new={:?} {}x{}",
                        view_id.index,
                        old.format,
                        old.width,
                        old.height,
                        view_base.format,
                        view_base.size.width.max(1),
                        view_base.size.height.max(1)
                    );
                }
                unsafe {
                    self.device.destroy_image_view(old.view, None);
                    self.device.destroy_image(old.image, None);
                    self.device.free_memory(old.memory, None);
                }
            }
        }

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
            .create_sampled_image(format, width, height, &view_base, texture_type)
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
            let (alpha_min, alpha_max, alpha_zero, alpha_full) =
                linear
                    .chunks_exact(4)
                    .fold((u8::MAX, 0u8, 0usize, 0usize), |acc, px| {
                        let alpha = px[3];
                        (
                            acc.0.min(alpha),
                            acc.1.max(alpha),
                            acc.2 + usize::from(alpha == 0),
                            acc.3 + usize::from(alpha == u8::MAX),
                        )
                    });
            let first_copy = copies.first().copied();
            log::warn!(
                "[VK_TEXTURE_UPLOAD] view_id={} image_id={} gpu=0x{:X} cpu=0x{:X} initialized={} type={:?} fmt={:?} size={}x{} guest={} unswizzled={} copies={} cpu_modified={} guest_nonzero={} linear_nonzero={} guest_crc=0x{:X} linear_crc=0x{:X} alpha_min={} alpha_max={} alpha_zero={} alpha_full={} first_copy={:?}",
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
                alpha_min,
                alpha_max,
                alpha_zero,
                alpha_full,
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
        _shared_depth_view: vk::ImageView,
    ) -> Result<CachedRenderTarget, vk::Result> {
        let pixel_format = crate::surface::pixel_format_from_render_target_format(rt.format);
        let format = pixel_format_to_vk(pixel_format);
        let (image, memory, view) = self.create_attachment(
            format,
            vk::ImageUsageFlags::COLOR_ATTACHMENT
                | vk::ImageUsageFlags::SAMPLED
                | vk::ImageUsageFlags::TRANSFER_DST
                | vk::ImageUsageFlags::TRANSFER_SRC,
            vk::ImageAspectFlags::COLOR,
            rt.width.max(1),
            rt.height.max(1),
        )?;
        // Create a depth attachment sized to this render target rather than
        // reusing the shared window-sized depth buffer. Vulkan clamps a
        // framebuffer to its smallest attachment, so a smaller depth buffer
        // would truncate rendering to targets larger than the initial window
        // (e.g. a 1280x720 depth buffer clipping a 1920x1080 target).
        let (depth_image, depth_memory, depth_view) = self.create_attachment(
            vk::Format::D32_SFLOAT,
            vk::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT,
            vk::ImageAspectFlags::DEPTH,
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
            depth_image,
            depth_memory,
            depth_view,
            framebuffer,
            extent: render_area,
            width: rt.width.max(1),
            height: rt.height.max(1),
            layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
        })
    }

    fn create_sampler_from_tsc(&self, tsc: &TscEntry) -> Result<vk::Sampler, vk::Result> {
        let mag_filter = texture_filter_from_raw(tsc.mag_filter());
        let min_filter = texture_filter_from_raw(tsc.min_filter());
        let mipmap_filter = texture_mipmap_filter_from_raw(tsc.mipmap_filter());
        let wrap_u = wrap_mode_from_raw(tsc.wrap_u());
        let wrap_v = wrap_mode_from_raw(tsc.wrap_v());
        let wrap_p = wrap_mode_from_raw(tsc.wrap_p());
        let max_anisotropy = tsc.computed_max_anisotropy().clamp(1.0, 16.0);
        let mut custom_border_color = vk::SamplerCustomBorderColorCreateInfoEXT::builder()
            .custom_border_color(vk::ClearColorValue {
                float32: tsc.computed_border_color(),
            })
            .format(vk::Format::UNDEFINED)
            .build();
        let sampler_info = vk::SamplerCreateInfo::builder()
            .push_next(&mut custom_border_color)
            .mag_filter(maxwell_to_vk::sampler::filter(mag_filter))
            .min_filter(maxwell_to_vk::sampler::filter(min_filter))
            .mipmap_mode(maxwell_to_vk::sampler::mipmap_mode(mipmap_filter))
            .address_mode_u(maxwell_to_vk::sampler::wrap_mode(false, wrap_u, mag_filter))
            .address_mode_v(maxwell_to_vk::sampler::wrap_mode(false, wrap_v, mag_filter))
            .address_mode_w(maxwell_to_vk::sampler::wrap_mode(false, wrap_p, mag_filter))
            .mip_lod_bias(tsc.lod_bias())
            .anisotropy_enable(max_anisotropy > 1.0)
            .max_anisotropy(max_anisotropy)
            .compare_enable(tsc.depth_compare_enabled() != 0)
            .compare_op(vk::CompareOp::ALWAYS)
            .min_lod(if mipmap_filter == TextureMipmapFilter::None {
                0.0
            } else {
                tsc.min_lod()
            })
            .max_lod(if mipmap_filter == TextureMipmapFilter::None {
                0.25
            } else {
                tsc.max_lod()
            })
            .border_color(vk::BorderColor::FLOAT_CUSTOM_EXT);
        unsafe { self.device.create_sampler(&sampler_info, None) }
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
            .flags(vk::ImageCreateFlags::MUTABLE_FORMAT)
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
        view_base: &crate::texture_cache::image_view_base::ImageViewBase,
        texture_type: TextureType,
    ) -> Result<(vk::Image, vk::DeviceMemory, vk::ImageView), vk::Result> {
        let usage = vk::ImageUsageFlags::SAMPLED | vk::ImageUsageFlags::TRANSFER_DST;
        let image_info = vk::ImageCreateInfo::builder()
            .flags(vk::ImageCreateFlags::MUTABLE_FORMAT)
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

        let aspect_mask = image_view_aspect_mask(view_base);
        let mut usage_info = vk::ImageViewUsageCreateInfo::builder().usage(usage).build();
        let view_info = vk::ImageViewCreateInfo::builder()
            .push_next(&mut usage_info)
            .image(image)
            .view_type(image_view_type_from_texture_type(texture_type))
            .format(format)
            .components(image_view_components(view_base))
            .subresource_range(sampled_subresource_range(
                aspect_mask,
                view_base.range,
                texture_type,
            ))
            .build();
        let view = unsafe { self.device.create_image_view(&view_info, None)? };

        trace!(
            "TextureCache: created sampled {:?} {}x{} {:?}",
            format,
            width,
            height,
            texture_type
        );

        Ok((image, memory, view))
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

fn pixel_format_to_vk(format: PixelFormat) -> vk::Format {
    match format {
        PixelFormat::A8B8G8R8Unorm => vk::Format::A8B8G8R8_UNORM_PACK32,
        PixelFormat::A8B8G8R8Srgb => vk::Format::A8B8G8R8_SRGB_PACK32,
        PixelFormat::B8G8R8A8Unorm => vk::Format::B8G8R8A8_UNORM,
        PixelFormat::B8G8R8A8Srgb => vk::Format::B8G8R8A8_SRGB,
        PixelFormat::R5G6B5Unorm => vk::Format::R5G6B5_UNORM_PACK16,
        PixelFormat::R8Unorm => vk::Format::R8_UNORM,
        PixelFormat::R8G8Unorm => vk::Format::R8G8_UNORM,
        PixelFormat::R16Unorm => vk::Format::R16_UNORM,
        PixelFormat::R16G16Unorm => vk::Format::R16G16_UNORM,
        PixelFormat::R16G16B16A16Unorm => vk::Format::R16G16B16A16_UNORM,
        PixelFormat::R16G16B16A16Float => vk::Format::R16G16B16A16_SFLOAT,
        PixelFormat::R32Float => vk::Format::R32_SFLOAT,
        PixelFormat::R32G32Float => vk::Format::R32G32_SFLOAT,
        PixelFormat::R32G32B32A32Float => vk::Format::R32G32B32A32_SFLOAT,
        PixelFormat::D16Unorm => vk::Format::D16_UNORM,
        PixelFormat::D32Float => vk::Format::D32_SFLOAT,
        PixelFormat::S8Uint => vk::Format::S8_UINT,
        PixelFormat::D24UnormS8Uint => vk::Format::D24_UNORM_S8_UINT,
        PixelFormat::D32FloatS8Uint => vk::Format::D32_SFLOAT_S8_UINT,
        _ => vk::Format::R8G8B8A8_UNORM,
    }
}

fn image_aspect_mask(format: PixelFormat) -> vk::ImageAspectFlags {
    match crate::surface::get_format_type(format) {
        SurfaceType::ColorTexture => vk::ImageAspectFlags::COLOR,
        SurfaceType::Depth => vk::ImageAspectFlags::DEPTH,
        SurfaceType::Stencil => vk::ImageAspectFlags::STENCIL,
        SurfaceType::DepthStencil => vk::ImageAspectFlags::DEPTH | vk::ImageAspectFlags::STENCIL,
        SurfaceType::Invalid => vk::ImageAspectFlags::empty(),
    }
}

fn image_view_aspect_mask(
    view: &crate::texture_cache::image_view_base::ImageViewBase,
) -> vk::ImageAspectFlags {
    if view.is_render_target() {
        return image_aspect_mask(view.format);
    }
    let any_r = view
        .swizzle
        .iter()
        .any(|&source| source == crate::texture_cache::image_view_info::SwizzleSource::R as u8);
    match view.format {
        PixelFormat::D24UnormS8Uint | PixelFormat::D32FloatS8Uint => {
            if any_r {
                vk::ImageAspectFlags::DEPTH
            } else {
                vk::ImageAspectFlags::STENCIL
            }
        }
        PixelFormat::S8UintD24Unorm => {
            if any_r {
                vk::ImageAspectFlags::STENCIL
            } else {
                vk::ImageAspectFlags::DEPTH
            }
        }
        PixelFormat::D16Unorm | PixelFormat::D32Float | PixelFormat::X8D24Unorm => {
            vk::ImageAspectFlags::DEPTH
        }
        PixelFormat::S8Uint => vk::ImageAspectFlags::STENCIL,
        _ => vk::ImageAspectFlags::COLOR,
    }
}

fn image_view_components(
    view: &crate::texture_cache::image_view_base::ImageViewBase,
) -> vk::ComponentMapping {
    let swizzle = if view.is_render_target() {
        [
            crate::texture_cache::image_view_info::SwizzleSource::R as u8,
            crate::texture_cache::image_view_info::SwizzleSource::G as u8,
            crate::texture_cache::image_view_info::SwizzleSource::B as u8,
            crate::texture_cache::image_view_info::SwizzleSource::A as u8,
        ]
    } else {
        view.swizzle
    };
    vk::ComponentMapping {
        r: component_swizzle(swizzle[0]),
        g: component_swizzle(swizzle[1]),
        b: component_swizzle(swizzle[2]),
        a: component_swizzle(swizzle[3]),
    }
}

fn component_swizzle(source: u8) -> vk::ComponentSwizzle {
    match source {
        0 => vk::ComponentSwizzle::ZERO,
        2 => vk::ComponentSwizzle::R,
        3 => vk::ComponentSwizzle::G,
        4 => vk::ComponentSwizzle::B,
        5 => vk::ComponentSwizzle::A,
        6 | 7 => vk::ComponentSwizzle::ONE,
        _ => vk::ComponentSwizzle::IDENTITY,
    }
}

fn color2d_subresource_range(
    aspect_mask: vk::ImageAspectFlags,
    range: SubresourceRange,
    flags: crate::texture_cache::image_view_base::ImageViewFlagBits,
) -> vk::ImageSubresourceRange {
    let base_layer =
        if flags.contains(crate::texture_cache::image_view_base::ImageViewFlagBits::SLICE) {
            0
        } else {
            range.base.layer.max(0) as u32
        };
    vk::ImageSubresourceRange {
        aspect_mask,
        base_mip_level: range.base.level.max(0) as u32,
        level_count: range.extent.levels.max(1) as u32,
        base_array_layer: base_layer,
        layer_count: 1,
    }
}

fn sampled_subresource_range(
    aspect_mask: vk::ImageAspectFlags,
    range: SubresourceRange,
    texture_type: TextureType,
) -> vk::ImageSubresourceRange {
    let mut subresource_range = vk::ImageSubresourceRange {
        aspect_mask,
        base_mip_level: range.base.level.max(0) as u32,
        level_count: range.extent.levels.max(1) as u32,
        base_array_layer: range.base.layer.max(0) as u32,
        layer_count: range.extent.layers.max(1) as u32,
    };
    match texture_type {
        TextureType::Color1D | TextureType::Color2D | TextureType::Color2DRect => {
            subresource_range.layer_count = 1;
        }
        TextureType::ColorCube => {
            subresource_range.layer_count = 6;
        }
        TextureType::Color3D => {
            subresource_range.base_array_layer = 0;
            subresource_range.layer_count = 1;
        }
        TextureType::ColorArray1D | TextureType::ColorArray2D | TextureType::ColorArrayCube => {}
        TextureType::Buffer => {
            subresource_range.base_array_layer = 0;
            subresource_range.layer_count = 1;
        }
    }
    subresource_range
}

fn image_view_type_from_texture_type(texture_type: TextureType) -> vk::ImageViewType {
    match texture_type {
        TextureType::Color1D => vk::ImageViewType::TYPE_1D,
        TextureType::ColorArray1D => vk::ImageViewType::TYPE_1D_ARRAY,
        TextureType::Color2D | TextureType::Color2DRect => vk::ImageViewType::TYPE_2D,
        TextureType::ColorArray2D => vk::ImageViewType::TYPE_2D_ARRAY,
        TextureType::Color3D => vk::ImageViewType::TYPE_3D,
        TextureType::ColorCube => vk::ImageViewType::CUBE,
        TextureType::ColorArrayCube => vk::ImageViewType::CUBE_ARRAY,
        TextureType::Buffer => vk::ImageViewType::TYPE_2D,
    }
}

impl Drop for TextureCache {
    fn drop(&mut self) {
        unsafe {
            for (_, view) in self.render_target_views.drain() {
                self.device.destroy_image_view(view.view, None);
            }
            for (_, target) in self.render_targets.drain() {
                self.device.destroy_framebuffer(target.framebuffer, None);
                self.device.destroy_image_view(target.view, None);
                self.device.destroy_image(target.image, None);
                self.device.free_memory(target.memory, None);
                self.device.destroy_image_view(target.depth_view, None);
                self.device.destroy_image(target.depth_image, None);
                self.device.free_memory(target.depth_memory, None);
            }
            for (_, tex) in self.textures.drain() {
                self.device.destroy_sampler(tex.sampler, None);
                self.device.destroy_image_view(tex.view, None);
                self.device.destroy_image(tex.image, None);
                self.device.free_memory(tex.memory, None);
            }
            for (_, sampler) in self.samplers.drain() {
                self.device.destroy_sampler(sampler.sampler, None);
            }
            for (_, fb) in self.framebuffers.drain() {
                self.device.destroy_framebuffer(fb.framebuffer, None);
            }
        }
    }
}

fn texture_filter_from_raw(raw: u32) -> TextureFilter {
    match raw {
        2 => TextureFilter::Linear,
        _ => TextureFilter::Nearest,
    }
}

fn texture_mipmap_filter_from_raw(raw: u32) -> TextureMipmapFilter {
    match raw {
        2 => TextureMipmapFilter::Nearest,
        3 => TextureMipmapFilter::Linear,
        _ => TextureMipmapFilter::None,
    }
}

fn wrap_mode_from_raw(raw: u32) -> WrapMode {
    match raw {
        0 => WrapMode::Wrap,
        1 => WrapMode::Mirror,
        2 => WrapMode::ClampToEdge,
        3 => WrapMode::Border,
        4 => WrapMode::Clamp,
        5 => WrapMode::MirrorOnceClampToEdge,
        6 => WrapMode::MirrorOnceBorder,
        7 => WrapMode::MirrorOnceClampOgl,
        _ => WrapMode::ClampToEdge,
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
