// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GPU texture cache — images, views, samplers, framebuffers.
//!
//! Ref: zuyu `vk_texture_cache.h` — caches VkImage/VkImageView/VkSampler
//! objects and VkFramebuffer objects by render target configuration.

use std::collections::{HashMap, VecDeque};
use std::ptr::NonNull;
use std::sync::atomic::Ordering;
use std::sync::{Arc, OnceLock};

use ash::vk;
use ash::vk::Handle;

use crate::control::channel_state::ChannelState;
use crate::control::channel_state_cache::{ChannelInfo, ChannelSetupCaches};
use crate::engines::maxwell_dma::dma;
use crate::framebuffer_config::FramebufferConfig;
use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
use crate::surface::{PixelFormat, SurfaceType};
use crate::texture_cache::image_base::{ImageBase, ImageFlagBits};
use crate::texture_cache::image_info::ImageInfo;
use crate::texture_cache::image_view_base::{ImageViewBase, ImageViewFlagBits};
use crate::texture_cache::image_view_info::ImageViewInfo;
use crate::texture_cache::render_targets::RenderTargets;
use crate::texture_cache::texture_cache_base::TICKS_TO_DESTROY;
use crate::texture_cache::texture_cache_base::{
    AsyncDecodeContext, BufferDownload, FramebufferImageView, JoinCopy, PendingDownload,
    TextureCacheBase as CommonTextureCache,
};
use crate::texture_cache::types::{
    BufferImageCopy, Extent2D, ImageCopy, ImageId, ImageType, ImageViewId, ImageViewType,
    RelaxedOptions, SamplerId, SubresourceExtent, SubresourceRange, NULL_IMAGE_ID,
    NULL_IMAGE_VIEW_ID, NUM_RT,
};
use crate::texture_cache::util::{
    convert_image, full_download_copies, full_upload_swizzles, make_shrink_image_copies,
    map_size_bytes, unswizzle_image,
};
use crate::textures::texture::{TextureFilter, TextureMipmapFilter, TscEntry, WrapMode};
use crate::textures::workers::ThreadWorker;
use shader_recompiler::shader_info::{ImageFormat, TextureType};

use super::blit_image::{
    BlitFramebufferInfo, BlitImageHelper, ConversionImageView, Filter as BlitFilter,
    Offset2D as BlitOffset2D, Operation as BlitOperation, Region2D as BlitRegion2D,
};
use super::compute_pass::{AstcDecoderPass, MsaaCopyPass};
use super::descriptor_pool::{DescriptorBankInfo as PoolDescriptorBankInfo, DescriptorPool};
use super::maxwell_to_vk;
use super::render_pass_cache::{RenderPassCache, RenderPassKey};
use super::scheduler::Scheduler;
use super::staging_buffer_pool::{StagingBuffer, StagingBufferPool};
use super::update_descriptor::ComputePassDescriptorQueue;
use crate::vulkan_common::vulkan_device::{
    query_device_memory_info, query_device_memory_usage, DeviceMemoryInfo,
};
use crate::vulkan_common::vulkan_memory_allocator::{AllocatedImage, MemoryAllocator, MemoryUsage};

struct ImageDumpConfig {
    path: std::path::PathBuf,
    draw: Option<u32>,
    fragment_offset: Option<u32>,
    gpu_address: Option<u64>,
    cpu_address: Option<u64>,
    image_id: Option<ImageId>,
    width: Option<u32>,
    height: Option<u32>,
    every: Option<u64>,
    at: u64,
}

fn image_dump_config() -> Option<&'static ImageDumpConfig> {
    static CONFIG: OnceLock<Option<ImageDumpConfig>> = OnceLock::new();
    CONFIG
        .get_or_init(|| {
            let path = std::env::var_os("RUZU_DUMP_VK_IMAGE_FRAME")?;
            let parse_u32 = |name: &str| {
                std::env::var(name)
                    .ok()
                    .and_then(|value| value.parse::<u32>().ok())
            };
            let parse_hex_u32 = |name: &str| {
                std::env::var(name)
                    .ok()
                    .and_then(|value| u32::from_str_radix(value.trim_start_matches("0x"), 16).ok())
            };
            let parse_hex_u64 = |name: &str| {
                std::env::var(name)
                    .ok()
                    .and_then(|value| u64::from_str_radix(value.trim_start_matches("0x"), 16).ok())
            };
            Some(ImageDumpConfig {
                path: path.into(),
                draw: parse_u32("RUZU_DUMP_VK_IMAGE_DRAW"),
                fragment_offset: parse_hex_u32("RUZU_DUMP_VK_IMAGE_FS"),
                gpu_address: parse_hex_u64("RUZU_DUMP_VK_IMAGE_GPU"),
                cpu_address: parse_hex_u64("RUZU_DUMP_VK_IMAGE_CPU"),
                image_id: parse_u32("RUZU_DUMP_VK_IMAGE_ID").map(|index| ImageId { index }),
                width: parse_u32("RUZU_DUMP_VK_IMAGE_WIDTH"),
                height: parse_u32("RUZU_DUMP_VK_IMAGE_HEIGHT"),
                every: std::env::var("RUZU_DUMP_VK_IMAGE_EVERY")
                    .ok()
                    .and_then(|value| value.parse::<u64>().ok())
                    .filter(|&every| every > 0),
                at: std::env::var("RUZU_DUMP_VK_IMAGE_AT")
                    .ok()
                    .and_then(|value| value.parse::<u64>().ok())
                    .unwrap_or(1),
            })
        })
        .as_ref()
}

fn convert_border_color(color: [f32; 4]) -> vk::BorderColor {
    if color == [0.0, 0.0, 0.0, 0.0] {
        vk::BorderColor::FLOAT_TRANSPARENT_BLACK
    } else if color == [0.0, 0.0, 0.0, 1.0] {
        vk::BorderColor::FLOAT_OPAQUE_BLACK
    } else if color == [1.0, 1.0, 1.0, 1.0] {
        vk::BorderColor::FLOAT_OPAQUE_WHITE
    } else if color[0] + color[1] + color[2] > 1.35 {
        vk::BorderColor::FLOAT_OPAQUE_WHITE
    } else if color[3] > 0.5 {
        vk::BorderColor::FLOAT_OPAQUE_BLACK
    } else {
        vk::BorderColor::FLOAT_TRANSPARENT_BLACK
    }
}

/// A cached framebuffer (VkFramebuffer + associated views).
pub struct CachedFramebuffer {
    pub framebuffer: vk::Framebuffer,
    pub render_pass: vk::RenderPass,
    pub color_views: Vec<vk::ImageView>,
    pub depth_view: Option<vk::ImageView>,
    pub extent: vk::Extent2D,
}

/// Backend-owned Vulkan image.
///
/// Port-facing counterpart of upstream `Vulkan::Image`. The common
/// `ImageBase` slot remains the source of truth in `TextureCacheBase`; this
/// backend owner materializes the Vulkan image, memory, current image handle,
/// aspect mask, lazy storage views and initialization/layout state.
pub struct Image {
    pub image_id: ImageId,
    pub base: ImageBase,
    pub original_image: AllocatedImage,
    pub current_image: vk::Image,
    pub scaled_image: Option<AllocatedImage>,
    pub storage_image_views: Vec<vk::ImageView>,
    pub format: vk::Format,
    pub aspect: vk::ImageAspectFlags,
    pub initialized: bool,
    pub layout: vk::ImageLayout,
    pub scale_view: vk::ImageView,
    pub normal_view: vk::ImageView,
    pub scale_depth_view: vk::ImageView,
    pub normal_depth_view: vk::ImageView,
    pub scale_stencil_view: vk::ImageView,
    pub normal_stencil_view: vk::ImageView,
    pub scale_framebuffer: Option<BlitFramebufferInfo>,
    pub normal_framebuffer: Option<BlitFramebufferInfo>,
}

impl Image {
    fn handle(&self) -> vk::Image {
        self.current_image
    }

    fn aspect_mask(&self) -> vk::ImageAspectFlags {
        self.aspect
    }

    fn exchange_initialization(&mut self) -> bool {
        std::mem::replace(&mut self.initialized, true)
    }

    fn is_rescaled(&self) -> bool {
        self.base.flags.contains(ImageFlagBits::RESCALED)
    }

    fn blit_scale_helper_color(
        &mut self,
        runtime: &mut TextureCacheRuntime,
        scale_up: bool,
    ) -> bool {
        let view = if scale_up {
            self.scale_view
        } else {
            self.normal_view
        };
        if view == vk::ImageView::null() {
            let view = match runtime.create_blit_color_view(self.handle(), self.format) {
                Ok(view) => view,
                Err(err) => {
                    log::warn!(
                        "TextureCacheVulkan: failed to create BlitScaleHelper color view id={} err={:?}",
                        self.image_id.index,
                        err
                    );
                    return false;
                }
            };
            if scale_up {
                self.scale_view = view;
            } else {
                self.normal_view = view;
            }
        }

        let is_2d = self.base.info.image_type == ImageType::E2D;
        let scaled_width = runtime.resolution.scale_up_u32(self.base.info.size.width);
        let scaled_height = if is_2d {
            runtime.resolution.scale_up_u32(self.base.info.size.height)
        } else {
            self.base.info.size.height
        };
        let extent = vk::Extent2D {
            width: scaled_width.max(self.base.info.size.width),
            height: scaled_height.max(self.base.info.size.height),
        };

        let (view, framebuffer) = if scale_up {
            (self.scale_view, self.scale_framebuffer)
        } else {
            (self.normal_view, self.normal_framebuffer)
        };
        if framebuffer.is_none() {
            let framebuffer = match runtime.create_blit_color_framebuffer(
                self.handle(),
                view,
                self.format,
                extent,
            ) {
                Ok(framebuffer) => framebuffer,
                Err(err) => {
                    log::warn!(
                            "TextureCacheVulkan: failed to create BlitScaleHelper color framebuffer id={} err={:?}",
                            self.image_id.index,
                            err
                        );
                    return false;
                }
            };
            if scale_up {
                self.scale_framebuffer = Some(framebuffer);
            } else {
                self.normal_framebuffer = Some(framebuffer);
            }
        }

        let (view, framebuffer) = if scale_up {
            (self.scale_view, self.scale_framebuffer)
        } else {
            (self.normal_view, self.normal_framebuffer)
        };
        let Some(framebuffer) = framebuffer else {
            return false;
        };
        let src_width = if scale_up {
            self.base.info.size.width
        } else {
            scaled_width
        };
        let src_height = if scale_up {
            self.base.info.size.height
        } else {
            scaled_height
        };
        let dst_width = if scale_up {
            scaled_width
        } else {
            self.base.info.size.width
        };
        let dst_height = if scale_up {
            scaled_height
        } else {
            self.base.info.size.height
        };
        let src_region = BlitRegion2D {
            start: BlitOffset2D { x: 0, y: 0 },
            end: BlitOffset2D {
                x: src_width as i32,
                y: src_height as i32,
            },
        };
        let dst_region = BlitRegion2D {
            start: BlitOffset2D { x: 0, y: 0 },
            end: BlitOffset2D {
                x: dst_width as i32,
                y: dst_height as i32,
            },
        };
        let filter = if !crate::surface::is_pixel_format_integer(self.base.info.format) {
            BlitFilter::Bilinear
        } else {
            BlitFilter::PointSample
        };
        runtime.blit_image_helper().blit_color(
            framebuffer,
            view,
            &dst_region,
            &src_region,
            filter,
            BlitOperation::SrcCopy,
        )
    }

    fn blit_scale_helper_depth_stencil(
        &mut self,
        runtime: &mut TextureCacheRuntime,
        scale_up: bool,
    ) -> bool {
        let view = if scale_up {
            self.scale_view
        } else {
            self.normal_view
        };
        if view == vk::ImageView::null() {
            let render_target_view = match runtime.create_blit_image_view(
                self.handle(),
                self.format,
                vk::ImageAspectFlags::DEPTH | vk::ImageAspectFlags::STENCIL,
            ) {
                Ok(view) => view,
                Err(err) => {
                    log::warn!(
                        "TextureCacheVulkan: failed to create BlitScaleHelper depth/stencil target view id={} err={:?}",
                        self.image_id.index,
                        err
                    );
                    return false;
                }
            };
            let depth_view = match runtime.create_blit_image_view(
                self.handle(),
                self.format,
                vk::ImageAspectFlags::DEPTH,
            ) {
                Ok(view) => view,
                Err(err) => {
                    unsafe {
                        runtime
                            .device()
                            .destroy_image_view(render_target_view, None);
                    }
                    log::warn!(
                        "TextureCacheVulkan: failed to create BlitScaleHelper depth view id={} err={:?}",
                        self.image_id.index,
                        err
                    );
                    return false;
                }
            };
            let stencil_view = match runtime.create_blit_image_view(
                self.handle(),
                self.format,
                vk::ImageAspectFlags::STENCIL,
            ) {
                Ok(view) => view,
                Err(err) => {
                    unsafe {
                        runtime
                            .device()
                            .destroy_image_view(render_target_view, None);
                        runtime.device().destroy_image_view(depth_view, None);
                    }
                    log::warn!(
                        "TextureCacheVulkan: failed to create BlitScaleHelper stencil view id={} err={:?}",
                        self.image_id.index,
                        err
                    );
                    return false;
                }
            };
            if scale_up {
                self.scale_view = render_target_view;
                self.scale_depth_view = depth_view;
                self.scale_stencil_view = stencil_view;
            } else {
                self.normal_view = render_target_view;
                self.normal_depth_view = depth_view;
                self.normal_stencil_view = stencil_view;
            }
        }

        let is_2d = self.base.info.image_type == ImageType::E2D;
        let scaled_width = runtime.resolution.scale_up_u32(self.base.info.size.width);
        let scaled_height = if is_2d {
            runtime.resolution.scale_up_u32(self.base.info.size.height)
        } else {
            self.base.info.size.height
        };
        let extent = vk::Extent2D {
            width: scaled_width.max(self.base.info.size.width),
            height: scaled_height.max(self.base.info.size.height),
        };
        let (view, framebuffer) = if scale_up {
            (self.scale_view, self.scale_framebuffer)
        } else {
            (self.normal_view, self.normal_framebuffer)
        };
        if framebuffer.is_none() {
            let framebuffer = match runtime.create_blit_depth_stencil_framebuffer(
                self.handle(),
                view,
                self.format,
                extent,
            ) {
                Ok(framebuffer) => framebuffer,
                Err(err) => {
                    log::warn!(
                        "TextureCacheVulkan: failed to create BlitScaleHelper depth/stencil framebuffer id={} err={:?}",
                        self.image_id.index,
                        err
                    );
                    return false;
                }
            };
            if scale_up {
                self.scale_framebuffer = Some(framebuffer);
            } else {
                self.normal_framebuffer = Some(framebuffer);
            }
        }

        let (depth_view, stencil_view, framebuffer) = if scale_up {
            (
                self.scale_depth_view,
                self.scale_stencil_view,
                self.scale_framebuffer,
            )
        } else {
            (
                self.normal_depth_view,
                self.normal_stencil_view,
                self.normal_framebuffer,
            )
        };
        let Some(framebuffer) = framebuffer else {
            return false;
        };
        let src_width = if scale_up {
            self.base.info.size.width
        } else {
            scaled_width
        };
        let src_height = if scale_up {
            self.base.info.size.height
        } else {
            scaled_height
        };
        let dst_width = if scale_up {
            scaled_width
        } else {
            self.base.info.size.width
        };
        let dst_height = if scale_up {
            scaled_height
        } else {
            self.base.info.size.height
        };
        let src_region = BlitRegion2D {
            start: BlitOffset2D { x: 0, y: 0 },
            end: BlitOffset2D {
                x: src_width as i32,
                y: src_height as i32,
            },
        };
        let dst_region = BlitRegion2D {
            start: BlitOffset2D { x: 0, y: 0 },
            end: BlitOffset2D {
                x: dst_width as i32,
                y: dst_height as i32,
            },
        };
        runtime.blit_image_helper().blit_depth_stencil(
            framebuffer,
            depth_view,
            stencil_view,
            &dst_region,
            &src_region,
            BlitFilter::PointSample,
            BlitOperation::SrcCopy,
        )
    }

    fn scale_up(&mut self, runtime: &mut TextureCacheRuntime, mut ignore: bool) -> bool {
        if !runtime.resolution.active {
            return false;
        }
        if self.base.flags.contains(ImageFlagBits::RESCALED) {
            return false;
        }
        if self.base.info.image_type == ImageType::Linear {
            return false;
        }
        self.base.flags.insert(ImageFlagBits::RESCALED);
        self.base.has_scaled = true;
        if self.scaled_image.is_none() {
            let scaled_image = match runtime.create_scaled_image(&self.base.info, self.format) {
                Ok(image) => image,
                Err(err) => {
                    log::warn!(
                        "TextureCacheVulkan: failed to create scaled image id={} err={:?}",
                        self.image_id.index,
                        err
                    );
                    self.base.flags.remove(ImageFlagBits::RESCALED);
                    return false;
                }
            };
            self.scaled_image = Some(scaled_image);
            ignore = false;
        }
        let Some(scaled) = self.scaled_image.as_ref() else {
            self.base.flags.remove(ImageFlagBits::RESCALED);
            return false;
        };
        self.current_image = scaled.handle();
        self.layout = vk::ImageLayout::GENERAL;
        if ignore {
            return true;
        }
        if runtime.needs_scale_helper(&self.base.info, self.format) {
            if self.aspect == vk::ImageAspectFlags::COLOR {
                return self.blit_scale_helper_color(runtime, true);
            }
            if self.aspect == (vk::ImageAspectFlags::DEPTH | vk::ImageAspectFlags::STENCIL) {
                return self.blit_scale_helper_depth_stencil(runtime, true);
            }
            log::warn!(
                "TextureCacheVulkan: ScaleUp needs unsupported BlitScaleHelper aspect for image id={} format={:?}",
                self.image_id.index,
                self.base.info.format
            );
            return false;
        }
        runtime.blit_scale(
            self.original_image.handle(),
            scaled.handle(),
            self.base.info.clone(),
            self.aspect,
            true,
        );
        true
    }

    fn scale_down(&mut self, runtime: &mut TextureCacheRuntime, ignore: bool) -> bool {
        if !runtime.resolution.active {
            return false;
        }
        if !self.base.flags.contains(ImageFlagBits::RESCALED) {
            return false;
        }
        if self.base.info.image_type == ImageType::Linear {
            return false;
        }
        if self.scaled_image.is_none() {
            return false;
        }
        let scaled = self.scaled_image.as_ref().unwrap().handle();
        self.base.flags.remove(ImageFlagBits::RESCALED);
        self.current_image = self.original_image.handle();
        self.layout = vk::ImageLayout::GENERAL;
        if ignore {
            return true;
        }
        if runtime.needs_scale_helper(&self.base.info, self.format) {
            if self.aspect == vk::ImageAspectFlags::COLOR {
                return self.blit_scale_helper_color(runtime, false);
            }
            if self.aspect == (vk::ImageAspectFlags::DEPTH | vk::ImageAspectFlags::STENCIL) {
                return self.blit_scale_helper_depth_stencil(runtime, false);
            }
            log::warn!(
                "TextureCacheVulkan: ScaleDown needs unsupported BlitScaleHelper aspect for image id={} format={:?}",
                self.image_id.index,
                self.base.info.format
            );
            return false;
        }
        runtime.blit_scale(
            scaled,
            self.original_image.handle(),
            self.base.info.clone(),
            self.aspect,
            false,
        );
        true
    }

    /// Port-facing counterpart of upstream `Vulkan::Image::UploadMemory`.
    fn upload_memory(
        &mut self,
        runtime: &mut TextureCacheRuntime,
        staging_buffer: vk::Buffer,
        staging_offset: vk::DeviceSize,
        copies: &[BufferImageCopy],
    ) -> bool {
        if copies.is_empty() {
            return true;
        }
        let is_rescaled = self.is_rescaled();
        if is_rescaled && !self.scale_down(runtime, true) {
            return false;
        }
        let is_initialized = self.exchange_initialization();
        let image = self.original_image.handle();
        let aspect = self.aspect_mask();
        let vk_copies = transform_buffer_image_copies(copies, staging_offset, aspect);

        let device = runtime.device().clone();
        let scheduler = runtime.scheduler();
        scheduler.request_outside_renderpass();
        scheduler.record(move |cmd| unsafe {
            copy_buffer_to_image(
                &device,
                cmd,
                staging_buffer,
                image,
                aspect,
                is_initialized,
                &vk_copies,
            );
        });
        self.layout = vk::ImageLayout::GENERAL;
        if is_rescaled && !self.scale_up(runtime, false) {
            return false;
        }
        true
    }

    /// Port-facing counterpart of upstream `Vulkan::Image::DownloadMemory`.
    fn download_memory(
        &mut self,
        runtime: &mut TextureCacheRuntime,
        buffers: &[vk::Buffer],
        offsets: &[vk::DeviceSize],
        copies: &[BufferImageCopy],
    ) -> bool {
        if copies.is_empty() {
            return true;
        }
        if buffers.len() != offsets.len() {
            return false;
        }
        let is_rescaled = self.is_rescaled();
        if is_rescaled && !self.scale_down(runtime, false) {
            return false;
        }

        let image = self.original_image.handle();
        let aspect = self.aspect_mask();
        let buffers = buffers.to_vec();
        let vk_copies = offsets
            .iter()
            .map(|offset| transform_buffer_image_copies(copies, *offset, aspect))
            .collect::<Vec<_>>();

        let device = runtime.device().clone();
        let scheduler = runtime.scheduler();
        scheduler.request_outside_renderpass();
        scheduler.record(move |cmd| unsafe {
            let read_barrier = vk::ImageMemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::MEMORY_WRITE)
                .dst_access_mask(vk::AccessFlags::TRANSFER_READ)
                .old_layout(vk::ImageLayout::GENERAL)
                .new_layout(vk::ImageLayout::TRANSFER_SRC_OPTIMAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(image)
                .subresource_range(vk::ImageSubresourceRange {
                    aspect_mask: aspect,
                    base_mip_level: 0,
                    level_count: vk::REMAINING_MIP_LEVELS,
                    base_array_layer: 0,
                    layer_count: vk::REMAINING_ARRAY_LAYERS,
                })
                .build();
            device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &[read_barrier],
            );

            for (buffer, copies) in buffers.iter().zip(vk_copies.iter()) {
                device.cmd_copy_image_to_buffer(
                    cmd,
                    image,
                    vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                    *buffer,
                    copies,
                );
            }

            let memory_write_barrier = vk::MemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::MEMORY_WRITE)
                .dst_access_mask(vk::AccessFlags::MEMORY_READ | vk::AccessFlags::MEMORY_WRITE)
                .build();
            let image_write_barrier = vk::ImageMemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::empty())
                .dst_access_mask(vk::AccessFlags::MEMORY_WRITE)
                .old_layout(vk::ImageLayout::TRANSFER_SRC_OPTIMAL)
                .new_layout(vk::ImageLayout::GENERAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(image)
                .subresource_range(vk::ImageSubresourceRange {
                    aspect_mask: aspect,
                    base_mip_level: 0,
                    level_count: vk::REMAINING_MIP_LEVELS,
                    base_array_layer: 0,
                    layer_count: vk::REMAINING_ARRAY_LAYERS,
                })
                .build();
            device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::DependencyFlags::empty(),
                &[memory_write_barrier],
                &[],
                &[image_write_barrier],
            );
        });
        self.layout = vk::ImageLayout::GENERAL;

        if is_rescaled && !self.scale_up(runtime, true) {
            return false;
        }
        true
    }

    fn download_memory_to_staging(
        &mut self,
        runtime: &mut TextureCacheRuntime,
        staging: &StagingBuffer,
        copies: &[BufferImageCopy],
    ) -> bool {
        self.download_memory(runtime, &[staging.buffer], &[staging.offset], copies)
    }
}

/// Backend-owned Vulkan view corresponding to upstream `Vulkan::ImageView`.
pub struct ImageView {
    pub view_id: ImageViewId,
    pub base: ImageViewBase,
    pub image_handle: vk::Image,
    pub image_views: [vk::ImageView; shader_recompiler::shader_info::NUM_TEXTURE_TYPES as usize],
    pub render_target: vk::ImageView,
    pub depth_view: vk::ImageView,
    pub stencil_view: vk::ImageView,
    pub color_view: vk::ImageView,
    pub storage_signeds:
        [vk::ImageView; shader_recompiler::shader_info::NUM_TEXTURE_TYPES as usize],
    pub storage_unsigneds:
        [vk::ImageView; shader_recompiler::shader_info::NUM_TEXTURE_TYPES as usize],
    pub samples: vk::SampleCountFlags,
    pub buffer_size: u32,
}

impl ImageView {
    fn handle(&self, texture_type: TextureType) -> vk::ImageView {
        self.image_views[texture_type as usize]
    }

    fn render_target(&self) -> vk::ImageView {
        self.render_target
    }

    fn image_handle(&self) -> vk::Image {
        self.image_handle
    }

    fn samples(&self) -> vk::SampleCountFlags {
        self.samples
    }
}

/// Backend-owned framebuffer corresponding to upstream `Vulkan::Framebuffer`.
pub struct Framebuffer {
    framebuffer: vk::Framebuffer,
    render_pass: vk::RenderPass,
    render_area: vk::Extent2D,
    num_color_buffers: u32,
    has_depth: bool,
    has_stencil: bool,
    is_rescaled: bool,
    samples: vk::SampleCountFlags,
    rt_map: [u8; NUM_RT],
    images: [vk::Image; NUM_RT + 1],
    image_ranges: [vk::ImageSubresourceRange; NUM_RT + 1],
    num_images: usize,
    image_ids: Vec<ImageId>,
    /// CPU address of RT0, used by the present/layout paths.
    rt0_cpu_addr: u64,
}

/// Backend-owned sampler corresponding to an upstream `TSCEntry`.
pub struct CachedSampler {
    pub sampler: vk::Sampler,
}

#[derive(Debug, Clone)]
pub struct RenderTargetFramebuffer {
    pub framebuffer: vk::Framebuffer,
    pub render_pass: vk::RenderPass,
    pub cpu_addr: u64,
    pub extent: vk::Extent2D,
    pub num_color: u32,
    pub has_depth: bool,
    pub has_stencil: bool,
    pub images: Vec<vk::Image>,
    pub image_ranges: Vec<vk::ImageSubresourceRange>,
    pub image_ids: Vec<ImageId>,
    rt_map: [u8; NUM_RT],
}

impl RenderTargetFramebuffer {
    /// Port of `Vulkan::Framebuffer::HasAspectColorBit`.
    pub fn has_aspect_color_bit(&self, index: usize) -> bool {
        let Some(&mapped) = self.rt_map.get(index) else {
            return false;
        };
        if mapped == u8::MAX {
            return false;
        }
        self.image_ranges
            .get(mapped as usize)
            .is_some_and(|range| range.aspect_mask.contains(vk::ImageAspectFlags::COLOR))
    }

    pub fn blit_framebuffer_info(&self) -> BlitFramebufferInfo {
        let mut images = [vk::Image::null(); NUM_RT + 1];
        let mut image_ranges = [vk::ImageSubresourceRange::default(); NUM_RT + 1];
        let num_images = self.images.len().min(NUM_RT + 1);
        images[..num_images].copy_from_slice(&self.images[..num_images]);
        image_ranges[..num_images].copy_from_slice(&self.image_ranges[..num_images]);
        BlitFramebufferInfo {
            framebuffer: self.framebuffer,
            render_pass: self.render_pass,
            render_area: self.extent,
            images,
            image_ranges,
            num_images,
        }
    }
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

enum DeferredVkResource {
    Framebuffer(vk::Framebuffer),
    ImageView(ImageView),
    Image(Image),
}

struct SentencedVkResource {
    retire_tick: u64,
    resource: DeferredVkResource,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum JoinCopyOperation {
    CopyImage,
    CopyImageMsaa,
    Reinterpret,
    Convert,
}

#[derive(Debug, Clone)]
enum PendingJoinCopyAction {
    Copy {
        src_id: ImageId,
        copies: Vec<ImageCopy>,
        modification_tick: u64,
    },
    DeleteOverlap {
        image_id: ImageId,
    },
}

fn should_reinterpret_join_copy(
    dst: &ImageBase,
    src: &ImageBase,
    shader_stencil_export_supported: bool,
) -> bool {
    if crate::surface::get_format_type(dst.info.format) == SurfaceType::DepthStencil
        && !shader_stencil_export_supported
    {
        return true;
    }
    dst.info.format == PixelFormat::D32FloatS8Uint || src.info.format == PixelFormat::D32FloatS8Uint
}

fn can_convert_join_copy_formats(dst_format: PixelFormat, src_format: PixelFormat) -> bool {
    matches!(
        (dst_format, src_format),
        (PixelFormat::R16Unorm, PixelFormat::D16Unorm)
            | (PixelFormat::A8B8G8R8Srgb, PixelFormat::D32Float)
            | (PixelFormat::A8B8G8R8Unorm, PixelFormat::S8UintD24Unorm)
            | (PixelFormat::A8B8G8R8Unorm, PixelFormat::D24UnormS8Uint)
            | (PixelFormat::A8B8G8R8Unorm, PixelFormat::D32Float)
            | (PixelFormat::B8G8R8A8Srgb, PixelFormat::D32Float)
            | (PixelFormat::B8G8R8A8Unorm, PixelFormat::D32Float)
            | (PixelFormat::R32Float, PixelFormat::D32Float)
            | (PixelFormat::D16Unorm, PixelFormat::R16Unorm)
            | (PixelFormat::S8UintD24Unorm, PixelFormat::A8B8G8R8Unorm)
            | (PixelFormat::S8UintD24Unorm, PixelFormat::B8G8R8A8Unorm)
            | (PixelFormat::D32Float, PixelFormat::A8B8G8R8Unorm)
            | (PixelFormat::D32Float, PixelFormat::B8G8R8A8Unorm)
            | (PixelFormat::D32Float, PixelFormat::A8B8G8R8Srgb)
            | (PixelFormat::D32Float, PixelFormat::B8G8R8A8Srgb)
            | (PixelFormat::D32Float, PixelFormat::R32Float)
    )
}

fn select_join_copy_operation(
    dst: &ImageBase,
    src: &ImageBase,
    shader_stencil_export_supported: bool,
) -> Option<JoinCopyOperation> {
    let dst_format_type = crate::surface::get_format_type(dst.info.format);
    let src_format_type = crate::surface::get_format_type(src.info.format);
    if dst_format_type == src_format_type {
        return Some(if dst.info.num_samples != src.info.num_samples {
            JoinCopyOperation::CopyImageMsaa
        } else {
            JoinCopyOperation::CopyImage
        });
    }
    if dst.info.image_type != ImageType::E2D || src.info.image_type != ImageType::E2D {
        return None;
    }
    if should_reinterpret_join_copy(dst, src, shader_stencil_export_supported) {
        return Some(JoinCopyOperation::Reinterpret);
    }
    can_convert_join_copy_formats(dst.info.format, src.info.format)
        .then_some(JoinCopyOperation::Convert)
}

fn make_pending_join_copy_actions(
    base: &mut CommonTextureCache,
    new_image_id: ImageId,
    alias_indices: &HashMap<ImageId, usize>,
    copy_object: &JoinCopy,
    can_rescale: bool,
) -> Vec<PendingJoinCopyAction> {
    if !TextureCache::base_image_exists_in(base, new_image_id)
        || !TextureCache::base_image_exists_in(base, copy_object.id)
    {
        return Vec::new();
    }

    if copy_object.is_alias {
        if !base.slot_images[copy_object.id].is_safe_download() {
            return Vec::new();
        }
        let Some(&alias_index) = alias_indices.get(&copy_object.id) else {
            return Vec::new();
        };
        let Some(alias) = base.slot_images[new_image_id]
            .aliased_images
            .get(alias_index)
            .cloned()
        else {
            return Vec::new();
        };
        return vec![PendingJoinCopyAction::Copy {
            src_id: alias.id,
            copies: alias.copies,
            modification_tick: base.slot_images[copy_object.id].modification_tick,
        }];
    }

    let overlap_snapshot = base.slot_images[copy_object.id].clone();
    let mut actions = Vec::new();
    if copy_object.gpu_modified_at_join
        || overlap_snapshot.flags.contains(ImageFlagBits::GPU_MODIFIED)
    {
        base.slot_images[new_image_id]
            .flags
            .insert(ImageFlagBits::GPU_MODIFIED);
        let base_subresource = base.slot_images[new_image_id]
            .try_find_base(overlap_snapshot.gpu_addr)
            .unwrap_or_else(|| {
                panic!(
                    "TextureCacheVulkan: pending join overlap base not found: dst={} src={}",
                    new_image_id.index, copy_object.id.index
                )
            });
        let new_info = base.slot_images[new_image_id].info.clone();
        let resolution = common::settings::values().resolution_info.clone();
        let (up_scale, down_shift) = if can_rescale {
            (resolution.up_scale, resolution.down_shift)
        } else {
            (1, 0)
        };
        actions.push(PendingJoinCopyAction::Copy {
            src_id: copy_object.id,
            copies: make_shrink_image_copies(
                &new_info,
                &overlap_snapshot.info,
                base_subresource,
                up_scale,
                down_shift,
            ),
            modification_tick: overlap_snapshot.modification_tick,
        });
    }

    actions.push(PendingJoinCopyAction::DeleteOverlap {
        image_id: copy_object.id,
    });
    actions
}

/// Runtime services used by the Vulkan texture cache backend.
///
/// Port-facing counterpart of upstream `Vulkan::TextureCacheRuntime`. In the
/// current split architecture, the common cache owns slot metadata and this
/// runtime owns Vulkan resource creation/destruction plus stable references to
/// the scheduler/staging services it needs for transfers.
pub struct TextureCacheRuntime {
    device: ash::Device,
    instance: ash::Instance,
    physical_device: vk::PhysicalDevice,
    scheduler: NonNull<Scheduler>,
    memory_allocator: NonNull<MemoryAllocator>,
    staging_buffer_pool: NonNull<StagingBufferPool>,
    blit_image_helper: NonNull<BlitImageHelper>,
    render_pass_cache: NonNull<RenderPassCache>,
    descriptor_pool: NonNull<DescriptorPool>,
    compute_pass_descriptor_queue: NonNull<ComputePassDescriptorQueue>,
    astc_decoder_pass: Option<AstcDecoderPass>,
    /// Cached `vkGetPhysicalDeviceFormatProperties` results (upstream caches
    /// these in `Device`); queried on hot per-draw paths via `surface_format`.
    format_properties:
        std::cell::RefCell<std::collections::HashMap<vk::Format, vk::FormatProperties>>,
    msaa_copy_pass: Option<MsaaCopyPass>,
    shader_stencil_export_supported: bool,
    resolution: common::settings::ResolutionScalingInfo,
    view_formats: Vec<Vec<vk::Format>>,
    buffers: [vk::Buffer; Self::INDEXING_SLOTS],
    device_memory_info: DeviceMemoryInfo,
    sentenced_resources: Vec<SentencedVkResource>,
    current_tick: u64,
    optimal_bcn_supported: bool,
    custom_border_color_supported: bool,
}

impl TextureCacheRuntime {
    const INDEXING_SLOTS: usize = 8 * std::mem::size_of::<usize>();

    pub fn new(
        device: ash::Device,
        instance: ash::Instance,
        physical_device: vk::PhysicalDevice,
        scheduler: &mut Scheduler,
        memory_allocator: &mut MemoryAllocator,
        staging_buffer_pool: &mut StagingBufferPool,
        blit_image_helper: &mut BlitImageHelper,
        render_pass_cache: &mut RenderPassCache,
        descriptor_pool: &mut DescriptorPool,
        compute_pass_descriptor_queue: &mut ComputePassDescriptorQueue,
        custom_border_color_supported: bool,
    ) -> Self {
        let device_memory_info = query_device_memory_info(&instance, physical_device);
        let storage_image_multisample_supported = unsafe {
            instance
                .get_physical_device_features(physical_device)
                .shader_storage_image_multisample
                != 0
        };
        let optimal_bcn_supported = unsafe {
            instance
                .get_physical_device_features(physical_device)
                .texture_compression_bc
                != 0
        };
        let astc_decoder_pass =
            match AstcDecoderPass::new(&device, descriptor_pool, compute_pass_descriptor_queue) {
                Ok(pass) => Some(pass),
                Err(err) => {
                    log::warn!(
                        "TextureCacheRuntime: failed to create ASTCDecoderPass: {:?}",
                        err
                    );
                    None
                }
            };
        let msaa_copy_pass = if storage_image_multisample_supported {
            match MsaaCopyPass::new(&device, descriptor_pool, compute_pass_descriptor_queue) {
                Ok(pass) => Some(pass),
                Err(err) => {
                    log::warn!(
                        "TextureCacheRuntime: failed to create MSAACopyPass: {:?}",
                        err
                    );
                    None
                }
            }
        } else {
            None
        };
        let shader_stencil_export_supported = blit_image_helper.shader_stencil_export_supported();
        Self {
            device,
            instance,
            physical_device,
            scheduler: NonNull::from(scheduler),
            memory_allocator: NonNull::from(memory_allocator),
            staging_buffer_pool: NonNull::from(staging_buffer_pool),
            blit_image_helper: NonNull::from(blit_image_helper),
            render_pass_cache: NonNull::from(render_pass_cache),
            descriptor_pool: NonNull::from(descriptor_pool),
            compute_pass_descriptor_queue: NonNull::from(compute_pass_descriptor_queue),
            astc_decoder_pass,
            format_properties: std::cell::RefCell::new(std::collections::HashMap::new()),
            msaa_copy_pass,
            shader_stencil_export_supported,
            resolution: common::settings::values().resolution_info.clone(),
            view_formats: vec![Vec::new(); crate::surface::MAX_PIXEL_FORMAT as usize],
            buffers: [vk::Buffer::null(); Self::INDEXING_SLOTS],
            device_memory_info,
            sentenced_resources: Vec::new(),
            current_tick: 0,
            optimal_bcn_supported,
            custom_border_color_supported,
        }
    }

    fn device(&self) -> &ash::Device {
        &self.device
    }

    fn get_device_local_memory(&self) -> u64 {
        self.device_memory_info.device_local_memory
    }

    fn get_device_memory_usage(&self) -> u64 {
        query_device_memory_usage(
            &self.instance,
            self.physical_device,
            &self.device_memory_info,
        )
    }

    fn can_report_memory_usage(&self) -> bool {
        self.device_memory_info.can_report_memory_usage
    }

    fn scheduler(&mut self) -> &mut Scheduler {
        // SAFETY: `TextureCacheRuntime` is constructed with pointers to boxed
        // `RasterizerVulkan` services. The boxes keep stable addresses and the
        // runtime is dropped before those services.
        unsafe { self.scheduler.as_mut() }
    }

    fn staging_buffer_pool(&mut self) -> &mut StagingBufferPool {
        // SAFETY: see `scheduler`.
        unsafe { self.staging_buffer_pool.as_mut() }
    }

    fn memory_allocator(&mut self) -> &mut MemoryAllocator {
        // SAFETY: see `scheduler`.
        unsafe { self.memory_allocator.as_mut() }
    }

    fn render_pass_cache(&mut self) -> &mut RenderPassCache {
        // SAFETY: see `scheduler`.
        unsafe { self.render_pass_cache.as_mut() }
    }

    fn blit_image_helper(&mut self) -> &mut BlitImageHelper {
        // SAFETY: see `scheduler`.
        unsafe { self.blit_image_helper.as_mut() }
    }

    fn upload_staging_buffer(&mut self, size: vk::DeviceSize) -> Option<StagingBuffer> {
        self.staging_buffer_pool().request_upload_buffer(size)
    }

    fn download_staging_buffer(
        &mut self,
        size: vk::DeviceSize,
        deferred: bool,
    ) -> Option<StagingBuffer> {
        self.staging_buffer_pool()
            .request_download_buffer(size, deferred)
    }

    fn free_deferred_staging_buffer(&mut self, buffer: &mut StagingBuffer) {
        self.staging_buffer_pool().free_deferred(buffer);
    }

    fn finish(&mut self) {
        self.scheduler().finish();
    }

    fn insert_upload_memory_barrier(&mut self) {
        // Upstream Vulkan keeps this empty: `Image::UploadMemory` records
        // `CopyBufferToImage`, including its post-copy barrier, in the same
        // scheduler stream as its consumers.
    }

    fn transition_image_layout(&mut self, image: &mut Image) {
        if image.exchange_initialization() {
            return;
        }
        let image_handle = image.handle();
        let aspect_mask = image.aspect_mask();
        let device = self.device.clone();
        let scheduler = self.scheduler();
        scheduler.request_outside_renderpass();
        scheduler.record(move |cmd| unsafe {
            let barrier = vk::ImageMemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::empty())
                .dst_access_mask(vk::AccessFlags::MEMORY_READ | vk::AccessFlags::MEMORY_WRITE)
                .old_layout(vk::ImageLayout::UNDEFINED)
                .new_layout(vk::ImageLayout::GENERAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(image_handle)
                .subresource_range(vk::ImageSubresourceRange {
                    aspect_mask,
                    base_mip_level: 0,
                    level_count: vk::REMAINING_MIP_LEVELS,
                    base_array_layer: 0,
                    layer_count: vk::REMAINING_ARRAY_LAYERS,
                })
                .build();
            device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &[barrier],
            );
        });
        image.layout = vk::ImageLayout::GENERAL;
    }

    fn barrier_feedback_loop(&mut self) {
        self.scheduler().request_outside_renderpass();
    }

    fn accelerate_image_upload(
        &mut self,
        image: &mut Image,
        map: StagingBuffer,
        swizzles: &[crate::texture_cache::types::SwizzleParameters],
    ) -> bool {
        if !crate::surface::is_pixel_format_astc(image.base.info.format) {
            log::warn!(
                "TextureCacheRuntime::accelerate_image_upload unsupported format {:?}",
                image.base.info.format
            );
            return false;
        }
        let Some(mut pass) = self.astc_decoder_pass.take() else {
            log::warn!("TextureCacheRuntime::accelerate_image_upload missing ASTCDecoderPass");
            return false;
        };
        let mut storage_views =
            vec![vk::ImageView::null(); image.base.info.resources.levels.max(1) as usize];
        for swizzle in swizzles {
            let view = match self.storage_image_view_with_format(
                image,
                swizzle.level as u32,
                vk::Format::A8B8G8R8_UNORM_PACK32,
            ) {
                Ok(view) => view,
                Err(err) => {
                    log::warn!(
                        "TextureCacheRuntime::accelerate_image_upload failed to create storage view level={} err={:?}",
                        swizzle.level,
                        err
                    );
                    self.astc_decoder_pass = Some(pass);
                    return false;
                }
            };
            let Some(slot) = storage_views.get_mut(swizzle.level as usize) else {
                self.astc_decoder_pass = Some(pass);
                return false;
            };
            *slot = view;
        }
        let is_initialized = image.exchange_initialization();
        let device = self.device.clone();
        let result = pass.assemble(
            &device,
            self.scheduler(),
            image.handle(),
            image.aspect_mask(),
            is_initialized,
            &image.base.info,
            image.base.guest_size_bytes as usize,
            map.buffer,
            map.offset,
            swizzles,
            &storage_views,
        );
        image.layout = vk::ImageLayout::GENERAL;
        self.astc_decoder_pass = Some(pass);
        result
    }

    fn get_temporary_buffer(&mut self, needed_size: usize) -> Option<vk::Buffer> {
        let needed_size = needed_size.max(1);
        let level = (usize::BITS - (needed_size - 1).leading_zeros()) as usize;
        let buffer = *self.buffers.get(level)?;
        if buffer != vk::Buffer::null() {
            return Some(buffer);
        }

        let new_size = needed_size.checked_next_power_of_two()? as vk::DeviceSize;
        let create_info = vk::BufferCreateInfo::builder()
            .size(new_size)
            .usage(
                vk::BufferUsageFlags::TRANSFER_SRC
                    | vk::BufferUsageFlags::TRANSFER_DST
                    | vk::BufferUsageFlags::UNIFORM_TEXEL_BUFFER
                    | vk::BufferUsageFlags::STORAGE_TEXEL_BUFFER,
            )
            .sharing_mode(vk::SharingMode::EXCLUSIVE)
            .build();
        let buffer = match self
            .memory_allocator()
            .create_buffer(&create_info, MemoryUsage::DeviceLocal)
        {
            Ok(buffer) => buffer,
            Err(err) => {
                log::warn!(
                    "TextureCacheRuntime::get_temporary_buffer: failed to create {} byte buffer: {:?}",
                    new_size,
                    err
                );
                return None;
            }
        };
        self.buffers[level] = buffer;
        Some(buffer)
    }

    fn reinterpret_image(&mut self, dst: &Image, src: &Image, copies: &[ImageCopy]) -> bool {
        if copies.is_empty() {
            return true;
        }

        let src_aspect = src.aspect_mask();
        let dst_aspect = dst.aspect_mask();
        let bpp_in = crate::surface::bytes_per_block(src.base.info.format)
            / crate::surface::default_block_width(src.base.info.format);
        let bpp_out = crate::surface::bytes_per_block(dst.base.info.format)
            / crate::surface::default_block_width(dst.base.info.format);
        if bpp_in == 0 {
            return false;
        }

        let vk_in_copies = copies
            .iter()
            .map(|copy| {
                let mut adjusted = *copy;
                adjusted.src_offset.x = ((bpp_out as i32 * copy.src_offset.x) / bpp_in as i32);
                adjusted.extent.width = (bpp_out * copy.extent.width) / bpp_in;
                make_buffer_image_copy(&adjusted, true, src_aspect)
            })
            .collect::<Vec<_>>();
        let vk_out_copies = copies
            .iter()
            .map(|copy| make_buffer_image_copy(copy, false, dst_aspect))
            .collect::<Vec<_>>();

        let img_bpp = crate::surface::bytes_per_block(dst.base.info.format) as u64;
        let mut total_size = 0u64;
        for copy in copies {
            total_size = total_size.saturating_add(
                copy.extent.width as u64
                    * copy.extent.height as u64
                    * copy.extent.depth as u64
                    * img_bpp,
            );
        }
        let Some(copy_buffer) = self.get_temporary_buffer(total_size as usize) else {
            return false;
        };

        let dst_image = dst.handle();
        let src_image = src.handle();
        let device = self.device.clone();
        let scheduler = self.scheduler();
        scheduler.request_outside_renderpass();
        scheduler.record(move |cmd| unsafe {
            let mut dst_range = RangedBarrierRange::default();
            let mut src_range = RangedBarrierRange::default();
            for copy in &vk_in_copies {
                src_range.add_layers(copy.image_subresource);
            }
            for copy in &vk_out_copies {
                dst_range.add_layers(copy.image_subresource);
            }

            let read_barrier = vk::MemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::MEMORY_WRITE)
                .dst_access_mask(vk::AccessFlags::TRANSFER_READ | vk::AccessFlags::TRANSFER_WRITE)
                .build();
            let write_barrier = vk::MemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .dst_access_mask(vk::AccessFlags::MEMORY_READ | vk::AccessFlags::MEMORY_WRITE)
                .build();
            let pre_barriers = [vk::ImageMemoryBarrier::builder()
                .src_access_mask(
                    vk::AccessFlags::SHADER_WRITE
                        | vk::AccessFlags::COLOR_ATTACHMENT_WRITE
                        | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE
                        | vk::AccessFlags::TRANSFER_WRITE,
                )
                .dst_access_mask(vk::AccessFlags::TRANSFER_READ)
                .old_layout(vk::ImageLayout::GENERAL)
                .new_layout(vk::ImageLayout::TRANSFER_SRC_OPTIMAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(src_image)
                .subresource_range(src_range.subresource_range(src_aspect))
                .build()];
            let middle_in_barriers = [vk::ImageMemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::empty())
                .dst_access_mask(vk::AccessFlags::empty())
                .old_layout(vk::ImageLayout::TRANSFER_SRC_OPTIMAL)
                .new_layout(vk::ImageLayout::GENERAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(src_image)
                .subresource_range(src_range.subresource_range(src_aspect))
                .build()];
            let middle_out_barriers = [vk::ImageMemoryBarrier::builder()
                .src_access_mask(
                    vk::AccessFlags::SHADER_WRITE
                        | vk::AccessFlags::COLOR_ATTACHMENT_WRITE
                        | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE
                        | vk::AccessFlags::TRANSFER_WRITE,
                )
                .dst_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .old_layout(vk::ImageLayout::GENERAL)
                .new_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(dst_image)
                .subresource_range(dst_range.subresource_range(dst_aspect))
                .build()];
            let post_barriers = [vk::ImageMemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .dst_access_mask(
                    vk::AccessFlags::SHADER_READ
                        | vk::AccessFlags::SHADER_WRITE
                        | vk::AccessFlags::COLOR_ATTACHMENT_READ
                        | vk::AccessFlags::COLOR_ATTACHMENT_WRITE
                        | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_READ
                        | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE
                        | vk::AccessFlags::TRANSFER_READ
                        | vk::AccessFlags::TRANSFER_WRITE,
                )
                .old_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .new_layout(vk::ImageLayout::GENERAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(dst_image)
                .subresource_range(dst_range.subresource_range(dst_aspect))
                .build()];

            device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &pre_barriers,
            );
            device.cmd_copy_image_to_buffer(
                cmd,
                src_image,
                vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                copy_buffer,
                &vk_in_copies,
            );
            device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::DependencyFlags::empty(),
                &[write_barrier],
                &[],
                &middle_in_barriers,
            );
            device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[read_barrier],
                &[],
                &middle_out_barriers,
            );
            device.cmd_copy_buffer_to_image(
                cmd,
                copy_buffer,
                dst_image,
                vk::ImageLayout::GENERAL,
                &vk_out_copies,
            );
            device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &post_barriers,
            );
        });
        true
    }

    fn copy_image(&mut self, dst: &Image, src: &Image, copies: &[ImageCopy]) {
        if copies.is_empty() {
            return;
        }
        if trace_image_write(dst.base.gpu_addr) {
            log::info!(
                "[VK_IMAGE_WRITE] op=CopyImage dst_id={} dst_gpu=0x{:X} dst_cpu=0x{:X} dst_fmt={:?} src_id={} src_gpu=0x{:X} src_cpu=0x{:X} src_fmt={:?} copies={}",
                dst.image_id.index,
                dst.base.gpu_addr,
                dst.base.cpu_addr,
                dst.base.info.format,
                src.image_id.index,
                src.base.gpu_addr,
                src.base.cpu_addr,
                src.base.info.format,
                copies.len(),
            );
        }
        if std::env::var_os("RUZU_TRACE_VK_JOIN_COPY").is_some() {
            log::warn!(
                "[VK_JOIN_COPY] runtime.CopyImage dst={} src={} copies={} dst_fmt={:?} src_fmt={:?}",
                dst.image_id.index,
                src.image_id.index,
                copies.len(),
                dst.base.info.format,
                src.base.info.format
            );
        }
        let aspect = dst.aspect_mask();
        debug_assert_eq!(aspect, src.aspect_mask());
        let vk_copies = copies
            .iter()
            .map(|copy| make_image_copy(copy, aspect))
            .collect::<Vec<_>>();
        let dst_image = dst.handle();
        let src_image = src.handle();
        let device = self.device.clone();
        let scheduler = self.scheduler();
        scheduler.request_outside_renderpass();
        scheduler.record(move |cmd| unsafe {
            let barriers = make_copy_image_barriers(src_image, dst_image, aspect, &vk_copies);
            device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &barriers.pre,
            );
            device.cmd_copy_image(
                cmd,
                src_image,
                vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                dst_image,
                vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                &vk_copies,
            );
            device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &barriers.post,
            );
        });
    }

    fn blit_image(
        &mut self,
        dst_framebuffer: BlitFramebufferInfo,
        dst: &ImageView,
        src: &ImageView,
        dst_region: BlitRegion2D,
        src_region: BlitRegion2D,
        filter: BlitFilter,
        operation: BlitOperation,
    ) -> bool {
        if trace_image_write(dst.base.gpu_addr) {
            log::info!(
                "[VK_IMAGE_WRITE] op=BlitImage dst_view={} dst_image={} dst_gpu=0x{:X} dst_fmt={:?} src_view={} src_image={} src_gpu=0x{:X} src_fmt={:?} dst_region=({},{})->({},{}) src_region=({},{})->({},{}) filter={:?} operation={:?}",
                dst.view_id.index,
                dst.base.image_id.index,
                dst.base.gpu_addr,
                dst.base.format,
                src.view_id.index,
                src.base.image_id.index,
                src.base.gpu_addr,
                src.base.format,
                dst_region.start.x,
                dst_region.start.y,
                dst_region.end.x,
                dst_region.end.y,
                src_region.start.x,
                src_region.start.y,
                src_region.end.x,
                src_region.end.y,
                filter,
                operation,
            );
        }
        let aspect_mask = image_aspect_mask(src.base.format);
        if aspect_mask != image_aspect_mask(dst.base.format) {
            log::warn!(
                "TextureCacheRuntime::blit_image: incompatible blit from {:?} to {:?}",
                src.base.format,
                dst.base.format
            );
            return false;
        }

        let is_dst_msaa = dst.samples() != vk::SampleCountFlags::TYPE_1;
        let is_src_msaa = src.samples() != vk::SampleCountFlags::TYPE_1;
        if aspect_mask == vk::ImageAspectFlags::COLOR && !is_src_msaa && !is_dst_msaa {
            return self.blit_image_helper().blit_color(
                dst_framebuffer,
                src.handle(TextureType::Color2D),
                &dst_region,
                &src_region,
                filter,
                operation,
            );
        }

        if src.base.format != dst.base.format {
            log::warn!(
                "TextureCacheRuntime::blit_image: unsupported format reinterpretation from {:?} to {:?}",
                src.base.format,
                dst.base.format
            );
            return false;
        }

        if aspect_mask == (vk::ImageAspectFlags::DEPTH | vk::ImageAspectFlags::STENCIL)
            && !self.is_blit_depth_stencil_supported(src.base.format)
        {
            if is_src_msaa || is_dst_msaa {
                log::warn!("TextureCacheRuntime::blit_image: MSAA depth/stencil helper blit is not implemented");
                return false;
            }
            return self.blit_image_helper().blit_depth_stencil(
                dst_framebuffer,
                src.depth_view,
                src.stencil_view,
                &dst_region,
                &src_region,
                filter,
                operation,
            );
        }

        if is_dst_msaa && !is_src_msaa {
            log::warn!("TextureCacheRuntime::blit_image: non-MSAA to MSAA blit is unsupported");
            return false;
        }
        if operation != BlitOperation::SrcCopy {
            log::warn!(
                "TextureCacheRuntime::blit_image: unsupported operation {:?}",
                operation
            );
            return false;
        }

        let dst_image = dst.image_handle();
        let src_image = src.image_handle();
        let dst_layers = make_image_subresource_layers_from_view(dst);
        let src_layers = make_image_subresource_layers_from_view(src);
        let is_resolve = is_src_msaa && !is_dst_msaa;
        let device = self.device.clone();
        let scheduler = self.scheduler();
        scheduler.request_outside_renderpass();
        scheduler.record(move |cmd| unsafe {
            let full_range = vk::ImageSubresourceRange {
                aspect_mask,
                base_mip_level: 0,
                level_count: vk::REMAINING_MIP_LEVELS,
                base_array_layer: 0,
                layer_count: vk::REMAINING_ARRAY_LAYERS,
            };
            let read_barriers = [
                vk::ImageMemoryBarrier::builder()
                    .src_access_mask(
                        vk::AccessFlags::SHADER_WRITE
                            | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE
                            | vk::AccessFlags::TRANSFER_WRITE,
                    )
                    .dst_access_mask(vk::AccessFlags::TRANSFER_READ)
                    .old_layout(vk::ImageLayout::GENERAL)
                    .new_layout(vk::ImageLayout::GENERAL)
                    .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .image(src_image)
                    .subresource_range(full_range)
                    .build(),
                vk::ImageMemoryBarrier::builder()
                    .src_access_mask(
                        vk::AccessFlags::SHADER_WRITE
                            | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE
                            | vk::AccessFlags::TRANSFER_WRITE,
                    )
                    .dst_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                    .old_layout(vk::ImageLayout::GENERAL)
                    .new_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                    .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .image(dst_image)
                    .subresource_range(full_range)
                    .build(),
            ];
            let write_barrier = vk::ImageMemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .dst_access_mask(
                    vk::AccessFlags::SHADER_READ
                        | vk::AccessFlags::SHADER_WRITE
                        | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_READ
                        | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE
                        | vk::AccessFlags::TRANSFER_READ
                        | vk::AccessFlags::TRANSFER_WRITE,
                )
                .old_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .new_layout(vk::ImageLayout::GENERAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(dst_image)
                .subresource_range(full_range)
                .build();
            device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &read_barriers,
            );
            if is_resolve {
                device.cmd_resolve_image(
                    cmd,
                    src_image,
                    vk::ImageLayout::GENERAL,
                    dst_image,
                    vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                    &[make_image_resolve(
                        dst_region, src_region, dst_layers, src_layers,
                    )],
                );
            } else {
                let vk_filter = if filter == BlitFilter::Bilinear {
                    vk::Filter::LINEAR
                } else {
                    vk::Filter::NEAREST
                };
                device.cmd_blit_image(
                    cmd,
                    src_image,
                    vk::ImageLayout::GENERAL,
                    dst_image,
                    vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                    &[make_image_blit(
                        dst_region, src_region, dst_layers, src_layers,
                    )],
                    vk_filter,
                );
            }
            device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &[write_barrier],
            );
        });
        true
    }

    fn convert_image(
        &mut self,
        dst_framebuffer: BlitFramebufferInfo,
        dst_format: PixelFormat,
        src_format: PixelFormat,
        src_view: ConversionImageView,
    ) -> bool {
        match dst_format {
            PixelFormat::R16Unorm if src_format == PixelFormat::D16Unorm => self
                .blit_image_helper()
                .convert_d16_to_r16(dst_framebuffer, src_view),
            PixelFormat::A8B8G8R8Srgb if src_format == PixelFormat::D32Float => self
                .blit_image_helper()
                .convert_d32f_to_abgr8(dst_framebuffer, src_view),
            PixelFormat::A8B8G8R8Unorm if src_format == PixelFormat::S8UintD24Unorm => self
                .blit_image_helper()
                .convert_d24s8_to_abgr8(dst_framebuffer, src_view),
            PixelFormat::A8B8G8R8Unorm if src_format == PixelFormat::D24UnormS8Uint => self
                .blit_image_helper()
                .convert_s8d24_to_abgr8(dst_framebuffer, src_view),
            PixelFormat::A8B8G8R8Unorm if src_format == PixelFormat::D32Float => self
                .blit_image_helper()
                .convert_d32f_to_abgr8(dst_framebuffer, src_view),
            PixelFormat::B8G8R8A8Srgb if src_format == PixelFormat::D32Float => self
                .blit_image_helper()
                .convert_d32f_to_abgr8(dst_framebuffer, src_view),
            PixelFormat::B8G8R8A8Unorm if src_format == PixelFormat::D32Float => self
                .blit_image_helper()
                .convert_d32f_to_abgr8(dst_framebuffer, src_view),
            PixelFormat::R32Float if src_format == PixelFormat::D32Float => self
                .blit_image_helper()
                .convert_d32_to_r32(dst_framebuffer, src_view),
            PixelFormat::D16Unorm if src_format == PixelFormat::R16Unorm => self
                .blit_image_helper()
                .convert_r16_to_d16(dst_framebuffer, src_view),
            PixelFormat::S8UintD24Unorm
                if src_format == PixelFormat::A8B8G8R8Unorm
                    || src_format == PixelFormat::B8G8R8A8Unorm =>
            {
                self.blit_image_helper()
                    .convert_abgr8_to_d24s8(dst_framebuffer, src_view)
            }
            PixelFormat::D32Float
                if src_format == PixelFormat::A8B8G8R8Unorm
                    || src_format == PixelFormat::B8G8R8A8Unorm
                    || src_format == PixelFormat::A8B8G8R8Srgb
                    || src_format == PixelFormat::B8G8R8A8Srgb =>
            {
                self.blit_image_helper()
                    .convert_abgr8_to_d32f(dst_framebuffer, src_view)
            }
            PixelFormat::D32Float if src_format == PixelFormat::R32Float => self
                .blit_image_helper()
                .convert_r32_to_d32(dst_framebuffer, src_view),
            _ => {
                log::warn!(
                    "TextureCacheRuntime::convert_image: unimplemented format copy from {:?} to {:?}",
                    src_format,
                    dst_format
                );
                false
            }
        }
    }

    fn is_blit_depth_stencil_supported(&self, format: PixelFormat) -> bool {
        match format {
            PixelFormat::D24UnormS8Uint | PixelFormat::S8UintD24Unorm => self.is_format_supported(
                vk::Format::D24_UNORM_S8_UINT,
                vk::FormatFeatureFlags::BLIT_SRC | vk::FormatFeatureFlags::BLIT_DST,
                true,
            ),
            PixelFormat::D32FloatS8Uint => self.is_format_supported(
                vk::Format::D32_SFLOAT_S8_UINT,
                vk::FormatFeatureFlags::BLIT_SRC | vk::FormatFeatureFlags::BLIT_DST,
                true,
            ),
            _ => true,
        }
    }

    fn has_msaa_copy_pass(&self) -> bool {
        self.msaa_copy_pass.is_some()
    }

    fn is_format_supported(
        &self,
        format: vk::Format,
        usage: vk::FormatFeatureFlags,
        optimal: bool,
    ) -> bool {
        // `vkGetPhysicalDeviceFormatProperties` is a driver call and this
        // runs several times per draw through `surface_format`. Upstream
        // caches format properties in `Device`; do the same here.
        let props = *self
            .format_properties
            .borrow_mut()
            .entry(format)
            .or_insert_with(|| unsafe {
                self.instance
                    .get_physical_device_format_properties(self.physical_device, format)
            });
        let supported = if optimal {
            props.optimal_tiling_features
        } else {
            props.linear_tiling_features
        };
        supported.contains(usage)
    }

    /// Port of `MaxwellToVK::SurfaceFormat(device, FormatType::Optimal, ...)`.
    ///
    /// The static table gives the guest's preferred Vulkan format, but the
    /// actual image/view format must be selected through the device's supported
    /// alternatives. This matters on MoltenVK where D24S8 is not natively
    /// supported and must resolve to D32S8 before image/view creation.
    fn surface_format_info(&self, format: PixelFormat) -> maxwell_to_vk::FormatInfo {
        let mut format_info = maxwell_to_vk::surface_format(format);
        if crate::surface::is_pixel_format_bcn(format) && !self.optimal_bcn_supported {
            format_info.format = bcn_transcoded_format(format);
        }
        let mut usage = vk::FormatFeatureFlags::SAMPLED_IMAGE
            | vk::FormatFeatureFlags::TRANSFER_DST
            | vk::FormatFeatureFlags::TRANSFER_SRC;
        if format_info.attachable {
            usage |= match crate::surface::get_format_type(format) {
                SurfaceType::ColorTexture => vk::FormatFeatureFlags::COLOR_ATTACHMENT,
                SurfaceType::Depth | SurfaceType::Stencil | SurfaceType::DepthStencil => {
                    vk::FormatFeatureFlags::DEPTH_STENCIL_ATTACHMENT
                }
                SurfaceType::Invalid => vk::FormatFeatureFlags::empty(),
            };
        }
        if format_info.storage {
            usage |= vk::FormatFeatureFlags::STORAGE_IMAGE;
        }
        format_info.format = self.supported_surface_format(format_info.format, usage, true);
        format_info
    }

    fn surface_format(&self, format: PixelFormat) -> vk::Format {
        self.surface_format_info(format).format
    }

    fn supported_surface_format(
        &self,
        wanted_format: vk::Format,
        wanted_usage: vk::FormatFeatureFlags,
        optimal: bool,
    ) -> vk::Format {
        if self.is_format_supported(wanted_format, wanted_usage, optimal) {
            return wanted_format;
        }
        for &format in surface_format_alternatives(wanted_format) {
            if self.is_format_supported(format, wanted_usage, optimal) {
                return format;
            }
        }
        wanted_format
    }

    fn needs_scale_helper(&self, _info: &ImageInfo, format: vk::Format) -> bool {
        // Upstream also checks `device.CantBlitMSAA()` here. The Rust runtime
        // currently owns only ash device handles, not the full Vulkan Device
        // wrapper where that driver quirk is tracked, so this slice preserves
        // the format-feature gate and documents the remaining MSAA-device gap
        // in DIFF.md.
        let blit_usage = vk::FormatFeatureFlags::BLIT_SRC | vk::FormatFeatureFlags::BLIT_DST;
        !self.is_format_supported(format, blit_usage, true)
    }

    fn storage_image_view(
        &self,
        image: &mut Image,
        level: u32,
    ) -> Result<vk::ImageView, vk::Result> {
        self.storage_image_view_with_format(image, level, image.format)
    }

    fn storage_image_view_with_format(
        &self,
        image: &mut Image,
        level: u32,
        format: vk::Format,
    ) -> Result<vk::ImageView, vk::Result> {
        let index = level as usize;
        if index >= image.storage_image_views.len() {
            return Err(vk::Result::ERROR_INITIALIZATION_FAILED);
        }
        if image.storage_image_views[index] != vk::ImageView::null() {
            return Ok(image.storage_image_views[index]);
        }

        let mut usage_info = vk::ImageViewUsageCreateInfo::builder()
            .usage(vk::ImageUsageFlags::STORAGE)
            .build();
        let view_info = vk::ImageViewCreateInfo::builder()
            .push_next(&mut usage_info)
            .image(image.handle())
            .view_type(vk::ImageViewType::TYPE_2D_ARRAY)
            .format(format)
            .components(vk::ComponentMapping {
                r: vk::ComponentSwizzle::IDENTITY,
                g: vk::ComponentSwizzle::IDENTITY,
                b: vk::ComponentSwizzle::IDENTITY,
                a: vk::ComponentSwizzle::IDENTITY,
            })
            .subresource_range(vk::ImageSubresourceRange {
                aspect_mask: vk::ImageAspectFlags::COLOR,
                base_mip_level: level,
                level_count: 1,
                base_array_layer: 0,
                layer_count: vk::REMAINING_ARRAY_LAYERS,
            })
            .build();
        let view = unsafe { self.device.create_image_view(&view_info, None)? };
        image.storage_image_views[index] = view;
        Ok(view)
    }

    fn copy_image_msaa(&mut self, dst: &mut Image, src: &mut Image, copies: &[ImageCopy]) -> bool {
        if copies.is_empty() {
            return true;
        }
        let Some(pass) = self.msaa_copy_pass.as_ref() else {
            log::warn!(
                "TextureCacheRuntime: CopyImageMSAA requested but MSAACopyPass is unavailable"
            );
            return false;
        };
        let msaa_to_non_msaa = src.base.info.num_samples > 1 && dst.base.info.num_samples == 1;
        let pipeline = pass.pipeline(msaa_to_non_msaa);
        let layout = pass.layout();
        let descriptor_set_layout = pass.descriptor_set_layout();

        let device = self.device.clone();
        let mut dispatches = Vec::with_capacity(copies.len());
        for copy in copies {
            if copy.src_subresource.base_layer != 0
                || copy.src_subresource.num_layers != 1
                || copy.dst_subresource.base_layer != 0
                || copy.dst_subresource.num_layers != 1
            {
                log::warn!("TextureCacheRuntime: CopyImageMSAA only supports single-layer copies");
                return false;
            }
            let src_view =
                match self.storage_image_view(src, copy.src_subresource.base_level as u32) {
                    Ok(view) => view,
                    Err(err) => {
                        log::warn!(
                            "TextureCacheRuntime: failed to create MSAA source storage view: {:?}",
                            err
                        );
                        return false;
                    }
                };
            let dst_view =
                match self.storage_image_view(dst, copy.dst_subresource.base_level as u32) {
                    Ok(view) => view,
                    Err(err) => {
                        log::warn!(
                        "TextureCacheRuntime: failed to create MSAA destination storage view: {:?}",
                        err
                    );
                        return false;
                    }
                };

            unsafe {
                self.compute_pass_descriptor_queue.as_mut().acquire();
                self.compute_pass_descriptor_queue
                    .as_mut()
                    .add_image(src_view);
                self.compute_pass_descriptor_queue
                    .as_mut()
                    .add_image(dst_view);
            }
            let descriptor_set = match unsafe {
                self.descriptor_pool.as_ref().allocate(
                    descriptor_set_layout,
                    &PoolDescriptorBankInfo {
                        uniform_buffers: 0,
                        storage_buffers: 0,
                        texture_buffers: 0,
                        image_buffers: 0,
                        textures: 0,
                        images: 2,
                        score: 2,
                    },
                )
            } {
                Ok(set) => set,
                Err(err) => {
                    log::warn!(
                        "TextureCacheRuntime: failed to allocate MSAA descriptor set: {:?}",
                        err
                    );
                    return false;
                }
            };
            let descriptor_images = [
                vk::DescriptorImageInfo {
                    sampler: vk::Sampler::null(),
                    image_view: src_view,
                    image_layout: vk::ImageLayout::GENERAL,
                },
                vk::DescriptorImageInfo {
                    sampler: vk::Sampler::null(),
                    image_view: dst_view,
                    image_layout: vk::ImageLayout::GENERAL,
                },
            ];
            let writes = [vk::WriteDescriptorSet::builder()
                .dst_set(descriptor_set)
                .dst_binding(0)
                .descriptor_type(vk::DescriptorType::STORAGE_IMAGE)
                .image_info(&descriptor_images)
                .build()];
            unsafe {
                device.update_descriptor_sets(&writes, &[]);
            }
            dispatches.push((
                descriptor_set,
                dst.handle(),
                (copy.extent.width + 7) / 8,
                (copy.extent.height + 7) / 8,
                copy.extent.depth,
            ));
        }

        let scheduler = self.scheduler();
        scheduler.request_outside_renderpass();
        scheduler.record(move |cmd| unsafe {
            for (descriptor_set, dst_image, x, y, z) in dispatches {
                device.cmd_bind_pipeline(cmd, vk::PipelineBindPoint::COMPUTE, pipeline);
                device.cmd_bind_descriptor_sets(
                    cmd,
                    vk::PipelineBindPoint::COMPUTE,
                    layout,
                    0,
                    &[descriptor_set],
                    &[],
                );
                device.cmd_dispatch(cmd, x, y, z);
                let write_barrier = vk::ImageMemoryBarrier::builder()
                    .src_access_mask(vk::AccessFlags::SHADER_WRITE)
                    .dst_access_mask(vk::AccessFlags::SHADER_READ)
                    .old_layout(vk::ImageLayout::GENERAL)
                    .new_layout(vk::ImageLayout::GENERAL)
                    .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .image(dst_image)
                    .subresource_range(vk::ImageSubresourceRange {
                        aspect_mask: vk::ImageAspectFlags::COLOR,
                        base_mip_level: 0,
                        level_count: vk::REMAINING_MIP_LEVELS,
                        base_array_layer: 0,
                        layer_count: vk::REMAINING_ARRAY_LAYERS,
                    })
                    .build();
                device.cmd_pipeline_barrier(
                    cmd,
                    vk::PipelineStageFlags::COMPUTE_SHADER,
                    vk::PipelineStageFlags::FRAGMENT_SHADER,
                    vk::DependencyFlags::empty(),
                    &[],
                    &[],
                    &[write_barrier],
                );
            }
        });
        true
    }

    fn create_scaled_image(
        &mut self,
        info: &ImageInfo,
        format: vk::Format,
    ) -> Result<AllocatedImage, vk::Result> {
        let is_2d = info.image_type == ImageType::E2D;
        let mut scaled_info = info.clone();
        scaled_info.size.width = self.resolution.scale_up_u32(info.size.width);
        if is_2d {
            scaled_info.size.height = self.resolution.scale_up_u32(info.size.height);
        }
        self.create_image_from_info(&scaled_info, format)
    }

    fn blit_scale(
        &mut self,
        src_image: vk::Image,
        dst_image: vk::Image,
        info: ImageInfo,
        aspect_mask: vk::ImageAspectFlags,
        up_scaling: bool,
    ) {
        let is_2d = info.image_type == ImageType::E2D;
        let resources = info.resources;
        let extent = vk::Extent2D {
            width: info.size.width,
            height: info.size.height,
        };
        let is_color = aspect_mask == vk::ImageAspectFlags::COLOR;
        let is_bilinear = is_color && !crate::surface::is_pixel_format_integer(info.format);
        let vk_filter = if is_bilinear {
            vk::Filter::LINEAR
        } else {
            vk::Filter::NEAREST
        };
        let resolution = self.resolution.clone();
        let device = self.device.clone();
        let scheduler = self.scheduler();
        scheduler.request_outside_renderpass();
        scheduler.record(move |cmd| unsafe {
            let src_size = vk::Offset2D {
                x: if up_scaling {
                    extent.width as i32
                } else {
                    resolution.scale_up_i32(extent.width as i32)
                },
                y: if is_2d && up_scaling {
                    extent.height as i32
                } else {
                    resolution.scale_up_i32(extent.height as i32)
                },
            };
            let dst_size = vk::Offset2D {
                x: if up_scaling {
                    resolution.scale_up_i32(extent.width as i32)
                } else {
                    extent.width as i32
                },
                y: if is_2d && up_scaling {
                    resolution.scale_up_i32(extent.height as i32)
                } else {
                    extent.height as i32
                },
            };
            let mut regions = Vec::with_capacity(resources.levels.max(1) as usize);
            for level in 0..resources.levels.max(1) {
                regions.push(vk::ImageBlit {
                    src_subresource: vk::ImageSubresourceLayers {
                        aspect_mask,
                        mip_level: level as u32,
                        base_array_layer: 0,
                        layer_count: resources.layers.max(1) as u32,
                    },
                    src_offsets: [
                        vk::Offset3D { x: 0, y: 0, z: 0 },
                        vk::Offset3D {
                            x: (src_size.x >> level).max(1),
                            y: (src_size.y >> level).max(1),
                            z: 1,
                        },
                    ],
                    dst_subresource: vk::ImageSubresourceLayers {
                        aspect_mask,
                        mip_level: level as u32,
                        base_array_layer: 0,
                        layer_count: resources.layers.max(1) as u32,
                    },
                    dst_offsets: [
                        vk::Offset3D { x: 0, y: 0, z: 0 },
                        vk::Offset3D {
                            x: (dst_size.x >> level).max(1),
                            y: (dst_size.y >> level).max(1),
                            z: 1,
                        },
                    ],
                });
            }
            let subresource_range = vk::ImageSubresourceRange {
                aspect_mask,
                base_mip_level: 0,
                level_count: vk::REMAINING_MIP_LEVELS,
                base_array_layer: 0,
                layer_count: vk::REMAINING_ARRAY_LAYERS,
            };
            let read_barriers = [
                vk::ImageMemoryBarrier::builder()
                    .src_access_mask(vk::AccessFlags::MEMORY_WRITE)
                    .dst_access_mask(vk::AccessFlags::TRANSFER_READ)
                    .old_layout(vk::ImageLayout::GENERAL)
                    .new_layout(vk::ImageLayout::TRANSFER_SRC_OPTIMAL)
                    .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .image(src_image)
                    .subresource_range(subresource_range)
                    .build(),
                vk::ImageMemoryBarrier::builder()
                    .src_access_mask(
                        vk::AccessFlags::SHADER_WRITE
                            | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE
                            | vk::AccessFlags::TRANSFER_WRITE,
                    )
                    .dst_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                    .old_layout(vk::ImageLayout::UNDEFINED)
                    .new_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                    .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .image(dst_image)
                    .subresource_range(subresource_range)
                    .build(),
            ];
            let write_barriers = [
                vk::ImageMemoryBarrier::builder()
                    .src_access_mask(vk::AccessFlags::empty())
                    .dst_access_mask(vk::AccessFlags::MEMORY_WRITE | vk::AccessFlags::MEMORY_READ)
                    .old_layout(vk::ImageLayout::TRANSFER_SRC_OPTIMAL)
                    .new_layout(vk::ImageLayout::GENERAL)
                    .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .image(src_image)
                    .subresource_range(subresource_range)
                    .build(),
                vk::ImageMemoryBarrier::builder()
                    .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                    .dst_access_mask(vk::AccessFlags::MEMORY_WRITE | vk::AccessFlags::MEMORY_READ)
                    .old_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                    .new_layout(vk::ImageLayout::GENERAL)
                    .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                    .image(dst_image)
                    .subresource_range(subresource_range)
                    .build(),
            ];
            device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &read_barriers,
            );
            device.cmd_blit_image(
                cmd,
                src_image,
                vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                dst_image,
                vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                &regions,
                vk_filter,
            );
            device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &write_barriers,
            );
        });
    }

    fn create_image_from_info(
        &mut self,
        info: &ImageInfo,
        format: vk::Format,
    ) -> Result<AllocatedImage, vk::Result> {
        let mut format_info = self.surface_format_info(info.format);
        format_info.format = format;
        let image_info = make_image_create_info(info, format_info);
        self.memory_allocator()
            .create_owned_image(&image_info)
            .map_err(|err| err.result)
    }

    fn make_image_view(
        &self,
        view_id: ImageViewId,
        view_base: &ImageViewBase,
        image: &Image,
    ) -> Result<ImageView, vk::Result> {
        let format_info = self.surface_format_info(view_base.format);
        let format = format_info.format;
        let aspect_mask = image_view_aspect_mask(view_base);
        let components = image_view_components(view_base);
        let base_range = make_subresource_range(aspect_mask, view_base.range, view_base.flags);
        let usage = image_usage_flags(format_info, view_base.format);
        let mut image_views =
            [vk::ImageView::null(); shader_recompiler::shader_info::NUM_TEXTURE_TYPES as usize];

        let create = |texture_type: TextureType,
                      layer_count: Option<u32>|
         -> Result<vk::ImageView, vk::Result> {
            let mut range = base_range;
            if let Some(layer_count) = layer_count {
                range.layer_count = layer_count;
            }
            let mut usage_info = vk::ImageViewUsageCreateInfo::builder().usage(usage).build();
            let view_info = vk::ImageViewCreateInfo::builder()
                .push_next(&mut usage_info)
                .image(image.handle())
                .view_type(image_view_type_from_texture_type(texture_type))
                .format(format)
                .components(components)
                .subresource_range(range)
                .build();
            unsafe { self.device.create_image_view(&view_info, None) }
        };

        let render_target = match view_base.view_type {
            crate::texture_cache::types::ImageViewType::E1D
            | crate::texture_cache::types::ImageViewType::E1DArray => {
                image_views[TextureType::Color1D as usize] = create(TextureType::Color1D, Some(1))?;
                image_views[TextureType::ColorArray1D as usize] =
                    create(TextureType::ColorArray1D, None)?;
                image_views[TextureType::ColorArray1D as usize]
            }
            crate::texture_cache::types::ImageViewType::E2D
            | crate::texture_cache::types::ImageViewType::E2DArray
            | crate::texture_cache::types::ImageViewType::Rect => {
                image_views[TextureType::Color2D as usize] = create(TextureType::Color2D, Some(1))?;
                image_views[TextureType::Color2DRect as usize] =
                    image_views[TextureType::Color2D as usize];
                image_views[TextureType::ColorArray2D as usize] =
                    create(TextureType::ColorArray2D, None)?;
                image_views[TextureType::ColorArray2D as usize]
            }
            crate::texture_cache::types::ImageViewType::E3D => {
                image_views[TextureType::Color3D as usize] = create(TextureType::Color3D, None)?;
                image_views[TextureType::Color3D as usize]
            }
            crate::texture_cache::types::ImageViewType::Cube
            | crate::texture_cache::types::ImageViewType::CubeArray => {
                image_views[TextureType::ColorCube as usize] =
                    create(TextureType::ColorCube, Some(6))?;
                image_views[TextureType::ColorArrayCube as usize] =
                    create(TextureType::ColorArrayCube, None)?;
                image_views[TextureType::ColorArrayCube as usize]
            }
            crate::texture_cache::types::ImageViewType::Buffer => vk::ImageView::null(),
        };

        Ok(ImageView {
            view_id,
            base: view_base.clone(),
            image_handle: image.handle(),
            image_views,
            render_target,
            depth_view: vk::ImageView::null(),
            stencil_view: vk::ImageView::null(),
            color_view: vk::ImageView::null(),
            storage_signeds: [vk::ImageView::null();
                shader_recompiler::shader_info::NUM_TEXTURE_TYPES as usize],
            storage_unsigneds: [vk::ImageView::null();
                shader_recompiler::shader_info::NUM_TEXTURE_TYPES as usize],
            samples: convert_sample_count(image.base.info.num_samples),
            buffer_size: image.base.guest_size_bytes,
        })
    }

    fn make_aux_image_view(
        &self,
        image: vk::Image,
        view_base: &ImageViewBase,
        format: vk::Format,
        aspect_mask: vk::ImageAspectFlags,
    ) -> Result<vk::ImageView, vk::Result> {
        let view_info = vk::ImageViewCreateInfo::builder()
            .image(image)
            .view_type(image_view_type_from_view_type(view_base.view_type))
            .format(format)
            .components(vk::ComponentMapping {
                r: vk::ComponentSwizzle::IDENTITY,
                g: vk::ComponentSwizzle::IDENTITY,
                b: vk::ComponentSwizzle::IDENTITY,
                a: vk::ComponentSwizzle::IDENTITY,
            })
            .subresource_range(make_subresource_range(
                aspect_mask,
                view_base.range,
                view_base.flags,
            ))
            .build();
        unsafe { self.device.create_image_view(&view_info, None) }
    }

    fn create_framebuffer(
        &self,
        render_pass: vk::RenderPass,
        attachments: &[vk::ImageView],
        extent: vk::Extent2D,
    ) -> Result<vk::Framebuffer, vk::Result> {
        let fb_info = vk::FramebufferCreateInfo::builder()
            .render_pass(render_pass)
            .attachments(attachments)
            .width(extent.width)
            .height(extent.height)
            .layers(1)
            .build();
        unsafe { self.device.create_framebuffer(&fb_info, None) }
    }

    fn create_blit_color_view(
        &self,
        image: vk::Image,
        format: vk::Format,
    ) -> Result<vk::ImageView, vk::Result> {
        self.create_blit_image_view(image, format, vk::ImageAspectFlags::COLOR)
    }

    fn create_blit_image_view(
        &self,
        image: vk::Image,
        format: vk::Format,
        aspect_mask: vk::ImageAspectFlags,
    ) -> Result<vk::ImageView, vk::Result> {
        let view_info = vk::ImageViewCreateInfo::builder()
            .image(image)
            .view_type(vk::ImageViewType::TYPE_2D)
            .format(format)
            .components(vk::ComponentMapping {
                r: vk::ComponentSwizzle::IDENTITY,
                g: vk::ComponentSwizzle::IDENTITY,
                b: vk::ComponentSwizzle::IDENTITY,
                a: vk::ComponentSwizzle::IDENTITY,
            })
            .subresource_range(vk::ImageSubresourceRange {
                aspect_mask,
                base_mip_level: 0,
                level_count: 1,
                base_array_layer: 0,
                layer_count: 1,
            })
            .build();
        unsafe { self.device.create_image_view(&view_info, None) }
    }

    fn create_blit_color_framebuffer(
        &mut self,
        image: vk::Image,
        view: vk::ImageView,
        format: vk::Format,
        extent: vk::Extent2D,
    ) -> Result<BlitFramebufferInfo, vk::Result> {
        let mut rp_key = RenderPassKey::default();
        rp_key.color_formats[0] = format;
        rp_key.num_color_attachments = 1;
        rp_key.samples = vk::SampleCountFlags::TYPE_1;
        let render_pass = self.render_pass_cache().get(&rp_key)?;
        let framebuffer = self.create_framebuffer(render_pass, &[view], extent)?;
        let mut images = [vk::Image::null(); NUM_RT + 1];
        images[0] = image;
        let mut image_ranges = [vk::ImageSubresourceRange::default(); NUM_RT + 1];
        image_ranges[0] = vk::ImageSubresourceRange {
            aspect_mask: vk::ImageAspectFlags::COLOR,
            base_mip_level: 0,
            level_count: 1,
            base_array_layer: 0,
            layer_count: 1,
        };
        Ok(BlitFramebufferInfo {
            framebuffer,
            render_pass,
            render_area: extent,
            images,
            image_ranges,
            num_images: 1,
        })
    }

    fn create_blit_depth_stencil_framebuffer(
        &mut self,
        image: vk::Image,
        view: vk::ImageView,
        format: vk::Format,
        extent: vk::Extent2D,
    ) -> Result<BlitFramebufferInfo, vk::Result> {
        let rp_key = RenderPassKey {
            depth_format: format,
            samples: vk::SampleCountFlags::TYPE_1,
            ..RenderPassKey::default()
        };
        let render_pass = self.render_pass_cache().get(&rp_key)?;
        let framebuffer = self.create_framebuffer(render_pass, &[view], extent)?;
        let mut images = [vk::Image::null(); NUM_RT + 1];
        images[0] = image;
        let mut image_ranges = [vk::ImageSubresourceRange::default(); NUM_RT + 1];
        image_ranges[0] = vk::ImageSubresourceRange {
            aspect_mask: vk::ImageAspectFlags::DEPTH | vk::ImageAspectFlags::STENCIL,
            base_mip_level: 0,
            level_count: 1,
            base_array_layer: 0,
            layer_count: 1,
        };
        Ok(BlitFramebufferInfo {
            framebuffer,
            render_pass,
            render_area: extent,
            images,
            image_ranges,
            num_images: 1,
        })
    }

    fn destroy_image(&self, mut image: Image) {
        unsafe {
            for view in image.storage_image_views.drain(..) {
                if view != vk::ImageView::null() {
                    self.device.destroy_image_view(view, None);
                }
            }
            for view in [
                image.scale_view,
                image.normal_view,
                image.scale_depth_view,
                image.normal_depth_view,
                image.scale_stencil_view,
                image.normal_stencil_view,
            ] {
                if view != vk::ImageView::null() {
                    self.device.destroy_image_view(view, None);
                }
            }
            for framebuffer in [image.scale_framebuffer, image.normal_framebuffer]
                .into_iter()
                .flatten()
            {
                self.device
                    .destroy_framebuffer(framebuffer.framebuffer, None);
            }
        }
    }

    fn destroy_image_view(&self, view: ImageView) {
        let mut destroyed = Vec::new();
        let mut destroy_once = |handle: vk::ImageView| {
            if handle == vk::ImageView::null() || destroyed.contains(&handle) {
                return;
            }
            destroyed.push(handle);
            unsafe {
                self.device.destroy_image_view(handle, None);
            }
        };
        for handle in view.image_views {
            destroy_once(handle);
        }
        for handle in view.storage_signeds {
            destroy_once(handle);
        }
        for handle in view.storage_unsigneds {
            destroy_once(handle);
        }
        for handle in [view.depth_view, view.stencil_view, view.color_view] {
            destroy_once(handle);
        }
    }

    fn destroy_framebuffer(&self, framebuffer: vk::Framebuffer) {
        unsafe {
            self.device.destroy_framebuffer(framebuffer, None);
        }
    }

    fn destroy_sampler(&self, sampler: CachedSampler) {
        unsafe {
            self.device.destroy_sampler(sampler.sampler, None);
        }
    }

    fn sentence_framebuffer(&mut self, framebuffer: vk::Framebuffer) {
        if framebuffer == vk::Framebuffer::null() {
            return;
        }
        self.sentence_resource(DeferredVkResource::Framebuffer(framebuffer));
    }

    fn sentence_image_view(&mut self, view: ImageView) {
        self.sentence_resource(DeferredVkResource::ImageView(view));
    }

    fn sentence_image(&mut self, image: Image) {
        self.sentence_resource(DeferredVkResource::Image(image));
    }

    fn sentence_resource(&mut self, resource: DeferredVkResource) {
        // The last submission that can reference the resource is the pending
        // tick (the flush that will carry the currently recorded chunk).
        // Retire once the GPU (timeline counter) passes it — the submission
        // counter itself runs ahead of the GPU with pipelined submits.
        let retire_tick = self.scheduler().pending_tick();
        self.sentenced_resources.push(SentencedVkResource {
            retire_tick,
            resource,
        });
    }

    /// `gpu_tick` is `Scheduler::known_gpu_tick()` — the last tick the GPU
    /// has fully completed.
    fn tick_frame(&mut self, gpu_tick: u64) {
        let scheduler_tick = gpu_tick;
        self.current_tick = scheduler_tick;
        let mut retained = Vec::with_capacity(self.sentenced_resources.len());
        let mut ready = Vec::new();
        for sentenced in self.sentenced_resources.drain(..) {
            if sentenced.retire_tick <= scheduler_tick {
                ready.push(sentenced.resource);
            } else {
                retained.push(sentenced);
            }
        }
        self.sentenced_resources = retained;
        for resource in ready {
            match resource {
                DeferredVkResource::Framebuffer(framebuffer) => {
                    self.destroy_framebuffer(framebuffer)
                }
                DeferredVkResource::ImageView(view) => self.destroy_image_view(view),
                DeferredVkResource::Image(image) => self.destroy_image(image),
            }
        }
    }
}

impl Drop for TextureCacheRuntime {
    fn drop(&mut self) {
        let resources = self
            .sentenced_resources
            .drain(..)
            .map(|sentenced| sentenced.resource)
            .collect::<Vec<_>>();
        for resource in resources {
            match resource {
                DeferredVkResource::Framebuffer(framebuffer) => {
                    self.destroy_framebuffer(framebuffer)
                }
                DeferredVkResource::ImageView(view) => self.destroy_image_view(view),
                DeferredVkResource::Image(image) => self.destroy_image(image),
            }
        }
    }
}

/// Manages GPU textures (images, views, samplers, framebuffers).
///
/// Ref: zuyu TextureCacheRuntime — caches textures by TIC index and
/// framebuffers by render target configuration.
pub struct TextureCache {
    pub base: CommonTextureCache,
    runtime: TextureCacheRuntime,
    channel_caches: ChannelSetupCaches<ChannelInfo>,

    /// Cached framebuffers by render target config key.
    framebuffers: HashMap<FramebufferKey, CachedFramebuffer>,

    /// Backend Vulkan images keyed by common-cache `ImageId`, matching
    /// upstream `slot_images` ownership.
    images: HashMap<ImageId, Image>,

    /// Compatibility index for frontend/present paths that still arrive with a
    /// CPU address. This is not the owning identity.
    render_target_cpu_map: HashMap<u64, ImageId>,

    /// Assembled framebuffers keyed by the bound render-target set.
    framebuffers_by_render_targets: HashMap<RenderTargets, Framebuffer>,

    /// Backend Vulkan image views keyed by common-cache `ImageViewId`.
    image_views: HashMap<ImageViewId, ImageView>,

    /// Vulkan samplers materialized from common-cache `SamplerId`s.
    samplers: HashMap<SamplerId, CachedSampler>,

    /// Deferred staging buffers used by upstream `CommitAsyncFlushes` /
    /// `PopAsyncFlushes` for preemptive texture downloads.
    uncommitted_async_buffers: Vec<StagingBuffer>,
    async_buffers: VecDeque<Vec<StagingBuffer>>,
    async_buffers_death_ring: Vec<StagingBuffer>,

    texture_decode_worker: ThreadWorker,
    present_source_dumped: bool,
    present_source_seen: u64,
    image_dumped: bool,
    image_dump_seen: u64,
}

impl TextureCache {
    pub fn new(
        device: ash::Device,
        instance: ash::Instance,
        physical_device: vk::PhysicalDevice,
        device_memory: Arc<MaxwellDeviceMemoryManager>,
        scheduler: &mut Scheduler,
        memory_allocator: &mut MemoryAllocator,
        staging_buffer_pool: &mut StagingBufferPool,
        blit_image_helper: &mut BlitImageHelper,
        render_pass_cache: &mut RenderPassCache,
        descriptor_pool: &mut DescriptorPool,
        compute_pass_descriptor_queue: &mut ComputePassDescriptorQueue,
        custom_border_color_supported: bool,
    ) -> Self {
        let mut base = CommonTextureCache::new(device_memory);
        base.set_backend_completes_join_images(true);
        let runtime = TextureCacheRuntime::new(
            device,
            instance,
            physical_device,
            scheduler,
            memory_allocator,
            staging_buffer_pool,
            blit_image_helper,
            render_pass_cache,
            descriptor_pool,
            compute_pass_descriptor_queue,
            custom_border_color_supported,
        );
        base.configure_device_memory_budget(runtime.get_device_local_memory());
        Self {
            base,
            runtime,
            channel_caches: ChannelSetupCaches::new(),
            framebuffers: HashMap::new(),
            images: HashMap::new(),
            render_target_cpu_map: HashMap::new(),
            framebuffers_by_render_targets: HashMap::new(),
            image_views: HashMap::new(),
            samplers: HashMap::new(),
            uncommitted_async_buffers: Vec::new(),
            async_buffers: VecDeque::new(),
            async_buffers_death_ring: Vec::new(),
            texture_decode_worker: ThreadWorker::new_named(1, "TextureDecoder"),
            present_source_dumped: false,
            present_source_seen: 0,
            image_dumped: false,
            image_dump_seen: 0,
        }
    }

    pub fn set_guest_memory_writer(&mut self, writer: crate::renderer_base::GuestMemoryWriter) {
        self.base.set_guest_memory_writer(writer);
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

    /// Vulkan-backed port of upstream `TextureCache<P>::DownloadMemory`.
    pub fn download_memory(&mut self, cpu_addr: u64, size: usize) {
        if cpu_addr == 0 || size == 0 {
            return;
        }
        if self.base.channel_gpu_memory.is_none() && self.base.guest_memory_writer.is_none() {
            return;
        }

        let mut images = self.base.collect_images_in_region(cpu_addr, size);
        images.retain(|&image_id| self.base.slot_images[image_id].is_safe_download());
        if images.is_empty() {
            return;
        }
        for &image_id in &images {
            self.base.slot_images[image_id]
                .flags
                .remove(ImageFlagBits::GPU_MODIFIED);
        }
        images.sort_by_key(|&image_id| self.base.slot_images[image_id].modification_tick);

        for image_id in images {
            let Some((image_base, staging)) = self.download_image_to_host_staging(image_id) else {
                continue;
            };
            let copies = full_download_copies(&image_base.info);
            let _ = self
                .base
                .write_downloaded_image(&image_base, &copies, &staging);
        }
    }

    pub fn should_wait_async_flushes(&self) -> bool {
        self.base.should_wait_async_flushes()
    }

    pub fn has_uncommitted_flushes(&self) -> bool {
        self.base.has_uncommitted_flushes()
    }

    pub fn commit_async_flushes(&mut self) {
        let mut download_ids = std::mem::take(&mut self.base.uncommitted_downloads);
        if download_ids.is_empty() {
            self.base.committed_downloads.push_back(download_ids);
            self.async_buffers
                .push_back(std::mem::take(&mut self.uncommitted_async_buffers));
            return;
        }

        let mut total_size_bytes = 0usize;
        let last_async_buffer_id = self.uncommitted_async_buffers.len();
        let mut any_swizzle = false;
        for download_info in &mut download_ids {
            if download_info.is_swizzle {
                total_size_bytes = total_size_bytes.saturating_add(common::alignment::align_up(
                    self.base.slot_images[download_info.object_id].unswizzled_size_bytes as u64,
                    64,
                ) as usize);
                any_swizzle = true;
                download_info.async_buffer_id = last_async_buffer_id;
            }
        }

        if any_swizzle && total_size_bytes != 0 {
            let Some(mut download_map) = self
                .runtime
                .download_staging_buffer(total_size_bytes as vk::DeviceSize, true)
            else {
                self.base.committed_downloads.push_back(download_ids);
                self.async_buffers
                    .push_back(std::mem::take(&mut self.uncommitted_async_buffers));
                return;
            };
            for download_info in &download_ids {
                if !download_info.is_swizzle {
                    continue;
                }
                let image_id = download_info.object_id;
                let image_base = self.base.slot_images[image_id].clone();
                let format = self.runtime.surface_format(image_base.info.format);
                let aspect = image_aspect_mask(image_base.info.format);
                if aspect.is_empty()
                    || self
                        .ensure_image(image_id, &image_base, format, aspect)
                        .is_err()
                {
                    continue;
                }
                let copies = full_download_copies(&image_base.info);
                let Some(mut image) = self.images.remove(&image_id) else {
                    continue;
                };
                image.base = image_base.clone();
                let _ = image.download_memory_to_staging(&mut self.runtime, &download_map, &copies);
                self.base.slot_images[image_id].flags = image.base.flags;
                self.base.slot_images[image_id].has_scaled = image.base.has_scaled;
                self.images.insert(image_id, image);
                download_map.offset +=
                    common::alignment::align_up(image_base.unswizzled_size_bytes as u64, 64)
                        as vk::DeviceSize;
            }
            self.uncommitted_async_buffers.push(download_map);
        }

        self.async_buffers
            .push_back(std::mem::take(&mut self.uncommitted_async_buffers));
        self.base.committed_downloads.push_back(download_ids);
    }

    pub fn pop_async_flushes(&mut self) {
        let Some(download_ids) = self.base.committed_downloads.pop_front() else {
            return;
        };
        let mut download_map = self.async_buffers.pop_front().unwrap_or_default();
        if download_ids.is_empty() {
            return;
        }
        for download_info in download_ids.iter().rev() {
            let Some(download_buffer) = download_map.get_mut(download_info.async_buffer_id) else {
                log::warn!(
                    "TextureCacheVulkan::pop_async_flushes missing async buffer {}",
                    download_info.async_buffer_id
                );
                continue;
            };
            let start = download_buffer.offset as usize;
            let span = unsafe {
                std::slice::from_raw_parts(download_buffer.mapped, download_buffer.size as usize)
            };
            if download_info.is_swizzle {
                let image = self.base.slot_images[download_info.object_id].clone();
                let aligned_size =
                    common::alignment::align_up(image.unswizzled_size_bytes as u64, 64)
                        as vk::DeviceSize;
                download_buffer.offset = download_buffer.offset.saturating_sub(aligned_size);
                let start = download_buffer.offset as usize;
                let end = start.saturating_add(image.unswizzled_size_bytes as usize);
                if end <= span.len() {
                    let copies = full_download_copies(&image.info);
                    let _ = self
                        .base
                        .write_downloaded_image(&image, &copies, &span[start..end]);
                } else {
                    log::warn!(
                        "TextureCacheVulkan::pop_async_flushes swizzle range out of bounds start={} end={} len={}",
                        start,
                        end,
                        span.len()
                    );
                }
            } else {
                let buffer_info = self
                    .base
                    .slot_buffer_downloads
                    .take(download_info.object_id);
                let end = start.saturating_add(buffer_info.size);
                if end <= span.len() {
                    let _ = self
                        .base
                        .write_downloaded_buffer(buffer_info.address, &span[start..end]);
                } else {
                    log::warn!(
                        "TextureCacheVulkan::pop_async_flushes DMA range out of bounds start={} end={} len={}",
                        start,
                        end,
                        span.len()
                    );
                }
            }
        }
        self.async_buffers_death_ring.extend(download_map);
    }

    /// Vulkan-backed port of `TextureCache<P>::DmaBufferImageCopy`.
    pub fn dma_buffer_image_copy(
        &mut self,
        copy_info: &dma::ImageCopy,
        buffer_operand: &dma::BufferOperand,
        image_operand: &dma::ImageOperand,
        image_id: ImageId,
        buffer: vk::Buffer,
        buffer_offset: vk::DeviceSize,
        is_upload: bool,
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
    ) -> bool {
        let trace_dma = std::env::var_os("RUZU_TRACE_DMA_IMAGE").is_some();
        if buffer == vk::Buffer::null() || image_id == NULL_IMAGE_ID || !image_id.is_valid() {
            if trace_dma {
                log::info!(
                    "[DMA_IMAGE] texture copy invalid input upload={} image_id={} buffer=0x{:X}",
                    is_upload,
                    image_id.index,
                    buffer.as_raw()
                );
            }
            return false;
        }
        let Some(copy) = self
            .base
            .dma_buffer_image_copy_descriptor(copy_info, buffer_operand, image_operand, image_id)
            .map(|result| result.copy)
        else {
            if trace_dma {
                log::info!(
                    "[DMA_IMAGE] texture copy descriptor failed upload={} image_id={} len={}x{} buffer_addr=0x{:X} image_addr=0x{:X}",
                    is_upload,
                    image_id.index,
                    copy_info.length_x,
                    copy_info.length_y,
                    buffer_operand.address,
                    image_operand.address
                );
            }
            return false;
        };

        if is_upload {
            let image_base_for_trace = &self.base.slot_images[image_id];
            if trace_image_write(image_base_for_trace.gpu_addr) {
                log::info!(
                    "[VK_IMAGE_WRITE] op=DmaBufferToImage image_id={} image_gpu=0x{:X} image_cpu=0x{:X} fmt={:?} buffer_addr=0x{:X} image_addr=0x{:X} len={}x{} pitch={} height={}",
                    image_id.index,
                    image_base_for_trace.gpu_addr,
                    image_base_for_trace.cpu_addr,
                    image_base_for_trace.info.format,
                    buffer_operand.address,
                    image_operand.address,
                    copy_info.length_x,
                    copy_info.length_y,
                    buffer_operand.pitch,
                    buffer_operand.height,
                );
            }
            if !self.prepare_image_with_reader(image_id, true, false, read_gpu_unsafe) {
                if trace_dma {
                    log::info!(
                        "[DMA_IMAGE] texture upload prepare failed image_id={} buffer_addr=0x{:X} image_addr=0x{:X}",
                        image_id.index,
                        buffer_operand.address,
                        image_operand.address
                    );
                }
                return false;
            }
        } else {
            if !self.prepare_image_with_reader(image_id, false, false, read_gpu_unsafe) {
                if trace_dma {
                    log::info!(
                        "[DMA_IMAGE] texture download prepare failed image_id={} buffer_addr=0x{:X} image_addr=0x{:X}",
                        image_id.index,
                        buffer_operand.address,
                        image_operand.address
                    );
                }
                return false;
            }
            let bpp = crate::surface::bytes_per_block(self.base.slot_images[image_id].info.format);
            if buffer_offset as usize % bpp as usize != 0 {
                if trace_dma {
                    log::info!(
                        "[DMA_IMAGE] texture download unaligned buffer offset image_id={} offset={} bpp={}",
                        image_id.index,
                        buffer_offset,
                        bpp
                    );
                }
                return false;
            }
        }

        let copies = [copy];
        if is_upload {
            let image_base = self.base.slot_images[image_id].clone();
            let format = self.runtime.surface_format(image_base.info.format);
            let aspect = image_aspect_mask(image_base.info.format);
            if aspect.is_empty()
                || self
                    .ensure_image(image_id, &image_base, format, aspect)
                    .is_err()
            {
                if trace_dma {
                    log::info!(
                        "[DMA_IMAGE] texture upload ensure image failed image_id={} format={:?} aspect={:?}",
                        image_id.index,
                        format,
                        aspect
                    );
                }
                return false;
            }
            let Some(mut image) = self.images.remove(&image_id) else {
                if trace_dma {
                    log::info!(
                        "[DMA_IMAGE] texture upload missing backend image image_id={}",
                        image_id.index
                    );
                }
                return false;
            };
            image.base = image_base;
            let uploaded = image.upload_memory(&mut self.runtime, buffer, buffer_offset, &copies);
            self.base.slot_images[image_id].flags = image.base.flags;
            self.base.slot_images[image_id].has_scaled = image.base.has_scaled;
            self.images.insert(image_id, image);
            if trace_dma {
                log::info!(
                    "[DMA_IMAGE] texture upload image_id={} buffer=0x{:X} offset={} uploaded={}",
                    image_id.index,
                    buffer.as_raw(),
                    buffer_offset,
                    uploaded
                );
            }
            uploaded
        } else {
            let size = buffer_operand.pitch.saturating_mul(buffer_operand.height) as usize;
            let downloaded = self.download_image_into_buffer(
                image_id,
                buffer,
                buffer_offset,
                &copies,
                buffer_operand.address,
                size,
            );
            if trace_dma {
                log::info!(
                    "[DMA_IMAGE] texture download image_id={} buffer=0x{:X} offset={} size={} downloaded={}",
                    image_id.index,
                    buffer.as_raw(),
                    buffer_offset,
                    size,
                    downloaded
                );
            }
            downloaded
        }
    }

    /// Vulkan-backed port of `TextureCache<P>::DownloadImageIntoBuffer`.
    fn download_image_into_buffer(
        &mut self,
        image_id: ImageId,
        buffer: vk::Buffer,
        buffer_offset: vk::DeviceSize,
        copies: &[BufferImageCopy],
        address: u64,
        size: usize,
    ) -> bool {
        let trace_dma = std::env::var_os("RUZU_TRACE_DMA_IMAGE").is_some();
        if size == 0 || buffer == vk::Buffer::null() || !self.base_image_exists(image_id) {
            if trace_dma {
                log::info!(
                    "[DMA_IMAGE] download into buffer invalid input image_id={} buffer=0x{:X} size={}",
                    image_id.index,
                    buffer.as_raw(),
                    size
                );
            }
            return false;
        }

        let mut image_base = self.base.slot_images[image_id].clone();
        self.apply_backend_image_flags(&mut image_base);
        let format = self.runtime.surface_format(image_base.info.format);
        let aspect = image_aspect_mask(image_base.info.format);
        if aspect.is_empty()
            || self
                .ensure_image(image_id, &image_base, format, aspect)
                .is_err()
        {
            if trace_dma {
                log::info!(
                    "[DMA_IMAGE] download into buffer ensure image failed image_id={} format={:?} aspect={:?}",
                    image_id.index,
                    format,
                    aspect
                );
            }
            return false;
        }

        let slot = self
            .base
            .slot_buffer_downloads
            .insert(BufferDownload { address, size });
        self.base.uncommitted_downloads.push(PendingDownload {
            is_swizzle: false,
            async_buffer_id: self.uncommitted_async_buffers.len(),
            object_id: slot,
        });

        let Some(mut download_map) = self
            .runtime
            .download_staging_buffer(size as vk::DeviceSize, true)
        else {
            if trace_dma {
                log::info!(
                    "[DMA_IMAGE] download into buffer staging allocation failed image_id={} size={}",
                    image_id.index,
                    size
                );
            }
            let _ = self.base.slot_buffer_downloads.take(slot);
            let _ = self.base.uncommitted_downloads.pop();
            return false;
        };

        let async_buffer_id = self.uncommitted_async_buffers.len();
        self.uncommitted_async_buffers.push(download_map);

        let Some(mut image) = self.images.remove(&image_id) else {
            if trace_dma {
                log::info!(
                    "[DMA_IMAGE] download into buffer missing backend image image_id={}",
                    image_id.index
                );
            }
            let _ = self.base.slot_buffer_downloads.take(slot);
            let _ = self.base.uncommitted_downloads.pop();
            if let Some(mut download_map) = self.uncommitted_async_buffers.pop() {
                self.runtime.free_deferred_staging_buffer(&mut download_map);
            }
            return false;
        };
        image.base = image_base;
        let download_buffer = self.uncommitted_async_buffers[async_buffer_id].buffer;
        let download_offset = self.uncommitted_async_buffers[async_buffer_id].offset;
        let downloaded = image.download_memory(
            &mut self.runtime,
            &[buffer, download_buffer],
            &[buffer_offset, download_offset],
            copies,
        );
        self.base.slot_images[image_id].flags = image.base.flags;
        self.base.slot_images[image_id].has_scaled = image.base.has_scaled;
        self.images.insert(image_id, image);

        if !downloaded {
            if trace_dma {
                log::info!(
                    "[DMA_IMAGE] download into buffer copy failed image_id={} buffer=0x{:X} staging=0x{:X} size={}",
                    image_id.index,
                    buffer.as_raw(),
                    download_map.buffer.as_raw(),
                    size
                );
            }
            let _ = self.base.slot_buffer_downloads.take(slot);
            let _ = self.base.uncommitted_downloads.pop();
            if let Some(mut download_map) = self.uncommitted_async_buffers.pop() {
                self.runtime.free_deferred_staging_buffer(&mut download_map);
            }
            return false;
        }

        if trace_dma {
            log::info!(
                "[DMA_IMAGE] download into buffer queued image_id={} address=0x{:X} size={} async_buffer_id={} staging=0x{:X}",
                image_id.index,
                address,
                size,
                async_buffer_id,
                download_buffer.as_raw()
            );
        }
        true
    }

    fn download_image_to_host_staging(
        &mut self,
        image_id: ImageId,
    ) -> Option<(ImageBase, Vec<u8>)> {
        if !self.base_image_exists(image_id) {
            return None;
        }
        let image_base = self.base.slot_images[image_id].clone();
        let staging_size = image_base.unswizzled_size_bytes as usize;
        if staging_size == 0 {
            return None;
        }
        let format = self.runtime.surface_format(image_base.info.format);
        let aspect = image_aspect_mask(image_base.info.format);
        if aspect.is_empty()
            || self
                .ensure_image(image_id, &image_base, format, aspect)
                .is_err()
        {
            return None;
        }

        let copies = full_download_copies(&image_base.info);
        let staging = self
            .runtime
            .download_staging_buffer(staging_size as vk::DeviceSize, false)?;
        let Some(mut image) = self.images.remove(&image_id) else {
            return None;
        };
        image.base = image_base.clone();
        let downloaded = image.download_memory_to_staging(&mut self.runtime, &staging, &copies);
        self.base.slot_images[image_id].flags = image.base.flags;
        self.base.slot_images[image_id].has_scaled = image.base.has_scaled;
        self.images.insert(image_id, image);
        if !downloaded {
            return None;
        }

        self.runtime.finish();
        let staging_bytes =
            unsafe { std::slice::from_raw_parts(staging.mapped, staging_size) }.to_vec();
        Some((image_base, staging_bytes))
    }

    pub fn debug_dump_image_at_gpu_native(
        &mut self,
        gpu_addr: u64,
        path: &std::path::Path,
    ) -> bool {
        let Some(image_id) = self
            .base
            .slot_images
            .iter()
            .find(|(_, image)| image.gpu_addr == gpu_addr)
            .map(|(id, _)| id)
        else {
            return false;
        };
        self.debug_dump_image_native(image_id, path)
    }

    pub fn debug_dump_image_at_cpu_native(
        &mut self,
        cpu_addr: u64,
        path: &std::path::Path,
    ) -> bool {
        let Some(image_id) = self
            .base
            .slot_images
            .iter()
            .find(|(_, image)| image.cpu_addr == cpu_addr)
            .map(|(id, _)| id)
        else {
            return false;
        };
        self.debug_dump_image_native(image_id, path)
    }

    /// Dump the image backing the color attachment currently bound in `slot`.
    ///
    /// GPU addresses may have several live cache aliases. Diagnostics that
    /// search by address can therefore select a stale image instead of the
    /// framebuffer attachment used by the draw being inspected.
    pub fn debug_dump_bound_color_rgba(
        &mut self,
        slot: usize,
        path: &std::path::Path,
    ) -> bool {
        let Some(&view_id) = self.base.render_targets.color_buffer_ids.get(slot) else {
            return false;
        };
        if !view_id.is_valid() || view_id == NULL_IMAGE_VIEW_ID {
            return false;
        }
        let image_id = self.base.slot_image_views[view_id].image_id;
        if !image_id.is_valid() || image_id == NULL_IMAGE_ID {
            return false;
        }
        let Some((image_base, bytes)) = self.download_image_to_host_staging(image_id) else {
            return false;
        };
        let bytes = decode_debug_dump_to_rgba8(&bytes, &image_base.info);
        let written = write_rgba_like_ppm(
            path,
            &bytes,
            image_base.info.size.width.max(1),
            image_base.info.size.height.max(1),
        )
        .is_ok();
        if written {
            log::info!(
                "[VK_BOUND_COLOR_DUMP] slot={} view_id={} image_id={} format={:?} size={}x{} gpu=0x{:X} path={}",
                slot,
                view_id.index,
                image_id.index,
                image_base.info.format,
                image_base.info.size.width,
                image_base.info.size.height,
                image_base.gpu_addr,
                path.display(),
            );
        }
        written
    }

    pub fn debug_dump_image_native(&mut self, image_id: ImageId, path: &std::path::Path) -> bool {
        let Some((image_base, bytes)) = self.download_image_to_host_staging(image_id) else {
            return false;
        };
        let written = std::fs::write(path, bytes).is_ok();
        if written {
            log::info!(
                "[VK_NATIVE_DUMP] image_id={} format={:?} size={}x{}x{} levels={} gpu=0x{:X} cpu=0x{:X} path={}",
                image_id.index,
                image_base.info.format,
                image_base.info.size.width,
                image_base.info.size.height,
                image_base.info.size.depth,
                image_base.info.resources.levels,
                image_base.gpu_addr,
                image_base.cpu_addr,
                path.display()
            );
        }
        written
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
        dirty_flags: &mut [bool; 256],
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
        is_clear: bool,
        clear_scissor: Option<(u32, u32, u32, u32)>,
    ) -> Option<RenderTargetFramebuffer> {
        self.finish_pending_backend_deletions();
        let gpu_memory = self.base.channel_gpu_memory.as_ref().cloned()?;
        self.update_render_targets_from_snapshot_with_dirty_flags_and_finish(
            render_targets,
            dirty_flags,
            read_gpu_unsafe,
            |gpu_addr, guest_size| {
                let gpu_memory = gpu_memory.lock();
                gpu_memory
                    .gpu_to_cpu_address(gpu_addr)
                    .or_else(|| gpu_memory.gpu_to_cpu_address_range(gpu_addr, guest_size))
            },
        );
        if !self.prepare_bound_render_target_views(read_gpu_unsafe, is_clear, clear_scissor) {
            return None;
        }
        self.finish_pending_backend_insertions_with_reader(read_gpu_unsafe);

        // Materialise every bound colour attachment (MRT) and the depth/stencil
        // attachment from the common cache, then assemble a framebuffer keyed by
        // the whole set — mirroring upstream GetFramebuffer rather than keying on
        // a single RT0 address with a synthesised depth buffer.
        let color_ids = self.base.render_targets.color_buffer_ids;
        let depth_id = self.base.render_targets.depth_buffer_id;

        // Upstream treats a render target with format 0 as DISABLED (the
        // per-target binding above already bound ImageViewId::default() for
        // it) and still renders depth-only — shadow maps and depth pre-pass
        // draws have no colour target at all. Only bail out when NOTHING is
        // bound. The old `rt0.format == 0 → None` early-out silently dropped
        // every depth-only draw into the offscreen fallback (MK8D issues
        // 260k+ such draws during loading).
        let rt0 = render_targets.render_targets[0];
        let rt0_disabled = rt0.address == 0 || rt0.width == 0 || rt0.height == 0 || rt0.format == 0;
        let any_color_bound = color_ids
            .iter()
            .any(|id| id.is_valid() && *id != NULL_IMAGE_VIEW_ID);
        let depth_bound = depth_id.is_valid() && depth_id != NULL_IMAGE_VIEW_ID;
        if rt0_disabled && !any_color_bound && !depth_bound {
            return None;
        }
        let base_size = self.base.render_targets.size;

        let mut recreated = false;
        let mut colors: Vec<(usize, ImageViewId, vk::Format)> = Vec::new();
        let mut color_views: Vec<vk::ImageView> = Vec::new();
        let mut extent = vk::Extent2D {
            width: base_size.width.max(1),
            height: base_size.height.max(1),
        };

        for (rt_index, &view_id) in color_ids.iter().enumerate() {
            if !view_id.is_valid() || view_id == NULL_IMAGE_VIEW_ID {
                continue;
            }
            let view = self.base.slot_image_views[view_id].clone();
            if !view.image_id.is_valid() || view.image_id == NULL_IMAGE_ID {
                continue;
            }
            let format = self.runtime.surface_format(view.format);
            let width = view.size.width.max(1);
            let height = view.size.height.max(1);
            // Per-draw fast path: only clone the ImageBase (six Vecs) and run
            // the full `ensure_image` when the backend image actually needs
            // (re)creation.
            if !self.backend_image_matches(view.image_id, format, vk::ImageAspectFlags::COLOR) {
                let image_base = self.base.slot_images[view.image_id].clone();
                recreated |= self
                    .ensure_image(
                        view.image_id,
                        &image_base,
                        format,
                        vk::ImageAspectFlags::COLOR,
                    )
                    .ok()?;
            }
            self.ensure_image_view(view_id).ok()?;
            extent.width = extent.width.min(width);
            extent.height = extent.height.min(height);
            let view_handle = self.image_views.get(&view_id)?.render_target();
            if std::env::var_os("RUZU_TRACE_VK_RT_FRAMEBUFFER").is_some() {
                log::info!(
                    "[VK_RT_FRAMEBUFFER_VIEW] color view_id={} image_id={} view_gpu=0x{:X} image_gpu=0x{:X} view_size={}x{} image_size={}x{} range={:?}",
                    view_id.index,
                    view.image_id.index,
                    view.gpu_addr,
                    self.base.slot_images[view.image_id].gpu_addr,
                    view.size.width,
                    view.size.height,
                    width,
                    height,
                    view.range,
                );
            }
            colors.push((rt_index, view_id, format));
            color_views.push(view_handle);
        }

        if colors.is_empty() && !rt0_disabled {
            // The common cache did not register a colour view; fall back to
            // resolving RT0 directly by GPU address (parity with prior behaviour).
            let gpu_memory = self.base.channel_gpu_memory.as_ref()?.lock();
            let cpu_addr = gpu_memory.gpu_to_cpu_address(rt0.address).or_else(|| {
                let approx = (rt0.width as u64)
                    .saturating_mul(rt0.height as u64)
                    .saturating_mul(4);
                gpu_memory.gpu_to_cpu_address_range(rt0.address, approx)
            })?;
            drop(gpu_memory);
            let format = self.runtime.surface_format(
                crate::surface::pixel_format_from_render_target_format(rt0.format),
            );
            let width = rt0.width.max(1);
            let height = rt0.height.max(1);
            let image_id = self.base.render_targets.color_buffer_ids[0];
            if !image_id.is_valid() || image_id == NULL_IMAGE_VIEW_ID {
                return None;
            }
            let image_id = self.base.slot_image_views[image_id].image_id;
            let image_base = self.base.slot_images[image_id].clone();
            recreated |= self
                .ensure_image(image_id, &image_base, format, vk::ImageAspectFlags::COLOR)
                .ok()?;
            self.ensure_image_view(self.base.render_targets.color_buffer_ids[0])
                .ok()?;
            extent.width = extent.width.min(width);
            extent.height = extent.height.min(height);
            let view_handle = self
                .image_views
                .get(&self.base.render_targets.color_buffer_ids[0])?
                .render_target();
            colors.push((0, self.base.render_targets.color_buffer_ids[0], format));
            color_views.push(view_handle);
        }

        let mut depth_format = vk::Format::UNDEFINED;
        let mut depth_view: Option<vk::ImageView> = None;
        if depth_id.is_valid() && depth_id != NULL_IMAGE_VIEW_ID {
            let view = self.base.slot_image_views[depth_id].clone();
            if view.image_id.is_valid() && view.image_id != NULL_IMAGE_ID {
                let format = self.runtime.surface_format(view.format);
                let aspect = image_aspect_mask(view.format);
                let width = view.size.width.max(1);
                let height = view.size.height.max(1);
                if !self.backend_image_matches(view.image_id, format, aspect) {
                    let image_base = self.base.slot_images[view.image_id].clone();
                    recreated |= self
                        .ensure_image(view.image_id, &image_base, format, aspect)
                        .ok()?;
                }
                self.ensure_image_view(depth_id).ok()?;
                extent.width = extent.width.min(width);
                extent.height = extent.height.min(height);
                depth_format = format;
                depth_view = Some(self.image_views.get(&depth_id)?.render_target());
            }
        }

        // Depth-only framebuffers have no colour attachment at all.
        if colors.is_empty() && depth_view.is_none() {
            return None;
        }
        let rt0_cpu = if let Some(&(_, rt0_view_id, _)) = colors.first() {
            let rt0_image_id = self.base.slot_image_views[rt0_view_id].image_id;
            self.base.slot_images[rt0_image_id].cpu_addr
        } else {
            // Depth-only: key writes/invalidation on the depth image instead.
            let depth_image_id = self.base.slot_image_views[depth_id].image_id;
            self.base.slot_images[depth_image_id].cpu_addr
        };

        if std::env::var_os("RUZU_TRACE_VK_RT_FRAMEBUFFER").is_some() {
            log::info!(
                "[VK_RT_FRAMEBUFFER] colors={} depth={} extent={}x{} base_size={}x{} rt0_cpu=0x{:X}",
                colors.len(),
                depth_view.is_some(),
                extent.width,
                extent.height,
                base_size.width,
                base_size.height,
                rt0_cpu,
            );
        }

        // A resized/reformatted surface invalidates any framebuffer that
        // referenced its old image view.
        if recreated {
            self.evict_rt_framebuffers();
        }

        let mut key = self.base.render_targets;
        key.size.width = extent.width;
        key.size.height = extent.height;
        if !self.framebuffers_by_render_targets.contains_key(&key) {
            let mut rp_key = RenderPassKey::default();
            let max_colors = rp_key.color_formats.len();
            let mut num_attachments = 0usize;
            for (rt_index, _, fmt) in colors.iter().take(max_colors) {
                if *rt_index < max_colors {
                    rp_key.color_formats[*rt_index] = *fmt;
                    num_attachments = num_attachments.max(*rt_index + 1);
                }
            }
            rp_key.num_color_attachments = num_attachments as u8;
            rp_key.depth_format = depth_format;
            let samples = colors
                .first()
                .and_then(|(_, view_id, _)| self.image_views.get(view_id))
                .map(ImageView::samples)
                .or_else(|| {
                    depth_view.and_then(|_| self.image_views.get(&depth_id).map(ImageView::samples))
                })
                .unwrap_or(vk::SampleCountFlags::TYPE_1);
            rp_key.samples = samples;
            let render_pass = self.runtime.render_pass_cache().get(&rp_key).ok()?;
            let framebuffer = self
                .create_framebuffer_owner(
                    render_pass,
                    &color_views,
                    &colors,
                    depth_view,
                    extent,
                    depth_id,
                    rt0_cpu,
                )
                .ok()?;
            self.framebuffers_by_render_targets
                .insert(key.clone(), framebuffer);
        }
        let fb = self.framebuffers_by_render_targets.get(&key)?;
        Some(RenderTargetFramebuffer {
            framebuffer: fb.framebuffer,
            render_pass: fb.render_pass,
            cpu_addr: fb.rt0_cpu_addr,
            extent: fb.render_area,
            num_color: fb.num_color_buffers,
            has_depth: fb.has_depth,
            has_stencil: fb.has_stencil,
            images: fb.images[..fb.num_images].to_vec(),
            image_ranges: fb.image_ranges[..fb.num_images].to_vec(),
            image_ids: fb.image_ids.clone(),
            rt_map: fb.rt_map,
        })
    }

    fn update_render_targets_from_snapshot_with_dirty_flags_and_finish(
        &mut self,
        render_targets: &crate::engines::draw_manager::Maxwell3DRenderTargets,
        dirty_flags: &mut [bool; 256],
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
        mut gpu_to_cpu: impl FnMut(u64, u64) -> Option<u64>,
    ) {
        if !dirty_flags[crate::dirty_flags::flags::RENDER_TARGETS as usize] {
            return;
        }

        let was_rescaling = self.base.is_rescaling;
        let mut dirty_color_targets = [false; NUM_RT];
        for index in 0..NUM_RT {
            dirty_color_targets[index] = dirty_flags
                [(crate::dirty_flags::flags::COLOR_BUFFER0 + index as u8) as usize]
                || dirty_flags[crate::dirty_flags::flags::RENDER_TARGET_CONTROL as usize];
        }
        let mut dirty_depth_target = dirty_flags[crate::dirty_flags::flags::ZETA_BUFFER as usize]
            || dirty_flags[crate::dirty_flags::flags::RENDER_TARGET_CONTROL as usize];
        dirty_flags[crate::dirty_flags::flags::RENDER_TARGETS as usize] = false;
        dirty_flags[crate::dirty_flags::flags::RENDER_TARGET_CONTROL as usize] = false;

        let (rescaled, scale_rating, color_images, depth_image) = loop {
            self.base.has_deleted_images = false;

            for index in 0..NUM_RT {
                if !dirty_color_targets[index] {
                    continue;
                }
                dirty_color_targets[index] = false;
                dirty_flags[(crate::dirty_flags::flags::COLOR_BUFFER0 + index as u8) as usize] =
                    false;
                if index >= render_targets.rt_control.count as usize {
                    self.base
                        .bind_color_render_target(index, ImageViewId::default());
                    continue;
                }
                let rt = render_targets.render_targets[index];
                if rt.address == 0 || rt.width == 0 || rt.height == 0 || rt.format == 0 {
                    self.base
                        .bind_color_render_target(index, ImageViewId::default());
                    continue;
                }

                let info =
                    ImageInfo::from_render_target_info(&rt, render_targets.anti_alias_samples_mode);
                let guest_size =
                    crate::texture_cache::util::calculate_guest_size_in_bytes(&info) as u64;
                let cpu_addr = gpu_to_cpu(rt.address, guest_size).unwrap_or_else(|| {
                    if std::env::var_os("RUZU_TRACE_RT").is_some() {
                        log::info!(
                            "[RT] miss translate color={} target={} gpu=0x{:X} {}x{} fmt=0x{:X}; using virtual-invalid fallback",
                            index,
                            index,
                            rt.address,
                            rt.width,
                            rt.height,
                            rt.format
                        );
                    }
                    self.base.resolve_or_allocate_cpu_addr(rt.address, guest_size)
                });
                let image_id = self.find_or_insert_render_target_image_with_retry(
                    &info,
                    rt.address,
                    cpu_addr,
                    read_gpu_unsafe,
                );
                let view_id = self.base.find_render_target_view_from_image(
                    image_id,
                    &rt,
                    render_targets.anti_alias_samples_mode,
                    rt.address,
                );
                self.base.bind_color_render_target(index, view_id);

                if std::env::var_os("RUZU_TRACE_RT").is_some() {
                    let image = &self.base.slot_images[image_id];
                    log::info!(
                        "[RT] color={} target={} gpu=0x{:X} cpu=0x{:X} {}x{} fmt=0x{:X} image={} views={}",
                        index,
                        index,
                        rt.address,
                        cpu_addr,
                        rt.width,
                        rt.height,
                        rt.format,
                        image_id.index,
                        image.image_view_ids.len()
                    );
                }
            }

            if dirty_depth_target {
                dirty_depth_target = false;
                dirty_flags[crate::dirty_flags::flags::ZETA_BUFFER as usize] = false;
                let zeta = render_targets.zeta;
                if zeta.enabled && zeta.address != 0 && zeta.width != 0 && zeta.height != 0 {
                    let info =
                        ImageInfo::from_zeta_info(&zeta, render_targets.anti_alias_samples_mode);
                    let guest_size =
                        crate::texture_cache::util::calculate_guest_size_in_bytes(&info) as u64;
                    let cpu_addr = gpu_to_cpu(zeta.address, guest_size).unwrap_or_else(|| {
                        if std::env::var_os("RUZU_TRACE_RT").is_some() {
                            log::info!(
                                "[RT] miss translate zeta gpu=0x{:X} {}x{} fmt=0x{:X}; using virtual-invalid fallback",
                                zeta.address,
                                zeta.width,
                                zeta.height,
                                zeta.format
                            );
                        }
                        self.base.resolve_or_allocate_cpu_addr(zeta.address, guest_size)
                    });
                    let image_id = self.find_or_insert_render_target_image_with_retry(
                        &info,
                        zeta.address,
                        cpu_addr,
                        read_gpu_unsafe,
                    );
                    let view_id =
                        self.base
                            .find_image_view_from_image_info(image_id, &info, zeta.address);
                    self.base.bind_depth_render_target(view_id);
                } else {
                    self.base.bind_depth_render_target(ImageViewId::default());
                }
            }

            let result = self.rescale_current_render_targets();
            if !self.base.has_deleted_images {
                break result;
            }
            dirty_color_targets.fill(true);
            dirty_depth_target = true;
        };

        self.set_render_target_scale_rating(scale_rating, color_images, depth_image);
        self.base.is_rescaling = rescaled;
        if was_rescaling != rescaled {
            dirty_flags[crate::dirty_flags::flags::RESCALE_VIEWPORTS as usize] = true;
            dirty_flags[crate::dirty_flags::flags::RESCALE_SCISSORS as usize] = true;
        }

        for index in 0..NUM_RT {
            self.base.render_targets.draw_buffers[index] =
                render_targets.rt_control.map[index] as u8;
        }
        let resolution = common::settings::values().resolution_info.clone();
        let (up_scale, down_shift) = if self.base.is_rescaling {
            (resolution.up_scale, resolution.down_shift)
        } else {
            (1, 0)
        };
        self.base.render_targets.size = Extent2D {
            width: (render_targets.surface_clip.width.wrapping_mul(up_scale)) >> down_shift,
            height: (render_targets.surface_clip.height.wrapping_mul(up_scale)) >> down_shift,
        };
        self.base.render_targets.is_rescaled = self.base.is_rescaling;
        dirty_flags[crate::dirty_flags::flags::DEPTH_BIAS_GLOBAL as usize] = true;
    }

    fn rescale_current_render_targets(
        &mut self,
    ) -> (bool, u32, [Option<ImageId>; NUM_RT], Option<ImageId>) {
        let mut scale_rating = 0u32;
        let mut any_rescaled = false;
        let mut can_rescale = true;
        let mut color_images = [None; NUM_RT];
        let mut depth_image = None;

        for (index, save) in color_images.iter_mut().enumerate() {
            let view_id = self.base.render_targets.color_buffer_ids[index];
            self.check_render_target_rescale(
                view_id,
                save,
                &mut can_rescale,
                &mut any_rescaled,
                &mut scale_rating,
            );
        }
        self.check_render_target_rescale(
            self.base.render_targets.depth_buffer_id,
            &mut depth_image,
            &mut can_rescale,
            &mut any_rescaled,
            &mut scale_rating,
        );

        let rescaled = if can_rescale {
            let rescaled = any_rescaled || scale_rating >= 2;
            if rescaled {
                for image_id in color_images.iter().flatten().copied().chain(depth_image) {
                    self.ensure_render_target_scaled_up(image_id);
                }
                scale_rating = 2;
            }
            rescaled
        } else {
            for image_id in color_images.iter().flatten().copied().chain(depth_image) {
                self.ensure_render_target_scaled_down(image_id);
            }
            scale_rating = 1;
            false
        };

        (rescaled, scale_rating, color_images, depth_image)
    }

    fn check_render_target_rescale(
        &mut self,
        view_id: ImageViewId,
        save: &mut Option<ImageId>,
        can_rescale: &mut bool,
        any_rescaled: &mut bool,
        scale_rating: &mut u32,
    ) {
        if !view_id.is_valid() || view_id == NULL_IMAGE_VIEW_ID {
            *save = None;
            return;
        }
        let image_id = self.base.slot_image_views[view_id].image_id;
        *save = Some(image_id);
        *can_rescale &= Self::image_can_rescale_base(&mut self.base, image_id);
        let image = &self.base.slot_images[image_id];
        *any_rescaled |= image.flags.contains(ImageFlagBits::RESCALED)
            || crate::surface::get_format_type(image.info.format) != SurfaceType::ColorTexture;
        *scale_rating = (*scale_rating).max(if image.scale_tick <= self.base.frame_tick {
            image.scale_rating + 1
        } else {
            image.scale_rating
        });
    }

    fn ensure_render_target_scaled_up(&mut self, image_id: ImageId) {
        if self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::RESCALED)
        {
            return;
        }
        let _ = self.scale_up_image(image_id, false);
    }

    fn ensure_render_target_scaled_down(&mut self, image_id: ImageId) {
        if !self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::RESCALED)
        {
            return;
        }
        let _ = self.scale_down_image(image_id, false);
    }

    fn set_render_target_scale_rating(
        &mut self,
        scale_rating: u32,
        color_images: [Option<ImageId>; NUM_RT],
        depth_image: Option<ImageId>,
    ) {
        for image_id in color_images.into_iter().flatten().chain(depth_image) {
            let image = &mut self.base.slot_images[image_id];
            image.scale_rating = scale_rating;
            if image.scale_tick <= self.base.frame_tick {
                image.scale_tick = self.base.frame_tick + 1;
            }
        }
    }

    fn find_or_insert_render_target_image_with_retry(
        &mut self,
        info: &ImageInfo,
        gpu_addr: u64,
        cpu_addr: u64,
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
    ) -> ImageId {
        let mut image_id = NULL_IMAGE_ID;
        let mut delete_state = self.base.has_deleted_images;
        loop {
            self.base.has_deleted_images = false;
            image_id = self.find_or_insert_image_from_info_with_options_and_finish(
                info,
                gpu_addr,
                cpu_addr,
                RelaxedOptions::empty(),
                read_gpu_unsafe,
            );
            delete_state |= self.base.has_deleted_images;
            if !self.base.has_deleted_images {
                break;
            }
        }
        self.base.has_deleted_images = delete_state;
        image_id
    }

    fn render_target_view_is_full_clear(
        &self,
        view_id: ImageViewId,
        clear_scissor: Option<(u32, u32, u32, u32)>,
    ) -> bool {
        if !view_id.is_valid() || view_id == NULL_IMAGE_VIEW_ID {
            return true;
        }
        let view = self.base.slot_image_views.get(view_id);
        let image = self.base.slot_images.get(view.image_id);
        if image.info.resources.levels > 1 || image.info.resources.layers > 1 {
            return false;
        }
        let Some((min_x, min_y, max_x, max_y)) = clear_scissor else {
            return true;
        };
        min_x == 0 && min_y == 0 && max_x >= view.size.width && max_y >= view.size.height
    }

    fn prepare_bound_render_target_views(
        &mut self,
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
        is_clear: bool,
        clear_scissor: Option<(u32, u32, u32, u32)>,
    ) -> bool {
        let mut view_ids = Vec::with_capacity(NUM_RT + 1);
        for &view_id in self.base.render_targets.color_buffer_ids.iter() {
            if view_id.is_valid() && view_id != NULL_IMAGE_VIEW_ID {
                view_ids.push(view_id);
            }
        }
        let depth_id = self.base.render_targets.depth_buffer_id;
        if depth_id.is_valid() && depth_id != NULL_IMAGE_VIEW_ID {
            view_ids.push(depth_id);
        }

        for view_id in view_ids {
            let image_id = self.base.slot_image_views[view_id].image_id;
            if !image_id.is_valid() || image_id == NULL_IMAGE_ID {
                continue;
            }
            if !self.finish_pending_backend_insertion_with_reader(image_id, read_gpu_unsafe) {
                return false;
            }
            let invalidate =
                is_clear && self.render_target_view_is_full_clear(view_id, clear_scissor);
            if !self.prepare_image_with_reader(image_id, true, invalidate, read_gpu_unsafe) {
                return false;
            }
        }
        true
    }

    /// In-place fast check: the image has no pending CPU upload and the
    /// backend image exists with matching format/geometry (i.e. both
    /// `RefreshContents` and `ensure_image` would be no-ops). Performs no
    /// clones and no mutations.
    fn image_up_to_date(&self, image_id: ImageId) -> bool {
        let base = &self.base.slot_images[image_id];
        if base.flags.contains(ImageFlagBits::CPU_MODIFIED) {
            return false;
        }
        let format = self.runtime.surface_format(base.info.format);
        let aspect = image_aspect_mask(base.info.format);
        self.backend_image_matches(image_id, format, aspect)
    }

    /// In-place equivalent of `ensure_image`'s no-op test: the backend image
    /// exists and matches format/geometry, so `ensure_image` would not
    /// recreate anything. Performs no clones and no mutations.
    fn backend_image_matches(
        &self,
        image_id: ImageId,
        format: vk::Format,
        aspect: vk::ImageAspectFlags,
    ) -> bool {
        let Some(existing) = self.images.get(&image_id) else {
            return false;
        };
        let base = &self.base.slot_images[image_id];
        existing.format == format
            && existing.aspect == aspect
            && existing.base.info.size == base.info.size
            && existing.base.info.resources == base.info.resources
            && existing.base.info.image_type == base.info.image_type
            && existing.base.info.num_samples == base.info.num_samples
    }

    /// Ensure a backend `Image` exists for the common-cache `ImageId`.
    /// Returns `true` when an existing backend image was recreated.
    fn ensure_image(
        &mut self,
        image_id: ImageId,
        image_base: &ImageBase,
        format: vk::Format,
        aspect: vk::ImageAspectFlags,
    ) -> Result<bool, vk::Result> {
        if let Some(existing) = self.images.get(&image_id) {
            if existing.format == format
                && existing.aspect == aspect
                && existing.base.info.size == image_base.info.size
                && existing.base.info.resources == image_base.info.resources
                && existing.base.info.image_type == image_base.info.image_type
                && existing.base.info.num_samples == image_base.info.num_samples
            {
                return Ok(false);
            }
            // Sentence the stale image and its views to the delayed-destruction
            // ring rather than destroying them immediately: a full
            // `device_wait_idle()` on every resize/reformat is a GPU stall, and
            // the ring already keeps resources alive until the GPU passes the
            // tick. Dependent framebuffers are evicted by the caller's
            // `recreated` path (`evict_rt_framebuffers`).
            if let Some(old) = self.images.remove(&image_id) {
                self.render_target_cpu_map.remove(&old.base.cpu_addr);
                self.runtime.sentence_image(old);
            }
            let removed_view_ids: Vec<_> = self
                .image_views
                .iter()
                .filter_map(|(&view_id, view)| (view.base.image_id == image_id).then_some(view_id))
                .collect();
            for view_id in removed_view_ids {
                if let Some(view) = self.image_views.remove(&view_id) {
                    self.runtime.sentence_image_view(view);
                }
            }
            let image = self.make_image(image_id, image_base, format, aspect)?;
            self.base.slot_images[image_id].flags = image.base.flags;
            self.render_target_cpu_map
                .insert(image_base.cpu_addr, image_id);
            self.images.insert(image_id, image);
            return Ok(true);
        }
        let image = self.make_image(image_id, image_base, format, aspect)?;
        self.base.slot_images[image_id].flags = image.base.flags;
        self.render_target_cpu_map
            .insert(image_base.cpu_addr, image_id);
        self.images.insert(image_id, image);
        Ok(false)
    }

    fn make_image(
        &mut self,
        image_id: ImageId,
        image_base: &ImageBase,
        format: vk::Format,
        aspect: vk::ImageAspectFlags,
    ) -> Result<Image, vk::Result> {
        let image = self
            .runtime
            .create_image_from_info(&image_base.info, format)?;
        let image_handle = image.handle();
        let mut base = image_base.clone();
        self.apply_backend_image_flags(&mut base);
        Ok(Image {
            image_id,
            base,
            original_image: image,
            current_image: image_handle,
            scaled_image: None,
            storage_image_views: vec![
                vk::ImageView::null();
                image_base.info.resources.levels.max(1) as usize
            ],
            format,
            aspect,
            initialized: false,
            layout: vk::ImageLayout::UNDEFINED,
            scale_view: vk::ImageView::null(),
            normal_view: vk::ImageView::null(),
            scale_depth_view: vk::ImageView::null(),
            normal_depth_view: vk::ImageView::null(),
            scale_stencil_view: vk::ImageView::null(),
            normal_stencil_view: vk::ImageView::null(),
            scale_framebuffer: None,
            normal_framebuffer: None,
        })
    }

    fn apply_backend_image_flags(&self, image: &mut ImageBase) {
        if crate::surface::is_pixel_format_bcn(image.info.format)
            && !self.runtime.optimal_bcn_supported
        {
            image
                .flags
                .insert(ImageFlagBits::CONVERTED | ImageFlagBits::COSTLY_LOAD);
        }
    }

    fn ensure_image_view(&mut self, view_id: ImageViewId) -> Result<(), vk::Result> {
        if self.image_views.contains_key(&view_id) {
            return Ok(());
        }
        let view_base = self.base.slot_image_views[view_id].clone();
        let image = self
            .images
            .get(&view_base.image_id)
            .ok_or(vk::Result::ERROR_INITIALIZATION_FAILED)?;
        let view = self.runtime.make_image_view(view_id, &view_base, image)?;
        self.image_views.insert(view_id, view);
        Ok(())
    }

    fn evict_rt_framebuffers(&mut self) {
        for (_, fb) in self.framebuffers_by_render_targets.drain() {
            self.runtime.sentence_framebuffer(fb.framebuffer);
        }
    }

    fn remove_framebuffers_for_view(&mut self, view_id: ImageViewId) {
        let removed_ids = [view_id];
        let remove_keys = self
            .framebuffers_by_render_targets
            .keys()
            .filter(|key| key.contains(&removed_ids))
            .copied()
            .collect::<Vec<_>>();
        for key in remove_keys {
            if let Some(framebuffer) = self.framebuffers_by_render_targets.remove(&key) {
                self.runtime.sentence_framebuffer(framebuffer.framebuffer);
            }
        }
    }

    fn finish_scale_invalidation_backend(&mut self, image_view_ids: &[ImageViewId]) {
        for &view_id in image_view_ids {
            self.remove_framebuffers_for_view(view_id);
            if let Some(view) = self.image_views.remove(&view_id) {
                self.runtime.sentence_image_view(view);
            }
        }
    }

    fn finish_pending_backend_deletions(&mut self) {
        let pending = std::mem::take(&mut self.base.pending_backend_deletions);
        if pending.is_empty() {
            return;
        }
        for deletion in pending {
            self.render_target_cpu_map
                .retain(|_, &mut image_id| image_id != deletion.image_id);
            for view_id in deletion.image_view_ids {
                self.remove_framebuffers_for_view(view_id);
                if let Some(view) = self.image_views.remove(&view_id) {
                    self.runtime.sentence_image_view(view);
                }
            }
            if let Some(image) = self.images.remove(&deletion.image_id) {
                self.runtime.sentence_image(image);
            }
        }
    }

    fn base_image_exists(&self, image_id: ImageId) -> bool {
        image_id.is_valid() && self.base.slot_images.iter().any(|(id, _)| id == image_id)
    }

    fn materialize_backend_image(&mut self, image_id: ImageId) -> bool {
        if !self.base_image_exists(image_id) {
            return false;
        }
        let image_base = self.base.slot_images[image_id].clone();
        let format = self.runtime.surface_format(image_base.info.format);
        let aspect = image_aspect_mask(image_base.info.format);
        !aspect.is_empty()
            && self
                .ensure_image(image_id, &image_base, format, aspect)
                .is_ok()
    }

    fn finish_pending_backend_insertions(&mut self) {
        let pending = std::mem::take(&mut self.base.pending_backend_insertions);
        if pending.is_empty() {
            return;
        }
        let mut deferred = Vec::new();
        for image_id in pending {
            if !self.base_image_exists(image_id) {
                continue;
            }
            if !self.materialize_backend_image(image_id) {
                deferred.push(image_id);
                continue;
            }
            if self.base.slot_images[image_id]
                .flags
                .contains(ImageFlagBits::CPU_MODIFIED)
            {
                deferred.push(image_id);
                continue;
            }
            self.register_completed_backend_image(image_id);
        }
        self.base.pending_backend_insertions.extend(deferred);
    }

    fn finish_pending_backend_insertions_with_reader(
        &mut self,
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
    ) -> bool {
        let pending = std::mem::take(&mut self.base.pending_backend_insertions);
        if pending.is_empty() {
            return true;
        }
        let mut all_finished = true;
        for image_id in pending {
            if !self.base_image_exists(image_id) {
                continue;
            }
            if !self.refresh_contents_with_reader(image_id, read_gpu_unsafe) {
                self.base.pending_backend_insertions.push(image_id);
                all_finished = false;
                continue;
            }
            self.register_completed_backend_image(image_id);
        }
        all_finished
    }

    fn finish_pending_backend_insertion_with_reader(
        &mut self,
        image_id: ImageId,
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
    ) -> bool {
        let pending = std::mem::take(&mut self.base.pending_backend_insertions);
        let mut found = false;
        for pending_id in pending {
            if pending_id == image_id {
                found = true;
            } else {
                self.base.pending_backend_insertions.push(pending_id);
            }
        }
        if !found {
            return true;
        }
        if !self.base_image_exists(image_id) {
            return false;
        }
        if !self.refresh_contents_with_reader(image_id, read_gpu_unsafe) {
            self.base.pending_backend_insertions.push(image_id);
            return false;
        }
        self.register_completed_backend_image(image_id);
        true
    }

    fn image_can_rescale_base(base: &mut CommonTextureCache, image_id: ImageId) -> bool {
        if !image_id.is_valid() || !Self::base_image_exists_in(base, image_id) {
            return false;
        }
        if !base.slot_images[image_id].info.rescaleable {
            return false;
        }
        let resolution = common::settings::values().resolution_info.clone();
        if resolution.downscale && !base.slot_images[image_id].info.downscaleable {
            return false;
        }
        if base.slot_images[image_id]
            .flags
            .intersects(ImageFlagBits::RESCALED | ImageFlagBits::CHECKING_RESCALABLE)
        {
            return true;
        }
        if base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::IS_RESCALABLE)
        {
            return true;
        }

        base.slot_images[image_id]
            .flags
            .insert(ImageFlagBits::CHECKING_RESCALABLE);
        let aliases = base.slot_images[image_id]
            .aliased_images
            .iter()
            .map(|alias| alias.id)
            .collect::<Vec<_>>();
        for alias_id in aliases {
            if !Self::image_can_rescale_base(base, alias_id) {
                base.slot_images[image_id]
                    .flags
                    .remove(ImageFlagBits::CHECKING_RESCALABLE);
                return false;
            }
        }
        base.slot_images[image_id]
            .flags
            .remove(ImageFlagBits::CHECKING_RESCALABLE);
        base.slot_images[image_id]
            .flags
            .insert(ImageFlagBits::IS_RESCALABLE);
        true
    }

    fn base_image_exists_in(base: &CommonTextureCache, image_id: ImageId) -> bool {
        image_id.is_valid() && base.slot_images.iter().any(|(id, _)| id == image_id)
    }

    fn prepare_pending_join_sibling_rescale_gate(
        base: &mut CommonTextureCache,
        new_image_id: ImageId,
        copies: &[crate::texture_cache::texture_cache_base::JoinCopy],
    ) -> bool {
        if !Self::base_image_exists_in(base, new_image_id) {
            return false;
        }

        let mut can_rescale = base.slot_images[new_image_id].info.rescaleable;
        let mut any_rescaled = false;
        for copy in copies {
            if !can_rescale {
                break;
            }
            if !Self::base_image_exists_in(base, copy.id) {
                can_rescale = false;
                break;
            }
            can_rescale &= Self::image_can_rescale_base(base, copy.id);
            any_rescaled |= base.slot_images[copy.id]
                .flags
                .contains(ImageFlagBits::RESCALED);
        }
        can_rescale && any_rescaled
    }

    fn scale_up_image(&mut self, image_id: ImageId, ignore: bool) -> bool {
        if !self.base_image_exists(image_id) {
            return false;
        }
        if self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::RESCALED)
        {
            return false;
        }
        let image_base = self.base.slot_images[image_id].clone();
        let format = self.runtime.surface_format(image_base.info.format);
        let aspect = image_aspect_mask(image_base.info.format);
        if aspect.is_empty()
            || self
                .ensure_image(image_id, &image_base, format, aspect)
                .is_err()
        {
            return false;
        }
        let Some(mut image) = self.images.remove(&image_id) else {
            return false;
        };
        image.base = image_base;
        let had_scaled_copy = image.base.has_scaled;
        let scaled = image.scale_up(&mut self.runtime, ignore);
        self.base.slot_images[image_id].flags = image.base.flags;
        self.base.slot_images[image_id].has_scaled = image.base.has_scaled;
        if scaled {
            self.base.account_scale_up_memory(image_id, had_scaled_copy);
            let removed_views = self.base.invalidate_scale(image_id);
            self.finish_scale_invalidation_backend(&removed_views);
            image.base = self.base.slot_images[image_id].clone();
        }
        self.images.insert(image_id, image);
        scaled
    }

    fn scale_down_image(&mut self, image_id: ImageId, ignore: bool) -> bool {
        if !self.base_image_exists(image_id) {
            return false;
        }
        if !self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::RESCALED)
        {
            return false;
        }
        if self.base.slot_images[image_id].info.image_type == ImageType::Linear {
            return false;
        }
        let image_base = self.base.slot_images[image_id].clone();
        let Some(mut image) = self.images.remove(&image_id) else {
            return false;
        };
        image.base = image_base;
        let scaled = image.scale_down(&mut self.runtime, ignore);
        self.base.slot_images[image_id].flags = image.base.flags;
        self.base.slot_images[image_id].has_scaled = image.base.has_scaled;
        if scaled {
            let removed_views = self.base.invalidate_scale(image_id);
            self.finish_scale_invalidation_backend(&removed_views);
            image.base = self.base.slot_images[image_id].clone();
        }
        self.images.insert(image_id, image);
        scaled
    }

    fn prepare_pending_join_sibling_rescale(
        &mut self,
        copies: &[crate::texture_cache::texture_cache_base::JoinCopy],
        can_rescale: bool,
    ) -> bool {
        for copy in copies {
            if !self.base_image_exists(copy.id) {
                continue;
            }
            let is_rescaled = self.base.slot_images[copy.id]
                .flags
                .contains(ImageFlagBits::RESCALED);
            if can_rescale {
                if !is_rescaled && !self.scale_up_image(copy.id, false) {
                    return false;
                }
            } else if is_rescaled && !self.scale_down_image(copy.id, false) {
                return false;
            }
        }
        true
    }

    fn prepare_pending_join_new_image_rescale(
        &mut self,
        image_id: ImageId,
        can_rescale: bool,
    ) -> bool {
        if !self.base_image_exists(image_id) {
            return false;
        }
        let is_rescaled = self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::RESCALED);
        if can_rescale {
            is_rescaled || self.scale_up_image(image_id, false)
        } else {
            !is_rescaled || self.scale_down_image(image_id, false)
        }
    }

    fn register_completed_backend_image(&mut self, image_id: ImageId) {
        if !self.base_image_exists(image_id) {
            return;
        }
        if !self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::REGISTERED)
        {
            self.base.register_image(image_id);
        }
        self.base.register_image_alloc(image_id);
    }

    fn ensure_image_rescale_state(
        &mut self,
        image_id: ImageId,
        should_rescale: bool,
        ignore_copy: bool,
    ) -> bool {
        if !self.base_image_exists(image_id) {
            return false;
        }
        let is_rescaled = self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::RESCALED);
        if should_rescale {
            is_rescaled || self.scale_up_image(image_id, ignore_copy)
        } else {
            !is_rescaled || self.scale_down_image(image_id, ignore_copy)
        }
    }

    /// Vulkan-backed port of upstream `TextureCache<P>::SynchronizeAliases`.
    fn synchronize_aliases(&mut self, image_id: ImageId) -> bool {
        if !self.base_image_exists(image_id) {
            return false;
        }

        let image_snapshot = self.base.slot_images[image_id].clone();
        let mut aliases = image_snapshot
            .aliased_images
            .iter()
            .filter_map(|alias| {
                if !self.base_image_exists(alias.id) {
                    return None;
                }
                let alias_image = &self.base.slot_images[alias.id];
                (image_snapshot.modification_tick < alias_image.modification_tick)
                    .then(|| alias.clone())
            })
            .collect::<Vec<_>>();
        if aliases.is_empty() {
            return true;
        }

        let mut most_recent_tick = image_snapshot.modification_tick;
        let mut any_modified = image_snapshot.flags.contains(ImageFlagBits::GPU_MODIFIED);
        let mut any_rescaled = image_snapshot.flags.contains(ImageFlagBits::RESCALED);
        for alias in &aliases {
            let alias_image = &self.base.slot_images[alias.id];
            most_recent_tick = most_recent_tick.max(alias_image.modification_tick);
            any_modified |= alias_image.flags.contains(ImageFlagBits::GPU_MODIFIED);
            any_rescaled |= alias_image.flags.contains(ImageFlagBits::RESCALED);
        }

        let can_rescale = Self::image_can_rescale_base(&mut self.base, image_id);
        if any_rescaled && !self.ensure_image_rescale_state(image_id, can_rescale, false) {
            return false;
        }

        {
            let image = &mut self.base.slot_images[image_id];
            image.modification_tick = most_recent_tick;
            if any_modified {
                image.flags.insert(ImageFlagBits::GPU_MODIFIED);
            }
        }

        aliases.sort_by_key(|alias| self.base.slot_images[alias.id].modification_tick);
        let resolution_active = common::settings::values().resolution_info.active;
        for alias in aliases {
            if resolution_active
                && any_rescaled
                && !self.ensure_image_rescale_state(alias.id, can_rescale, false)
            {
                return false;
            }
            if !self.copy_join_image(image_id, alias.id, &alias.copies) {
                return false;
            }
        }
        true
    }

    /// Vulkan-backed port of upstream `TextureCache<P>::PrepareImage`.
    pub fn prepare_image_with_reader(
        &mut self,
        image_id: ImageId,
        is_modification: bool,
        invalidate: bool,
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
    ) -> bool {
        if !self.base_image_exists(image_id) {
            return false;
        }
        if invalidate {
            let image = &mut self.base.slot_images[image_id];
            image
                .flags
                .remove(ImageFlagBits::CPU_MODIFIED | ImageFlagBits::GPU_MODIFIED);
            if !image.flags.contains(ImageFlagBits::TRACKED) {
                self.base.track_image(image_id);
            }
        } else {
            if !self.refresh_contents_with_reader(image_id, read_gpu_unsafe) {
                return false;
            }
            if !self.synchronize_aliases(image_id) {
                return false;
            }
        }
        if is_modification {
            self.base.mark_modification_by_id(image_id);
        }
        self.base.touch_image(image_id);
        true
    }

    fn find_or_insert_image_from_info_with_options_and_finish(
        &mut self,
        info: &ImageInfo,
        gpu_addr: u64,
        cpu_addr: u64,
        options: RelaxedOptions,
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
    ) -> ImageId {
        let result = self
            .base
            .find_or_insert_image_from_info_with_options_result(info, gpu_addr, cpu_addr, options);
        debug_assert!(!result.needs_backend_completion || result.inserted);
        debug_assert!(!result.queued_join_tail || result.inserted);
        if result.queued_join_tail {
            self.finish_pending_join_copies_with_reader(read_gpu_unsafe);
        } else if result.needs_backend_completion && self.base_image_exists(result.image_id) {
            self.finish_pending_backend_insertion_with_reader(result.image_id, read_gpu_unsafe);
        }
        result.image_id
    }

    fn blit_framebuffer_from_image_view(
        &mut self,
        view_id: ImageViewId,
    ) -> Option<BlitFramebufferInfo> {
        if !view_id.is_valid() || view_id == NULL_IMAGE_VIEW_ID {
            return None;
        }
        let view_base = self.base.slot_image_views[view_id].clone();
        let image_id = view_base.image_id;
        if !self.base_image_exists(image_id) {
            return None;
        }
        let image_base = self.base.slot_images[image_id].clone();
        let aspect = image_view_aspect_mask(&view_base);
        let format = self.runtime.surface_format(view_base.format);
        if aspect.is_empty()
            || self
                .ensure_image(image_id, &image_base, format, aspect)
                .is_err()
        {
            return None;
        }
        self.ensure_image_view(view_id).ok()?;

        let is_rescaled = self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::RESCALED);
        let mut extent = view_base.size;
        if is_rescaled {
            let resolution = common::settings::values().resolution_info.clone();
            extent.width = resolution.scale_up_i32(extent.width as i32) as u32;
            if image_base.info.image_type == ImageType::E2D {
                extent.height = resolution.scale_up_i32(extent.height as i32) as u32;
            }
        }
        let (samples_x, samples_y) =
            crate::texture_cache::samples_helper::samples_log2(image_base.info.num_samples as i32);
        let fb_extent = vk::Extent2D {
            width: (extent.width >> samples_x).max(1),
            height: (extent.height >> samples_y).max(1),
        };
        let is_color =
            crate::surface::get_format_type(view_base.format) == SurfaceType::ColorTexture;
        let mut key = RenderTargets {
            size: Extent2D {
                width: fb_extent.width,
                height: fb_extent.height,
            },
            is_rescaled,
            ..RenderTargets::default()
        };
        if is_color {
            key.color_buffer_ids[0] = view_id;
        } else {
            key.depth_buffer_id = view_id;
        }

        if !self.framebuffers_by_render_targets.contains_key(&key) {
            let view = self.image_views.get(&view_id)?;
            let view_handle = view.render_target();
            let mut rp_key = RenderPassKey::default();
            let color_views;
            let depth_view;
            if is_color {
                rp_key.color_formats[0] = format;
                rp_key.num_color_attachments = 1;
                color_views = vec![view_handle];
                depth_view = None;
            } else {
                rp_key.depth_format = format;
                color_views = Vec::new();
                depth_view = Some(view_handle);
            }
            rp_key.samples = vk::SampleCountFlags::TYPE_1;
            let render_pass = self.runtime.render_pass_cache().get(&rp_key).ok()?;
            let framebuffer = self
                .runtime
                .create_framebuffer(
                    render_pass,
                    &{
                        let mut attachments = color_views.clone();
                        if let Some(depth) = depth_view {
                            attachments.push(depth);
                        }
                        attachments
                    },
                    fb_extent,
                )
                .ok()?;
            let mut images = [vk::Image::null(); NUM_RT + 1];
            let mut image_ranges = [vk::ImageSubresourceRange::default(); NUM_RT + 1];
            images[0] = self.images.get(&image_id)?.handle();
            image_ranges[0] = make_subresource_range(aspect, view_base.range, view_base.flags);
            let owner = Framebuffer {
                framebuffer,
                render_pass,
                render_area: fb_extent,
                num_color_buffers: if is_color { 1 } else { 0 },
                has_depth: !is_color,
                has_stencil: aspect.contains(vk::ImageAspectFlags::STENCIL),
                is_rescaled,
                samples: vk::SampleCountFlags::TYPE_1,
                rt_map: if is_color {
                    let mut map = [u8::MAX; NUM_RT];
                    map[0] = 0;
                    map
                } else {
                    [u8::MAX; NUM_RT]
                },
                images,
                image_ranges,
                num_images: 1,
                image_ids: vec![image_id],
                rt0_cpu_addr: image_base.cpu_addr,
            };
            self.framebuffers_by_render_targets.insert(key, owner);
        }

        let fb = self.framebuffers_by_render_targets.get(&key)?;
        Some(BlitFramebufferInfo {
            framebuffer: fb.framebuffer,
            render_pass: fb.render_pass,
            render_area: fb.render_area,
            images: fb.images,
            image_ranges: fb.image_ranges,
            num_images: fb.num_images,
        })
    }

    fn conversion_image_view_from_image_view(
        &mut self,
        view_id: ImageViewId,
    ) -> Option<ConversionImageView> {
        if !view_id.is_valid() || view_id == NULL_IMAGE_VIEW_ID {
            return None;
        }
        self.ensure_image_view(view_id).ok()?;
        let color_view = self.image_views.get(&view_id)?.handle(TextureType::Color2D);
        let depth_view = self
            .image_view_depth_view(view_id)
            .unwrap_or(vk::ImageView::null());
        let stencil_view = self
            .image_view_stencil_view(view_id)
            .unwrap_or(vk::ImageView::null());
        let view = self.image_views.get(&view_id)?;
        let is_rescaled = self
            .base
            .slot_images
            .get(view.base.image_id)
            .flags
            .contains(ImageFlagBits::RESCALED);
        Some(ConversionImageView {
            color_view,
            depth_view,
            stencil_view,
            size: super::blit_image::Extent3D {
                width: view.base.size.width,
                height: view.base.size.height,
                depth: view.base.size.depth,
            },
            is_rescaled,
        })
    }

    /// Vulkan-backed port of upstream `TextureCache<P>::BlitImage`.
    pub fn blit_image(
        &mut self,
        dst: &crate::engines::fermi_2d::Surface,
        src: &crate::engines::fermi_2d::Surface,
        copy: &crate::engines::fermi_2d::Config,
        mut gpu_to_cpu: impl FnMut(u64) -> Option<u64>,
        read_gpu_unsafe: impl Fn(u64, &mut [u8]) -> bool,
    ) -> bool {
        let dst_addr = dst.address();
        let src_addr = src.address();
        let mut dst_info = ImageInfo::from_fermi2d_surface(dst);
        let mut src_info = ImageInfo::from_fermi2d_surface(src);
        let can_be_depth_blit = dst_info.format == src_info.format
            && copy.filter == crate::engines::fermi_2d::Filter::Point;
        let try_options = if can_be_depth_blit {
            RelaxedOptions::SAMPLES | RelaxedOptions::FORMAT
        } else {
            RelaxedOptions::SAMPLES
        };

        let Some(src_cpu_addr) = gpu_to_cpu(src_addr) else {
            return false;
        };
        let Some(dst_cpu_addr) = gpu_to_cpu(dst_addr) else {
            return false;
        };

        let mut src_id;
        let mut dst_id;
        loop {
            self.base.has_deleted_images = false;
            src_id = self.base.find_image_in_cpu_region_with_caps(
                &src_info,
                src_addr,
                src_cpu_addr,
                try_options,
                self.base.has_broken_texture_view_formats,
                self.base.has_native_bgr,
            );
            dst_id = self.base.find_image_in_cpu_region_with_caps(
                &dst_info,
                dst_addr,
                dst_cpu_addr,
                try_options,
                self.base.has_broken_texture_view_formats,
                self.base.has_native_bgr,
            );
            if !copy.must_accelerate {
                let src_gpu_modified = src_id
                    .map(|id| {
                        self.base.slot_images[id]
                            .flags
                            .contains(ImageFlagBits::GPU_MODIFIED)
                    })
                    .unwrap_or(false);
                let dst_gpu_modified = dst_id
                    .map(|id| {
                        self.base.slot_images[id]
                            .flags
                            .contains(ImageFlagBits::GPU_MODIFIED)
                    })
                    .unwrap_or(false);
                if src_id.is_none() && dst_id.is_none() {
                    return false;
                }
                if !src_gpu_modified && !dst_gpu_modified {
                    return false;
                }
            }

            let src_image = src_id.map(|id| &self.base.slot_images[id]);
            if src_image.is_some_and(|image| image.info.num_samples > 1) {
                let msaa_options = RelaxedOptions::SAMPLES | RelaxedOptions::FORCE_BROKEN_VIEWS;
                src_id = Some(self.find_or_insert_image_from_info_with_options_and_finish(
                    &src_info,
                    src_addr,
                    src_cpu_addr,
                    msaa_options,
                    &read_gpu_unsafe,
                ));
                dst_id = Some(self.find_or_insert_image_from_info_with_options_and_finish(
                    &dst_info,
                    dst_addr,
                    dst_cpu_addr,
                    msaa_options,
                    &read_gpu_unsafe,
                ));
                if self.base.has_deleted_images {
                    continue;
                }
                break;
            }

            if can_be_depth_blit {
                let src_image = src_id.map(|id| &self.base.slot_images[id]);
                let dst_image = dst_id.map(|id| &self.base.slot_images[id]);
                crate::texture_cache::util::deduce_blit_images(
                    &mut dst_info,
                    &mut src_info,
                    dst_image,
                    src_image,
                );
                if crate::surface::get_format_type(dst_info.format)
                    != crate::surface::get_format_type(src_info.format)
                {
                    continue;
                }
            }

            if src_id.is_none() {
                src_id = Some(self.find_or_insert_image_from_info_with_options_and_finish(
                    &src_info,
                    src_addr,
                    src_cpu_addr,
                    RelaxedOptions::empty(),
                    &read_gpu_unsafe,
                ));
            }
            if dst_id.is_none() {
                dst_id = Some(self.find_or_insert_image_from_info_with_options_and_finish(
                    &dst_info,
                    dst_addr,
                    dst_cpu_addr,
                    RelaxedOptions::empty(),
                    &read_gpu_unsafe,
                ));
            }
            if !self.base.has_deleted_images {
                break;
            }
        }

        let mut src_id = src_id.unwrap_or(NULL_IMAGE_ID);
        let mut dst_id = dst_id.unwrap_or(NULL_IMAGE_ID);
        if !src_id.is_valid() || !dst_id.is_valid() {
            return false;
        }

        self.finish_pending_join_copies_with_reader(&read_gpu_unsafe);
        if !self.base_image_exists(src_id) || !self.base_image_exists(dst_id) {
            return false;
        }

        let native_bgr = self.base.has_native_bgr;
        if crate::surface::get_format_type(dst_info.format)
            != crate::surface::get_format_type(self.base.slot_images[dst_id].info.format)
            || crate::surface::get_format_type(src_info.format)
                != crate::surface::get_format_type(self.base.slot_images[src_id].info.format)
            || !crate::surface::is_view_compatible(
                dst_info.format,
                self.base.slot_images[dst_id].info.format,
                false,
                native_bgr,
            )
            || !crate::surface::is_view_compatible(
                src_info.format,
                self.base.slot_images[src_id].info.format,
                false,
                native_bgr,
            )
        {
            loop {
                self.base.has_deleted_images = false;
                src_id = self.find_or_insert_image_from_info_with_options_and_finish(
                    &src_info,
                    src_addr,
                    src_cpu_addr,
                    RelaxedOptions::empty(),
                    &read_gpu_unsafe,
                );
                dst_id = self.find_or_insert_image_from_info_with_options_and_finish(
                    &dst_info,
                    dst_addr,
                    dst_cpu_addr,
                    RelaxedOptions::empty(),
                    &read_gpu_unsafe,
                );
                if !self.base.has_deleted_images {
                    break;
                }
            }
            self.finish_pending_join_copies_with_reader(&read_gpu_unsafe);
            if !self.base_image_exists(src_id) || !self.base_image_exists(dst_id) {
                return false;
            }
        }

        if !self.prepare_image_with_reader(src_id, false, false, &read_gpu_unsafe)
            || !self.prepare_image_with_reader(dst_id, true, false, &read_gpu_unsafe)
        {
            return false;
        }

        let mut is_src_rescaled = self.base.slot_images[src_id]
            .flags
            .contains(ImageFlagBits::RESCALED);
        let mut is_dst_rescaled = self.base.slot_images[dst_id]
            .flags
            .contains(ImageFlagBits::RESCALED);
        let is_resolve = self.base.slot_images[src_id].info.num_samples != 1
            && self.base.slot_images[dst_id].info.num_samples == 1;
        if is_src_rescaled != is_dst_rescaled {
            if Self::image_can_rescale_base(&mut self.base, src_id) {
                self.ensure_image_rescale_state(src_id, true, false);
                is_src_rescaled = self.base.slot_images[src_id]
                    .flags
                    .contains(ImageFlagBits::RESCALED);
                if is_resolve {
                    self.base.slot_images[dst_id].info.rescaleable = true;
                    let aliases = self.base.slot_images[dst_id].aliased_images.clone();
                    for alias in aliases {
                        self.base.slot_images[alias.id].info.rescaleable = true;
                    }
                }
            }
            if Self::image_can_rescale_base(&mut self.base, dst_id) {
                self.ensure_image_rescale_state(dst_id, true, false);
                is_dst_rescaled = self.base.slot_images[dst_id]
                    .flags
                    .contains(ImageFlagBits::RESCALED);
            }
        }
        if is_resolve && is_src_rescaled != is_dst_rescaled {
            self.ensure_image_rescale_state(src_id, false, false);
            self.ensure_image_rescale_state(dst_id, false, false);
            is_src_rescaled = self.base.slot_images[src_id]
                .flags
                .contains(ImageFlagBits::RESCALED);
            is_dst_rescaled = self.base.slot_images[dst_id]
                .flags
                .contains(ImageFlagBits::RESCALED);
        }

        let Some(src_base) = self.base.slot_images[src_id].try_find_base(src_addr) else {
            return false;
        };
        let src_view_id = self.base.find_or_emplace_image_view(
            src_id,
            ImageViewInfo::for_render_target(
                ImageViewType::E2D,
                src_info.format,
                SubresourceRange {
                    base: src_base,
                    extent: SubresourceExtent {
                        levels: 1,
                        layers: 1,
                    },
                },
            ),
            src_addr,
        );
        let Some(dst_base) = self.base.slot_images[dst_id].try_find_base(dst_addr) else {
            return false;
        };
        let dst_view_id = self.base.find_or_emplace_image_view(
            dst_id,
            ImageViewInfo::for_render_target(
                ImageViewType::E2D,
                dst_info.format,
                SubresourceRange {
                    base: dst_base,
                    extent: SubresourceExtent {
                        levels: 1,
                        layers: 1,
                    },
                },
            ),
            dst_addr,
        );

        self.ensure_image_view(src_view_id).ok();
        self.ensure_image_view(dst_view_id).ok();
        let dst_framebuffer = match self.blit_framebuffer_from_image_view(dst_view_id) {
            Some(framebuffer) => framebuffer,
            None => return false,
        };
        let src_view = match self.image_views.remove(&src_view_id) {
            Some(view) => view,
            None => return false,
        };
        let dst_view = match self.image_views.remove(&dst_view_id) {
            Some(view) => view,
            None => {
                self.image_views.insert(src_view_id, src_view);
                return false;
            }
        };

        let (src_samples_x, src_samples_y) = crate::texture_cache::samples_helper::samples_log2(
            self.base.slot_images[src_id].info.num_samples as i32,
        );
        let (dst_samples_x, dst_samples_y) = crate::texture_cache::samples_helper::samples_log2(
            self.base.slot_images[dst_id].info.num_samples as i32,
        );
        let mut src_region = BlitRegion2D {
            start: BlitOffset2D {
                x: copy.src_x0 >> src_samples_x,
                y: copy.src_y0 >> src_samples_y,
            },
            end: BlitOffset2D {
                x: copy.src_x1 >> src_samples_x,
                y: copy.src_y1 >> src_samples_y,
            },
        };
        let mut dst_region = BlitRegion2D {
            start: BlitOffset2D {
                x: copy.dst_x0 >> dst_samples_x,
                y: copy.dst_y0 >> dst_samples_y,
            },
            end: BlitOffset2D {
                x: copy.dst_x1 >> dst_samples_x,
                y: copy.dst_y1 >> dst_samples_y,
            },
        };
        let resolution = common::settings::values().resolution_info.clone();
        let scale_region = |region: &mut BlitRegion2D| {
            region.start.x = resolution.scale_up_i32(region.start.x);
            region.start.y = resolution.scale_up_i32(region.start.y);
            region.end.x = resolution.scale_up_i32(region.end.x);
            region.end.y = resolution.scale_up_i32(region.end.y);
        };
        if is_src_rescaled {
            scale_region(&mut src_region);
        }
        if is_dst_rescaled {
            scale_region(&mut dst_region);
        }
        let filter = match copy.filter {
            crate::engines::fermi_2d::Filter::Point => BlitFilter::PointSample,
            crate::engines::fermi_2d::Filter::Bilinear => BlitFilter::Bilinear,
        };
        let operation = match copy.operation {
            crate::engines::fermi_2d::Operation::SrcCopyAnd => BlitOperation::SrcCopyAnd,
            crate::engines::fermi_2d::Operation::RopAnd => BlitOperation::RopAnd,
            crate::engines::fermi_2d::Operation::Blend => BlitOperation::Blend,
            crate::engines::fermi_2d::Operation::SrcCopy => BlitOperation::SrcCopy,
            crate::engines::fermi_2d::Operation::Rop => BlitOperation::Rop,
            crate::engines::fermi_2d::Operation::SrcCopyPremult => BlitOperation::SrcCopyPremult,
            crate::engines::fermi_2d::Operation::BlendPremult => BlitOperation::BlendPremult,
        };
        let copied = self.runtime.blit_image(
            dst_framebuffer,
            &dst_view,
            &src_view,
            dst_region,
            src_region,
            filter,
            operation,
        );
        self.image_views.insert(src_view_id, src_view);
        self.image_views.insert(dst_view_id, dst_view);
        copied
    }

    fn refresh_contents_with_reader(
        &mut self,
        image_id: ImageId,
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
    ) -> bool {
        if !self.base_image_exists(image_id) {
            return false;
        }
        // Fast path, port of upstream `RefreshContents`' first check: nothing
        // was CPU-modified and the backend image matches — this runs for
        // every bound render target on every draw, so it must not clone
        // `ImageBase` (six `Vec` fields) or touch anything else.
        if self.image_up_to_date(image_id) {
            return true;
        }
        let image_base = self.base.slot_images[image_id].clone();
        let format = self.runtime.surface_format(image_base.info.format);
        let aspect = image_aspect_mask(image_base.info.format);
        if aspect.is_empty() {
            return false;
        }
        let recreated = match self.ensure_image(image_id, &image_base, format, aspect) {
            Ok(recreated) => recreated,
            Err(_) => return false,
        };
        // A recreated backend image starts with UNDEFINED contents even when
        // the guest data was already synced into the OLD image — skipping the
        // upload here left the fresh image blank (uninitialized-memory
        // artifacts on render targets whose format toggles during boot).
        // Force the guest re-upload whenever ensure_image rebuilt the image.
        if !recreated && !image_base.flags.contains(ImageFlagBits::CPU_MODIFIED) {
            return true;
        }
        if !self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::TRACKED)
        {
            self.base.track_image(image_id);
        }
        if image_base.info.num_samples > 1 {
            log::warn!("TextureCacheVulkan: MSAA image uploads are not implemented");
            if let Some(image) = self.images.get_mut(&image_id) {
                self.runtime.transition_image_layout(image);
            }
            self.base.slot_images.get_mut(image_id).flags &= !ImageFlagBits::CPU_MODIFIED;
            return true;
        }
        if image_base
            .flags
            .contains(ImageFlagBits::ASYNCHRONOUS_DECODE)
        {
            let mut guest = vec![0u8; image_base.guest_size_bytes as usize];
            if !read_gpu_unsafe(image_base.gpu_addr, &mut guest) {
                log::warn!(
                    "TextureCacheVulkan: async texture decode read missed gpu=0x{:X} size={}",
                    image_base.gpu_addr,
                    guest.len()
                );
                return false;
            }
            if self.queue_async_decode(image_id, &image_base, &guest) {
                self.base.slot_images.get_mut(image_id).flags &= !ImageFlagBits::CPU_MODIFIED;
                return true;
            }
            return false;
        }
        let guest_size = image_base.guest_size_bytes as usize;
        let staging_size = map_size_bytes(&image_base) as usize;
        let Some(staging) = self
            .runtime
            .upload_staging_buffer(staging_size as vk::DeviceSize)
        else {
            return false;
        };
        let mut guest = vec![0u8; guest_size];
        if !read_gpu_unsafe(image_base.gpu_addr, &mut guest) {
            log::warn!(
                "TextureCacheVulkan: texture cache upload read missed gpu=0x{:X} size={}",
                image_base.gpu_addr,
                guest.len()
            );
            return false;
        }
        let trace_upload = std::env::var_os("RUZU_TRACE_VK_TEXTURE_UPLOAD").is_some();
        if trace_upload {
            let guest_nonzero = guest.iter().filter(|&&b| b != 0).count();
            log::warn!(
                "[VK_TEXTURE_UPLOAD] start image_id={} gpu=0x{:X} cpu=0x{:X} fmt={:?} type={:?} size={}x{}x{} guest_size={} staging_size={} flags={:?} guest_nonzero={}",
                image_id.index,
                image_base.gpu_addr,
                image_base.cpu_addr,
                image_base.info.format,
                image_base.info.image_type,
                image_base.info.size.width,
                image_base.info.size.height,
                image_base.info.size.depth,
                guest_size,
                staging_size,
                image_base.flags,
                guest_nonzero,
            );
        }
        if image_base.flags.contains(ImageFlagBits::ACCELERATED_UPLOAD) {
            if staging_size < guest.len() {
                log::warn!(
                    "TextureCacheVulkan: accelerated upload staging too small id={} staging={} guest={}",
                    image_id.index,
                    staging_size,
                    guest.len()
                );
                return false;
            }
            unsafe {
                std::ptr::copy_nonoverlapping(guest.as_ptr(), staging.mapped, guest.len());
            }
            let swizzles = full_upload_swizzles(&image_base.info);
            let Some(mut image) = self.images.remove(&image_id) else {
                return false;
            };
            image.base = self.base.slot_images[image_id].clone();
            let uploaded = self
                .runtime
                .accelerate_image_upload(&mut image, staging, &swizzles);
            if trace_upload {
                log::warn!(
                    "[VK_TEXTURE_UPLOAD] accelerated image_id={} copies={} uploaded={}",
                    image_id.index,
                    swizzles.len(),
                    uploaded,
                );
            }
            self.base.slot_images[image_id].flags = image.base.flags;
            self.base.slot_images[image_id].has_scaled = image.base.has_scaled;
            self.images.insert(image_id, image);
            if !uploaded {
                return false;
            }
            self.runtime.insert_upload_memory_barrier();
            self.base.slot_images.get_mut(image_id).flags &= !ImageFlagBits::CPU_MODIFIED;
            return true;
        }
        let mut upload = vec![0u8; staging_size];
        let copies = if image_base.flags.contains(ImageFlagBits::CONVERTED) {
            let mut unswizzled = vec![0u8; image_base.unswizzled_size_bytes as usize];
            let mut copies = unswizzle_image(
                &(),
                image_base.gpu_addr,
                &image_base.info,
                &guest,
                &mut unswizzled,
            );
            convert_image(&unswizzled, &image_base.info, &mut upload, &mut copies);
            copies
        } else {
            unswizzle_image(
                &(),
                image_base.gpu_addr,
                &image_base.info,
                &guest,
                &mut upload,
            )
        };
        if trace_upload {
            let upload_nonzero = upload.iter().filter(|&&b| b != 0).count();
            log::warn!(
                "[VK_TEXTURE_UPLOAD] staged image_id={} copies={} upload_nonzero={}",
                image_id.index,
                copies.len(),
                upload_nonzero,
            );
        }
        unsafe {
            std::ptr::copy_nonoverlapping(upload.as_ptr(), staging.mapped, upload.len());
        }
        let Some(mut image) = self.images.remove(&image_id) else {
            return false;
        };
        image.base = self.base.slot_images[image_id].clone();
        let uploaded =
            image.upload_memory(&mut self.runtime, staging.buffer, staging.offset, &copies);
        if trace_upload {
            log::warn!(
                "[VK_TEXTURE_UPLOAD] submitted image_id={} uploaded={}",
                image_id.index,
                uploaded,
            );
        }
        self.base.slot_images[image_id].flags = image.base.flags;
        self.base.slot_images[image_id].has_scaled = image.base.has_scaled;
        self.images.insert(image_id, image);
        if !uploaded {
            return false;
        }
        self.runtime.insert_upload_memory_barrier();
        self.base.slot_images.get_mut(image_id).flags &= !ImageFlagBits::CPU_MODIFIED;
        true
    }

    /// Vulkan-backed port of upstream `TextureCache<P>::QueueAsyncDecode`.
    fn queue_async_decode(
        &mut self,
        image_id: ImageId,
        base_image: &ImageBase,
        guest: &[u8],
    ) -> bool {
        if !base_image.flags.contains(ImageFlagBits::CONVERTED) {
            log::warn!(
                "TextureCacheVulkan::queue_async_decode called for non-converted image id={}",
                image_id.index
            );
            return false;
        }
        log::info!("Queuing async texture decode");
        self.base.slot_images[image_id]
            .flags
            .insert(ImageFlagBits::IS_DECODING);

        let decode = Arc::new(AsyncDecodeContext::new(image_id));
        self.base.async_decodes.push(Arc::clone(&decode));

        let mut unswizzled = vec![0u8; base_image.unswizzled_size_bytes as usize];
        let mut copies = unswizzle_image(
            &(),
            base_image.gpu_addr,
            &base_image.info,
            guest,
            &mut unswizzled,
        );
        let out_size = map_size_bytes(base_image) as usize;
        let info = base_image.info.clone();
        self.texture_decode_worker.queue_work(move || {
            let mut decoded_data = vec![0u8; out_size];
            convert_image(&unswizzled, &info, &mut decoded_data, &mut copies);
            {
                let mut output = decode.output.lock().unwrap();
                output.decoded_data = decoded_data;
                output.copies = copies;
            }
            decode.complete.store(true, Ordering::Release);
        });
        true
    }

    /// Vulkan-backed port of upstream `TextureCache<P>::TickAsyncDecode`.
    fn tick_async_decode(&mut self) {
        let mut has_uploads = false;
        let mut index = 0;
        while index < self.base.async_decodes.len() {
            let decode = Arc::clone(&self.base.async_decodes[index]);
            if !decode.complete.load(Ordering::Acquire) {
                index += 1;
                continue;
            }

            let image_id = decode.image_id;
            let mut output = decode.output.lock().unwrap();
            let decoded_data = std::mem::take(&mut output.decoded_data);
            let copies = std::mem::take(&mut output.copies);
            drop(output);

            if self.base_image_exists(image_id) {
                let base_image = self.base.slot_images[image_id].clone();
                let format = self.runtime.surface_format(base_image.info.format);
                let aspect = image_aspect_mask(base_image.info.format);
                let upload_ok = !aspect.is_empty()
                    && self
                        .ensure_image(image_id, &base_image, format, aspect)
                        .is_ok()
                    && self.upload_decoded_async_image(
                        image_id,
                        &base_image,
                        &decoded_data,
                        &copies,
                    );
                if upload_ok {
                    self.base.slot_images[image_id]
                        .flags
                        .remove(ImageFlagBits::IS_DECODING);
                    has_uploads = true;
                }
            }
            self.base.async_decodes.remove(index);
        }
        if has_uploads {
            self.runtime.insert_upload_memory_barrier();
        }
    }

    fn upload_decoded_async_image(
        &mut self,
        image_id: ImageId,
        base_image: &ImageBase,
        decoded_data: &[u8],
        copies: &[BufferImageCopy],
    ) -> bool {
        let Some(staging) = self
            .runtime
            .upload_staging_buffer(decoded_data.len() as vk::DeviceSize)
        else {
            return false;
        };
        unsafe {
            std::ptr::copy_nonoverlapping(
                decoded_data.as_ptr(),
                staging.mapped,
                decoded_data.len(),
            );
        }
        let Some(mut image) = self.images.remove(&image_id) else {
            return false;
        };
        image.base = base_image.clone();
        let uploaded =
            image.upload_memory(&mut self.runtime, staging.buffer, staging.offset, copies);
        self.base.slot_images[image_id].flags = image.base.flags;
        self.base.slot_images[image_id].has_scaled = image.base.has_scaled;
        self.images.insert(image_id, image);
        uploaded
    }

    fn finish_pending_join_copies_with_reader(
        &mut self,
        read_gpu_unsafe: &dyn Fn(u64, &mut [u8]) -> bool,
    ) {
        let pending = std::mem::take(&mut self.base.pending_join_copies);
        if pending.is_empty() {
            return;
        }
        let trace_join_copy = std::env::var_os("RUZU_TRACE_VK_JOIN_COPY").is_some();
        if trace_join_copy {
            log::warn!("[VK_JOIN_COPY] drain pending={}", pending.len());
        }
        let mut pending = pending.into_iter();
        while let Some(mut join) = pending.next() {
            if !self.base_image_exists(join.new_image_id) {
                continue;
            }
            if trace_join_copy {
                log::warn!(
                    "[VK_JOIN_COPY] join_tail new={} copies={} left_alias={} right_alias={} bad_overlap={}",
                    join.new_image_id.index,
                    join.copies.len(),
                    join.left_aliased_ids.len(),
                    join.right_aliased_ids.len(),
                    join.bad_overlap_ids.len()
                );
            }

            let can_rescale = Self::prepare_pending_join_sibling_rescale_gate(
                &mut self.base,
                join.new_image_id,
                &join.copies,
            );
            if !self.prepare_pending_join_sibling_rescale(&join.copies, can_rescale) {
                panic!(
                    "TextureCacheVulkan: pending join sibling ScaleUp/ScaleDown failed: new_image_id={} copies={}",
                    join.new_image_id.index,
                    join.copies.len()
                );
            }

            if !self.refresh_contents_with_reader(join.new_image_id, read_gpu_unsafe) {
                panic!(
                    "TextureCacheVulkan: pending join RefreshContents failed: new_image_id={}",
                    join.new_image_id.index
                );
            }
            if !self.prepare_pending_join_new_image_rescale(join.new_image_id, can_rescale) {
                panic!(
                    "TextureCacheVulkan: pending join new image ScaleUp/ScaleDown failed: new_image_id={} copies={}",
                    join.new_image_id.index,
                    join.copies.len()
                );
            }

            if !join.alias_relations_applied {
                join.alias_indices = self.base.apply_join_relations(
                    join.new_image_id,
                    &join.right_aliased_ids,
                    &join.left_aliased_ids,
                    &join.bad_overlap_ids,
                );
                join.alias_relations_applied = true;
            }

            for copy_object in join.copies {
                let actions = make_pending_join_copy_actions(
                    &mut self.base,
                    join.new_image_id,
                    &join.alias_indices,
                    &copy_object,
                    can_rescale,
                );
                for action in actions {
                    match action {
                        PendingJoinCopyAction::Copy {
                            src_id,
                            copies,
                            modification_tick,
                        } => {
                            if trace_join_copy {
                                log::warn!(
                                    "[VK_JOIN_COPY] copy dst={} src={} copies={} can_rescale={}",
                                    join.new_image_id.index,
                                    src_id.index,
                                    copies.len(),
                                    can_rescale
                                );
                            }
                            if !self.copy_join_image(join.new_image_id, src_id, &copies) {
                                panic!(
                                    "TextureCacheVulkan: unsupported pending join copy: dst={} src={}",
                                    join.new_image_id.index, src_id.index
                                );
                            }
                            self.base.slot_images[join.new_image_id].modification_tick =
                                modification_tick;
                        }
                        PendingJoinCopyAction::DeleteOverlap { image_id } => {
                            self.delete_join_overlap_image(image_id);
                        }
                    }
                }
            }

            if self.base_image_exists(join.new_image_id) {
                self.register_completed_backend_image(join.new_image_id);
            }
        }
    }

    fn copy_join_image(&mut self, dst_id: ImageId, src_id: ImageId, copies: &[ImageCopy]) -> bool {
        if !dst_id.is_valid() || !src_id.is_valid() || copies.is_empty() {
            return true;
        }
        if !self.base_image_exists(dst_id) || !self.base_image_exists(src_id) {
            return false;
        }
        let dst_base = self.base.slot_images[dst_id].clone();
        let src_base = self.base.slot_images[src_id].clone();
        if dst_base.flags.contains(ImageFlagBits::RESCALED)
            != src_base.flags.contains(ImageFlagBits::RESCALED)
        {
            return false;
        }
        let mut copies = copies.to_vec();
        let is_rescaled = src_base.flags.contains(ImageFlagBits::RESCALED);
        if is_rescaled {
            let both_2d = src_base.info.image_type == ImageType::E2D
                && dst_base.info.image_type == ImageType::E2D;
            let resolution = common::settings::values().resolution_info.clone();
            for copy in &mut copies {
                copy.src_offset.x = resolution.scale_up_i32(copy.src_offset.x);
                copy.dst_offset.x = resolution.scale_up_i32(copy.dst_offset.x);
                copy.extent.width = resolution.scale_up_u32(copy.extent.width);
                if both_2d {
                    copy.src_offset.y = resolution.scale_up_i32(copy.src_offset.y);
                    copy.dst_offset.y = resolution.scale_up_i32(copy.dst_offset.y);
                    copy.extent.height = resolution.scale_up_u32(copy.extent.height);
                }
            }
        }
        let Some(operation) = select_join_copy_operation(
            &dst_base,
            &src_base,
            self.runtime.shader_stencil_export_supported,
        ) else {
            return false;
        };
        let dst_aspect = image_aspect_mask(dst_base.info.format);
        let src_aspect = image_aspect_mask(src_base.info.format);
        let same_format_type = matches!(
            operation,
            JoinCopyOperation::CopyImage | JoinCopyOperation::CopyImageMsaa
        );
        if dst_aspect.is_empty()
            || src_aspect.is_empty()
            || (same_format_type && dst_aspect != src_aspect)
        {
            return false;
        }
        if self
            .ensure_image(
                dst_id,
                &dst_base,
                self.runtime.surface_format(dst_base.info.format),
                dst_aspect,
            )
            .is_err()
            || self
                .ensure_image(
                    src_id,
                    &src_base,
                    self.runtime.surface_format(src_base.info.format),
                    src_aspect,
                )
                .is_err()
        {
            return false;
        }
        if operation == JoinCopyOperation::CopyImageMsaa {
            let Some(mut src) = self.images.remove(&src_id) else {
                return false;
            };
            let copied = if let Some(dst) = self.images.get_mut(&dst_id) {
                self.runtime.copy_image_msaa(dst, &mut src, &copies)
            } else {
                false
            };
            self.images.insert(src_id, src);
            return copied;
        }

        let Some(dst) = self.images.get(&dst_id) else {
            return false;
        };
        let Some(src) = self.images.get(&src_id) else {
            return false;
        };
        match operation {
            JoinCopyOperation::CopyImage => self.runtime.copy_image(dst, src, &copies),
            JoinCopyOperation::CopyImageMsaa => unreachable!("CopyImageMSAA handled above"),
            JoinCopyOperation::Reinterpret => {
                if !self.runtime.reinterpret_image(dst, src, &copies) {
                    return false;
                }
            }
            JoinCopyOperation::Convert => {
                if !self.convert_join_image(dst_id, src_id, &dst_base, &src_base, &copies) {
                    return false;
                }
            }
        }
        true
    }

    fn convert_join_image(
        &mut self,
        dst_id: ImageId,
        src_id: ImageId,
        dst_base: &ImageBase,
        src_base: &ImageBase,
        copies: &[ImageCopy],
    ) -> bool {
        for copy in copies {
            if copy.dst_subresource.num_layers != 1
                || copy.src_subresource.num_layers != 1
                || copy.src_offset != crate::texture_cache::types::Offset3D::default()
                || copy.dst_offset != crate::texture_cache::types::Offset3D::default()
            {
                return false;
            }

            let dst_range = SubresourceRange {
                base: crate::texture_cache::types::SubresourceBase {
                    level: copy.dst_subresource.base_level,
                    layer: copy.dst_subresource.base_layer,
                },
                extent: SubresourceExtent {
                    levels: 1,
                    layers: 1,
                },
            };
            let src_range = SubresourceRange {
                base: crate::texture_cache::types::SubresourceBase {
                    level: copy.src_subresource.base_level,
                    layer: copy.src_subresource.base_layer,
                },
                extent: SubresourceExtent {
                    levels: 1,
                    layers: 1,
                },
            };
            let mut dst_format = dst_base.info.format;
            if crate::surface::get_format_type(src_base.info.format) == SurfaceType::DepthStencil
                && crate::surface::get_format_type(dst_format) == SurfaceType::ColorTexture
                && crate::surface::bytes_per_block(dst_format) == 4
            {
                dst_format = PixelFormat::A8B8G8R8Unorm;
            }
            let dst_view_id = self.base.find_or_emplace_image_view(
                dst_id,
                ImageViewInfo::for_render_target(ImageViewType::E2D, dst_format, dst_range),
                dst_base.cpu_addr,
            );
            let src_view_id = self.base.find_or_emplace_image_view(
                src_id,
                ImageViewInfo::for_render_target(
                    ImageViewType::E2D,
                    src_base.info.format,
                    src_range,
                ),
                src_base.cpu_addr,
            );
            let Some(dst_framebuffer) = self.blit_framebuffer_from_image_view(dst_view_id) else {
                return false;
            };
            let Some(src_view) = self.conversion_image_view_from_image_view(src_view_id) else {
                return false;
            };
            let expected_width = self
                .base
                .slot_image_views
                .get(dst_view_id)
                .size
                .width
                .min(self.base.slot_image_views.get(src_view_id).size.width);
            let expected_height = self
                .base
                .slot_image_views
                .get(dst_view_id)
                .size
                .height
                .min(self.base.slot_image_views.get(src_view_id).size.height);
            let expected_depth = self
                .base
                .slot_image_views
                .get(dst_view_id)
                .size
                .depth
                .min(self.base.slot_image_views.get(src_view_id).size.depth);
            let mut scaled_extent = crate::texture_cache::types::Extent3D {
                width: expected_width,
                height: expected_height,
                depth: expected_depth,
            };
            if src_base.flags.contains(ImageFlagBits::RESCALED) {
                let resolution = common::settings::values().resolution_info.clone();
                scaled_extent.width = resolution.scale_up_u32(scaled_extent.width);
                scaled_extent.height = resolution.scale_up_u32(scaled_extent.height);
            }
            if copy.extent != scaled_extent {
                return false;
            }
            if !self.runtime.convert_image(
                dst_framebuffer,
                dst_format,
                src_base.info.format,
                src_view,
            ) {
                return false;
            }
        }
        true
    }

    fn delete_join_overlap_image(&mut self, image_id: ImageId) {
        if !self.base_image_exists(image_id) {
            return;
        }
        if self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::TRACKED)
        {
            self.base.untrack_image(image_id);
        }
        if self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::REGISTERED)
        {
            self.base.unregister_image(image_id);
        }
        let view_ids = self.base.slot_images[image_id].image_view_ids.clone();
        self.render_target_cpu_map
            .retain(|_, &mut target_id| target_id != image_id);
        for view_id in view_ids {
            self.remove_framebuffers_for_view(view_id);
            if let Some(view) = self.image_views.remove(&view_id) {
                self.runtime.sentence_image_view(view);
            }
        }
        if let Some(image) = self.images.remove(&image_id) {
            self.runtime.sentence_image(image);
        }
        self.base
            .delete_image_after_backend_cleanup(image_id, false);
    }

    pub fn tick_frame(&mut self, scheduler_tick: u64) {
        self.finish_pending_backend_deletions();
        self.finish_pending_backend_insertions();
        if self.runtime.can_report_memory_usage() {
            self.base
                .update_total_used_memory_from_runtime(self.runtime.get_device_memory_usage());
        }
        if self.base.total_used_memory > self.base.minimum_memory {
            let runtime = &mut self.runtime;
            let images = &mut self.images;
            self.base
                .run_garbage_collector_with_downloader(|image_id, base_image, staging| {
                    if staging.is_empty() {
                        return false;
                    }
                    let Some(image) = images.get_mut(&image_id) else {
                        return false;
                    };
                    image.base = base_image.clone();
                    let copies = full_download_copies(&base_image.info);
                    let Some(staging_buffer) =
                        runtime.download_staging_buffer(staging.len() as vk::DeviceSize, false)
                    else {
                        return false;
                    };
                    if !image.download_memory_to_staging(runtime, &staging_buffer, &copies) {
                        return false;
                    }
                    runtime.finish();
                    let mapped =
                        unsafe { std::slice::from_raw_parts(staging_buffer.mapped, staging.len()) };
                    staging.copy_from_slice(mapped);
                    true
                });
            self.finish_pending_backend_deletions();
        }
        self.base.tick_delayed_destruction_rings();
        self.tick_async_decode();
        self.runtime.tick_frame(scheduler_tick);
        self.base.tick_frame();
        for buffer in &mut self.async_buffers_death_ring {
            self.runtime.free_deferred_staging_buffer(buffer);
        }
        self.async_buffers_death_ring.clear();
    }

    pub fn prepare_render_target_for_render(&mut self, cpu_addr: u64, cmd: vk::CommandBuffer) {
        let Some(&image_id) = self.render_target_cpu_map.get(&cpu_addr) else {
            return;
        };
        self.prepare_render_target_image_for_render(image_id, cmd);
    }

    pub fn prepare_render_targets_for_render(
        &mut self,
        image_ids: &[ImageId],
        cmd: vk::CommandBuffer,
    ) {
        for &image_id in image_ids {
            self.prepare_render_target_image_for_render(image_id, cmd);
        }
    }

    pub fn barrier_feedback_loop(&mut self) {
        self.runtime.barrier_feedback_loop();
    }

    fn prepare_render_target_image_for_render(
        &mut self,
        image_id: ImageId,
        _cmd: vk::CommandBuffer,
    ) {
        let Some((image, old_layout, aspect)) = self
            .images
            .get(&image_id)
            .map(|target| (target.handle(), target.layout, target.aspect))
        else {
            return;
        };
        if old_layout == vk::ImageLayout::GENERAL {
            return;
        }
        let device = self.runtime.device().clone();
        let scheduler = self.runtime.scheduler();
        scheduler.request_outside_renderpass();
        scheduler.record(move |cmdbuf| unsafe {
            cmd_transition_layout(
                &device,
                cmdbuf,
                image,
                old_layout,
                vk::ImageLayout::GENERAL,
                aspect,
            );
        });
        if let Some(target) = self.images.get_mut(&image_id) {
            target.layout = vk::ImageLayout::GENERAL;
            target.exchange_initialization();
        }
    }

    pub fn prepare_framebuffer_for_present(&mut self, cpu_addr: u64, cmd: vk::CommandBuffer) {
        let Some(&image_id) = self.render_target_cpu_map.get(&cpu_addr) else {
            return;
        };
        let (image, old_layout) = self
            .images
            .get(&image_id)
            .map(|target| (target.handle(), target.layout))
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
            if let Some(target) = self.images.get_mut(&image_id) {
                target.layout = vk::ImageLayout::GENERAL;
                target.exchange_initialization();
            }
        }
        self.dump_present_source_if_requested(cpu_addr, image_id);
    }

    pub fn dump_image_if_requested(&mut self, draw_counter: u32, fragment_offset: u32) {
        if self.image_dumped {
            return;
        }
        // The disabled hot path is one OnceLock load; environment parsing happens once.
        let Some(config) = image_dump_config() else {
            return;
        };
        if config.draw.is_some_and(|target| target != draw_counter)
            || config
                .fragment_offset
                .is_some_and(|target| target != fragment_offset)
        {
            return;
        }
        // RUZU_DUMP_VK_IMAGE_GPU=0xADDR selects the image by guest address
        // (handy for render targets that are never bound as textures).
        let image_id = if let Some(target) = config.gpu_address {
            let found = self
                .base
                .slot_images
                .iter()
                .find(|(_, img)| {
                    img.gpu_addr == target
                        && config
                            .width
                            .is_none_or(|width| img.info.size.width == width)
                        && config
                            .height
                            .is_none_or(|height| img.info.size.height == height)
                })
                .map(|(id, _)| id);
            match found {
                Some(id) => id,
                None => return,
            }
        } else if let Some(target) = config.cpu_address {
            let found = self
                .base
                .slot_images
                .iter()
                .find(|(_, img)| {
                    img.cpu_addr == target
                        && config
                            .width
                            .is_none_or(|width| img.info.size.width == width)
                        && config
                            .height
                            .is_none_or(|height| img.info.size.height == height)
                })
                .map(|(id, _)| id);
            match found {
                Some(id) => id,
                None => return,
            }
        } else if let Some(id) = config.image_id {
            id
        } else {
            return;
        };
        self.image_dump_seen = self.image_dump_seen.saturating_add(1);
        // RUZU_DUMP_VK_IMAGE_EVERY=N: periodic numbered dumps (never one-shot),
        // so a single run can sample an image (e.g. a downsampled glow buffer)
        // across many frames and let the caller pick the frame of interest.
        if let Some(every) = config.every {
            if self.image_dump_seen % every != 0 {
                return;
            }
            if let Some((image_base, bytes)) = self.download_image_to_host_staging(image_id) {
                let bytes = decode_debug_dump_to_rgba8(&bytes, &image_base.info);
                let base = &config.path;
                let stem = base
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("vk_image")
                    .to_string();
                let numbered =
                    base.with_file_name(format!("{stem}_{:06}.ppm", self.image_dump_seen));
                if write_rgba_like_ppm(
                    &numbered,
                    &bytes,
                    image_base.info.size.width.max(1),
                    image_base.info.size.height.max(1),
                )
                .is_ok()
                {
                    log::info!(
                        "[VK_IMAGE_DUMP] periodic #{} image_id={} {}x{} format={:?} gpu=0x{:X} to {}",
                        self.image_dump_seen,
                        image_id.index,
                        image_base.info.size.width,
                        image_base.info.size.height,
                        image_base.info.format,
                        image_base.gpu_addr,
                        numbered.display()
                    );
                }
            }
            return;
        }
        if self.image_dump_seen < config.at {
            return;
        }
        let Some((image_base, bytes)) = self.download_image_to_host_staging(image_id) else {
            return;
        };
        if let Some(path) = std::env::var_os("RUZU_DUMP_VK_IMAGE_NATIVE_FRAME") {
            if let Err(err) = std::fs::write(&path, &bytes) {
                log::warn!(
                    "TextureCacheVulkan: failed to dump native image_id={} to {}: {}",
                    image_id.index,
                    std::path::Path::new(&path).display(),
                    err
                );
            }
        }
        // Debug dumps need CPU-visible RGBA-like bytes. Packed/BCn images are
        // downloaded in their native representation, so expand them here.
        let bytes = decode_debug_dump_to_rgba8(&bytes, &image_base.info);
        let path = &config.path;
        if let Err(err) = write_rgba_like_ppm(
            path,
            &bytes,
            image_base.info.size.width.max(1),
            image_base.info.size.height.max(1),
        ) {
            log::warn!(
                "TextureCacheVulkan: failed to dump image_id={} to {}: {}",
                image_id.index,
                path.display(),
                err
            );
            return;
        }
        self.image_dumped = true;
        log::info!(
            "[VK_IMAGE_DUMP] dumped image_id={} {}x{} format={:?} gpu=0x{:X} cpu=0x{:X} bytes={} to {}",
            image_id.index,
            image_base.info.size.width,
            image_base.info.size.height,
            image_base.info.format,
            image_base.gpu_addr,
            image_base.cpu_addr,
            bytes.len(),
            path.display()
        );
    }

    fn dump_present_source_if_requested(&mut self, cpu_addr: u64, image_id: ImageId) {
        if self.present_source_dumped {
            return;
        }
        let Some(path) = std::env::var_os("RUZU_DUMP_VK_PRESENT_SOURCE_FRAME") else {
            return;
        };
        self.present_source_seen = self.present_source_seen.saturating_add(1);
        static PRESENT_STARTED: std::sync::OnceLock<std::time::Instant> =
            std::sync::OnceLock::new();
        let present_started = PRESENT_STARTED.get_or_init(std::time::Instant::now);
        // RUZU_DUMP_VK_PRESENT_SOURCE_EVERY=N: periodic timeline dumps to
        // numbered files instead of a single one-shot dump.
        if let Some(every) = std::env::var("RUZU_DUMP_VK_PRESENT_SOURCE_EVERY")
            .ok()
            .and_then(|value| value.parse::<u64>().ok())
            .filter(|&every| every > 0)
        {
            if self.present_source_seen % every != 0 {
                return;
            }
            let numbered = {
                let base = std::path::PathBuf::from(&path);
                let stem = base
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("present_source")
                    .to_string();
                base.with_file_name(format!("{stem}_{:06}.ppm", self.present_source_seen))
            };
            if let Some((image_base, bytes)) = self.download_image_to_host_staging(image_id) {
                let width = image_base.info.size.width.max(1);
                let height = image_base.info.size.height.max(1);
                if write_rgba_like_ppm(&numbered, &bytes, width, height).is_ok() {
                    log::info!(
                        "[VK_PRESENT_SOURCE] periodic dump #{} addr=0x{:X} image_id={} to {}",
                        self.present_source_seen,
                        cpu_addr,
                        image_id.index,
                        numbered.display()
                    );
                }
            }
            self.dump_present_extra_image_if_requested(self.present_source_seen);
            return;
        }
        let target_present = std::env::var("RUZU_DUMP_VK_PRESENT_SOURCE_AT")
            .ok()
            .and_then(|value| value.parse::<u64>().ok());
        let target_after_ms = std::env::var("RUZU_DUMP_VK_PRESENT_SOURCE_AFTER_MS")
            .ok()
            .and_then(|value| value.parse::<u64>().ok());
        let marker = std::env::var_os("RUZU_DUMP_VK_PRESENT_SOURCE_MARKER");
        let marker_reached = marker
            .as_ref()
            .map(|path| std::path::Path::new(path).exists())
            .unwrap_or(false);
        let reached_target = target_present
            .map(|target| self.present_source_seen >= target)
            .unwrap_or(false)
            || target_after_ms
                .map(|target| present_started.elapsed().as_millis() >= u128::from(target))
                .unwrap_or(target_present.is_none() && marker.is_none())
            || marker_reached;
        if !reached_target {
            return;
        }
        let Some((image_base, bytes)) = self.download_image_to_host_staging(image_id) else {
            log::warn!(
                "[VK_PRESENT_SOURCE] dump failed addr=0x{:X} image_id={}",
                cpu_addr,
                image_id.index
            );
            self.present_source_dumped = true;
            return;
        };
        let width = image_base.info.size.width.max(1);
        let height = image_base.info.size.height.max(1);
        let path = std::path::PathBuf::from(path);
        match write_rgba_like_ppm(&path, &bytes, width, height) {
            Ok(()) => {
                log::info!(
                    "[VK_PRESENT_SOURCE] dumped source addr=0x{:X} image_id={} {}x{} format={:?} bytes={} to {}",
                    cpu_addr,
                    image_id.index,
                    width,
                    height,
                    image_base.info.format,
                    bytes.len(),
                    path.display()
                );
            }
            Err(err) => {
                log::warn!(
                    "[VK_PRESENT_SOURCE] dump write failed addr=0x{:X} image_id={} path={} err={}",
                    cpu_addr,
                    image_id.index,
                    path.display(),
                    err
                );
            }
        }
        self.dump_present_extra_image_if_requested(self.present_source_seen);
        self.present_source_dumped = true;
    }

    fn dump_present_extra_image_if_requested(&mut self, present_seen: u64) {
        let Some(path) = std::env::var_os("RUZU_DUMP_VK_PRESENT_EXTRA_FRAME") else {
            return;
        };
        let Some(target_gpu) = std::env::var("RUZU_DUMP_VK_PRESENT_EXTRA_GPU")
            .ok()
            .and_then(|value| u64::from_str_radix(value.trim_start_matches("0x"), 16).ok())
        else {
            return;
        };
        let image_ids = self
            .base
            .slot_images
            .iter()
            .filter(|(_, image)| image.gpu_addr == target_gpu)
            .map(|(id, _)| id)
            .collect::<Vec<_>>();
        if image_ids.is_empty() {
            return;
        }
        let base_path = {
            let base = std::path::PathBuf::from(path);
            if std::env::var("RUZU_DUMP_VK_PRESENT_SOURCE_EVERY")
                .ok()
                .and_then(|value| value.parse::<u64>().ok())
                .filter(|&every| every > 0)
                .is_some()
            {
                let stem = base
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("present_extra")
                    .to_string();
                base.with_file_name(format!("{stem}_{present_seen:06}.ppm"))
            } else {
                base
            }
        };
        for image_id in image_ids {
            let Some((image_base, bytes)) = self.download_image_to_host_staging(image_id) else {
                continue;
            };
            let bytes = decode_debug_dump_to_rgba8(&bytes, &image_base.info);
            let width = image_base.info.size.width.max(1);
            let height = image_base.info.size.height.max(1);
            let stem = base_path
                .file_stem()
                .and_then(|value| value.to_str())
                .unwrap_or("present_extra");
            let path = base_path.with_file_name(format!(
                "{stem}_id{}_{}x{}_{:?}.ppm",
                image_id.index, width, height, image_base.info.format
            ));
            if write_rgba_like_ppm(&path, &bytes, width, height).is_ok() {
                log::info!(
                    "[VK_PRESENT_EXTRA] dumped image_id={} gpu=0x{:X} {}x{} format={:?} to {}",
                    image_id.index,
                    image_base.gpu_addr,
                    width,
                    height,
                    image_base.info.format,
                    path.display()
                );
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

        let image_id = *self.render_target_cpu_map.get(&cpu_addr)?;
        let target = self.images.get(&image_id)?;
        Some(RenderTargetImageInfo {
            image: target.handle(),
            width: target.base.info.size.width.max(1),
            height: target.base.info.size.height.max(1),
        })
    }

    /// Port-facing subset of upstream `TextureCache<P>::TryFindFramebufferImageView`.
    pub fn try_find_framebuffer_image_view(
        &mut self,
        config: &FramebufferConfig,
        cpu_addr: u64,
    ) -> Option<FramebufferImageViewVulkan> {
        self.finish_pending_backend_deletions();
        if let Some(framebuffer_view) = self.base.try_find_framebuffer_image_view(config, cpu_addr)
        {
            self.ensure_image_view(framebuffer_view.view_id).ok()?;
            let view = self.image_views.get(&framebuffer_view.view_id)?;
            let target_image = view.image_handle();
            let image_view = view.handle(TextureType::Color2D);
            return Some(FramebufferImageViewVulkan {
                width: framebuffer_view.view.size.width,
                height: framebuffer_view.view.size.height,
                common: framebuffer_view,
                image: target_image,
                image_view,
            });
        }

        // Compatibility path while present still arrives by CPU address. The
        // owning backend identity remains ImageId/ImageViewId; this only
        // bridges display acceleration when the common CPU-range lookup cannot
        // see a freshly backend-completed render target yet.
        let image_id = *self.render_target_cpu_map.get(&cpu_addr)?;
        let (&view_id, view) = self
            .image_views
            .iter()
            .find(|(_, view)| view.base.image_id == image_id)?;
        let framebuffer_view = FramebufferImageView {
            view_id,
            view: view.base.clone(),
            scaled: self
                .images
                .get(&image_id)
                .is_some_and(|image| image.is_rescaled()),
        };
        Some(FramebufferImageViewVulkan {
            width: framebuffer_view.view.size.width,
            height: framebuffer_view.view.size.height,
            common: framebuffer_view,
            image: view.image_handle(),
            image_view: view.handle(TextureType::Color2D),
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
        self.finish_pending_backend_deletions();
        if !view_id.is_valid() || view_id == NULL_IMAGE_VIEW_ID {
            return None;
        }
        let view_base = self.base.slot_image_views.get(view_id).clone();
        if !view_base.image_id.is_valid() || view_base.image_id == NULL_IMAGE_ID {
            return None;
        }
        if self.images.contains_key(&view_base.image_id) {
            self.ensure_image_view(view_id).ok()?;
            let view = self.image_views.get(&view_id)?;
            return match texture_type {
                TextureType::Color2D | TextureType::Color2DRect => Some(view.handle(texture_type)),
                TextureType::ColorArray2D => Some(view.render_target()),
                _ => Some(view.handle(texture_type)),
            };
        }
        None
    }

    pub fn image_view_depth_view(&mut self, view_id: ImageViewId) -> Option<vk::ImageView> {
        self.finish_pending_backend_deletions();
        self.ensure_image_view(view_id).ok()?;
        if self.image_views.get(&view_id)?.depth_view != vk::ImageView::null() {
            return Some(self.image_views.get(&view_id)?.depth_view);
        }
        let image_handle = self.image_views.get(&view_id)?.image_handle();
        let format = self
            .runtime
            .surface_format(self.image_views.get(&view_id)?.base.format);
        let view = self
            .runtime
            .make_aux_image_view(
                image_handle,
                &self.image_views.get(&view_id)?.base,
                format,
                vk::ImageAspectFlags::DEPTH,
            )
            .ok()?;
        self.image_views.get_mut(&view_id)?.depth_view = view;
        Some(view)
    }

    pub fn image_view_stencil_view(&mut self, view_id: ImageViewId) -> Option<vk::ImageView> {
        self.finish_pending_backend_deletions();
        self.ensure_image_view(view_id).ok()?;
        if self.image_views.get(&view_id)?.stencil_view != vk::ImageView::null() {
            return Some(self.image_views.get(&view_id)?.stencil_view);
        }
        let image_handle = self.image_views.get(&view_id)?.image_handle();
        let format = self
            .runtime
            .surface_format(self.image_views.get(&view_id)?.base.format);
        let view = self
            .runtime
            .make_aux_image_view(
                image_handle,
                &self.image_views.get(&view_id)?.base,
                format,
                vk::ImageAspectFlags::STENCIL,
            )
            .ok()?;
        self.image_views.get_mut(&view_id)?.stencil_view = view;
        Some(view)
    }

    pub fn image_view_color_view(&mut self, view_id: ImageViewId) -> Option<vk::ImageView> {
        self.finish_pending_backend_deletions();
        self.ensure_image_view(view_id).ok()?;
        if self.image_views.get(&view_id)?.color_view != vk::ImageView::null() {
            return Some(self.image_views.get(&view_id)?.color_view);
        }
        let image_handle = self.image_views.get(&view_id)?.image_handle();
        let view = self
            .runtime
            .make_aux_image_view(
                image_handle,
                &self.image_views.get(&view_id)?.base,
                vk::Format::R8G8B8A8_UNORM,
                vk::ImageAspectFlags::COLOR,
            )
            .ok()?;
        self.image_views.get_mut(&view_id)?.color_view = view;
        Some(view)
    }

    pub fn image_view_storage_view(
        &mut self,
        view_id: ImageViewId,
        texture_type: TextureType,
        image_format: ImageFormat,
    ) -> Option<vk::ImageView> {
        self.finish_pending_backend_deletions();
        self.ensure_image_view(view_id).ok()?;
        if image_format == ImageFormat::Typeless {
            return self.image_view_handle(view_id, texture_type);
        }
        let is_signed = matches!(image_format, ImageFormat::R8Sint | ImageFormat::R16Sint);
        let index = texture_type as usize;
        let cached = if is_signed {
            self.image_views.get(&view_id)?.storage_signeds[index]
        } else {
            self.image_views.get(&view_id)?.storage_unsigneds[index]
        };
        if cached != vk::ImageView::null() {
            return Some(cached);
        }
        let image_handle = self.image_views.get(&view_id)?.image_handle();
        let format = image_format_to_vk(image_format);
        let view = self
            .runtime
            .make_aux_image_view(
                image_handle,
                &self.image_views.get(&view_id)?.base,
                format,
                vk::ImageAspectFlags::COLOR,
            )
            .ok()?;
        let target = self.image_views.get_mut(&view_id)?;
        if is_signed {
            target.storage_signeds[index] = view;
        } else {
            target.storage_unsigneds[index] = view;
        }
        Some(view)
    }

    /// Port-facing subset of upstream `TextureCache::GetSampler(id).Handle()`.
    pub fn sampler_handle(&mut self, sampler_id: SamplerId) -> Option<vk::Sampler> {
        self.finish_pending_backend_deletions();
        if !sampler_id.is_valid() {
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
        _cmd: vk::CommandBuffer,
    ) -> Option<vk::ImageView> {
        self.finish_pending_backend_deletions();
        if !view_id.is_valid() || view_id == NULL_IMAGE_VIEW_ID {
            return None;
        }

        let view_base = self.base.slot_image_views.get(view_id).clone();
        if !view_base.image_id.is_valid() || view_base.image_id == NULL_IMAGE_ID {
            return None;
        }
        let trace_b200_source = std::env::var_os("RUZU_TRACE_B200_SOURCE_LIFECYCLE").is_some()
            && self.base.slot_images[view_base.image_id].gpu_addr == 0x558FC0000;
        if trace_b200_source {
            let image = &self.base.slot_images[view_base.image_id];
            eprintln!(
                "[B200_SOURCE_MATERIALIZE_BEGIN] image_id={} view_id={} gpu=0x{:X} flags={:?}",
                view_base.image_id.index, view_id.index, image.gpu_addr, image.flags,
            );
        }
        if !self
            .finish_pending_backend_insertion_with_reader(view_base.image_id, read_gpu_unsafe)
        {
            return None;
        }
        self.finish_pending_join_copies_with_reader(read_gpu_unsafe);
        if !self.prepare_image_with_reader(
            view_base.image_id,
            false,
            false,
            read_gpu_unsafe,
        ) {
            return None;
        }
        if let Some(view) = self.image_view_handle(view_id, texture_type) {
            if trace_b200_source {
                let image_handle = self
                    .image_views
                    .get(&view_id)
                    .map(|entry| entry.image_handle());
                eprintln!(
                    "[B200_SOURCE_MATERIALIZE_HIT] image_id={} view_id={} image={:?} view=0x{:X}",
                    view_base.image_id.index,
                    view_id.index,
                    image_handle.map(|image| image.as_raw()),
                    view.as_raw(),
                );
            }
            return Some(view);
        }
        let image_base = self.base.slot_images.get(view_base.image_id).clone();
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
        let format = self.runtime.surface_format(image_base.info.format);
        let aspect = image_aspect_mask(image_base.info.format);
        if aspect.is_empty() {
            return None;
        }
        self.ensure_image(view_base.image_id, &image_base, format, aspect)
            .ok()?;
        self.ensure_image_view(view_id).ok()?;
        let view = self.image_view_handle(view_id, texture_type);
        if trace_b200_source {
            let image_handle = self
                .image_views
                .get(&view_id)
                .map(|entry| entry.image_handle());
            eprintln!(
                "[B200_SOURCE_MATERIALIZE_NEW] image_id={} view_id={} image={:?} view={:?}",
                view_base.image_id.index,
                view_id.index,
                image_handle.map(|image| image.as_raw()),
                view.map(|handle| handle.as_raw()),
            );
        }
        view
    }

    fn create_sampler_from_tsc(&self, tsc: &TscEntry) -> Result<vk::Sampler, vk::Result> {
        let mag_filter = texture_filter_from_raw(tsc.mag_filter());
        let min_filter = texture_filter_from_raw(tsc.min_filter());
        let mipmap_filter = texture_mipmap_filter_from_raw(tsc.mipmap_filter());
        let wrap_u = wrap_mode_from_raw(tsc.wrap_u());
        let wrap_v = wrap_mode_from_raw(tsc.wrap_v());
        let wrap_p = wrap_mode_from_raw(tsc.wrap_p());
        let max_anisotropy = tsc.computed_max_anisotropy().clamp(1.0, 16.0);
        let border_color = tsc.computed_border_color();
        let mut custom_border_color = vk::SamplerCustomBorderColorCreateInfoEXT::builder()
            .custom_border_color(vk::ClearColorValue {
                float32: border_color,
            })
            .format(vk::Format::UNDEFINED)
            .build();
        let mut sampler_info = vk::SamplerCreateInfo::builder()
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
            .compare_op(maxwell_to_vk::sampler::depth_compare_function(
                crate::textures::texture::DepthCompareFunc::from_raw(tsc.depth_compare_func()),
            ))
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
            .border_color(convert_border_color(border_color));
        if self.runtime.custom_border_color_supported {
            sampler_info = sampler_info
                .push_next(&mut custom_border_color)
                .border_color(vk::BorderColor::FLOAT_CUSTOM_EXT);
        }
        let sampler_info = sampler_info.build();
        unsafe { self.runtime.device().create_sampler(&sampler_info, None) }
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

        unsafe { self.runtime.device().create_framebuffer(&fb_info, None) }
    }

    fn create_framebuffer_owner(
        &self,
        render_pass: vk::RenderPass,
        color_views: &[vk::ImageView],
        colors: &[(usize, ImageViewId, vk::Format)],
        depth_view: Option<vk::ImageView>,
        extent: vk::Extent2D,
        depth_id: ImageViewId,
        rt0_cpu_addr: u64,
    ) -> Result<Framebuffer, vk::Result> {
        let mut attachments: Vec<vk::ImageView> = color_views.to_vec();
        if let Some(depth_view) = depth_view {
            attachments.push(depth_view);
        }
        let mut layers = 1u32;
        for &(_, view_id, _) in colors {
            layers = layers.max(self.image_views[&view_id].base.range.extent.layers.max(1) as u32);
        }
        if depth_view.is_some() {
            layers = layers.max(self.image_views[&depth_id].base.range.extent.layers.max(1) as u32);
        }
        let framebuffer_info = vk::FramebufferCreateInfo::builder()
            .render_pass(render_pass)
            .attachments(&attachments)
            .width(extent.width)
            .height(extent.height)
            .layers(layers)
            .build();
        let framebuffer = unsafe {
            self.runtime
                .device()
                .create_framebuffer(&framebuffer_info, None)?
        };
        let has_depth = depth_view.is_some();
        let has_stencil = has_depth
            && depth_id.is_valid()
            && depth_id != NULL_IMAGE_VIEW_ID
            && image_aspect_mask(self.base.slot_image_views[depth_id].format)
                .contains(vk::ImageAspectFlags::STENCIL);
        let mut images = [vk::Image::null(); NUM_RT + 1];
        let mut image_ranges = [vk::ImageSubresourceRange::default(); NUM_RT + 1];
        let mut rt_map = [u8::MAX; NUM_RT];
        let mut image_ids = Vec::with_capacity(colors.len() + usize::from(has_depth));
        for (attachment_index, &(rt_index, view_id, _)) in colors.iter().enumerate() {
            let view = &self.image_views[&view_id];
            rt_map[rt_index] = attachment_index as u8;
            images[attachment_index] = view.image_handle();
            image_ranges[attachment_index] = make_subresource_range(
                image_view_aspect_mask(&view.base),
                view.base.range,
                view.base.flags,
            );
            image_ids.push(view.base.image_id);
        }
        if has_depth {
            let attachment_index = colors.len();
            let view = &self.image_views[&depth_id];
            images[attachment_index] = view.image_handle();
            image_ranges[attachment_index] = make_subresource_range(
                image_view_aspect_mask(&view.base),
                view.base.range,
                view.base.flags,
            );
            image_ids.push(view.base.image_id);
        }
        Ok(Framebuffer {
            framebuffer,
            render_pass,
            render_area: extent,
            num_color_buffers: colors.len() as u32,
            has_depth,
            has_stencil,
            is_rescaled: self.base.render_targets.is_rescaled,
            samples: colors
                .first()
                .map(|(_, view_id, _)| self.image_views[view_id].samples())
                .or_else(|| depth_view.map(|_| self.image_views[&depth_id].samples()))
                .unwrap_or(vk::SampleCountFlags::TYPE_1),
            rt_map,
            images,
            image_ranges,
            num_images: image_ids.len(),
            image_ids,
            rt0_cpu_addr,
        })
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
        unsafe {
            cmd_transition_layout(
                self.runtime.device(),
                cmd,
                image,
                old_layout,
                new_layout,
                aspect,
            );
        }
    }
}

unsafe fn cmd_transition_layout(
    device: &ash::Device,
    cmd: vk::CommandBuffer,
    image: vk::Image,
    old_layout: vk::ImageLayout,
    new_layout: vk::ImageLayout,
    aspect: vk::ImageAspectFlags,
) {
        let (src_access, src_stage, dst_access, dst_stage) = match (old_layout, new_layout) {
            (vk::ImageLayout::UNDEFINED, vk::ImageLayout::GENERAL) => (
                vk::AccessFlags::empty(),
                vk::PipelineStageFlags::TOP_OF_PIPE,
                vk::AccessFlags::MEMORY_READ | vk::AccessFlags::MEMORY_WRITE,
                vk::PipelineStageFlags::ALL_COMMANDS,
            ),
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

        device.cmd_pipeline_barrier(
            cmd,
            src_stage,
            dst_stage,
            vk::DependencyFlags::empty(),
            &[],
            &[],
            &[barrier],
        );
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
        PixelFormat::X8D24Unorm => vk::Format::X8_D24_UNORM_PACK32,
        PixelFormat::S8Uint => vk::Format::S8_UINT,
        PixelFormat::D24UnormS8Uint => vk::Format::D24_UNORM_S8_UINT,
        PixelFormat::S8UintD24Unorm => vk::Format::D24_UNORM_S8_UINT,
        PixelFormat::D32FloatS8Uint => vk::Format::D32_SFLOAT_S8_UINT,
        _ => vk::Format::R8G8B8A8_UNORM,
    }
}

fn surface_format_alternatives(format: vk::Format) -> &'static [vk::Format] {
    match format {
        vk::Format::S8_UINT => &[
            vk::Format::D16_UNORM_S8_UINT,
            vk::Format::D24_UNORM_S8_UINT,
            vk::Format::D32_SFLOAT_S8_UINT,
        ],
        vk::Format::D24_UNORM_S8_UINT => &[
            vk::Format::D32_SFLOAT_S8_UINT,
            vk::Format::D16_UNORM_S8_UINT,
        ],
        vk::Format::D16_UNORM_S8_UINT => &[
            vk::Format::D24_UNORM_S8_UINT,
            vk::Format::D32_SFLOAT_S8_UINT,
        ],
        vk::Format::R5G6B5_UNORM_PACK16 => &[vk::Format::R5G6B5_UNORM_PACK16],
        vk::Format::R16G16B16_SFLOAT => &[vk::Format::R16G16B16A16_SFLOAT],
        vk::Format::R16G16B16_SSCALED => &[vk::Format::R16G16B16A16_SSCALED],
        vk::Format::R8G8B8_SSCALED => &[vk::Format::R8G8B8A8_SSCALED],
        vk::Format::R32G32B32_SFLOAT => &[vk::Format::R32G32B32A32_SFLOAT],
        vk::Format::A4B4G4R4_UNORM_PACK16_EXT => &[vk::Format::R4G4B4A4_UNORM_PACK16],
        _ => &[],
    }
}

fn bcn_transcoded_format(format: PixelFormat) -> vk::Format {
    match format {
        PixelFormat::Bc4Snorm => vk::Format::R8_SNORM,
        PixelFormat::Bc4Unorm => vk::Format::R8_UNORM,
        PixelFormat::Bc5Snorm => vk::Format::R8G8_SNORM,
        PixelFormat::Bc5Unorm => vk::Format::R8G8_UNORM,
        PixelFormat::Bc6hSfloat | PixelFormat::Bc6hUfloat => vk::Format::R16G16B16A16_SFLOAT,
        PixelFormat::Bc1RgbaSrgb
        | PixelFormat::Bc2Srgb
        | PixelFormat::Bc3Srgb
        | PixelFormat::Bc7Srgb => vk::Format::A8B8G8R8_SRGB_PACK32,
        PixelFormat::Bc1RgbaUnorm
        | PixelFormat::Bc2Unorm
        | PixelFormat::Bc3Unorm
        | PixelFormat::Bc7Unorm => vk::Format::A8B8G8R8_UNORM_PACK32,
        _ => pixel_format_to_vk(format),
    }
}

struct RangedBarrierRange {
    min_mip: u32,
    max_mip: u32,
    min_layer: u32,
    max_layer: u32,
}

impl Default for RangedBarrierRange {
    fn default() -> Self {
        Self {
            min_mip: u32::MAX,
            max_mip: u32::MIN,
            min_layer: u32::MAX,
            max_layer: u32::MIN,
        }
    }
}

impl RangedBarrierRange {
    fn add_layers(&mut self, layers: vk::ImageSubresourceLayers) {
        self.min_mip = self.min_mip.min(layers.mip_level);
        self.max_mip = self.max_mip.max(layers.mip_level + 1);
        self.min_layer = self.min_layer.min(layers.base_array_layer);
        self.max_layer = self
            .max_layer
            .max(layers.base_array_layer + layers.layer_count);
    }

    fn subresource_range(&self, aspect: vk::ImageAspectFlags) -> vk::ImageSubresourceRange {
        vk::ImageSubresourceRange {
            aspect_mask: aspect,
            base_mip_level: self.min_mip,
            level_count: self.max_mip - self.min_mip,
            base_array_layer: self.min_layer,
            layer_count: self.max_layer - self.min_layer,
        }
    }
}

fn make_image_subresource_layers(
    layers: crate::texture_cache::types::SubresourceLayers,
    aspect: vk::ImageAspectFlags,
) -> vk::ImageSubresourceLayers {
    vk::ImageSubresourceLayers {
        aspect_mask: aspect,
        mip_level: layers.base_level as u32,
        base_array_layer: layers.base_layer as u32,
        layer_count: layers.num_layers as u32,
    }
}

fn make_image_copy(copy: &ImageCopy, aspect: vk::ImageAspectFlags) -> vk::ImageCopy {
    vk::ImageCopy {
        src_subresource: make_image_subresource_layers(copy.src_subresource, aspect),
        src_offset: vk::Offset3D {
            x: copy.src_offset.x,
            y: copy.src_offset.y,
            z: copy.src_offset.z,
        },
        dst_subresource: make_image_subresource_layers(copy.dst_subresource, aspect),
        dst_offset: vk::Offset3D {
            x: copy.dst_offset.x,
            y: copy.dst_offset.y,
            z: copy.dst_offset.z,
        },
        extent: vk::Extent3D {
            width: copy.extent.width,
            height: copy.extent.height,
            depth: copy.extent.depth,
        },
    }
}

struct CopyImageBarriers {
    pre: [vk::ImageMemoryBarrier; 2],
    post: [vk::ImageMemoryBarrier; 2],
}

fn make_copy_image_barriers(
    src_image: vk::Image,
    dst_image: vk::Image,
    aspect: vk::ImageAspectFlags,
    copies: &[vk::ImageCopy],
) -> CopyImageBarriers {
    let mut dst_range = RangedBarrierRange::default();
    let mut src_range = RangedBarrierRange::default();
    for copy in copies {
        dst_range.add_layers(copy.dst_subresource);
        src_range.add_layers(copy.src_subresource);
    }
    let write_access = vk::AccessFlags::SHADER_WRITE
        | vk::AccessFlags::COLOR_ATTACHMENT_WRITE
        | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE
        | vk::AccessFlags::TRANSFER_WRITE;
    CopyImageBarriers {
        pre: [
            vk::ImageMemoryBarrier::builder()
                .src_access_mask(write_access)
                .dst_access_mask(vk::AccessFlags::TRANSFER_READ)
                .old_layout(vk::ImageLayout::GENERAL)
                .new_layout(vk::ImageLayout::TRANSFER_SRC_OPTIMAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(src_image)
                .subresource_range(src_range.subresource_range(aspect))
                .build(),
            vk::ImageMemoryBarrier::builder()
                .src_access_mask(write_access)
                .dst_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .old_layout(vk::ImageLayout::GENERAL)
                .new_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(dst_image)
                .subresource_range(dst_range.subresource_range(aspect))
                .build(),
        ],
        post: [
            vk::ImageMemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::empty())
                .dst_access_mask(vk::AccessFlags::empty())
                .old_layout(vk::ImageLayout::TRANSFER_SRC_OPTIMAL)
                .new_layout(vk::ImageLayout::GENERAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(src_image)
                .subresource_range(src_range.subresource_range(aspect))
                .build(),
            vk::ImageMemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .dst_access_mask(
                    vk::AccessFlags::SHADER_READ
                        | vk::AccessFlags::SHADER_WRITE
                        | vk::AccessFlags::COLOR_ATTACHMENT_READ
                        | vk::AccessFlags::COLOR_ATTACHMENT_WRITE
                        | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_READ
                        | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE
                        | vk::AccessFlags::TRANSFER_READ
                        | vk::AccessFlags::TRANSFER_WRITE,
                )
                .old_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .new_layout(vk::ImageLayout::GENERAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(dst_image)
                .subresource_range(dst_range.subresource_range(aspect))
                .build(),
        ],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::texture_cache::types::{Extent3D, Offset3D, SubresourceExtent, SubresourceLayers};
    use ash::vk::Handle;

    #[test]
    fn sampled_view_prepares_image_before_resolving_backend_view() {
        let source = include_str!("texture_cache.rs");
        let function = source
            .split("pub fn materialize_sampled_image_view")
            .nth(1)
            .expect("materialize_sampled_image_view must exist")
            .split("fn create_sampler_from_tsc")
            .next()
            .expect("materialize_sampled_image_view must precede sampler creation");
        let prepare = function
            .find("prepare_image_with_reader")
            .expect("sampled images must follow upstream PrepareImageView");
        let resolve = function
            .find("image_view_handle")
            .expect("sampled image view must be resolved");

        assert!(prepare < resolve, "PrepareImageView must precede Handle");
    }

    #[test]
    fn render_target_framebuffer_preserves_sparse_rt_slot_map() {
        let color_range = vk::ImageSubresourceRange {
            aspect_mask: vk::ImageAspectFlags::COLOR,
            base_mip_level: 2,
            level_count: 1,
            base_array_layer: 3,
            layer_count: 1,
        };
        let framebuffer = RenderTargetFramebuffer {
            framebuffer: vk::Framebuffer::null(),
            render_pass: vk::RenderPass::null(),
            cpu_addr: 0,
            extent: vk::Extent2D {
                width: 1280,
                height: 720,
            },
            num_color: 2,
            has_depth: false,
            has_stencil: false,
            images: vec![vk::Image::from_raw(1), vk::Image::from_raw(2)],
            image_ranges: vec![color_range, color_range],
            image_ids: vec![ImageId { index: 1 }, ImageId { index: 2 }],
            rt_map: [u8::MAX, 0, u8::MAX, u8::MAX, 1, u8::MAX, u8::MAX, u8::MAX],
        };

        assert!(!framebuffer.has_aspect_color_bit(0));
        assert!(framebuffer.has_aspect_color_bit(1));
        assert!(!framebuffer.has_aspect_color_bit(2));
        assert!(framebuffer.has_aspect_color_bit(4));
        assert!(!framebuffer.has_aspect_color_bit(NUM_RT));
    }

    #[test]
    fn convert_border_color_matches_upstream_fallback() {
        assert_eq!(
            convert_border_color([0.0, 0.0, 0.0, 0.0]),
            vk::BorderColor::FLOAT_TRANSPARENT_BLACK
        );
        assert_eq!(
            convert_border_color([0.0, 0.0, 0.0, 1.0]),
            vk::BorderColor::FLOAT_OPAQUE_BLACK
        );
        assert_eq!(
            convert_border_color([1.0, 1.0, 1.0, 1.0]),
            vk::BorderColor::FLOAT_OPAQUE_WHITE
        );
        assert_eq!(
            convert_border_color([0.46, 0.45, 0.45, 0.0]),
            vk::BorderColor::FLOAT_OPAQUE_WHITE
        );
        assert_eq!(
            convert_border_color([0.1, 0.2, 0.3, 0.6]),
            vk::BorderColor::FLOAT_OPAQUE_BLACK
        );
        assert_eq!(
            convert_border_color([0.1, 0.2, 0.3, 0.4]),
            vk::BorderColor::FLOAT_TRANSPARENT_BLACK
        );
    }

    #[test]
    fn image_usage_flags_use_resolved_format_info_storage_bit() {
        let mut format_info = maxwell_to_vk::surface_format(PixelFormat::A2B10G10R10Unorm);
        assert!(format_info.storage);
        assert!(
            image_usage_flags(format_info, PixelFormat::A2B10G10R10Unorm)
                .contains(vk::ImageUsageFlags::STORAGE)
        );

        format_info.storage = false;
        assert!(
            !image_usage_flags(format_info, PixelFormat::A2B10G10R10Unorm)
                .contains(vk::ImageUsageFlags::STORAGE)
        );
    }

    #[test]
    fn decode_b10g11r11_handles_channel_widths_and_hdr_values() {
        let one_r = 15u32 << 6;
        let half_g = (14u32 << 6) << 11;
        let two_b = (16u32 << 5) << 22;
        let decoded = decode_b10g11r11_to_rgba8(&(one_r | half_g | two_b).to_le_bytes());

        // Reinhard mapping: 1 -> 0.5, 0.5 -> 1/3, 2 -> 2/3.
        assert_eq!(decoded, [128, 85, 170, 255]);
        assert_eq!(
            decode_b10g11r11_to_rgba8(&0u32.to_le_bytes()),
            [0, 0, 0, 255]
        );
    }

    #[test]
    fn ranged_barrier_range_matches_upstream_min_max_layers() {
        let mut range = RangedBarrierRange::default();

        range.add_layers(vk::ImageSubresourceLayers {
            aspect_mask: vk::ImageAspectFlags::COLOR,
            mip_level: 4,
            base_array_layer: 6,
            layer_count: 2,
        });
        range.add_layers(vk::ImageSubresourceLayers {
            aspect_mask: vk::ImageAspectFlags::COLOR,
            mip_level: 2,
            base_array_layer: 3,
            layer_count: 4,
        });

        let vk_range = range.subresource_range(vk::ImageAspectFlags::COLOR);
        assert_eq!(vk_range.aspect_mask, vk::ImageAspectFlags::COLOR);
        assert_eq!(vk_range.base_mip_level, 2);
        assert_eq!(vk_range.level_count, 3);
        assert_eq!(vk_range.base_array_layer, 3);
        assert_eq!(vk_range.layer_count, 5);
    }

    #[test]
    fn make_image_copy_matches_upstream_field_mapping() {
        let copy = ImageCopy {
            src_subresource: SubresourceLayers {
                base_level: 1,
                base_layer: 2,
                num_layers: 3,
            },
            dst_subresource: SubresourceLayers {
                base_level: 4,
                base_layer: 5,
                num_layers: 6,
            },
            src_offset: Offset3D { x: -1, y: 2, z: 3 },
            dst_offset: Offset3D { x: 4, y: -5, z: 6 },
            extent: Extent3D {
                width: 7,
                height: 8,
                depth: 9,
            },
        };

        let vk_copy = make_image_copy(&copy, vk::ImageAspectFlags::DEPTH);

        assert_eq!(
            vk_copy.src_subresource.aspect_mask,
            vk::ImageAspectFlags::DEPTH
        );
        assert_eq!(vk_copy.src_subresource.mip_level, 1);
        assert_eq!(vk_copy.src_subresource.base_array_layer, 2);
        assert_eq!(vk_copy.src_subresource.layer_count, 3);
        assert_eq!(vk_copy.src_offset, vk::Offset3D { x: -1, y: 2, z: 3 });
        assert_eq!(
            vk_copy.dst_subresource.aspect_mask,
            vk::ImageAspectFlags::DEPTH
        );
        assert_eq!(vk_copy.dst_subresource.mip_level, 4);
        assert_eq!(vk_copy.dst_subresource.base_array_layer, 5);
        assert_eq!(vk_copy.dst_subresource.layer_count, 6);
        assert_eq!(vk_copy.dst_offset, vk::Offset3D { x: 4, y: -5, z: 6 });
        assert_eq!(
            vk_copy.extent,
            vk::Extent3D {
                width: 7,
                height: 8,
                depth: 9,
            }
        );
    }

    #[test]
    fn copy_image_barriers_match_upstream_access_layout_and_ranges() {
        let src_image = vk::Image::from_raw(0x1000);
        let dst_image = vk::Image::from_raw(0x2000);
        let copies = [
            vk::ImageCopy {
                src_subresource: vk::ImageSubresourceLayers {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    mip_level: 3,
                    base_array_layer: 5,
                    layer_count: 2,
                },
                dst_subresource: vk::ImageSubresourceLayers {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    mip_level: 1,
                    base_array_layer: 4,
                    layer_count: 3,
                },
                src_offset: vk::Offset3D::default(),
                dst_offset: vk::Offset3D::default(),
                extent: vk::Extent3D {
                    width: 16,
                    height: 16,
                    depth: 1,
                },
            },
            vk::ImageCopy {
                src_subresource: vk::ImageSubresourceLayers {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    mip_level: 1,
                    base_array_layer: 2,
                    layer_count: 1,
                },
                dst_subresource: vk::ImageSubresourceLayers {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    mip_level: 4,
                    base_array_layer: 1,
                    layer_count: 2,
                },
                src_offset: vk::Offset3D::default(),
                dst_offset: vk::Offset3D::default(),
                extent: vk::Extent3D {
                    width: 8,
                    height: 8,
                    depth: 1,
                },
            },
        ];

        let barriers =
            make_copy_image_barriers(src_image, dst_image, vk::ImageAspectFlags::COLOR, &copies);
        let write_access = vk::AccessFlags::SHADER_WRITE
            | vk::AccessFlags::COLOR_ATTACHMENT_WRITE
            | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE
            | vk::AccessFlags::TRANSFER_WRITE;
        let final_dst_access = vk::AccessFlags::SHADER_READ
            | vk::AccessFlags::SHADER_WRITE
            | vk::AccessFlags::COLOR_ATTACHMENT_READ
            | vk::AccessFlags::COLOR_ATTACHMENT_WRITE
            | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_READ
            | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE
            | vk::AccessFlags::TRANSFER_READ
            | vk::AccessFlags::TRANSFER_WRITE;

        assert_eq!(barriers.pre[0].src_access_mask, write_access);
        assert_eq!(
            barriers.pre[0].dst_access_mask,
            vk::AccessFlags::TRANSFER_READ
        );
        assert_eq!(barriers.pre[0].old_layout, vk::ImageLayout::GENERAL);
        assert_eq!(
            barriers.pre[0].new_layout,
            vk::ImageLayout::TRANSFER_SRC_OPTIMAL
        );
        assert_eq!(barriers.pre[0].image, src_image);
        assert_eq!(barriers.pre[0].subresource_range.base_mip_level, 1);
        assert_eq!(barriers.pre[0].subresource_range.level_count, 3);
        assert_eq!(barriers.pre[0].subresource_range.base_array_layer, 2);
        assert_eq!(barriers.pre[0].subresource_range.layer_count, 5);

        assert_eq!(barriers.pre[1].src_access_mask, write_access);
        assert_eq!(
            barriers.pre[1].dst_access_mask,
            vk::AccessFlags::TRANSFER_WRITE
        );
        assert_eq!(barriers.pre[1].old_layout, vk::ImageLayout::GENERAL);
        assert_eq!(
            barriers.pre[1].new_layout,
            vk::ImageLayout::TRANSFER_DST_OPTIMAL
        );
        assert_eq!(barriers.pre[1].image, dst_image);
        assert_eq!(barriers.pre[1].subresource_range.base_mip_level, 1);
        assert_eq!(barriers.pre[1].subresource_range.level_count, 4);
        assert_eq!(barriers.pre[1].subresource_range.base_array_layer, 1);
        assert_eq!(barriers.pre[1].subresource_range.layer_count, 6);

        assert_eq!(barriers.post[0].src_access_mask, vk::AccessFlags::empty());
        assert_eq!(barriers.post[0].dst_access_mask, vk::AccessFlags::empty());
        assert_eq!(
            barriers.post[0].old_layout,
            vk::ImageLayout::TRANSFER_SRC_OPTIMAL
        );
        assert_eq!(barriers.post[0].new_layout, vk::ImageLayout::GENERAL);
        assert_eq!(barriers.post[0].image, src_image);
        assert_eq!(
            barriers.post[0].subresource_range.aspect_mask,
            barriers.pre[0].subresource_range.aspect_mask
        );
        assert_eq!(
            barriers.post[0].subresource_range.base_mip_level,
            barriers.pre[0].subresource_range.base_mip_level
        );
        assert_eq!(
            barriers.post[0].subresource_range.level_count,
            barriers.pre[0].subresource_range.level_count
        );
        assert_eq!(
            barriers.post[0].subresource_range.base_array_layer,
            barriers.pre[0].subresource_range.base_array_layer
        );
        assert_eq!(
            barriers.post[0].subresource_range.layer_count,
            barriers.pre[0].subresource_range.layer_count
        );

        assert_eq!(
            barriers.post[1].src_access_mask,
            vk::AccessFlags::TRANSFER_WRITE
        );
        assert_eq!(barriers.post[1].dst_access_mask, final_dst_access);
        assert_eq!(
            barriers.post[1].old_layout,
            vk::ImageLayout::TRANSFER_DST_OPTIMAL
        );
        assert_eq!(barriers.post[1].new_layout, vk::ImageLayout::GENERAL);
        assert_eq!(barriers.post[1].image, dst_image);
        assert_eq!(
            barriers.post[1].subresource_range.aspect_mask,
            barriers.pre[1].subresource_range.aspect_mask
        );
        assert_eq!(
            barriers.post[1].subresource_range.base_mip_level,
            barriers.pre[1].subresource_range.base_mip_level
        );
        assert_eq!(
            barriers.post[1].subresource_range.level_count,
            barriers.pre[1].subresource_range.level_count
        );
        assert_eq!(
            barriers.post[1].subresource_range.base_array_layer,
            barriers.pre[1].subresource_range.base_array_layer
        );
        assert_eq!(
            barriers.post[1].subresource_range.layer_count,
            barriers.pre[1].subresource_range.layer_count
        );
    }

    #[test]
    fn pending_join_shrink_copy_selects_runtime_copy_image() {
        let mut full = ImageInfo {
            format: PixelFormat::A8B8G8R8Unorm,
            image_type: ImageType::E2D,
            resources: SubresourceExtent {
                levels: 2,
                layers: 1,
            },
            size: Extent3D {
                width: 64,
                height: 64,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        full.layer_stride = crate::texture_cache::util::calculate_layer_stride(&full);
        full.maybe_unaligned_layer_stride = crate::texture_cache::util::calculate_layer_size(&full);
        let sub = ImageInfo {
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: 32,
                height: 32,
                depth: 1,
            },
            ..full.clone()
        };

        let full_base = ImageBase::new(full.clone(), 0x5000, 0x9000);
        let mip_offset = full_base.mip_level_offsets[1] as u64;
        let sub_base = ImageBase::new(sub.clone(), 0x5000 + mip_offset, 0x9000 + mip_offset);
        let base = full_base
            .try_find_base(sub_base.gpu_addr)
            .expect("mip-sized overlap must map into the full image");
        let copies = make_shrink_image_copies(&full, &sub, base, 1, 0);

        assert!(!copies.is_empty());
        assert_eq!(
            select_join_copy_operation(&full_base, &sub_base, true),
            Some(JoinCopyOperation::CopyImage)
        );
    }

    #[test]
    fn pending_join_non_alias_gpu_modified_actions_copy_then_delete() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = CommonTextureCache::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut full = ImageInfo {
            format: PixelFormat::A8B8G8R8Unorm,
            image_type: ImageType::E2D,
            resources: SubresourceExtent {
                levels: 2,
                layers: 1,
            },
            size: Extent3D {
                width: 64,
                height: 64,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        full.layer_stride = crate::texture_cache::util::calculate_layer_stride(&full);
        full.maybe_unaligned_layer_stride = crate::texture_cache::util::calculate_layer_size(&full);
        let sub = ImageInfo {
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: 32,
                height: 32,
                depth: 1,
            },
            ..full.clone()
        };

        let full_base = ImageBase::new(full.clone(), 0x5000, 0x9000);
        let mip_offset = full_base.mip_level_offsets[1] as u64;
        let sub_id = cache.join_images(&sub, 0x5000 + mip_offset, 0x9000 + mip_offset);
        cache.mark_modification_by_id(sub_id);
        let modification_tick = cache.slot_images[sub_id].modification_tick;
        cache.set_backend_completes_join_images(true);
        let full_id = cache.join_images(&full, 0x5000, 0x9000);
        let pending = cache.pending_join_copies[0].clone();

        let actions = make_pending_join_copy_actions(
            &mut cache,
            pending.new_image_id,
            &pending.alias_indices,
            &pending.copies[0],
            false,
        );

        assert_eq!(pending.new_image_id, full_id);
        assert_eq!(actions.len(), 2);
        match &actions[0] {
            PendingJoinCopyAction::Copy {
                src_id,
                copies,
                modification_tick: tick,
            } => {
                assert_eq!(*src_id, sub_id);
                assert!(!copies.is_empty());
                assert_eq!(*tick, modification_tick);
            }
            PendingJoinCopyAction::DeleteOverlap { .. } => panic!("copy must precede delete"),
        }
        match actions[1] {
            PendingJoinCopyAction::DeleteOverlap { image_id } => assert_eq!(image_id, sub_id),
            PendingJoinCopyAction::Copy { .. } => panic!("delete must follow copy"),
        }
        assert!(cache.slot_images[full_id]
            .flags
            .contains(ImageFlagBits::GPU_MODIFIED));
    }
}

fn transform_buffer_image_copies(
    copies: &[BufferImageCopy],
    base_offset: vk::DeviceSize,
    aspect: vk::ImageAspectFlags,
) -> Vec<vk::BufferImageCopy> {
    copies
        .iter()
        .map(|copy| {
            vk::BufferImageCopy::builder()
                .buffer_offset(base_offset + copy.buffer_offset as vk::DeviceSize)
                .buffer_row_length(copy.buffer_row_length)
                .buffer_image_height(copy.buffer_image_height)
                .image_subresource(vk::ImageSubresourceLayers {
                    aspect_mask: aspect,
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
        .collect()
}

fn copy_buffer_to_image(
    device: &ash::Device,
    cmd: vk::CommandBuffer,
    src_buffer: vk::Buffer,
    image: vk::Image,
    aspect_mask: vk::ImageAspectFlags,
    is_initialized: bool,
    copies: &[vk::BufferImageCopy],
) {
    let write_access = vk::AccessFlags::SHADER_WRITE
        | vk::AccessFlags::COLOR_ATTACHMENT_WRITE
        | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE;
    let read_access = vk::AccessFlags::SHADER_READ
        | vk::AccessFlags::COLOR_ATTACHMENT_READ
        | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_READ;
    let read_barrier = vk::ImageMemoryBarrier::builder()
        .src_access_mask(write_access)
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
            aspect_mask,
            base_mip_level: 0,
            level_count: vk::REMAINING_MIP_LEVELS,
            base_array_layer: 0,
            layer_count: vk::REMAINING_ARRAY_LAYERS,
        })
        .build();
    let write_barrier = vk::ImageMemoryBarrier::builder()
        .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
        .dst_access_mask(write_access | read_access)
        .old_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
        .new_layout(vk::ImageLayout::GENERAL)
        .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
        .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
        .image(image)
        .subresource_range(vk::ImageSubresourceRange {
            aspect_mask,
            base_mip_level: 0,
            level_count: vk::REMAINING_MIP_LEVELS,
            base_array_layer: 0,
            layer_count: vk::REMAINING_ARRAY_LAYERS,
        })
        .build();
    unsafe {
        device.cmd_pipeline_barrier(
            cmd,
            vk::PipelineStageFlags::ALL_COMMANDS,
            vk::PipelineStageFlags::TRANSFER,
            vk::DependencyFlags::empty(),
            &[],
            &[],
            &[read_barrier],
        );
        device.cmd_copy_buffer_to_image(
            cmd,
            src_buffer,
            image,
            vk::ImageLayout::TRANSFER_DST_OPTIMAL,
            copies,
        );
        device.cmd_pipeline_barrier(
            cmd,
            vk::PipelineStageFlags::TRANSFER,
            vk::PipelineStageFlags::ALL_COMMANDS,
            vk::DependencyFlags::empty(),
            &[],
            &[],
            &[write_barrier],
        );
    }
}

fn make_buffer_image_copy(
    copy: &ImageCopy,
    is_src: bool,
    aspect: vk::ImageAspectFlags,
) -> vk::BufferImageCopy {
    let subresource = if is_src {
        copy.src_subresource
    } else {
        copy.dst_subresource
    };
    let offset = if is_src {
        copy.src_offset
    } else {
        copy.dst_offset
    };
    vk::BufferImageCopy {
        buffer_offset: 0,
        buffer_row_length: 0,
        buffer_image_height: 0,
        image_subresource: make_image_subresource_layers(subresource, aspect),
        image_offset: vk::Offset3D {
            x: offset.x,
            y: offset.y,
            z: offset.z,
        },
        image_extent: vk::Extent3D {
            width: copy.extent.width,
            height: copy.extent.height,
            depth: copy.extent.depth,
        },
    }
}

fn make_image_subresource_layers_from_view(view: &ImageView) -> vk::ImageSubresourceLayers {
    vk::ImageSubresourceLayers {
        aspect_mask: image_aspect_mask(view.base.format),
        mip_level: view.base.range.base.level.max(0) as u32,
        base_array_layer: view.base.range.base.layer.max(0) as u32,
        layer_count: view.base.range.extent.layers.max(1) as u32,
    }
}

fn make_image_blit(
    dst_region: BlitRegion2D,
    src_region: BlitRegion2D,
    dst_layers: vk::ImageSubresourceLayers,
    src_layers: vk::ImageSubresourceLayers,
) -> vk::ImageBlit {
    vk::ImageBlit {
        src_subresource: src_layers,
        src_offsets: [
            vk::Offset3D {
                x: src_region.start.x,
                y: src_region.start.y,
                z: 0,
            },
            vk::Offset3D {
                x: src_region.end.x,
                y: src_region.end.y,
                z: 1,
            },
        ],
        dst_subresource: dst_layers,
        dst_offsets: [
            vk::Offset3D {
                x: dst_region.start.x,
                y: dst_region.start.y,
                z: 0,
            },
            vk::Offset3D {
                x: dst_region.end.x,
                y: dst_region.end.y,
                z: 1,
            },
        ],
    }
}

fn make_image_resolve(
    dst_region: BlitRegion2D,
    src_region: BlitRegion2D,
    dst_layers: vk::ImageSubresourceLayers,
    src_layers: vk::ImageSubresourceLayers,
) -> vk::ImageResolve {
    vk::ImageResolve {
        src_subresource: src_layers,
        src_offset: vk::Offset3D {
            x: src_region.start.x,
            y: src_region.start.y,
            z: 0,
        },
        dst_subresource: dst_layers,
        dst_offset: vk::Offset3D {
            x: dst_region.start.x,
            y: dst_region.start.y,
            z: 0,
        },
        extent: vk::Extent3D {
            width: (dst_region.end.x - dst_region.start.x).max(0) as u32,
            height: (dst_region.end.y - dst_region.start.y).max(0) as u32,
            depth: 1,
        },
    }
}

fn convert_image_type(image_type: ImageType) -> vk::ImageType {
    match image_type {
        ImageType::E1D => vk::ImageType::TYPE_1D,
        ImageType::E2D | ImageType::Linear => vk::ImageType::TYPE_2D,
        ImageType::E3D => vk::ImageType::TYPE_3D,
        ImageType::Buffer => vk::ImageType::TYPE_2D,
    }
}

fn convert_sample_count(num_samples: u32) -> vk::SampleCountFlags {
    match num_samples {
        1 => vk::SampleCountFlags::TYPE_1,
        2 => vk::SampleCountFlags::TYPE_2,
        4 => vk::SampleCountFlags::TYPE_4,
        8 => vk::SampleCountFlags::TYPE_8,
        16 => vk::SampleCountFlags::TYPE_16,
        _ => vk::SampleCountFlags::TYPE_1,
    }
}

fn image_usage_flags(
    format_info: maxwell_to_vk::FormatInfo,
    format: PixelFormat,
) -> vk::ImageUsageFlags {
    let mut usage = vk::ImageUsageFlags::TRANSFER_SRC
        | vk::ImageUsageFlags::TRANSFER_DST
        | vk::ImageUsageFlags::SAMPLED;
    if format_info.attachable {
        match crate::surface::get_format_type(format) {
            SurfaceType::ColorTexture => usage |= vk::ImageUsageFlags::COLOR_ATTACHMENT,
            SurfaceType::Depth | SurfaceType::Stencil | SurfaceType::DepthStencil => {
                usage |= vk::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT;
            }
            SurfaceType::Invalid => {}
        }
    }
    if format_info.storage {
        usage |= vk::ImageUsageFlags::STORAGE;
    }
    usage
}

fn make_image_create_info(
    info: &ImageInfo,
    format_info: maxwell_to_vk::FormatInfo,
) -> vk::ImageCreateInfo {
    let mut flags = vk::ImageCreateFlags::empty();
    if info.image_type == ImageType::E2D
        && info.resources.layers >= 6
        && info.size.width == info.size.height
    {
        flags |= vk::ImageCreateFlags::CUBE_COMPATIBLE;
    }
    if info.image_type == ImageType::E3D {
        flags |= vk::ImageCreateFlags::TYPE_2D_ARRAY_COMPATIBLE;
    }
    let (samples_x, samples_y) =
        crate::texture_cache::samples_helper::samples_log2(info.num_samples as i32);
    vk::ImageCreateInfo::builder()
        .flags(flags)
        .image_type(convert_image_type(info.image_type))
        .format(format_info.format)
        .extent(vk::Extent3D {
            width: (info.size.width >> samples_x.max(0) as u32).max(1),
            height: (info.size.height >> samples_y.max(0) as u32).max(1),
            depth: info.size.depth.max(1),
        })
        .mip_levels(info.resources.levels.max(1) as u32)
        .array_layers(info.resources.layers.max(1) as u32)
        .samples(convert_sample_count(info.num_samples))
        .tiling(vk::ImageTiling::OPTIMAL)
        .usage(image_usage_flags(format_info, info.format))
        .sharing_mode(vk::SharingMode::EXCLUSIVE)
        .initial_layout(vk::ImageLayout::UNDEFINED)
        .build()
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

fn trace_image_write(gpu_addr: u64) -> bool {
    if std::env::var_os("RUZU_TRACE_VK_IMAGE_WRITES").is_some() {
        return true;
    }
    std::env::var("RUZU_TRACE_VK_IMAGE_WRITES_GPU")
        .ok()
        .and_then(|value| u64::from_str_radix(value.trim_start_matches("0x"), 16).ok())
        .is_some_and(|target| target == gpu_addr)
}

/// Unpack little-endian A2B10G10R10 (R in low 10 bits, then G, B, A in top 2)
/// into RGBA8 so debug dumps show true colors.
fn decode_a2b10g10r10_to_rgba8(bytes: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(bytes.len());
    for chunk in bytes.chunks_exact(4) {
        let v = u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
        let r = (v & 0x3FF) >> 2;
        let g = ((v >> 10) & 0x3FF) >> 2;
        let b = ((v >> 20) & 0x3FF) >> 2;
        let a = ((v >> 30) & 0x3) * 85;
        out.extend_from_slice(&[r as u8, g as u8, b as u8, a as u8]);
    }
    out
}

fn decode_unsigned_float(bits: u32, mantissa_bits: u32) -> f32 {
    let mantissa_mask = (1 << mantissa_bits) - 1;
    let mantissa = bits & mantissa_mask;
    let exponent = bits >> mantissa_bits;
    match exponent {
        0 => (mantissa as f32) * 2.0f32.powi(1 - 15 - mantissa_bits as i32),
        0x1f => {
            if mantissa == 0 {
                f32::INFINITY
            } else {
                f32::NAN
            }
        }
        _ => {
            (1.0 + mantissa as f32 / (1 << mantissa_bits) as f32)
                * 2.0f32.powi(exponent as i32 - 15)
        }
    }
}

/// Decode Vulkan `B10G11R11_UFLOAT_PACK32` and apply a Reinhard curve so
/// values above 1.0 remain distinguishable in the 8-bit diagnostic dump.
fn decode_b10g11r11_to_rgba8(bytes: &[u8]) -> Vec<u8> {
    fn to_u8(value: f32) -> u8 {
        if value.is_nan() {
            return 0;
        }
        let mapped = if value.is_infinite() {
            1.0
        } else {
            value.max(0.0) / (1.0 + value.max(0.0))
        };
        (mapped * 255.0).round() as u8
    }

    let mut out = Vec::with_capacity(bytes.len());
    for chunk in bytes.chunks_exact(4) {
        let value = u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
        let r = decode_unsigned_float(value & 0x7ff, 6);
        let g = decode_unsigned_float((value >> 11) & 0x7ff, 6);
        let b = decode_unsigned_float((value >> 22) & 0x3ff, 5);
        out.extend_from_slice(&[to_u8(r), to_u8(g), to_u8(b), 255]);
    }
    out
}

fn decode_debug_dump_to_rgba8(bytes: &[u8], info: &ImageInfo) -> Vec<u8> {
    match info.format {
        PixelFormat::R8Unorm => bytes
            .iter()
            .flat_map(|&value| [value, value, value, 255])
            .collect(),
        PixelFormat::A2B10G10R10Unorm | PixelFormat::A2B10G10R10Uint => {
            decode_a2b10g10r10_to_rgba8(bytes)
        }
        PixelFormat::B10G11R11Float => decode_b10g11r11_to_rgba8(bytes),
        format if crate::surface::is_pixel_format_bcn(format) => {
            let mut copies = full_download_copies(info);
            let mut decoded_len = 0usize;
            for copy in &copies {
                decoded_len += copy.image_extent.width as usize
                    * copy.image_extent.height as usize
                    * copy.image_subresource.num_layers as usize
                    * crate::texture_cache::decode_bc::converted_bytes_per_block(format) as usize;
            }
            let mut decoded = vec![0u8; decoded_len];
            convert_image(bytes, info, &mut decoded, &mut copies);
            match crate::texture_cache::decode_bc::converted_bytes_per_block(format) {
                1 => decoded
                    .into_iter()
                    .flat_map(|v| [v, v, v, 255])
                    .collect::<Vec<u8>>(),
                2 => decoded
                    .chunks_exact(2)
                    .flat_map(|px| [px[0], px[1], 0, 255])
                    .collect::<Vec<u8>>(),
                4 => decoded,
                _ => bytes.to_vec(),
            }
        }
        _ => bytes.to_vec(),
    }
}

fn write_rgba_like_ppm(
    path: &std::path::Path,
    rgba: &[u8],
    width: u32,
    height: u32,
) -> std::io::Result<()> {
    use std::io::Write;

    let pixel_count = width as usize * height as usize;
    let required_len = pixel_count * 4;
    if rgba.len() < required_len {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "RGBA-like buffer is smaller than framebuffer dimensions",
        ));
    }

    let mut output =
        Vec::with_capacity(format!("P6\n{} {}\n255\n", width, height).len() + pixel_count * 3);
    write!(&mut output, "P6\n{} {}\n255\n", width, height)?;
    for pixel in rgba[..required_len].chunks_exact(4) {
        output.extend_from_slice(&pixel[..3]);
    }
    std::fs::write(path, output)?;
    if std::env::var_os("RUZU_DUMP_VK_IMAGE_ALPHA").is_some() {
        let alpha_path = path.with_extension("alpha.ppm");
        let mut alpha_output =
            Vec::with_capacity(format!("P6\n{} {}\n255\n", width, height).len() + pixel_count * 3);
        write!(&mut alpha_output, "P6\n{} {}\n255\n", width, height)?;
        for pixel in rgba[..required_len].chunks_exact(4) {
            alpha_output.extend_from_slice(&[pixel[3], pixel[3], pixel[3]]);
        }
        std::fs::write(alpha_path, alpha_output)?;
    }
    Ok(())
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

fn make_subresource_range(
    aspect_mask: vk::ImageAspectFlags,
    range: SubresourceRange,
    flags: ImageViewFlagBits,
) -> vk::ImageSubresourceRange {
    let base_layer = if flags.contains(ImageViewFlagBits::SLICE) {
        0
    } else {
        range.base.layer.max(0) as u32
    };
    let layer_count = if flags.contains(ImageViewFlagBits::SLICE) {
        1
    } else {
        range.extent.layers.max(1) as u32
    };
    vk::ImageSubresourceRange {
        aspect_mask,
        base_mip_level: range.base.level.max(0) as u32,
        level_count: range.extent.levels.max(1) as u32,
        base_array_layer: base_layer,
        layer_count,
    }
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

fn image_view_type_from_view_type(
    view_type: crate::texture_cache::types::ImageViewType,
) -> vk::ImageViewType {
    match view_type {
        crate::texture_cache::types::ImageViewType::E1D => vk::ImageViewType::TYPE_1D,
        crate::texture_cache::types::ImageViewType::E2D
        | crate::texture_cache::types::ImageViewType::Rect => vk::ImageViewType::TYPE_2D,
        crate::texture_cache::types::ImageViewType::Cube => vk::ImageViewType::CUBE,
        crate::texture_cache::types::ImageViewType::E3D => vk::ImageViewType::TYPE_3D,
        crate::texture_cache::types::ImageViewType::E1DArray => vk::ImageViewType::TYPE_1D_ARRAY,
        crate::texture_cache::types::ImageViewType::E2DArray => vk::ImageViewType::TYPE_2D_ARRAY,
        crate::texture_cache::types::ImageViewType::CubeArray => vk::ImageViewType::CUBE_ARRAY,
        crate::texture_cache::types::ImageViewType::Buffer => vk::ImageViewType::TYPE_1D,
    }
}

fn image_format_to_vk(format: ImageFormat) -> vk::Format {
    match format {
        ImageFormat::Typeless => vk::Format::UNDEFINED,
        ImageFormat::R8Sint => vk::Format::R8_SINT,
        ImageFormat::R8Uint => vk::Format::R8_UINT,
        ImageFormat::R16Uint => vk::Format::R16_UINT,
        ImageFormat::R16Sint => vk::Format::R16_SINT,
        ImageFormat::R32Uint => vk::Format::R32_UINT,
        ImageFormat::R32G32Uint => vk::Format::R32G32_UINT,
        ImageFormat::R32G32B32A32Uint => vk::Format::R32G32B32A32_UINT,
    }
}

impl Drop for TextureCache {
    fn drop(&mut self) {
        for (_, fb) in self.framebuffers_by_render_targets.drain() {
            self.runtime.destroy_framebuffer(fb.framebuffer);
        }
        for (_, view) in self.image_views.drain() {
            self.runtime.destroy_image_view(view);
        }
        for (_, image) in self.images.drain() {
            self.runtime.destroy_image(image);
        }
        for (_, sampler) in self.samplers.drain() {
            self.runtime.destroy_sampler(sampler);
        }
        for (_, fb) in self.framebuffers.drain() {
            self.runtime.destroy_framebuffer(fb.framebuffer);
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
