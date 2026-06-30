// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/layer.h` / `present/layer.cpp`.
//!
//! A presentation layer that converts a guest framebuffer into a Vulkan
//! image suitable for composition, applying anti-aliasing and FSR as needed.

use ash::vk;

use crate::framebuffer_config::{normalize_crop, AndroidPixelFormat, FramebufferConfig, RectF};
use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
use crate::present::{PresentFilters, ScalingFilter};
use crate::renderer_vulkan::scheduler::Scheduler;
use crate::renderer_vulkan::RasterizerVulkan;
use crate::textures::decoders;
use crate::vulkan_common::vulkan_memory_allocator::{MappedBuffer, MemoryAllocator, MemoryUsage};

use super::anti_alias_pass::{AntiAliasPass, NoAa};
use super::fsr::Fsr;
use super::fxaa::Fxaa;
use super::present_push_constants::{
    make_orthographic_matrix, PresentPushConstants, ScreenRectVertex,
};
use super::smaa::Smaa;
use super::util;
use ruzu_core::frontend::framebuffer_layout::FramebufferLayout;

// ---------------------------------------------------------------------------
// Anonymous namespace helpers (port of file-static functions)
// ---------------------------------------------------------------------------

/// Port of anonymous `GetBytesPerPixel` helper.
fn get_bytes_per_pixel(pixel_format: AndroidPixelFormat) -> u32 {
    match pixel_format.0 {
        1 | 2 | 5 => 4,
        4 => 2,
        _ => {
            log::warn!("Unknown framebuffer pixel format: {}", pixel_format.0);
            4
        }
    }
}

/// Port of anonymous `GetSizeInBytes` helper.
fn get_size_in_bytes(stride: u32, height: u32, pixel_format: AndroidPixelFormat) -> u64 {
    stride as u64 * height as u64 * get_bytes_per_pixel(pixel_format) as u64
}

/// Port of anonymous `GetFormat` helper.
fn get_vk_format(pixel_format: AndroidPixelFormat) -> vk::Format {
    match pixel_format.0 {
        1 | 2 => vk::Format::A8B8G8R8_UNORM_PACK32,
        4 => vk::Format::R5G6B5_UNORM_PACK16,
        5 => vk::Format::B8G8R8A8_UNORM,
        _ => {
            log::warn!("Unknown framebuffer pixel format: {}", pixel_format.0);
            vk::Format::A8B8G8R8_UNORM_PACK32
        }
    }
}

/// Simplified anti-aliasing setting matching upstream `Settings::AntiAliasing`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AntiAliasingSetting {
    None = 0,
    Fxaa = 1,
    Smaa = 2,
}

/// Port of `Layer` class.
///
/// Owns raw images for framebuffer upload, anti-aliasing state, FSR state,
/// descriptor sets, and a staging buffer. Configures per-draw push constants
/// and descriptor sets for the window adapt pass.
pub struct Layer {
    device: ash::Device,
    image_count: usize,

    descriptor_pool: vk::DescriptorPool,
    descriptor_sets: Vec<vk::DescriptorSet>,

    buffer: Option<MappedBuffer>,
    raw_images: Vec<vk::Image>,
    raw_image_views: Vec<vk::ImageView>,
    raw_image_initialized: Vec<bool>,
    raw_width: u32,
    raw_height: u32,
    pixel_format: Option<AndroidPixelFormat>,

    anti_alias_setting: AntiAliasingSetting,
    anti_alias: Box<dyn AntiAliasPass>,

    fsr: Option<Fsr>,
    resource_ticks: Vec<u64>,
    filters: &'static PresentFilters,
}

impl Layer {
    /// Port of `Layer::Layer`.
    pub fn new(
        device: ash::Device,
        image_count: usize,
        output_size: vk::Extent2D,
        layout: vk::DescriptorSetLayout,
        filters: &'static PresentFilters,
        allocator: &MemoryAllocator,
        supports_float16: bool,
    ) -> Self {
        // Create descriptor pool
        let descriptor_pool = util::create_wrapped_descriptor_pool(
            &device,
            image_count as u32,
            image_count as u32,
            &[vk::DescriptorType::COMBINED_IMAGE_SAMPLER],
        );

        // Create descriptor sets
        let layouts = vec![layout; image_count];
        let descriptor_sets =
            util::create_wrapped_descriptor_sets(&device, descriptor_pool, &layouts);

        let fsr = if (filters.get_scaling_filter)() == ScalingFilter::Fsr {
            Some(Fsr::new(
                device.clone(),
                allocator,
                image_count,
                output_size,
                supports_float16,
            ))
        } else {
            None
        };

        Layer {
            device,
            image_count,
            descriptor_pool,
            descriptor_sets,
            buffer: None,
            raw_images: Vec::new(),
            raw_image_views: Vec::new(),
            raw_image_initialized: Vec::new(),
            raw_width: 0,
            raw_height: 0,
            pixel_format: None,
            anti_alias_setting: AntiAliasingSetting::None,
            anti_alias: Box::new(NoAa),
            fsr,
            resource_ticks: Vec::new(),
            filters,
        }
    }

    /// Port of `Layer::ConfigureDraw`.
    ///
    /// Prepares push constants and descriptor set for drawing this layer.
    /// Applies anti-aliasing and FSR if configured.
    pub fn configure_draw(
        &mut self,
        out_push_constants: &mut PresentPushConstants,
        out_descriptor_set: &mut vk::DescriptorSet,
        scheduler: &mut Scheduler,
        sampler: vk::Sampler,
        image_index: usize,
        source_image: vk::Image,
        source_image_view: vk::ImageView,
        texture_width: u32,
        texture_height: u32,
        scaled_width: u32,
        scaled_height: u32,
        layout: &FramebufferLayout,
        crop_rect: RectF,
    ) {
        let mut current_image = source_image;
        let mut current_view = source_image_view;

        // Apply anti-aliasing
        self.anti_alias.draw(
            scheduler,
            image_index,
            &mut current_image,
            &mut current_view,
        );

        // Apply FSR if active
        let mut effective_crop = crop_rect;
        if let Some(ref mut fsr) = self.fsr {
            let render_extent = vk::Extent2D {
                width: scaled_width,
                height: scaled_height,
            };
            current_view = fsr.draw(
                scheduler,
                image_index,
                current_image,
                current_view,
                render_extent,
                [
                    crop_rect.left,
                    crop_rect.top,
                    crop_rect.right,
                    crop_rect.bottom,
                ],
            );
            effective_crop = RectF {
                left: 0.0,
                top: 0.0,
                right: 1.0,
                bottom: 1.0,
            };
        }

        // Set matrix data
        Self::set_matrix_data(out_push_constants, layout);

        // Set vertex data
        Self::set_vertex_data(out_push_constants, layout, &effective_crop);

        // Update descriptor set
        self.update_descriptor_set(current_view, sampler, image_index);
        *out_descriptor_set = self.descriptor_sets[image_index];
    }

    /// Port-facing subset of `Layer::ConfigureDraw`.
    ///
    /// This follows upstream's raw-image path when `rasterizer.AccelerateDisplay`
    /// is not available in the Rust Vulkan backend yet. It prepares the same
    /// push constants and descriptor set, but the guest-memory upload into the
    /// raw image remains a separate `UpdateRawImage` parity step.
    pub fn configure_draw_from_framebuffer(
        &mut self,
        out_push_constants: &mut PresentPushConstants,
        out_descriptor_set: &mut vk::DescriptorSet,
        rasterizer: &mut RasterizerVulkan,
        scheduler: &mut Scheduler,
        allocator: &MemoryAllocator,
        device_memory: &MaxwellDeviceMemoryManager,
        sampler: vk::Sampler,
        image_index: usize,
        framebuffer: &FramebufferConfig,
        layout: &FramebufferLayout,
    ) {
        let framebuffer_addr = framebuffer
            .address
            .saturating_add(framebuffer.offset as u64);
        let texture_info =
            rasterizer.accelerate_display(framebuffer, framebuffer_addr, framebuffer.stride);
        let texture_width = texture_info
            .as_ref()
            .map_or(framebuffer.width, |info| info.width);
        let texture_height = texture_info
            .as_ref()
            .map_or(framebuffer.height, |info| info.height);
        let scaled_width = texture_info
            .as_ref()
            .map_or(texture_width, |info| info.scaled_width);
        let scaled_height = texture_info
            .as_ref()
            .map_or(texture_height, |info| info.scaled_height);

        self.refresh_resources(framebuffer, allocator, scheduler);
        self.set_anti_alias_pass(allocator);
        scheduler.request_outside_renderpass();
        if let Some(tick) = self.resource_ticks.get(image_index).copied() {
            scheduler.wait(tick);
        }
        if texture_info.is_none() {
            self.update_raw_image(scheduler, device_memory, framebuffer, image_index);
        }
        if image_index < self.resource_ticks.len() {
            self.resource_ticks[image_index] = scheduler.pending_tick();
        }

        let (source_image, source_image_view) = texture_info.as_ref().map_or_else(
            || {
                (
                    self.raw_images[image_index],
                    self.raw_image_views[image_index],
                )
            },
            |info| (info.image, info.image_view),
        );
        let crop_rect = normalize_crop(framebuffer, texture_width, texture_height);
        let trace_present_layer = std::env::var_os("RUZU_TRACE_VK_PRESENT_LAYER").is_some();

        self.configure_draw(
            out_push_constants,
            out_descriptor_set,
            scheduler,
            sampler,
            image_index,
            source_image,
            source_image_view,
            texture_width,
            texture_height,
            scaled_width,
            scaled_height,
            layout,
            crop_rect,
        );
        if trace_present_layer {
            let v = &out_push_constants.vertices;
            log::info!(
                "[VK_PRESENT_LAYER] fb=0x{:X}+0x{:X} fb={}x{} stride={} fmt={} crop=({}, {}, {}, {}) tex={}x{} scaled={}x{} uv=({}, {}, {}, {}) accelerated={} layout={}x{} screen=({}, {}, {}, {}) vertices=({}, {}) ({}, {}) ({}, {}) ({}, {})",
                framebuffer.address,
                framebuffer.offset,
                framebuffer.width,
                framebuffer.height,
                framebuffer.stride,
                framebuffer.pixel_format.0,
                framebuffer.crop_rect.left,
                framebuffer.crop_rect.top,
                framebuffer.crop_rect.right,
                framebuffer.crop_rect.bottom,
                texture_width,
                texture_height,
                scaled_width,
                scaled_height,
                crop_rect.left,
                crop_rect.top,
                crop_rect.right,
                crop_rect.bottom,
                texture_info.is_some(),
                layout.width,
                layout.height,
                layout.screen.left,
                layout.screen.top,
                layout.screen.right,
                layout.screen.bottom,
                v[0].position[0],
                v[0].position[1],
                v[1].position[0],
                v[1].position[1],
                v[2].position[0],
                v[2].position[1],
                v[3].position[0],
                v[3].position[1],
            );
        }
    }

    fn update_raw_image(
        &mut self,
        scheduler: &mut Scheduler,
        device_memory: &MaxwellDeviceMemoryManager,
        framebuffer: &FramebufferConfig,
        image_index: usize,
    ) {
        let image_offset = self.get_raw_image_offset(framebuffer, image_index);
        let linear_size = get_size_in_bytes(
            framebuffer.stride,
            framebuffer.height,
            framebuffer.pixel_format,
        );
        let Some(buffer) = self.buffer.as_mut() else {
            return;
        };
        let end = image_offset.saturating_add(linear_size) as usize;
        let mapped = buffer.mapped_slice_mut();
        if end > mapped.len() {
            log::error!(
                "Vulkan Layer::UpdateRawImage staging range out of bounds offset={} size={} len={}",
                image_offset,
                linear_size,
                mapped.len()
            );
            return;
        }

        let bytes_per_pixel = get_bytes_per_pixel(framebuffer.pixel_format);
        const BLOCK_HEIGHT_LOG2: u32 = 4;
        let tiled_size = decoders::calculate_size(
            true,
            bytes_per_pixel,
            framebuffer.stride,
            framebuffer.height,
            1,
            BLOCK_HEIGHT_LOG2,
            0,
        );
        let framebuffer_addr = framebuffer
            .address
            .saturating_add(framebuffer.offset as u64);
        let host_ptr = device_memory.get_pointer(framebuffer_addr);
        if host_ptr.is_null() {
            log::warn!(
                "Vulkan Layer::UpdateRawImage could not map framebuffer address 0x{:X}",
                framebuffer_addr
            );
            mapped[image_offset as usize..end].fill(0);
        } else {
            let input = unsafe { std::slice::from_raw_parts(host_ptr, tiled_size) };
            decoders::unswizzle_texture(
                &mut mapped[image_offset as usize..end],
                input,
                bytes_per_pixel,
                framebuffer.width,
                framebuffer.height,
                1,
                BLOCK_HEIGHT_LOG2,
                0,
                0,
            );
            if std::env::var_os("RUZU_TRACE_VK_RAW_PRESENT").is_some() {
                use std::sync::atomic::{AtomicU64, Ordering};
                static TRACE_COUNT: AtomicU64 = AtomicU64::new(0);
                let n = TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
                if n < 32 || n.is_power_of_two() {
                    let output = &mapped[image_offset as usize..end];
                    let nonzero = output.iter().filter(|&&byte| byte != 0).count();
                    let first = output
                        .chunks_exact(4)
                        .take(4)
                        .map(|px| u32::from_le_bytes([px[0], px[1], px[2], px[3]]))
                        .collect::<Vec<_>>();
                    log::info!(
                        "[VK_RAW_PRESENT] #{} fb=0x{:X} {}x{} stride={} fmt={} tiled_size={} linear_size={} nonzero={} first={:08X?}",
                        n,
                        framebuffer_addr,
                        framebuffer.width,
                        framebuffer.height,
                        framebuffer.stride,
                        framebuffer.pixel_format.0,
                        tiled_size,
                        linear_size,
                        nonzero,
                        first,
                    );
                }
            }
        }
        buffer.flush();

        let image = self.raw_images[image_index];
        let staging_buffer = buffer.buffer();
        let was_initialized = self.raw_image_initialized.get(image_index).copied() == Some(true);
        let buffer_row_length = framebuffer.stride;
        let buffer_image_height = framebuffer.height;
        let image_width = framebuffer.width;
        let image_height = framebuffer.height;
        let device = self.device.clone();
        scheduler.record(move |cmdbuf| unsafe {
            let upload_barrier = vk::ImageMemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::empty())
                .dst_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .old_layout(if was_initialized {
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
            device.cmd_pipeline_barrier(
                cmdbuf,
                vk::PipelineStageFlags::TOP_OF_PIPE,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &[upload_barrier],
            );

            let copy = vk::BufferImageCopy::builder()
                .buffer_offset(image_offset)
                .buffer_row_length(buffer_row_length)
                .buffer_image_height(buffer_image_height)
                .image_subresource(vk::ImageSubresourceLayers {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    mip_level: 0,
                    base_array_layer: 0,
                    layer_count: 1,
                })
                .image_offset(vk::Offset3D { x: 0, y: 0, z: 0 })
                .image_extent(vk::Extent3D {
                    width: image_width,
                    height: image_height,
                    depth: 1,
                })
                .build();
            device.cmd_copy_buffer_to_image(
                cmdbuf,
                staging_buffer,
                image,
                vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                &[copy],
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
            device.cmd_pipeline_barrier(
                cmdbuf,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::FRAGMENT_SHADER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &[shader_barrier],
            );
        });
        self.raw_image_initialized[image_index] = true;
    }

    /// Port of `Layer::SetMatrixData`.
    fn set_matrix_data(data: &mut PresentPushConstants, layout: &FramebufferLayout) {
        data.modelview_matrix = make_orthographic_matrix(layout.width as f32, layout.height as f32);
    }

    /// Port of `Layer::SetVertexData`.
    fn set_vertex_data(data: &mut PresentPushConstants, layout: &FramebufferLayout, crop: &RectF) {
        let x = layout.screen.left as f32;
        let y = layout.screen.top as f32;
        let w = layout.screen.get_width() as f32;
        let h = layout.screen.get_height() as f32;

        data.vertices[0] = ScreenRectVertex::new(x, y, crop.left, crop.top);
        data.vertices[1] = ScreenRectVertex::new(x + w, y, crop.right, crop.top);
        data.vertices[2] = ScreenRectVertex::new(x, y + h, crop.left, crop.bottom);
        data.vertices[3] = ScreenRectVertex::new(x + w, y + h, crop.right, crop.bottom);
    }

    /// Port of `Layer::UpdateDescriptorSet`.
    fn update_descriptor_set(
        &self,
        image_view: vk::ImageView,
        sampler: vk::Sampler,
        image_index: usize,
    ) {
        let image_info = vk::DescriptorImageInfo {
            sampler,
            image_view,
            image_layout: vk::ImageLayout::GENERAL,
        };

        let sampler_write = vk::WriteDescriptorSet {
            s_type: vk::StructureType::WRITE_DESCRIPTOR_SET,
            p_next: std::ptr::null(),
            dst_set: self.descriptor_sets[image_index],
            dst_binding: 0,
            dst_array_element: 0,
            descriptor_count: 1,
            descriptor_type: vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
            p_image_info: &image_info,
            p_buffer_info: std::ptr::null(),
            p_texel_buffer_view: std::ptr::null(),
        };

        unsafe {
            self.device.update_descriptor_sets(&[sampler_write], &[]);
        }
    }

    /// Port of `Layer::CalculateBufferSize`.
    fn calculate_buffer_size(&self, framebuffer: &FramebufferConfig) -> u64 {
        get_size_in_bytes(
            framebuffer.stride,
            framebuffer.height,
            framebuffer.pixel_format,
        ) * self.image_count as u64
    }

    /// Port of `Layer::GetRawImageOffset`.
    fn get_raw_image_offset(&self, framebuffer: &FramebufferConfig, image_index: usize) -> u64 {
        get_size_in_bytes(
            framebuffer.stride,
            framebuffer.height,
            framebuffer.pixel_format,
        ) * image_index as u64
    }

    /// Port of `Layer::ReleaseRawImages`.
    pub fn release_raw_images(&mut self) {
        self.raw_images.clear();
        self.raw_image_views.clear();
        self.raw_image_initialized.clear();
        self.buffer = None;
    }

    /// Port of `Layer::RefreshResources`.
    ///
    /// Recreates raw images and staging buffer if the framebuffer dimensions
    /// or pixel format have changed.
    pub fn refresh_resources(
        &mut self,
        framebuffer: &FramebufferConfig,
        allocator: &MemoryAllocator,
        scheduler: &mut Scheduler,
    ) {
        if Some(framebuffer.pixel_format) == self.pixel_format
            && framebuffer.width == self.raw_width
            && framebuffer.height == self.raw_height
            && !self.raw_images.is_empty()
        {
            return;
        }

        self.raw_width = framebuffer.width;
        self.raw_height = framebuffer.height;
        self.pixel_format = Some(framebuffer.pixel_format);
        self.anti_alias_setting = AntiAliasingSetting::None;
        self.anti_alias = Box::new(NoAa);

        for tick in self.resource_ticks.iter().copied() {
            scheduler.wait(tick);
        }
        self.release_raw_images();

        self.create_staging_buffer(framebuffer, allocator);

        // Create raw images
        let format = get_vk_format(framebuffer.pixel_format);
        let extent = vk::Extent2D {
            width: framebuffer.width,
            height: framebuffer.height,
        };
        self.resource_ticks.resize(self.image_count, 0);
        self.raw_images.resize(self.image_count, vk::Image::null());
        self.raw_image_views
            .resize(self.image_count, vk::ImageView::null());
        self.raw_image_initialized.resize(self.image_count, false);

        for i in 0..self.image_count {
            self.raw_images[i] =
                util::create_wrapped_image(&self.device, allocator, extent, format);
            self.raw_image_views[i] =
                util::create_wrapped_image_view(&self.device, self.raw_images[i], format);
        }
    }

    /// Port of `Layer::CreateStagingBuffer`.
    fn create_staging_buffer(
        &mut self,
        framebuffer: &FramebufferConfig,
        allocator: &MemoryAllocator,
    ) {
        let size = self.calculate_buffer_size(framebuffer).max(1);
        let ci = vk::BufferCreateInfo::builder()
            .size(size)
            .usage(
                vk::BufferUsageFlags::TRANSFER_SRC
                    | vk::BufferUsageFlags::TRANSFER_DST
                    | vk::BufferUsageFlags::VERTEX_BUFFER
                    | vk::BufferUsageFlags::UNIFORM_BUFFER,
            )
            .sharing_mode(vk::SharingMode::EXCLUSIVE)
            .build();
        self.buffer = match allocator.create_mapped_buffer(&ci, MemoryUsage::Upload) {
            Ok(buffer) => Some(buffer),
            Err(err) => {
                log::error!("Failed to create Vulkan layer staging buffer: {}", err);
                None
            }
        };
    }

    /// Port-facing subset of `Layer::SetAntiAliasPass`.
    fn set_anti_alias_pass(&mut self, allocator: &MemoryAllocator) {
        let requested = match (self.filters.get_anti_aliasing)() {
            crate::present::AntiAliasing::None => AntiAliasingSetting::None,
            crate::present::AntiAliasing::Fxaa => AntiAliasingSetting::Fxaa,
            crate::present::AntiAliasing::Smaa => AntiAliasingSetting::Smaa,
        };
        if self.anti_alias_setting == requested {
            return;
        }

        self.anti_alias_setting = requested;
        self.anti_alias = match requested {
            AntiAliasingSetting::None => Box::new(NoAa),
            AntiAliasingSetting::Fxaa => {
                let resolution = common::settings::values().resolution_info.clone();
                let extent = vk::Extent2D {
                    width: resolution.scale_up_u32(self.raw_width),
                    height: resolution.scale_up_u32(self.raw_height),
                };
                Box::new(Fxaa::new(
                    self.device.clone(),
                    allocator,
                    extent,
                    self.image_count,
                ))
            }
            AntiAliasingSetting::Smaa => {
                let resolution = common::settings::values().resolution_info.clone();
                let extent = vk::Extent2D {
                    width: resolution.scale_up_u32(self.raw_width),
                    height: resolution.scale_up_u32(self.raw_height),
                };
                Box::new(Smaa::new(
                    self.device.clone(),
                    allocator,
                    extent,
                    self.image_count,
                ))
            }
        };
    }
}
