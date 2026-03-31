// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/layer.h` / `present/layer.cpp`.
//!
//! A presentation layer that converts a guest framebuffer into a Vulkan
//! image suitable for composition, applying anti-aliasing and FSR as needed.

use ash::vk;

use super::anti_alias_pass::{AntiAliasPass, NoAa};
use super::fsr::Fsr;
use super::present_push_constants::{
    make_orthographic_matrix, PresentPushConstants, ScreenRectVertex,
};
use super::util;

// ---------------------------------------------------------------------------
// Anonymous namespace helpers (port of file-static functions)
// ---------------------------------------------------------------------------

/// Port of anonymous `GetBytesPerPixel` helper.
fn get_bytes_per_pixel(pixel_format: PixelFormat) -> u32 {
    match pixel_format {
        PixelFormat::Rgba8888 | PixelFormat::Rgbx8888 | PixelFormat::Bgra8888 => 4,
        PixelFormat::Rgb565 => 2,
    }
}

/// Port of anonymous `GetSizeInBytes` helper.
fn get_size_in_bytes(stride: u32, height: u32, pixel_format: PixelFormat) -> u64 {
    stride as u64 * height as u64 * get_bytes_per_pixel(pixel_format) as u64
}

/// Port of anonymous `GetFormat` helper.
fn get_vk_format(pixel_format: PixelFormat) -> vk::Format {
    match pixel_format {
        PixelFormat::Rgba8888 | PixelFormat::Rgbx8888 => vk::Format::A8B8G8R8_UNORM_PACK32,
        PixelFormat::Rgb565 => vk::Format::R5G6B5_UNORM_PACK16,
        PixelFormat::Bgra8888 => vk::Format::B8G8R8A8_UNORM,
    }
}

/// Simplified pixel format enum matching upstream `Service::android::PixelFormat`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum PixelFormat {
    Rgba8888 = 1,
    Rgbx8888 = 2,
    Rgb565 = 4,
    Bgra8888 = 5,
}

/// Simplified anti-aliasing setting matching upstream `Settings::AntiAliasing`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AntiAliasingSetting {
    None = 0,
    Fxaa = 1,
    Smaa = 2,
}

// ---------------------------------------------------------------------------
// FramebufferLayout (simplified, matching upstream Layout::FramebufferLayout)
// ---------------------------------------------------------------------------

/// Simplified framebuffer layout for presentation, matching upstream.
pub struct FramebufferLayout {
    pub width: u32,
    pub height: u32,
    pub screen_left: f32,
    pub screen_top: f32,
    pub screen_width: f32,
    pub screen_height: f32,
}

/// Simplified framebuffer config matching upstream `Tegra::FramebufferConfig`.
pub struct FramebufferConfig {
    pub address: u64,
    pub offset: u64,
    pub width: u32,
    pub height: u32,
    pub stride: u32,
    pub pixel_format: PixelFormat,
}

/// Crop rectangle: [left, top, right, bottom], normalized 0..1.
pub type CropRect = [f32; 4];

// ---------------------------------------------------------------------------
// Layer
// ---------------------------------------------------------------------------

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

    buffer: vk::Buffer,
    raw_images: Vec<vk::Image>,
    raw_image_views: Vec<vk::ImageView>,
    raw_width: u32,
    raw_height: u32,
    pixel_format: Option<PixelFormat>,

    anti_alias_setting: AntiAliasingSetting,
    anti_alias: Box<dyn AntiAliasPass>,

    fsr: Option<Fsr>,
    resource_ticks: Vec<u64>,
}

impl Layer {
    /// Port of `Layer::Layer`.
    pub fn new(
        device: ash::Device,
        image_count: usize,
        _output_size: vk::Extent2D,
        layout: vk::DescriptorSetLayout,
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

        Layer {
            device,
            image_count,
            descriptor_pool,
            descriptor_sets,
            buffer: vk::Buffer::null(),
            raw_images: Vec::new(),
            raw_image_views: Vec::new(),
            raw_width: 0,
            raw_height: 0,
            pixel_format: None,
            anti_alias_setting: AntiAliasingSetting::None,
            anti_alias: Box::new(NoAa),
            fsr: None,
            resource_ticks: Vec::new(),
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
        sampler: vk::Sampler,
        image_index: usize,
        source_image: vk::Image,
        source_image_view: vk::ImageView,
        texture_width: u32,
        texture_height: u32,
        layout: &FramebufferLayout,
        crop_rect: CropRect,
    ) {
        let mut current_image = source_image;
        let mut current_view = source_image_view;

        // Apply anti-aliasing
        self.anti_alias
            .draw(image_index, &mut current_image, &mut current_view);

        // Apply FSR if active
        let mut effective_crop = crop_rect;
        if let Some(ref mut fsr) = self.fsr {
            let render_extent = vk::Extent2D {
                width: texture_width,
                height: texture_height,
            };
            current_view = fsr.draw(
                image_index,
                current_image,
                current_view,
                render_extent,
                crop_rect,
            );
            effective_crop = [0.0, 0.0, 1.0, 1.0];
        }

        // Set matrix data
        Self::set_matrix_data(out_push_constants, layout);

        // Set vertex data
        Self::set_vertex_data(out_push_constants, layout, &effective_crop);

        // Update descriptor set
        self.update_descriptor_set(current_view, sampler, image_index);
        *out_descriptor_set = self.descriptor_sets[image_index];
    }

    /// Port of `Layer::SetMatrixData`.
    fn set_matrix_data(data: &mut PresentPushConstants, layout: &FramebufferLayout) {
        data.modelview_matrix = make_orthographic_matrix(layout.width as f32, layout.height as f32);
    }

    /// Port of `Layer::SetVertexData`.
    fn set_vertex_data(
        data: &mut PresentPushConstants,
        layout: &FramebufferLayout,
        crop: &CropRect,
    ) {
        let x = layout.screen_left;
        let y = layout.screen_top;
        let w = layout.screen_width;
        let h = layout.screen_height;

        data.vertices[0] = ScreenRectVertex::new(x, y, crop[0], crop[1]);
        data.vertices[1] = ScreenRectVertex::new(x + w, y, crop[2], crop[1]);
        data.vertices[2] = ScreenRectVertex::new(x, y + h, crop[0], crop[3]);
        data.vertices[3] = ScreenRectVertex::new(x + w, y + h, crop[2], crop[3]);
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
        // In the full implementation, this would wait on resource_ticks
        // and destroy the raw images and buffer.
        self.raw_images.clear();
        self.raw_image_views.clear();
        self.buffer = vk::Buffer::null();
    }

    /// Port of `Layer::RefreshResources`.
    ///
    /// Recreates raw images and staging buffer if the framebuffer dimensions
    /// or pixel format have changed.
    pub fn refresh_resources(
        &mut self,
        framebuffer: &FramebufferConfig,
        allocator: &vk::DeviceMemory,
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

        self.release_raw_images();

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

        for i in 0..self.image_count {
            self.raw_images[i] =
                util::create_wrapped_image(&self.device, allocator, extent, format);
            self.raw_image_views[i] =
                util::create_wrapped_image_view(&self.device, self.raw_images[i], format);
        }
    }
}
