// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/layer.h` / `present/layer.cpp`.
//!
//! A presentation layer that converts a guest framebuffer into a Vulkan
//! image suitable for composition, applying anti-aliasing and FSR as needed.

use ash::vk;

use super::anti_alias_pass::AntiAliasPass;
use super::fsr::Fsr;
use super::present_push_constants::PresentPushConstants;

// ---------------------------------------------------------------------------
// Layer
// ---------------------------------------------------------------------------

/// Port of `Layer` class.
///
/// Owns raw images for framebuffer upload, anti-aliasing state, FSR state,
/// descriptor sets, and a staging buffer. Configures per-draw push constants
/// and descriptor sets for the window adapt pass.
pub struct Layer {
    _image_count: usize,
    _raw_width: u32,
    _raw_height: u32,
    _pixel_format: u32,
    _anti_alias_setting: u32,
    _anti_alias: Option<Box<dyn AntiAliasPass>>,
    _fsr: Option<Fsr>,
    _resource_ticks: Vec<u64>,
}

impl Layer {
    /// Port of `Layer::Layer`.
    pub fn new(
        _image_count: usize,
        _output_size: vk::Extent2D,
        _layout: vk::DescriptorSetLayout,
    ) -> Self {
        todo!("Layer::new")
    }

    /// Port of `Layer::ConfigureDraw`.
    pub fn configure_draw(
        &mut self,
        _out_push_constants: &mut PresentPushConstants,
        _out_descriptor_set: &mut vk::DescriptorSet,
        _sampler: vk::Sampler,
        _image_index: usize,
    ) {
        todo!("Layer::configure_draw")
    }

    // --- Private ---
    fn create_descriptor_pool(&mut self) { todo!() }
    fn create_descriptor_sets(&mut self, _layout: vk::DescriptorSetLayout) { todo!() }
    fn create_staging_buffer(&mut self) { todo!() }
    fn create_raw_images(&mut self) { todo!() }
    fn create_fsr(&mut self, _output_size: vk::Extent2D) { todo!() }
    fn refresh_resources(&mut self) { todo!() }
    fn set_anti_alias_pass(&mut self) { todo!() }
    fn release_raw_images(&mut self) { todo!() }
    fn calculate_buffer_size(&self) -> u64 { todo!() }
    fn get_raw_image_offset(&self, _image_index: usize) -> u64 { todo!() }
    fn set_matrix_data(&self, _data: &mut PresentPushConstants) { todo!() }
    fn set_vertex_data(&self, _data: &mut PresentPushConstants) { todo!() }
    fn update_descriptor_set(&mut self, _image_view: vk::ImageView, _sampler: vk::Sampler, _image_index: usize) { todo!() }
    fn update_raw_image(&mut self, _image_index: usize) { todo!() }
}
