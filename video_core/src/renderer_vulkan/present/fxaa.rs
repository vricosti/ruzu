// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/fxaa.h` / `present/fxaa.cpp`.
//!
//! Fast Approximate Anti-Aliasing (FXAA) post-processing pass.

use ash::vk;

use super::anti_alias_pass::AntiAliasPass;

// ---------------------------------------------------------------------------
// Per-image dynamic resources
// ---------------------------------------------------------------------------

/// Port of `FXAA::Image` inner struct.
struct FxaaImage {
    _framebuffer: vk::Framebuffer,
    _image: vk::Image,
    _image_view: vk::ImageView,
}

// ---------------------------------------------------------------------------
// FXAA
// ---------------------------------------------------------------------------

/// Port of `FXAA` class.
///
/// Single-pass FXAA anti-aliasing using a fullscreen fragment shader.
pub struct Fxaa {
    _extent: vk::Extent2D,
    _image_count: u32,
    _images_ready: bool,
    _dynamic_images: Vec<FxaaImage>,
}

impl Fxaa {
    /// Port of `FXAA::FXAA`.
    pub fn new(
        _extent: vk::Extent2D,
        _image_count: usize,
    ) -> Self {
        todo!("Fxaa::new")
    }

    // --- Private ---
    fn create_images(&mut self) { todo!() }
    fn create_render_passes(&mut self) { todo!() }
    fn create_sampler(&mut self) { todo!() }
    fn create_shaders(&mut self) { todo!() }
    fn create_descriptor_pool(&mut self) { todo!() }
    fn create_descriptor_set_layouts(&mut self) { todo!() }
    fn create_descriptor_sets(&mut self) { todo!() }
    fn create_pipeline_layouts(&mut self) { todo!() }
    fn create_pipelines(&mut self) { todo!() }
    fn update_descriptor_sets(&mut self, _image_view: vk::ImageView, _image_index: usize) { todo!() }
    fn upload_images(&mut self) { todo!() }
}

impl AntiAliasPass for Fxaa {
    /// Port of `FXAA::Draw`.
    fn draw(
        &mut self,
        _image_index: usize,
        _inout_image: &mut vk::Image,
        _inout_image_view: &mut vk::ImageView,
    ) {
        todo!("Fxaa::draw")
    }
}
