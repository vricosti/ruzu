// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/fsr.h` / `present/fsr.cpp`.
//!
//! AMD FidelityFX Super Resolution (FSR) 1.0 upscaling pass.

use ash::vk;

// ---------------------------------------------------------------------------
// FSR stage enum
// ---------------------------------------------------------------------------

/// Port of `FSR::FsrStage` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum FsrStage {
    Easu = 0,
    Rcas = 1,
}

/// Total number of FSR stages.
pub const MAX_FSR_STAGE: usize = 2;

// ---------------------------------------------------------------------------
// Per-image dynamic resources
// ---------------------------------------------------------------------------

/// Port of `FSR::Images` inner struct.
pub struct FsrImages {
    pub images: [vk::Image; MAX_FSR_STAGE],
    pub image_views: [vk::ImageView; MAX_FSR_STAGE],
    pub framebuffers: [vk::Framebuffer; MAX_FSR_STAGE],
}

// ---------------------------------------------------------------------------
// FSR
// ---------------------------------------------------------------------------

/// Port of `FSR` class.
///
/// Two-pass (EASU + RCAS) FidelityFX Super Resolution upscaling.
pub struct Fsr {
    _extent: vk::Extent2D,
    _image_count: usize,
    _images_ready: bool,
    _dynamic_images: Vec<FsrImages>,
}

impl Fsr {
    /// Port of `FSR::FSR`.
    pub fn new(
        _extent: vk::Extent2D,
        _image_count: usize,
    ) -> Self {
        todo!("Fsr::new")
    }

    /// Port of `FSR::Draw`.
    pub fn draw(
        &mut self,
        _image_index: usize,
        _source_image: vk::Image,
        _source_image_view: vk::ImageView,
        _input_image_extent: vk::Extent2D,
        _crop_rect: [f32; 4],
    ) -> vk::ImageView {
        todo!("Fsr::draw")
    }

    // --- Private ---
    fn create_images(&mut self) { todo!() }
    fn create_render_passes(&mut self) { todo!() }
    fn create_sampler(&mut self) { todo!() }
    fn create_shaders(&mut self) { todo!() }
    fn create_descriptor_pool(&mut self) { todo!() }
    fn create_descriptor_set_layout(&mut self) { todo!() }
    fn create_descriptor_sets(&mut self) { todo!() }
    fn create_pipeline_layouts(&mut self) { todo!() }
    fn create_pipelines(&mut self) { todo!() }
    fn upload_images(&mut self) { todo!() }
    fn update_descriptor_sets(&mut self, _image_view: vk::ImageView, _image_index: usize) { todo!() }
}
