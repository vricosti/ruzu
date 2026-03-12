// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/smaa.h` / `present/smaa.cpp`.
//!
//! Subpixel Morphological Anti-Aliasing (SMAA) post-processing pass.

use ash::vk;

use super::anti_alias_pass::AntiAliasPass;

// ---------------------------------------------------------------------------
// Enums
// ---------------------------------------------------------------------------

/// Port of `SMAA::SMAAStage` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SmaaStage {
    EdgeDetection = 0,
    BlendingWeightCalculation = 1,
    NeighborhoodBlending = 2,
}

pub const MAX_SMAA_STAGE: usize = 3;

/// Port of `SMAA::StaticImageType` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum StaticImageType {
    Area = 0,
    Search = 1,
}

pub const MAX_STATIC_IMAGE: usize = 2;

/// Port of `SMAA::DynamicImageType` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
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
    _extent: vk::Extent2D,
    _image_count: u32,
    _images_ready: bool,
    _dynamic_images: Vec<SmaaImages>,
    _static_images: [vk::Image; MAX_STATIC_IMAGE],
    _static_image_views: [vk::ImageView; MAX_STATIC_IMAGE],
}

impl Smaa {
    /// Port of `SMAA::SMAA`.
    pub fn new(
        _extent: vk::Extent2D,
        _image_count: usize,
    ) -> Self {
        todo!("Smaa::new")
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

impl AntiAliasPass for Smaa {
    /// Port of `SMAA::Draw`.
    fn draw(
        &mut self,
        _image_index: usize,
        _inout_image: &mut vk::Image,
        _inout_image_view: &mut vk::ImageView,
    ) {
        todo!("Smaa::draw")
    }
}
