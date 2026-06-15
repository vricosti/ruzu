// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/filters.h and filters.cpp
//!
//! Factory functions for creating window adapt passes with different scaling filters.
//! Each filter creates a WindowAdaptPass configured with the appropriate sampler
//! (nearest or bilinear) and fragment shader source.

use super::util;
use super::window_adapt_pass::WindowAdaptPass;
use crate::host_shaders::fragment_shaders::{
    OPENGL_PRESENT_FRAG, OPENGL_PRESENT_SCALEFORCE_FRAG, PRESENT_BICUBIC_FRAG,
    PRESENT_GAUSSIAN_FRAG,
};
use crate::renderer_opengl::Device;

// ---------------------------------------------------------------------------
// Factory functions
// ---------------------------------------------------------------------------

/// Create a nearest-neighbor scaling filter pass.
///
/// Port of `OpenGL::MakeNearestNeighbor()`.
pub fn make_nearest_neighbor(device: *const Device) -> WindowAdaptPass {
    let sampler = util::create_nearest_neighbor_sampler();
    WindowAdaptPass::new(device, sampler, OPENGL_PRESENT_FRAG)
}

/// Create a bilinear scaling filter pass.
///
/// Port of `OpenGL::MakeBilinear()`.
pub fn make_bilinear(device: *const Device) -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler();
    WindowAdaptPass::new(device, sampler, OPENGL_PRESENT_FRAG)
}

/// Create a bicubic scaling filter pass.
///
/// Port of `OpenGL::MakeBicubic()`.
pub fn make_bicubic(device: *const Device) -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler();
    WindowAdaptPass::new(device, sampler, PRESENT_BICUBIC_FRAG)
}

/// Create a Gaussian scaling filter pass.
///
/// Port of `OpenGL::MakeGaussian()`.
pub fn make_gaussian(device: *const Device) -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler();
    WindowAdaptPass::new(device, sampler, PRESENT_GAUSSIAN_FRAG)
}

/// Create a ScaleForce scaling filter pass.
///
/// Port of `OpenGL::MakeScaleForce()`.
/// Upstream prepends `#version 460\n` to the scaleforce shader source.
pub fn make_scale_force(device: *const Device) -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler();
    let source = format!("#version 460\n{}", OPENGL_PRESENT_SCALEFORCE_FRAG);
    WindowAdaptPass::new(device, sampler, &source)
}

/// Scaling filter enum for dispatching.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalingFilter {
    NearestNeighbor,
    Bilinear,
    Bicubic,
    Gaussian,
    ScaleForce,
    Fsr,
}

/// Create the appropriate scaling filter based on the enum variant.
pub fn make_filter(filter: ScalingFilter, device: *const Device) -> WindowAdaptPass {
    match filter {
        ScalingFilter::NearestNeighbor => make_nearest_neighbor(device),
        ScalingFilter::Bilinear | ScalingFilter::Fsr => make_bilinear(device),
        ScalingFilter::Bicubic => make_bicubic(device),
        ScalingFilter::Gaussian => make_gaussian(device),
        ScalingFilter::ScaleForce => make_scale_force(device),
    }
}
