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

// ---------------------------------------------------------------------------
// Factory functions
// ---------------------------------------------------------------------------

/// Create a nearest-neighbor scaling filter pass.
///
/// Port of `OpenGL::MakeNearestNeighbor()`.
pub fn make_nearest_neighbor() -> WindowAdaptPass {
    let sampler = util::create_nearest_neighbor_sampler();
    WindowAdaptPass::new(sampler, OPENGL_PRESENT_FRAG)
}

/// Create a bilinear scaling filter pass.
///
/// Port of `OpenGL::MakeBilinear()`.
pub fn make_bilinear() -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler();
    WindowAdaptPass::new(sampler, OPENGL_PRESENT_FRAG)
}

/// Create a bicubic scaling filter pass.
///
/// Port of `OpenGL::MakeBicubic()`.
pub fn make_bicubic() -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler();
    WindowAdaptPass::new(sampler, PRESENT_BICUBIC_FRAG)
}

/// Create a Gaussian scaling filter pass.
///
/// Port of `OpenGL::MakeGaussian()`.
pub fn make_gaussian() -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler();
    WindowAdaptPass::new(sampler, PRESENT_GAUSSIAN_FRAG)
}

/// Create a ScaleForce scaling filter pass.
///
/// Port of `OpenGL::MakeScaleForce()`.
/// Upstream prepends `#version 460\n` to the scaleforce shader source.
pub fn make_scale_force() -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler();
    WindowAdaptPass::new(sampler, OPENGL_PRESENT_SCALEFORCE_FRAG)
}

/// Scaling filter enum for dispatching.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalingFilter {
    NearestNeighbor,
    Bilinear,
    Bicubic,
    Gaussian,
    ScaleForce,
}

/// Create the appropriate scaling filter based on the enum variant.
pub fn make_filter(filter: ScalingFilter) -> WindowAdaptPass {
    match filter {
        ScalingFilter::NearestNeighbor => make_nearest_neighbor(),
        ScalingFilter::Bilinear => make_bilinear(),
        ScalingFilter::Bicubic => make_bicubic(),
        ScalingFilter::Gaussian => make_gaussian(),
        ScalingFilter::ScaleForce => make_scale_force(),
    }
}
