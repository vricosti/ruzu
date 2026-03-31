// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/filters.h` / `present/filters.cpp`.
//!
//! Factory functions for creating window adaptation passes with different
//! scaling filters. Each filter creates a WindowAdaptPass configured with
//! the appropriate sampler (nearest or bilinear) and fragment shader.

use ash::vk;

use super::util;
use super::window_adapt_pass::WindowAdaptPass;

// ---------------------------------------------------------------------------
// ScalingFilter enum
// ---------------------------------------------------------------------------

/// Port of the scaling filter selection from upstream `filters.h`.
///
/// Enumerates the available window scaling filters.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalingFilter {
    NearestNeighbor,
    Bilinear,
    Bicubic,
    Gaussian,
    ScaleForce,
}

// ---------------------------------------------------------------------------
// Factory functions
// ---------------------------------------------------------------------------

/// Port of `MakeNearestNeighbor`.
///
/// Creates a window adapt pass using nearest-neighbor sampling and
/// the basic present fragment shader.
pub fn make_nearest_neighbor(device: &ash::Device, frame_format: vk::Format) -> WindowAdaptPass {
    let sampler = util::create_nearest_neighbor_sampler(device);
    WindowAdaptPass::new(
        device.clone(),
        frame_format,
        sampler,
        vk::ShaderModule::null(),
    )
}

/// Port of `MakeBilinear`.
///
/// Creates a window adapt pass using bilinear sampling and the basic
/// present fragment shader.
pub fn make_bilinear(device: &ash::Device, frame_format: vk::Format) -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler(device);
    WindowAdaptPass::new(
        device.clone(),
        frame_format,
        sampler,
        vk::ShaderModule::null(),
    )
}

/// Port of `MakeBicubic`.
///
/// Creates a window adapt pass using bilinear sampling with the
/// bicubic interpolation fragment shader.
pub fn make_bicubic(device: &ash::Device, frame_format: vk::Format) -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler(device);
    WindowAdaptPass::new(
        device.clone(),
        frame_format,
        sampler,
        vk::ShaderModule::null(),
    )
}

/// Port of `MakeGaussian`.
///
/// Creates a window adapt pass using bilinear sampling with the
/// Gaussian blur fragment shader.
pub fn make_gaussian(device: &ash::Device, frame_format: vk::Format) -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler(device);
    WindowAdaptPass::new(
        device.clone(),
        frame_format,
        sampler,
        vk::ShaderModule::null(),
    )
}

/// Port of `MakeScaleForce`.
///
/// Creates a window adapt pass using bilinear sampling with the
/// ScaleForce shader (fp16 preferred, fp32 fallback).
pub fn make_scale_force(device: &ash::Device, frame_format: vk::Format) -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler(device);
    WindowAdaptPass::new(
        device.clone(),
        frame_format,
        sampler,
        vk::ShaderModule::null(),
    )
}

/// Creates the appropriate scaling filter based on the enum variant.
pub fn make_filter(
    device: &ash::Device,
    frame_format: vk::Format,
    filter: ScalingFilter,
) -> WindowAdaptPass {
    match filter {
        ScalingFilter::NearestNeighbor => make_nearest_neighbor(device, frame_format),
        ScalingFilter::Bilinear => make_bilinear(device, frame_format),
        ScalingFilter::Bicubic => make_bicubic(device, frame_format),
        ScalingFilter::Gaussian => make_gaussian(device, frame_format),
        ScalingFilter::ScaleForce => make_scale_force(device, frame_format),
    }
}
