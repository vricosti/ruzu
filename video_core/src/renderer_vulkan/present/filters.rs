// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/filters.h` / `present/filters.cpp`.
//!
//! Factory functions for creating window adaptation passes with different
//! scaling filters. Each filter creates a WindowAdaptPass configured with
//! the appropriate sampler (nearest or bilinear) and fragment shader.

use ash::vk;

use crate::host_shaders::spirv_shaders::{
    PRESENT_BICUBIC_FRAG_SPV, PRESENT_GAUSSIAN_FRAG_SPV, VULKAN_PRESENT_FRAG_SPV,
    VULKAN_PRESENT_SCALEFORCE_FP16_FRAG_SPV, VULKAN_PRESENT_SCALEFORCE_FP32_FRAG_SPV,
};
use crate::renderer_vulkan::shader_util::build_shader;

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
    let fragment_shader =
        build_shader(device, VULKAN_PRESENT_FRAG_SPV).expect("Failed to build vulkan_present.frag");
    WindowAdaptPass::new(device.clone(), frame_format, sampler, fragment_shader)
}

/// Port of `MakeBilinear`.
///
/// Creates a window adapt pass using bilinear sampling and the basic
/// present fragment shader.
pub fn make_bilinear(device: &ash::Device, frame_format: vk::Format) -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler(device);
    let fragment_shader =
        build_shader(device, VULKAN_PRESENT_FRAG_SPV).expect("Failed to build vulkan_present.frag");
    WindowAdaptPass::new(device.clone(), frame_format, sampler, fragment_shader)
}

/// Port of `MakeBicubic`.
///
/// Creates a window adapt pass using bilinear sampling with the
/// bicubic interpolation fragment shader.
pub fn make_bicubic(device: &ash::Device, frame_format: vk::Format) -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler(device);
    let fragment_shader = build_shader(device, PRESENT_BICUBIC_FRAG_SPV)
        .expect("Failed to build present_bicubic.frag");
    WindowAdaptPass::new(device.clone(), frame_format, sampler, fragment_shader)
}

/// Port of `MakeGaussian`.
///
/// Creates a window adapt pass using bilinear sampling with the
/// Gaussian blur fragment shader.
pub fn make_gaussian(device: &ash::Device, frame_format: vk::Format) -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler(device);
    let fragment_shader = build_shader(device, PRESENT_GAUSSIAN_FRAG_SPV)
        .expect("Failed to build present_gaussian.frag");
    WindowAdaptPass::new(device.clone(), frame_format, sampler, fragment_shader)
}

/// Port of `MakeScaleForce`.
///
/// Creates a window adapt pass using bilinear sampling with the
/// ScaleForce shader (fp16 preferred, fp32 fallback).
pub fn make_scale_force(
    device: &ash::Device,
    frame_format: vk::Format,
    supports_float16: bool,
) -> WindowAdaptPass {
    let sampler = util::create_bilinear_sampler(device);
    let (shader_spv, shader_name) = if supports_float16 {
        (
            VULKAN_PRESENT_SCALEFORCE_FP16_FRAG_SPV,
            "vulkan_present_scaleforce_fp16.frag",
        )
    } else {
        (
            VULKAN_PRESENT_SCALEFORCE_FP32_FRAG_SPV,
            "vulkan_present_scaleforce_fp32.frag",
        )
    };
    let fragment_shader = build_shader(device, shader_spv)
        .unwrap_or_else(|_| panic!("Failed to build {shader_name}"));
    WindowAdaptPass::new(device.clone(), frame_format, sampler, fragment_shader)
}

/// Creates the appropriate scaling filter based on the enum variant.
pub fn make_filter(
    device: &ash::Device,
    frame_format: vk::Format,
    filter: ScalingFilter,
    supports_float16: bool,
) -> WindowAdaptPass {
    match filter {
        ScalingFilter::NearestNeighbor => make_nearest_neighbor(device, frame_format),
        ScalingFilter::Bilinear => make_bilinear(device, frame_format),
        ScalingFilter::Bicubic => make_bicubic(device, frame_format),
        ScalingFilter::Gaussian => make_gaussian(device, frame_format),
        ScalingFilter::ScaleForce => make_scale_force(device, frame_format, supports_float16),
    }
}
