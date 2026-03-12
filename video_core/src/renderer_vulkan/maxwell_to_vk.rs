// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `maxwell_to_vk.h` / `maxwell_to_vk.cpp`.
//!
//! Conversion functions from Maxwell GPU enumerations and types to their
//! Vulkan equivalents.

use ash::vk;

// ---------------------------------------------------------------------------
// FormatInfo
// ---------------------------------------------------------------------------

/// Port of `MaxwellToVK::FormatInfo`.
#[derive(Debug, Clone, Copy)]
pub struct FormatInfo {
    pub format: vk::Format,
    pub attachable: bool,
    pub storage: bool,
}

// ---------------------------------------------------------------------------
// Sampler namespace conversions
// ---------------------------------------------------------------------------

/// Port of `MaxwellToVK::Sampler` namespace.
pub mod sampler {
    use ash::vk;

    /// Port of `Sampler::Filter`.
    pub fn filter(_filter: u32) -> vk::Filter {
        todo!("sampler::filter")
    }

    /// Port of `Sampler::MipmapMode`.
    pub fn mipmap_mode(_mipmap_filter: u32) -> vk::SamplerMipmapMode {
        todo!("sampler::mipmap_mode")
    }

    /// Port of `Sampler::WrapMode`.
    pub fn wrap_mode(_wrap_mode: u32, _filter: u32) -> vk::SamplerAddressMode {
        todo!("sampler::wrap_mode")
    }

    /// Port of `Sampler::DepthCompareFunction`.
    pub fn depth_compare_function(_func: u32) -> vk::CompareOp {
        todo!("sampler::depth_compare_function")
    }
}

// ---------------------------------------------------------------------------
// Top-level conversion functions
// ---------------------------------------------------------------------------

/// Port of `MaxwellToVK::SurfaceFormat`.
pub fn surface_format(
    _format_type: u32,
    _with_srgb: bool,
    _pixel_format: u32,
) -> FormatInfo {
    todo!("surface_format")
}

/// Port of `MaxwellToVK::ShaderStage`.
pub fn shader_stage(_stage: u32) -> vk::ShaderStageFlags {
    todo!("shader_stage")
}

/// Port of `MaxwellToVK::PrimitiveTopology`.
pub fn primitive_topology(_topology: u32) -> vk::PrimitiveTopology {
    todo!("primitive_topology")
}

/// Port of `MaxwellToVK::VertexFormat`.
pub fn vertex_format(_type_: u32, _size: u32) -> vk::Format {
    todo!("vertex_format")
}

/// Port of `MaxwellToVK::ComparisonOp`.
pub fn comparison_op(_comparison: u32) -> vk::CompareOp {
    todo!("comparison_op")
}

/// Port of `MaxwellToVK::IndexFormat`.
pub fn index_format(_index_format: u32) -> vk::IndexType {
    todo!("index_format")
}

/// Port of `MaxwellToVK::StencilOp`.
pub fn stencil_op(_stencil_op: u32) -> vk::StencilOp {
    todo!("stencil_op")
}

/// Port of `MaxwellToVK::BlendEquation`.
pub fn blend_equation(_equation: u32) -> vk::BlendOp {
    todo!("blend_equation")
}

/// Port of `MaxwellToVK::BlendFactor`.
pub fn blend_factor(_factor: u32) -> vk::BlendFactor {
    todo!("blend_factor")
}

/// Port of `MaxwellToVK::FrontFace`.
pub fn front_face(_front_face: u32) -> vk::FrontFace {
    todo!("front_face")
}

/// Port of `MaxwellToVK::CullFace`.
pub fn cull_face(_cull_face: u32) -> vk::CullModeFlags {
    todo!("cull_face")
}

/// Port of `MaxwellToVK::PolygonMode`.
pub fn polygon_mode(_polygon_mode: u32) -> vk::PolygonMode {
    todo!("polygon_mode")
}

/// Port of `MaxwellToVK::SwizzleSource`.
pub fn swizzle_source(_swizzle: u32) -> vk::ComponentSwizzle {
    todo!("swizzle_source")
}

/// Port of `MaxwellToVK::ViewportSwizzle`.
pub fn viewport_swizzle(_swizzle: u32) -> vk::ViewportCoordinateSwizzleNV {
    todo!("viewport_swizzle")
}

/// Port of `MaxwellToVK::SamplerReduction`.
pub fn sampler_reduction(_reduction: u32) -> vk::SamplerReductionMode {
    todo!("sampler_reduction")
}

/// Port of `MaxwellToVK::MsaaMode`.
pub fn msaa_mode(_msaa_mode: u32) -> vk::SampleCountFlags {
    todo!("msaa_mode")
}
