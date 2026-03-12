// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `maxwell_to_vk.h` / `maxwell_to_vk.cpp`.
//!
//! Conversion functions from Maxwell GPU enumerations and types to their
//! Vulkan equivalents.

use ash::vk;

use crate::engines::maxwell_3d::{
    BlendEquation, BlendFactor, ComparisonOp, CullFace, FrontFace, IndexFormat, PolygonMode,
    PrimitiveTopology, StencilOp, VertexAttribSize, VertexAttribType,
};
use crate::shader_recompiler::stage::Stage;
use crate::texture_cache::format_lookup_table::PixelFormat;
use crate::textures::texture::{
    DepthCompareFunc, MsaaMode, SamplerReduction, SwizzleSource, TextureFilter,
    TextureMipmapFilter, WrapMode,
};

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
// Format table constants
// ---------------------------------------------------------------------------

const ATTACHABLE: u32 = 1 << 0;
const STORAGE: u32 = 1 << 1;

struct FormatTuple {
    format: vk::Format,
    usage: u32,
}

/// Texture format table indexed by PixelFormat.
/// Port of anonymous `tex_format_tuples[]` in maxwell_to_vk.cpp.
const TEX_FORMAT_TUPLES: &[FormatTuple] = &[
    FormatTuple { format: vk::Format::A8B8G8R8_UNORM_PACK32, usage: ATTACHABLE | STORAGE },    // A8B8G8R8_UNORM
    FormatTuple { format: vk::Format::A8B8G8R8_SNORM_PACK32, usage: ATTACHABLE | STORAGE },    // A8B8G8R8_SNORM
    FormatTuple { format: vk::Format::A8B8G8R8_SINT_PACK32, usage: ATTACHABLE | STORAGE },     // A8B8G8R8_SINT
    FormatTuple { format: vk::Format::A8B8G8R8_UINT_PACK32, usage: ATTACHABLE | STORAGE },     // A8B8G8R8_UINT
    FormatTuple { format: vk::Format::R5G6B5_UNORM_PACK16, usage: ATTACHABLE },                // R5G6B5_UNORM
    FormatTuple { format: vk::Format::B5G6R5_UNORM_PACK16, usage: 0 },                         // B5G6R5_UNORM
    FormatTuple { format: vk::Format::A1R5G5B5_UNORM_PACK16, usage: ATTACHABLE },              // A1R5G5B5_UNORM
    FormatTuple { format: vk::Format::A2B10G10R10_UNORM_PACK32, usage: ATTACHABLE | STORAGE }, // A2B10G10R10_UNORM
    FormatTuple { format: vk::Format::A2B10G10R10_UINT_PACK32, usage: ATTACHABLE | STORAGE },  // A2B10G10R10_UINT
    FormatTuple { format: vk::Format::A2R10G10B10_UNORM_PACK32, usage: ATTACHABLE },           // A2R10G10B10_UNORM
    FormatTuple { format: vk::Format::A1R5G5B5_UNORM_PACK16, usage: ATTACHABLE },              // A1B5G5R5_UNORM (flipped with swizzle)
    FormatTuple { format: vk::Format::R5G5B5A1_UNORM_PACK16, usage: 0 },                      // A5B5G5R1_UNORM (specially swizzled)
    FormatTuple { format: vk::Format::R8_UNORM, usage: ATTACHABLE | STORAGE },                 // R8_UNORM
    FormatTuple { format: vk::Format::R8_SNORM, usage: ATTACHABLE | STORAGE },                 // R8_SNORM
    FormatTuple { format: vk::Format::R8_SINT, usage: ATTACHABLE | STORAGE },                  // R8_SINT
    FormatTuple { format: vk::Format::R8_UINT, usage: ATTACHABLE | STORAGE },                  // R8_UINT
    FormatTuple { format: vk::Format::R16G16B16A16_SFLOAT, usage: ATTACHABLE | STORAGE },      // R16G16B16A16_FLOAT
    FormatTuple { format: vk::Format::R16G16B16A16_UNORM, usage: ATTACHABLE | STORAGE },       // R16G16B16A16_UNORM
    FormatTuple { format: vk::Format::R16G16B16A16_SNORM, usage: ATTACHABLE | STORAGE },       // R16G16B16A16_SNORM
    FormatTuple { format: vk::Format::R16G16B16A16_SINT, usage: ATTACHABLE | STORAGE },        // R16G16B16A16_SINT
    FormatTuple { format: vk::Format::R16G16B16A16_UINT, usage: ATTACHABLE | STORAGE },        // R16G16B16A16_UINT
    FormatTuple { format: vk::Format::B10G11R11_UFLOAT_PACK32, usage: ATTACHABLE | STORAGE },  // B10G11R11_FLOAT
    FormatTuple { format: vk::Format::R32G32B32A32_UINT, usage: ATTACHABLE | STORAGE },        // R32G32B32A32_UINT
    FormatTuple { format: vk::Format::BC1_RGBA_UNORM_BLOCK, usage: 0 },                       // BC1_RGBA_UNORM
    FormatTuple { format: vk::Format::BC2_UNORM_BLOCK, usage: 0 },                            // BC2_UNORM
    FormatTuple { format: vk::Format::BC3_UNORM_BLOCK, usage: 0 },                            // BC3_UNORM
    FormatTuple { format: vk::Format::BC4_UNORM_BLOCK, usage: 0 },                            // BC4_UNORM
    FormatTuple { format: vk::Format::BC4_SNORM_BLOCK, usage: 0 },                            // BC4_SNORM
    FormatTuple { format: vk::Format::BC5_UNORM_BLOCK, usage: 0 },                            // BC5_UNORM
    FormatTuple { format: vk::Format::BC5_SNORM_BLOCK, usage: 0 },                            // BC5_SNORM
    FormatTuple { format: vk::Format::BC7_UNORM_BLOCK, usage: 0 },                            // BC7_UNORM
    FormatTuple { format: vk::Format::BC6H_UFLOAT_BLOCK, usage: 0 },                          // BC6H_UFLOAT
    FormatTuple { format: vk::Format::BC6H_SFLOAT_BLOCK, usage: 0 },                          // BC6H_SFLOAT
    FormatTuple { format: vk::Format::ASTC_4X4_UNORM_BLOCK, usage: 0 },                       // ASTC_2D_4X4_UNORM
    FormatTuple { format: vk::Format::B8G8R8A8_UNORM, usage: ATTACHABLE | STORAGE },          // B8G8R8A8_UNORM
    FormatTuple { format: vk::Format::R32G32B32A32_SFLOAT, usage: ATTACHABLE | STORAGE },     // R32G32B32A32_FLOAT
    FormatTuple { format: vk::Format::R32G32B32A32_SINT, usage: ATTACHABLE | STORAGE },       // R32G32B32A32_SINT
    FormatTuple { format: vk::Format::R32G32_SFLOAT, usage: ATTACHABLE | STORAGE },           // R32G32_FLOAT
    FormatTuple { format: vk::Format::R32G32_SINT, usage: ATTACHABLE | STORAGE },             // R32G32_SINT
    FormatTuple { format: vk::Format::R32_SFLOAT, usage: ATTACHABLE | STORAGE },              // R32_FLOAT
    FormatTuple { format: vk::Format::R16_SFLOAT, usage: ATTACHABLE | STORAGE },              // R16_FLOAT
    FormatTuple { format: vk::Format::R16_UNORM, usage: ATTACHABLE | STORAGE },               // R16_UNORM
    FormatTuple { format: vk::Format::R16_SNORM, usage: ATTACHABLE | STORAGE },               // R16_SNORM
    FormatTuple { format: vk::Format::R16_UINT, usage: ATTACHABLE | STORAGE },                // R16_UINT
    FormatTuple { format: vk::Format::R16_SINT, usage: ATTACHABLE | STORAGE },                // R16_SINT
    FormatTuple { format: vk::Format::R16G16_UNORM, usage: ATTACHABLE | STORAGE },            // R16G16_UNORM
    FormatTuple { format: vk::Format::R16G16_SFLOAT, usage: ATTACHABLE | STORAGE },           // R16G16_FLOAT
    FormatTuple { format: vk::Format::R16G16_UINT, usage: ATTACHABLE | STORAGE },             // R16G16_UINT
    FormatTuple { format: vk::Format::R16G16_SINT, usage: ATTACHABLE | STORAGE },             // R16G16_SINT
    FormatTuple { format: vk::Format::R16G16_SNORM, usage: ATTACHABLE | STORAGE },            // R16G16_SNORM
    FormatTuple { format: vk::Format::R32G32B32_SFLOAT, usage: 0 },                           // R32G32B32_FLOAT
    FormatTuple { format: vk::Format::A8B8G8R8_SRGB_PACK32, usage: ATTACHABLE },              // A8B8G8R8_SRGB
    FormatTuple { format: vk::Format::R8G8_UNORM, usage: ATTACHABLE | STORAGE },              // R8G8_UNORM
    FormatTuple { format: vk::Format::R8G8_SNORM, usage: ATTACHABLE | STORAGE },              // R8G8_SNORM
    FormatTuple { format: vk::Format::R8G8_SINT, usage: ATTACHABLE | STORAGE },               // R8G8_SINT
    FormatTuple { format: vk::Format::R8G8_UINT, usage: ATTACHABLE | STORAGE },               // R8G8_UINT
    FormatTuple { format: vk::Format::R32G32_UINT, usage: ATTACHABLE | STORAGE },             // R32G32_UINT
    FormatTuple { format: vk::Format::R16G16B16A16_SFLOAT, usage: ATTACHABLE | STORAGE },     // R16G16B16X16_FLOAT
    FormatTuple { format: vk::Format::R32_UINT, usage: ATTACHABLE | STORAGE },                // R32_UINT
    FormatTuple { format: vk::Format::R32_SINT, usage: ATTACHABLE | STORAGE },                // R32_SINT
    FormatTuple { format: vk::Format::ASTC_8X8_UNORM_BLOCK, usage: 0 },                       // ASTC_2D_8X8_UNORM
    FormatTuple { format: vk::Format::ASTC_8X5_UNORM_BLOCK, usage: 0 },                       // ASTC_2D_8X5_UNORM
    FormatTuple { format: vk::Format::ASTC_5X4_UNORM_BLOCK, usage: 0 },                       // ASTC_2D_5X4_UNORM
    FormatTuple { format: vk::Format::B8G8R8A8_SRGB, usage: ATTACHABLE },                     // B8G8R8A8_SRGB
    FormatTuple { format: vk::Format::BC1_RGBA_SRGB_BLOCK, usage: 0 },                        // BC1_RGBA_SRGB
    FormatTuple { format: vk::Format::BC2_SRGB_BLOCK, usage: 0 },                             // BC2_SRGB
    FormatTuple { format: vk::Format::BC3_SRGB_BLOCK, usage: 0 },                             // BC3_SRGB
    FormatTuple { format: vk::Format::BC7_SRGB_BLOCK, usage: 0 },                             // BC7_SRGB
    FormatTuple { format: vk::Format::A4B4G4R4_UNORM_PACK16, usage: 0 },                      // A4B4G4R4_UNORM
    FormatTuple { format: vk::Format::R4G4_UNORM_PACK8, usage: 0 },                           // G4R4_UNORM
    FormatTuple { format: vk::Format::ASTC_4X4_SRGB_BLOCK, usage: 0 },                        // ASTC_2D_4X4_SRGB
    FormatTuple { format: vk::Format::ASTC_8X8_SRGB_BLOCK, usage: 0 },                        // ASTC_2D_8X8_SRGB
    FormatTuple { format: vk::Format::ASTC_8X5_SRGB_BLOCK, usage: 0 },                        // ASTC_2D_8X5_SRGB
    FormatTuple { format: vk::Format::ASTC_5X4_SRGB_BLOCK, usage: 0 },                        // ASTC_2D_5X4_SRGB
    FormatTuple { format: vk::Format::ASTC_5X5_UNORM_BLOCK, usage: 0 },                       // ASTC_2D_5X5_UNORM
    FormatTuple { format: vk::Format::ASTC_5X5_SRGB_BLOCK, usage: 0 },                        // ASTC_2D_5X5_SRGB
    FormatTuple { format: vk::Format::ASTC_10X8_UNORM_BLOCK, usage: 0 },                      // ASTC_2D_10X8_UNORM
    FormatTuple { format: vk::Format::ASTC_10X8_SRGB_BLOCK, usage: 0 },                       // ASTC_2D_10X8_SRGB
    FormatTuple { format: vk::Format::ASTC_6X6_UNORM_BLOCK, usage: 0 },                       // ASTC_2D_6X6_UNORM
    FormatTuple { format: vk::Format::ASTC_6X6_SRGB_BLOCK, usage: 0 },                        // ASTC_2D_6X6_SRGB
    FormatTuple { format: vk::Format::ASTC_10X6_UNORM_BLOCK, usage: 0 },                      // ASTC_2D_10X6_UNORM
    FormatTuple { format: vk::Format::ASTC_10X6_SRGB_BLOCK, usage: 0 },                       // ASTC_2D_10X6_SRGB
    FormatTuple { format: vk::Format::ASTC_10X5_UNORM_BLOCK, usage: 0 },                      // ASTC_2D_10X5_UNORM
    FormatTuple { format: vk::Format::ASTC_10X5_SRGB_BLOCK, usage: 0 },                       // ASTC_2D_10X5_SRGB
    FormatTuple { format: vk::Format::ASTC_10X10_UNORM_BLOCK, usage: 0 },                     // ASTC_2D_10X10_UNORM
    FormatTuple { format: vk::Format::ASTC_10X10_SRGB_BLOCK, usage: 0 },                      // ASTC_2D_10X10_SRGB
    FormatTuple { format: vk::Format::ASTC_12X10_UNORM_BLOCK, usage: 0 },                     // ASTC_2D_12X10_UNORM
    FormatTuple { format: vk::Format::ASTC_12X10_SRGB_BLOCK, usage: 0 },                      // ASTC_2D_12X10_SRGB
    FormatTuple { format: vk::Format::ASTC_12X12_UNORM_BLOCK, usage: 0 },                     // ASTC_2D_12X12_UNORM
    FormatTuple { format: vk::Format::ASTC_12X12_SRGB_BLOCK, usage: 0 },                      // ASTC_2D_12X12_SRGB
    FormatTuple { format: vk::Format::ASTC_8X6_UNORM_BLOCK, usage: 0 },                       // ASTC_2D_8X6_UNORM
    FormatTuple { format: vk::Format::ASTC_8X6_SRGB_BLOCK, usage: 0 },                        // ASTC_2D_8X6_SRGB
    FormatTuple { format: vk::Format::ASTC_6X5_UNORM_BLOCK, usage: 0 },                       // ASTC_2D_6X5_UNORM
    FormatTuple { format: vk::Format::ASTC_6X5_SRGB_BLOCK, usage: 0 },                        // ASTC_2D_6X5_SRGB
    FormatTuple { format: vk::Format::E5B9G9R9_UFLOAT_PACK32, usage: 0 },                     // E5B9G9R9_FLOAT
    // Depth formats
    FormatTuple { format: vk::Format::D32_SFLOAT, usage: ATTACHABLE },                        // D32_FLOAT
    FormatTuple { format: vk::Format::D16_UNORM, usage: ATTACHABLE },                         // D16_UNORM
    FormatTuple { format: vk::Format::X8_D24_UNORM_PACK32, usage: ATTACHABLE },               // X8_D24_UNORM
    // Stencil formats
    FormatTuple { format: vk::Format::S8_UINT, usage: ATTACHABLE },                           // S8_UINT
    // DepthStencil formats
    FormatTuple { format: vk::Format::D24_UNORM_S8_UINT, usage: ATTACHABLE },                 // D24_UNORM_S8_UINT
    FormatTuple { format: vk::Format::D24_UNORM_S8_UINT, usage: ATTACHABLE },                 // S8_UINT_D24_UNORM (emulated)
    FormatTuple { format: vk::Format::D32_SFLOAT_S8_UINT, usage: ATTACHABLE },                // D32_FLOAT_S8_UINT
];

/// Returns true if the pixel format is a depth or stencil format.
fn is_zeta_format(pixel_format: PixelFormat) -> bool {
    // MaxColorFormat starts at E5B9G9R9Float+1 = D32Float
    let val = pixel_format as u32;
    val >= PixelFormat::D32Float as u32 && val < PixelFormat::MaxDepthStencilFormat as u32
}

// ---------------------------------------------------------------------------
// Sampler namespace conversions
// ---------------------------------------------------------------------------

/// Port of `MaxwellToVK::Sampler` namespace.
pub mod sampler {
    use super::*;

    /// Port of `Sampler::Filter`.
    pub fn filter(tex_filter: TextureFilter) -> vk::Filter {
        match tex_filter {
            TextureFilter::Nearest => vk::Filter::NEAREST,
            TextureFilter::Linear => vk::Filter::LINEAR,
        }
    }

    /// Port of `Sampler::MipmapMode`.
    pub fn mipmap_mode(mipmap_filter: TextureMipmapFilter) -> vk::SamplerMipmapMode {
        match mipmap_filter {
            // There are no Vulkan filter modes that directly correspond to OpenGL minification
            // filters of GL_LINEAR or GL_NEAREST, but they can be emulated using
            // VK_SAMPLER_MIPMAP_MODE_NEAREST, minLod = 0, and maxLod = 0.25.
            TextureMipmapFilter::None => vk::SamplerMipmapMode::NEAREST,
            TextureMipmapFilter::Nearest => vk::SamplerMipmapMode::NEAREST,
            TextureMipmapFilter::Linear => vk::SamplerMipmapMode::LINEAR,
        }
    }

    /// Port of `Sampler::WrapMode`.
    pub fn wrap_mode(
        is_nvidia: bool,
        wrap: WrapMode,
        tex_filter: TextureFilter,
    ) -> vk::SamplerAddressMode {
        match wrap {
            WrapMode::Wrap => vk::SamplerAddressMode::REPEAT,
            WrapMode::Mirror => vk::SamplerAddressMode::MIRRORED_REPEAT,
            WrapMode::ClampToEdge => vk::SamplerAddressMode::CLAMP_TO_EDGE,
            WrapMode::Border => vk::SamplerAddressMode::CLAMP_TO_BORDER,
            WrapMode::Clamp => {
                if is_nvidia {
                    // Nvidia's Vulkan driver defaults to GL_CLAMP on invalid enumerations,
                    // we can hack this by sending an invalid enumeration.
                    vk::SamplerAddressMode::from_raw(0xcafe)
                } else {
                    // TODO: Emulate GL_CLAMP properly on other vendors
                    match tex_filter {
                        TextureFilter::Nearest => vk::SamplerAddressMode::CLAMP_TO_EDGE,
                        TextureFilter::Linear => vk::SamplerAddressMode::CLAMP_TO_BORDER,
                    }
                }
            }
            WrapMode::MirrorOnceClampToEdge => vk::SamplerAddressMode::MIRROR_CLAMP_TO_EDGE,
            WrapMode::MirrorOnceBorder => {
                log::warn!("Unimplemented wrap mode MirrorOnceBorder, using MirrorClampToEdge");
                vk::SamplerAddressMode::MIRROR_CLAMP_TO_EDGE
            }
            WrapMode::MirrorOnceClampOgl => {
                log::warn!("Unimplemented wrap mode MirrorOnceClampOgl, using MirrorClampToEdge");
                vk::SamplerAddressMode::MIRROR_CLAMP_TO_EDGE
            }
        }
    }

    /// Port of `Sampler::DepthCompareFunction`.
    pub fn depth_compare_function(func: DepthCompareFunc) -> vk::CompareOp {
        match func {
            DepthCompareFunc::Never => vk::CompareOp::NEVER,
            DepthCompareFunc::Less => vk::CompareOp::LESS,
            DepthCompareFunc::LessEqual => vk::CompareOp::LESS_OR_EQUAL,
            DepthCompareFunc::Equal => vk::CompareOp::EQUAL,
            DepthCompareFunc::NotEqual => vk::CompareOp::NOT_EQUAL,
            DepthCompareFunc::Greater => vk::CompareOp::GREATER,
            DepthCompareFunc::GreaterEqual => vk::CompareOp::GREATER_OR_EQUAL,
            DepthCompareFunc::Always => vk::CompareOp::ALWAYS,
        }
    }
}

// ---------------------------------------------------------------------------
// Top-level conversion functions
// ---------------------------------------------------------------------------

/// Port of `MaxwellToVK::SurfaceFormat`.
///
/// Returns format properties supported by the host. The `pixel_format` index is
/// used to look up the default Vulkan format and usage flags from the table.
///
/// Note: ASTC/BCn transcoding and device format querying are simplified here;
/// the full implementation requires a Device reference for `GetSupportedFormat`
/// and Settings for ASTC recompression mode.
pub fn surface_format(pixel_format: PixelFormat) -> FormatInfo {
    let idx = pixel_format as usize;
    if idx >= TEX_FORMAT_TUPLES.len() {
        return FormatInfo {
            format: vk::Format::UNDEFINED,
            attachable: false,
            storage: false,
        };
    }
    let tuple = &TEX_FORMAT_TUPLES[idx];
    FormatInfo {
        format: tuple.format,
        attachable: (tuple.usage & ATTACHABLE) != 0,
        storage: (tuple.usage & STORAGE) != 0,
    }
}

/// Port of `MaxwellToVK::ShaderStage`.
pub fn shader_stage(stage: Stage) -> vk::ShaderStageFlags {
    match stage {
        Stage::VertexA | Stage::VertexB => vk::ShaderStageFlags::VERTEX,
        Stage::TessellationControl => vk::ShaderStageFlags::TESSELLATION_CONTROL,
        Stage::TessellationEval => vk::ShaderStageFlags::TESSELLATION_EVALUATION,
        Stage::Geometry => vk::ShaderStageFlags::GEOMETRY,
        Stage::Fragment => vk::ShaderStageFlags::FRAGMENT,
        Stage::Compute => vk::ShaderStageFlags::COMPUTE,
    }
}

/// Port of `MaxwellToVK::PrimitiveTopology`.
pub fn primitive_topology(topology: PrimitiveTopology) -> vk::PrimitiveTopology {
    match topology {
        PrimitiveTopology::Points => vk::PrimitiveTopology::POINT_LIST,
        PrimitiveTopology::Lines => vk::PrimitiveTopology::LINE_LIST,
        PrimitiveTopology::LineLoop => {
            // LineLoop has no direct Vulkan equivalent; upstream maps to triangle list
            vk::PrimitiveTopology::TRIANGLE_LIST
        }
        PrimitiveTopology::LineStrip => vk::PrimitiveTopology::LINE_STRIP,
        PrimitiveTopology::Triangles => vk::PrimitiveTopology::TRIANGLE_LIST,
        PrimitiveTopology::TriangleStrip => vk::PrimitiveTopology::TRIANGLE_STRIP,
        PrimitiveTopology::TriangleFan => vk::PrimitiveTopology::TRIANGLE_FAN,
        PrimitiveTopology::LinesAdjacency => {
            vk::PrimitiveTopology::LINE_LIST_WITH_ADJACENCY
        }
        PrimitiveTopology::LineStripAdjacency => {
            vk::PrimitiveTopology::LINE_STRIP_WITH_ADJACENCY
        }
        PrimitiveTopology::TrianglesAdjacency => {
            vk::PrimitiveTopology::TRIANGLE_LIST_WITH_ADJACENCY
        }
        PrimitiveTopology::TriangleStripAdjacency => {
            vk::PrimitiveTopology::TRIANGLE_STRIP_WITH_ADJACENCY
        }
        PrimitiveTopology::Quads | PrimitiveTopology::QuadStrip => {
            // TODO: Use VK_PRIMITIVE_TOPOLOGY_QUAD_LIST_EXT whenever it releases
            vk::PrimitiveTopology::TRIANGLE_LIST
        }
        PrimitiveTopology::Patches => vk::PrimitiveTopology::PATCH_LIST,
        PrimitiveTopology::Polygon => {
            log::warn!(
                "Draw mode is Polygon with a polygon mode of lines should be a \
                 single body and not a bunch of triangles."
            );
            vk::PrimitiveTopology::TRIANGLE_FAN
        }
    }
}

/// Port of `MaxwellToVK::VertexFormat`.
///
/// Converts Maxwell vertex attribute type + size to the corresponding Vulkan format.
/// Note: The `must_emulate_scaled_formats` parameter mirrors the device query
/// from upstream `device.MustEmulateScaledFormats()`.
pub fn vertex_format(
    must_emulate_scaled_formats: bool,
    mut attrib_type: VertexAttribType,
    size: VertexAttribSize,
) -> vk::Format {
    if must_emulate_scaled_formats {
        if attrib_type == VertexAttribType::SScaled {
            attrib_type = VertexAttribType::SInt;
        } else if attrib_type == VertexAttribType::UScaled {
            attrib_type = VertexAttribType::UInt;
        }
    }

    let format = match attrib_type {
        VertexAttribType::UNorm => match size {
            VertexAttribSize::R8 | VertexAttribSize::A8 => vk::Format::R8_UNORM,
            VertexAttribSize::R8G8 | VertexAttribSize::G8R8 => vk::Format::R8G8_UNORM,
            VertexAttribSize::R8G8B8 => vk::Format::R8G8B8_UNORM,
            VertexAttribSize::R8G8B8A8 | VertexAttribSize::X8B8G8R8 => {
                vk::Format::R8G8B8A8_UNORM
            }
            VertexAttribSize::R16 => vk::Format::R16_UNORM,
            VertexAttribSize::R16G16 => vk::Format::R16G16_UNORM,
            VertexAttribSize::R16G16B16 => vk::Format::R16G16B16_UNORM,
            VertexAttribSize::R16G16B16A16 => vk::Format::R16G16B16A16_UNORM,
            VertexAttribSize::A2B10G10R10 => vk::Format::A2B10G10R10_UNORM_PACK32,
            _ => vk::Format::UNDEFINED,
        },
        VertexAttribType::SNorm => match size {
            VertexAttribSize::R8 | VertexAttribSize::A8 => vk::Format::R8_SNORM,
            VertexAttribSize::R8G8 | VertexAttribSize::G8R8 => vk::Format::R8G8_SNORM,
            VertexAttribSize::R8G8B8 => vk::Format::R8G8B8_SNORM,
            VertexAttribSize::R8G8B8A8 | VertexAttribSize::X8B8G8R8 => {
                vk::Format::R8G8B8A8_SNORM
            }
            VertexAttribSize::R16 => vk::Format::R16_SNORM,
            VertexAttribSize::R16G16 => vk::Format::R16G16_SNORM,
            VertexAttribSize::R16G16B16 => vk::Format::R16G16B16_SNORM,
            VertexAttribSize::R16G16B16A16 => vk::Format::R16G16B16A16_SNORM,
            VertexAttribSize::A2B10G10R10 => vk::Format::A2B10G10R10_SNORM_PACK32,
            _ => vk::Format::UNDEFINED,
        },
        VertexAttribType::UScaled => match size {
            VertexAttribSize::R8 | VertexAttribSize::A8 => vk::Format::R8_USCALED,
            VertexAttribSize::R8G8 | VertexAttribSize::G8R8 => vk::Format::R8G8_USCALED,
            VertexAttribSize::R8G8B8 => vk::Format::R8G8B8_USCALED,
            VertexAttribSize::R8G8B8A8 | VertexAttribSize::X8B8G8R8 => {
                vk::Format::R8G8B8A8_USCALED
            }
            VertexAttribSize::R16 => vk::Format::R16_USCALED,
            VertexAttribSize::R16G16 => vk::Format::R16G16_USCALED,
            VertexAttribSize::R16G16B16 => vk::Format::R16G16B16_USCALED,
            VertexAttribSize::R16G16B16A16 => vk::Format::R16G16B16A16_USCALED,
            VertexAttribSize::A2B10G10R10 => vk::Format::A2B10G10R10_USCALED_PACK32,
            _ => vk::Format::UNDEFINED,
        },
        VertexAttribType::SScaled => match size {
            VertexAttribSize::R8 | VertexAttribSize::A8 => vk::Format::R8_SSCALED,
            VertexAttribSize::R8G8 | VertexAttribSize::G8R8 => vk::Format::R8G8_SSCALED,
            VertexAttribSize::R8G8B8 => vk::Format::R8G8B8_SSCALED,
            VertexAttribSize::R8G8B8A8 | VertexAttribSize::X8B8G8R8 => {
                vk::Format::R8G8B8A8_SSCALED
            }
            VertexAttribSize::R16 => vk::Format::R16_SSCALED,
            VertexAttribSize::R16G16 => vk::Format::R16G16_SSCALED,
            VertexAttribSize::R16G16B16 => vk::Format::R16G16B16_SSCALED,
            VertexAttribSize::R16G16B16A16 => vk::Format::R16G16B16A16_SSCALED,
            VertexAttribSize::A2B10G10R10 => vk::Format::A2B10G10R10_SSCALED_PACK32,
            _ => vk::Format::UNDEFINED,
        },
        VertexAttribType::UInt => match size {
            VertexAttribSize::R8 | VertexAttribSize::A8 => vk::Format::R8_UINT,
            VertexAttribSize::R8G8 | VertexAttribSize::G8R8 => vk::Format::R8G8_UINT,
            VertexAttribSize::R8G8B8 => vk::Format::R8G8B8_UINT,
            VertexAttribSize::R8G8B8A8 | VertexAttribSize::X8B8G8R8 => {
                vk::Format::R8G8B8A8_UINT
            }
            VertexAttribSize::R16 => vk::Format::R16_UINT,
            VertexAttribSize::R16G16 => vk::Format::R16G16_UINT,
            VertexAttribSize::R16G16B16 => vk::Format::R16G16B16_UINT,
            VertexAttribSize::R16G16B16A16 => vk::Format::R16G16B16A16_UINT,
            VertexAttribSize::R32 => vk::Format::R32_UINT,
            VertexAttribSize::R32G32 => vk::Format::R32G32_UINT,
            VertexAttribSize::R32G32B32 => vk::Format::R32G32B32_UINT,
            VertexAttribSize::R32G32B32A32 => vk::Format::R32G32B32A32_UINT,
            VertexAttribSize::A2B10G10R10 => vk::Format::A2B10G10R10_UINT_PACK32,
            _ => vk::Format::UNDEFINED,
        },
        VertexAttribType::SInt => match size {
            VertexAttribSize::R8 | VertexAttribSize::A8 => vk::Format::R8_SINT,
            VertexAttribSize::R8G8 | VertexAttribSize::G8R8 => vk::Format::R8G8_SINT,
            VertexAttribSize::R8G8B8 => vk::Format::R8G8B8_SINT,
            VertexAttribSize::R8G8B8A8 | VertexAttribSize::X8B8G8R8 => {
                vk::Format::R8G8B8A8_SINT
            }
            VertexAttribSize::R16 => vk::Format::R16_SINT,
            VertexAttribSize::R16G16 => vk::Format::R16G16_SINT,
            VertexAttribSize::R16G16B16 => vk::Format::R16G16B16_SINT,
            VertexAttribSize::R16G16B16A16 => vk::Format::R16G16B16A16_SINT,
            VertexAttribSize::R32 => vk::Format::R32_SINT,
            VertexAttribSize::R32G32 => vk::Format::R32G32_SINT,
            VertexAttribSize::R32G32B32 => vk::Format::R32G32B32_SINT,
            VertexAttribSize::R32G32B32A32 => vk::Format::R32G32B32A32_SINT,
            VertexAttribSize::A2B10G10R10 => vk::Format::A2B10G10R10_SINT_PACK32,
            _ => vk::Format::UNDEFINED,
        },
        VertexAttribType::Float => match size {
            VertexAttribSize::R16 => vk::Format::R16_SFLOAT,
            VertexAttribSize::R16G16 => vk::Format::R16G16_SFLOAT,
            VertexAttribSize::R16G16B16 => vk::Format::R16G16B16_SFLOAT,
            VertexAttribSize::R16G16B16A16 => vk::Format::R16G16B16A16_SFLOAT,
            VertexAttribSize::R32 => vk::Format::R32_SFLOAT,
            VertexAttribSize::R32G32 => vk::Format::R32G32_SFLOAT,
            VertexAttribSize::R32G32B32 => vk::Format::R32G32B32_SFLOAT,
            VertexAttribSize::R32G32B32A32 => vk::Format::R32G32B32A32_SFLOAT,
            VertexAttribSize::B10G11R11 => vk::Format::B10G11R11_UFLOAT_PACK32,
            _ => vk::Format::UNDEFINED,
        },
        VertexAttribType::Invalid => vk::Format::UNDEFINED,
    };

    if format == vk::Format::UNDEFINED {
        log::warn!(
            "Unimplemented vertex format of type={:?} and size={:?}",
            attrib_type,
            size
        );
    }
    format
}

/// Port of `MaxwellToVK::ComparisonOp`.
pub fn comparison_op(comparison: ComparisonOp) -> vk::CompareOp {
    match comparison {
        ComparisonOp::Never => vk::CompareOp::NEVER,
        ComparisonOp::Less => vk::CompareOp::LESS,
        ComparisonOp::Equal => vk::CompareOp::EQUAL,
        ComparisonOp::LessEqual => vk::CompareOp::LESS_OR_EQUAL,
        ComparisonOp::Greater => vk::CompareOp::GREATER,
        ComparisonOp::NotEqual => vk::CompareOp::NOT_EQUAL,
        ComparisonOp::GreaterEqual => vk::CompareOp::GREATER_OR_EQUAL,
        ComparisonOp::Always => vk::CompareOp::ALWAYS,
    }
}

/// Port of `MaxwellToVK::IndexFormat`.
pub fn index_format(idx_format: IndexFormat) -> vk::IndexType {
    match idx_format {
        IndexFormat::UnsignedByte => vk::IndexType::UINT8_EXT,
        IndexFormat::UnsignedShort => vk::IndexType::UINT16,
        IndexFormat::UnsignedInt => vk::IndexType::UINT32,
    }
}

/// Port of `MaxwellToVK::StencilOp`.
pub fn stencil_op(op: StencilOp) -> vk::StencilOp {
    match op {
        StencilOp::Keep => vk::StencilOp::KEEP,
        StencilOp::Zero => vk::StencilOp::ZERO,
        StencilOp::Replace => vk::StencilOp::REPLACE,
        StencilOp::IncrSat => vk::StencilOp::INCREMENT_AND_CLAMP,
        StencilOp::DecrSat => vk::StencilOp::DECREMENT_AND_CLAMP,
        StencilOp::Invert => vk::StencilOp::INVERT,
        StencilOp::Incr => vk::StencilOp::INCREMENT_AND_WRAP,
        StencilOp::Decr => vk::StencilOp::DECREMENT_AND_WRAP,
    }
}

/// Port of `MaxwellToVK::BlendEquation`.
pub fn blend_equation(equation: BlendEquation) -> vk::BlendOp {
    match equation {
        BlendEquation::Add => vk::BlendOp::ADD,
        BlendEquation::Subtract => vk::BlendOp::SUBTRACT,
        BlendEquation::ReverseSubtract => vk::BlendOp::REVERSE_SUBTRACT,
        BlendEquation::Min => vk::BlendOp::MIN,
        BlendEquation::Max => vk::BlendOp::MAX,
    }
}

/// Port of `MaxwellToVK::BlendFactor`.
pub fn blend_factor(factor: BlendFactor) -> vk::BlendFactor {
    match factor {
        BlendFactor::Zero => vk::BlendFactor::ZERO,
        BlendFactor::One => vk::BlendFactor::ONE,
        BlendFactor::SrcColor => vk::BlendFactor::SRC_COLOR,
        BlendFactor::OneMinusSrcColor => vk::BlendFactor::ONE_MINUS_SRC_COLOR,
        BlendFactor::SrcAlpha => vk::BlendFactor::SRC_ALPHA,
        BlendFactor::OneMinusSrcAlpha => vk::BlendFactor::ONE_MINUS_SRC_ALPHA,
        BlendFactor::DstAlpha => vk::BlendFactor::DST_ALPHA,
        BlendFactor::OneMinusDstAlpha => vk::BlendFactor::ONE_MINUS_DST_ALPHA,
        BlendFactor::DstColor => vk::BlendFactor::DST_COLOR,
        BlendFactor::OneMinusDstColor => vk::BlendFactor::ONE_MINUS_DST_COLOR,
        BlendFactor::SrcAlphaSaturate => vk::BlendFactor::SRC_ALPHA_SATURATE,
        BlendFactor::Src1Color => vk::BlendFactor::SRC1_COLOR,
        BlendFactor::OneMinusSrc1Color => vk::BlendFactor::ONE_MINUS_SRC1_COLOR,
        BlendFactor::Src1Alpha => vk::BlendFactor::SRC1_ALPHA,
        BlendFactor::OneMinusSrc1Alpha => vk::BlendFactor::ONE_MINUS_SRC1_ALPHA,
        BlendFactor::ConstantColor => vk::BlendFactor::CONSTANT_COLOR,
        BlendFactor::OneMinusConstantColor => vk::BlendFactor::ONE_MINUS_CONSTANT_COLOR,
        BlendFactor::ConstantAlpha => vk::BlendFactor::CONSTANT_ALPHA,
        BlendFactor::OneMinusConstantAlpha => vk::BlendFactor::ONE_MINUS_CONSTANT_ALPHA,
    }
}

/// Port of `MaxwellToVK::FrontFace`.
pub fn front_face(face: FrontFace) -> vk::FrontFace {
    match face {
        FrontFace::CW => vk::FrontFace::CLOCKWISE,
        FrontFace::CCW => vk::FrontFace::COUNTER_CLOCKWISE,
    }
}

/// Port of `MaxwellToVK::CullFace`.
pub fn cull_face(face: CullFace) -> vk::CullModeFlags {
    match face {
        CullFace::Front => vk::CullModeFlags::FRONT,
        CullFace::Back => vk::CullModeFlags::BACK,
        CullFace::FrontAndBack => vk::CullModeFlags::FRONT_AND_BACK,
    }
}

/// Port of `MaxwellToVK::PolygonMode`.
pub fn polygon_mode(mode: PolygonMode) -> vk::PolygonMode {
    match mode {
        PolygonMode::Point => vk::PolygonMode::POINT,
        PolygonMode::Line => vk::PolygonMode::LINE,
        PolygonMode::Fill => vk::PolygonMode::FILL,
    }
}

/// Port of `MaxwellToVK::SwizzleSource`.
pub fn swizzle_source(swizzle: SwizzleSource) -> vk::ComponentSwizzle {
    match swizzle {
        SwizzleSource::Zero => vk::ComponentSwizzle::ZERO,
        SwizzleSource::R => vk::ComponentSwizzle::R,
        SwizzleSource::G => vk::ComponentSwizzle::G,
        SwizzleSource::B => vk::ComponentSwizzle::B,
        SwizzleSource::A => vk::ComponentSwizzle::A,
        SwizzleSource::OneInt | SwizzleSource::OneFloat => vk::ComponentSwizzle::ONE,
    }
}

/// Viewport swizzle (NV extension).
///
/// Port of `MaxwellToVK::ViewportSwizzle`.
/// Uses raw u32 values since ViewportSwizzle is not defined as a dedicated enum yet.
pub fn viewport_swizzle(swizzle: u32) -> vk::ViewportCoordinateSwizzleNV {
    match swizzle {
        0 => vk::ViewportCoordinateSwizzleNV::POSITIVE_X,
        1 => vk::ViewportCoordinateSwizzleNV::NEGATIVE_X,
        2 => vk::ViewportCoordinateSwizzleNV::POSITIVE_Y,
        3 => vk::ViewportCoordinateSwizzleNV::NEGATIVE_Y,
        4 => vk::ViewportCoordinateSwizzleNV::POSITIVE_Z,
        5 => vk::ViewportCoordinateSwizzleNV::NEGATIVE_Z,
        6 => vk::ViewportCoordinateSwizzleNV::POSITIVE_W,
        7 => vk::ViewportCoordinateSwizzleNV::NEGATIVE_W,
        _ => {
            log::error!("Invalid viewport swizzle={}", swizzle);
            vk::ViewportCoordinateSwizzleNV::POSITIVE_X
        }
    }
}

/// Port of `MaxwellToVK::SamplerReduction`.
pub fn sampler_reduction(reduction: SamplerReduction) -> vk::SamplerReductionMode {
    match reduction {
        SamplerReduction::WeightedAverage => vk::SamplerReductionMode::WEIGHTED_AVERAGE,
        SamplerReduction::Min => vk::SamplerReductionMode::MIN,
        SamplerReduction::Max => vk::SamplerReductionMode::MAX,
    }
}

/// Port of `MaxwellToVK::MsaaMode`.
pub fn msaa_mode(mode: MsaaMode) -> vk::SampleCountFlags {
    match mode {
        MsaaMode::Msaa1x1 => vk::SampleCountFlags::TYPE_1,
        MsaaMode::Msaa2x1 | MsaaMode::Msaa2x1D3d => vk::SampleCountFlags::TYPE_2,
        MsaaMode::Msaa2x2 | MsaaMode::Msaa2x2Vc4 | MsaaMode::Msaa2x2Vc12 => {
            vk::SampleCountFlags::TYPE_4
        }
        MsaaMode::Msaa4x2
        | MsaaMode::Msaa4x2D3d
        | MsaaMode::Msaa4x2Vc8
        | MsaaMode::Msaa4x2Vc24 => vk::SampleCountFlags::TYPE_8,
        MsaaMode::Msaa4x4 => vk::SampleCountFlags::TYPE_16,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shader_stage_vertex() {
        assert_eq!(
            shader_stage(Stage::VertexB),
            vk::ShaderStageFlags::VERTEX
        );
        assert_eq!(
            shader_stage(Stage::VertexA),
            vk::ShaderStageFlags::VERTEX
        );
    }

    #[test]
    fn test_shader_stage_fragment() {
        assert_eq!(
            shader_stage(Stage::Fragment),
            vk::ShaderStageFlags::FRAGMENT
        );
    }

    #[test]
    fn test_primitive_topology_triangles() {
        assert_eq!(
            primitive_topology(PrimitiveTopology::Triangles),
            vk::PrimitiveTopology::TRIANGLE_LIST
        );
    }

    #[test]
    fn test_comparison_op_never() {
        assert_eq!(comparison_op(ComparisonOp::Never), vk::CompareOp::NEVER);
    }

    #[test]
    fn test_comparison_op_always() {
        assert_eq!(comparison_op(ComparisonOp::Always), vk::CompareOp::ALWAYS);
    }

    #[test]
    fn test_index_format() {
        assert_eq!(index_format(IndexFormat::UnsignedByte), vk::IndexType::UINT8_EXT);
        assert_eq!(index_format(IndexFormat::UnsignedShort), vk::IndexType::UINT16);
        assert_eq!(index_format(IndexFormat::UnsignedInt), vk::IndexType::UINT32);
    }

    #[test]
    fn test_stencil_op_keep() {
        assert_eq!(stencil_op(StencilOp::Keep), vk::StencilOp::KEEP);
    }

    #[test]
    fn test_blend_equation_add() {
        assert_eq!(blend_equation(BlendEquation::Add), vk::BlendOp::ADD);
    }

    #[test]
    fn test_blend_factor_zero() {
        assert_eq!(blend_factor(BlendFactor::Zero), vk::BlendFactor::ZERO);
    }

    #[test]
    fn test_blend_factor_one() {
        assert_eq!(blend_factor(BlendFactor::One), vk::BlendFactor::ONE);
    }

    #[test]
    fn test_front_face() {
        assert_eq!(front_face(FrontFace::CW), vk::FrontFace::CLOCKWISE);
        assert_eq!(front_face(FrontFace::CCW), vk::FrontFace::COUNTER_CLOCKWISE);
    }

    #[test]
    fn test_cull_face() {
        assert_eq!(cull_face(CullFace::Front), vk::CullModeFlags::FRONT);
        assert_eq!(cull_face(CullFace::Back), vk::CullModeFlags::BACK);
    }

    #[test]
    fn test_polygon_mode() {
        assert_eq!(polygon_mode(PolygonMode::Fill), vk::PolygonMode::FILL);
        assert_eq!(polygon_mode(PolygonMode::Line), vk::PolygonMode::LINE);
        assert_eq!(polygon_mode(PolygonMode::Point), vk::PolygonMode::POINT);
    }

    #[test]
    fn test_swizzle_source() {
        assert_eq!(swizzle_source(SwizzleSource::Zero), vk::ComponentSwizzle::ZERO);
        assert_eq!(swizzle_source(SwizzleSource::R), vk::ComponentSwizzle::R);
        assert_eq!(swizzle_source(SwizzleSource::OneInt), vk::ComponentSwizzle::ONE);
    }

    #[test]
    fn test_sampler_filter() {
        assert_eq!(sampler::filter(TextureFilter::Nearest), vk::Filter::NEAREST);
        assert_eq!(sampler::filter(TextureFilter::Linear), vk::Filter::LINEAR);
    }

    #[test]
    fn test_sampler_mipmap_mode() {
        assert_eq!(
            sampler::mipmap_mode(TextureMipmapFilter::None),
            vk::SamplerMipmapMode::NEAREST
        );
        assert_eq!(
            sampler::mipmap_mode(TextureMipmapFilter::Linear),
            vk::SamplerMipmapMode::LINEAR
        );
    }

    #[test]
    fn test_depth_compare_function() {
        assert_eq!(
            sampler::depth_compare_function(DepthCompareFunc::Never),
            vk::CompareOp::NEVER
        );
        assert_eq!(
            sampler::depth_compare_function(DepthCompareFunc::Always),
            vk::CompareOp::ALWAYS
        );
    }

    #[test]
    fn test_msaa_mode() {
        assert_eq!(msaa_mode(MsaaMode::Msaa1x1), vk::SampleCountFlags::TYPE_1);
        assert_eq!(msaa_mode(MsaaMode::Msaa2x2), vk::SampleCountFlags::TYPE_4);
        assert_eq!(msaa_mode(MsaaMode::Msaa4x4), vk::SampleCountFlags::TYPE_16);
    }

    #[test]
    fn test_surface_format_a8b8g8r8_unorm() {
        let info = surface_format(PixelFormat::A8B8G8R8Unorm);
        assert_eq!(info.format, vk::Format::A8B8G8R8_UNORM_PACK32);
        assert!(info.attachable);
        assert!(info.storage);
    }

    #[test]
    fn test_surface_format_bc1() {
        let info = surface_format(PixelFormat::Bc1RgbaUnorm);
        assert_eq!(info.format, vk::Format::BC1_RGBA_UNORM_BLOCK);
        assert!(!info.attachable);
        assert!(!info.storage);
    }

    #[test]
    fn test_vertex_format_float_r32g32b32a32() {
        let fmt = vertex_format(false, VertexAttribType::Float, VertexAttribSize::R32G32B32A32);
        assert_eq!(fmt, vk::Format::R32G32B32A32_SFLOAT);
    }

    #[test]
    fn test_vertex_format_uint_r8() {
        let fmt = vertex_format(false, VertexAttribType::UInt, VertexAttribSize::R8);
        assert_eq!(fmt, vk::Format::R8_UINT);
    }

    #[test]
    fn test_vertex_format_emulate_scaled() {
        // When must_emulate_scaled_formats is true, SScaled becomes SInt
        let fmt = vertex_format(true, VertexAttribType::SScaled, VertexAttribSize::R16);
        assert_eq!(fmt, vk::Format::R16_SINT);
    }

    #[test]
    fn test_sampler_reduction() {
        assert_eq!(
            sampler_reduction(SamplerReduction::WeightedAverage),
            vk::SamplerReductionMode::WEIGHTED_AVERAGE
        );
        assert_eq!(
            sampler_reduction(SamplerReduction::Min),
            vk::SamplerReductionMode::MIN
        );
        assert_eq!(
            sampler_reduction(SamplerReduction::Max),
            vk::SamplerReductionMode::MAX
        );
    }
}
