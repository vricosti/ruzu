// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/surface.h` and `video_core/surface.cpp`.
//!
//! Pixel format utilities, surface target helpers, block dimension tables,
//! and format classification functions.
//!
//! The canonical `PixelFormat` enum lives in
//! `crate::texture_cache::format_lookup_table` (matching upstream's placement
//! in `surface.h`). This module re-exports it and provides the associated
//! lookup tables and utility functions.

pub use crate::texture_cache::format_lookup_table::PixelFormat;

// ---------------------------------------------------------------------------
// SurfaceType
// ---------------------------------------------------------------------------

/// Surface type classification.
///
/// Port of `VideoCore::Surface::SurfaceType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SurfaceType {
    ColorTexture = 0,
    Depth = 1,
    Stencil = 2,
    DepthStencil = 3,
    Invalid = 4,
}

// ---------------------------------------------------------------------------
// SurfaceTarget
// ---------------------------------------------------------------------------

/// Surface target (texture dimensionality).
///
/// Port of `VideoCore::Surface::SurfaceTarget`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SurfaceTarget {
    Texture1D,
    TextureBuffer,
    Texture2D,
    Texture3D,
    Texture1DArray,
    Texture2DArray,
    TextureCubemap,
    TextureCubeArray,
}

// ---------------------------------------------------------------------------
// PixelFormat boundary constants
// ---------------------------------------------------------------------------

// These constants mark the boundaries between format categories in the
// PixelFormat enum. They match the upstream sentinel values.

/// Number of color formats (up to and not including depth formats).
///
/// Port of `PixelFormat::MaxColorFormat`.
pub const MAX_COLOR_FORMAT: u32 = PixelFormat::D32Float as u32;

/// Number of color + depth formats.
///
/// Port of `PixelFormat::MaxDepthFormat`.
pub const MAX_DEPTH_FORMAT: u32 = PixelFormat::S8Uint as u32;

/// Number of color + depth + stencil formats.
///
/// Port of `PixelFormat::MaxStencilFormat`.
pub const MAX_STENCIL_FORMAT: u32 = PixelFormat::D24UnormS8Uint as u32;

/// Number of all formats (color + depth + stencil + depth-stencil).
///
/// Port of `PixelFormat::MaxDepthStencilFormat`.
pub const MAX_DEPTH_STENCIL_FORMAT: u32 = PixelFormat::MaxDepthStencilFormat as u32;

/// Total number of pixel formats (for table sizing).
pub const MAX_PIXEL_FORMAT: usize = MAX_DEPTH_STENCIL_FORMAT as usize;

// ---------------------------------------------------------------------------
// Block width table
// ---------------------------------------------------------------------------

/// Default block width for each pixel format.
///
/// Port of `BLOCK_WIDTH_TABLE` from `surface.h`.
pub const BLOCK_WIDTH_TABLE: [u8; MAX_PIXEL_FORMAT] = [
    1,  // A8B8G8R8_UNORM
    1,  // A8B8G8R8_SNORM
    1,  // A8B8G8R8_SINT
    1,  // A8B8G8R8_UINT
    1,  // R5G6B5_UNORM
    1,  // B5G6R5_UNORM
    1,  // A1R5G5B5_UNORM
    1,  // A2B10G10R10_UNORM
    1,  // A2B10G10R10_UINT
    1,  // A2R10G10B10_UNORM
    1,  // A1B5G5R5_UNORM
    1,  // A5B5G5R1_UNORM
    1,  // R8_UNORM
    1,  // R8_SNORM
    1,  // R8_SINT
    1,  // R8_UINT
    1,  // R16G16B16A16_FLOAT
    1,  // R16G16B16A16_UNORM
    1,  // R16G16B16A16_SNORM
    1,  // R16G16B16A16_SINT
    1,  // R16G16B16A16_UINT
    1,  // B10G11R11_FLOAT
    1,  // R32G32B32A32_UINT
    4,  // BC1_RGBA_UNORM
    4,  // BC2_UNORM
    4,  // BC3_UNORM
    4,  // BC4_UNORM
    4,  // BC4_SNORM
    4,  // BC5_UNORM
    4,  // BC5_SNORM
    4,  // BC7_UNORM
    4,  // BC6H_UFLOAT
    4,  // BC6H_SFLOAT
    4,  // ASTC_2D_4X4_UNORM
    1,  // B8G8R8A8_UNORM
    1,  // R32G32B32A32_FLOAT
    1,  // R32G32B32A32_SINT
    1,  // R32G32_FLOAT
    1,  // R32G32_SINT
    1,  // R32_FLOAT
    1,  // R16_FLOAT
    1,  // R16_UNORM
    1,  // R16_SNORM
    1,  // R16_UINT
    1,  // R16_SINT
    1,  // R16G16_UNORM
    1,  // R16G16_FLOAT
    1,  // R16G16_UINT
    1,  // R16G16_SINT
    1,  // R16G16_SNORM
    1,  // R32G32B32_FLOAT
    1,  // A8B8G8R8_SRGB
    1,  // R8G8_UNORM
    1,  // R8G8_SNORM
    1,  // R8G8_SINT
    1,  // R8G8_UINT
    1,  // R32G32_UINT
    1,  // R16G16B16X16_FLOAT
    1,  // R32_UINT
    1,  // R32_SINT
    8,  // ASTC_2D_8X8_UNORM
    8,  // ASTC_2D_8X5_UNORM
    5,  // ASTC_2D_5X4_UNORM
    1,  // B8G8R8A8_SRGB
    4,  // BC1_RGBA_SRGB
    4,  // BC2_SRGB
    4,  // BC3_SRGB
    4,  // BC7_SRGB
    1,  // A4B4G4R4_UNORM
    1,  // G4R4_UNORM
    4,  // ASTC_2D_4X4_SRGB
    8,  // ASTC_2D_8X8_SRGB
    8,  // ASTC_2D_8X5_SRGB
    5,  // ASTC_2D_5X4_SRGB
    5,  // ASTC_2D_5X5_UNORM
    5,  // ASTC_2D_5X5_SRGB
    10, // ASTC_2D_10X8_UNORM
    10, // ASTC_2D_10X8_SRGB
    6,  // ASTC_2D_6X6_UNORM
    6,  // ASTC_2D_6X6_SRGB
    10, // ASTC_2D_10X6_UNORM
    10, // ASTC_2D_10X6_SRGB
    10, // ASTC_2D_10X5_UNORM
    10, // ASTC_2D_10X5_SRGB
    10, // ASTC_2D_10X10_UNORM
    10, // ASTC_2D_10X10_SRGB
    12, // ASTC_2D_12X10_UNORM
    12, // ASTC_2D_12X10_SRGB
    12, // ASTC_2D_12X12_UNORM
    12, // ASTC_2D_12X12_SRGB
    8,  // ASTC_2D_8X6_UNORM
    8,  // ASTC_2D_8X6_SRGB
    6,  // ASTC_2D_6X5_UNORM
    6,  // ASTC_2D_6X5_SRGB
    1,  // E5B9G9R9_FLOAT
    1,  // D32_FLOAT
    1,  // D16_UNORM
    1,  // X8_D24_UNORM
    1,  // S8_UINT
    1,  // D24_UNORM_S8_UINT
    1,  // S8_UINT_D24_UNORM
    1,  // D32_FLOAT_S8_UINT
];

/// Returns the default block width for a pixel format.
///
/// Port of `DefaultBlockWidth` from `surface.h`.
pub fn default_block_width(format: PixelFormat) -> u32 {
    let idx = format as usize;
    assert!(idx < BLOCK_WIDTH_TABLE.len());
    BLOCK_WIDTH_TABLE[idx] as u32
}

// ---------------------------------------------------------------------------
// Block height table
// ---------------------------------------------------------------------------

/// Default block height for each pixel format.
///
/// Port of `BLOCK_HEIGHT_TABLE` from `surface.h`.
pub const BLOCK_HEIGHT_TABLE: [u8; MAX_PIXEL_FORMAT] = [
    1,  // A8B8G8R8_UNORM
    1,  // A8B8G8R8_SNORM
    1,  // A8B8G8R8_SINT
    1,  // A8B8G8R8_UINT
    1,  // R5G6B5_UNORM
    1,  // B5G6R5_UNORM
    1,  // A1R5G5B5_UNORM
    1,  // A2B10G10R10_UNORM
    1,  // A2B10G10R10_UINT
    1,  // A2R10G10B10_UNORM
    1,  // A1B5G5R5_UNORM
    1,  // A5B5G5R1_UNORM
    1,  // R8_UNORM
    1,  // R8_SNORM
    1,  // R8_SINT
    1,  // R8_UINT
    1,  // R16G16B16A16_FLOAT
    1,  // R16G16B16A16_UNORM
    1,  // R16G16B16A16_SNORM
    1,  // R16G16B16A16_SINT
    1,  // R16G16B16A16_UINT
    1,  // B10G11R11_FLOAT
    1,  // R32G32B32A32_UINT
    4,  // BC1_RGBA_UNORM
    4,  // BC2_UNORM
    4,  // BC3_UNORM
    4,  // BC4_UNORM
    4,  // BC4_SNORM
    4,  // BC5_UNORM
    4,  // BC5_SNORM
    4,  // BC7_UNORM
    4,  // BC6H_UFLOAT
    4,  // BC6H_SFLOAT
    4,  // ASTC_2D_4X4_UNORM
    1,  // B8G8R8A8_UNORM
    1,  // R32G32B32A32_FLOAT
    1,  // R32G32B32A32_SINT
    1,  // R32G32_FLOAT
    1,  // R32G32_SINT
    1,  // R32_FLOAT
    1,  // R16_FLOAT
    1,  // R16_UNORM
    1,  // R16_SNORM
    1,  // R16_UINT
    1,  // R16_SINT
    1,  // R16G16_UNORM
    1,  // R16G16_FLOAT
    1,  // R16G16_UINT
    1,  // R16G16_SINT
    1,  // R16G16_SNORM
    1,  // R32G32B32_FLOAT
    1,  // A8B8G8R8_SRGB
    1,  // R8G8_UNORM
    1,  // R8G8_SNORM
    1,  // R8G8_SINT
    1,  // R8G8_UINT
    1,  // R32G32_UINT
    1,  // R16G16B16X16_FLOAT
    1,  // R32_UINT
    1,  // R32_SINT
    8,  // ASTC_2D_8X8_UNORM
    5,  // ASTC_2D_8X5_UNORM
    4,  // ASTC_2D_5X4_UNORM
    1,  // B8G8R8A8_SRGB
    4,  // BC1_RGBA_SRGB
    4,  // BC2_SRGB
    4,  // BC3_SRGB
    4,  // BC7_SRGB
    1,  // A4B4G4R4_UNORM
    1,  // G4R4_UNORM
    4,  // ASTC_2D_4X4_SRGB
    8,  // ASTC_2D_8X8_SRGB
    5,  // ASTC_2D_8X5_SRGB
    4,  // ASTC_2D_5X4_SRGB
    5,  // ASTC_2D_5X5_UNORM
    5,  // ASTC_2D_5X5_SRGB
    8,  // ASTC_2D_10X8_UNORM
    8,  // ASTC_2D_10X8_SRGB
    6,  // ASTC_2D_6X6_UNORM
    6,  // ASTC_2D_6X6_SRGB
    6,  // ASTC_2D_10X6_UNORM
    6,  // ASTC_2D_10X6_SRGB
    5,  // ASTC_2D_10X5_UNORM
    5,  // ASTC_2D_10X5_SRGB
    10, // ASTC_2D_10X10_UNORM
    10, // ASTC_2D_10X10_SRGB
    10, // ASTC_2D_12X10_UNORM
    10, // ASTC_2D_12X10_SRGB
    12, // ASTC_2D_12X12_UNORM
    12, // ASTC_2D_12X12_SRGB
    6,  // ASTC_2D_8X6_UNORM
    6,  // ASTC_2D_8X6_SRGB
    5,  // ASTC_2D_6X5_UNORM
    5,  // ASTC_2D_6X5_SRGB
    1,  // E5B9G9R9_FLOAT
    1,  // D32_FLOAT
    1,  // D16_UNORM
    1,  // X8_D24_UNORM
    1,  // S8_UINT
    1,  // D24_UNORM_S8_UINT
    1,  // S8_UINT_D24_UNORM
    1,  // D32_FLOAT_S8_UINT
];

/// Returns the default block height for a pixel format.
///
/// Port of `DefaultBlockHeight` from `surface.h`.
pub fn default_block_height(format: PixelFormat) -> u32 {
    let idx = format as usize;
    assert!(idx < BLOCK_HEIGHT_TABLE.len());
    BLOCK_HEIGHT_TABLE[idx] as u32
}

// ---------------------------------------------------------------------------
// Bits per block table
// ---------------------------------------------------------------------------

/// Bits per block for each pixel format.
///
/// Port of `BITS_PER_BLOCK_TABLE` from `surface.h`.
pub const BITS_PER_BLOCK_TABLE: [u16; MAX_PIXEL_FORMAT] = [
    32,  // A8B8G8R8_UNORM
    32,  // A8B8G8R8_SNORM
    32,  // A8B8G8R8_SINT
    32,  // A8B8G8R8_UINT
    16,  // R5G6B5_UNORM
    16,  // B5G6R5_UNORM
    16,  // A1R5G5B5_UNORM
    32,  // A2B10G10R10_UNORM
    32,  // A2B10G10R10_UINT
    32,  // A2R10G10B10_UNORM
    16,  // A1B5G5R5_UNORM
    16,  // A5B5G5R1_UNORM
    8,   // R8_UNORM
    8,   // R8_SNORM
    8,   // R8_SINT
    8,   // R8_UINT
    64,  // R16G16B16A16_FLOAT
    64,  // R16G16B16A16_UNORM
    64,  // R16G16B16A16_SNORM
    64,  // R16G16B16A16_SINT
    64,  // R16G16B16A16_UINT
    32,  // B10G11R11_FLOAT
    128, // R32G32B32A32_UINT
    64,  // BC1_RGBA_UNORM
    128, // BC2_UNORM
    128, // BC3_UNORM
    64,  // BC4_UNORM
    64,  // BC4_SNORM
    128, // BC5_UNORM
    128, // BC5_SNORM
    128, // BC7_UNORM
    128, // BC6H_UFLOAT
    128, // BC6H_SFLOAT
    128, // ASTC_2D_4X4_UNORM
    32,  // B8G8R8A8_UNORM
    128, // R32G32B32A32_FLOAT
    128, // R32G32B32A32_SINT
    64,  // R32G32_FLOAT
    64,  // R32G32_SINT
    32,  // R32_FLOAT
    16,  // R16_FLOAT
    16,  // R16_UNORM
    16,  // R16_SNORM
    16,  // R16_UINT
    16,  // R16_SINT
    32,  // R16G16_UNORM
    32,  // R16G16_FLOAT
    32,  // R16G16_UINT
    32,  // R16G16_SINT
    32,  // R16G16_SNORM
    96,  // R32G32B32_FLOAT
    32,  // A8B8G8R8_SRGB
    16,  // R8G8_UNORM
    16,  // R8G8_SNORM
    16,  // R8G8_SINT
    16,  // R8G8_UINT
    64,  // R32G32_UINT
    64,  // R16G16B16X16_FLOAT
    32,  // R32_UINT
    32,  // R32_SINT
    128, // ASTC_2D_8X8_UNORM
    128, // ASTC_2D_8X5_UNORM
    128, // ASTC_2D_5X4_UNORM
    32,  // B8G8R8A8_SRGB
    64,  // BC1_RGBA_SRGB
    128, // BC2_SRGB
    128, // BC3_SRGB
    128, // BC7_SRGB
    16,  // A4B4G4R4_UNORM
    8,   // G4R4_UNORM
    128, // ASTC_2D_4X4_SRGB
    128, // ASTC_2D_8X8_SRGB
    128, // ASTC_2D_8X5_SRGB
    128, // ASTC_2D_5X4_SRGB
    128, // ASTC_2D_5X5_UNORM
    128, // ASTC_2D_5X5_SRGB
    128, // ASTC_2D_10X8_UNORM
    128, // ASTC_2D_10X8_SRGB
    128, // ASTC_2D_6X6_UNORM
    128, // ASTC_2D_6X6_SRGB
    128, // ASTC_2D_10X6_UNORM
    128, // ASTC_2D_10X6_SRGB
    128, // ASTC_2D_10X5_UNORM
    128, // ASTC_2D_10X5_SRGB
    128, // ASTC_2D_10X10_UNORM
    128, // ASTC_2D_10X10_SRGB
    128, // ASTC_2D_12X10_UNORM
    128, // ASTC_2D_12X10_SRGB
    128, // ASTC_2D_12X12_UNORM
    128, // ASTC_2D_12X12_SRGB
    128, // ASTC_2D_8X6_UNORM
    128, // ASTC_2D_8X6_SRGB
    128, // ASTC_2D_6X5_UNORM
    128, // ASTC_2D_6X5_SRGB
    32,  // E5B9G9R9_FLOAT
    32,  // D32_FLOAT
    16,  // D16_UNORM
    32,  // X8_D24_UNORM
    8,   // S8_UINT
    32,  // D24_UNORM_S8_UINT
    32,  // S8_UINT_D24_UNORM
    64,  // D32_FLOAT_S8_UINT
];

/// Returns bits per block for a pixel format.
///
/// Port of `BitsPerBlock` from `surface.h`.
pub fn bits_per_block(format: PixelFormat) -> u32 {
    let idx = format as usize;
    assert!(idx < BITS_PER_BLOCK_TABLE.len());
    BITS_PER_BLOCK_TABLE[idx] as u32
}

/// Returns bytes per block for a pixel format.
///
/// Port of `BytesPerBlock` from `surface.h`.
pub fn bytes_per_block(format: PixelFormat) -> u32 {
    bits_per_block(format) / 8
}

// ---------------------------------------------------------------------------
// SurfaceTarget helpers
// ---------------------------------------------------------------------------

/// Returns whether a surface target is layered (array or cubemap).
///
/// Port of `SurfaceTargetIsLayered` from `surface.cpp`.
pub fn surface_target_is_layered(target: SurfaceTarget) -> bool {
    matches!(
        target,
        SurfaceTarget::Texture1DArray
            | SurfaceTarget::Texture2DArray
            | SurfaceTarget::TextureCubemap
            | SurfaceTarget::TextureCubeArray
    )
}

/// Returns whether a surface target is an array type.
///
/// Port of `SurfaceTargetIsArray` from `surface.cpp`.
pub fn surface_target_is_array(target: SurfaceTarget) -> bool {
    matches!(
        target,
        SurfaceTarget::Texture1DArray
            | SurfaceTarget::Texture2DArray
            | SurfaceTarget::TextureCubeArray
    )
}

// ---------------------------------------------------------------------------
// Format type classification
// ---------------------------------------------------------------------------

/// Returns the surface type (color, depth, stencil, depth-stencil) for a format.
///
/// Port of `GetFormatType` from `surface.cpp`.
pub fn get_format_type(pixel_format: PixelFormat) -> SurfaceType {
    let idx = pixel_format as u32;
    if idx < MAX_COLOR_FORMAT {
        return SurfaceType::ColorTexture;
    }
    if idx < MAX_DEPTH_FORMAT {
        return SurfaceType::Depth;
    }
    if idx < MAX_STENCIL_FORMAT {
        return SurfaceType::Stencil;
    }
    if idx < MAX_DEPTH_STENCIL_FORMAT {
        return SurfaceType::DepthStencil;
    }
    SurfaceType::Invalid
}

// ---------------------------------------------------------------------------
// Format classification functions
// ---------------------------------------------------------------------------

/// Returns true if the format is an ASTC compressed format.
///
/// Port of `IsPixelFormatASTC` from `surface.cpp`.
pub fn is_pixel_format_astc(format: PixelFormat) -> bool {
    matches!(
        format,
        PixelFormat::Astc2d4x4Unorm
            | PixelFormat::Astc2d5x4Unorm
            | PixelFormat::Astc2d5x5Unorm
            | PixelFormat::Astc2d8x8Unorm
            | PixelFormat::Astc2d8x5Unorm
            | PixelFormat::Astc2d4x4Srgb
            | PixelFormat::Astc2d5x4Srgb
            | PixelFormat::Astc2d5x5Srgb
            | PixelFormat::Astc2d8x8Srgb
            | PixelFormat::Astc2d8x5Srgb
            | PixelFormat::Astc2d10x8Unorm
            | PixelFormat::Astc2d10x8Srgb
            | PixelFormat::Astc2d6x6Unorm
            | PixelFormat::Astc2d6x6Srgb
            | PixelFormat::Astc2d10x6Unorm
            | PixelFormat::Astc2d10x6Srgb
            | PixelFormat::Astc2d10x5Unorm
            | PixelFormat::Astc2d10x5Srgb
            | PixelFormat::Astc2d10x10Unorm
            | PixelFormat::Astc2d10x10Srgb
            | PixelFormat::Astc2d12x10Unorm
            | PixelFormat::Astc2d12x10Srgb
            | PixelFormat::Astc2d12x12Unorm
            | PixelFormat::Astc2d12x12Srgb
            | PixelFormat::Astc2d8x6Unorm
            | PixelFormat::Astc2d8x6Srgb
            | PixelFormat::Astc2d6x5Unorm
            | PixelFormat::Astc2d6x5Srgb
    )
}

/// Returns true if the format is a BCn compressed format.
///
/// Port of `IsPixelFormatBCn` from `surface.cpp`.
pub fn is_pixel_format_bcn(format: PixelFormat) -> bool {
    matches!(
        format,
        PixelFormat::Bc1RgbaUnorm
            | PixelFormat::Bc2Unorm
            | PixelFormat::Bc3Unorm
            | PixelFormat::Bc4Unorm
            | PixelFormat::Bc4Snorm
            | PixelFormat::Bc5Unorm
            | PixelFormat::Bc5Snorm
            | PixelFormat::Bc1RgbaSrgb
            | PixelFormat::Bc2Srgb
            | PixelFormat::Bc3Srgb
            | PixelFormat::Bc7Unorm
            | PixelFormat::Bc6hUfloat
            | PixelFormat::Bc6hSfloat
            | PixelFormat::Bc7Srgb
    )
}

/// Returns true if the format is an sRGB format.
///
/// Port of `IsPixelFormatSRGB` from `surface.cpp`.
pub fn is_pixel_format_srgb(format: PixelFormat) -> bool {
    matches!(
        format,
        PixelFormat::A8B8G8R8Srgb
            | PixelFormat::B8G8R8A8Srgb
            | PixelFormat::Bc1RgbaSrgb
            | PixelFormat::Bc2Srgb
            | PixelFormat::Bc3Srgb
            | PixelFormat::Bc7Srgb
            | PixelFormat::Astc2d4x4Srgb
            | PixelFormat::Astc2d8x8Srgb
            | PixelFormat::Astc2d8x5Srgb
            | PixelFormat::Astc2d5x4Srgb
            | PixelFormat::Astc2d5x5Srgb
            | PixelFormat::Astc2d10x6Srgb
            | PixelFormat::Astc2d10x8Srgb
            | PixelFormat::Astc2d6x6Srgb
            | PixelFormat::Astc2d10x5Srgb
            | PixelFormat::Astc2d10x10Srgb
            | PixelFormat::Astc2d12x12Srgb
            | PixelFormat::Astc2d12x10Srgb
            | PixelFormat::Astc2d8x6Srgb
            | PixelFormat::Astc2d6x5Srgb
    )
}

/// Returns true if the format is an integer format.
///
/// Port of `IsPixelFormatInteger` from `surface.cpp`.
pub fn is_pixel_format_integer(format: PixelFormat) -> bool {
    matches!(
        format,
        PixelFormat::A8B8G8R8Sint
            | PixelFormat::A8B8G8R8Uint
            | PixelFormat::A2B10G10R10Uint
            | PixelFormat::R8Sint
            | PixelFormat::R8Uint
            | PixelFormat::R16G16B16A16Sint
            | PixelFormat::R16G16B16A16Uint
            | PixelFormat::R32G32B32A32Uint
            | PixelFormat::R32G32B32A32Sint
            | PixelFormat::R32G32Sint
            | PixelFormat::R16Uint
            | PixelFormat::R16Sint
            | PixelFormat::R16G16Uint
            | PixelFormat::R16G16Sint
            | PixelFormat::R8G8Sint
            | PixelFormat::R8G8Uint
            | PixelFormat::R32G32Uint
            | PixelFormat::R32Uint
            | PixelFormat::R32Sint
    )
}

/// Returns true if the format is a signed integer format.
///
/// Port of `IsPixelFormatSignedInteger` from `surface.cpp`.
pub fn is_pixel_format_signed_integer(format: PixelFormat) -> bool {
    matches!(
        format,
        PixelFormat::A8B8G8R8Sint
            | PixelFormat::R8Sint
            | PixelFormat::R16G16B16A16Sint
            | PixelFormat::R32G32B32A32Sint
            | PixelFormat::R32G32Sint
            | PixelFormat::R16Sint
            | PixelFormat::R16G16Sint
            | PixelFormat::R8G8Sint
            | PixelFormat::R32Sint
    )
}

/// Returns the component size in bits for integer formats.
///
/// Port of `PixelComponentSizeBitsInteger` from `surface.cpp`.
pub fn pixel_component_size_bits_integer(format: PixelFormat) -> usize {
    match format {
        PixelFormat::A8B8G8R8Sint
        | PixelFormat::A8B8G8R8Uint
        | PixelFormat::R8Sint
        | PixelFormat::R8Uint
        | PixelFormat::R8G8Sint
        | PixelFormat::R8G8Uint => 8,
        PixelFormat::A2B10G10R10Uint => 10,
        PixelFormat::R16G16B16A16Sint
        | PixelFormat::R16G16B16A16Uint
        | PixelFormat::R16Uint
        | PixelFormat::R16Sint
        | PixelFormat::R16G16Uint
        | PixelFormat::R16G16Sint => 16,
        PixelFormat::R32G32B32A32Uint
        | PixelFormat::R32G32B32A32Sint
        | PixelFormat::R32G32Sint
        | PixelFormat::R32G32Uint
        | PixelFormat::R32Uint
        | PixelFormat::R32Sint => 32,
        _ => 0,
    }
}

/// Returns (block_width, block_height) for an ASTC format.
///
/// Port of `GetASTCBlockSize` from `surface.cpp`.
pub fn get_astc_block_size(format: PixelFormat) -> (u32, u32) {
    (default_block_width(format), default_block_height(format))
}

/// Returns the size of an ASTC texture after transcoding.
///
/// Port of `TranscodedAstcSize` from `surface.cpp`.
/// Note: the `astc_recompression` setting is not available here;
/// this returns the uncompressed size (RGBA8 equivalent).
pub fn transcoded_astc_size(base_size: u64, format: PixelFormat) -> u64 {
    const RGBA8_PIXEL_SIZE: u64 = 4;
    let base_block_size =
        (default_block_width(format) as u64) * (default_block_height(format) as u64) * RGBA8_PIXEL_SIZE;
    let bpb = bytes_per_block(format) as u64;
    if bpb == 0 {
        return 0;
    }
    (base_size * base_block_size) / bpb
}

// ---------------------------------------------------------------------------
// IsViewCompatible (referenced by image_view_base)
// ---------------------------------------------------------------------------

/// Check if a view format is compatible with an image format.
///
/// Port of `VideoCore::Surface::IsViewCompatible`.
/// Simplified: checks if the two formats have the same bits-per-block
/// and belong to the same format class. A full implementation would check
/// the compatibility table from `compatible_formats.rs`.
pub fn is_view_compatible(
    image_format: PixelFormat,
    view_format: PixelFormat,
    _broken_views: bool,
    _native_bgr: bool,
) -> bool {
    if image_format == view_format {
        return true;
    }
    // Same bits per block is a necessary (but not sufficient) condition
    let image_idx = image_format as usize;
    let view_idx = view_format as usize;
    if image_idx >= BITS_PER_BLOCK_TABLE.len() || view_idx >= BITS_PER_BLOCK_TABLE.len() {
        return false;
    }
    BITS_PER_BLOCK_TABLE[image_idx] == BITS_PER_BLOCK_TABLE[view_idx]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn table_sizes() {
        assert_eq!(BLOCK_WIDTH_TABLE.len(), MAX_PIXEL_FORMAT);
        assert_eq!(BLOCK_HEIGHT_TABLE.len(), MAX_PIXEL_FORMAT);
        assert_eq!(BITS_PER_BLOCK_TABLE.len(), MAX_PIXEL_FORMAT);
    }

    #[test]
    fn block_dimensions_basic() {
        // Uncompressed RGBA8 should be 1x1
        assert_eq!(default_block_width(PixelFormat::A8B8G8R8Unorm), 1);
        assert_eq!(default_block_height(PixelFormat::A8B8G8R8Unorm), 1);
        // BC1 should be 4x4
        assert_eq!(default_block_width(PixelFormat::Bc1RgbaUnorm), 4);
        assert_eq!(default_block_height(PixelFormat::Bc1RgbaUnorm), 4);
        // ASTC 8x8 should be 8x8
        assert_eq!(default_block_width(PixelFormat::Astc2d8x8Unorm), 8);
        assert_eq!(default_block_height(PixelFormat::Astc2d8x8Unorm), 8);
    }

    #[test]
    fn bits_per_block_basic() {
        assert_eq!(bits_per_block(PixelFormat::A8B8G8R8Unorm), 32);
        assert_eq!(bits_per_block(PixelFormat::R8Unorm), 8);
        assert_eq!(bits_per_block(PixelFormat::R16G16B16A16Float), 64);
        assert_eq!(bits_per_block(PixelFormat::R32G32B32A32Float), 128);
        assert_eq!(bits_per_block(PixelFormat::Bc1RgbaUnorm), 64);
    }

    #[test]
    fn bytes_per_block_basic() {
        assert_eq!(bytes_per_block(PixelFormat::A8B8G8R8Unorm), 4);
        assert_eq!(bytes_per_block(PixelFormat::R8Unorm), 1);
        assert_eq!(bytes_per_block(PixelFormat::R16Float), 2);
    }

    #[test]
    fn format_type_classification() {
        assert_eq!(get_format_type(PixelFormat::A8B8G8R8Unorm), SurfaceType::ColorTexture);
        assert_eq!(get_format_type(PixelFormat::D32Float), SurfaceType::Depth);
        assert_eq!(get_format_type(PixelFormat::S8Uint), SurfaceType::Stencil);
        assert_eq!(get_format_type(PixelFormat::D24UnormS8Uint), SurfaceType::DepthStencil);
    }

    #[test]
    fn astc_classification() {
        assert!(is_pixel_format_astc(PixelFormat::Astc2d4x4Unorm));
        assert!(is_pixel_format_astc(PixelFormat::Astc2d8x8Srgb));
        assert!(!is_pixel_format_astc(PixelFormat::A8B8G8R8Unorm));
        assert!(!is_pixel_format_astc(PixelFormat::Bc1RgbaUnorm));
    }

    #[test]
    fn bcn_classification() {
        assert!(is_pixel_format_bcn(PixelFormat::Bc1RgbaUnorm));
        assert!(is_pixel_format_bcn(PixelFormat::Bc7Srgb));
        assert!(!is_pixel_format_bcn(PixelFormat::A8B8G8R8Unorm));
        assert!(!is_pixel_format_bcn(PixelFormat::Astc2d4x4Unorm));
    }

    #[test]
    fn srgb_classification() {
        assert!(is_pixel_format_srgb(PixelFormat::A8B8G8R8Srgb));
        assert!(is_pixel_format_srgb(PixelFormat::Bc7Srgb));
        assert!(!is_pixel_format_srgb(PixelFormat::A8B8G8R8Unorm));
    }

    #[test]
    fn integer_classification() {
        assert!(is_pixel_format_integer(PixelFormat::A8B8G8R8Uint));
        assert!(is_pixel_format_integer(PixelFormat::R32Uint));
        assert!(!is_pixel_format_integer(PixelFormat::R32Float));
        assert!(is_pixel_format_signed_integer(PixelFormat::R32Sint));
        assert!(!is_pixel_format_signed_integer(PixelFormat::R32Uint));
    }

    #[test]
    fn component_size_bits() {
        assert_eq!(pixel_component_size_bits_integer(PixelFormat::R8Uint), 8);
        assert_eq!(pixel_component_size_bits_integer(PixelFormat::R16Uint), 16);
        assert_eq!(pixel_component_size_bits_integer(PixelFormat::R32Uint), 32);
        assert_eq!(pixel_component_size_bits_integer(PixelFormat::A2B10G10R10Uint), 10);
        assert_eq!(pixel_component_size_bits_integer(PixelFormat::R32Float), 0);
    }

    #[test]
    fn surface_target_layered() {
        assert!(surface_target_is_layered(SurfaceTarget::Texture2DArray));
        assert!(surface_target_is_layered(SurfaceTarget::TextureCubemap));
        assert!(!surface_target_is_layered(SurfaceTarget::Texture2D));
        assert!(!surface_target_is_layered(SurfaceTarget::Texture3D));
    }

    #[test]
    fn surface_target_array() {
        assert!(surface_target_is_array(SurfaceTarget::Texture1DArray));
        assert!(surface_target_is_array(SurfaceTarget::Texture2DArray));
        assert!(!surface_target_is_array(SurfaceTarget::TextureCubemap));
        assert!(!surface_target_is_array(SurfaceTarget::Texture2D));
    }

    #[test]
    fn view_compatible_same_format() {
        assert!(is_view_compatible(
            PixelFormat::A8B8G8R8Unorm,
            PixelFormat::A8B8G8R8Unorm,
            false,
            true,
        ));
    }

    #[test]
    fn astc_block_size() {
        assert_eq!(get_astc_block_size(PixelFormat::Astc2d4x4Unorm), (4, 4));
        assert_eq!(get_astc_block_size(PixelFormat::Astc2d8x8Unorm), (8, 8));
        assert_eq!(get_astc_block_size(PixelFormat::Astc2d10x6Unorm), (10, 6));
    }

    #[test]
    fn transcoded_astc_size_basic() {
        // A 128-bit block covering 4x4 pixels -> 4*4*4 = 64 bytes uncompressed
        // base_size = 16 bytes (one ASTC block), format 4x4 (block=16 bytes)
        let size = transcoded_astc_size(16, PixelFormat::Astc2d4x4Unorm);
        // 16 * (4*4*4) / 16 = 64
        assert_eq!(size, 64);
    }
}
