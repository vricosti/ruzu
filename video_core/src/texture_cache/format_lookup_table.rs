// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/format_lookup_table.h and format_lookup_table.cpp
//!
//! Maps Tegra texture format + component types + sRGB flag to `PixelFormat`.

// ── PixelFormat ────────────────────────────────────────────────────────
// Upstream lives in video_core/surface.h (`VideoCore::Surface::PixelFormat`).
// Until that module is fully ported we carry the enum here so that all
// texture_cache files can reference it.  Keep in sync with upstream.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
#[allow(non_camel_case_types)]
pub enum PixelFormat {
    A8B8G8R8Unorm = 0,
    A8B8G8R8Snorm,
    A8B8G8R8Sint,
    A8B8G8R8Uint,
    R5G6B5Unorm,
    B5G6R5Unorm,
    A1R5G5B5Unorm,
    A2B10G10R10Unorm,
    A2B10G10R10Uint,
    A2R10G10B10Unorm,
    A1B5G5R5Unorm,
    A5B5G5R1Unorm,
    R8Unorm,
    R8Snorm,
    R8Sint,
    R8Uint,
    R16G16B16A16Float,
    R16G16B16A16Unorm,
    R16G16B16A16Snorm,
    R16G16B16A16Sint,
    R16G16B16A16Uint,
    B10G11R11Float,
    R32G32B32A32Uint,
    Bc1RgbaUnorm,
    Bc2Unorm,
    Bc3Unorm,
    Bc4Unorm,
    Bc4Snorm,
    Bc5Unorm,
    Bc5Snorm,
    Bc7Unorm,
    Bc6hUfloat,
    Bc6hSfloat,
    Astc2d4x4Unorm,
    B8G8R8A8Unorm,
    R32G32B32A32Float,
    R32G32B32A32Sint,
    R32G32Float,
    R32G32Sint,
    R32Float,
    R16Float,
    R16Unorm,
    R16Snorm,
    R16Uint,
    R16Sint,
    R16G16Unorm,
    R16G16Float,
    R16G16Uint,
    R16G16Sint,
    R16G16Snorm,
    R32G32B32Float,
    A8B8G8R8Srgb,
    R8G8Unorm,
    R8G8Snorm,
    R8G8Sint,
    R8G8Uint,
    R32G32Uint,
    R16G16B16X16Float,
    R32Uint,
    R32Sint,
    Astc2d8x8Unorm,
    Astc2d8x5Unorm,
    Astc2d5x4Unorm,
    B8G8R8A8Srgb,
    Bc1RgbaSrgb,
    Bc2Srgb,
    Bc3Srgb,
    Bc7Srgb,
    A4B4G4R4Unorm,
    G4R4Unorm,
    Astc2d4x4Srgb,
    Astc2d8x8Srgb,
    Astc2d8x5Srgb,
    Astc2d5x4Srgb,
    Astc2d5x5Unorm,
    Astc2d5x5Srgb,
    Astc2d10x8Unorm,
    Astc2d10x8Srgb,
    Astc2d6x6Unorm,
    Astc2d6x6Srgb,
    Astc2d10x6Unorm,
    Astc2d10x6Srgb,
    Astc2d10x5Unorm,
    Astc2d10x5Srgb,
    Astc2d10x10Unorm,
    Astc2d10x10Srgb,
    Astc2d12x10Unorm,
    Astc2d12x10Srgb,
    Astc2d12x12Unorm,
    Astc2d12x12Srgb,
    Astc2d8x6Unorm,
    Astc2d8x6Srgb,
    Astc2d6x5Unorm,
    Astc2d6x5Srgb,
    E5B9G9R9Float,

    // Depth formats
    D32Float,
    D16Unorm,
    X8D24Unorm,
    S8Uint,
    D24UnormS8Uint,
    S8UintD24Unorm,
    D32FloatS8Uint,

    MaxDepthStencilFormat,

    #[default]
    Invalid,
}

// ── ComponentType placeholder ──────────────────────────────────────────
// Upstream: Tegra::Texture::ComponentType

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum ComponentType {
    Snorm = 1,
    Unorm = 2,
    Sint = 3,
    Uint = 4,
    Float = 7,
}

// ── TextureFormat placeholder ──────────────────────────────────────────
// Upstream: Tegra::Texture::TextureFormat

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum TextureFormat {
    R32G32B32A32 = 0x01,
    R32G32B32 = 0x02,
    R16G16B16A16 = 0x03,
    R32G32 = 0x04,
    R32B24G8 = 0x05,
    E5B9G9R9 = 0x06,
    A8B8G8R8 = 0x08,
    A2B10G10R10 = 0x09,
    G8R8 = 0x0c,
    R16 = 0x0e,
    R8 = 0x0f,
    R16G16 = 0x0d,
    R32 = 0x0b,
    B10G11R11 = 0x07,
    B5G6R5 = 0x15,
    A1B5G5R5 = 0x14,
    A5B5G5R1 = 0x16,
    A4B4G4R4 = 0x12,
    G4R4 = 0x1d,
    A2R10G10B10 = 0x0a,
    // Z32 = 0x0a — same discriminant as A2R10G10B10, use alias below
    X8Z24 = 0x10,
    Z16 = 0x13,
    Z24S8 = 0x11,
    // G24R8 = 0x11 — same discriminant as Z24S8, use alias below
    // S8Z24 = 0x14 — same discriminant as A1B5G5R5, use alias below
    Z32X24S8 = 0x19,
    DXT1 = 0x24,
    DXT23 = 0x25,
    DXT45 = 0x26,
    DXN1 = 0x27,
    DXN2 = 0x28,
    BC7U = 0x17,
    // BC6hS16 = 0x10 — same discriminant as X8Z24, use alias below
    // BC6hU16 = 0x10 — same discriminant as X8Z24, use alias below
    Astc2d4x4 = 0x40,
    Astc2d5x4 = 0x50,
    Astc2d5x5 = 0x41,
    Astc2d8x8 = 0x44,
    Astc2d8x5 = 0x51,
    Astc2d10x8 = 0x52,
    Astc2d6x6 = 0x42,
    Astc2d10x6 = 0x53,
    Astc2d10x5 = 0x54,
    Astc2d10x10 = 0x45,
    Astc2d12x10 = 0x55,
    Astc2d12x12 = 0x46,
    Astc2d8x6 = 0x43,
    Astc2d6x5 = 0x56,
    // S8 = 0x0a — same discriminant as A2R10G10B10, use alias below
}

// Aliases for duplicate discriminant values.
// Upstream C++ uses these as separate enum values with the same numeric value.
impl TextureFormat {
    pub const Z32: Self = Self::A2R10G10B10;
    pub const G24R8: Self = Self::Z24S8;
    pub const S8Z24: Self = Self::A1B5G5R5;
    pub const BC6H_S16: Self = Self::X8Z24;
    pub const BC6H_U16: Self = Self::X8Z24;
    pub const S8: Self = Self::A2R10G10B10;
}

// ── Hash function ──────────────────────────────────────────────────────

/// Port of the anonymous-namespace `Hash` function from format_lookup_table.cpp.
const fn format_hash(
    format: u32,
    red: u32,
    green: u32,
    blue: u32,
    alpha: u32,
    is_srgb: bool,
) -> u32 {
    let mut h = if is_srgb { 1u32 } else { 0u32 };
    h |= red << 1;
    h |= green << 4;
    h |= blue << 7;
    h |= alpha << 10;
    h |= format << 13;
    h
}

/// Convenience overload: all four components are the same.
const fn format_hash_uniform(format: u32, component: u32, is_srgb: bool) -> u32 {
    format_hash(format, component, component, component, component, is_srgb)
}

// ── pixel_format_from_texture_info ────────────────────────────────────

/// Map a Tegra texture descriptor's format fields to a `PixelFormat`.
///
/// Port of `VideoCommon::PixelFormatFromTextureInfo`.
///
/// NOTE: The full match table is reproduced from format_lookup_table.cpp.
/// The `TextureFormat` discriminant values used here must exactly match
/// the upstream enum.
pub fn pixel_format_from_texture_info(
    format: TextureFormat,
    red: ComponentType,
    green: ComponentType,
    blue: ComponentType,
    alpha: ComponentType,
    is_srgb: bool,
) -> PixelFormat {
    let hash = format_hash(
        format as u32,
        red as u32,
        green as u32,
        blue as u32,
        alpha as u32,
        is_srgb,
    );

    // Shorthand constants matching upstream
    const SNORM: u32 = ComponentType::Snorm as u32;
    const UNORM: u32 = ComponentType::Unorm as u32;
    const SINT: u32 = ComponentType::Sint as u32;
    const UINT: u32 = ComponentType::Uint as u32;
    const FLOAT: u32 = ComponentType::Float as u32;

    macro_rules! h_uni {
        ($fmt:expr, $comp:expr) => {
            format_hash_uniform($fmt as u32, $comp, false)
        };
        ($fmt:expr, $comp:expr, srgb) => {
            format_hash_uniform($fmt as u32, $comp, true)
        };
    }
    macro_rules! h {
        ($fmt:expr, $r:expr, $g:expr, $b:expr, $a:expr) => {
            format_hash($fmt as u32, $r, $g, $b, $a, false)
        };
    }

    use TextureFormat as TF;
    match hash {
        x if x == h_uni!(TF::A8B8G8R8, UNORM) => PixelFormat::A8B8G8R8Unorm,
        x if x == h_uni!(TF::A8B8G8R8, SNORM) => PixelFormat::A8B8G8R8Snorm,
        x if x == h_uni!(TF::A8B8G8R8, UINT) => PixelFormat::A8B8G8R8Uint,
        x if x == h_uni!(TF::A8B8G8R8, SINT) => PixelFormat::A8B8G8R8Sint,
        x if x == h_uni!(TF::A8B8G8R8, UNORM, srgb) => PixelFormat::A8B8G8R8Srgb,
        x if x == h_uni!(TF::B5G6R5, UNORM) => PixelFormat::B5G6R5Unorm,
        x if x == h_uni!(TF::A2B10G10R10, UNORM) => PixelFormat::A2B10G10R10Unorm,
        x if x == h_uni!(TF::A2B10G10R10, UINT) => PixelFormat::A2B10G10R10Uint,
        x if x == h_uni!(TF::A1B5G5R5, UNORM) => PixelFormat::A1B5G5R5Unorm,
        x if x == h_uni!(TF::A4B4G4R4, UNORM) => PixelFormat::A4B4G4R4Unorm,
        x if x == h_uni!(TF::G4R4, UNORM) => PixelFormat::G4R4Unorm,
        x if x == h_uni!(TF::A5B5G5R1, UNORM) => PixelFormat::A5B5G5R1Unorm,
        x if x == h_uni!(TF::R8, UNORM) => PixelFormat::R8Unorm,
        x if x == h_uni!(TF::R8, SNORM) => PixelFormat::R8Snorm,
        x if x == h_uni!(TF::R8, UINT) => PixelFormat::R8Uint,
        x if x == h_uni!(TF::R8, SINT) => PixelFormat::R8Sint,
        x if x == h_uni!(TF::G8R8, UNORM) => PixelFormat::R8G8Unorm,
        x if x == h_uni!(TF::G8R8, SNORM) => PixelFormat::R8G8Snorm,
        x if x == h_uni!(TF::G8R8, UINT) => PixelFormat::R8G8Uint,
        x if x == h_uni!(TF::G8R8, SINT) => PixelFormat::R8G8Sint,
        x if x == h_uni!(TF::R16G16B16A16, FLOAT) => PixelFormat::R16G16B16A16Float,
        x if x == h_uni!(TF::R16G16B16A16, UNORM) => PixelFormat::R16G16B16A16Unorm,
        x if x == h_uni!(TF::R16G16B16A16, SNORM) => PixelFormat::R16G16B16A16Snorm,
        x if x == h_uni!(TF::R16G16B16A16, UINT) => PixelFormat::R16G16B16A16Uint,
        x if x == h_uni!(TF::R16G16B16A16, SINT) => PixelFormat::R16G16B16A16Sint,
        x if x == h_uni!(TF::R16G16, FLOAT) => PixelFormat::R16G16Float,
        x if x == h_uni!(TF::R16G16, UNORM) => PixelFormat::R16G16Unorm,
        x if x == h_uni!(TF::R16G16, SNORM) => PixelFormat::R16G16Snorm,
        x if x == h_uni!(TF::R16G16, UINT) => PixelFormat::R16G16Uint,
        x if x == h_uni!(TF::R16G16, SINT) => PixelFormat::R16G16Sint,
        x if x == h_uni!(TF::R16, FLOAT) => PixelFormat::R16Float,
        x if x == h_uni!(TF::R16, UNORM) => PixelFormat::R16Unorm,
        x if x == h_uni!(TF::R16, SNORM) => PixelFormat::R16Snorm,
        x if x == h_uni!(TF::R16, UINT) => PixelFormat::R16Uint,
        x if x == h_uni!(TF::R16, SINT) => PixelFormat::R16Sint,
        x if x == h_uni!(TF::B10G11R11, FLOAT) => PixelFormat::B10G11R11Float,
        x if x == h_uni!(TF::R32G32B32A32, FLOAT) => PixelFormat::R32G32B32A32Float,
        x if x == h_uni!(TF::R32G32B32A32, UINT) => PixelFormat::R32G32B32A32Uint,
        x if x == h_uni!(TF::R32G32B32A32, SINT) => PixelFormat::R32G32B32A32Sint,
        x if x == h_uni!(TF::R32G32B32, FLOAT) => PixelFormat::R32G32B32Float,
        x if x == h_uni!(TF::R32G32, FLOAT) => PixelFormat::R32G32Float,
        x if x == h_uni!(TF::R32G32, UINT) => PixelFormat::R32G32Uint,
        x if x == h_uni!(TF::R32G32, SINT) => PixelFormat::R32G32Sint,
        x if x == h_uni!(TF::R32, FLOAT) => PixelFormat::R32Float,
        x if x == h_uni!(TF::R32, UINT) => PixelFormat::R32Uint,
        x if x == h_uni!(TF::R32, SINT) => PixelFormat::R32Sint,
        x if x == h_uni!(TF::E5B9G9R9, FLOAT) => PixelFormat::E5B9G9R9Float,
        // Depth/stencil formats
        x if x == h_uni!(TF::Z32, FLOAT) => PixelFormat::D32Float,
        x if x == h!(TF::Z32, FLOAT, UINT, UINT, UINT) => PixelFormat::D32Float,
        x if x == h_uni!(TF::Z16, UNORM) => PixelFormat::D16Unorm,
        x if x == h!(TF::Z16, UNORM, UINT, UINT, UINT) => PixelFormat::D16Unorm,
        x if x == h_uni!(TF::X8Z24, UNORM) => PixelFormat::X8D24Unorm,
        x if x == h!(TF::X8Z24, UNORM, UINT, UINT, UINT) => PixelFormat::X8D24Unorm,
        x if x == h!(TF::Z24S8, UINT, UNORM, UNORM, UNORM) => PixelFormat::S8UintD24Unorm,
        x if x == h!(TF::Z24S8, UINT, UNORM, UINT, UINT) => PixelFormat::S8UintD24Unorm,
        x if x == h!(TF::G24R8, UINT, UNORM, UNORM, UNORM) => PixelFormat::S8UintD24Unorm,
        x if x == h!(TF::S8Z24, UNORM, UINT, UINT, UINT) => PixelFormat::D24UnormS8Uint,
        x if x == h!(TF::Z32X24S8, FLOAT, UINT, UNORM, UNORM) => PixelFormat::D32FloatS8Uint,
        x if x == h!(TF::R32B24G8, FLOAT, UINT, UNORM, UNORM) => PixelFormat::D32FloatS8Uint,
        // Compressed
        x if x == h_uni!(TF::DXT1, UNORM) => PixelFormat::Bc1RgbaUnorm,
        x if x == h_uni!(TF::DXT1, UNORM, srgb) => PixelFormat::Bc1RgbaSrgb,
        x if x == h_uni!(TF::DXT23, UNORM) => PixelFormat::Bc2Unorm,
        x if x == h_uni!(TF::DXT23, UNORM, srgb) => PixelFormat::Bc2Srgb,
        x if x == h_uni!(TF::DXT45, UNORM) => PixelFormat::Bc3Unorm,
        x if x == h_uni!(TF::DXT45, UNORM, srgb) => PixelFormat::Bc3Srgb,
        x if x == h_uni!(TF::DXN1, UNORM) => PixelFormat::Bc4Unorm,
        x if x == h_uni!(TF::DXN1, SNORM) => PixelFormat::Bc4Snorm,
        x if x == h_uni!(TF::DXN2, UNORM) => PixelFormat::Bc5Unorm,
        x if x == h_uni!(TF::DXN2, SNORM) => PixelFormat::Bc5Snorm,
        x if x == h_uni!(TF::BC7U, UNORM) => PixelFormat::Bc7Unorm,
        x if x == h_uni!(TF::BC7U, UNORM, srgb) => PixelFormat::Bc7Srgb,
        x if x == h_uni!(TF::BC6H_S16, FLOAT) => PixelFormat::Bc6hSfloat,
        x if x == h_uni!(TF::BC6H_U16, FLOAT) => PixelFormat::Bc6hUfloat,
        // ASTC
        x if x == h_uni!(TF::Astc2d4x4, UNORM) => PixelFormat::Astc2d4x4Unorm,
        x if x == h_uni!(TF::Astc2d4x4, UNORM, srgb) => PixelFormat::Astc2d4x4Srgb,
        x if x == h_uni!(TF::Astc2d5x4, UNORM) => PixelFormat::Astc2d5x4Unorm,
        x if x == h_uni!(TF::Astc2d5x4, UNORM, srgb) => PixelFormat::Astc2d5x4Srgb,
        x if x == h_uni!(TF::Astc2d5x5, UNORM) => PixelFormat::Astc2d5x5Unorm,
        x if x == h_uni!(TF::Astc2d5x5, UNORM, srgb) => PixelFormat::Astc2d5x5Srgb,
        x if x == h_uni!(TF::Astc2d8x8, UNORM) => PixelFormat::Astc2d8x8Unorm,
        x if x == h_uni!(TF::Astc2d8x8, UNORM, srgb) => PixelFormat::Astc2d8x8Srgb,
        x if x == h_uni!(TF::Astc2d8x5, UNORM) => PixelFormat::Astc2d8x5Unorm,
        x if x == h_uni!(TF::Astc2d8x5, UNORM, srgb) => PixelFormat::Astc2d8x5Srgb,
        x if x == h_uni!(TF::Astc2d10x8, UNORM) => PixelFormat::Astc2d10x8Unorm,
        x if x == h_uni!(TF::Astc2d10x8, UNORM, srgb) => PixelFormat::Astc2d10x8Srgb,
        x if x == h_uni!(TF::Astc2d6x6, UNORM) => PixelFormat::Astc2d6x6Unorm,
        x if x == h_uni!(TF::Astc2d6x6, UNORM, srgb) => PixelFormat::Astc2d6x6Srgb,
        x if x == h_uni!(TF::Astc2d10x6, UNORM) => PixelFormat::Astc2d10x6Unorm,
        x if x == h_uni!(TF::Astc2d10x6, UNORM, srgb) => PixelFormat::Astc2d10x6Srgb,
        x if x == h_uni!(TF::Astc2d10x5, UNORM) => PixelFormat::Astc2d10x5Unorm,
        x if x == h_uni!(TF::Astc2d10x5, UNORM, srgb) => PixelFormat::Astc2d10x5Srgb,
        x if x == h_uni!(TF::Astc2d10x10, UNORM) => PixelFormat::Astc2d10x10Unorm,
        x if x == h_uni!(TF::Astc2d10x10, UNORM, srgb) => PixelFormat::Astc2d10x10Srgb,
        x if x == h_uni!(TF::Astc2d12x10, UNORM) => PixelFormat::Astc2d12x10Unorm,
        x if x == h_uni!(TF::Astc2d12x10, UNORM, srgb) => PixelFormat::Astc2d12x10Srgb,
        x if x == h_uni!(TF::Astc2d12x12, UNORM) => PixelFormat::Astc2d12x12Unorm,
        x if x == h_uni!(TF::Astc2d12x12, UNORM, srgb) => PixelFormat::Astc2d12x12Srgb,
        x if x == h_uni!(TF::Astc2d8x6, UNORM) => PixelFormat::Astc2d8x6Unorm,
        x if x == h_uni!(TF::Astc2d8x6, UNORM, srgb) => PixelFormat::Astc2d8x6Srgb,
        x if x == h_uni!(TF::Astc2d6x5, UNORM) => PixelFormat::Astc2d6x5Unorm,
        x if x == h_uni!(TF::Astc2d6x5, UNORM, srgb) => PixelFormat::Astc2d6x5Srgb,
        _ => {
            log::error!(
                "PixelFormatFromTextureInfo: unimplemented format={:?} srgb={} components=({:?} {:?} {:?} {:?})",
                format, is_srgb, red, green, blue, alpha
            );
            PixelFormat::A8B8G8R8Unorm
        }
    }
}
