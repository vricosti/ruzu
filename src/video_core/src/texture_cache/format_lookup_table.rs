// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/format_lookup_table.h and format_lookup_table.cpp
//!
//! Maps Tegra texture format + component types + sRGB flag to `PixelFormat`.

// `PixelFormat` is defined in `surface.rs`, mirroring upstream's
// `video_core/surface.h`. Re-exported here so the texture_cache modules that
// already import it from this file keep resolving.
pub use crate::surface::PixelFormat;

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

impl ComponentType {
    pub fn from_raw(value: u32) -> Option<Self> {
        match value {
            1 => Some(Self::Snorm),
            2 => Some(Self::Unorm),
            3 => Some(Self::Sint),
            4 => Some(Self::Uint),
            7 => Some(Self::Float),
            _ => None,
        }
    }
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
    ETC2_RGB = 0x06,
    X8B8G8R8 = 0x07,
    A8B8G8R8 = 0x08,
    A2B10G10R10 = 0x09,
    ETC2_RGB_PTA = 0x0a,
    ETC2_RGBA = 0x0b,
    R16G16 = 0x0c,
    G8R24 = 0x0d,
    G24R8 = 0x0e,
    R32 = 0x0f,
    BC6H_S16 = 0x10,
    BC6H_U16 = 0x11,
    A4B4G4R4 = 0x12,
    A5B5G5R1 = 0x13,
    A1B5G5R5 = 0x14,
    B5G6R5 = 0x15,
    B6G5R5 = 0x16,
    BC7U = 0x17,
    G8R8 = 0x18,
    EAC = 0x19,
    EACX2 = 0x1a,
    R16 = 0x1b,
    Y8_VIDEO = 0x1c,
    R8 = 0x1d,
    G4R4 = 0x1e,
    R1 = 0x1f,
    E5B9G9R9 = 0x20,
    B10G11R11 = 0x21,
    G8B8G8R8 = 0x22,
    B8G8R8G8 = 0x23,
    DXT1 = 0x24,
    DXT23 = 0x25,
    DXT45 = 0x26,
    DXN1 = 0x27,
    DXN2 = 0x28,
    Z24S8 = 0x29,
    X8Z24 = 0x2a,
    S8Z24 = 0x2b,
    Z32 = 0x2f,
    Z32X24S8 = 0x30,
    Z16 = 0x3a,
    Astc2d4x4 = 0x40,
    Astc2d5x5 = 0x41,
    Astc2d6x6 = 0x42,
    Astc2d8x8 = 0x44,
    Astc2d10x10 = 0x45,
    Astc2d12x12 = 0x46,
    Astc2d5x4 = 0x50,
    Astc2d6x5 = 0x51,
    Astc2d8x6 = 0x52,
    Astc2d10x8 = 0x53,
    Astc2d12x10 = 0x54,
    Astc2d8x5 = 0x55,
    Astc2d10x5 = 0x56,
    Astc2d10x6 = 0x57,
}

impl TextureFormat {
    pub fn from_raw(value: u32) -> Option<Self> {
        match value {
            0x01 => Some(Self::R32G32B32A32),
            0x02 => Some(Self::R32G32B32),
            0x03 => Some(Self::R16G16B16A16),
            0x04 => Some(Self::R32G32),
            0x05 => Some(Self::R32B24G8),
            0x06 => Some(Self::ETC2_RGB),
            0x07 => Some(Self::X8B8G8R8),
            0x08 => Some(Self::A8B8G8R8),
            0x09 => Some(Self::A2B10G10R10),
            0x0a => Some(Self::ETC2_RGB_PTA),
            0x0b => Some(Self::ETC2_RGBA),
            0x0c => Some(Self::R16G16),
            0x0d => Some(Self::G8R24),
            0x0e => Some(Self::G24R8),
            0x0f => Some(Self::R32),
            0x10 => Some(Self::BC6H_S16),
            0x11 => Some(Self::BC6H_U16),
            0x12 => Some(Self::A4B4G4R4),
            0x13 => Some(Self::A5B5G5R1),
            0x14 => Some(Self::A1B5G5R5),
            0x15 => Some(Self::B5G6R5),
            0x16 => Some(Self::B6G5R5),
            0x17 => Some(Self::BC7U),
            0x18 => Some(Self::G8R8),
            0x19 => Some(Self::EAC),
            0x1a => Some(Self::EACX2),
            0x1b => Some(Self::R16),
            0x1c => Some(Self::Y8_VIDEO),
            0x1d => Some(Self::R8),
            0x1e => Some(Self::G4R4),
            0x1f => Some(Self::R1),
            0x20 => Some(Self::E5B9G9R9),
            0x21 => Some(Self::B10G11R11),
            0x22 => Some(Self::G8B8G8R8),
            0x23 => Some(Self::B8G8R8G8),
            0x24 => Some(Self::DXT1),
            0x25 => Some(Self::DXT23),
            0x26 => Some(Self::DXT45),
            0x27 => Some(Self::DXN1),
            0x28 => Some(Self::DXN2),
            0x29 => Some(Self::Z24S8),
            0x2a => Some(Self::X8Z24),
            0x2b => Some(Self::S8Z24),
            0x2f => Some(Self::Z32),
            0x30 => Some(Self::Z32X24S8),
            0x3a => Some(Self::Z16),
            0x40 => Some(Self::Astc2d4x4),
            0x41 => Some(Self::Astc2d5x5),
            0x42 => Some(Self::Astc2d6x6),
            0x44 => Some(Self::Astc2d8x8),
            0x45 => Some(Self::Astc2d10x10),
            0x46 => Some(Self::Astc2d12x12),
            0x50 => Some(Self::Astc2d5x4),
            0x51 => Some(Self::Astc2d6x5),
            0x52 => Some(Self::Astc2d8x6),
            0x53 => Some(Self::Astc2d10x8),
            0x54 => Some(Self::Astc2d12x10),
            0x55 => Some(Self::Astc2d8x5),
            0x56 => Some(Self::Astc2d10x5),
            0x57 => Some(Self::Astc2d10x6),
            _ => None,
        }
    }
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
    pixel_format_from_texture_info_raw(
        format as u32,
        red as u32,
        green as u32,
        blue as u32,
        alpha as u32,
        is_srgb,
    )
}

fn unimplemented_texture_format(
    format: u32,
    red: u32,
    green: u32,
    blue: u32,
    alpha: u32,
    is_srgb: bool,
) -> PixelFormat {
    // Upstream's `PixelFormatFromTextureInfo` is `noexcept`: an unknown TIC
    // tuple reaches `UNIMPLEMENTED_MSG` and then deliberately falls back to
    // A8B8G8R8_UNORM.  Panicking here kills Reden's GPU thread while Eden
    // continues compiling the shader with the fallback format.
    log::error!(
        "PixelFormatFromTextureInfo: unimplemented texture format={} srgb={} components=({} {} {} {})",
        format,
        is_srgb,
        red,
        green,
        blue,
        alpha
    );
    PixelFormat::A8B8G8R8Unorm
}

pub fn pixel_format_from_texture_info_raw(
    format: u32,
    red: u32,
    green: u32,
    blue: u32,
    alpha: u32,
    is_srgb: bool,
) -> PixelFormat {
    let hash = format_hash(format, red, green, blue, alpha, is_srgb);

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
        // ETC2
        x if x == h_uni!(TF::ETC2_RGB, UNORM) => PixelFormat::Etc2RgbUnorm,
        x if x == h_uni!(TF::ETC2_RGB_PTA, UNORM) => PixelFormat::Etc2RgbPtaUnorm,
        x if x == h_uni!(TF::ETC2_RGBA, UNORM) => PixelFormat::Etc2RgbaUnorm,
        x if x == h_uni!(TF::ETC2_RGB, UNORM, srgb) => PixelFormat::Etc2RgbSrgb,
        x if x == h_uni!(TF::ETC2_RGB_PTA, UNORM, srgb) => PixelFormat::Etc2RgbPtaSrgb,
        x if x == h_uni!(TF::ETC2_RGBA, UNORM, srgb) => PixelFormat::Etc2RgbaSrgb,
        // EAC
        x if x == h_uni!(TF::EAC, UNORM) => PixelFormat::EacR11Unorm,
        x if x == h_uni!(TF::EAC, SNORM) => PixelFormat::EacR11Snorm,
        x if x == h_uni!(TF::EACX2, UNORM) => PixelFormat::EacR11G11Unorm,
        x if x == h_uni!(TF::EACX2, SNORM) => PixelFormat::EacR11G11Snorm,
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
        _ => unimplemented_texture_format(format, red, green, blue, alpha, is_srgb),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn known_texture_format_mapping_still_matches_upstream_table() {
        assert_eq!(
            pixel_format_from_texture_info(
                TextureFormat::A8B8G8R8,
                ComponentType::Unorm,
                ComponentType::Unorm,
                ComponentType::Unorm,
                ComponentType::Unorm,
                false,
            ),
            PixelFormat::A8B8G8R8Unorm
        );
    }

    #[test]
    fn unknown_texture_component_tuple_uses_upstream_fallback() {
        assert_eq!(
            pixel_format_from_texture_info_raw(
                TextureFormat::A8B8G8R8 as u32,
                ComponentType::Float as u32,
                ComponentType::Float as u32,
                ComponentType::Float as u32,
                ComponentType::Float as u32,
                false,
            ),
            PixelFormat::A8B8G8R8Unorm,
        );
    }

    #[test]
    fn unknown_texture_format_uses_upstream_fallback() {
        assert_eq!(
            pixel_format_from_texture_info_raw(0xFFFF_FFFF, 0, 0, 0, 0, false),
            PixelFormat::A8B8G8R8Unorm,
        );
    }

    #[test]
    fn zeroed_texture_info_uses_upstream_fallback() {
        assert_eq!(
            pixel_format_from_texture_info_raw(0, 0, 0, 0, 0, false),
            PixelFormat::A8B8G8R8Unorm,
        );
    }
    // The ten ETC2/EAC entries upstream's `PixelFormatFromTextureInfo` maps.
    // Note upstream's ordering quirk: `ETC2_RGB_PTA` (0x0a) precedes
    // `ETC2_RGBA` (0x0b) in the switch, and EAC has no sRGB variant.
    #[test]
    fn etc2_and_eac_texture_formats_map_like_upstream() {
        let uni = |format, component, srgb| {
            pixel_format_from_texture_info(format, component, component, component, component, srgb)
        };
        use ComponentType::{Snorm, Unorm};

        assert_eq!(
            uni(TextureFormat::ETC2_RGB, Unorm, false),
            PixelFormat::Etc2RgbUnorm
        );
        assert_eq!(
            uni(TextureFormat::ETC2_RGB_PTA, Unorm, false),
            PixelFormat::Etc2RgbPtaUnorm
        );
        assert_eq!(
            uni(TextureFormat::ETC2_RGBA, Unorm, false),
            PixelFormat::Etc2RgbaUnorm
        );
        assert_eq!(
            uni(TextureFormat::ETC2_RGB, Unorm, true),
            PixelFormat::Etc2RgbSrgb
        );
        assert_eq!(
            uni(TextureFormat::ETC2_RGB_PTA, Unorm, true),
            PixelFormat::Etc2RgbPtaSrgb
        );
        assert_eq!(
            uni(TextureFormat::ETC2_RGBA, Unorm, true),
            PixelFormat::Etc2RgbaSrgb
        );
        assert_eq!(
            uni(TextureFormat::EAC, Unorm, false),
            PixelFormat::EacR11Unorm
        );
        assert_eq!(
            uni(TextureFormat::EAC, Snorm, false),
            PixelFormat::EacR11Snorm
        );
        assert_eq!(
            uni(TextureFormat::EACX2, Unorm, false),
            PixelFormat::EacR11G11Unorm
        );
        assert_eq!(
            uni(TextureFormat::EACX2, Snorm, false),
            PixelFormat::EacR11G11Snorm
        );
    }
}
