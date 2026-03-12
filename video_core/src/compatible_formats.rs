// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/compatible_formats.h and video_core/compatible_formats.cpp
//!
//! Format compatibility tables for view and copy operations.
//! Compatibility table taken from Table 3.X.2 in:
//! https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_view.txt
//! and Table 4.X.1 in:
//! https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_copy_image.txt

// NOTE: This module depends on PixelFormat from the surface module.
// Once surface.rs is ported with the full PixelFormat enum from upstream
// video_core/surface.h, replace this re-export.
//
// For now, this module is structurally complete but will not compile until
// PixelFormat is available. The enum values referenced below correspond to
// the upstream VideoCore::Surface::PixelFormat enum.

/// Placeholder: this will be replaced by `crate::surface::PixelFormat` once ported.
/// The enum must have variants matching all referenced formats and a MAX_PIXEL_FORMAT const.
pub use pixel_format_stub::PixelFormat;

mod pixel_format_stub {
    /// Pixel format enum stub for compatible_formats.
    /// These values are indices matching upstream VideoCore::Surface::PixelFormat.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(usize)]
    #[allow(non_camel_case_types)]
    pub enum PixelFormat {
        A8B8G8R8Unorm = 0,
        A8B8G8R8Snorm = 1,
        A8B8G8R8Sint = 2,
        A8B8G8R8Uint = 3,
        R5G6B5Unorm = 4,
        B5G6R5Unorm = 5,
        A1R5G5B5Unorm = 6,
        A2B10G10R10Unorm = 7,
        A2B10G10R10Uint = 8,
        A2R10G10B10Unorm = 9,
        A1B5G5R5Unorm = 10,
        A5B5G5R1Unorm = 11,
        R8Unorm = 12,
        R8Snorm = 13,
        R8Sint = 14,
        R8Uint = 15,
        R16G16B16A16Float = 16,
        R16G16B16A16Unorm = 17,
        R16G16B16A16Snorm = 18,
        R16G16B16A16Sint = 19,
        R16G16B16A16Uint = 20,
        B10G11R11Float = 21,
        R32G32B32A32Float = 22,
        R32G32B32A32Sint = 23,
        R32G32B32A32Uint = 24,
        R32G32Float = 25,
        R32G32Sint = 26,
        R32G32Uint = 27,
        R16G16Unorm = 28,
        R16G16Snorm = 29,
        R16G16Float = 30,
        R16G16Uint = 31,
        R16G16Sint = 32,
        R16Unorm = 33,
        R16Snorm = 34,
        R16Uint = 35,
        R16Sint = 36,
        R16Float = 37,
        R32Float = 38,
        R32Uint = 39,
        R32Sint = 40,
        E5B9G9R9Float = 41,
        R8G8Unorm = 42,
        R8G8Snorm = 43,
        R8G8Sint = 44,
        R8G8Uint = 45,
        R32G32B32Float = 46,
        A8B8G8R8Srgb = 47,
        B8G8R8A8Unorm = 48,
        B8G8R8A8Srgb = 49,
        // Compressed formats
        BC1RgbaUnorm = 50,
        BC1RgbaSrgb = 51,
        BC2Unorm = 52,
        BC2Srgb = 53,
        BC3Unorm = 54,
        BC3Srgb = 55,
        BC4Unorm = 56,
        BC4Snorm = 57,
        BC5Unorm = 58,
        BC5Snorm = 59,
        BC7Unorm = 60,
        BC7Srgb = 61,
        BC6hUfloat = 62,
        BC6hSfloat = 63,
        // ASTC formats
        Astc2d4x4Unorm = 64,
        Astc2d4x4Srgb = 65,
        Astc2d5x4Unorm = 66,
        Astc2d5x4Srgb = 67,
        Astc2d5x5Unorm = 68,
        Astc2d5x5Srgb = 69,
        Astc2d6x5Unorm = 70,
        Astc2d6x5Srgb = 71,
        Astc2d6x6Unorm = 72,
        Astc2d6x6Srgb = 73,
        Astc2d8x5Unorm = 74,
        Astc2d8x5Srgb = 75,
        Astc2d8x8Unorm = 76,
        Astc2d8x8Srgb = 77,
        Astc2d10x5Unorm = 78,
        Astc2d10x5Srgb = 79,
        Astc2d10x6Unorm = 80,
        Astc2d10x6Srgb = 81,
        Astc2d10x8Unorm = 82,
        Astc2d10x8Srgb = 83,
        Astc2d10x10Unorm = 84,
        Astc2d10x10Srgb = 85,
        Astc2d12x10Unorm = 86,
        Astc2d12x10Srgb = 87,
        Astc2d12x12Unorm = 88,
        Astc2d12x12Srgb = 89,
        MaxPixelFormat = 90,
    }

    impl PixelFormat {
        pub const MAX_PIXEL_FORMAT: usize = Self::MaxPixelFormat as usize;
    }
}

/// Maximum pixel format count.
const MAX_PIXEL_FORMAT: usize = PixelFormat::MAX_PIXEL_FORMAT;

/// Bitfield table: for each format, 2 u64s encode which other formats are compatible.
type Table = [[u64; 2]; MAX_PIXEL_FORMAT];

fn enable(table: &mut Table, a: usize, b: usize) {
    table[a][b / 64] |= 1u64 << (b % 64);
    table[b][a / 64] |= 1u64 << (a % 64);
}

fn enable_format(table: &mut Table, a: PixelFormat, b: PixelFormat) {
    enable(table, a as usize, b as usize);
}

fn enable_range(table: &mut Table, range: &[PixelFormat]) {
    for (i, &a) in range.iter().enumerate() {
        for &b in &range[i..] {
            enable_format(table, a, b);
        }
    }
}

fn is_supported(table: &Table, a: PixelFormat, b: PixelFormat) -> bool {
    let a = a as usize;
    let b = b as usize;
    ((table[a][b / 64] >> (b % 64)) & 1) != 0
}

// View class compatibility groups
const VIEW_CLASS_128_BITS: &[PixelFormat] = &[
    PixelFormat::R32G32B32A32Float,
    PixelFormat::R32G32B32A32Uint,
    PixelFormat::R32G32B32A32Sint,
];

const VIEW_CLASS_96_BITS: &[PixelFormat] = &[PixelFormat::R32G32B32Float];

const VIEW_CLASS_64_BITS: &[PixelFormat] = &[
    PixelFormat::R32G32Float,
    PixelFormat::R32G32Uint,
    PixelFormat::R32G32Sint,
    PixelFormat::R16G16B16A16Float,
    PixelFormat::R16G16B16A16Unorm,
    PixelFormat::R16G16B16A16Snorm,
    PixelFormat::R16G16B16A16Uint,
    PixelFormat::R16G16B16A16Sint,
];

const VIEW_CLASS_32_BITS: &[PixelFormat] = &[
    PixelFormat::R16G16Float,
    PixelFormat::B10G11R11Float,
    PixelFormat::R32Float,
    PixelFormat::A2B10G10R10Unorm,
    PixelFormat::R16G16Uint,
    PixelFormat::R32Uint,
    PixelFormat::R16G16Sint,
    PixelFormat::R32Sint,
    PixelFormat::A8B8G8R8Unorm,
    PixelFormat::R16G16Unorm,
    PixelFormat::A8B8G8R8Snorm,
    PixelFormat::R16G16Snorm,
    PixelFormat::A8B8G8R8Srgb,
    PixelFormat::E5B9G9R9Float,
    PixelFormat::B8G8R8A8Unorm,
    PixelFormat::B8G8R8A8Srgb,
    PixelFormat::A8B8G8R8Uint,
    PixelFormat::A8B8G8R8Sint,
    PixelFormat::A2B10G10R10Uint,
];

const VIEW_CLASS_32_BITS_NO_BGR: &[PixelFormat] = &[
    PixelFormat::R16G16Float,
    PixelFormat::B10G11R11Float,
    PixelFormat::R32Float,
    PixelFormat::A2B10G10R10Unorm,
    PixelFormat::R16G16Uint,
    PixelFormat::R32Uint,
    PixelFormat::R16G16Sint,
    PixelFormat::R32Sint,
    PixelFormat::A8B8G8R8Unorm,
    PixelFormat::R16G16Unorm,
    PixelFormat::A8B8G8R8Snorm,
    PixelFormat::R16G16Snorm,
    PixelFormat::A8B8G8R8Srgb,
    PixelFormat::E5B9G9R9Float,
    PixelFormat::A8B8G8R8Uint,
    PixelFormat::A8B8G8R8Sint,
    PixelFormat::A2B10G10R10Uint,
];

const VIEW_CLASS_16_BITS: &[PixelFormat] = &[
    PixelFormat::R16Float,
    PixelFormat::R8G8Uint,
    PixelFormat::R16Uint,
    PixelFormat::R16Sint,
    PixelFormat::R8G8Unorm,
    PixelFormat::R16Unorm,
    PixelFormat::R8G8Snorm,
    PixelFormat::R16Snorm,
    PixelFormat::R8G8Sint,
];

const VIEW_CLASS_8_BITS: &[PixelFormat] = &[
    PixelFormat::R8Uint,
    PixelFormat::R8Unorm,
    PixelFormat::R8Sint,
    PixelFormat::R8Snorm,
];

const VIEW_CLASS_RGTC1_RED: &[PixelFormat] = &[PixelFormat::BC4Unorm, PixelFormat::BC4Snorm];
const VIEW_CLASS_RGTC2_RG: &[PixelFormat] = &[PixelFormat::BC5Unorm, PixelFormat::BC5Snorm];
const VIEW_CLASS_BPTC_UNORM: &[PixelFormat] = &[PixelFormat::BC7Unorm, PixelFormat::BC7Srgb];
const VIEW_CLASS_BPTC_FLOAT: &[PixelFormat] =
    &[PixelFormat::BC6hSfloat, PixelFormat::BC6hUfloat];

const VIEW_CLASS_ASTC_4X4_RGBA: &[PixelFormat] =
    &[PixelFormat::Astc2d4x4Unorm, PixelFormat::Astc2d4x4Srgb];
const VIEW_CLASS_ASTC_5X4_RGBA: &[PixelFormat] =
    &[PixelFormat::Astc2d5x4Unorm, PixelFormat::Astc2d5x4Srgb];
const VIEW_CLASS_ASTC_5X5_RGBA: &[PixelFormat] =
    &[PixelFormat::Astc2d5x5Unorm, PixelFormat::Astc2d5x5Srgb];
const VIEW_CLASS_ASTC_6X5_RGBA: &[PixelFormat] =
    &[PixelFormat::Astc2d6x5Unorm, PixelFormat::Astc2d6x5Srgb];
const VIEW_CLASS_ASTC_6X6_RGBA: &[PixelFormat] =
    &[PixelFormat::Astc2d6x6Unorm, PixelFormat::Astc2d6x6Srgb];
const VIEW_CLASS_ASTC_8X5_RGBA: &[PixelFormat] =
    &[PixelFormat::Astc2d8x5Unorm, PixelFormat::Astc2d8x5Srgb];
const VIEW_CLASS_ASTC_8X8_RGBA: &[PixelFormat] =
    &[PixelFormat::Astc2d8x8Unorm, PixelFormat::Astc2d8x8Srgb];
const VIEW_CLASS_ASTC_10X5_RGBA: &[PixelFormat] =
    &[PixelFormat::Astc2d10x5Unorm, PixelFormat::Astc2d10x5Srgb];
const VIEW_CLASS_ASTC_10X6_RGBA: &[PixelFormat] =
    &[PixelFormat::Astc2d10x6Unorm, PixelFormat::Astc2d10x6Srgb];
const VIEW_CLASS_ASTC_10X8_RGBA: &[PixelFormat] =
    &[PixelFormat::Astc2d10x8Unorm, PixelFormat::Astc2d10x8Srgb];
const VIEW_CLASS_ASTC_10X10_RGBA: &[PixelFormat] =
    &[PixelFormat::Astc2d10x10Unorm, PixelFormat::Astc2d10x10Srgb];
const VIEW_CLASS_ASTC_12X10_RGBA: &[PixelFormat] =
    &[PixelFormat::Astc2d12x10Unorm, PixelFormat::Astc2d12x10Srgb];
const VIEW_CLASS_ASTC_12X12_RGBA: &[PixelFormat] =
    &[PixelFormat::Astc2d12x12Unorm, PixelFormat::Astc2d12x12Srgb];

// Copy class compatibility groups
const COPY_CLASS_128_BITS: &[PixelFormat] = &[
    PixelFormat::R32G32B32A32Uint,
    PixelFormat::R32G32B32A32Float,
    PixelFormat::R32G32B32A32Sint,
    PixelFormat::BC2Unorm,
    PixelFormat::BC2Srgb,
    PixelFormat::BC3Unorm,
    PixelFormat::BC3Srgb,
    PixelFormat::BC5Unorm,
    PixelFormat::BC5Snorm,
    PixelFormat::BC7Unorm,
    PixelFormat::BC7Srgb,
    PixelFormat::BC6hSfloat,
    PixelFormat::BC6hUfloat,
];

const COPY_CLASS_64_BITS: &[PixelFormat] = &[
    PixelFormat::R16G16B16A16Float,
    PixelFormat::R16G16B16A16Uint,
    PixelFormat::R16G16B16A16Unorm,
    PixelFormat::R16G16B16A16Snorm,
    PixelFormat::R16G16B16A16Sint,
    PixelFormat::R32G32Uint,
    PixelFormat::R32G32Float,
    PixelFormat::R32G32Sint,
    PixelFormat::BC1RgbaUnorm,
    PixelFormat::BC1RgbaSrgb,
];

fn make_view_table() -> Table {
    let mut view = [[0u64; 2]; MAX_PIXEL_FORMAT];
    // Identity is allowed
    for i in 0..MAX_PIXEL_FORMAT {
        enable(&mut view, i, i);
    }
    enable_range(&mut view, VIEW_CLASS_128_BITS);
    enable_range(&mut view, VIEW_CLASS_96_BITS);
    enable_range(&mut view, VIEW_CLASS_64_BITS);
    enable_range(&mut view, VIEW_CLASS_16_BITS);
    enable_range(&mut view, VIEW_CLASS_8_BITS);
    enable_range(&mut view, VIEW_CLASS_RGTC1_RED);
    enable_range(&mut view, VIEW_CLASS_RGTC2_RG);
    enable_range(&mut view, VIEW_CLASS_BPTC_UNORM);
    enable_range(&mut view, VIEW_CLASS_BPTC_FLOAT);
    enable_range(&mut view, VIEW_CLASS_ASTC_4X4_RGBA);
    enable_range(&mut view, VIEW_CLASS_ASTC_5X4_RGBA);
    enable_range(&mut view, VIEW_CLASS_ASTC_5X5_RGBA);
    enable_range(&mut view, VIEW_CLASS_ASTC_6X5_RGBA);
    enable_range(&mut view, VIEW_CLASS_ASTC_6X6_RGBA);
    enable_range(&mut view, VIEW_CLASS_ASTC_8X5_RGBA);
    enable_range(&mut view, VIEW_CLASS_ASTC_8X8_RGBA);
    enable_range(&mut view, VIEW_CLASS_ASTC_10X5_RGBA);
    enable_range(&mut view, VIEW_CLASS_ASTC_10X6_RGBA);
    enable_range(&mut view, VIEW_CLASS_ASTC_10X8_RGBA);
    enable_range(&mut view, VIEW_CLASS_ASTC_10X10_RGBA);
    enable_range(&mut view, VIEW_CLASS_ASTC_12X10_RGBA);
    enable_range(&mut view, VIEW_CLASS_ASTC_12X12_RGBA);
    view
}

fn make_copy_table() -> Table {
    let mut copy = make_view_table();
    enable_range(&mut copy, COPY_CLASS_128_BITS);
    enable_range(&mut copy, COPY_CLASS_64_BITS);
    copy
}

fn make_native_bgr_view_table() -> Table {
    let mut table = make_view_table();
    enable_range(&mut table, VIEW_CLASS_32_BITS);
    table
}

fn make_non_native_bgr_view_table() -> Table {
    let mut table = make_view_table();
    enable_range(&mut table, VIEW_CLASS_32_BITS_NO_BGR);
    table
}

fn make_native_bgr_copy_table() -> Table {
    let mut table = make_copy_table();
    enable_range(&mut table, VIEW_CLASS_32_BITS);
    table
}

fn make_non_native_bgr_copy_table() -> Table {
    let mut table = make_copy_table();
    enable_range(&mut table, VIEW_CLASS_32_BITS);
    table
}

use std::sync::LazyLock;

static BGR_VIEW_TABLE: LazyLock<Table> = LazyLock::new(make_native_bgr_view_table);
static NO_BGR_VIEW_TABLE: LazyLock<Table> = LazyLock::new(make_non_native_bgr_view_table);
static BGR_COPY_TABLE: LazyLock<Table> = LazyLock::new(make_native_bgr_copy_table);
static NO_BGR_COPY_TABLE: LazyLock<Table> = LazyLock::new(make_non_native_bgr_copy_table);

/// Returns true if `format_a` and `format_b` are view-compatible.
pub fn is_view_compatible(
    format_a: PixelFormat,
    format_b: PixelFormat,
    broken_views: bool,
    native_bgr: bool,
) -> bool {
    if format_a == format_b {
        return true;
    }
    if broken_views {
        // If format views are broken, only accept identical formats.
        return false;
    }
    let table = if native_bgr {
        &*BGR_VIEW_TABLE
    } else {
        &*NO_BGR_VIEW_TABLE
    };
    is_supported(table, format_a, format_b)
}

/// Returns true if `format_a` and `format_b` are copy-compatible.
pub fn is_copy_compatible(format_a: PixelFormat, format_b: PixelFormat, native_bgr: bool) -> bool {
    if format_a == format_b {
        return true;
    }
    let table = if native_bgr {
        &*BGR_COPY_TABLE
    } else {
        &*NO_BGR_COPY_TABLE
    };
    is_supported(table, format_a, format_b)
}
