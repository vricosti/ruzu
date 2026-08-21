// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/compatible_formats.h and video_core/compatible_formats.cpp
//!
//! Format compatibility tables for view and copy operations.
//! Compatibility table taken from Table 3.X.2 in:
//! https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_view.txt
//! and Table 4.X.1 in:
//! https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_copy_image.txt

use crate::surface::{PixelFormat, MAX_PIXEL_FORMAT};

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

const VIEW_CLASS_RGTC1_RED: &[PixelFormat] = &[PixelFormat::Bc4Unorm, PixelFormat::Bc4Snorm];
const VIEW_CLASS_RGTC2_RG: &[PixelFormat] = &[PixelFormat::Bc5Unorm, PixelFormat::Bc5Snorm];
const VIEW_CLASS_BPTC_UNORM: &[PixelFormat] = &[PixelFormat::Bc7Unorm, PixelFormat::Bc7Srgb];
const VIEW_CLASS_BPTC_FLOAT: &[PixelFormat] = &[PixelFormat::Bc6hSfloat, PixelFormat::Bc6hUfloat];

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
    PixelFormat::Bc2Unorm,
    PixelFormat::Bc2Srgb,
    PixelFormat::Bc3Unorm,
    PixelFormat::Bc3Srgb,
    PixelFormat::Bc5Unorm,
    PixelFormat::Bc5Snorm,
    PixelFormat::Bc7Unorm,
    PixelFormat::Bc7Srgb,
    PixelFormat::Bc6hSfloat,
    PixelFormat::Bc6hUfloat,
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
    PixelFormat::Bc1RgbaUnorm,
    PixelFormat::Bc1RgbaSrgb,
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
