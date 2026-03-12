// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/formatter.h and formatter.cpp
//!
//! `Display` / debug-format implementations for texture-cache types, plus
//! `name()` helpers for `ImageBase`, `ImageViewBase`, and `RenderTargets`.

use std::fmt;

use super::format_lookup_table::PixelFormat;
use super::image_base::{GPUVAddr, ImageBase};
use super::image_view_base::ImageViewBase;
use super::render_targets::RenderTargets;
use super::samples_helper::samples_log2;
use super::types::*;

// ── Display for PixelFormat ────────────────────────────────────────────

impl fmt::Display for PixelFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            PixelFormat::A8B8G8R8Unorm => "A8B8G8R8_UNORM",
            PixelFormat::A8B8G8R8Snorm => "A8B8G8R8_SNORM",
            PixelFormat::A8B8G8R8Sint => "A8B8G8R8_SINT",
            PixelFormat::A8B8G8R8Uint => "A8B8G8R8_UINT",
            PixelFormat::R5G6B5Unorm => "R5G6B5_UNORM",
            PixelFormat::B5G6R5Unorm => "B5G6R5_UNORM",
            PixelFormat::A1R5G5B5Unorm => "A1R5G5B5_UNORM",
            PixelFormat::A2B10G10R10Unorm => "A2B10G10R10_UNORM",
            PixelFormat::A2B10G10R10Uint => "A2B10G10R10_UINT",
            PixelFormat::A2R10G10B10Unorm => "A2R10G10B10_UNORM",
            PixelFormat::A1B5G5R5Unorm => "A1B5G5R5_UNORM",
            PixelFormat::A5B5G5R1Unorm => "A5B5G5R1_UNORM",
            PixelFormat::R8Unorm => "R8_UNORM",
            PixelFormat::R8Snorm => "R8_SNORM",
            PixelFormat::R8Sint => "R8_SINT",
            PixelFormat::R8Uint => "R8_UINT",
            PixelFormat::R16G16B16A16Float => "R16G16B16A16_FLOAT",
            PixelFormat::R16G16B16A16Unorm => "R16G16B16A16_UNORM",
            PixelFormat::R16G16B16A16Snorm => "R16G16B16A16_SNORM",
            PixelFormat::R16G16B16A16Sint => "R16G16B16A16_SINT",
            PixelFormat::R16G16B16A16Uint => "R16G16B16A16_UINT",
            PixelFormat::B10G11R11Float => "B10G11R11_FLOAT",
            PixelFormat::R32G32B32A32Uint => "R32G32B32A32_UINT",
            PixelFormat::Bc1RgbaUnorm => "BC1_RGBA_UNORM",
            PixelFormat::Bc2Unorm => "BC2_UNORM",
            PixelFormat::Bc3Unorm => "BC3_UNORM",
            PixelFormat::Bc4Unorm => "BC4_UNORM",
            PixelFormat::Bc4Snorm => "BC4_SNORM",
            PixelFormat::Bc5Unorm => "BC5_UNORM",
            PixelFormat::Bc5Snorm => "BC5_SNORM",
            PixelFormat::Bc7Unorm => "BC7_UNORM",
            PixelFormat::Bc6hUfloat => "BC6H_UFLOAT",
            PixelFormat::Bc6hSfloat => "BC6H_SFLOAT",
            PixelFormat::B8G8R8A8Unorm => "B8G8R8A8_UNORM",
            PixelFormat::R32G32B32A32Float => "R32G32B32A32_FLOAT",
            PixelFormat::R32G32B32A32Sint => "R32G32B32A32_SINT",
            PixelFormat::R32G32Float => "R32G32_FLOAT",
            PixelFormat::R32G32Sint => "R32G32_SINT",
            PixelFormat::R32Float => "R32_FLOAT",
            PixelFormat::R16Float => "R16_FLOAT",
            PixelFormat::R16Unorm => "R16_UNORM",
            PixelFormat::R16Snorm => "R16_SNORM",
            PixelFormat::R16Uint => "R16_UINT",
            PixelFormat::R16Sint => "R16_SINT",
            PixelFormat::R16G16Unorm => "R16G16_UNORM",
            PixelFormat::R16G16Float => "R16G16_FLOAT",
            PixelFormat::R16G16Uint => "R16G16_UINT",
            PixelFormat::R16G16Sint => "R16G16_SINT",
            PixelFormat::R16G16Snorm => "R16G16_SNORM",
            PixelFormat::R32G32B32Float => "R32G32B32_FLOAT",
            PixelFormat::A8B8G8R8Srgb => "A8B8G8R8_SRGB",
            PixelFormat::R8G8Unorm => "R8G8_UNORM",
            PixelFormat::R8G8Snorm => "R8G8_SNORM",
            PixelFormat::R8G8Sint => "R8G8_SINT",
            PixelFormat::R8G8Uint => "R8G8_UINT",
            PixelFormat::R32G32Uint => "R32G32_UINT",
            PixelFormat::R16G16B16X16Float => "R16G16B16X16_FLOAT",
            PixelFormat::R32Uint => "R32_UINT",
            PixelFormat::R32Sint => "R32_SINT",
            PixelFormat::D32Float => "D32_FLOAT",
            PixelFormat::D16Unorm => "D16_UNORM",
            PixelFormat::X8D24Unorm => "X8_D24_UNORM",
            PixelFormat::S8Uint => "S8_UINT",
            PixelFormat::D24UnormS8Uint => "D24_UNORM_S8_UINT",
            PixelFormat::S8UintD24Unorm => "S8_UINT_D24_UNORM",
            PixelFormat::D32FloatS8Uint => "D32_FLOAT_S8_UINT",
            PixelFormat::E5B9G9R9Float => "E5B9G9R9_FLOAT",
            _ => "Invalid",
        })
    }
}

// ── Display for ImageType ──────────────────────────────────────────────

impl fmt::Display for ImageType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ImageType::E1D => "1D",
            ImageType::E2D => "2D",
            ImageType::E3D => "3D",
            ImageType::Linear => "Linear",
            ImageType::Buffer => "Buffer",
        })
    }
}

// ── Display for Extent3D ───────────────────────────────────────────────

impl fmt::Display for Extent3D {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{{}, {}, {}}}", self.width, self.height, self.depth)
    }
}

// ── Name helpers ───────────────────────────────────────────────────────

/// Human-readable name for an image.
///
/// Port of `VideoCommon::Name(const ImageBase&)`.
pub fn image_name(image: &ImageBase) -> String {
    let gpu_addr = image.gpu_addr;
    let info = &image.info;
    let mut width = info.size.width;
    let mut height = info.size.height;
    let depth = info.size.depth;
    let num_layers = info.resources.layers as u32;
    let num_levels = info.resources.levels as u32;
    let mut resource = String::new();
    if info.num_samples > 1 {
        let (sx, sy) = samples_log2(info.num_samples as i32);
        width >>= sx;
        height >>= sy;
        resource += &format!(":{}xMSAA", info.num_samples);
    }
    if num_layers > 1 {
        resource += &format!(":L{}", num_layers);
    }
    if num_levels > 1 {
        resource += &format!(":M{}", num_levels);
    }
    match info.image_type {
        ImageType::E1D => format!("Image 1D 0x{:x} {}{}", gpu_addr, width, resource),
        ImageType::E2D => {
            format!("Image 2D 0x{:x} {}x{}{}", gpu_addr, width, height, resource)
        }
        ImageType::E3D => format!(
            "Image 2D 0x{:x} {}x{}x{}{}",
            gpu_addr, width, height, depth, resource
        ),
        ImageType::Linear => format!("Image Linear 0x{:x} {}x{}", gpu_addr, width, height),
        ImageType::Buffer => format!("Buffer 0x{:x} {}", gpu_addr, width),
    }
}

/// Human-readable name for an image view.
///
/// Port of `VideoCommon::Name(const ImageViewBase&, GPUVAddr)`.
pub fn image_view_name(view: &ImageViewBase, addr: GPUVAddr) -> String {
    let w = view.size.width;
    let h = view.size.height;
    let d = view.size.depth;
    let levels = view.range.extent.levels as u32;
    let layers = view.range.extent.layers as u32;
    let level_str = if levels > 1 {
        format!(":{}", levels)
    } else {
        String::new()
    };
    match view.view_type {
        ImageViewType::E1D => format!("ImageView 1D 0x{:X} {}{}", addr, w, level_str),
        ImageViewType::E2D => {
            format!("ImageView 2D 0x{:X} {}x{}{}", addr, w, h, level_str)
        }
        ImageViewType::Cube => {
            format!("ImageView Cube 0x{:X} {}x{}{}", addr, w, h, level_str)
        }
        ImageViewType::E3D => {
            format!("ImageView 3D 0x{:X} {}x{}x{}{}", addr, w, h, d, level_str)
        }
        ImageViewType::E1DArray => {
            format!(
                "ImageView 1DArray 0x{:X} {}{}|{}",
                addr, w, level_str, layers
            )
        }
        ImageViewType::E2DArray => format!(
            "ImageView 2DArray 0x{:X} {}x{}{}|{}",
            addr, w, h, level_str, layers
        ),
        ImageViewType::CubeArray => format!(
            "ImageView CubeArray 0x{:X} {}x{}{}|{}",
            addr, w, h, level_str, layers
        ),
        ImageViewType::Rect => {
            format!("ImageView Rect 0x{:X} {}x{}{}", addr, w, h, level_str)
        }
        ImageViewType::Buffer => format!("BufferView 0x{:X} {}", addr, w),
    }
}

/// Human-readable name for a framebuffer (render target set).
///
/// Port of `VideoCommon::Name(const RenderTargets&)`.
pub fn render_targets_name(rt: &RenderTargets) -> String {
    let num_color = rt
        .color_buffer_ids
        .iter()
        .filter(|id| id.is_valid())
        .count();
    let has_depth = rt.depth_buffer_id.is_valid();
    let prefix = match (has_depth, num_color > 0) {
        (true, true) => "R",
        (true, false) => "Z",
        (false, true) => "C",
        (false, false) => "X",
    };
    let size = rt.size;
    if num_color > 0 {
        format!(
            "Framebuffer {}{} {}x{}",
            prefix, num_color, size.width, size.height
        )
    } else {
        format!("Framebuffer {} {}x{}", prefix, size.width, size.height)
    }
}
