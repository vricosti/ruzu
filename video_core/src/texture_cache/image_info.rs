// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/image_info.h and image_info.cpp
//!
//! Contains `ImageInfo`, which describes the properties of a GPU image (texture
//! or render target).  Multiple constructors map from different Tegra register
//! configurations (TIC entry, render target config, depth/stencil config,
//! Fermi2D surface, DMA operand).

use super::types::*;

// ── PixelFormat placeholder ────────────────────────────────────────────
// Upstream lives in video_core/surface.h.  Until that module is ported we
// carry a minimal stand-in so the struct compiles.  Replace with the real
// type once video_core::surface is available.
pub use super::format_lookup_table::PixelFormat;

// ── Constants (from image_info.cpp) ────────────────────────────────────

pub const RESCALE_HEIGHT_THRESHOLD: u32 = 288;
pub const DOWNSCALE_HEIGHT_THRESHOLD: u32 = 512;

// ── ImageInfo ──────────────────────────────────────────────────────────

/// Describes an image's format, dimensions, tiling, and resource layout.
///
/// Port of `VideoCommon::ImageInfo`.
#[derive(Debug, Clone)]
pub struct ImageInfo {
    pub format: PixelFormat,
    pub image_type: ImageType,
    pub resources: SubresourceExtent,
    pub size: Extent3D,
    /// Block-linear tiling parameters (width/height/depth log2).
    /// When the image is pitch-linear, `pitch` is used instead (overlapping
    /// via the C++ anonymous union).  We model this with an enum.
    pub tiling: TilingMode,
    pub layer_stride: u32,
    pub maybe_unaligned_layer_stride: u32,
    pub num_samples: u32,
    pub tile_width_spacing: u32,
    pub rescaleable: bool,
    pub downscaleable: bool,
    pub forced_flushed: bool,
    pub dma_downloaded: bool,
    pub is_sparse: bool,
}

/// Discriminated union replacing the C++ anonymous `union { Extent3D block; u32 pitch; }`.
#[derive(Debug, Clone, Copy)]
pub enum TilingMode {
    BlockLinear(Extent3D),
    PitchLinear(u32),
}

impl Default for TilingMode {
    fn default() -> Self {
        TilingMode::BlockLinear(Extent3D {
            width: 0,
            height: 0,
            depth: 0,
        })
    }
}

impl Default for ImageInfo {
    fn default() -> Self {
        Self {
            format: PixelFormat::Invalid,
            image_type: ImageType::E1D,
            resources: SubresourceExtent::default(),
            size: Extent3D {
                width: 1,
                height: 1,
                depth: 1,
            },
            tiling: TilingMode::default(),
            layer_stride: 0,
            maybe_unaligned_layer_stride: 0,
            num_samples: 1,
            tile_width_spacing: 0,
            rescaleable: false,
            downscaleable: false,
            forced_flushed: false,
            dma_downloaded: false,
            is_sparse: false,
        }
    }
}

impl ImageInfo {
    /// Helper to get the block extent (only valid for block-linear images).
    pub fn block(&self) -> Extent3D {
        match self.tiling {
            TilingMode::BlockLinear(b) => b,
            TilingMode::PitchLinear(_) => Extent3D {
                width: 0,
                height: 0,
                depth: 0,
            },
        }
    }

    /// Helper to get the pitch (only valid for pitch-linear images).
    pub fn pitch(&self) -> u32 {
        match self.tiling {
            TilingMode::PitchLinear(p) => p,
            TilingMode::BlockLinear(_) => 0,
        }
    }

    /// Construct from a TIC entry.
    ///
    /// Port of `ImageInfo::ImageInfo(const TICEntry& config)`.
    ///
    /// Full implementation requires TICEntry, PixelFormatFromTextureInfo,
    /// NumSamples*, CalculateLayerStride/Size helpers — these types are not yet
    /// ported from Tegra::Texture.  Returns a default `ImageInfo` and logs a
    /// warning until the upstream types are available.
    pub fn from_tic_entry(_config: &()) -> Self {
        log::warn!("ImageInfo::from_tic_entry: TICEntry not yet ported — returning default");
        Self::default()
    }

    /// Construct from a render target config.
    ///
    /// Port of `ImageInfo::ImageInfo(const RenderTargetConfig& ct, MsaaMode)`.
    ///
    /// Requires Maxwell3D register types not yet ported.
    pub fn from_render_target_config(_ct: &(), _msaa_mode: u32) -> Self {
        log::warn!(
            "ImageInfo::from_render_target_config: Maxwell3D regs not yet ported — returning default"
        );
        Self::default()
    }

    /// Construct from depth/stencil config.
    ///
    /// Port of `ImageInfo::ImageInfo(const Zeta&, const ZetaSize&, MsaaMode)`.
    ///
    /// Requires Maxwell3D register types not yet ported.
    pub fn from_zeta(_zt: &(), _zt_size: &(), _msaa_mode: u32) -> Self {
        log::warn!("ImageInfo::from_zeta: Maxwell3D regs not yet ported — returning default");
        Self::default()
    }

    /// Construct from a Fermi2D surface.
    ///
    /// Port of `ImageInfo::ImageInfo(const Fermi2D::Surface& config)`.
    ///
    /// Requires Fermi2D engine types not yet ported.
    pub fn from_fermi2d_surface(_config: &()) -> Self {
        log::warn!("ImageInfo::from_fermi2d_surface: Fermi2D not yet ported — returning default");
        Self::default()
    }

    /// Construct from a DMA image operand.
    ///
    /// Port of `ImageInfo::ImageInfo(const DMA::ImageOperand& config)`.
    ///
    /// Requires DMA engine types not yet ported.
    pub fn from_dma_operand(_config: &()) -> Self {
        log::warn!("ImageInfo::from_dma_operand: DMA engine not yet ported — returning default");
        Self::default()
    }
}

/// Map a byte-per-pixel value to a PixelFormat suitable for raw DMA copies.
///
/// Port of the file-static `ByteSizeToFormat` in image_info.cpp.
pub fn byte_size_to_format(bytes_per_pixel: u32) -> PixelFormat {
    match bytes_per_pixel {
        1 => PixelFormat::R8Uint,
        2 => PixelFormat::R8G8Uint,
        4 => PixelFormat::A8B8G8R8Uint,
        8 => PixelFormat::R16G16B16A16Uint,
        16 => PixelFormat::R32G32B32A32Uint,
        _ => {
            log::error!("ByteSizeToFormat: unimplemented bpp={}", bytes_per_pixel);
            PixelFormat::Invalid
        }
    }
}
