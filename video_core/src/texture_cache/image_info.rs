// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/image_info.h and image_info.cpp
//!
//! Contains `ImageInfo`, which describes the properties of a GPU image (texture
//! or render target).  Multiple constructors map from different Tegra register
//! configurations (TIC entry, render target config, depth/stencil config,
//! Fermi2D surface, DMA operand).

use super::types::*;
use crate::surface::{self, SurfaceType};

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
    /// Port of upstream `ImageInfo::ImageInfo(const TICEntry& config)`
    /// (image_info.cpp:28-125). Decodes format, tiling, MSAA, type and
    /// per-dimension sizes off `TicEntry`'s bitfield accessors.
    ///
    /// `forced_flushed` / `dma_downloaded` need
    /// `Settings::values.use_reactive_flushing` from common::settings, which
    /// is not wired here yet, so default to the pre-flushing branch.
    pub fn from_tic_entry(config: &crate::textures::texture::TicEntry) -> Self {
        // `format_lookup_table::{ComponentType,TextureFormat}` are the placeholder
        // enums the lookup table is keyed on. They're a strict subset of the
        // full `textures::texture::{ComponentType,TextureFormat}`, so we decode
        // straight from the raw TIC bit fields.
        use super::format_lookup_table::{ComponentType, TextureFormat};
        use crate::textures::texture::TextureType;

        let mut info = Self::default();

        // PixelFormat decode — malformed TIC bits can return None on
        // `from_raw`. Fall back to a sentinel + Snorm so the lookup table
        // folds the entry to `PixelFormat::Invalid` (hash miss). The outer
        // `create_image_view` then knows to skip this view.
        let format =
            TextureFormat::from_raw(config.format()).unwrap_or(TextureFormat::R32G32B32A32);
        let r_type = ComponentType::from_raw(config.r_type()).unwrap_or(ComponentType::Snorm);
        let g_type = ComponentType::from_raw(config.g_type()).unwrap_or(ComponentType::Snorm);
        let b_type = ComponentType::from_raw(config.b_type()).unwrap_or(ComponentType::Snorm);
        let a_type = ComponentType::from_raw(config.a_type()).unwrap_or(ComponentType::Snorm);
        let is_srgb = config.srgb_conversion() != 0;
        info.format = super::format_lookup_table::pixel_format_from_texture_info(
            format, r_type, g_type, b_type, a_type, is_srgb,
        );

        info.num_samples = super::samples_helper::num_samples(
            super::samples_helper::MsaaMode::from_raw(config.msaa_mode())
                .unwrap_or(super::samples_helper::MsaaMode::Msaa1x1),
        ) as u32;
        info.resources.levels = (config.max_mip_level() + 1) as i32;

        // Tiling: pitch- or block-linear branch.
        if config.is_pitch_linear() {
            info.tiling = TilingMode::PitchLinear(config.pitch());
        } else if config.is_block_linear() {
            info.tiling = TilingMode::BlockLinear(Extent3D {
                width: config.block_width(),
                height: config.block_height(),
                depth: config.block_depth(),
            });
        }

        info.is_sparse = config.is_sparse() != 0;
        info.tile_width_spacing = config.tile_width_spacing();

        // TextureType → ImageType + size.
        let texture_type =
            TextureType::from_raw(config.texture_type()).unwrap_or(TextureType::Texture2D);
        match texture_type {
            TextureType::Texture1D => {
                info.image_type = ImageType::E1D;
                info.size.width = config.width();
                info.resources.layers = 1;
            }
            TextureType::Texture1DArray => {
                info.image_type = ImageType::E1D;
                info.size.width = config.width();
                info.resources.layers = config.depth() as i32;
            }
            TextureType::Texture2D | TextureType::Texture2DNoMipmap => {
                info.image_type = if config.is_pitch_linear() {
                    ImageType::Linear
                } else {
                    ImageType::E2D
                };
                info.size.width = config.width();
                info.size.height = config.height();
                info.resources.layers = config.base_layer() as i32 + 1;
                info.rescaleable = !config.is_pitch_linear();
            }
            TextureType::Texture2DArray => {
                info.image_type = ImageType::E2D;
                info.size.width = config.width();
                info.size.height = config.height();
                info.resources.layers = (config.base_layer() + config.depth()) as i32;
                info.rescaleable = true;
            }
            TextureType::TextureCubemap => {
                info.image_type = ImageType::E2D;
                info.size.width = config.width();
                info.size.height = config.height();
                info.resources.layers = config.base_layer() as i32 + 6;
            }
            TextureType::TextureCubeArray => {
                info.image_type = ImageType::E2D;
                info.size.width = config.width();
                info.size.height = config.height();
                info.resources.layers = (config.base_layer() + config.depth() * 6) as i32;
            }
            TextureType::Texture3D => {
                info.image_type = ImageType::E3D;
                info.size.width = config.width();
                info.size.height = config.height();
                info.size.depth = config.depth();
                info.resources.layers = 1;
            }
            TextureType::Texture1DBuffer => {
                info.image_type = ImageType::Buffer;
                info.size.width = config.width();
                info.resources.layers = 1;
            }
        }

        if info.num_samples > 1 {
            let msaa_mode = super::samples_helper::MsaaMode::from_raw(config.msaa_mode())
                .unwrap_or(super::samples_helper::MsaaMode::Msaa1x1);
            info.size.width *= super::samples_helper::num_samples_x(msaa_mode) as u32;
            info.size.height *= super::samples_helper::num_samples_y(msaa_mode) as u32;
        }

        if info.image_type != ImageType::Linear {
            info.layer_stride = super::util::calculate_layer_stride(&info);
            info.maybe_unaligned_layer_stride = super::util::calculate_layer_size(&info);
            let block = info.block();
            info.rescaleable &= block.depth == 0 && info.resources.levels == 1;
            info.rescaleable &= info.size.height > RESCALE_HEIGHT_THRESHOLD
                || surface::get_format_type(info.format) != SurfaceType::ColorTexture;
            info.downscaleable = info.size.height > DOWNSCALE_HEIGHT_THRESHOLD;
        }

        info
    }

    /// Construct from a render target config.
    ///
    /// Port of `ImageInfo::ImageInfo(const RenderTargetConfig& ct, MsaaMode)`.
    pub fn from_render_target_info(
        config: &crate::engines::maxwell_3d::RenderTargetInfo,
        msaa_mode: u32,
    ) -> Self {
        let format = crate::surface::pixel_format_from_render_target_format(config.format);
        let is_pitch_linear = (config.tile_mode & (1 << 12)) != 0;
        let dim_control_define_depth_size = (config.tile_mode & (1 << 16)) != 0;
        let mut info = Self {
            format,
            image_type: ImageType::E2D,
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: config.width,
                height: config.height,
                depth: 1,
            },
            tiling: TilingMode::default(),
            layer_stride: 0,
            maybe_unaligned_layer_stride: 0,
            num_samples: super::samples_helper::num_samples(
                super::samples_helper::MsaaMode::from_raw(msaa_mode)
                    .unwrap_or(super::samples_helper::MsaaMode::Msaa1x1),
            ) as u32,
            tile_width_spacing: 0,
            rescaleable: false,
            downscaleable: false,
            forced_flushed: is_pitch_linear,
            dma_downloaded: is_pitch_linear,
            is_sparse: false,
        };

        if is_pitch_linear {
            let pitch = config.width;
            info.image_type = ImageType::Linear;
            info.tiling = TilingMode::PitchLinear(pitch);
            info.size.width = pitch / crate::surface::bytes_per_block(format);
            return info;
        }

        info.layer_stride = config.array_pitch.saturating_mul(4);
        info.maybe_unaligned_layer_stride = info.layer_stride;
        info.tiling = TilingMode::BlockLinear(Extent3D {
            width: config.tile_mode & 0xF,
            height: (config.tile_mode >> 4) & 0xF,
            depth: (config.tile_mode >> 8) & 0xF,
        });
        if dim_control_define_depth_size {
            info.image_type = ImageType::E3D;
            info.size.depth = config.depth & 0xFFFF;
        } else {
            info.image_type = ImageType::E2D;
            info.resources.layers = (config.depth & 0xFFFF).max(1) as i32;
            info.rescaleable =
                info.block().depth == 0 && info.size.height > RESCALE_HEIGHT_THRESHOLD;
            info.downscaleable = info.size.height > DOWNSCALE_HEIGHT_THRESHOLD;
        }

        info
    }

    /// Construct from depth/stencil config.
    ///
    /// Port of `ImageInfo::ImageInfo(const Zeta&, const ZetaSize&, MsaaMode)`.
    pub fn from_zeta_info(config: &crate::engines::maxwell_3d::ZetaInfo, msaa_mode: u32) -> Self {
        let format = crate::surface::pixel_format_from_depth_format(config.format);
        let is_pitch_linear = (config.tile_mode & (1 << 12)) != 0;
        let dim_control_define_depth_size = (config.tile_mode & (1 << 16)) != 0;
        let mut info = Self {
            format,
            image_type: ImageType::E2D,
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: config.width,
                height: config.height,
                depth: 1,
            },
            tiling: TilingMode::default(),
            layer_stride: config.array_pitch.saturating_mul(4),
            maybe_unaligned_layer_stride: config.array_pitch.saturating_mul(4),
            num_samples: super::samples_helper::num_samples(
                super::samples_helper::MsaaMode::from_raw(msaa_mode)
                    .unwrap_or(super::samples_helper::MsaaMode::Msaa1x1),
            ) as u32,
            tile_width_spacing: 0,
            rescaleable: false,
            downscaleable: false,
            forced_flushed: is_pitch_linear,
            dma_downloaded: is_pitch_linear,
            is_sparse: false,
        };

        if is_pitch_linear {
            info.image_type = ImageType::Linear;
            info.tiling =
                TilingMode::PitchLinear(config.width * crate::surface::bytes_per_block(format));
            return info;
        }

        info.tiling = TilingMode::BlockLinear(Extent3D {
            width: config.tile_mode & 0xF,
            height: (config.tile_mode >> 4) & 0xF,
            depth: (config.tile_mode >> 8) & 0xF,
        });
        if dim_control_define_depth_size {
            info.image_type = ImageType::E3D;
            info.size.depth = config.depth & 0xFFFF;
        } else {
            info.image_type = ImageType::E2D;
            let array_size_is_one = ((config.depth >> 16) & 1) != 0;
            info.resources.layers = if array_size_is_one {
                1
            } else {
                (config.depth & 0xFFFF).max(1) as i32
            };
            info.rescaleable = info.block().depth == 0;
            info.downscaleable = info.size.height > DOWNSCALE_HEIGHT_THRESHOLD;
        }

        info
    }

    /// Compatibility stub for older Rust call sites.
    pub fn from_zeta(_zt: &(), _zt_size: &(), _msaa_mode: u32) -> Self {
        log::warn!("ImageInfo::from_zeta: Maxwell3D regs not yet ported — returning default");
        Self::default()
    }

    /// Construct from a Fermi2D surface.
    ///
    /// Port of `ImageInfo::ImageInfo(const Fermi2D::Surface& config)`.
    pub fn from_fermi2d_surface(config: &crate::engines::fermi_2d::Surface) -> Self {
        if config.layer != 0 {
            log::warn!("ImageInfo::from_fermi2d_surface: surface layer is not zero");
        }
        let format = crate::surface::pixel_format_from_render_target_format(config.format as u32);
        let forced_flushed = config.linear == crate::engines::fermi_2d::MemoryLayout::Pitch;
        let mut info = Self {
            format,
            image_type: ImageType::E2D,
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: config.width,
                height: config.height,
                depth: 1,
            },
            tiling: TilingMode::default(),
            layer_stride: 0,
            maybe_unaligned_layer_stride: 0,
            num_samples: 1,
            tile_width_spacing: 0,
            rescaleable: false,
            downscaleable: false,
            forced_flushed,
            dma_downloaded: forced_flushed,
            is_sparse: false,
        };

        if config.linear == crate::engines::fermi_2d::MemoryLayout::Pitch {
            info.image_type = ImageType::Linear;
            info.size.width = config.pitch / crate::surface::bytes_per_block(format);
            info.tiling = TilingMode::PitchLinear(config.pitch);
        } else {
            info.image_type = if config.block_depth() > 0 {
                ImageType::E3D
            } else {
                ImageType::E2D
            };
            info.tiling = TilingMode::BlockLinear(Extent3D {
                width: config.block_width(),
                height: config.block_height(),
                depth: config.block_depth(),
            });
        }

        info
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engines::maxwell_3d::{RenderTargetInfo, ZetaInfo};
    use crate::surface::PixelFormat as SurfacePixelFormat;
    use crate::textures::texture::{ComponentType, TextureFormat, TextureType, TicEntry};

    #[test]
    fn render_target_info_decodes_block_linear_layout() {
        let info = ImageInfo::from_render_target_info(
            &RenderTargetInfo {
                address: 0x5000_0000,
                width: 1920,
                height: 1080,
                format: 0xD5,
                tile_mode: 2 | (3 << 4) | (1 << 8),
                depth: 4,
                array_pitch: 0x200,
                base_layer: 0,
            },
            0,
        );

        assert_eq!(info.format, SurfacePixelFormat::A8B8G8R8Unorm);
        assert_eq!(info.image_type, ImageType::E2D);
        assert_eq!(info.size.width, 1920);
        assert_eq!(info.size.height, 1080);
        assert_eq!(info.resources.layers, 4);
        assert_eq!(info.layer_stride, 0x800);
        assert_eq!(info.block().width, 2);
        assert_eq!(info.block().height, 3);
        assert_eq!(info.block().depth, 1);
    }

    #[test]
    fn render_target_info_decodes_pitch_linear_layout() {
        let info = ImageInfo::from_render_target_info(
            &RenderTargetInfo {
                address: 0x5000_0000,
                width: 256,
                height: 16,
                format: 0xD5,
                tile_mode: 1 << 12,
                depth: 1,
                array_pitch: 0,
                base_layer: 0,
            },
            0,
        );

        assert_eq!(info.image_type, ImageType::Linear);
        assert_eq!(info.pitch(), 256);
        assert_eq!(info.size.width, 64);
        assert_eq!(info.size.height, 16);
        assert!(info.forced_flushed);
        assert!(info.dma_downloaded);
    }

    #[test]
    fn render_target_and_zeta_info_use_msaa_mode() {
        let render_target = RenderTargetInfo {
            address: 0x5000_0000,
            width: 1280,
            height: 720,
            format: 0xD5,
            tile_mode: 2 | (1 << 4),
            depth: 1,
            array_pitch: 0x200,
            base_layer: 0,
        };
        let zeta = ZetaInfo {
            enabled: true,
            address: 0x6000_0000,
            format: 0xA,
            tile_mode: 2 | (1 << 4),
            array_pitch: 0x200,
            width: 1280,
            height: 720,
            depth: 1,
        };

        assert_eq!(
            ImageInfo::from_render_target_info(&render_target, 3).num_samples,
            8
        );
        assert_eq!(ImageInfo::from_zeta_info(&zeta, 3).num_samples, 8);
    }

    #[test]
    fn tic_entry_decodes_mips_msaa_and_layer_stride() {
        let word0 = (TextureFormat::A8B8G8R8 as u32)
            | ((ComponentType::Unorm as u32) << 7)
            | ((ComponentType::Unorm as u32) << 10)
            | ((ComponentType::Unorm as u32) << 13)
            | ((ComponentType::Unorm as u32) << 16);
        let word3 = (2 << 10) | (3 << 28);
        let word4 = 63 | ((TextureType::Texture2D as u32) << 23);
        let word5 = 31 | (1 << 31);
        let word7 = 2 << 8;
        let word2 = 3 << 21;
        let tic = TicEntry {
            raw: [
                word0 as u64,
                ((word3 as u64) << 32) | word2 as u64,
                ((word5 as u64) << 32) | word4 as u64,
                (word7 as u64) << 32,
            ],
        };

        let info = ImageInfo::from_tic_entry(&tic);

        assert_eq!(info.resources.levels, 4);
        assert_eq!(info.num_samples, 4);
        assert_eq!(info.tile_width_spacing, 2);
        assert_eq!(info.size.width, 128);
        assert_eq!(info.size.height, 64);
        assert_ne!(info.layer_stride, 0);
        assert_ne!(info.maybe_unaligned_layer_stride, 0);
    }
}
