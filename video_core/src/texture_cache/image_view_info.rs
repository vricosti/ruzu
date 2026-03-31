// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/image_view_info.h and image_view_info.cpp
//!
//! `ImageViewInfo` describes the properties used to look up or create an image
//! view (sub-resource of a texture).

use super::format_lookup_table::PixelFormat;
use super::types::*;

// ── SwizzleSource placeholder ──────────────────────────────────────────
// Upstream: Tegra::Texture::SwizzleSource in textures/texture.h
// Minimal stand-in; replace with real type once available.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SwizzleSource {
    Zero = 0,
    R = 2,
    G = 3,
    B = 4,
    A = 5,
    OneInt = 6,
    OneFloat = 7,
}

/// Sentinel value used by render-target views (no real swizzle).
const RENDER_TARGET_SWIZZLE: u8 = u8::MAX;

// ── ImageViewInfo ──────────────────────────────────────────────────────

/// Properties used to determine an image view.
///
/// Port of `VideoCommon::ImageViewInfo`.
///
/// The struct has a trivial object representation in C++ (checked via
/// `static_assert(std::has_unique_object_representations_v<ImageViewInfo>)`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct ImageViewInfo {
    pub view_type: ImageViewType,
    pub format: PixelFormat,
    pub range: SubresourceRange,
    pub x_source: u8,
    pub y_source: u8,
    pub z_source: u8,
    pub w_source: u8,
}

impl Default for ImageViewInfo {
    fn default() -> Self {
        Self {
            view_type: ImageViewType::E1D,
            format: PixelFormat::Invalid,
            range: SubresourceRange::default(),
            x_source: SwizzleSource::R as u8,
            y_source: SwizzleSource::G as u8,
            z_source: SwizzleSource::B as u8,
            w_source: SwizzleSource::A as u8,
        }
    }
}

impl ImageViewInfo {
    /// Construct from a TIC entry and base layer.
    ///
    /// Port of `ImageViewInfo::ImageViewInfo(const TICEntry&, s32 base_layer)`.
    ///
    /// Full implementation requires TICEntry from Tegra::Texture, which is not
    /// yet ported.  Returns a default `ImageViewInfo` and logs a warning.
    pub fn from_tic_entry(_config: &(), _base_layer: i32) -> Self {
        log::warn!("ImageViewInfo::from_tic_entry: TICEntry not yet ported — returning default");
        Self::default()
    }

    /// Construct for a render target (no real swizzle).
    ///
    /// Port of `ImageViewInfo::ImageViewInfo(ImageViewType, PixelFormat, SubresourceRange)`.
    pub fn for_render_target(
        view_type: ImageViewType,
        format: PixelFormat,
        range: SubresourceRange,
    ) -> Self {
        Self {
            view_type,
            format,
            range,
            x_source: RENDER_TARGET_SWIZZLE,
            y_source: RENDER_TARGET_SWIZZLE,
            z_source: RENDER_TARGET_SWIZZLE,
            w_source: RENDER_TARGET_SWIZZLE,
        }
    }

    /// Returns the swizzle sources as an array.
    pub fn swizzle(&self) -> [SwizzleSource; 4] {
        [
            // Safety: values are validated on construction in the full port.
            unsafe { std::mem::transmute(self.x_source) },
            unsafe { std::mem::transmute(self.y_source) },
            unsafe { std::mem::transmute(self.z_source) },
            unsafe { std::mem::transmute(self.w_source) },
        ]
    }

    /// Whether this view was created as a render target (all swizzle fields
    /// set to the sentinel value).
    ///
    /// Port of `ImageViewInfo::IsRenderTarget`.
    pub fn is_render_target(&self) -> bool {
        self.x_source == RENDER_TARGET_SWIZZLE
            && self.y_source == RENDER_TARGET_SWIZZLE
            && self.z_source == RENDER_TARGET_SWIZZLE
            && self.w_source == RENDER_TARGET_SWIZZLE
    }
}
