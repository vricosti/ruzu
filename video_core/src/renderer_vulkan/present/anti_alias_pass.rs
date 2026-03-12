// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/anti_alias_pass.h`.
//!
//! Abstract anti-aliasing pass interface and no-op implementation.

use ash::vk;

// ---------------------------------------------------------------------------
// AntiAliasPass trait
// ---------------------------------------------------------------------------

/// Port of `AntiAliasPass` abstract class.
///
/// Interface for anti-aliasing passes that operate on a presentable image
/// in-place (swapping the image/view pointers if needed).
pub trait AntiAliasPass {
    /// Port of `AntiAliasPass::Draw`.
    fn draw(
        &mut self,
        _image_index: usize,
        _inout_image: &mut vk::Image,
        _inout_image_view: &mut vk::ImageView,
    );
}

// ---------------------------------------------------------------------------
// NoAA
// ---------------------------------------------------------------------------

/// Port of `NoAA` class — a no-op anti-aliasing pass.
pub struct NoAa;

impl AntiAliasPass for NoAa {
    fn draw(
        &mut self,
        _image_index: usize,
        _inout_image: &mut vk::Image,
        _inout_image_view: &mut vk::ImageView,
    ) {
        // No-op, matching upstream.
    }
}
