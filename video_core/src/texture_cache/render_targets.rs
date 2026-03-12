// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/render_targets.h
//!
//! `RenderTargets` — framebuffer properties used to look up a cached
//! framebuffer.

use std::hash::{Hash, Hasher};

use super::types::*;

// ── RenderTargets ──────────────────────────────────────────────────────

/// Framebuffer properties used to look up a cached framebuffer.
///
/// Port of `VideoCommon::RenderTargets`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RenderTargets {
    pub color_buffer_ids: [ImageViewId; NUM_RT],
    pub depth_buffer_id: ImageViewId,
    pub draw_buffers: [u8; NUM_RT],
    pub size: Extent2D,
    pub is_rescaled: bool,
}

impl Default for RenderTargets {
    fn default() -> Self {
        Self {
            color_buffer_ids: [ImageViewId::default(); NUM_RT],
            depth_buffer_id: ImageViewId::default(),
            draw_buffers: [0; NUM_RT],
            size: Extent2D::default(),
            is_rescaled: false,
        }
    }
}

impl RenderTargets {
    /// Whether any of the render targets reference one of the given view ids.
    ///
    /// Port of `RenderTargets::Contains`.
    pub fn contains(&self, elements: &[ImageViewId]) -> bool {
        let check = |item: ImageViewId| elements.contains(&item);
        self.color_buffer_ids.iter().any(|&id| check(id)) || check(self.depth_buffer_id)
    }
}

impl Hash for RenderTargets {
    /// Port of the `std::hash<RenderTargets>` specialisation.
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.depth_buffer_id.hash(state);
        for id in &self.color_buffer_ids {
            id.hash(state);
        }
        self.draw_buffers.hash(state);
        self.size.hash(state);
    }
}
