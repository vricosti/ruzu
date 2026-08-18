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
        let mut value = self.depth_buffer_id.index as u64;
        for id in &self.color_buffer_ids {
            value ^= id.index as u64;
        }
        value ^= u64::from_ne_bytes(self.draw_buffers);
        // SAFETY: upstream bit-casts the same two-u32 `Extent2D` to `u64`.
        // This transmute is also a compile-time assertion that both are 8 bytes.
        value ^= u64::from_ne_bytes(unsafe { std::mem::transmute::<Extent2D, [u8; 8]>(self.size) });
        state.write_u64(value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::hash::BuildIdentityHasher;
    use std::hash::BuildHasher;

    fn hash_value(render_targets: &RenderTargets) -> u64 {
        let mut hasher = BuildIdentityHasher.build_hasher();
        render_targets.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn hash_matches_upstream_xor_and_ignores_rescale_flag() {
        let mut render_targets = RenderTargets {
            color_buffer_ids: std::array::from_fn(|index| ImageViewId {
                index: (index as u32 + 1) * 0x11,
            }),
            depth_buffer_id: ImageViewId { index: 0x1234_5678 },
            draw_buffers: [0, 1, 2, 3, 4, 5, 6, 7],
            size: Extent2D {
                width: 1280,
                height: 720,
            },
            is_rescaled: false,
        };

        assert_eq!(hash_value(&render_targets), 0x0706_07d4_1136_52f0);
        render_targets.is_rescaled = true;
        assert_eq!(hash_value(&render_targets), 0x0706_07d4_1136_52f0);
    }
}
