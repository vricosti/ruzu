// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `gl_state_tracker.h` / `gl_state_tracker.cpp`.
//!
//! Tracks OpenGL state changes to avoid redundant GL calls.
//! Uses dirty flags indexed by the `Dirty` enum constants to selectively
//! re-apply state per draw call.

use gl::types::{GLenum, GLuint};

// ---------------------------------------------------------------------------
// Dirty flag indices — port of OpenGL::Dirty enum
// ---------------------------------------------------------------------------

/// Dirty flag indices matching upstream `OpenGL::Dirty::` enum.
///
/// Common entries occupy indices 0..LAST_COMMON_ENTRY, then OpenGL-specific
/// flags continue from `First`.
pub mod dirty {
    // Common entries (from VideoCommon::Dirty, shared with Vulkan)
    pub const RENDER_TARGETS: u8 = 0;
    pub const VERTEX_BUFFERS: u8 = 10;
    pub const VERTEX_BUFFER_0: u8 = 11;
    pub const RESCALE_VIEWPORTS: u8 = 43;
    pub const RESCALE_SCISSORS: u8 = 44;
    pub const DEPTH_BIAS_GLOBAL: u8 = 45;
    pub const LAST_COMMON_ENTRY: u8 = 46;

    // OpenGL-specific entries
    pub const FIRST: u8 = LAST_COMMON_ENTRY;

    pub const VERTEX_FORMATS: u8 = FIRST;
    pub const VERTEX_FORMAT_0: u8 = VERTEX_FORMATS + 1;
    pub const VERTEX_FORMAT_31: u8 = VERTEX_FORMAT_0 + 31;

    pub const VERTEX_INSTANCES: u8 = VERTEX_FORMAT_31 + 1;
    pub const VERTEX_INSTANCE_0: u8 = VERTEX_INSTANCES + 1;
    pub const VERTEX_INSTANCE_31: u8 = VERTEX_INSTANCE_0 + 31;

    pub const VIEWPORT_TRANSFORM: u8 = VERTEX_INSTANCE_31 + 1;
    pub const VIEWPORTS: u8 = VIEWPORT_TRANSFORM + 1;
    pub const VIEWPORT_0: u8 = VIEWPORTS + 1;
    pub const VIEWPORT_15: u8 = VIEWPORT_0 + 15;

    pub const SCISSORS: u8 = VIEWPORT_15 + 1;
    pub const SCISSOR_0: u8 = SCISSORS + 1;
    pub const SCISSOR_15: u8 = SCISSOR_0 + 15;

    pub const COLOR_MASK_COMMON: u8 = SCISSOR_15 + 1;
    pub const COLOR_MASKS: u8 = COLOR_MASK_COMMON + 1;
    pub const COLOR_MASK_0: u8 = COLOR_MASKS + 1;
    pub const COLOR_MASK_7: u8 = COLOR_MASK_0 + 7;

    pub const BLEND_COLOR: u8 = COLOR_MASK_7 + 1;
    pub const BLEND_INDEPENDENT_ENABLED: u8 = BLEND_COLOR + 1;
    pub const BLEND_STATES: u8 = BLEND_INDEPENDENT_ENABLED + 1;
    pub const BLEND_STATE_0: u8 = BLEND_STATES + 1;
    pub const BLEND_STATE_7: u8 = BLEND_STATE_0 + 7;

    pub const CLIP_DISTANCES: u8 = BLEND_STATE_7 + 1;

    pub const POLYGON_MODES: u8 = CLIP_DISTANCES + 1;
    pub const POLYGON_MODE_FRONT: u8 = POLYGON_MODES + 1;
    pub const POLYGON_MODE_BACK: u8 = POLYGON_MODE_FRONT + 1;

    pub const COLOR_MASK: u8 = POLYGON_MODE_BACK + 1;
    pub const FRONT_FACE: u8 = COLOR_MASK + 1;
    pub const CULL_TEST: u8 = FRONT_FACE + 1;
    pub const DEPTH_MASK: u8 = CULL_TEST + 1;
    pub const DEPTH_TEST: u8 = DEPTH_MASK + 1;
    pub const STENCIL_TEST: u8 = DEPTH_TEST + 1;
    pub const ALPHA_TEST: u8 = STENCIL_TEST + 1;
    pub const PRIMITIVE_RESTART: u8 = ALPHA_TEST + 1;
    pub const POLYGON_OFFSET: u8 = PRIMITIVE_RESTART + 1;
    pub const MULTISAMPLE_CONTROL: u8 = POLYGON_OFFSET + 1;
    pub const RASTERIZE_ENABLE: u8 = MULTISAMPLE_CONTROL + 1;
    pub const FRAMEBUFFER_SRGB: u8 = RASTERIZE_ENABLE + 1;
    pub const LOGIC_OP: u8 = FRAMEBUFFER_SRGB + 1;
    pub const FRAGMENT_CLAMP_COLOR: u8 = LOGIC_OP + 1;
    pub const POINT_SIZE: u8 = FRAGMENT_CLAMP_COLOR + 1;
    pub const LINE_WIDTH: u8 = POINT_SIZE + 1;
    pub const CLIP_CONTROL: u8 = LINE_WIDTH + 1;
    pub const DEPTH_CLAMP_ENABLED: u8 = CLIP_CONTROL + 1;

    pub const LAST: u8 = DEPTH_CLAMP_ENABLED + 1;
}

/// Backing store for dirty flags — a simple boolean array.
pub type DirtyFlags = [bool; 256];

// ---------------------------------------------------------------------------
// StateTracker
// ---------------------------------------------------------------------------

/// Tracks OpenGL state to minimize redundant API calls.
///
/// Port of `OpenGL::StateTracker` class.
///
/// Caches the currently bound index buffer, framebuffer, clip control state,
/// and Y-negate flag to skip redundant GL calls when state hasn't changed.
pub struct StateTracker {
    flags: DirtyFlags,
    default_flags: DirtyFlags,
    framebuffer: GLuint,
    index_buffer: GLuint,
    origin: GLenum,
    depth: GLenum,
    y_negate: bool,
}

impl StateTracker {
    /// Port of `StateTracker::StateTracker`.
    pub fn new() -> Self {
        let mut flags = [false; 256];
        // Start with all flags dirty
        for i in 0..(dirty::LAST as usize) {
            flags[i] = true;
        }
        Self {
            flags,
            default_flags: [false; 256],
            framebuffer: 0,
            index_buffer: 0,
            origin: gl::LOWER_LEFT,
            depth: gl::NEGATIVE_ONE_TO_ONE,
            y_negate: false,
        }
    }

    /// Port of `StateTracker::BindIndexBuffer`.
    pub fn bind_index_buffer(&mut self, new_index_buffer: GLuint) {
        if self.index_buffer == new_index_buffer {
            return;
        }
        self.index_buffer = new_index_buffer;
        unsafe {
            gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, new_index_buffer);
        }
    }

    /// Port of `StateTracker::BindFramebuffer`.
    pub fn bind_framebuffer(&mut self, new_framebuffer: GLuint) {
        if self.framebuffer == new_framebuffer {
            return;
        }
        self.framebuffer = new_framebuffer;
        unsafe {
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, self.framebuffer);
        }
    }

    /// Port of `StateTracker::ClipControl`.
    pub fn clip_control(&mut self, new_origin: GLenum, new_depth: GLenum) {
        if new_origin == self.origin && new_depth == self.depth {
            return;
        }
        self.origin = new_origin;
        self.depth = new_depth;
        unsafe {
            gl::ClipControl(self.origin, self.depth);
        }
    }

    /// Port of `StateTracker::SetYNegate`.
    ///
    /// Upstream maps Y_NEGATE to `gl_FrontMaterial.ambient.a` via `glMaterialfv`.
    /// The `gl` crate may not expose the legacy fixed-function `glMaterialfv`;
    /// if unavailable we store the flag and let the shader uniform path handle it.
    pub fn set_y_negate(&mut self, new_y_negate: bool) {
        if new_y_negate == self.y_negate {
            return;
        }
        self.y_negate = new_y_negate;
        // Legacy GL path: glMaterialfv(GL_FRONT, GL_AMBIENT, [0,0,0, y_negate ? -1 : 1])
        // Skipped here because the gl crate does not expose glMaterialfv.
        // The y_negate value is tracked and can be forwarded to shaders via uniforms.
    }

    /// Returns the current Y-negate state.
    pub fn y_negate(&self) -> bool {
        self.y_negate
    }

    /// Port of `StateTracker::NotifyScreenDrawVertexArray`.
    pub fn notify_screen_draw_vertex_array(&mut self) {
        self.flags[dirty::VERTEX_FORMATS as usize] = true;
        self.flags[(dirty::VERTEX_FORMAT_0) as usize] = true;
        self.flags[(dirty::VERTEX_FORMAT_0 + 1) as usize] = true;

        self.flags[dirty::VERTEX_BUFFERS as usize] = true;
        self.flags[dirty::VERTEX_BUFFER_0 as usize] = true;

        self.flags[dirty::VERTEX_INSTANCES as usize] = true;
        self.flags[(dirty::VERTEX_INSTANCE_0) as usize] = true;
        self.flags[(dirty::VERTEX_INSTANCE_0 + 1) as usize] = true;
    }

    /// Port of `StateTracker::NotifyPolygonModes`.
    pub fn notify_polygon_modes(&mut self) {
        self.flags[dirty::POLYGON_MODES as usize] = true;
        self.flags[dirty::POLYGON_MODE_FRONT as usize] = true;
        self.flags[dirty::POLYGON_MODE_BACK as usize] = true;
    }

    /// Port of `StateTracker::NotifyViewport0`.
    pub fn notify_viewport0(&mut self) {
        self.flags[dirty::VIEWPORTS as usize] = true;
        self.flags[dirty::VIEWPORT_0 as usize] = true;
    }

    /// Port of `StateTracker::NotifyScissor0`.
    pub fn notify_scissor0(&mut self) {
        self.flags[dirty::SCISSORS as usize] = true;
        self.flags[dirty::SCISSOR_0 as usize] = true;
    }

    /// Port of `StateTracker::NotifyColorMask`.
    pub fn notify_color_mask(&mut self, index: usize) {
        self.flags[dirty::COLOR_MASKS as usize] = true;
        self.flags[(dirty::COLOR_MASK_0 as usize) + index] = true;
    }

    /// Port of `StateTracker::NotifyBlend0`.
    pub fn notify_blend0(&mut self) {
        self.flags[dirty::BLEND_STATES as usize] = true;
        self.flags[dirty::BLEND_STATE_0 as usize] = true;
    }

    /// Port of `StateTracker::NotifyFramebuffer`.
    pub fn notify_framebuffer(&mut self) {
        self.flags[dirty::RENDER_TARGETS as usize] = true;
    }

    /// Port of `StateTracker::NotifyFrontFace`.
    pub fn notify_front_face(&mut self) {
        self.flags[dirty::FRONT_FACE as usize] = true;
    }

    /// Port of `StateTracker::NotifyCullTest`.
    pub fn notify_cull_test(&mut self) {
        self.flags[dirty::CULL_TEST as usize] = true;
    }

    /// Port of `StateTracker::NotifyDepthMask`.
    pub fn notify_depth_mask(&mut self) {
        self.flags[dirty::DEPTH_MASK as usize] = true;
    }

    /// Port of `StateTracker::NotifyDepthTest`.
    pub fn notify_depth_test(&mut self) {
        self.flags[dirty::DEPTH_TEST as usize] = true;
    }

    /// Port of `StateTracker::NotifyStencilTest`.
    pub fn notify_stencil_test(&mut self) {
        self.flags[dirty::STENCIL_TEST as usize] = true;
    }

    /// Port of `StateTracker::NotifyPolygonOffset`.
    pub fn notify_polygon_offset(&mut self) {
        self.flags[dirty::POLYGON_OFFSET as usize] = true;
    }

    /// Port of `StateTracker::NotifyRasterizeEnable`.
    pub fn notify_rasterize_enable(&mut self) {
        self.flags[dirty::RASTERIZE_ENABLE as usize] = true;
    }

    /// Port of `StateTracker::NotifyFramebufferSRGB`.
    pub fn notify_framebuffer_srgb(&mut self) {
        self.flags[dirty::FRAMEBUFFER_SRGB as usize] = true;
    }

    /// Port of `StateTracker::NotifyLogicOp`.
    pub fn notify_logic_op(&mut self) {
        self.flags[dirty::LOGIC_OP as usize] = true;
    }

    /// Port of `StateTracker::NotifyClipControl`.
    pub fn notify_clip_control(&mut self) {
        self.flags[dirty::CLIP_CONTROL as usize] = true;
    }

    /// Port of `StateTracker::NotifyAlphaTest`.
    pub fn notify_alpha_test(&mut self) {
        self.flags[dirty::ALPHA_TEST as usize] = true;
    }

    /// Port of `StateTracker::NotifyRange`.
    pub fn notify_range(&mut self, start: u8, end: u8) {
        for flag in start..=end {
            self.flags[flag as usize] = true;
        }
    }

    /// Port of `StateTracker::InvalidateState`.
    pub fn invalidate_state(&mut self) {
        for flag in self.flags.iter_mut() {
            *flag = true;
        }
    }

    /// Access the dirty flags array directly.
    pub fn flags_mut(&mut self) -> &mut DirtyFlags {
        &mut self.flags
    }

    /// Read-only access to dirty flags.
    pub fn flags(&self) -> &DirtyFlags {
        &self.flags
    }

    /// Check if a flag is dirty.
    pub fn is_dirty(&self, flag: u8) -> bool {
        self.flags[flag as usize]
    }

    /// Check if a flag is dirty, and clear it. Returns true if it was dirty.
    pub fn exchange(&mut self, flag: u8) -> bool {
        let is_dirty = self.flags[flag as usize];
        self.flags[flag as usize] = false;
        is_dirty
    }
}

impl Default for StateTracker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_all_dirty() {
        let tracker = StateTracker::new();
        assert!(tracker.is_dirty(dirty::VIEWPORTS));
        assert!(tracker.is_dirty(dirty::SCISSORS));
        assert!(tracker.is_dirty(dirty::FRONT_FACE));
        assert!(tracker.is_dirty(dirty::DEPTH_TEST));
    }

    #[test]
    fn test_exchange_clears_flag() {
        let mut tracker = StateTracker::new();
        assert!(tracker.exchange(dirty::VIEWPORTS));
        assert!(!tracker.exchange(dirty::VIEWPORTS));
    }

    #[test]
    fn test_notify_methods() {
        let mut tracker = StateTracker::new();
        // Clear everything first
        for flag in tracker.flags.iter_mut() {
            *flag = false;
        }
        // Now test notify methods set flags
        tracker.notify_front_face();
        assert!(tracker.is_dirty(dirty::FRONT_FACE));
        tracker.notify_cull_test();
        assert!(tracker.is_dirty(dirty::CULL_TEST));
        tracker.notify_depth_test();
        assert!(tracker.is_dirty(dirty::DEPTH_TEST));
    }

    #[test]
    fn test_notify_range() {
        let mut tracker = StateTracker::new();
        for flag in tracker.flags.iter_mut() {
            *flag = false;
        }
        tracker.notify_range(dirty::VIEWPORT_0, dirty::VIEWPORT_15);
        for i in dirty::VIEWPORT_0..=dirty::VIEWPORT_15 {
            assert!(tracker.is_dirty(i));
        }
    }
}
