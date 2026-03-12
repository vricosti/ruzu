// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_state_tracker.h and gl_state_tracker.cpp
//! Status: EN COURS
//!
//! Tracks OpenGL state changes to avoid redundant GL calls.
//! Uses dirty flags to selectively re-apply state.

/// Dirty flags for OpenGL state that needs updating.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DirtyFlags(u64);

impl DirtyFlags {
    pub const VIEWPORT: Self = Self(1 << 0);
    pub const SCISSOR: Self = Self(1 << 1);
    pub const DEPTH_TEST: Self = Self(1 << 2);
    pub const STENCIL_TEST: Self = Self(1 << 3);
    pub const BLEND: Self = Self(1 << 4);
    pub const COLOR_MASK: Self = Self(1 << 5);
    pub const CULL_FACE: Self = Self(1 << 6);
    pub const FRONT_FACE: Self = Self(1 << 7);
    pub const POLYGON_OFFSET: Self = Self(1 << 8);
    pub const VERTEX_BUFFERS: Self = Self(1 << 9);
    pub const VERTEX_FORMAT: Self = Self(1 << 10);
    pub const SHADERS: Self = Self(1 << 11);
    pub const RENDER_TARGETS: Self = Self(1 << 12);
    pub const UNIFORM_BUFFER: Self = Self(1 << 13);
    pub const ALL: Self = Self(!0);

    pub const fn empty() -> Self {
        Self(0)
    }

    pub const fn contains(self, other: Self) -> bool {
        (self.0 & other.0) == other.0
    }
}

/// Tracks OpenGL state to minimize redundant API calls.
///
/// Corresponds to zuyu's `StateTracker` class.
pub struct StateTracker {
    dirty: DirtyFlags,
}

impl StateTracker {
    pub fn new() -> Self {
        Self {
            dirty: DirtyFlags::ALL,
        }
    }

    /// Mark all state as dirty (e.g., after context switch).
    pub fn invalidate_all(&mut self) {
        self.dirty = DirtyFlags::ALL;
    }

    /// Check if a specific flag is dirty.
    pub fn is_dirty(&self, flag: DirtyFlags) -> bool {
        self.dirty.contains(flag)
    }

    /// Mark a flag as dirty.
    pub fn mark_dirty(&mut self, flag: DirtyFlags) {
        self.dirty = DirtyFlags(self.dirty.0 | flag.0);
    }

    /// Clear a dirty flag (after applying the state).
    pub fn clear_dirty(&mut self, flag: DirtyFlags) {
        self.dirty = DirtyFlags(self.dirty.0 & !flag.0);
    }

    /// Clear all dirty flags.
    pub fn clear_all(&mut self) {
        self.dirty = DirtyFlags::empty();
    }
}

impl Default for StateTracker {
    fn default() -> Self {
        Self::new()
    }
}
