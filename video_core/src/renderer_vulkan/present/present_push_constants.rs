// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/present_push_constants.h`.
//!
//! Push constant structures and helpers for the presentation pipeline.

// ---------------------------------------------------------------------------
// ScreenRectVertex
// ---------------------------------------------------------------------------

/// Port of `ScreenRectVertex` struct.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ScreenRectVertex {
    pub position: [f32; 2],
    pub tex_coord: [f32; 2],
}

impl ScreenRectVertex {
    /// Port of `ScreenRectVertex::ScreenRectVertex(f32, f32, f32, f32)`.
    pub fn new(x: f32, y: f32, u: f32, v: f32) -> Self {
        ScreenRectVertex {
            position: [x, y],
            tex_coord: [u, v],
        }
    }
}

// ---------------------------------------------------------------------------
// MakeOrthographicMatrix
// ---------------------------------------------------------------------------

/// Port of `MakeOrthographicMatrix`.
///
/// Creates a 4x4 orthographic projection matrix for screen-space rendering.
pub fn make_orthographic_matrix(width: f32, height: f32) -> [f32; 16] {
    [
        2.0 / width,
        0.0,
        0.0,
        0.0,
        0.0,
        2.0 / height,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
        -1.0,
        -1.0,
        0.0,
        1.0,
    ]
}

// ---------------------------------------------------------------------------
// PresentPushConstants
// ---------------------------------------------------------------------------

/// Port of `PresentPushConstants` struct.
///
/// Contains the modelview matrix and four screen-rect vertices,
/// fitting within the 128-byte push constant limit.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PresentPushConstants {
    pub modelview_matrix: [f32; 16],
    pub vertices: [ScreenRectVertex; 4],
}

// Static assert equivalent: size must be <= 128 bytes.
const _: () = assert!(
    std::mem::size_of::<PresentPushConstants>() <= 128,
    "Push constants are too large"
);

impl Default for PresentPushConstants {
    fn default() -> Self {
        PresentPushConstants {
            modelview_matrix: [0.0; 16],
            vertices: [ScreenRectVertex::default(); 4],
        }
    }
}
