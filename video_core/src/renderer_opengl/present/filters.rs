// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/filters.h and filters.cpp
//!
//! Factory functions for creating window adapt passes with different scaling filters.

use super::window_adapt_pass::WindowAdaptPass;

/// Create a nearest-neighbor scaling filter pass.
///
/// Corresponds to `OpenGL::MakeNearestNeighbor()`.
pub fn make_nearest_neighbor() -> WindowAdaptPass {
    todo!("MakeNearestNeighbor")
}

/// Create a bilinear scaling filter pass.
///
/// Corresponds to `OpenGL::MakeBilinear()`.
pub fn make_bilinear() -> WindowAdaptPass {
    todo!("MakeBilinear")
}

/// Create a bicubic scaling filter pass.
///
/// Corresponds to `OpenGL::MakeBicubic()`.
pub fn make_bicubic() -> WindowAdaptPass {
    todo!("MakeBicubic")
}

/// Create a Gaussian scaling filter pass.
///
/// Corresponds to `OpenGL::MakeGaussian()`.
pub fn make_gaussian() -> WindowAdaptPass {
    todo!("MakeGaussian")
}

/// Create a ScaleForce scaling filter pass.
///
/// Corresponds to `OpenGL::MakeScaleForce()`.
pub fn make_scale_force() -> WindowAdaptPass {
    todo!("MakeScaleForce")
}
