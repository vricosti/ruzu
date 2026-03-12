// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `present/filters.h` / `present/filters.cpp`.
//!
//! Factory functions for creating window adaptation passes with different
//! scaling filters.

use ash::vk;

/// Port of `MakeNearestNeighbor`.
pub fn make_nearest_neighbor(_frame_format: vk::Format) -> super::window_adapt_pass::WindowAdaptPass {
    todo!("make_nearest_neighbor")
}

/// Port of `MakeBilinear`.
pub fn make_bilinear(_frame_format: vk::Format) -> super::window_adapt_pass::WindowAdaptPass {
    todo!("make_bilinear")
}

/// Port of `MakeBicubic`.
pub fn make_bicubic(_frame_format: vk::Format) -> super::window_adapt_pass::WindowAdaptPass {
    todo!("make_bicubic")
}

/// Port of `MakeGaussian`.
pub fn make_gaussian(_frame_format: vk::Format) -> super::window_adapt_pass::WindowAdaptPass {
    todo!("make_gaussian")
}

/// Port of `MakeScaleForce`.
pub fn make_scale_force(_frame_format: vk::Format) -> super::window_adapt_pass::WindowAdaptPass {
    todo!("make_scale_force")
}
