// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/
//!
//! Presentation pipeline — filters, anti-aliasing, upscaling, and final screen composition.

pub mod filters;
pub mod fsr;
pub mod fxaa;
pub mod layer;
pub mod present_uniforms;
pub mod smaa;
pub mod util;
pub mod window_adapt_pass;
