// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_texture_cache_base.cpp
//!
//! Template instantiation of the generic texture cache for OpenGL.
//! In Rust, this is a type alias rather than a template instantiation.

// In upstream C++, this file contains:
//   template class VideoCommon::TextureCache<OpenGL::TextureCacheParams>;
//
// In Rust the equivalent is the type alias in gl_texture_cache.rs.
// This file exists for file-structure parity.

pub use super::gl_texture_cache::{TextureCacheParams, TextureCacheRuntime};
