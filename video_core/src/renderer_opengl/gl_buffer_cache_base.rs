// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_buffer_cache_base.cpp
//!
//! Template instantiation of the generic buffer cache for OpenGL.
//! In Rust, this is a type alias rather than a template instantiation.

// In upstream C++, this file contains:
//   template class VideoCommon::BufferCache<OpenGL::BufferCacheParams>;
//
// In Rust the equivalent is the type alias in gl_buffer_cache.rs.
// This file exists for file-structure parity.

pub use super::gl_buffer_cache::{BufferCacheParams, BufferCacheRuntime};
