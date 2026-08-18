// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of Eden `video_core/renderer_opengl/gl_texture_cache_base.cpp`.
//!
//! Explicit instantiation owner for Eden's generic OpenGL texture cache.
//!
//! Rust monomorphizes `CommonTextureCache<TextureCacheParams>` where the
//! transparent `gl_texture_cache::TextureCache` owner uses it, so the C++
//! explicit-instantiation statement has no executable Rust counterpart. This
//! module remains present solely to preserve the upstream translation-unit
//! boundary; it intentionally exports no second alias or runtime API.
