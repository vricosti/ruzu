// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_texture_cache_base.cpp`.
//!
//! Explicit template instantiation file for the Vulkan texture cache.
//! In Rust this is handled by the generic type alias in `texture_cache.rs`,
//! but this file exists for structural parity with upstream.

// Upstream `vk_texture_cache_base.cpp` contains only:
//   #include "video_core/texture_cache/texture_cache.h"
//   #include "video_core/renderer_vulkan/vk_texture_cache.h"
//
// This forces the compiler to instantiate the TextureCache template
// with Vulkan-specific parameters. In Rust, monomorphization is automatic,
// so no explicit instantiation is needed.
//
// This file is kept for file-structure parity per CLAUDE.md rules.
