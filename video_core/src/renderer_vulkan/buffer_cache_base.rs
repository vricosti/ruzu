// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_buffer_cache_base.cpp`.
//!
//! Explicit template instantiation file for the Vulkan buffer cache.
//! In Rust this is handled by the generic type alias in `buffer_cache.rs`,
//! but this file exists for structural parity with upstream.

// Upstream `vk_buffer_cache_base.cpp` contains only:
//   #include "video_core/buffer_cache/buffer_cache.h"
//   #include "video_core/renderer_vulkan/vk_buffer_cache.h"
//
// This forces the compiler to instantiate the BufferCache template
// with Vulkan-specific parameters. In Rust, monomorphization is automatic,
// so no explicit instantiation is needed.
//
// This file is kept for file-structure parity per CLAUDE.md rules.
