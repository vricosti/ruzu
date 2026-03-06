// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_null/renderer_null.h and renderer_null.cpp
//! Status: COMPLET
//!
//! Null rendering backend — all draw/render calls are silently ignored.
//! Used for headless mode and testing without GPU output.
//!
//! Also provides the [`GpuBackend`] trait used by the GPU context.

pub mod null_backend;
pub mod null_rasterizer;
pub mod renderer_null;

/// Backend type selection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackendType {
    Null,
    OpenGL,
    Vulkan,
}

/// Trait for GPU rendering backends.
///
/// All methods have default no-op implementations so that backends only
/// need to override the operations they support.
pub trait GpuBackend: Send + Sync {
    fn name(&self) -> &str;
    fn draw(&mut self) {}
    fn clear(&mut self) {}
    fn dispatch_compute(&mut self) {}
    fn copy_buffer(&mut self) {}
    fn blit(&mut self) {}
}
