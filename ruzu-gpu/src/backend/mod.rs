// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GPU rendering backend abstraction.
//!
//! Provides a trait for rendering backends. Currently only a null backend
//! is implemented; Vulkan will be added in a future phase.

pub mod null_backend;

/// Backend type selection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackendType {
    Null,
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
