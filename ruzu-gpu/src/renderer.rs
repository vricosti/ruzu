// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Vulkan renderer initialization.
//!
//! Phase 1 stub: sets up the Vulkan instance, device, and queue.
//! Actual rendering will be implemented in later phases.

use log::info;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum RendererError {
    #[error("Vulkan initialization failed: {0}")]
    InitFailed(String),
    #[error("No suitable GPU found")]
    NoSuitableDevice,
    #[error("Surface creation failed: {0}")]
    SurfaceFailed(String),
}

/// Vulkan renderer state.
///
/// Phase 1: This is a placeholder. Full vulkano integration requires
/// the Vulkan loader and GPU to be available at build time.
/// The actual renderer will use:
/// - vulkano::instance::Instance
/// - vulkano::device::{Device, Queue}
/// - vulkano::swapchain::Swapchain
pub struct Renderer {
    /// Whether the renderer has been initialized.
    pub initialized: bool,
}

impl Renderer {
    /// Create a new renderer (stub for Phase 1).
    ///
    /// In the full implementation, this will:
    /// 1. Create a Vulkan Instance with required extensions
    /// 2. Select a physical device (prefer discrete GPU)
    /// 3. Create a logical device with a graphics queue
    pub fn new() -> Result<Self, RendererError> {
        info!("Renderer: Phase 1 stub initialized");
        info!("  Full Vulkan rendering will be enabled when vulkano integration is complete");

        Ok(Self { initialized: true })
    }

    /// Present a frame (stub: does nothing in Phase 1).
    pub fn present_frame(&mut self) {
        // Phase 1: no-op
        // Full implementation will acquire swapchain image, clear, and present
    }
}

impl Default for Renderer {
    fn default() -> Self {
        Self { initialized: false }
    }
}
