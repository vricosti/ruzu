// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SDL2 -> Vulkan surface bridge.
//!
//! Phase 1 stub: documents the integration path for SDL2_Vulkan_CreateSurface
//! with vulkano's Surface wrapper.
//!
//! The full implementation will:
//! 1. Get Vulkan instance extensions from SDL2 via `SDL_Vulkan_GetInstanceExtensions`
//! 2. Create the Vulkan instance with those extensions
//! 3. Call `SDL_Vulkan_CreateSurface` to get a VkSurfaceKHR
//! 4. Wrap it in vulkano's `Surface` using `Surface::from_raw`
//!
//! This requires the `ash` crate for raw Vulkan handles or vulkano's unsafe surface creation.

use log::info;

/// Surface bridge between SDL2 and Vulkan.
///
/// Phase 1 stub.
pub struct SurfaceBridge {
    pub width: u32,
    pub height: u32,
}

impl SurfaceBridge {
    /// Create a new surface bridge (stub).
    pub fn new(width: u32, height: u32) -> Self {
        info!("SurfaceBridge: {}x{} (Phase 1 stub)", width, height);
        Self { width, height }
    }
}
