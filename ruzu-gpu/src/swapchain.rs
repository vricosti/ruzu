// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Swapchain management and frame presentation.
//!
//! Phase 1 stub: the full implementation will handle:
//! - Swapchain creation with optimal format/present mode
//! - Image acquisition and presentation
//! - Clear color rendering (cornflower blue: 0.39, 0.58, 0.93, 1.0)
//! - Window resize handling (swapchain recreation)

use log::info;

/// Clear color: cornflower blue (matches XNA/MonoGame default).
pub const CLEAR_COLOR: [f32; 4] = [0.39, 0.58, 0.93, 1.0];

/// Swapchain manager (Phase 1 stub).
pub struct SwapchainManager {
    pub width: u32,
    pub height: u32,
}

impl SwapchainManager {
    /// Create a new swapchain manager (stub).
    pub fn new(width: u32, height: u32) -> Self {
        info!(
            "SwapchainManager: {}x{} (Phase 1 stub, clear color: cornflower blue)",
            width, height
        );
        Self { width, height }
    }

    /// Resize the swapchain (stub).
    pub fn resize(&mut self, width: u32, height: u32) {
        info!("SwapchainManager: resize {}x{}", width, height);
        self.width = width;
        self.height = height;
    }
}
