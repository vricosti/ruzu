// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_blit_screen.h` / `vk_blit_screen.cpp`.
//!
//! Composites guest framebuffers into a presentable frame using layers
//! and a window adaptation pass.

use ash::vk;

// ---------------------------------------------------------------------------
// FramebufferTextureInfo
// ---------------------------------------------------------------------------

/// Port of `FramebufferTextureInfo` struct.
///
/// Information about a guest framebuffer's backing Vulkan image/view.
#[derive(Debug, Clone, Copy, Default)]
pub struct FramebufferTextureInfo {
    pub image: vk::Image,
    pub image_view: vk::ImageView,
    pub width: u32,
    pub height: u32,
    pub scaled_width: u32,
    pub scaled_height: u32,
}

// ---------------------------------------------------------------------------
// BlitScreen
// ---------------------------------------------------------------------------

/// Port of `BlitScreen` class.
///
/// Manages layers, anti-aliasing, scaling, and the window adaptation pass
/// to composite guest framebuffers into a presentable output frame.
pub struct BlitScreen {
    _image_count: usize,
    _image_index: usize,
    _swapchain_view_format: vk::Format,
    _scaling_filter: u32,
}

impl BlitScreen {
    /// Port of `BlitScreen::BlitScreen`.
    pub fn new() -> Self {
        todo!("BlitScreen::new")
    }

    /// Port of `BlitScreen::DrawToFrame`.
    pub fn draw_to_frame(
        &mut self,
        _frame: &mut super::present_manager::Frame,
        _current_swapchain_image_count: usize,
        _current_swapchain_view_format: vk::Format,
    ) {
        todo!("BlitScreen::draw_to_frame")
    }

    /// Port of `BlitScreen::CreateFramebuffer`.
    pub fn create_framebuffer(
        &self,
        _image_view: vk::ImageView,
        _current_view_format: vk::Format,
    ) -> vk::Framebuffer {
        todo!("BlitScreen::create_framebuffer")
    }

    // --- Private ---

    fn wait_idle(&self) { todo!() }
    fn set_window_adapt_pass(&mut self) { todo!() }
}
