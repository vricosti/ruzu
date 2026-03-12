// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_present_manager.h` / `vk_present_manager.cpp`.
//!
//! Manages presentation frames, a present thread, and swapchain copies.

use std::collections::VecDeque;
use std::sync::{Condvar, Mutex};

use ash::vk;

// ---------------------------------------------------------------------------
// Frame
// ---------------------------------------------------------------------------

/// Port of `Frame` struct.
///
/// A single presentation frame with its image, views, and synchronization
/// primitives.
#[derive(Default)]
pub struct Frame {
    pub width: u32,
    pub height: u32,
    pub image: vk::Image,
    pub image_view: vk::ImageView,
    pub framebuffer: vk::Framebuffer,
    pub cmdbuf: vk::CommandBuffer,
    pub render_ready: vk::Semaphore,
    pub present_done: vk::Fence,
}

// ---------------------------------------------------------------------------
// PresentManager
// ---------------------------------------------------------------------------

/// Port of `PresentManager` class.
///
/// Manages a pool of `Frame` objects, a present queue, and an optional
/// present thread that copies rendered frames to the swapchain.
pub struct PresentManager {
    _cmdpool: vk::CommandPool,
    _frames: Vec<Frame>,
    _present_queue: VecDeque<usize>,
    _free_queue: VecDeque<usize>,
    _frame_cv: Condvar,
    _free_cv: Condvar,
    _swapchain_mutex: Mutex<()>,
    _queue_mutex: Mutex<()>,
    _free_mutex: Mutex<()>,
    _blit_supported: bool,
    _use_present_thread: bool,
    _image_count: usize,
}

impl PresentManager {
    /// Port of `PresentManager::PresentManager`.
    pub fn new() -> Self {
        todo!("PresentManager::new")
    }

    /// Port of `PresentManager::GetRenderFrame`.
    pub fn get_render_frame(&mut self) -> &mut Frame {
        todo!("PresentManager::get_render_frame")
    }

    /// Port of `PresentManager::Present`.
    pub fn present(&mut self, _frame: &mut Frame) {
        todo!("PresentManager::present")
    }

    /// Port of `PresentManager::RecreateFrame`.
    pub fn recreate_frame(
        &mut self,
        _frame: &mut Frame,
        _width: u32,
        _height: u32,
        _image_view_format: vk::Format,
        _render_pass: vk::RenderPass,
    ) {
        todo!("PresentManager::recreate_frame")
    }

    /// Port of `PresentManager::WaitPresent`.
    pub fn wait_present(&mut self) {
        todo!("PresentManager::wait_present")
    }

    // --- Private ---

    fn present_thread(&mut self) { todo!() }
    fn copy_to_swapchain(&mut self, _frame: &mut Frame) { todo!() }
    fn copy_to_swapchain_impl(&mut self, _frame: &mut Frame) { todo!() }
    fn recreate_swapchain(&mut self, _frame: &mut Frame) { todo!() }
    fn set_image_count(&mut self) { todo!() }
}
