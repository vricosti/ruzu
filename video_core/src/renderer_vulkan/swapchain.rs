// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_swapchain.h` / `vk_swapchain.cpp`.
//!
//! Vulkan swapchain management — creation, image acquisition, presentation,
//! and recreation.

use ash::vk;

// ---------------------------------------------------------------------------
// Swapchain
// ---------------------------------------------------------------------------

/// Port of `Swapchain` class.
pub struct Swapchain {
    surface: vk::SurfaceKHR,
    _swapchain: vk::SwapchainKHR,

    image_count: usize,
    images: Vec<vk::Image>,
    resource_ticks: Vec<u64>,
    present_semaphores: Vec<vk::Semaphore>,
    render_semaphores: Vec<vk::Semaphore>,

    width: u32,
    height: u32,
    image_index: u32,
    frame_index: u32,

    image_view_format: vk::Format,
    extent: vk::Extent2D,
    present_mode: vk::PresentModeKHR,
    surface_format: vk::SurfaceFormatKHR,

    has_imm: bool,
    has_mailbox: bool,
    has_fifo_relaxed: bool,

    is_outdated: bool,
    is_suboptimal: bool,
}

impl Swapchain {
    /// Port of `Swapchain::Swapchain`.
    pub fn new(
        _surface: vk::SurfaceKHR,
        _width: u32,
        _height: u32,
    ) -> Self {
        todo!("Swapchain::new")
    }

    /// Port of `Swapchain::Create`.
    pub fn create(&mut self, _surface: vk::SurfaceKHR, _width: u32, _height: u32) {
        todo!("Swapchain::create")
    }

    /// Port of `Swapchain::AcquireNextImage`.
    pub fn acquire_next_image(&mut self) -> bool {
        todo!("Swapchain::acquire_next_image")
    }

    /// Port of `Swapchain::Present`.
    pub fn present(&mut self, _render_semaphore: vk::Semaphore) {
        todo!("Swapchain::present")
    }

    /// Port of `Swapchain::NeedsRecreation`.
    pub fn needs_recreation(&self) -> bool {
        self.is_suboptimal() || self.needs_present_mode_update()
    }

    /// Port of `Swapchain::IsOutDated`.
    pub fn is_outdated(&self) -> bool {
        self.is_outdated
    }

    /// Port of `Swapchain::IsSubOptimal`.
    pub fn is_suboptimal(&self) -> bool {
        self.is_suboptimal
    }

    /// Port of `Swapchain::GetSize`.
    pub fn get_size(&self) -> vk::Extent2D {
        self.extent
    }

    /// Port of `Swapchain::GetImageCount`.
    pub fn get_image_count(&self) -> usize {
        self.image_count
    }

    /// Port of `Swapchain::GetImageIndex`.
    pub fn get_image_index(&self) -> usize {
        self.image_index as usize
    }

    /// Port of `Swapchain::GetFrameIndex`.
    pub fn get_frame_index(&self) -> usize {
        self.frame_index as usize
    }

    /// Port of `Swapchain::GetImageIndex(index)`.
    pub fn get_image_at(&self, index: usize) -> vk::Image {
        self.images[index]
    }

    /// Port of `Swapchain::CurrentImage`.
    pub fn current_image(&self) -> vk::Image {
        self.images[self.image_index as usize]
    }

    /// Port of `Swapchain::GetImageViewFormat`.
    pub fn get_image_view_format(&self) -> vk::Format {
        self.image_view_format
    }

    /// Port of `Swapchain::GetImageFormat`.
    pub fn get_image_format(&self) -> vk::Format {
        self.surface_format.format
    }

    /// Port of `Swapchain::CurrentPresentSemaphore`.
    pub fn current_present_semaphore(&self) -> vk::Semaphore {
        self.present_semaphores[self.frame_index as usize]
    }

    /// Port of `Swapchain::CurrentRenderSemaphore`.
    pub fn current_render_semaphore(&self) -> vk::Semaphore {
        self.render_semaphores[self.frame_index as usize]
    }

    /// Port of `Swapchain::GetWidth`.
    pub fn get_width(&self) -> u32 {
        self.width
    }

    /// Port of `Swapchain::GetHeight`.
    pub fn get_height(&self) -> u32 {
        self.height
    }

    /// Port of `Swapchain::GetExtent`.
    pub fn get_extent(&self) -> vk::Extent2D {
        self.extent
    }

    // --- Private ---

    fn create_swapchain(&mut self, _capabilities: &vk::SurfaceCapabilitiesKHR) { todo!() }
    fn create_semaphores(&mut self) { todo!() }
    fn create_image_views(&mut self) { todo!() }
    fn destroy(&mut self) { todo!() }

    fn needs_present_mode_update(&self) -> bool {
        todo!("Swapchain::needs_present_mode_update")
    }
}
