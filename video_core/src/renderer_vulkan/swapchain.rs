// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_swapchain.h` / `vk_swapchain.cpp`.
//!
//! Vulkan swapchain management -- creation, image acquisition, presentation,
//! and recreation.

use ash::vk;

// ---------------------------------------------------------------------------
// VSyncMode (matching upstream Settings::VSyncMode)
// ---------------------------------------------------------------------------

/// Port of `Settings::VSyncMode`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum VSyncMode {
    Immediate = 0,
    Mailbox = 1,
    Fifo = 2,
    FifoRelaxed = 3,
}

// ---------------------------------------------------------------------------
// Anonymous namespace helpers
// ---------------------------------------------------------------------------

/// Port of `ChooseSwapSurfaceFormat`.
fn choose_swap_surface_format(formats: &[vk::SurfaceFormatKHR]) -> vk::SurfaceFormatKHR {
    if formats.len() == 1 && formats[0].format == vk::Format::UNDEFINED {
        return vk::SurfaceFormatKHR {
            format: vk::Format::B8G8R8A8_UNORM,
            color_space: vk::ColorSpaceKHR::SRGB_NONLINEAR,
        };
    }
    formats
        .iter()
        .find(|f| {
            f.format == vk::Format::B8G8R8A8_UNORM
                && f.color_space == vk::ColorSpaceKHR::SRGB_NONLINEAR
        })
        .copied()
        .unwrap_or(formats[0])
}

/// Port of `ChooseSwapPresentMode`.
fn choose_swap_present_mode(
    has_imm: bool,
    has_mailbox: bool,
    has_fifo_relaxed: bool,
    vsync_mode: VSyncMode,
    use_speed_limit: bool,
) -> vk::PresentModeKHR {
    let setting = if !use_speed_limit {
        match vsync_mode {
            VSyncMode::Fifo | VSyncMode::FifoRelaxed => {
                if has_mailbox {
                    VSyncMode::Mailbox
                } else if has_imm {
                    VSyncMode::Immediate
                } else {
                    vsync_mode
                }
            }
            other => other,
        }
    } else {
        vsync_mode
    };

    // Validate availability, fallback to FIFO
    let validated = match setting {
        VSyncMode::Mailbox if !has_mailbox => VSyncMode::Fifo,
        VSyncMode::Immediate if !has_imm => VSyncMode::Fifo,
        VSyncMode::FifoRelaxed if !has_fifo_relaxed => VSyncMode::Fifo,
        other => other,
    };

    match validated {
        VSyncMode::Immediate => vk::PresentModeKHR::IMMEDIATE,
        VSyncMode::Mailbox => vk::PresentModeKHR::MAILBOX,
        VSyncMode::Fifo => vk::PresentModeKHR::FIFO,
        VSyncMode::FifoRelaxed => vk::PresentModeKHR::FIFO_RELAXED,
    }
}

/// Port of `ChooseSwapExtent`.
fn choose_swap_extent(
    capabilities: &vk::SurfaceCapabilitiesKHR,
    width: u32,
    height: u32,
) -> vk::Extent2D {
    if capabilities.current_extent.width != u32::MAX {
        return capabilities.current_extent;
    }
    vk::Extent2D {
        width: width
            .max(capabilities.min_image_extent.width)
            .min(capabilities.max_image_extent.width),
        height: height
            .max(capabilities.min_image_extent.height)
            .min(capabilities.max_image_extent.height),
    }
}

/// Port of `ChooseAlphaFlags`.
fn choose_alpha_flags(capabilities: &vk::SurfaceCapabilitiesKHR) -> vk::CompositeAlphaFlagsKHR {
    if capabilities
        .supported_composite_alpha
        .contains(vk::CompositeAlphaFlagsKHR::OPAQUE)
    {
        vk::CompositeAlphaFlagsKHR::OPAQUE
    } else if capabilities
        .supported_composite_alpha
        .contains(vk::CompositeAlphaFlagsKHR::INHERIT)
    {
        vk::CompositeAlphaFlagsKHR::INHERIT
    } else {
        log::error!(
            "Unknown composite alpha flags value {:#x}",
            capabilities.supported_composite_alpha.as_raw()
        );
        vk::CompositeAlphaFlagsKHR::OPAQUE
    }
}

// ---------------------------------------------------------------------------
// Swapchain
// ---------------------------------------------------------------------------

/// Port of `Swapchain` class.
pub struct Swapchain {
    surface: vk::SurfaceKHR,
    swapchain: vk::SwapchainKHR,

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
    pub fn new(surface: vk::SurfaceKHR, width: u32, height: u32) -> Self {
        Swapchain {
            surface,
            swapchain: vk::SwapchainKHR::null(),
            image_count: 0,
            images: Vec::new(),
            resource_ticks: Vec::new(),
            present_semaphores: Vec::new(),
            render_semaphores: Vec::new(),
            width,
            height,
            image_index: 0,
            frame_index: 0,
            image_view_format: vk::Format::UNDEFINED,
            extent: vk::Extent2D::default(),
            present_mode: vk::PresentModeKHR::FIFO,
            surface_format: vk::SurfaceFormatKHR::default(),
            has_imm: false,
            has_mailbox: false,
            has_fifo_relaxed: false,
            is_outdated: false,
            is_suboptimal: false,
        }
    }

    /// Port of `Swapchain::Create`.
    pub fn create(&mut self, surface: vk::SurfaceKHR, width: u32, height: u32) {
        self.is_outdated = false;
        self.is_suboptimal = false;
        self.width = width;
        self.height = height;
        self.surface = surface;

        // NOTE: actual creation requires physical device + logical device.
        // The full Create() body depends on Device integration which is
        // done when the Device type is wired in.
        // For now: clear and prepare resource_ticks.
        self.destroy();
        self.resource_ticks.clear();
        self.resource_ticks.resize(self.image_count, 0);
    }

    /// Port of `Swapchain::AcquireNextImage`.
    ///
    /// Returns `true` if the swapchain is suboptimal or outdated.
    pub fn acquire_next_image(&mut self) -> bool {
        // In the full implementation, this calls vkAcquireNextImageKHR
        // and handles VK_SUBOPTIMAL_KHR / VK_ERROR_OUT_OF_DATE_KHR.
        self.is_suboptimal || self.is_outdated
    }

    /// Port of `Swapchain::Present`.
    pub fn present(&mut self, _render_semaphore: vk::Semaphore) {
        // In the full implementation, this calls vkQueuePresentKHR.
        self.frame_index += 1;
        if self.frame_index as usize >= self.image_count {
            self.frame_index = 0;
        }
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

    /// Port of `Swapchain::CreateSwapchain`.
    ///
    /// Creates the Vulkan swapchain with the given surface capabilities.
    fn create_swapchain(
        &mut self,
        capabilities: &vk::SurfaceCapabilitiesKHR,
        available_formats: &[vk::SurfaceFormatKHR],
        available_present_modes: &[vk::PresentModeKHR],
    ) {
        self.has_mailbox = available_present_modes.contains(&vk::PresentModeKHR::MAILBOX);
        self.has_imm = available_present_modes.contains(&vk::PresentModeKHR::IMMEDIATE);
        self.has_fifo_relaxed =
            available_present_modes.contains(&vk::PresentModeKHR::FIFO_RELAXED);

        let _alpha_flags = choose_alpha_flags(capabilities);
        self.surface_format = choose_swap_surface_format(available_formats);
        self.present_mode = choose_swap_present_mode(
            self.has_imm,
            self.has_mailbox,
            self.has_fifo_relaxed,
            VSyncMode::Fifo, // default
            true,            // use_speed_limit default
        );

        // Request triple buffering if possible
        let mut requested_image_count = capabilities.min_image_count + 1;
        if capabilities.max_image_count > 0 {
            if requested_image_count > capabilities.max_image_count {
                requested_image_count = capabilities.max_image_count;
            } else {
                requested_image_count =
                    requested_image_count.max(3u32.min(capabilities.max_image_count));
            }
        } else {
            requested_image_count = requested_image_count.max(3);
        }

        self.extent = choose_swap_extent(capabilities, self.width, self.height);
        self.image_view_format = vk::Format::B8G8R8A8_UNORM;

        // NOTE: actual vkCreateSwapchainKHR call requires a VkDevice handle.
        // The swapchain creation info would be:
        //   surface, min_image_count, image_format, color_space, image_extent,
        //   image_array_layers=1, image_usage=COLOR_ATTACHMENT|TRANSFER_DST,
        //   sharing_mode, pre_transform, composite_alpha, present_mode, clipped=false
        let _ = requested_image_count;
    }

    /// Port of `Swapchain::CreateSemaphores`.
    fn create_semaphores(&mut self, device: &ash::Device) {
        self.present_semaphores.clear();
        self.render_semaphores.clear();
        let semaphore_ci = vk::SemaphoreCreateInfo::builder().build();
        for _i in 0..self.image_count {
            let present = unsafe {
                device
                    .create_semaphore(&semaphore_ci, None)
                    .expect("Failed to create present semaphore")
            };
            let render = unsafe {
                device
                    .create_semaphore(&semaphore_ci, None)
                    .expect("Failed to create render semaphore")
            };
            self.present_semaphores.push(present);
            self.render_semaphores.push(render);
        }
    }

    /// Port of `Swapchain::Destroy`.
    fn destroy(&mut self) {
        self.frame_index = 0;
        self.present_semaphores.clear();
        self.render_semaphores.clear();
        self.swapchain = vk::SwapchainKHR::null();
    }

    /// Port of `Swapchain::NeedsPresentModeUpdate`.
    fn needs_present_mode_update(&self) -> bool {
        let requested = choose_swap_present_mode(
            self.has_imm,
            self.has_mailbox,
            self.has_fifo_relaxed,
            VSyncMode::Fifo,
            true,
        );
        self.present_mode != requested
    }
}
