// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_blit_screen.h` / `vk_blit_screen.cpp`.
//!
//! Composites guest framebuffers into a presentable frame using layers
//! and a window adaptation pass.

use ash::vk;

use super::present::layer::Layer;
use super::present::window_adapt_pass::WindowAdaptPass;
use super::present_manager::{Frame, PresentManager};

// ---------------------------------------------------------------------------
// ScalingFilter (matching upstream Settings::ScalingFilter)
// ---------------------------------------------------------------------------

/// Port of `Settings::ScalingFilter`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ScalingFilter {
    NearestNeighbor = 0,
    Bilinear = 1,
    Bicubic = 2,
    Gaussian = 3,
    ScaleForce = 4,
    Fsr = 5,
}

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
    device: ash::Device,
    image_count: usize,
    image_index: usize,
    swapchain_view_format: vk::Format,
    scaling_filter: Option<ScalingFilter>,
    window_adapt: Option<WindowAdaptPass>,
    layers: Vec<Layer>,
}

impl BlitScreen {
    /// Port of `BlitScreen::BlitScreen`.
    pub fn new(device: ash::Device) -> Self {
        BlitScreen {
            device,
            image_count: 1,
            image_index: 0,
            swapchain_view_format: vk::Format::B8G8R8A8_UNORM,
            scaling_filter: None,
            window_adapt: None,
            layers: Vec::new(),
        }
    }

    /// Port of `BlitScreen::DrawToFrame`.
    ///
    /// Draws the guest framebuffers into the given presentation frame,
    /// recreating resources as needed when the swapchain format/size changes.
    pub fn draw_to_frame(
        &mut self,
        frame: &mut Frame,
        present_manager: &PresentManager,
        current_swapchain_image_count: usize,
        current_swapchain_view_format: vk::Format,
        layout_width: u32,
        layout_height: u32,
        current_scaling_filter: ScalingFilter,
    ) {
        let mut resource_update_required = false;
        let mut presentation_recreate_required = false;

        // Check if scaling filter changed
        if self.window_adapt.is_none() || self.scaling_filter != Some(current_scaling_filter) {
            resource_update_required = true;
        }

        // Check if image count changed
        let old_count = self.image_count;
        self.image_count = current_swapchain_image_count;
        if old_count != current_swapchain_image_count {
            resource_update_required = true;
        }

        // Check if format or dimensions changed
        let old_format = self.swapchain_view_format;
        self.swapchain_view_format = current_swapchain_view_format;
        if old_format != current_swapchain_view_format
            || layout_width != frame.width
            || layout_height != frame.height
        {
            resource_update_required = true;
            presentation_recreate_required = true;
        }

        if resource_update_required {
            // Wait idle
            present_manager.wait_present();
            // NOTE: scheduler.Finish() and device.WaitIdle() would be called here

            // Update window adapt pass
            self.set_window_adapt_pass(current_scaling_filter);

            // Recreate frame if needed
            if presentation_recreate_required {
                if let Some(ref window_adapt) = self.window_adapt {
                    present_manager.recreate_frame(
                        frame,
                        layout_width,
                        layout_height,
                        self.swapchain_view_format,
                        window_adapt.get_render_pass(),
                    );
                }
            }
        }

        // Advance image index
        self.image_index += 1;
        if self.image_index >= self.image_count {
            self.image_index = 0;
        }
    }

    /// Port of `BlitScreen::CreateFramebuffer`.
    pub fn create_framebuffer(
        &mut self,
        image_view: vk::ImageView,
        current_view_format: vk::Format,
        layout_width: u32,
        layout_height: u32,
        scaling_filter: ScalingFilter,
    ) -> vk::Framebuffer {
        let format_updated = self.swapchain_view_format != current_view_format;
        self.swapchain_view_format = current_view_format;

        if self.window_adapt.is_none()
            || self.scaling_filter != Some(scaling_filter)
            || format_updated
        {
            self.set_window_adapt_pass(scaling_filter);
        }

        let render_pass = self
            .window_adapt
            .as_ref()
            .expect("window_adapt must be set")
            .get_render_pass();

        let extent = vk::Extent2D {
            width: layout_width,
            height: layout_height,
        };

        self.create_framebuffer_impl(image_view, extent, render_pass)
    }

    // --- Private ---

    /// Port of `BlitScreen::SetWindowAdaptPass`.
    fn set_window_adapt_pass(&mut self, filter: ScalingFilter) {
        self.layers.clear();
        self.scaling_filter = Some(filter);

        // Create the appropriate sampler for the scaling filter
        let sampler_filter = match filter {
            ScalingFilter::NearestNeighbor => vk::Filter::NEAREST,
            _ => vk::Filter::LINEAR,
        };

        let sampler_ci = vk::SamplerCreateInfo::builder()
            .mag_filter(sampler_filter)
            .min_filter(sampler_filter)
            .address_mode_u(vk::SamplerAddressMode::CLAMP_TO_BORDER)
            .address_mode_v(vk::SamplerAddressMode::CLAMP_TO_BORDER)
            .border_color(vk::BorderColor::FLOAT_OPAQUE_BLACK)
            .build();
        let sampler = unsafe {
            self.device
                .create_sampler(&sampler_ci, None)
                .expect("Failed to create blit screen sampler")
        };

        // Fragment shader would come from host_shaders based on filter type
        let fragment_shader = vk::ShaderModule::null();

        self.window_adapt = Some(WindowAdaptPass::new(
            self.device.clone(),
            self.swapchain_view_format,
            sampler,
            fragment_shader,
        ));
    }

    /// Port of `BlitScreen::CreateFramebuffer` (private overload).
    fn create_framebuffer_impl(
        &self,
        image_view: vk::ImageView,
        extent: vk::Extent2D,
        render_pass: vk::RenderPass,
    ) -> vk::Framebuffer {
        let attachments = [image_view];
        let fb_ci = vk::FramebufferCreateInfo::builder()
            .render_pass(render_pass)
            .attachments(&attachments)
            .width(extent.width)
            .height(extent.height)
            .layers(1)
            .build();

        unsafe {
            self.device
                .create_framebuffer(&fb_ci, None)
                .expect("Failed to create blit screen framebuffer")
        }
    }
}
