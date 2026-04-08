// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/renderer_base.h and video_core/renderer_base.cpp
//!
//! Abstract base renderer interface.

use std::sync::atomic::{AtomicBool, Ordering};

use crate::framebuffer_config::FramebufferConfig;

/// Framebuffer layout for screenshots.
#[derive(Debug, Clone, Default)]
pub struct FramebufferLayout {
    pub width: u32,
    pub height: u32,
}

/// Renderer settings (screenshots, etc.).
pub struct RendererSettings {
    pub screenshot_requested: AtomicBool,
    pub screenshot_bits: *mut std::ffi::c_void,
    pub screenshot_complete_callback: Option<Box<dyn FnOnce(bool) + Send>>,
    pub screenshot_framebuffer_layout: FramebufferLayout,
}

// Safety: screenshot_bits is only accessed on the render thread.
unsafe impl Send for RendererSettings {}
unsafe impl Sync for RendererSettings {}

impl Default for RendererSettings {
    fn default() -> Self {
        Self {
            screenshot_requested: AtomicBool::new(false),
            screenshot_bits: std::ptr::null_mut(),
            screenshot_complete_callback: None,
            screenshot_framebuffer_layout: FramebufferLayout::default(),
        }
    }
}

/// Callback for reading guest GPU memory by address.
/// Upstream: `Tegra::MaxwellDeviceMemoryManager& device_memory`.
/// In Rust, we use a shared callback since Memory is behind locks.
pub type DeviceMemoryReader = std::sync::Arc<dyn Fn(u64, &mut [u8]) + Send + Sync>;

/// Abstract renderer base trait.
///
/// Renderers (OpenGL, Vulkan, Null) implement this trait.
pub trait RendererBase: Send {
    /// Get a mutable pointer to the graphics context owned by this renderer.
    /// Matches upstream `RendererBase::Context()`.
    /// The returned pointer is valid for the lifetime of the renderer.
    fn context_ptr(&mut self) -> *mut dyn ruzu_core::frontend::graphics_context::GraphicsContext;

    /// Finalize rendering the guest frame and draw into the presentation texture.
    fn composite(&mut self, layers: &[FramebufferConfig]);

    /// Get the tiled applet layer capture buffer.
    fn get_applet_capture_buffer(&self) -> Vec<u8>;

    /// Get the rasterizer interface.
    fn read_rasterizer(&self) -> *mut dyn crate::rasterizer_interface::RasterizerInterface;

    /// Get the device vendor string.
    fn get_device_vendor(&self) -> String;

    /// Get current FPS.
    fn current_fps(&self) -> f32;

    /// Get current frame count.
    fn current_frame(&self) -> i32;

    /// Refresh base settings.
    fn refresh_base_settings(&mut self);

    /// Returns true if a screenshot is being processed.
    fn is_screenshot_pending(&self) -> bool;

    /// Set the device memory reader for framebuffer loading.
    /// Upstream passes `device_memory` in the constructor; in Rust we set it
    /// post-construction since the GPU creates the renderer before memory is wired.
    fn set_device_memory_reader(&mut self, _reader: DeviceMemoryReader) {}
}

/// Concrete base renderer data, shared by all renderer implementations.
pub struct RendererBaseData {
    pub current_fps: f32,
    pub current_frame: i32,
    pub settings: RendererSettings,
}

impl RendererBaseData {
    pub fn new() -> Self {
        Self {
            current_fps: 0.0,
            current_frame: 0,
            settings: RendererSettings::default(),
        }
    }

    /// Returns true if a screenshot is being processed.
    pub fn is_screenshot_pending(&self) -> bool {
        self.settings.screenshot_requested.load(Ordering::Relaxed)
    }

    /// Request a screenshot of the next frame.
    pub fn request_screenshot(
        &mut self,
        data: *mut std::ffi::c_void,
        callback: Box<dyn FnOnce(bool) + Send>,
        layout: FramebufferLayout,
    ) {
        if self.is_screenshot_pending() {
            log::error!("A screenshot is already requested or in progress, ignoring the request");
            return;
        }
        self.settings.screenshot_bits = data;
        self.settings.screenshot_complete_callback = Some(callback);
        self.settings.screenshot_framebuffer_layout = layout;
        self.settings
            .screenshot_requested
            .store(true, Ordering::Relaxed);
    }
}

impl Default for RendererBaseData {
    fn default() -> Self {
        Self::new()
    }
}
