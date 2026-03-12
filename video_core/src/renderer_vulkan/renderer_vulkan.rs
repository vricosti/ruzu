// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `renderer_vulkan.h` / `renderer_vulkan.cpp`.
//!
//! Top-level Vulkan renderer that owns the device, swapchain, present manager,
//! blit screens, rasterizer, and optional turbo mode.

use ash::vk;

// ---------------------------------------------------------------------------
// Constants (from renderer_vulkan.cpp anonymous namespace)
// ---------------------------------------------------------------------------

/// Capture image size used for applet capture.
/// Maps to `CaptureImageSize` in upstream.
pub const CAPTURE_IMAGE_WIDTH: u32 = 1280;
pub const CAPTURE_IMAGE_HEIGHT: u32 = 720;
pub const CAPTURE_IMAGE_DEPTH: u32 = 1;

/// Maps to `CaptureFormat` in upstream.
pub const CAPTURE_FORMAT: vk::Format = vk::Format::A8B8G8R8_UNORM_PACK32;

// ---------------------------------------------------------------------------
// Helper functions (from renderer_vulkan.cpp anonymous namespace)
// ---------------------------------------------------------------------------

/// Returns a human-readable Vulkan version string.
/// Port of `GetReadableVersion`.
pub fn get_readable_version(version: u32) -> String {
    format!(
        "{}.{}.{}",
        vk::api_version_major(version),
        vk::api_version_minor(version),
        vk::api_version_patch(version),
    )
}

/// Returns a driver-specific version string.
/// Port of `GetDriverVersion`.
pub fn get_driver_version(driver_id: vk::DriverId, version: u32) -> String {
    if driver_id == vk::DriverId::NVIDIA_PROPRIETARY {
        let major = (version >> 22) & 0x3ff;
        let minor = (version >> 14) & 0x0ff;
        let secondary = (version >> 6) & 0x0ff;
        let tertiary = version & 0x003f;
        return format!("{}.{}.{}.{}", major, minor, secondary, tertiary);
    }
    if driver_id == vk::DriverId::INTEL_PROPRIETARY_WINDOWS {
        let major = version >> 14;
        let minor = version & 0x3fff;
        return format!("{}.{}", major, minor);
    }
    get_readable_version(version)
}

/// Builds a comma-separated string of extensions.
/// Port of `BuildCommaSeparatedExtensions`.
pub fn build_comma_separated_extensions(extensions: &[String]) -> String {
    extensions.join(",")
}

// ---------------------------------------------------------------------------
// Frame (re-exported from present_manager, referenced here for completeness)
// ---------------------------------------------------------------------------

// Frame is defined in vk_present_manager.rs, matching upstream.

// ---------------------------------------------------------------------------
// RendererVulkan
// ---------------------------------------------------------------------------

/// Port of `RendererVulkan` class.
///
/// Owns the Vulkan instance, device, memory allocator, scheduler, swapchain,
/// present manager, blit screens (swapchain / capture / applet), rasterizer,
/// and optional turbo mode.
pub struct RendererVulkan {
    // NOTE: Full field set omitted; downstream crates do not yet provide
    // Device, MemoryAllocator, etc. Placeholder fields shown below.
    _private: (),
}

impl RendererVulkan {
    /// Port of `RendererVulkan::RendererVulkan`.
    pub fn new() -> Self {
        todo!("RendererVulkan::new — requires Device, MemoryAllocator, etc.")
    }

    /// Port of `RendererVulkan::Composite`.
    pub fn composite(&mut self, _framebuffers: &[()]) {
        todo!("RendererVulkan::composite")
    }

    /// Port of `RendererVulkan::GetAppletCaptureBuffer`.
    pub fn get_applet_capture_buffer(&self) -> Vec<u8> {
        todo!("RendererVulkan::get_applet_capture_buffer")
    }

    /// Port of `RendererVulkan::GetDeviceVendor`.
    pub fn get_device_vendor(&self) -> String {
        todo!("RendererVulkan::get_device_vendor")
    }

    /// Port of `RendererVulkan::Report`.
    fn report(&self) {
        todo!("RendererVulkan::report")
    }

    /// Port of `RendererVulkan::RenderToBuffer`.
    fn render_to_buffer(
        &mut self,
        _framebuffers: &[()],
        _layout: &(),
        _format: vk::Format,
        _buffer_size: vk::DeviceSize,
    ) {
        todo!("RendererVulkan::render_to_buffer")
    }

    /// Port of `RendererVulkan::RenderScreenshot`.
    fn render_screenshot(&mut self, _framebuffers: &[()]) {
        todo!("RendererVulkan::render_screenshot")
    }

    /// Port of `RendererVulkan::RenderAppletCaptureLayer`.
    fn render_applet_capture_layer(&mut self, _framebuffers: &[()]) {
        todo!("RendererVulkan::render_applet_capture_layer")
    }
}

impl Drop for RendererVulkan {
    /// Port of `RendererVulkan::~RendererVulkan`.
    fn drop(&mut self) {
        // Upstream: scheduler.RegisterOnSubmit([] {}); device.GetLogical().WaitIdle();
    }
}

/// Port of free function `CreateDevice`.
pub fn create_device(
    _instance: vk::Instance,
    _surface: vk::SurfaceKHR,
) {
    todo!("create_device")
}
