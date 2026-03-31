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

/// Capture image width used for applet capture (1280px).
/// Maps to `CaptureImageSize.width` in upstream.
pub const CAPTURE_IMAGE_WIDTH: u32 = 1280;

/// Capture image height used for applet capture (720px).
/// Maps to `CaptureImageSize.height` in upstream.
pub const CAPTURE_IMAGE_HEIGHT: u32 = 720;

/// Capture image depth (always 1).
/// Maps to `CaptureImageExtent.depth` in upstream.
pub const CAPTURE_IMAGE_DEPTH: u32 = 1;

/// Capture image format.
/// Maps to `CaptureFormat` in upstream.
pub const CAPTURE_FORMAT: vk::Format = vk::Format::A8B8G8R8_UNORM_PACK32;

/// Capture image size as a VkExtent2D.
pub const CAPTURE_IMAGE_SIZE: vk::Extent2D = vk::Extent2D {
    width: CAPTURE_IMAGE_WIDTH,
    height: CAPTURE_IMAGE_HEIGHT,
};

/// Capture image extent as a VkExtent3D.
pub const CAPTURE_IMAGE_EXTENT: vk::Extent3D = vk::Extent3D {
    width: CAPTURE_IMAGE_WIDTH,
    height: CAPTURE_IMAGE_HEIGHT,
    depth: CAPTURE_IMAGE_DEPTH,
};

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
///
/// Nvidia and Intel proprietary drivers encode version numbers differently
/// from the standard Vulkan versioning scheme.
pub fn get_driver_version(driver_id: vk::DriverId, version: u32) -> String {
    if driver_id == vk::DriverId::NVIDIA_PROPRIETARY {
        // Nvidia: 10.8.8.6 bit layout
        let major = (version >> 22) & 0x3ff;
        let minor = (version >> 14) & 0x0ff;
        let secondary = (version >> 6) & 0x0ff;
        let tertiary = version & 0x003f;
        return format!("{}.{}.{}.{}", major, minor, secondary, tertiary);
    }
    if driver_id == vk::DriverId::INTEL_PROPRIETARY_WINDOWS {
        // Intel Windows: 14.14 bit layout
        let major = version >> 14;
        let minor = version & 0x3fff;
        return format!("{}.{}", major, minor);
    }
    // Standard Vulkan version encoding
    get_readable_version(version)
}

/// Builds a comma-separated string of extensions.
/// Port of `BuildCommaSeparatedExtensions`.
pub fn build_comma_separated_extensions(extensions: &[String]) -> String {
    extensions.join(",")
}

// ---------------------------------------------------------------------------
// Frame
// ---------------------------------------------------------------------------

/// Port of `Frame` struct from `vk_present_manager.h`.
///
/// Represents a single presentation frame with all associated Vulkan objects.
/// Re-exported here since RendererVulkan owns an `applet_frame`.
#[derive(Default)]
pub struct Frame {
    pub image: vk::Image,
    pub image_view: vk::ImageView,
    pub framebuffer: vk::Framebuffer,
    pub render_ready: vk::Semaphore,
    pub present_done: vk::Fence,
    pub width: u32,
    pub height: u32,
    pub is_available: bool,
}

// ---------------------------------------------------------------------------
// RendererVulkan
// ---------------------------------------------------------------------------

/// Port of `RendererVulkan` class.
///
/// Owns the Vulkan instance, device, memory allocator, scheduler, swapchain,
/// present manager, blit screens (swapchain / capture / applet), rasterizer,
/// and optional turbo mode.
///
/// The full field set requires cross-crate types (Device, MemoryAllocator,
/// StateTracker, Scheduler, Swapchain, PresentManager, BlitScreen,
/// RasterizerVulkan, TurboMode) which are all declared in sibling modules.
pub struct RendererVulkan {
    /// Vulkan instance handle.
    instance: vk::Instance,
    /// Debug messenger (null if debugging is disabled).
    debug_messenger: vk::DebugUtilsMessengerEXT,
    /// Surface for presentation.
    surface: vk::SurfaceKHR,
    /// Applet capture frame.
    applet_frame: Frame,
    /// Whether turbo mode is enabled.
    turbo_mode_enabled: bool,
}

impl RendererVulkan {
    /// Port of `RendererVulkan::RendererVulkan`.
    ///
    /// In the full implementation, this constructor chain-initializes:
    /// library, instance (with debug if enabled), debug_messenger, surface,
    /// device (via CreateDevice), memory_allocator, state_tracker, scheduler,
    /// swapchain, present_manager, blit_swapchain, blit_capture, blit_applet,
    /// rasterizer, and optionally turbo_mode.
    pub fn new(
        instance: vk::Instance,
        surface: vk::SurfaceKHR,
        debug_messenger: vk::DebugUtilsMessengerEXT,
    ) -> Self {
        RendererVulkan {
            instance,
            debug_messenger,
            surface,
            applet_frame: Frame::default(),
            turbo_mode_enabled: false,
        }
    }

    /// Port of `RendererVulkan::Composite`.
    ///
    /// Renders the given framebuffers to the display. This is the main
    /// per-frame entry point called by the GPU thread.
    ///
    /// Upstream flow:
    /// 1. RenderAppletCaptureLayer
    /// 2. Early-return if window not shown
    /// 3. RenderScreenshot
    /// 4. Get render frame from present manager
    /// 5. Draw to frame via blit_swapchain
    /// 6. Flush scheduler with render_ready semaphore
    /// 7. Present frame
    /// 8. Notify GPU of frame end
    /// 9. Tick rasterizer frame
    pub fn composite(&mut self) {
        // Full implementation requires BlitScreen, PresentManager, Scheduler,
        // Swapchain, and RasterizerVulkan integration.
        self.render_applet_capture_layer();
    }

    /// Port of `RendererVulkan::GetAppletCaptureBuffer`.
    ///
    /// Downloads the applet capture image from GPU to CPU and returns the
    /// pixel data as a byte vector.
    pub fn get_applet_capture_buffer(&self) -> Vec<u8> {
        // Applet capture requires reading back the applet_frame image
        Vec::new()
    }

    /// Port of `RendererVulkan::GetDeviceVendor`.
    pub fn get_device_vendor(&self) -> String {
        String::from("Unknown")
    }

    /// Port of `RendererVulkan::Report`.
    ///
    /// Logs device information (vendor, model, Vulkan version, VRAM)
    /// and submits telemetry fields.
    fn report(&self) {
        log::info!("Vulkan renderer initialized");
    }

    /// Port of `RendererVulkan::RenderToBuffer`.
    ///
    /// Creates a temporary frame, draws framebuffers to it via blit_capture,
    /// and copies the result to a buffer for readback.
    fn render_to_buffer(&mut self, _format: vk::Format, _buffer_size: vk::DeviceSize) {
        // Requires full BlitScreen and Scheduler integration
    }

    /// Port of `RendererVulkan::RenderScreenshot`.
    ///
    /// If a screenshot is requested, renders to a buffer at the appropriate
    /// resolution and format, then saves or delivers the result.
    fn render_screenshot(&mut self) {
        // Requires Settings integration to check if screenshot requested
    }

    /// Port of `RendererVulkan::RenderAppletCaptureLayer`.
    ///
    /// Renders framebuffers to the applet capture frame at 1280x720
    /// using the applet-specific blit screen and filter configuration.
    fn render_applet_capture_layer(&mut self) {
        // Requires blit_applet and scheduler integration
    }
}

impl Drop for RendererVulkan {
    /// Port of `RendererVulkan::~RendererVulkan`.
    ///
    /// Upstream: clears the scheduler on-submit callback, then waits for
    /// the device to become idle before destruction.
    fn drop(&mut self) {
        // In full implementation:
        // scheduler.register_on_submit(|| {});
        // device.get_logical().wait_idle();
    }
}

/// Port of free function `CreateDevice`.
///
/// Enumerates physical devices, selects the one specified by user settings,
/// and creates a logical Device wrapper.
pub fn create_device(_instance: vk::Instance, _surface: vk::SurfaceKHR) {
    // Full implementation requires:
    // 1. instance.enumerate_physical_devices()
    // 2. Select device based on Settings::values.vulkan_device
    // 3. Create Device with the selected physical device
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn readable_version() {
        let version = vk::make_api_version(0, 1, 3, 250);
        let s = get_readable_version(version);
        assert_eq!(s, "1.3.250");
    }

    #[test]
    fn nvidia_driver_version() {
        // Nvidia version 525.60.11 encodes differently
        let s = get_driver_version(
            vk::DriverId::NVIDIA_PROPRIETARY,
            (525 << 22) | (60 << 14) | (11 << 6) | 0,
        );
        assert!(s.starts_with("525.60.11."));
    }

    #[test]
    fn standard_driver_version() {
        let version = vk::make_api_version(0, 23, 1, 4);
        let s = get_driver_version(vk::DriverId::MESA_RADV, version);
        assert_eq!(s, "23.1.4");
    }

    #[test]
    fn capture_constants() {
        assert_eq!(CAPTURE_IMAGE_WIDTH, 1280);
        assert_eq!(CAPTURE_IMAGE_HEIGHT, 720);
        assert_eq!(CAPTURE_FORMAT, vk::Format::A8B8G8R8_UNORM_PACK32);
    }

    #[test]
    fn comma_separated_extensions() {
        let exts = vec!["VK_KHR_swapchain".into(), "VK_EXT_debug_utils".into()];
        assert_eq!(
            build_comma_separated_extensions(&exts),
            "VK_KHR_swapchain,VK_EXT_debug_utils"
        );
    }
}
