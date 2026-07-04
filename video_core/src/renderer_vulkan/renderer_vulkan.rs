// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `renderer_vulkan.h` / `renderer_vulkan.cpp`.
//!
//! Top-level Vulkan renderer that owns the device, swapchain, present manager,
//! blit screens, rasterizer, and optional turbo mode.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};

use ash::vk;

use crate::framebuffer_config::FramebufferConfig;
use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
use crate::host1x::syncpoint_manager::SyncpointManager;
use crate::present::{PRESENT_FILTERS_FOR_APPLET_CAPTURE, PRESENT_FILTERS_FOR_DISPLAY};
use crate::rasterizer_interface::RasterizerInterface;
use crate::renderer_base::{RendererBase, RendererBaseData};
use crate::textures::decoders;
use crate::vulkan_common::vulkan_device::Device;
use crate::vulkan_common::vulkan_instance;
use crate::vulkan_common::vulkan_library;
use crate::vulkan_common::vulkan_memory_allocator::{MappedBuffer, MemoryAllocator, MemoryUsage};
use crate::vulkan_common::vulkan_surface;
use crate::vulkan_common::vulkan_wrapper::{Instance, VulkanError};
use common::telemetry::{FieldType, FieldValue};
use ruzu_core::frontend::framebuffer_layout::{default_frame_layout, FramebufferLayout, Rectangle};
use ruzu_core::telemetry_session::TelemetrySession;

use super::blit_screen::{BlitFrame, BlitScreen};
use super::present::util::{create_wrapped_image, create_wrapped_image_view, download_color_image};
use super::present_manager::{Frame, PresentManager};
use super::scheduler::Scheduler;
use super::swapchain::Swapchain;

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

fn build_driver_name(vendor_name: &str, driver_version: &str) -> String {
    if vendor_name.is_empty() {
        driver_version.to_string()
    } else if driver_version.is_empty() {
        vendor_name.to_string()
    } else {
        format!("{} {}", vendor_name, driver_version)
    }
}

fn bytes_to_gib(bytes: u64) -> f64 {
    bytes as f64 / 1024.0 / 1024.0 / 1024.0
}

/// Port of `CanBlitToSwapchain`.
fn can_blit_to_swapchain(device: &Device, format: vk::Format) -> bool {
    let props = device.format_properties(format);
    props
        .optimal_tiling_features
        .contains(vk::FormatFeatureFlags::BLIT_DST)
}

fn add_report_telemetry_fields(
    telemetry_session: &mut TelemetrySession,
    vendor_name: &str,
    model_name: &str,
    driver_name: &str,
    api_version: &str,
    extensions: &str,
) {
    let field = FieldType::UserSystem;
    telemetry_session.add_field(
        field,
        "GPU_Vendor",
        FieldValue::String(vendor_name.to_string()),
    );
    telemetry_session.add_field(
        field,
        "GPU_Model",
        FieldValue::String(model_name.to_string()),
    );
    telemetry_session.add_field(
        field,
        "GPU_Vulkan_Driver",
        FieldValue::String(driver_name.to_string()),
    );
    telemetry_session.add_field(
        field,
        "GPU_Vulkan_Version",
        FieldValue::String(api_version.to_string()),
    );
    telemetry_session.add_field(
        field,
        "GPU_Vulkan_Extensions",
        FieldValue::String(extensions.to_string()),
    );
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
    /// Vulkan instance owner.
    instance: Instance,
    debug_messenger: vk::DebugUtilsMessengerEXT,
    surface_loader: ash::extensions::khr::Surface,
    /// Surface for presentation.
    surface: vk::SurfaceKHR,
    /// Shared Tegra device memory manager used by presentation uploads.
    device_memory: Arc<MaxwellDeviceMemoryManager>,
    /// Frontend visibility state used for upstream `render_window.IsShown()`.
    window_shown: Arc<AtomicBool>,
    /// Frontend framebuffer layout used for upstream `render_window.GetFramebufferLayout()`.
    framebuffer_layout: Arc<RwLock<FramebufferLayout>>,
    /// Callback for upstream `render_window.OnFrameDisplayed()`.
    frame_displayed_notify: Arc<dyn Fn() + Send + Sync>,
    /// Callback for upstream `gpu.RendererFrameEndNotify()`.
    frame_end_notify: Arc<dyn Fn() + Send + Sync>,
    /// Physical/logical Vulkan device owner.
    device: Device,
    /// Presentation swapchain owner.
    /// Shared with the present thread (upstream `Swapchain& swapchain` used
    /// from `PresentThread` under `swapchain_mutex`).
    swapchain: std::sync::Arc<std::sync::Mutex<Swapchain>>,
    /// Presentation frame manager.
    present_manager: PresentManager,
    /// Presentation command scheduler.
    scheduler: Scheduler,
    /// Swapchain blit/composition owner.
    blit_swapchain: BlitScreen,
    /// Screenshot/capture blit/composition owner.
    blit_capture: BlitScreen,
    /// Applet capture blit/composition owner.
    blit_applet: BlitScreen,
    /// Vulkan rasterizer owner.
    rasterizer: super::RasterizerVulkan,
    /// Vulkan memory allocator owner.
    ///
    /// Declared after `rasterizer` so Rust drops the rasterizer first. This
    /// mirrors upstream C++ field destruction, where `RasterizerVulkan`
    /// references `RendererVulkan::memory_allocator`.
    memory_allocator: Box<MemoryAllocator>,
    /// RendererBase shared state.
    base_data: RendererBaseData,
    /// Vulkan does not require a shared GL-style context.
    dummy_context: VulkanDummyContext,
    /// Applet capture frame.
    applet_frame: Frame,
    /// Whether turbo mode is enabled.
    turbo_mode_enabled: bool,
    /// One-shot diagnostic dump for the presented framebuffer.
    present_frame_dumped: bool,
}

unsafe impl Send for RendererVulkan {}

struct VulkanDummyContext;

impl ruzu_core::frontend::graphics_context::GraphicsContext for VulkanDummyContext {}

impl RendererVulkan {
    /// Port of `RendererVulkan::RendererVulkan`.
    ///
    /// In the full implementation, this constructor chain-initializes:
    /// library, instance (with debug if enabled), debug_messenger, surface,
    /// device (via CreateDevice), memory_allocator, state_tracker, scheduler,
    /// swapchain, present_manager, blit_swapchain, blit_capture, blit_applet,
    /// rasterizer, and optionally turbo_mode.
    pub fn new(
        telemetry_session: Option<&mut TelemetrySession>,
        window_info: &ruzu_core::frontend::emu_window::WindowSystemInfo,
        drawable_size: (u32, u32),
        window_shown: Arc<AtomicBool>,
        framebuffer_layout: Arc<RwLock<FramebufferLayout>>,
        frame_displayed_notify: Arc<dyn Fn() + Send + Sync>,
        frame_end_notify: Arc<dyn Fn() + Send + Sync>,
        syncpoints: Arc<SyncpointManager>,
        device_memory: Arc<MaxwellDeviceMemoryManager>,
    ) -> Result<Self, VulkanError> {
        let entry = vulkan_library::open_library()?;
        let window_type = map_window_type(window_info.type_)?;
        let instance = vulkan_instance::create_instance(
            entry,
            vk::API_VERSION_1_1,
            window_type,
            *common::settings::values().renderer_debug.get_value(),
        )?;
        let surface_info = vulkan_surface::WindowSystemInfo {
            window_type,
            display_connection: window_info.display_connection as *mut std::ffi::c_void,
            render_surface: window_info.render_surface as *mut std::ffi::c_void,
        };
        let surface = unsafe {
            vulkan_surface::create_surface(&instance.entry, &instance.instance, &surface_info)?
        };
        let surface_loader =
            ash::extensions::khr::Surface::new(&instance.entry, &instance.instance);

        let physical_device = select_physical_device(&instance.instance)?;
        let memory_properties = unsafe {
            instance
                .instance
                .get_physical_device_memory_properties(physical_device)
        };
        let physical_properties = unsafe {
            instance
                .instance
                .get_physical_device_properties(physical_device)
        };

        let device = create_device(&instance.instance, physical_device, surface)?;
        let mut memory_allocator = Box::new(MemoryAllocator::new(
            device.get_logical().clone(),
            memory_properties,
            physical_properties.limits.buffer_image_granularity,
            false,
        ));
        let scheduler_command_pool = unsafe {
            let pool_info = vk::CommandPoolCreateInfo::builder()
                .queue_family_index(device.get_graphics_family())
                .flags(vk::CommandPoolCreateFlags::RESET_COMMAND_BUFFER)
                .build();
            device
                .get_logical()
                .create_command_pool(&pool_info, None)
                .map_err(VulkanError::new)?
        };
        let scheduler = Scheduler::new(
            device.get_logical().clone(),
            device.get_graphics_queue(),
            scheduler_command_pool,
            device.is_timeline_semaphore_supported(),
        )
        .map_err(VulkanError::new)?;
        let submit_mutex = scheduler.submit_mutex();
        let swapchain = Swapchain::new(
            &instance.instance,
            surface_loader.clone(),
            surface,
            &device,
            submit_mutex.clone(),
            drawable_size.0.max(1),
            drawable_size.1.max(1),
        )?;
        let blit_supported = can_blit_to_swapchain(&device, swapchain.get_image_view_format());
        let frame_image_format = swapchain.get_image_format();
        let swapchain_image_count = swapchain.get_image_count();
        let swapchain = std::sync::Arc::new(std::sync::Mutex::new(swapchain));
        // Upstream gates the present thread on `Settings::values.async_presentation`.
        // On macOS/MoltenVK a blocked `nextDrawable` inside the present copy
        // must never stall the GPU thread (it wedges the whole emulator), so
        // ruzu defaults the setting to on.
        let use_present_thread = *common::settings::values().async_presentation.get_value();
        let present_manager = PresentManager::new(
            device.get_logical().clone(),
            memory_properties,
            frame_image_format,
            device.get_graphics_family(),
            swapchain_image_count,
            blit_supported,
            use_present_thread,
            submit_mutex,
            std::sync::Arc::clone(&swapchain),
            device.get_graphics_queue(),
        );
        let supports_float16 = device.is_float16_supported();
        let blit_swapchain = BlitScreen::new(
            device.get_logical().clone(),
            &PRESENT_FILTERS_FOR_DISPLAY,
            supports_float16,
        );
        let blit_capture = BlitScreen::new(
            device.get_logical().clone(),
            &PRESENT_FILTERS_FOR_DISPLAY,
            supports_float16,
        );
        let blit_applet = BlitScreen::new(
            device.get_logical().clone(),
            &PRESENT_FILTERS_FOR_APPLET_CAPTURE,
            supports_float16,
        );
        let rasterizer = super::RasterizerVulkan::new(
            instance.instance.clone(),
            device.get_physical(),
            device.get_logical().clone(),
            device.get_graphics_queue(),
            device.get_graphics_family(),
            CAPTURE_IMAGE_WIDTH,
            CAPTURE_IMAGE_HEIGHT,
            device.supported_spirv_version(),
            device.is_ext_extended_dynamic_state_supported(),
            device.is_ext_extended_dynamic_state2_supported(),
            device.is_topology_list_primitive_restart_supported(),
            device.is_patch_list_primitive_restart_supported(),
            device.is_ext_shader_stencil_export_supported(),
            device.is_timeline_semaphore_supported(),
            device.is_ext_custom_border_color_supported(),
            device.get_max_viewports(),
            syncpoints,
            Arc::clone(&device_memory),
            memory_allocator.as_mut(),
        )
        .map_err(|err| {
            log::error!("Failed to initialize Vulkan rasterizer: {}", err);
            VulkanError::new(vk::Result::ERROR_INITIALIZATION_FAILED)
        })?;

        let renderer = RendererVulkan {
            instance,
            debug_messenger: vk::DebugUtilsMessengerEXT::null(),
            surface_loader,
            surface,
            device_memory,
            window_shown,
            framebuffer_layout,
            frame_displayed_notify,
            frame_end_notify,
            device,
            swapchain,
            present_manager,
            scheduler,
            blit_swapchain,
            blit_capture,
            blit_applet,
            rasterizer,
            memory_allocator,
            base_data: RendererBaseData::new(),
            dummy_context: VulkanDummyContext,
            applet_frame: Frame::default(),
            turbo_mode_enabled: false,
            present_frame_dumped: false,
        };
        renderer.report(telemetry_session);
        Ok(renderer)
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
    pub fn composite_impl(&mut self, framebuffers: &[FramebufferConfig]) {
        let trace_present = std::env::var_os("RUZU_TRACE_PRESENT").is_some();
        if trace_present {
            log::info!(
                "[PRESENT] RendererVulkan::Composite enter layers={} current_frame={} shown={}",
                framebuffers.len(),
                self.base_data.current_frame,
                self.window_shown.load(Ordering::Relaxed)
            );
        }
        let _frame_displayed = FrameDisplayedNotifyGuard::new(&self.frame_displayed_notify);
        if framebuffers.is_empty() {
            if trace_present {
                log::info!("[PRESENT] RendererVulkan::Composite skip empty layers");
            }
            return;
        }
        self.render_applet_capture_layer(framebuffers);
        if !should_present_window(&self.window_shown) {
            if trace_present {
                log::info!("[PRESENT] RendererVulkan::Composite skip hidden window");
            }
            return;
        }
        let layout = self.current_framebuffer_layout_for_present(trace_present);
        self.render_screenshot(framebuffers);
        self.dump_present_frame_if_requested(framebuffers, &layout);

        let frame_index = self.present_manager.get_render_frame_index();
        let swapchain_extent = self.swapchain.lock().unwrap().get_extent();
        if trace_present {
            log::info!(
                "[PRESENT] RendererVulkan::Composite draw frame_index={} swapchain={}x{} layout={}x{} image_count={}",
                frame_index,
                swapchain_extent.width,
                swapchain_extent.height,
                layout.width,
                layout.height,
                self.swapchain.lock().unwrap().get_image_count()
            );
        }
        let (swapchain_image_count, swapchain_image_view_format) = {
            let swapchain = self.swapchain.lock().unwrap();
            (
                swapchain.get_image_count(),
                swapchain.get_image_view_format(),
            )
        };
        self.blit_swapchain.draw_to_present_frame(
            &mut self.rasterizer,
            &mut self.scheduler,
            &mut self.present_manager,
            &self.memory_allocator,
            &self.device_memory,
            frame_index,
            framebuffers,
            &layout,
            swapchain_image_count,
            swapchain_image_view_format,
        );

        let render_ready = self.present_manager.frame(frame_index).render_ready;
        self.scheduler.flush_with_signal(render_ready);
        if trace_present {
            log::info!(
                "[PRESENT] RendererVulkan::Composite flushed frame_index={}",
                frame_index
            );
        }
        self.present_manager
            .present(frame_index, &mut self.scheduler);
        self.base_data.current_frame += 1;

        (self.frame_end_notify)();
        self.rasterizer.tick_frame();
        if trace_present {
            log::info!(
                "[PRESENT] RendererVulkan::Composite exit frame_index={} current_frame={}",
                frame_index,
                self.base_data.current_frame
            );
        }
    }

    /// Port of `RendererVulkan::GetAppletCaptureBuffer`.
    ///
    /// Downloads the applet capture image from GPU to CPU and returns the
    /// pixel data as a byte vector.
    pub fn get_applet_capture_buffer(&mut self) -> Vec<u8> {
        let mut out = vec![0; crate::capture::tiled_size() as usize];

        if self.applet_frame.image == vk::Image::null() {
            return out;
        }

        let dst_buffer =
            self.create_download_buffer(crate::capture::tiled_size() as vk::DeviceSize);
        self.scheduler.request_outside_renderpass();
        let device = self.device.get_logical().clone();
        let image = self.applet_frame.image;
        let buffer = dst_buffer.buffer();
        self.scheduler.record(move |cmdbuf| {
            download_color_image(&device, cmdbuf, image, buffer, CAPTURE_IMAGE_EXTENT);
        });
        self.scheduler.finish();
        dst_buffer.invalidate();
        decoders::swizzle_texture(
            &mut out,
            dst_buffer.mapped_slice(),
            crate::capture::BYTES_PER_PIXEL,
            crate::capture::LINEAR_WIDTH,
            crate::capture::LINEAR_HEIGHT,
            crate::capture::LINEAR_DEPTH,
            crate::capture::BLOCK_HEIGHT,
            crate::capture::BLOCK_DEPTH,
            0,
        );
        out
    }

    /// Port of `RendererVulkan::GetDeviceVendor`.
    pub fn get_device_vendor(&self) -> String {
        self.device.get_driver_name()
    }

    /// Port of `RendererVulkan::Report`.
    ///
    /// Logs device information and submits the same telemetry fields as upstream.
    fn report(&self, telemetry_session: Option<&mut TelemetrySession>) {
        let vendor_name = self.device.get_vendor_name();
        let model_name = self.device.get_model_name();
        let driver_version = get_driver_version(
            self.device.get_driver_id(),
            self.device.get_driver_version(),
        );
        let driver_name = build_driver_name(&vendor_name, &driver_version);
        let api_version = get_readable_version(self.device.api_version());
        let extensions = self
            .device
            .get_available_extensions()
            .iter()
            .cloned()
            .collect::<Vec<_>>();
        let extensions = build_comma_separated_extensions(&extensions);
        let available_vram = bytes_to_gib(self.device.get_device_local_memory());

        log::info!("Driver: {}", driver_name);
        log::info!("Device: {}", model_name);
        log::info!("Vulkan: {}", api_version);
        log::info!("Available VRAM: {:.2} GiB", available_vram);
        log::debug!("Vulkan extensions: {}", extensions);

        if let Some(telemetry_session) = telemetry_session {
            add_report_telemetry_fields(
                telemetry_session,
                &vendor_name,
                &model_name,
                &driver_name,
                &api_version,
                &extensions,
            );
        }
    }

    /// Port of `RendererVulkan::RenderToBuffer`.
    ///
    /// Creates a temporary frame, draws framebuffers to it via blit_capture,
    /// and copies the result to a buffer for readback.
    fn render_to_buffer(
        &mut self,
        framebuffers: &[FramebufferConfig],
        layout: &FramebufferLayout,
        format: vk::Format,
        buffer_size: vk::DeviceSize,
    ) -> MappedBuffer {
        let mut frame = Frame::default();
        frame.width = layout.width;
        frame.height = layout.height;
        frame.image = create_wrapped_image(
            self.device.get_logical(),
            &self.memory_allocator,
            vk::Extent2D {
                width: layout.width,
                height: layout.height,
            },
            format,
        );
        frame.image_view =
            create_wrapped_image_view(self.device.get_logical(), frame.image, format);
        frame.framebuffer = self.blit_capture.create_framebuffer(
            &mut self.scheduler,
            &self.present_manager,
            frame.image_view,
            format,
            layout.width,
            layout.height,
        );

        let dst_buffer = self.create_download_buffer(buffer_size);
        self.blit_capture.draw_to_frame(
            &mut self.rasterizer,
            &mut self.scheduler,
            &self.present_manager,
            &self.memory_allocator,
            &self.device_memory,
            BlitFrame::from(&frame),
            framebuffers,
            layout,
            1,
            format,
        );

        self.scheduler.request_outside_renderpass();
        let device = self.device.get_logical().clone();
        let image = frame.image;
        let buffer = dst_buffer.buffer();
        let extent = vk::Extent3D {
            width: layout.width,
            height: layout.height,
            depth: 1,
        };
        self.scheduler.record(move |cmdbuf| {
            download_color_image(&device, cmdbuf, image, buffer, extent);
        });
        self.scheduler.finish();
        dst_buffer.invalidate();

        unsafe {
            if frame.framebuffer != vk::Framebuffer::null() {
                self.device
                    .get_logical()
                    .destroy_framebuffer(frame.framebuffer, None);
            }
            if frame.image_view != vk::ImageView::null() {
                self.device
                    .get_logical()
                    .destroy_image_view(frame.image_view, None);
            }
        }
        dst_buffer
    }

    /// Port of `RendererVulkan::RenderScreenshot`.
    ///
    /// If a screenshot is requested, renders to a buffer at the appropriate
    /// resolution and format, then saves or delivers the result.
    fn render_screenshot(&mut self, framebuffers: &[FramebufferConfig]) {
        if !self.base_data.is_screenshot_pending() {
            return;
        }

        let screenshot_layout = self
            .base_data
            .settings
            .screenshot_framebuffer_layout
            .clone();
        let layout = FramebufferLayout {
            width: screenshot_layout.width,
            height: screenshot_layout.height,
            screen: Rectangle::new(0, 0, screenshot_layout.width, screenshot_layout.height),
            is_srgb: false,
        };
        let buffer_size = layout.width as vk::DeviceSize * layout.height as vk::DeviceSize * 4;
        let dst_buffer = self.render_to_buffer(
            framebuffers,
            &layout,
            vk::Format::B8G8R8A8_UNORM,
            buffer_size,
        );
        let dst = self.base_data.settings.screenshot_bits.cast::<u8>();
        if !dst.is_null() {
            let copy_len = buffer_size as usize;
            unsafe {
                std::ptr::copy_nonoverlapping(dst_buffer.mapped_slice().as_ptr(), dst, copy_len);
            }
        }
        if let Some(callback) = self.base_data.settings.screenshot_complete_callback.take() {
            callback(false);
        }
        self.base_data
            .settings
            .screenshot_requested
            .store(false, Ordering::Relaxed);
    }

    /// Diagnostic-only capture of the presented framebuffer.
    ///
    /// This intentionally reuses the same owner and readback path as upstream
    /// `RendererVulkan::RenderToBuffer`; it is gated by environment variables
    /// and does not affect normal presentation.
    fn dump_present_frame_if_requested(
        &mut self,
        framebuffers: &[FramebufferConfig],
        layout: &FramebufferLayout,
    ) {
        if self.present_frame_dumped {
            return;
        }
        let Some(path) = std::env::var_os("RUZU_DUMP_PRESENT_FRAME") else {
            return;
        };
        let target_frame = std::env::var("RUZU_DUMP_PRESENT_FRAME_AT")
            .ok()
            .and_then(|value| value.parse::<i32>().ok())
            .unwrap_or(300);
        if self.base_data.current_frame < target_frame {
            return;
        }

        let buffer_size = layout.width as vk::DeviceSize * layout.height as vk::DeviceSize * 4;
        let dst_buffer = self.render_to_buffer(
            framebuffers,
            layout,
            vk::Format::B8G8R8A8_UNORM,
            buffer_size,
        );
        let path = std::path::PathBuf::from(path);
        match write_bgra_ppm(
            &path,
            dst_buffer.mapped_slice(),
            layout.width,
            layout.height,
        ) {
            Ok(()) => {
                self.present_frame_dumped = true;
                log::info!(
                    "[PRESENT] dumped presented frame to {} at frame {}",
                    path.display(),
                    self.base_data.current_frame
                );
            }
            Err(err) => {
                self.present_frame_dumped = true;
                log::error!(
                    "[PRESENT] failed to dump presented frame to {}: {}",
                    path.display(),
                    err
                );
            }
        }
    }

    /// Port of `RendererVulkan::RenderAppletCaptureLayer`.
    ///
    /// Renders framebuffers to the applet capture frame at 1280x720
    /// using the applet-specific blit screen and filter configuration.
    fn render_applet_capture_layer(&mut self, framebuffers: &[FramebufferConfig]) {
        if self.applet_frame.image == vk::Image::null() {
            self.applet_frame.width = CAPTURE_IMAGE_WIDTH;
            self.applet_frame.height = CAPTURE_IMAGE_HEIGHT;
            self.applet_frame.image = create_wrapped_image(
                self.device.get_logical(),
                &self.memory_allocator,
                CAPTURE_IMAGE_SIZE,
                CAPTURE_FORMAT,
            );
            self.applet_frame.image_view = create_wrapped_image_view(
                self.device.get_logical(),
                self.applet_frame.image,
                CAPTURE_FORMAT,
            );
            self.applet_frame.framebuffer = self.blit_applet.create_framebuffer(
                &mut self.scheduler,
                &self.present_manager,
                self.applet_frame.image_view,
                CAPTURE_FORMAT,
                CAPTURE_IMAGE_WIDTH,
                CAPTURE_IMAGE_HEIGHT,
            );
        }

        let layout = capture_framebuffer_layout();
        let frame = BlitFrame::from(&self.applet_frame);
        self.blit_applet.draw_to_frame(
            &mut self.rasterizer,
            &mut self.scheduler,
            &self.present_manager,
            &self.memory_allocator,
            &self.device_memory,
            frame,
            framebuffers,
            &layout,
            1,
            CAPTURE_FORMAT,
        );
    }

    fn create_download_buffer(&self, size: vk::DeviceSize) -> MappedBuffer {
        let ci = vk::BufferCreateInfo::builder()
            .size(size.max(1))
            .usage(vk::BufferUsageFlags::TRANSFER_SRC | vk::BufferUsageFlags::TRANSFER_DST)
            .sharing_mode(vk::SharingMode::EXCLUSIVE)
            .build();
        self.memory_allocator
            .create_mapped_buffer(&ci, MemoryUsage::Download)
            .expect("Failed to create Vulkan download buffer")
    }

    /// Keep the present layout synchronized with the fixed WSI surface extent.
    ///
    /// Upstream relies on `EmuWindow_SDL2::OnResize` updating
    /// `render_window.GetFramebufferLayout()` before `RendererVulkan::Composite`.
    /// On macOS/MoltenVK, SDL resize events can lag behind the surface extent
    /// visible to Vulkan during live resize/maximize, so synchronize here before
    /// `BlitScreen::DrawToFrame` recreates the presentation frame.
    fn current_framebuffer_layout_for_present(&self, trace_present: bool) -> FramebufferLayout {
        let layout = self.framebuffer_layout.read().unwrap().clone();
        let Some(extent) = self.swapchain.lock().unwrap().current_surface_extent() else {
            return layout;
        };
        if extent.width == layout.width && extent.height == layout.height {
            return layout;
        }

        let updated = default_frame_layout(extent.width, extent.height);
        if trace_present {
            log::info!(
                "[PRESENT] RendererVulkan::Composite sync layout from surface old={}x{} new={}x{} screen={}x{}",
                layout.width,
                layout.height,
                updated.width,
                updated.height,
                updated.screen.get_width(),
                updated.screen.get_height()
            );
        }
        *self.framebuffer_layout.write().unwrap() = updated.clone();
        updated
    }
}

impl Drop for RendererVulkan {
    /// Port of `RendererVulkan::~RendererVulkan`.
    ///
    /// Upstream: clears the scheduler on-submit callback, then waits for
    /// the device to become idle before destruction.
    fn drop(&mut self) {
        unsafe {
            self.device.get_logical().device_wait_idle().ok();
            if self.applet_frame.framebuffer != vk::Framebuffer::null() {
                self.device
                    .get_logical()
                    .destroy_framebuffer(self.applet_frame.framebuffer, None);
                self.applet_frame.framebuffer = vk::Framebuffer::null();
            }
            if self.applet_frame.image_view != vk::ImageView::null() {
                self.device
                    .get_logical()
                    .destroy_image_view(self.applet_frame.image_view, None);
                self.applet_frame.image_view = vk::ImageView::null();
            }
            if self.surface != vk::SurfaceKHR::null() {
                self.surface_loader.destroy_surface(self.surface, None);
                self.surface = vk::SurfaceKHR::null();
            }
        }
    }
}

impl RendererBase for RendererVulkan {
    fn context_ptr(&mut self) -> *mut dyn ruzu_core::frontend::graphics_context::GraphicsContext {
        &mut self.dummy_context as *mut dyn ruzu_core::frontend::graphics_context::GraphicsContext
    }

    fn composite(&mut self, layers: &[FramebufferConfig]) {
        self.composite_impl(layers);
    }

    fn get_applet_capture_buffer(&mut self) -> Vec<u8> {
        RendererVulkan::get_applet_capture_buffer(self)
    }

    fn read_rasterizer(&self) -> *mut dyn RasterizerInterface {
        let trait_ref: &dyn RasterizerInterface = &self.rasterizer;
        trait_ref as *const dyn RasterizerInterface as *mut dyn RasterizerInterface
    }

    fn get_device_vendor(&self) -> String {
        RendererVulkan::get_device_vendor(self)
    }

    fn current_fps(&self) -> f32 {
        self.base_data.current_fps
    }

    fn current_frame(&self) -> i32 {
        self.base_data.current_frame
    }

    fn refresh_base_settings(&mut self) {}

    fn is_screenshot_pending(&self) -> bool {
        self.base_data.is_screenshot_pending()
    }

    fn set_guest_memory_writer(&mut self, writer: crate::renderer_base::GuestMemoryWriter) {
        self.rasterizer.set_guest_memory_writer(writer);
    }

    fn set_gpu_ticks_getter(&mut self, getter: crate::renderer_base::GpuTicksGetter) {
        self.rasterizer.set_gpu_ticks_getter(getter);
    }
}

fn map_window_type(
    window_type: ruzu_core::frontend::emu_window::WindowSystemType,
) -> Result<vulkan_instance::WindowSystemType, VulkanError> {
    match window_type {
        ruzu_core::frontend::emu_window::WindowSystemType::Headless => {
            Ok(vulkan_instance::WindowSystemType::Headless)
        }
        #[cfg(target_os = "linux")]
        ruzu_core::frontend::emu_window::WindowSystemType::X11 => {
            Ok(vulkan_instance::WindowSystemType::X11)
        }
        #[cfg(target_os = "linux")]
        ruzu_core::frontend::emu_window::WindowSystemType::Wayland => {
            Ok(vulkan_instance::WindowSystemType::Wayland)
        }
        #[cfg(target_os = "macos")]
        ruzu_core::frontend::emu_window::WindowSystemType::Cocoa => {
            Ok(vulkan_instance::WindowSystemType::Cocoa)
        }
        #[cfg(target_os = "windows")]
        ruzu_core::frontend::emu_window::WindowSystemType::Windows => {
            Ok(vulkan_instance::WindowSystemType::Windows)
        }
        #[cfg(target_os = "android")]
        ruzu_core::frontend::emu_window::WindowSystemType::Android => {
            Ok(vulkan_instance::WindowSystemType::Android)
        }
        _ => {
            log::error!("Unsupported Vulkan window system: {:?}", window_type);
            Err(VulkanError::new(vk::Result::ERROR_INITIALIZATION_FAILED))
        }
    }
}

fn capture_framebuffer_layout() -> FramebufferLayout {
    FramebufferLayout {
        width: crate::capture::LINEAR_WIDTH,
        height: crate::capture::LINEAR_HEIGHT,
        screen: Rectangle::new(
            0,
            0,
            crate::capture::LINEAR_WIDTH,
            crate::capture::LINEAR_HEIGHT,
        ),
        is_srgb: false,
    }
}

fn should_present_window(window_shown: &AtomicBool) -> bool {
    window_shown.load(Ordering::Relaxed)
}

fn write_bgra_ppm(
    path: &std::path::Path,
    bgra: &[u8],
    width: u32,
    height: u32,
) -> std::io::Result<()> {
    use std::io::Write;

    let pixel_count = width as usize * height as usize;
    let required_len = pixel_count * 4;
    if bgra.len() < required_len {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "BGRA buffer is smaller than framebuffer dimensions",
        ));
    }

    let mut output =
        Vec::with_capacity(format!("P6\n{} {}\n255\n", width, height).len() + pixel_count * 3);
    write!(&mut output, "P6\n{} {}\n255\n", width, height)?;
    for pixel in bgra[..required_len].chunks_exact(4) {
        output.push(pixel[2]);
        output.push(pixel[1]);
        output.push(pixel[0]);
    }
    std::fs::write(path, output)
}

struct FrameDisplayedNotifyGuard {
    notify: Arc<dyn Fn() + Send + Sync>,
}

impl FrameDisplayedNotifyGuard {
    fn new(notify: &Arc<dyn Fn() + Send + Sync>) -> Self {
        Self {
            notify: Arc::clone(notify),
        }
    }
}

impl Drop for FrameDisplayedNotifyGuard {
    fn drop(&mut self) {
        (self.notify.as_ref())();
    }
}

/// Port of free function `CreateDevice`.
fn create_device(
    instance: &ash::Instance,
    physical_device: vk::PhysicalDevice,
    surface: vk::SurfaceKHR,
) -> Result<Device, VulkanError> {
    Device::new(instance.clone(), physical_device, surface)
}

/// Port of the physical-device selection portion of upstream `CreateDevice`.
fn select_physical_device(instance: &ash::Instance) -> Result<vk::PhysicalDevice, VulkanError> {
    let devices = unsafe {
        instance
            .enumerate_physical_devices()
            .map_err(VulkanError::new)?
    };
    let device_index = *common::settings::values().vulkan_device.get_value();
    let selected_index = validate_physical_device_index(device_index, devices.len())?;
    Ok(devices[selected_index])
}

fn validate_physical_device_index(
    device_index: i32,
    device_count: usize,
) -> Result<usize, VulkanError> {
    if device_index < 0 || device_index as usize >= device_count {
        log::error!("Invalid Vulkan device index {}!", device_index);
        return Err(VulkanError::new(vk::Result::ERROR_INITIALIZATION_FAILED));
    }
    Ok(device_index as usize)
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::telemetry::{Field, VisitorInterface};
    use std::collections::BTreeMap;

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
    fn physical_device_index_validation_matches_upstream_bounds() {
        assert_eq!(validate_physical_device_index(0, 1).unwrap(), 0);
        assert_eq!(validate_physical_device_index(1, 2).unwrap(), 1);
        assert!(validate_physical_device_index(-1, 2).is_err());
        assert!(validate_physical_device_index(2, 2).is_err());
        assert!(validate_physical_device_index(0, 0).is_err());
    }

    #[test]
    fn window_visibility_gate_matches_composite_branch() {
        let shown = AtomicBool::new(true);
        assert!(should_present_window(&shown));
        shown.store(false, Ordering::Relaxed);
        assert!(!should_present_window(&shown));
    }

    #[test]
    fn frame_displayed_notify_guard_matches_scope_exit() {
        let calls = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let calls_clone = Arc::clone(&calls);
        let notify: Arc<dyn Fn() + Send + Sync> = Arc::new(move || {
            calls_clone.fetch_add(1, Ordering::Relaxed);
        });

        {
            let _guard = FrameDisplayedNotifyGuard::new(&notify);
            assert_eq!(calls.load(Ordering::Relaxed), 0);
        }

        assert_eq!(calls.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn write_bgra_ppm_converts_to_rgb() {
        let path =
            std::env::temp_dir().join(format!("ruzu-test-present-dump-{}.ppm", std::process::id()));
        let bgra = [0x03, 0x02, 0x01, 0xFF, 0x30, 0x20, 0x10, 0x80];
        write_bgra_ppm(&path, &bgra, 2, 1).unwrap();
        let data = std::fs::read(&path).unwrap();
        let _ = std::fs::remove_file(&path);
        assert_eq!(&data[..11], b"P6\n2 1\n255\n");
        assert_eq!(&data[11..], &[0x01, 0x02, 0x03, 0x10, 0x20, 0x30]);
    }

    #[test]
    fn driver_name_builder_matches_report_format() {
        assert_eq!(
            build_driver_name("NVIDIA", "525.60.11.0"),
            "NVIDIA 525.60.11.0"
        );
        assert_eq!(build_driver_name("", "1.2.3"), "1.2.3");
        assert_eq!(build_driver_name("MoltenVK", ""), "MoltenVK");
    }

    #[test]
    fn bytes_to_gib_matches_upstream_report_units() {
        assert_eq!(bytes_to_gib(0), 0.0);
        assert_eq!(bytes_to_gib(1024 * 1024 * 1024), 1.0);
        assert_eq!(bytes_to_gib(5 * 1024 * 1024 * 1024), 5.0);
    }

    #[test]
    fn report_telemetry_fields_match_upstream_names() {
        struct Collector(BTreeMap<String, FieldValue>);

        impl VisitorInterface for Collector {
            fn visit(&mut self, field: &Field) {
                self.0
                    .insert(field.get_name().to_string(), field.get_value().clone());
            }

            fn complete(&mut self) {}

            fn submit_testcase(&mut self) -> bool {
                false
            }
        }

        let mut session = TelemetrySession::new();
        add_report_telemetry_fields(
            &mut session,
            "MoltenVK",
            "Apple M2 Pro",
            "MoltenVK 1.2.3",
            "1.3.250",
            "VK_KHR_swapchain,VK_EXT_memory_budget",
        );
        let mut collector = Collector(BTreeMap::new());
        session.field_collection().accept(&mut collector);

        assert_eq!(
            collector.0.get("GPU_Vendor"),
            Some(&FieldValue::String("MoltenVK".to_string()))
        );
        assert_eq!(
            collector.0.get("GPU_Model"),
            Some(&FieldValue::String("Apple M2 Pro".to_string()))
        );
        assert_eq!(
            collector.0.get("GPU_Vulkan_Driver"),
            Some(&FieldValue::String("MoltenVK 1.2.3".to_string()))
        );
        assert_eq!(
            collector.0.get("GPU_Vulkan_Version"),
            Some(&FieldValue::String("1.3.250".to_string()))
        );
        assert_eq!(
            collector.0.get("GPU_Vulkan_Extensions"),
            Some(&FieldValue::String(
                "VK_KHR_swapchain,VK_EXT_memory_budget".to_string()
            ))
        );
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
