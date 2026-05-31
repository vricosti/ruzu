// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/renderer_opengl.h and renderer_opengl.cpp
//! Status: EN COURS
//!
//! OpenGL GPU renderer — provides an alternative backend to Vulkan.
//!
//! # Architecture
//!
//! Matches zuyu's `RendererOpenGL` class hierarchy:
//! - [`Device`] — GPU capabilities query (gl_device)
//! - [`StateTracker`] — dirty-flag state management (gl_state_tracker)
//! - [`BlitScreen`] — framebuffer compositing to screen (gl_blit_screen)
//! - [`RendererOpenGL`] — main renderer orchestrator
//! - [`RasterizerOpenGL`] — GPU draw command processing (gl_rasterizer)
//!
//! Resource management wrappers in [`gl_resource_manager`].

pub mod blit_image;
pub mod gl_blit_screen;
pub mod gl_buffer_cache;
pub mod gl_buffer_cache_base;
pub mod gl_compute_pipeline;
pub mod gl_device;
pub mod gl_fence_manager;
pub mod gl_graphics_pipeline;
pub mod gl_query_cache;
pub mod gl_rasterizer;
pub mod gl_resource_manager;
pub mod gl_shader_cache;
pub mod gl_shader_context;
pub mod gl_shader_manager;
pub mod gl_shader_util;
pub mod gl_staging_buffer_pool;
pub mod gl_state_tracker;
pub mod gl_texture_cache;
pub mod gl_texture_cache_base;
pub mod maxwell_to_gl;
pub mod present;
pub mod renderer_opengl;
pub mod util_shaders;

use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Instant;

use log::{debug, info};
use thiserror::Error;

pub use gl_blit_screen::BlitScreen;
pub use gl_device::Device;
pub use gl_rasterizer::{dump_gl_draw_stall_profile, RasterizerOpenGL};
pub use gl_shader_cache::dump_shader_pipeline_stall_profile;
#[allow(unused_imports)]
pub use gl_state_tracker::StateTracker;

use crate::capture;
use crate::engines::maxwell_3d::DrawCall;
use crate::engines::Framebuffer;
use crate::framebuffer_config::FramebufferConfig;
use crate::host1x::syncpoint_manager::SyncpointManager;
use crate::rasterizer_interface::RasterizerInterface;
use crate::renderer_base::{RendererBase, RendererBaseData};
use gl_resource_manager::{OGLFramebuffer, OGLRenderbuffer};
use ruzu_core::frontend::framebuffer_layout::{
    default_frame_layout, FramebufferLayout, ScreenUndocked,
};
use ruzu_core::frontend::graphics_context::GraphicsContext;

static PRESENT_COUNT: AtomicU64 = AtomicU64::new(0);
static PRESENT_TOTAL_US: AtomicU64 = AtomicU64::new(0);
static PRESENT_MAX_US: AtomicU64 = AtomicU64::new(0);
static PRESENT_MAKE_CURRENT_US: AtomicU64 = AtomicU64::new(0);
static PRESENT_CAPTURE_US: AtomicU64 = AtomicU64::new(0);
static PRESENT_SCREENSHOT_US: AtomicU64 = AtomicU64::new(0);
static PRESENT_DRAW_SCREEN_US: AtomicU64 = AtomicU64::new(0);
static PRESENT_TICK_FRAME_US: AtomicU64 = AtomicU64::new(0);
static PRESENT_SWAP_BUFFERS_US: AtomicU64 = AtomicU64::new(0);
static PRESENT_PPM_DUMPED: AtomicBool = AtomicBool::new(false);

fn present_profile_enabled() -> bool {
    std::env::var_os("RUZU_PROFILE_PRESENT").is_some()
}

fn update_max(target: &AtomicU64, value: u64) {
    let mut current = target.load(Ordering::Relaxed);
    while value > current {
        match target.compare_exchange_weak(current, value, Ordering::Relaxed, Ordering::Relaxed) {
            Ok(_) => break,
            Err(next) => current = next,
        }
    }
}

fn elapsed_us(start: Instant) -> u64 {
    start.elapsed().as_micros().min(u128::from(u64::MAX)) as u64
}

fn dump_present_ppm_once(current_frame: u64, layout: &FramebufferLayout) {
    let Some(path) = std::env::var_os("RUZU_DUMP_PRESENT_PPM") else {
        return;
    };
    let target_frame = std::env::var("RUZU_DUMP_PRESENT_PPM_FRAME")
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
        .unwrap_or(0);
    if current_frame < target_frame
        || PRESENT_PPM_DUMPED.swap(true, Ordering::Relaxed)
        || layout.width == 0
        || layout.height == 0
    {
        return;
    }

    unsafe {
        let width = layout.width as usize;
        let height = layout.height as usize;
        let mut old_pack_buffer = 0;
        let mut old_pack_alignment = 0;
        let mut old_pack_row_length = 0;
        gl::GetIntegerv(gl::PIXEL_PACK_BUFFER_BINDING, &mut old_pack_buffer);
        gl::GetIntegerv(gl::PACK_ALIGNMENT, &mut old_pack_alignment);
        gl::GetIntegerv(gl::PACK_ROW_LENGTH, &mut old_pack_row_length);
        gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
        gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
        gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);

        let mut rgba = vec![0u8; width * height * 4];
        gl::ReadPixels(
            0,
            0,
            width as i32,
            height as i32,
            gl::RGBA,
            gl::UNSIGNED_BYTE,
            rgba.as_mut_ptr() as *mut _,
        );
        let gl_error = gl::GetError();

        gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pack_buffer as u32);
        gl::PixelStorei(gl::PACK_ALIGNMENT, old_pack_alignment);
        gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);

        let mut ppm = Vec::with_capacity(width * height * 3 + 64);
        ppm.extend_from_slice(format!("P6\n{} {}\n255\n", width, height).as_bytes());
        for row in (0..height).rev() {
            for px in rgba[row * width * 4..(row + 1) * width * 4].chunks_exact(4) {
                ppm.extend_from_slice(&px[..3]);
            }
        }
        match std::fs::write(&path, ppm) {
            Ok(()) => info!(
                "[PRESENT_PPM] wrote {} frame={} gl_error=0x{:X}",
                path.to_string_lossy(),
                current_frame,
                gl_error
            ),
            Err(err) => log::warn!(
                "[PRESENT_PPM] failed to write {}: {}",
                path.to_string_lossy(),
                err
            ),
        }
    }
}

pub fn dump_present_profile() {
    if !present_profile_enabled() {
        return;
    }
    let count = PRESENT_COUNT.load(Ordering::Relaxed);
    let total = PRESENT_TOTAL_US.load(Ordering::Relaxed);
    let avg = if count != 0 { total / count } else { 0 };
    eprintln!(
        "[PRESENT_PROFILE] count={} total_us={} avg_us={} max_us={} make_current_us={} capture_us={} screenshot_us={} draw_screen_us={} tick_frame_us={} swap_buffers_us={}",
        count,
        total,
        avg,
        PRESENT_MAX_US.load(Ordering::Relaxed),
        PRESENT_MAKE_CURRENT_US.load(Ordering::Relaxed),
        PRESENT_CAPTURE_US.load(Ordering::Relaxed),
        PRESENT_SCREENSHOT_US.load(Ordering::Relaxed),
        PRESENT_DRAW_SCREEN_US.load(Ordering::Relaxed),
        PRESENT_TICK_FRAME_US.load(Ordering::Relaxed),
        PRESENT_SWAP_BUFFERS_US.load(Ordering::Relaxed),
    );
}

#[derive(Debug, Error)]
pub enum OpenGLError {
    #[error("OpenGL initialization failed: {0}")]
    InitFailed(String),
    #[error("Shader compilation failed: {0}")]
    ShaderCompileFailed(String),
    #[error("Required GL extension missing: {0}")]
    MissingExtension(String),
}

/// Main OpenGL renderer, corresponding to zuyu's `RendererOpenGL`.
///
/// Owns the device info, state tracker, blit screen pipeline, rasterizer,
/// graphics context, and base renderer data.
pub struct RendererOpenGL {
    device: Device,
    blit_screen: BlitScreen,
    rasterizer: RasterizerOpenGL,
    /// Graphics context for swap buffers / make current.
    /// Upstream: `std::unique_ptr<Core::Frontend::GraphicsContext> context` in RendererBase.
    context: Box<dyn GraphicsContext + Send>,
    /// Common renderer state (frame count, FPS, screenshot settings).
    base_data: RendererBaseData,
    /// Screenshot framebuffer (created on demand).
    /// Upstream: `OGLFramebuffer screenshot_framebuffer`.
    screenshot_framebuffer: OGLFramebuffer,
    /// Applet capture framebuffer.
    /// Upstream: `OGLFramebuffer capture_framebuffer`.
    capture_framebuffer: OGLFramebuffer,
    /// Applet capture renderbuffer.
    /// Upstream: `OGLRenderbuffer capture_renderbuffer`.
    capture_renderbuffer: OGLRenderbuffer,
    /// Current framebuffer layout (window size + screen region).
    framebuffer_layout: FramebufferLayout,
    /// Device memory reader for framebuffer loading.
    /// Upstream: `Tegra::MaxwellDeviceMemoryManager& device_memory` held in RendererBase.
    /// Set post-construction via `set_device_memory_reader()`.
    device_memory: Option<crate::renderer_base::DeviceMemoryReader>,
}

impl RendererOpenGL {
    /// Create a new RendererOpenGL. Must be called with a current GL context.
    ///
    /// `load_fn` is used to load GL function pointers (typically SDL_GL_GetProcAddress).
    /// `context` is the graphics context used for swap buffers and thread binding.
    ///
    /// Upstream: `RendererOpenGL::RendererOpenGL(telemetry, emu_window, device_memory, gpu, context)`
    pub fn new<F>(
        mut load_fn: F,
        syncpoints: Arc<SyncpointManager>,
        device_memory: Arc<crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager>,
        mut context: Box<dyn GraphicsContext + Send>,
    ) -> Result<Self, OpenGLError>
    where
        F: FnMut(&'static str) -> *const std::os::raw::c_void,
    {
        context.make_current();

        // Load GL function pointers
        gl::load_with(&mut load_fn);
        gl_rasterizer::load_extra_functions(&mut load_fn);
        StateTracker::load_compat_functions(load_fn);

        // Query device capabilities
        let device = Device::new();

        // Initialize blit screen pipeline
        let blit_screen = BlitScreen::new().map_err(|e| OpenGLError::ShaderCompileFailed(e))?;

        // Initialize rasterizer with the shared MaxwellDeviceMemoryManager.
        // Upstream stores `StateTracker state_tracker` by value on
        // `RendererOpenGL` and injects a reference into `RasterizerOpenGL`;
        // Rust cannot express member references, so ruzu lets the rasterizer
        // own the tracker directly (see `RasterizerOpenGL::state_tracker_mut`).
        let rasterizer = RasterizerOpenGL::new(&device, syncpoints, device_memory);

        // Set up initial GL state (matching zuyu's RendererOpenGL constructor)
        unsafe {
            // Enable debug output if available
            let mut num_extensions = 0i32;
            gl::GetIntegerv(gl::NUM_EXTENSIONS, &mut num_extensions);
            let has_debug = (0..num_extensions as u32).any(|i| {
                let ptr = gl::GetStringi(gl::EXTENSIONS, i);
                if ptr.is_null() {
                    return false;
                }
                let s = std::ffi::CStr::from_ptr(ptr as *const _).to_string_lossy();
                s == "GL_KHR_debug"
            });

            if has_debug {
                gl::Enable(gl::DEBUG_OUTPUT);
                gl::Enable(gl::DEBUG_OUTPUT_SYNCHRONOUS);
                gl::DebugMessageCallback(Some(gl_debug_callback), std::ptr::null());
                debug!("OpenGL debug output enabled");
            }

            // Initialize vertex attributes to (0, 0, 0, 1)
            let mut max_attribs: i32 = 0;
            gl::GetIntegerv(gl::MAX_VERTEX_ATTRIBS, &mut max_attribs);
            for attrib in 0..max_attribs {
                gl::VertexAttrib4f(attrib as u32, 0.0, 0.0, 0.0, 1.0);
            }

            // Enable seamless cubemaps
            gl::Enable(gl::TEXTURE_CUBE_MAP_SEAMLESS);

            // Enable vertex buffer unified memory if available (NVIDIA extension).
            // GL_NV_vertex_buffer_unified_memory constants are not in the base gl crate.
            if device.has_vertex_buffer_unified_memory() {
                debug!("Skipping NV_vertex_buffer_unified_memory enable (NV extension not in gl crate)");
            }

            // Set clear color to black
            gl::ClearColor(0.0, 0.0, 0.0, 1.0);
        }

        info!(
            "RendererOpenGL initialized: {} ({})",
            device.renderer_name(),
            device.vendor_name()
        );

        // Create capture framebuffer and renderbuffer for applet capture layer.
        // Port of upstream constructor: capture_framebuffer.Create(); capture_renderbuffer.Create();
        // glBindRenderbuffer(...); glRenderbufferStorage(..., GL_SRGB8, LinearWidth, LinearHeight);
        let mut capture_framebuffer = OGLFramebuffer::new();
        capture_framebuffer.create();
        let mut capture_renderbuffer = OGLRenderbuffer::new();
        capture_renderbuffer.create();
        unsafe {
            gl::BindRenderbuffer(gl::RENDERBUFFER, capture_renderbuffer.handle);
            gl::RenderbufferStorage(
                gl::RENDERBUFFER,
                gl::SRGB8,
                capture::LINEAR_WIDTH as i32,
                capture::LINEAR_HEIGHT as i32,
            );
        }

        context.done_current();

        Ok(Self {
            device,
            blit_screen,
            rasterizer,
            context,
            base_data: RendererBaseData::new(),
            screenshot_framebuffer: OGLFramebuffer::new(),
            capture_framebuffer,
            capture_renderbuffer,
            framebuffer_layout: default_frame_layout(ScreenUndocked::WIDTH, ScreenUndocked::HEIGHT),
            device_memory: None,
        })
    }

    pub fn rasterizer_mut(&mut self) -> &mut RasterizerOpenGL {
        &mut self.rasterizer
    }

    /// Composite framebuffers to the screen.
    ///
    /// Port of `RendererOpenGL::Composite()`.
    ///
    /// Upstream flow:
    /// 1. RenderAppletCaptureLayer(framebuffers)
    /// 2. RenderScreenshot(framebuffers)
    /// 3. state_tracker.BindFramebuffer(0)
    /// 4. blit_screen->DrawScreen(framebuffers, layout, false)
    /// 5. ++m_current_frame
    /// 6. gpu.RendererFrameEndNotify()
    /// 7. rasterizer.TickFrame()
    /// 8. context->SwapBuffers()
    /// 9. render_window.OnFrameDisplayed()
    pub fn composite_impl(&mut self, framebuffers: &[FramebufferConfig]) {
        let profile = present_profile_enabled();
        let total_start = if profile { Some(Instant::now()) } else { None };
        let phase_start = if profile { Some(Instant::now()) } else { None };
        self.context.make_current();
        if let Some(start) = phase_start {
            PRESENT_MAKE_CURRENT_US.fetch_add(elapsed_us(start), Ordering::Relaxed);
        }

        if std::env::var_os("RUZU_TRACE_PRESENT").is_some() {
            log::info!(
                "[PRESENT] RendererOpenGL::composite_impl framebuffers={}",
                framebuffers.len()
            );
        }

        if framebuffers.is_empty() {
            return;
        }

        let phase_start = if profile { Some(Instant::now()) } else { None };
        self.render_applet_capture_layer(framebuffers);
        if let Some(start) = phase_start {
            PRESENT_CAPTURE_US.fetch_add(elapsed_us(start), Ordering::Relaxed);
        }
        let phase_start = if profile { Some(Instant::now()) } else { None };
        self.render_screenshot(framebuffers);
        if let Some(start) = phase_start {
            PRESENT_SCREENSHOT_US.fetch_add(elapsed_us(start), Ordering::Relaxed);
        }

        // Several Rust-side helper paths still bind framebuffers directly
        // while upstream routes render-target state through StateTracker.
        // Invalidate before binding the window framebuffer so BindFramebuffer(0)
        // cannot be skipped because of a stale cached value.
        {
            let state_tracker = self.rasterizer.state_tracker_mut();
            state_tracker.notify_framebuffer();
            state_tracker.bind_framebuffer(0);
        }
        let phase_start = if profile { Some(Instant::now()) } else { None };
        self.blit_screen.draw_screen(
            framebuffers,
            &self.framebuffer_layout,
            &mut self.rasterizer,
            false,
            self.device_memory.as_ref(),
        );
        if let Some(start) = phase_start {
            PRESENT_DRAW_SCREEN_US.fetch_add(elapsed_us(start), Ordering::Relaxed);
        }
        dump_present_ppm_once(self.base_data.current_frame.max(0) as u64, &self.framebuffer_layout);

        if std::env::var_os("RUZU_TRACE_PRESENT_READBACK").is_some() {
            unsafe {
                let width = self.framebuffer_layout.width;
                let height = self.framebuffer_layout.height;
                let sample_width = width.min(32) as i32;
                let sample_height = height.min(32) as i32;
                let mut old_pack_buffer = 0;
                let mut old_pack_alignment = 0;
                let mut old_pack_row_length = 0;
                gl::GetIntegerv(gl::PIXEL_PACK_BUFFER_BINDING, &mut old_pack_buffer);
                gl::GetIntegerv(gl::PACK_ALIGNMENT, &mut old_pack_alignment);
                gl::GetIntegerv(gl::PACK_ROW_LENGTH, &mut old_pack_row_length);
                gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
                gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
                gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);
                let max_x = width.saturating_sub(sample_width as u32) as i32;
                let max_y = height.saturating_sub(sample_height as u32) as i32;
                let origins = [
                    (0, 0),
                    (max_x / 2, max_y / 2),
                    (max_x, max_y),
                    (0, max_y),
                    (max_x, 0),
                ];
                let mut gl_error = 0;
                let mut summaries = Vec::with_capacity(origins.len());
                for (origin_x, origin_y) in origins {
                    let mut pixels = vec![0u8; (sample_width * sample_height * 4) as usize];
                    gl::ReadPixels(
                        origin_x,
                        origin_y,
                        sample_width,
                        sample_height,
                        gl::RGBA,
                        gl::UNSIGNED_BYTE,
                        pixels.as_mut_ptr() as *mut _,
                    );
                    gl_error |= gl::GetError();

                    let mut rgb_nonzero = 0usize;
                    let mut alpha_nonzero = 0usize;
                    let mut rgba_sum = [0u64; 4];
                    for px in pixels.chunks_exact(4) {
                        rgb_nonzero += px[0..3].iter().filter(|&&byte| byte != 0).count();
                        alpha_nonzero += usize::from(px[3] != 0);
                        for component in 0..4 {
                            rgba_sum[component] += px[component] as u64;
                        }
                    }
                    let checksum = pixels
                        .iter()
                        .fold(0u64, |acc, &byte| acc.wrapping_mul(16777619) ^ byte as u64);
                    summaries.push(format!(
                        "@{},{} rgb={} a={} sum={:?} crc=0x{:X}",
                        origin_x, origin_y, rgb_nonzero, alpha_nonzero, rgba_sum, checksum
                    ));
                }
                gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pack_buffer as u32);
                gl::PixelStorei(gl::PACK_ALIGNMENT, old_pack_alignment);
                gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);

                log::info!(
                    "[PRESENT_READBACK] {}x{} sample={}x{} regions=[{}] gl_error=0x{:X}",
                    width,
                    height,
                    sample_width,
                    sample_height,
                    summaries.join("; "),
                    gl_error
                );
            }
        }

        self.base_data.current_frame += 1;

        // Upstream: gpu.RendererFrameEndNotify() -> system.GetPerfStats().EndGameFrame()
        // PerfStats not yet integrated.
        let phase_start = if profile { Some(Instant::now()) } else { None };
        self.rasterizer.tick_frame();
        if let Some(start) = phase_start {
            PRESENT_TICK_FRAME_US.fetch_add(elapsed_us(start), Ordering::Relaxed);
        }

        let phase_start = if profile { Some(Instant::now()) } else { None };
        self.context.swap_buffers();
        if let Some(start) = phase_start {
            PRESENT_SWAP_BUFFERS_US.fetch_add(elapsed_us(start), Ordering::Relaxed);
        }
        if let Some(start) = total_start {
            let total = elapsed_us(start);
            PRESENT_COUNT.fetch_add(1, Ordering::Relaxed);
            PRESENT_TOTAL_US.fetch_add(total, Ordering::Relaxed);
            update_max(&PRESENT_MAX_US, total);
        }
        if std::env::var_os("RUZU_TRACE_PRESENT").is_some() {
            log::info!(
                "[PRESENT] RendererOpenGL::composite_impl swapped current_frame={}",
                self.base_data.current_frame
            );
        }
        // Upstream: render_window.OnFrameDisplayed()
        // EmuWindow callback not yet wired through renderer.
    }

    /// Render the applet capture layer to the capture framebuffer.
    ///
    /// Port of `RendererOpenGL::RenderAppletCaptureLayer()`.
    fn render_applet_capture_layer(&self, framebuffers: &[FramebufferConfig]) {
        let _ = framebuffers;
        // Full implementation:
        // 1. Save current framebuffer bindings
        // 2. Bind capture_framebuffer, attach capture_renderbuffer
        // 3. blit_applet->DrawScreen(framebuffers, Capture::Layout, true)
        // 4. Restore framebuffer bindings
        //
        // Requires a second BlitScreen instance (blit_applet) with
        // PresentFiltersForAppletCapture. Structural placeholder until
        // the applet capture path is needed.
    }

    /// Handle pending screenshot request.
    ///
    /// Port of `RendererOpenGL::RenderScreenshot()`.
    fn render_screenshot(&mut self, framebuffers: &[FramebufferConfig]) {
        if !self.base_data.is_screenshot_pending() {
            return;
        }

        let layout = self
            .base_data
            .settings
            .screenshot_framebuffer_layout
            .clone();
        let dst = self.base_data.settings.screenshot_bits;

        self.render_to_buffer(framebuffers, &layout, dst);

        if let Some(callback) = self.base_data.settings.screenshot_complete_callback.take() {
            callback(true);
        }
        self.base_data
            .settings
            .screenshot_requested
            .store(false, std::sync::atomic::Ordering::Relaxed);
    }

    /// Render framebuffers to a memory buffer (for screenshots).
    ///
    /// Port of `RendererOpenGL::RenderToBuffer()`.
    fn render_to_buffer(
        &mut self,
        _framebuffers: &[FramebufferConfig],
        layout: &crate::renderer_base::FramebufferLayout,
        dst: *mut std::ffi::c_void,
    ) {
        unsafe {
            let mut old_read_fb: i32 = 0;
            let mut old_draw_fb: i32 = 0;
            gl::GetIntegerv(gl::READ_FRAMEBUFFER_BINDING, &mut old_read_fb);
            gl::GetIntegerv(gl::DRAW_FRAMEBUFFER_BINDING, &mut old_draw_fb);

            self.screenshot_framebuffer.create();
            gl::BindFramebuffer(gl::FRAMEBUFFER, self.screenshot_framebuffer.handle);

            let mut renderbuffer: u32 = 0;
            gl::GenRenderbuffers(1, &mut renderbuffer);
            gl::BindRenderbuffer(gl::RENDERBUFFER, renderbuffer);
            gl::RenderbufferStorage(
                gl::RENDERBUFFER,
                gl::SRGB8,
                layout.width as i32,
                layout.height as i32,
            );
            gl::FramebufferRenderbuffer(
                gl::FRAMEBUFFER,
                gl::COLOR_ATTACHMENT0,
                gl::RENDERBUFFER,
                renderbuffer,
            );

            // Would call blit_screen.draw_screen here with the screenshot layout.

            gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
            gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);
            gl::ReadPixels(
                0,
                0,
                layout.width as i32,
                layout.height as i32,
                gl::BGRA,
                gl::UNSIGNED_INT_8_8_8_8_REV,
                dst,
            );

            self.screenshot_framebuffer.release();
            gl::DeleteRenderbuffers(1, &renderbuffer);

            gl::BindFramebuffer(gl::READ_FRAMEBUFFER, old_read_fb as u32);
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, old_draw_fb as u32);
        }
    }

    /// Render draw calls from the Maxwell 3D engine.
    ///
    /// This is the OpenGL equivalent of `RasterizerVulkan::render_draw_calls()`.
    pub fn render_draw_calls(
        &mut self,
        draw_calls: &[DrawCall],
        gpu_read: &dyn Fn(u64, &mut [u8]),
        framebuffer: Option<Framebuffer>,
    ) -> Option<Framebuffer> {
        self.rasterizer
            .render_draw_calls(draw_calls, gpu_read, framebuffer)
    }

    /// Get the device info.
    pub fn device(&self) -> &Device {
        &self.device
    }

    /// Get the vendor name string.
    pub fn device_vendor(&self) -> &str {
        self.device.vendor_name()
    }

    /// Get the current frame count.
    pub fn frame_count(&self) -> i32 {
        self.base_data.current_frame
    }

    /// Tick the rasterizer (end-of-frame cleanup).
    pub fn tick_frame(&mut self) {
        self.rasterizer.tick_frame();
    }
}

impl RendererBase for RendererOpenGL {
    fn context_ptr(&mut self) -> *mut dyn ruzu_core::frontend::graphics_context::GraphicsContext {
        &mut *self.context as *mut dyn ruzu_core::frontend::graphics_context::GraphicsContext
    }

    fn composite(&mut self, layers: &[FramebufferConfig]) {
        self.composite_impl(layers);
    }

    fn set_device_memory_reader(&mut self, reader: crate::renderer_base::DeviceMemoryReader) {
        if std::env::var_os("RUZU_TRACE_PRESENT").is_some() {
            log::info!("[PRESENT] RendererOpenGL::set_device_memory_reader");
        }
        self.rasterizer.set_device_memory_reader(reader.clone());
        self.device_memory = Some(reader);
    }

    fn set_shader_cache_gpu_reader(&mut self, reader: crate::renderer_base::DeviceMemoryReader) {
        // The trait's `DeviceMemoryReader` and the shader cache's
        // `GpuMemoryReader` are the same `Arc<dyn Fn(u64, &mut [u8]) +
        // Send + Sync>` shape, so we can hand it straight through.
        self.rasterizer.set_gpu_memory_reader(reader);
    }

    fn set_guest_memory_writer(&mut self, writer: crate::renderer_base::GuestMemoryWriter) {
        self.rasterizer.set_guest_memory_writer(writer);
    }

    fn set_gpu_ticks_getter(&mut self, getter: crate::renderer_base::GpuTicksGetter) {
        self.rasterizer.set_gpu_ticks_getter(getter);
    }

    fn set_gpu_tick_callback(&mut self, callback: crate::renderer_base::GpuTickCallback) {
        self.rasterizer.set_gpu_tick_callback(callback);
    }

    fn get_applet_capture_buffer(&self) -> Vec<u8> {
        // Port of `RendererOpenGL::GetAppletCaptureBuffer()`.
        // Full implementation reads from capture_framebuffer via glReadPixels,
        // then swizzles from linear to tiled format via Tegra::Texture::SwizzleTexture.
        // Returns TiledSize bytes. Requires texture swizzle utilities.
        use crate::capture;
        let tiled_size = capture::tiled_size() as usize;
        let mut out = vec![0u8; tiled_size];

        unsafe {
            let mut old_read_fb: i32 = 0;
            let mut old_draw_fb: i32 = 0;
            let mut old_pixel_pack_buffer: i32 = 0;
            let mut old_pack_row_length: i32 = 0;
            gl::GetIntegerv(gl::READ_FRAMEBUFFER_BINDING, &mut old_read_fb);
            gl::GetIntegerv(gl::DRAW_FRAMEBUFFER_BINDING, &mut old_draw_fb);
            gl::GetIntegerv(gl::PIXEL_PACK_BUFFER_BINDING, &mut old_pixel_pack_buffer);
            gl::GetIntegerv(gl::PACK_ROW_LENGTH, &mut old_pack_row_length);

            gl::BindFramebuffer(gl::FRAMEBUFFER, self.capture_framebuffer.handle);
            gl::FramebufferRenderbuffer(
                gl::FRAMEBUFFER,
                gl::COLOR_ATTACHMENT0,
                gl::RENDERBUFFER,
                self.capture_renderbuffer.handle,
            );
            gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
            gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);

            // Read linear pixels from capture renderbuffer.
            let mut linear = vec![0u8; tiled_size];
            gl::ReadPixels(
                0,
                0,
                capture::LINEAR_WIDTH as i32,
                capture::LINEAR_HEIGHT as i32,
                gl::RGBA,
                gl::UNSIGNED_INT_8_8_8_8_REV,
                linear.as_mut_ptr() as *mut _,
            );

            gl::BindFramebuffer(gl::READ_FRAMEBUFFER, old_read_fb as u32);
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, old_draw_fb as u32);
            gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pixel_pack_buffer as u32);
            gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);

            // Upstream calls Tegra::Texture::SwizzleTexture(out, linear, ...) here.
            // Without the swizzle utility, return the linear data directly.
            // The caller (applet capture) may need tiled format eventually.
            out = linear;
        }

        out
    }

    fn read_rasterizer(&self) -> *mut dyn RasterizerInterface {
        // Safety: We need a raw pointer to the rasterizer for GPU-level access.
        // This matches upstream's ReadRasterizer() returning a raw pointer.
        // Cast through a trait reference to create a wide pointer.
        let trait_ref: &dyn RasterizerInterface = &self.rasterizer;
        let ptr = trait_ref as *const dyn RasterizerInterface as *mut dyn RasterizerInterface;
        if std::env::var_os("RUZU_TRACE_RASTERIZER_BIND").is_some() {
            log::info!("RendererOpenGL::read_rasterizer rasterizer_ptr={:p}", ptr);
        }
        ptr
    }

    fn get_device_vendor(&self) -> String {
        self.device.vendor_name().to_string()
    }

    fn current_fps(&self) -> f32 {
        self.base_data.current_fps
    }

    fn current_frame(&self) -> i32 {
        self.base_data.current_frame
    }

    fn refresh_base_settings(&mut self) {
        // Port of `RendererBase::RefreshBaseSettings()` which calls
        // `UpdateCurrentFramebufferLayout()`.
        // Upstream: reads layout from render_window.GetFramebufferLayout()
        // then calls render_window.UpdateCurrentFramebufferLayout(width, height).
        // Without EmuWindow reference stored in the renderer, use current layout.
        let layout = &self.framebuffer_layout;
        if layout.width > 0 && layout.height > 0 {
            self.framebuffer_layout = ruzu_core::frontend::framebuffer_layout::default_frame_layout(
                layout.width,
                layout.height,
            );
        }
    }

    fn is_screenshot_pending(&self) -> bool {
        self.base_data.is_screenshot_pending()
    }
}

/// OpenGL debug message callback (GL_KHR_debug).
extern "system" fn gl_debug_callback(
    source: gl::types::GLenum,
    gltype: gl::types::GLenum,
    id: gl::types::GLuint,
    severity: gl::types::GLenum,
    _length: gl::types::GLsizei,
    message: *const gl::types::GLchar,
    _user_param: *mut std::os::raw::c_void,
) {
    let msg = unsafe {
        std::ffi::CStr::from_ptr(message)
            .to_string_lossy()
            .into_owned()
    };

    let source_str = match source {
        gl::DEBUG_SOURCE_API => "API",
        gl::DEBUG_SOURCE_WINDOW_SYSTEM => "Window",
        gl::DEBUG_SOURCE_SHADER_COMPILER => "Shader",
        gl::DEBUG_SOURCE_THIRD_PARTY => "3rdParty",
        gl::DEBUG_SOURCE_APPLICATION => "App",
        _ => "Other",
    };

    let type_str = match gltype {
        gl::DEBUG_TYPE_ERROR => "Error",
        gl::DEBUG_TYPE_DEPRECATED_BEHAVIOR => "Deprecated",
        gl::DEBUG_TYPE_UNDEFINED_BEHAVIOR => "UB",
        gl::DEBUG_TYPE_PORTABILITY => "Portability",
        gl::DEBUG_TYPE_PERFORMANCE => "Perf",
        gl::DEBUG_TYPE_MARKER => "Marker",
        _ => "Other",
    };

    match severity {
        gl::DEBUG_SEVERITY_HIGH => {
            log::error!("[GL {} {}] {}: {}", source_str, type_str, id, msg);
        }
        gl::DEBUG_SEVERITY_MEDIUM => {
            log::warn!("[GL {} {}] {}: {}", source_str, type_str, id, msg);
        }
        gl::DEBUG_SEVERITY_LOW => {
            debug!("[GL {} {}] {}: {}", source_str, type_str, id, msg);
        }
        _ => {
            // NOTIFICATION severity — too noisy, skip
        }
    }
}
