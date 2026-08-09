// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/renderer_opengl.h and renderer_opengl.cpp
//!
//! OpenGL GPU renderer — provides an alternative backend to Vulkan.
//!
use std::ffi::CStr;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, OnceLock};
use std::time::Instant;

use log::{debug, info};
use thiserror::Error;

use super::gl_blit_screen::BlitScreen;
use super::gl_device::Device;
use super::gl_rasterizer::RasterizerOpenGL;
use super::gl_resource_manager::{OGLFramebuffer, OGLRenderbuffer};
use super::gl_shader_manager::{ProgramManager, ProgramManagerHandle};
use super::gl_state_tracker::StateTracker;
use super::{
    gl_buffer_cache, gl_graphics_pipeline, gl_rasterizer, gl_shader_context, gl_shader_util,
    present,
};

use crate::capture;
use crate::engines::maxwell_3d::DrawCall;
use crate::engines::Framebuffer;
use crate::framebuffer_config::FramebufferConfig;
use crate::host1x::syncpoint_manager::SyncpointManager;
use crate::present::{PRESENT_FILTERS_FOR_APPLET_CAPTURE, PRESENT_FILTERS_FOR_DISPLAY};
use crate::rasterizer_interface::RasterizerInterface;
use crate::renderer_base::{RendererBase, RendererBaseData};
use common::telemetry::{FieldType, FieldValue};
use ruzu_core::frontend::framebuffer_layout::{
    default_frame_layout, FramebufferLayout, ScreenUndocked,
};
use ruzu_core::frontend::graphics_context::GraphicsContext;
use ruzu_core::telemetry_session::TelemetrySession;

const GL_VERTEX_ATTRIB_ARRAY_UNIFIED_NV: u32 = 0x8F1E;
const GL_ELEMENT_ARRAY_UNIFIED_NV: u32 = 0x8F1F;

type GlEnableClientState = unsafe extern "system" fn(cap: u32);
static GL_ENABLE_CLIENT_STATE: OnceLock<Option<GlEnableClientState>> = OnceLock::new();

fn load_renderer_extra_functions<F>(load_fn: &mut F)
where
    F: FnMut(&'static str) -> *const std::os::raw::c_void,
{
    let pointer = load_fn("glEnableClientState");
    let function = if pointer.is_null() {
        None
    } else {
        Some(unsafe {
            std::mem::transmute::<*const std::os::raw::c_void, GlEnableClientState>(pointer)
        })
    };
    let _ = GL_ENABLE_CLIENT_STATE.set(function);
}

fn gl_string(name: u32) -> String {
    unsafe {
        let pointer = gl::GetString(name);
        if pointer.is_null() {
            return String::new();
        }
        CStr::from_ptr(pointer.cast())
            .to_string_lossy()
            .into_owned()
    }
}

fn has_gl_extension(name: &str) -> bool {
    unsafe {
        let mut count = 0;
        gl::GetIntegerv(gl::NUM_EXTENSIONS, &mut count);
        (0..count as u32).any(|index| {
            let pointer = gl::GetStringi(gl::EXTENSIONS, index);
            !pointer.is_null() && CStr::from_ptr(pointer.cast()).to_bytes() == name.as_bytes()
        })
    }
}

fn add_telemetry_fields(
    telemetry_session: &mut TelemetrySession,
    vendor: &str,
    model: &str,
    version: &str,
) {
    let field = FieldType::UserSystem;
    telemetry_session.add_field(field, "GPU_Vendor", FieldValue::String(vendor.to_string()));
    telemetry_session.add_field(field, "GPU_Model", FieldValue::String(model.to_string()));
    telemetry_session.add_field(
        field,
        "GPU_OpenGL_Version",
        FieldValue::String(version.to_string()),
    );
}

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
static PRESENT_PPM_CALL_COUNT: AtomicU64 = AtomicU64::new(0);

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

fn emit_present_composite(
    stage: u64,
    frame: u64,
    framebuffers: u64,
    current_frame: u64,
    draw_count: u64,
    width: u64,
    height: u64,
    gl_error: u64,
) {
    if !common::trace::is_enabled(common::trace::cat::PRESENT_COMPOSITE) {
        return;
    }
    let _ = common::trace::emit_raw(
        common::trace::cat::PRESENT_COMPOSITE,
        &[
            stage,
            frame,
            framebuffers,
            current_frame,
            draw_count,
            width,
            height,
            gl_error,
        ],
    );
}

fn dump_present_ppm_once(
    current_frame: u64,
    draw_count: u64,
    layout: &FramebufferLayout,
) -> Option<u64> {
    let path = if let Some(path) = std::env::var_os("RUZU_DUMP_PRESENT_PPM") {
        path
    } else if let Some(dir) = std::env::var_os("RUZU_DUMP_PRESENT_PPM_DIR") {
        let dir = std::path::PathBuf::from(dir);
        if let Err(err) = std::fs::create_dir_all(&dir) {
            log::warn!("[PRESENT_PPM] failed to create {}: {}", dir.display(), err);
            return None;
        }
        dir.join("present.ppm").into_os_string()
    } else {
        return None;
    };
    let present_index = PRESENT_PPM_CALL_COUNT.fetch_add(1, Ordering::Relaxed);
    let target_present_indices = std::env::var("RUZU_DUMP_PRESENT_PPM_INDICES")
        .ok()
        .map(|spec| {
            spec.split(',')
                .filter_map(|value| value.trim().parse::<u64>().ok())
                .collect::<Vec<_>>()
        });
    let target_present_index = std::env::var("RUZU_DUMP_PRESENT_PPM_INDEX")
        .ok()
        .and_then(|value| value.parse::<u64>().ok());
    let target_present_start = std::env::var("RUZU_DUMP_PRESENT_PPM_START")
        .ok()
        .and_then(|value| value.parse::<u64>().ok());
    let target_present_end = std::env::var("RUZU_DUMP_PRESENT_PPM_END")
        .ok()
        .and_then(|value| value.parse::<u64>().ok());
    let target_present_every = std::env::var("RUZU_DUMP_PRESENT_PPM_EVERY")
        .ok()
        .and_then(|value| value.parse::<u64>().ok());
    let target_frame = std::env::var("RUZU_DUMP_PRESENT_PPM_FRAME")
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
        .unwrap_or(0);
    let target_draw_indices = std::env::var("RUZU_DUMP_PRESENT_PPM_DRAW_INDICES")
        .ok()
        .map(|spec| {
            spec.split(',')
                .filter_map(|value| value.trim().parse::<u64>().ok())
                .collect::<Vec<_>>()
        });
    let target_draw_min = std::env::var("RUZU_DUMP_PRESENT_PPM_DRAW_MIN")
        .ok()
        .and_then(|value| value.parse::<u64>().ok());
    let target_draw_max = std::env::var("RUZU_DUMP_PRESENT_PPM_DRAW_MAX")
        .ok()
        .and_then(|value| value.parse::<u64>().ok());
    let multi_index_match = target_present_indices
        .as_ref()
        .is_some_and(|indices| indices.contains(&present_index));
    let range_index_match = target_present_start.is_some_and(|start| {
        present_index >= start
            && target_present_end.is_none_or(|end| present_index <= end)
            && target_present_every
                .is_none_or(|every| every != 0 && (present_index - start) % every == 0)
    });
    let draw_index_match = target_draw_indices
        .as_ref()
        .is_some_and(|indices| indices.contains(&draw_count));
    let draw_range_match = target_draw_min.is_some_and(|min| draw_count >= min)
        && target_draw_max.is_none_or(|max| draw_count <= max);
    let draw_selector_active = target_draw_indices.is_some() || target_draw_min.is_some();
    let draw_match = draw_index_match || draw_range_match;
    if target_present_indices.is_some() && !multi_index_match {
        return None;
    }
    if target_present_start.is_some() && !range_index_match {
        return None;
    }
    if draw_selector_active && !draw_match {
        return None;
    }
    if target_present_indices.is_none()
        && target_present_start.is_none()
        && !draw_selector_active
        && (target_present_index.is_some_and(|target| present_index < target)
            || (target_present_index.is_none() && current_frame < target_frame)
            || PRESENT_PPM_DUMPED.swap(true, Ordering::Relaxed))
    {
        return None;
    }
    if layout.width == 0 || layout.height == 0 {
        return None;
    }
    if std::env::var_os("RUZU_DUMP_PRESENT_PPM_LOG").is_some() {
        eprintln!(
            "[PRESENT_PPM_DUMP] present_index={} frame={} draw_count={} path={}",
            present_index,
            current_frame,
            draw_count,
            std::path::Path::new(&path).display()
        );
    }
    let path = if draw_selector_active {
        let mut output = std::path::PathBuf::from(&path);
        let stem = output
            .file_stem()
            .and_then(|stem| stem.to_str())
            .unwrap_or("present")
            .to_string();
        let ext = output
            .extension()
            .and_then(|ext| ext.to_str())
            .unwrap_or("ppm")
            .to_string();
        output.set_file_name(format!("{stem}_{present_index}_draw_{draw_count}.{ext}"));
        output.into_os_string()
    } else if multi_index_match || range_index_match {
        let mut output = std::path::PathBuf::from(&path);
        let stem = output
            .file_stem()
            .and_then(|stem| stem.to_str())
            .unwrap_or("present")
            .to_string();
        let ext = output
            .extension()
            .and_then(|ext| ext.to_str())
            .unwrap_or("ppm")
            .to_string();
        output.set_file_name(format!("{stem}_{present_index}.{ext}"));
        output.into_os_string()
    } else {
        path
    };

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
                "[PRESENT_PPM] wrote {} frame={} present_index={} gl_error=0x{:X}",
                path.to_string_lossy(),
                current_frame,
                present_index,
                gl_error
            ),
            Err(err) => log::warn!(
                "[PRESENT_PPM] failed to write {}: {}",
                path.to_string_lossy(),
                err
            ),
        }
    }
    Some(present_index)
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
    // Rust drops fields in declaration order. Keep non-owning consumers before
    // the objects they reference, matching C++'s reverse member destruction.
    blit_applet: BlitScreen,
    blit_screen: BlitScreen,
    capture_renderbuffer: OGLRenderbuffer,
    capture_framebuffer: OGLFramebuffer,
    screenshot_framebuffer: OGLFramebuffer,
    rasterizer: Box<RasterizerOpenGL>,
    /// Concrete owner of the shared OpenGL program manager.
    ///
    /// Upstream declares this before `rasterizer`, but C++ destroys members in
    /// reverse order. Rust drops fields in declaration order, so this field is
    /// declared after `rasterizer` to keep the same effective teardown order.
    #[allow(dead_code)]
    program_manager: ProgramManagerHandle,
    state_tracker: Box<StateTracker>,
    device: Box<Device>,
    /// Callback for upstream `gpu.RendererFrameEndNotify()`.
    frame_end_notify: Arc<dyn Fn() + Send + Sync>,
    /// Callback for upstream `render_window.OnFrameDisplayed()`.
    frame_displayed_notify: Arc<dyn Fn() + Send + Sync>,
    /// Common renderer state (frame count, FPS, screenshot settings).
    base_data: RendererBaseData,
    /// Current framebuffer layout (window size + screen region).
    framebuffer_layout: FramebufferLayout,
    /// Graphics context for swap buffers / make current. It must outlive all
    /// OpenGL resources above.
    /// Upstream: `std::unique_ptr<Core::Frontend::GraphicsContext> context` in RendererBase.
    context: Box<dyn GraphicsContext + Send>,
}

// The renderer and its OpenGL-owned state move to, then remain on, the render
// thread. Raw non-owning references are only dereferenced on that thread.
unsafe impl Send for RendererOpenGL {}

impl RendererOpenGL {
    /// Create a new RendererOpenGL. Must be called with a current GL context.
    ///
    /// `load_fn` is used to load GL function pointers (typically SDL_GL_GetProcAddress).
    /// `context` is the graphics context used for swap buffers and thread binding.
    ///
    /// Upstream: `RendererOpenGL::RendererOpenGL(telemetry, emu_window, device_memory, gpu, context)`
    pub fn new<F>(
        telemetry_session: Option<&mut TelemetrySession>,
        mut load_fn: F,
        syncpoints: Arc<SyncpointManager>,
        device_memory: Arc<crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager>,
        shader_notify: crate::shader_notify::ShaderNotifyHandle,
        strict_context_required: bool,
        mut context: Box<dyn GraphicsContext + Send>,
        shared_context_factory: Option<gl_shader_context::SharedContextFactory>,
        frame_end_notify: Arc<dyn Fn() + Send + Sync>,
        frame_displayed_notify: Arc<dyn Fn() + Send + Sync>,
    ) -> Result<Self, OpenGLError>
    where
        F: FnMut(&'static str) -> *const std::os::raw::c_void,
    {
        context.make_current();

        // Load GL function pointers
        gl::load_with(&mut load_fn);
        gl_buffer_cache::load_extra_functions(&mut load_fn);
        gl_graphics_pipeline::load_extra_functions(&mut load_fn);
        gl_shader_util::load_extra_functions(&mut load_fn);
        gl_rasterizer::load_extra_functions(&mut load_fn);
        present::window_adapt_pass::load_extra_functions(&mut load_fn);
        load_renderer_extra_functions(&mut load_fn);
        StateTracker::load_compat_functions(load_fn);

        // Query device capabilities
        let device =
            Box::new(Device::new(strict_context_required).map_err(OpenGLError::InitFailed)?);
        let device_ptr: *const Device = &*device;

        let gl_version = gl_string(gl::VERSION);
        let gpu_vendor = gl_string(gl::VENDOR);
        let gpu_model = gl_string(gl::RENDERER);
        info!("GL_VERSION: {}", gl_version);
        info!("GL_VENDOR: {}", gpu_vendor);
        info!("GL_RENDERER: {}", gpu_model);
        if let Some(telemetry_session) = telemetry_session {
            add_telemetry_fields(telemetry_session, &gpu_vendor, &gpu_model, &gl_version);
        }

        let program_manager = ProgramManager::new_shared(&device);

        let device_memory_reader: crate::renderer_base::DeviceMemoryReader = {
            let device_memory = Arc::clone(&device_memory);
            Arc::new(move |addr, out| {
                let host_ptr = device_memory.get_pointer(addr);
                if host_ptr.is_null() {
                    return false;
                }
                unsafe {
                    std::ptr::copy_nonoverlapping(host_ptr, out.as_mut_ptr(), out.len());
                }
                true
            })
        };

        // Keep the tracker heap-stable: the rasterizer, texture cache, and
        // presentation helpers hold the same non-owning reference as upstream.
        let mut state_tracker = Box::new(StateTracker::new());
        let state_tracker_ptr: *mut StateTracker = state_tracker.as_mut();
        let mut rasterizer = Box::new(RasterizerOpenGL::new(
            &device,
            syncpoints,
            device_memory,
            Arc::clone(&program_manager),
            state_tracker.as_mut(),
            shared_context_factory,
            shader_notify,
        ));
        rasterizer.set_device_memory_reader(Arc::clone(&device_memory_reader));
        let rasterizer_ptr: *mut RasterizerOpenGL = &mut *rasterizer;

        // Initialize blit screen pipeline after the rasterizer is heap-stable so
        // present layers can store the same non-owning rasterizer reference as upstream.
        let blit_screen = BlitScreen::new(
            Arc::clone(&program_manager),
            rasterizer_ptr,
            state_tracker_ptr,
            device_ptr,
            Arc::clone(&device_memory_reader),
            &PRESENT_FILTERS_FOR_DISPLAY,
        )
        .map_err(|e| OpenGLError::ShaderCompileFailed(e))?;
        let blit_applet = BlitScreen::new(
            Arc::clone(&program_manager),
            rasterizer_ptr,
            state_tracker_ptr,
            device_ptr,
            Arc::clone(&device_memory_reader),
            &PRESENT_FILTERS_FOR_APPLET_CAPTURE,
        )
        .map_err(|e| OpenGLError::ShaderCompileFailed(e))?;

        // Set up initial GL state (matching zuyu's RendererOpenGL constructor)
        unsafe {
            // Enable debug output if available
            if *common::settings::values().renderer_debug.get_value()
                && has_gl_extension("GL_KHR_debug")
            {
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

            if !has_gl_extension("GL_ARB_seamless_cubemap_per_texture")
                && !has_gl_extension("GL_AMD_seamless_cubemap_per_texture")
            {
                gl::Enable(gl::TEXTURE_CUBE_MAP_SEAMLESS);
            }

            // Enable vertex buffer unified memory if available (NVIDIA extension).
            if device.has_vertex_buffer_unified_memory() {
                let enable_client_state = GL_ENABLE_CLIENT_STATE
                    .get()
                    .and_then(|entry| *entry)
                    .ok_or_else(|| {
                        OpenGLError::InitFailed(
                            "GL_NV_vertex_buffer_unified_memory is present but glEnableClientState is unavailable"
                                .to_string(),
                        )
                    })?;
                enable_client_state(GL_VERTEX_ATTRIB_ARRAY_UNIFIED_NV);
                enable_client_state(GL_ELEMENT_ARRAY_UNIFIED_NV);
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
            blit_applet,
            blit_screen,
            capture_renderbuffer,
            capture_framebuffer,
            screenshot_framebuffer: OGLFramebuffer::new(),
            rasterizer,
            program_manager,
            state_tracker,
            device,
            frame_end_notify,
            frame_displayed_notify,
            base_data: RendererBaseData::new(),
            framebuffer_layout: default_frame_layout(ScreenUndocked::WIDTH, ScreenUndocked::HEIGHT),
            context,
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
        emit_present_composite(
            0,
            self.base_data.current_frame.max(0) as u64,
            framebuffers.len() as u64,
            self.base_data.current_frame.max(0) as u64,
            self.rasterizer.total_draw_count(),
            self.framebuffer_layout.width as u64,
            self.framebuffer_layout.height as u64,
            0,
        );

        if framebuffers.is_empty() {
            emit_present_composite(
                1,
                self.base_data.current_frame.max(0) as u64,
                0,
                self.base_data.current_frame.max(0) as u64,
                self.rasterizer.total_draw_count(),
                self.framebuffer_layout.width as u64,
                self.framebuffer_layout.height as u64,
                0,
            );
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
            self.state_tracker.notify_framebuffer();
            self.state_tracker.bind_framebuffer(0);
        }
        let phase_start = if profile { Some(Instant::now()) } else { None };
        self.blit_screen
            .draw_screen(framebuffers, &self.framebuffer_layout, false);
        if let Some(start) = phase_start {
            PRESENT_DRAW_SCREEN_US.fetch_add(elapsed_us(start), Ordering::Relaxed);
        }
        let draw_gl_error = if common::trace::is_enabled(common::trace::cat::PRESENT_COMPOSITE) {
            unsafe { gl::GetError() as u64 }
        } else {
            0
        };
        emit_present_composite(
            2,
            self.base_data.current_frame.max(0) as u64,
            framebuffers.len() as u64,
            self.base_data.current_frame.max(0) as u64,
            self.rasterizer.total_draw_count(),
            self.framebuffer_layout.width as u64,
            self.framebuffer_layout.height as u64,
            draw_gl_error,
        );
        let dumped_present_index = dump_present_ppm_once(
            self.base_data.current_frame.max(0) as u64,
            self.rasterizer.total_draw_count(),
            &self.framebuffer_layout,
        );
        if let Some(present_index) = dumped_present_index {
            if std::env::var_os("RUZU_DUMP_PRESENT_EXTRA_ON_PPM").is_some() {
                self.rasterizer
                    .trace_present_images_by_gpu_addr_env(present_index);
            }
        }

        self.base_data.current_frame += 1;

        (self.frame_end_notify)();
        let phase_start = if profile { Some(Instant::now()) } else { None };
        self.rasterizer.tick_frame();
        if let Some(start) = phase_start {
            PRESENT_TICK_FRAME_US.fetch_add(elapsed_us(start), Ordering::Relaxed);
        }

        let phase_start = if profile { Some(Instant::now()) } else { None };
        emit_present_composite(
            3,
            self.base_data.current_frame.max(0) as u64,
            framebuffers.len() as u64,
            self.base_data.current_frame.max(0) as u64,
            self.rasterizer.total_draw_count(),
            self.framebuffer_layout.width as u64,
            self.framebuffer_layout.height as u64,
            0,
        );
        self.context.swap_buffers();
        emit_present_composite(
            4,
            self.base_data.current_frame.max(0) as u64,
            framebuffers.len() as u64,
            self.base_data.current_frame.max(0) as u64,
            self.rasterizer.total_draw_count(),
            self.framebuffer_layout.width as u64,
            self.framebuffer_layout.height as u64,
            0,
        );
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
        (self.frame_displayed_notify)();
    }

    /// Render the applet capture layer to the capture framebuffer.
    ///
    /// Port of `RendererOpenGL::RenderAppletCaptureLayer()`.
    fn render_applet_capture_layer(&mut self, framebuffers: &[FramebufferConfig]) {
        unsafe {
            let mut old_read_fb = 0;
            let mut old_draw_fb = 0;
            gl::GetIntegerv(gl::READ_FRAMEBUFFER_BINDING, &mut old_read_fb);
            gl::GetIntegerv(gl::DRAW_FRAMEBUFFER_BINDING, &mut old_draw_fb);
            gl::BindFramebuffer(gl::FRAMEBUFFER, self.capture_framebuffer.handle);
            gl::FramebufferRenderbuffer(
                gl::FRAMEBUFFER,
                gl::COLOR_ATTACHMENT0,
                gl::RENDERBUFFER,
                self.capture_renderbuffer.handle,
            );

            let layout = FramebufferLayout {
                width: capture::LINEAR_WIDTH,
                height: capture::LINEAR_HEIGHT,
                screen: ruzu_core::frontend::framebuffer_layout::Rectangle::new(
                    0,
                    0,
                    capture::LINEAR_WIDTH,
                    capture::LINEAR_HEIGHT,
                ),
                is_srgb: false,
            };
            self.blit_applet.draw_screen(framebuffers, &layout, true);

            gl::BindFramebuffer(gl::READ_FRAMEBUFFER, old_read_fb as u32);
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, old_draw_fb as u32);
        }
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
            .store(false, std::sync::atomic::Ordering::SeqCst);
    }

    /// Render framebuffers to a memory buffer (for screenshots).
    ///
    /// Port of `RendererOpenGL::RenderToBuffer()`.
    fn render_to_buffer(
        &mut self,
        framebuffers: &[FramebufferConfig],
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

            self.blit_screen.draw_screen(framebuffers, layout, false);

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

    fn request_screenshot(
        &mut self,
        data: *mut std::ffi::c_void,
        callback: Box<dyn FnOnce(bool) + Send>,
        layout: FramebufferLayout,
    ) {
        self.base_data.request_screenshot(data, callback, layout);
    }

    fn set_shader_cache_gpu_reader(&mut self, reader: crate::renderer_base::ShaderCacheGpuReader) {
        // The OpenGL shader cache now compiles graphics pipelines through the
        // channel-owned shared shader cache. Keep forwarding this reader to
        // the rasterizer for compatibility paths outside shader compilation.
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

    fn set_invalidate_gpu_cache_callback(
        &mut self,
        callback: crate::renderer_base::InvalidateGpuCacheCallback,
    ) {
        self.rasterizer.set_invalidate_gpu_cache_callback(callback);
    }

    fn get_applet_capture_buffer(&mut self) -> Vec<u8> {
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

            crate::textures::decoders::swizzle_texture(
                &mut out,
                &linear,
                capture::BYTES_PER_PIXEL,
                capture::LINEAR_WIDTH,
                capture::LINEAR_HEIGHT,
                capture::LINEAR_DEPTH,
                capture::BLOCK_HEIGHT,
                capture::BLOCK_DEPTH,
                0,
            );
        }

        out
    }

    fn read_rasterizer(&self) -> *mut dyn RasterizerInterface {
        // Safety: We need a raw pointer to the rasterizer for GPU-level access.
        // This matches upstream's ReadRasterizer() returning a raw pointer.
        // Cast through a trait reference to create a wide pointer.
        let trait_ref: &dyn RasterizerInterface = &*self.rasterizer;
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

#[cfg(test)]
mod tests {
    use super::*;
    use common::telemetry::{Field, VisitorInterface};
    use std::collections::BTreeMap;

    #[test]
    fn telemetry_fields_match_upstream_names() {
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
        add_telemetry_fields(&mut session, "Mesa/X.org", "AMD Radeon", "4.6 Mesa 24.0");
        let mut collector = Collector(BTreeMap::new());
        session.field_collection().accept(&mut collector);

        assert_eq!(
            collector.0.get("GPU_Vendor"),
            Some(&FieldValue::String("Mesa/X.org".to_string()))
        );
        assert_eq!(
            collector.0.get("GPU_Model"),
            Some(&FieldValue::String("AMD Radeon".to_string()))
        );
        assert_eq!(
            collector.0.get("GPU_OpenGL_Version"),
            Some(&FieldValue::String("4.6 Mesa 24.0".to_string()))
        );
    }
}
