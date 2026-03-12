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

use std::sync::Arc;

use log::{debug, info};
use thiserror::Error;

pub use gl_blit_screen::BlitScreen;
pub use gl_device::Device;
pub use gl_rasterizer::RasterizerOpenGL;
#[allow(unused_imports)]
pub use gl_state_tracker::StateTracker;

use crate::engines::maxwell_3d::DrawCall;
use crate::engines::Framebuffer;
use crate::rasterizer_interface::RasterizerInterface;
use crate::syncpoint::SyncpointManager;

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
/// Owns the device info, state tracker, blit screen pipeline, and rasterizer.
pub struct RendererOpenGL {
    device: Device,
    #[allow(dead_code)]
    state_tracker: StateTracker,
    blit_screen: BlitScreen,
    rasterizer: RasterizerOpenGL,
    frame_count: u64,
}

impl RendererOpenGL {
    /// Create a new RendererOpenGL. Must be called with a current GL context.
    ///
    /// `load_fn` is used to load GL function pointers (typically SDL_GL_GetProcAddress).
    pub fn new<F>(load_fn: F, syncpoints: Arc<SyncpointManager>) -> Result<Self, OpenGLError>
    where
        F: FnMut(&'static str) -> *const std::os::raw::c_void,
    {
        // Load GL function pointers
        gl::load_with(load_fn);

        // Query device capabilities
        let device = Device::new();
        let state_tracker = StateTracker::new();

        // Initialize blit screen pipeline
        let blit_screen = BlitScreen::new()
            .map_err(|e| OpenGLError::ShaderCompileFailed(e))?;

        // Initialize rasterizer
        let rasterizer = RasterizerOpenGL::new(&device, syncpoints);

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
                let s = std::ffi::CStr::from_ptr(ptr as *const _)
                    .to_string_lossy();
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

        Ok(Self {
            device,
            state_tracker,
            blit_screen,
            rasterizer,
            frame_count: 0,
        })
    }

    /// Composite framebuffers to the screen.
    ///
    /// Corresponds to zuyu's `RendererOpenGL::Composite()`.
    pub fn composite(
        &mut self,
        pixels: &[u8],
        fb_width: u32,
        fb_height: u32,
        viewport_width: u32,
        viewport_height: u32,
    ) {
        self.blit_screen
            .draw_screen(pixels, fb_width, fb_height, viewport_width, viewport_height);
        self.frame_count += 1;
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
    pub fn frame_count(&self) -> u64 {
        self.frame_count
    }

    /// Tick the rasterizer (end-of-frame cleanup).
    pub fn tick_frame(&mut self) {
        self.rasterizer.tick_frame();
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
