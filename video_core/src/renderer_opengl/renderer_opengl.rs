// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/renderer_opengl.h and renderer_opengl.cpp
//!
//! Main OpenGL renderer class — owns device, state tracker, program manager, rasterizer,
//! and blit screen. Orchestrates frame composition and screenshot capture.
//!
//! NOTE: The existing `mod.rs` already contains a `RendererOpenGL` struct with core
//! functionality. This file ports the *upstream* `RendererOpenGL` class structure more
//! faithfully for reference and future convergence.

/// Debug message callback source strings.
///
/// Corresponds to the anonymous `GetSource()` in renderer_opengl.cpp.
pub fn get_debug_source(source: u32) -> &'static str {
    match source {
        gl::DEBUG_SOURCE_API => "API",
        gl::DEBUG_SOURCE_WINDOW_SYSTEM => "WINDOW_SYSTEM",
        gl::DEBUG_SOURCE_SHADER_COMPILER => "SHADER_COMPILER",
        gl::DEBUG_SOURCE_THIRD_PARTY => "THIRD_PARTY",
        gl::DEBUG_SOURCE_APPLICATION => "APPLICATION",
        gl::DEBUG_SOURCE_OTHER => "OTHER",
        _ => "Unknown source",
    }
}

/// Debug message callback type strings.
///
/// Corresponds to the anonymous `GetType()` in renderer_opengl.cpp.
pub fn get_debug_type(gltype: u32) -> &'static str {
    match gltype {
        gl::DEBUG_TYPE_ERROR => "ERROR",
        gl::DEBUG_TYPE_DEPRECATED_BEHAVIOR => "DEPRECATED_BEHAVIOR",
        gl::DEBUG_TYPE_UNDEFINED_BEHAVIOR => "UNDEFINED_BEHAVIOR",
        gl::DEBUG_TYPE_PORTABILITY => "PORTABILITY",
        gl::DEBUG_TYPE_PERFORMANCE => "PERFORMANCE",
        gl::DEBUG_TYPE_OTHER => "OTHER",
        gl::DEBUG_TYPE_MARKER => "MARKER",
        _ => "Unknown type",
    }
}

/// OpenGL debug message callback.
///
/// Corresponds to `DebugHandler()` in renderer_opengl.cpp.
pub extern "system" fn debug_handler(
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
    let src = get_debug_source(source);
    let typ = get_debug_type(gltype);

    match severity {
        gl::DEBUG_SEVERITY_HIGH => {
            log::error!("{} {} {}: {}", src, typ, id, msg);
        }
        gl::DEBUG_SEVERITY_MEDIUM => {
            log::warn!("{} {} {}: {}", src, typ, id, msg);
        }
        gl::DEBUG_SEVERITY_LOW | gl::DEBUG_SEVERITY_NOTIFICATION => {
            log::debug!("{} {} {}: {}", src, typ, id, msg);
        }
        _ => {}
    }
}

/// Add telemetry fields for the OpenGL renderer.
///
/// Corresponds to `RendererOpenGL::AddTelemetryFields()`.
pub fn add_telemetry_fields() {
    unsafe {
        let gl_version = gl::GetString(gl::VERSION);
        let gpu_vendor = gl::GetString(gl::VENDOR);
        let gpu_model = gl::GetString(gl::RENDERER);

        if !gl_version.is_null() {
            let version = std::ffi::CStr::from_ptr(gl_version as *const _).to_string_lossy();
            log::info!("GL_VERSION: {}", version);
        }
        if !gpu_vendor.is_null() {
            let vendor = std::ffi::CStr::from_ptr(gpu_vendor as *const _).to_string_lossy();
            log::info!("GL_VENDOR: {}", vendor);
        }
        if !gpu_model.is_null() {
            let model = std::ffi::CStr::from_ptr(gpu_model as *const _).to_string_lossy();
            log::info!("GL_RENDERER: {}", model);
        }
    }
}
