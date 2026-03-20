// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_shader_util.h and gl_shader_util.cpp
//!
//! OpenGL shader compilation utilities.

use std::ffi::CString;

/// Compile and link a separable program from GLSL source code.
///
/// Corresponds to `OpenGL::CreateProgram(std::string_view code, GLenum stage)`.
pub fn create_program_from_source(code: &str, stage: u32) -> u32 {
    unsafe {
        let shader = gl::CreateShader(stage);
        let c_code = CString::new(code).unwrap();
        let code_ptr = c_code.as_ptr();
        let length = code.len() as gl::types::GLint;
        gl::ShaderSource(shader, 1, &code_ptr, &length);
        gl::CompileShader(shader);

        // Check compilation in debug builds
        log_shader(shader);

        let program = link_separable_program(shader);
        gl::DeleteShader(shader);
        program
    }
}

/// Compile and link a separable program from SPIR-V binary.
///
/// Corresponds to `OpenGL::CreateProgram(std::span<const u32> code, GLenum stage)`.
pub fn create_program_from_spirv(code: &[u32], stage: u32) -> u32 {
    // GL_SHADER_BINARY_FORMAT_SPIR_V_ARB = 0x9551
    const GL_SHADER_BINARY_FORMAT_SPIR_V_ARB: u32 = 0x9551;

    unsafe {
        let shader = gl::CreateShader(stage);
        gl::ShaderBinary(
            1,
            &shader,
            GL_SHADER_BINARY_FORMAT_SPIR_V_ARB,
            code.as_ptr() as *const _,
            (code.len() * std::mem::size_of::<u32>()) as gl::types::GLsizei,
        );

        // glSpecializeShader is GL 4.6 / ARB_gl_spirv — may not be in all gl crate builds.
        // Use the function pointer directly if available.
        // For now, this is a placeholder that will need the correct extension binding.
        // gl::SpecializeShader(shader, main_str.as_ptr(), 0, std::ptr::null(), std::ptr::null());

        log_shader(shader);

        let program = link_separable_program(shader);
        gl::DeleteShader(shader);
        program
    }
}

/// Compile an assembly program (GLASM / NV_gpu_program).
///
/// Corresponds to `OpenGL::CompileProgram(std::string_view code, GLenum target)`.
///
/// Upstream uses `glGenProgramsARB` and `glNamedProgramStringEXT` which are
/// NV_gpu_program5 / EXT_direct_state_access extensions. The `gl` crate does not
/// expose these entry points because they are vendor-specific ARB/NV extensions
/// not part of core OpenGL. Implementing this requires either:
///   1. Using `gl::GetProcAddress` to load the function pointers at runtime, or
///   2. Using a different GL bindings crate that exposes ARB/NV extensions.
/// Until one of those approaches is wired up, this returns 0 (no program).
/// Assembly shaders are only used when `Device::use_assembly_shaders()` is true
/// (NVIDIA-only path), so this does not block AMD/Intel rendering.
pub fn compile_assembly_program(_code: &str, _target: u32) -> u32 {
    // glGenProgramsARB + glNamedProgramStringEXT — NV extension
    // Not available in the base gl crate; requires runtime function pointer loading.
    0
}

/// Link a shader into a separable program.
fn link_separable_program(shader: u32) -> u32 {
    unsafe {
        let program = gl::CreateProgram();
        gl::ProgramParameteri(program, gl::PROGRAM_SEPARABLE, gl::TRUE as i32);
        gl::AttachShader(program, shader);
        gl::LinkProgram(program);
        gl::DetachShader(program, shader);

        // Check link status in debug
        let mut link_status: gl::types::GLint = 0;
        gl::GetProgramiv(program, gl::LINK_STATUS, &mut link_status);
        if link_status == gl::FALSE as i32 {
            let mut log_length: gl::types::GLint = 0;
            gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut log_length);
            if log_length > 0 {
                let mut log = vec![0u8; log_length as usize];
                gl::GetProgramInfoLog(
                    program,
                    log_length,
                    std::ptr::null_mut(),
                    log.as_mut_ptr() as *mut _,
                );
                log::error!(
                    "Shader link error: {}",
                    String::from_utf8_lossy(&log)
                );
            }
        }

        program
    }
}

/// Log shader compilation errors/warnings.
fn log_shader(shader: u32) {
    unsafe {
        let mut status: gl::types::GLint = 0;
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);
        let mut log_length: gl::types::GLint = 0;
        gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut log_length);
        if log_length > 0 {
            let mut log = vec![0u8; log_length as usize];
            gl::GetShaderInfoLog(
                shader,
                log_length,
                std::ptr::null_mut(),
                log.as_mut_ptr() as *mut _,
            );
            let msg = String::from_utf8_lossy(&log);
            if status == gl::FALSE as i32 {
                log::error!("Shader compile error: {}", msg);
            } else {
                log::warn!("Shader compile warning: {}", msg);
            }
        }
    }
}
