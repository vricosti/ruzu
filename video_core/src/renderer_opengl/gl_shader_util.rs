// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_shader_util.h and gl_shader_util.cpp
//!
//! OpenGL shader compilation utilities.

use std::ffi::{c_void, CString};
use std::sync::OnceLock;

type GlSpecializeShader = unsafe extern "system" fn(
    shader: gl::types::GLuint,
    p_entry_point: *const gl::types::GLchar,
    num_specialization_constants: gl::types::GLuint,
    p_constant_index: *const gl::types::GLuint,
    p_constant_value: *const gl::types::GLuint,
);
type GlGenProgramsArb =
    unsafe extern "system" fn(n: gl::types::GLsizei, programs: *mut gl::types::GLuint);
type GlNamedProgramStringExt = unsafe extern "system" fn(
    program: gl::types::GLuint,
    target: gl::types::GLenum,
    format: gl::types::GLenum,
    len: gl::types::GLsizei,
    string: *const c_void,
);
type GlBindProgramArb =
    unsafe extern "system" fn(target: gl::types::GLenum, program: gl::types::GLuint);
type GlProgramLocalParameter4fArb = unsafe extern "system" fn(
    target: gl::types::GLenum,
    index: gl::types::GLuint,
    x: gl::types::GLfloat,
    y: gl::types::GLfloat,
    z: gl::types::GLfloat,
    w: gl::types::GLfloat,
);

static GL_SPECIALIZE_SHADER: OnceLock<Option<GlSpecializeShader>> = OnceLock::new();
static GL_GEN_PROGRAMS_ARB: OnceLock<Option<GlGenProgramsArb>> = OnceLock::new();
static GL_NAMED_PROGRAM_STRING_EXT: OnceLock<Option<GlNamedProgramStringExt>> = OnceLock::new();
static GL_BIND_PROGRAM_ARB: OnceLock<Option<GlBindProgramArb>> = OnceLock::new();
static GL_PROGRAM_LOCAL_PARAMETER_4F_ARB: OnceLock<Option<GlProgramLocalParameter4fArb>> =
    OnceLock::new();

const GL_PROGRAM_FORMAT_ASCII_ARB: u32 = 0x8875;
const GL_PROGRAM_ERROR_STRING_NV: u32 = 0x8874;

fn renderer_debug_enabled() -> bool {
    *common::settings::values().renderer_debug.get_value()
}

fn load_optional_gl_function<T, F>(load_fn: &mut F, name: &'static str) -> Option<T>
where
    F: FnMut(&'static str) -> *const c_void,
{
    let ptr = load_fn(name);
    if ptr.is_null() {
        None
    } else {
        Some(unsafe { std::mem::transmute_copy::<*const c_void, T>(&ptr) })
    }
}

/// Load optional OpenGL entry points used by upstream shader utilities but not
/// emitted by the generated `gl` bindings.
pub fn load_extra_functions<F>(load_fn: &mut F)
where
    F: FnMut(&'static str) -> *const c_void,
{
    let _ = GL_SPECIALIZE_SHADER.set(load_optional_gl_function(load_fn, "glSpecializeShader"));
    let _ = GL_GEN_PROGRAMS_ARB.set(load_optional_gl_function(load_fn, "glGenProgramsARB"));
    let _ = GL_NAMED_PROGRAM_STRING_EXT.set(load_optional_gl_function(
        load_fn,
        "glNamedProgramStringEXT",
    ));
    let _ = GL_BIND_PROGRAM_ARB.set(load_optional_gl_function(load_fn, "glBindProgramARB"));
    let _ = GL_PROGRAM_LOCAL_PARAMETER_4F_ARB.set(load_optional_gl_function(
        load_fn,
        "glProgramLocalParameter4fARB",
    ));
}

pub fn bind_assembly_program(target: u32, program: u32) {
    if let Some(bind_program) = GL_BIND_PROGRAM_ARB.get().and_then(|f| *f) {
        unsafe {
            bind_program(target, program);
        }
    } else {
        log::warn!(
            "glBindProgramARB is unavailable; cannot bind assembly program {}",
            program
        );
    }
}

pub fn program_local_parameter_4f_arb(target: u32, index: u32, x: f32, y: f32, z: f32, w: f32) {
    let program_local_parameter = GL_PROGRAM_LOCAL_PARAMETER_4F_ARB
        .get()
        .and_then(|f| *f)
        .expect("glProgramLocalParameter4fARB must be loaded for GLASM shader uniforms");
    unsafe {
        program_local_parameter(target, index, x, y, z, w);
    }
}

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

        if renderer_debug_enabled() {
            log_shader(shader, Some(code));
        }

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

        let main = CString::new("main").unwrap();
        GL_SPECIALIZE_SHADER
            .get()
            .and_then(|f| *f)
            .expect("glSpecializeShader must be loaded for SPIR-V shader programs")(
            shader,
            main.as_ptr(),
            0,
            std::ptr::null(),
            std::ptr::null(),
        );

        if renderer_debug_enabled() {
            log_shader(shader, None);
        }

        let program = link_separable_program(shader);
        gl::DeleteShader(shader);
        program
    }
}

/// Compile an assembly program (GLASM / NV_gpu_program).
///
/// Corresponds to `OpenGL::CompileProgram(std::string_view code, GLenum target)`.
///
pub fn compile_assembly_program(code: &str, target: u32) -> u32 {
    let gen_programs = GL_GEN_PROGRAMS_ARB
        .get()
        .and_then(|f| *f)
        .expect("glGenProgramsARB must be loaded for GLASM shader programs");
    let named_program_string = GL_NAMED_PROGRAM_STRING_EXT
        .get()
        .and_then(|f| *f)
        .expect("glNamedProgramStringEXT must be loaded for GLASM shader programs");

    let mut program = 0;
    unsafe {
        gen_programs(1, &mut program);
        named_program_string(
            program,
            target,
            GL_PROGRAM_FORMAT_ASCII_ARB,
            code.len() as gl::types::GLsizei,
            code.as_ptr() as *const c_void,
        );

        if renderer_debug_enabled() {
            let err = gl::GetString(GL_PROGRAM_ERROR_STRING_NV);
            if !err.is_null() {
                let err = std::ffi::CStr::from_ptr(err as *const i8);
                if !err.to_bytes().is_empty() {
                    let err = err.to_string_lossy();
                    if err.contains("error") {
                        log::error!("\n{}", err);
                        log::info!("\n{}", code);
                    } else {
                        log::warn!("\n{}", err);
                    }
                }
            }
        }
    }

    program
}

/// Link a shader into a separable program.
fn link_separable_program(shader: u32) -> u32 {
    unsafe {
        let program = gl::CreateProgram();
        gl::ProgramParameteri(program, gl::PROGRAM_SEPARABLE, gl::TRUE as i32);
        gl::AttachShader(program, shader);
        gl::LinkProgram(program);
        gl::DetachShader(program, shader);

        if !renderer_debug_enabled() {
            return program;
        }

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
                log::error!("Shader link error: {}", String::from_utf8_lossy(&log));
            }
        }

        program
    }
}

/// Log shader compilation errors/warnings.
fn log_shader(shader: u32, code: Option<&str>) {
    unsafe {
        let mut status: gl::types::GLint = 0;
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);
        if status == gl::FALSE as i32 {
            log::error!("Failed to build shader");
        }

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
                log::error!("{}", msg);
                if let Some(code) = code {
                    if !code.is_empty() {
                        log::info!("\n{}", code);
                    }
                }
            } else {
                log::warn!("{}", msg);
            }
        }
    }
}
