// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_resource_manager.h
//! Status: EN COURS
//!
//! RAII wrappers for OpenGL resources (textures, buffers, framebuffers, etc.).

/// RAII wrapper for an OpenGL texture object.
pub struct OGLTexture {
    pub handle: gl::types::GLuint,
}

impl OGLTexture {
    pub fn new() -> Self {
        Self { handle: 0 }
    }

    pub fn create(&mut self) {
        if self.handle == 0 {
            unsafe {
                gl::GenTextures(1, &mut self.handle);
            }
        }
    }

    pub fn release(&mut self) {
        if self.handle != 0 {
            unsafe {
                gl::DeleteTextures(1, &self.handle);
            }
            self.handle = 0;
        }
    }
}

impl Drop for OGLTexture {
    fn drop(&mut self) {
        self.release();
    }
}

/// RAII wrapper for an OpenGL buffer object.
pub struct OGLBuffer {
    pub handle: gl::types::GLuint,
}

impl OGLBuffer {
    pub fn new() -> Self {
        Self { handle: 0 }
    }

    pub fn create(&mut self) {
        if self.handle == 0 {
            unsafe {
                gl::GenBuffers(1, &mut self.handle);
            }
        }
    }

    pub fn release(&mut self) {
        if self.handle != 0 {
            unsafe {
                gl::DeleteBuffers(1, &self.handle);
            }
            self.handle = 0;
        }
    }
}

impl Drop for OGLBuffer {
    fn drop(&mut self) {
        self.release();
    }
}

/// RAII wrapper for an OpenGL framebuffer object.
pub struct OGLFramebuffer {
    pub handle: gl::types::GLuint,
}

impl OGLFramebuffer {
    pub fn new() -> Self {
        Self { handle: 0 }
    }

    pub fn create(&mut self) {
        if self.handle == 0 {
            unsafe {
                gl::GenFramebuffers(1, &mut self.handle);
            }
        }
    }

    pub fn release(&mut self) {
        if self.handle != 0 {
            unsafe {
                gl::DeleteFramebuffers(1, &self.handle);
            }
            self.handle = 0;
        }
    }
}

impl Drop for OGLFramebuffer {
    fn drop(&mut self) {
        self.release();
    }
}

/// RAII wrapper for an OpenGL renderbuffer object.
pub struct OGLRenderbuffer {
    pub handle: gl::types::GLuint,
}

impl OGLRenderbuffer {
    pub fn new() -> Self {
        Self { handle: 0 }
    }

    pub fn create(&mut self) {
        if self.handle == 0 {
            unsafe {
                gl::GenRenderbuffers(1, &mut self.handle);
            }
        }
    }

    pub fn release(&mut self) {
        if self.handle != 0 {
            unsafe {
                gl::DeleteRenderbuffers(1, &self.handle);
            }
            self.handle = 0;
        }
    }
}

impl Drop for OGLRenderbuffer {
    fn drop(&mut self) {
        self.release();
    }
}

/// RAII wrapper for an OpenGL shader program.
pub struct OGLProgram {
    pub handle: gl::types::GLuint,
}

impl OGLProgram {
    pub fn new() -> Self {
        Self { handle: 0 }
    }

    pub fn create_from_source(
        vertex_src: &str,
        fragment_src: &str,
    ) -> Result<Self, String> {
        unsafe {
            let vs = compile_shader(gl::VERTEX_SHADER, vertex_src)?;
            let fs = compile_shader(gl::FRAGMENT_SHADER, fragment_src)?;

            let program = gl::CreateProgram();
            gl::AttachShader(program, vs);
            gl::AttachShader(program, fs);
            gl::LinkProgram(program);

            let mut success: gl::types::GLint = 0;
            gl::GetProgramiv(program, gl::LINK_STATUS, &mut success);
            if success == 0 {
                let mut len: gl::types::GLint = 0;
                gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut len);
                let mut buf = vec![0u8; len as usize];
                gl::GetProgramInfoLog(
                    program,
                    len,
                    std::ptr::null_mut(),
                    buf.as_mut_ptr() as *mut _,
                );
                let msg = String::from_utf8_lossy(&buf).to_string();
                gl::DeleteProgram(program);
                gl::DeleteShader(vs);
                gl::DeleteShader(fs);
                return Err(format!("Program link error: {}", msg));
            }

            gl::DeleteShader(vs);
            gl::DeleteShader(fs);

            Ok(Self { handle: program })
        }
    }

    pub fn release(&mut self) {
        if self.handle != 0 {
            unsafe {
                gl::DeleteProgram(self.handle);
            }
            self.handle = 0;
        }
    }
}

impl Drop for OGLProgram {
    fn drop(&mut self) {
        self.release();
    }
}

/// RAII wrapper for an OpenGL vertex array object.
pub struct OGLVertexArray {
    pub handle: gl::types::GLuint,
}

impl OGLVertexArray {
    pub fn new() -> Self {
        Self { handle: 0 }
    }

    pub fn create(&mut self) {
        if self.handle == 0 {
            unsafe {
                gl::GenVertexArrays(1, &mut self.handle);
            }
        }
    }

    pub fn release(&mut self) {
        if self.handle != 0 {
            unsafe {
                gl::DeleteVertexArrays(1, &self.handle);
            }
            self.handle = 0;
        }
    }
}

impl Drop for OGLVertexArray {
    fn drop(&mut self) {
        self.release();
    }
}

/// RAII wrapper for an OpenGL sampler object.
pub struct OGLSampler {
    pub handle: gl::types::GLuint,
}

impl OGLSampler {
    pub fn new() -> Self {
        Self { handle: 0 }
    }

    pub fn create(&mut self) {
        if self.handle == 0 {
            unsafe {
                gl::GenSamplers(1, &mut self.handle);
            }
        }
    }

    pub fn release(&mut self) {
        if self.handle != 0 {
            unsafe {
                gl::DeleteSamplers(1, &self.handle);
            }
            self.handle = 0;
        }
    }
}

impl Drop for OGLSampler {
    fn drop(&mut self) {
        self.release();
    }
}

// --- Helpers ---

unsafe fn compile_shader(
    shader_type: gl::types::GLenum,
    source: &str,
) -> Result<gl::types::GLuint, String> {
    let shader = gl::CreateShader(shader_type);
    let c_str = std::ffi::CString::new(source).unwrap();
    gl::ShaderSource(shader, 1, &c_str.as_ptr(), std::ptr::null());
    gl::CompileShader(shader);

    let mut success: gl::types::GLint = 0;
    gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut success);
    if success == 0 {
        let mut len: gl::types::GLint = 0;
        gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut len);
        let mut buf = vec![0u8; len as usize];
        gl::GetShaderInfoLog(
            shader,
            len,
            std::ptr::null_mut(),
            buf.as_mut_ptr() as *mut _,
        );
        let msg = String::from_utf8_lossy(&buf).to_string();
        gl::DeleteShader(shader);
        return Err(format!("Shader compile error: {}", msg));
    }
    Ok(shader)
}
