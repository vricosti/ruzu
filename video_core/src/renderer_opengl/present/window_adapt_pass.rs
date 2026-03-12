// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/window_adapt_pass.h and
//! window_adapt_pass.cpp
//!
//! Window adapt pass -- final composition of layers onto the window framebuffer
//! using a configurable fragment shader (filter).

use super::present_uniforms::ScreenRectVertex;

/// Vertex shader for the present pass.
const OPENGL_PRESENT_VERT: &str = r#"#version 460
layout(location = 0) in vec2 position;
layout(location = 1) in vec2 tex_coord;
layout(location = 0) out vec2 frag_tex_coord;
layout(location = 0) uniform mat3x2 model_view_matrix;
void main() {
    frag_tex_coord = tex_coord;
    gl_Position = vec4(model_view_matrix * vec3(position, 1.0), 0.0, 1.0);
}
"#;

/// Compile a shader from source.
fn compile_shader(source: &str, shader_type: u32) -> u32 {
    unsafe {
        let shader = gl::CreateShader(shader_type);
        let c_source = std::ffi::CString::new(source).unwrap();
        let ptr = c_source.as_ptr();
        gl::ShaderSource(shader, 1, &ptr, std::ptr::null());
        gl::CompileShader(shader);

        let mut success: i32 = 0;
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut success);
        if success == 0 {
            let mut log_len: i32 = 0;
            gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut log_len);
            let mut log_buf = vec![0u8; log_len as usize];
            gl::GetShaderInfoLog(
                shader,
                log_len,
                std::ptr::null_mut(),
                log_buf.as_mut_ptr() as *mut i8,
            );
            log::error!(
                "Shader compilation failed: {}",
                String::from_utf8_lossy(&log_buf)
            );
            gl::DeleteShader(shader);
            return 0;
        }
        shader
    }
}

/// Link a program from vertex and fragment shaders.
fn link_program(vert: u32, frag: u32) -> u32 {
    unsafe {
        let program = gl::CreateProgram();
        gl::AttachShader(program, vert);
        gl::AttachShader(program, frag);
        gl::LinkProgram(program);

        let mut success: i32 = 0;
        gl::GetProgramiv(program, gl::LINK_STATUS, &mut success);
        if success == 0 {
            let mut log_len: i32 = 0;
            gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut log_len);
            let mut log_buf = vec![0u8; log_len as usize];
            gl::GetProgramInfoLog(
                program,
                log_len,
                std::ptr::null_mut(),
                log_buf.as_mut_ptr() as *mut i8,
            );
            log::error!(
                "Program linking failed: {}",
                String::from_utf8_lossy(&log_buf)
            );
            gl::DeleteProgram(program);
            return 0;
        }
        program
    }
}

/// Window adapt pass for final framebuffer composition.
///
/// Corresponds to `OpenGL::WindowAdaptPass`.
pub struct WindowAdaptPass {
    sampler: u32,
    program: u32,
    vertex_buffer: u32,
    vao: u32,
    /// GPU address of the vertex buffer (NV unified memory).
    vertex_buffer_address: u64,
}

impl WindowAdaptPass {
    /// Create a new window adapt pass with the given sampler and fragment shader.
    ///
    /// Port of `WindowAdaptPass::WindowAdaptPass()`.
    pub fn new(sampler: u32, frag_source: &str) -> Self {
        let vert = compile_shader(OPENGL_PRESENT_VERT, gl::VERTEX_SHADER);
        let frag = compile_shader(frag_source, gl::FRAGMENT_SHADER);
        let program = if vert != 0 && frag != 0 {
            link_program(vert, frag)
        } else {
            0
        };
        // Clean up individual shaders after linking
        unsafe {
            if vert != 0 {
                gl::DeleteShader(vert);
            }
            if frag != 0 {
                gl::DeleteShader(frag);
            }
        }

        // Create vertex buffer for quad vertices
        let mut vertex_buffer: u32 = 0;
        let mut vao: u32 = 0;
        unsafe {
            gl::CreateBuffers(1, &mut vertex_buffer);
            gl::NamedBufferStorage(
                vertex_buffer,
                (4 * std::mem::size_of::<ScreenRectVertex>()) as isize,
                std::ptr::null(),
                gl::DYNAMIC_STORAGE_BIT,
            );

            gl::CreateVertexArrays(1, &mut vao);
            gl::VertexArrayVertexBuffer(
                vao,
                0,
                vertex_buffer,
                0,
                std::mem::size_of::<ScreenRectVertex>() as i32,
            );

            // position attribute (location 0)
            gl::EnableVertexArrayAttrib(vao, 0);
            gl::VertexArrayAttribFormat(vao, 0, 2, gl::FLOAT, gl::FALSE, 0);
            gl::VertexArrayAttribBinding(vao, 0, 0);

            // tex_coord attribute (location 1)
            gl::EnableVertexArrayAttrib(vao, 1);
            gl::VertexArrayAttribFormat(vao, 1, 2, gl::FLOAT, gl::FALSE, 8);
            gl::VertexArrayAttribBinding(vao, 1, 0);
        }

        Self {
            sampler,
            program,
            vertex_buffer,
            vao,
            vertex_buffer_address: 0,
        }
    }

    /// Draw all layers to the current framebuffer.
    ///
    /// Port of `WindowAdaptPass::DrawToFramebuffer()`.
    pub fn draw_to_framebuffer(
        &self,
        textures: &[u32],
        matrices: &[[f32; 6]],
        vertices: &[[ScreenRectVertex; 4]],
        layout_width: u32,
        layout_height: u32,
    ) {
        if self.program == 0 {
            return;
        }

        unsafe {
            gl::UseProgram(self.program);
            gl::BindVertexArray(self.vao);
            gl::BindSampler(0, self.sampler);

            gl::Disable(gl::FRAMEBUFFER_SRGB);
            gl::Viewport(0, 0, layout_width as i32, layout_height as i32);

            for (i, ((texture, matrix), verts)) in
                textures.iter().zip(matrices.iter()).zip(vertices.iter()).enumerate()
            {
                // Upload vertex data
                gl::NamedBufferSubData(
                    self.vertex_buffer,
                    0,
                    (4 * std::mem::size_of::<ScreenRectVertex>()) as isize,
                    verts.as_ptr() as *const _,
                );

                // Set model-view matrix uniform (location 0, mat3x2)
                gl::UniformMatrix3x2fv(0, 1, gl::FALSE, matrix.as_ptr());

                // Bind texture
                gl::BindTextureUnit(0, *texture);

                // Draw quad as triangle strip
                gl::DrawArrays(gl::TRIANGLE_STRIP, 0, 4);

                let _ = i;
            }

            gl::BindVertexArray(0);
        }
    }
}

impl Drop for WindowAdaptPass {
    fn drop(&mut self) {
        unsafe {
            if self.program != 0 {
                gl::DeleteProgram(self.program);
            }
            if self.sampler != 0 {
                gl::DeleteSamplers(1, &self.sampler);
            }
            if self.vertex_buffer != 0 {
                gl::DeleteBuffers(1, &self.vertex_buffer);
            }
            if self.vao != 0 {
                gl::DeleteVertexArrays(1, &self.vao);
            }
        }
    }
}
