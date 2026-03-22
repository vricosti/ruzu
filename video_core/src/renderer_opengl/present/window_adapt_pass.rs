// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/window_adapt_pass.h and
//! window_adapt_pass.cpp
//!
//! Window adapt pass -- final composition of layers onto the window framebuffer
//! using a configurable fragment shader (filter).

use super::layer::Layer;
use super::present_uniforms::ScreenRectVertex;
use crate::framebuffer_config::{BlendMode, FramebufferConfig};
use ruzu_core::frontend::framebuffer_layout::FramebufferLayout;

/// Vertex shader for the present pass.
///
/// Port of `HostShaders::OPENGL_PRESENT_VERT`.
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

        // Create vertex buffer and VAO for quad vertices.
        // Port of upstream constructor: CreateBuffers, NamedBufferData, vertex attrib setup.
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
        }
    }

    /// Draw all layers to the current framebuffer.
    ///
    /// Port of `WindowAdaptPass::DrawToFramebuffer()`.
    ///
    /// Upstream flow:
    /// 1. Save current framebuffer bindings
    /// 2. For each layer: ConfigureDraw → get texture, matrix, vertices
    /// 3. Restore framebuffer bindings
    /// 4. Bind present program, set state
    /// 5. For each layer: set blending, bind texture, upload matrix + vertices, draw
    pub fn draw_to_framebuffer(
        &self,
        layers: &mut [Layer],
        framebuffers: &[FramebufferConfig],
        layout: &FramebufferLayout,
        invert_y: bool,
    ) {
        if self.program == 0 {
            return;
        }

        let layer_count = framebuffers.len();

        // Save current framebuffer bindings (upstream saves and restores).
        let (old_read_fb, old_draw_fb) = unsafe {
            let mut read_fb: i32 = 0;
            let mut draw_fb: i32 = 0;
            gl::GetIntegerv(gl::READ_FRAMEBUFFER_BINDING, &mut read_fb);
            gl::GetIntegerv(gl::DRAW_FRAMEBUFFER_BINDING, &mut draw_fb);
            (read_fb, draw_fb)
        };

        // Phase 1: Configure all layers (may modify framebuffer bindings internally).
        let mut textures = Vec::with_capacity(layer_count);
        let mut matrices = Vec::with_capacity(layer_count);
        let mut vertices = Vec::with_capacity(layer_count);

        for i in 0..layer_count {
            let mut matrix = [0.0f32; 6];
            let mut verts = [ScreenRectVertex::default(); 4];
            let texture =
                layers[i].configure_draw(&mut matrix, &mut verts, &framebuffers[i], layout, invert_y);
            textures.push(texture);
            matrices.push(matrix);
            vertices.push(verts);
        }

        // Restore framebuffer bindings.
        unsafe {
            gl::BindFramebuffer(gl::READ_FRAMEBUFFER, old_read_fb as u32);
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, old_draw_fb as u32);
        }

        // Phase 2: Render all layers.
        unsafe {
            gl::UseProgram(self.program);
            gl::BindVertexArray(self.vao);
            gl::BindSampler(0, self.sampler);

            gl::Disable(gl::FRAMEBUFFER_SRGB);
            gl::ViewportIndexedf(
                0,
                0.0,
                0.0,
                layout.width as f32,
                layout.height as f32,
            );

            // Clear screen to background color.
            // Upstream reads from Settings::values.bg_red/green/blue.
            // Default to cornflower blue (matching common emulator convention).
            gl::ClearColor(0.39, 0.58, 0.93, 1.0);
            gl::Clear(gl::COLOR_BUFFER_BIT);

            for i in 0..layer_count {
                // Set blending mode per-layer.
                // Port of upstream switch on framebuffers[i].blending.
                match framebuffers[i].blending {
                    BlendMode::Opaque => {
                        gl::Disablei(gl::BLEND, 0);
                    }
                    BlendMode::Premultiplied => {
                        gl::Enablei(gl::BLEND, 0);
                        gl::BlendFuncSeparatei(
                            0,
                            gl::ONE,
                            gl::ONE_MINUS_SRC_ALPHA,
                            gl::ONE,
                            gl::ZERO,
                        );
                    }
                    BlendMode::Coverage => {
                        gl::Enablei(gl::BLEND, 0);
                        gl::BlendFuncSeparatei(
                            0,
                            gl::SRC_ALPHA,
                            gl::ONE_MINUS_SRC_ALPHA,
                            gl::ONE,
                            gl::ZERO,
                        );
                    }
                }

                // Bind texture.
                gl::BindTextureUnit(0, textures[i]);

                // Upload orthographic matrix (location 0, mat3x2).
                gl::ProgramUniformMatrix3x2fv(
                    self.program,
                    0,
                    1,
                    gl::FALSE,
                    matrices[i].as_ptr(),
                );

                // Upload vertex data to VBO.
                gl::NamedBufferSubData(
                    self.vertex_buffer,
                    0,
                    (4 * std::mem::size_of::<ScreenRectVertex>()) as isize,
                    vertices[i].as_ptr() as *const _,
                );

                // Draw quad as triangle strip.
                gl::DrawArrays(gl::TRIANGLE_STRIP, 0, 4);
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
