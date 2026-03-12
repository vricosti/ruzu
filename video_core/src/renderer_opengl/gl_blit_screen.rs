// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_blit_screen.h and gl_blit_screen.cpp
//! Status: EN COURS
//!
//! Handles compositing framebuffers to the screen using a textured fullscreen quad.
//! This is the final stage of the OpenGL rendering pipeline.

use log::info;

use super::gl_resource_manager::{OGLProgram, OGLSampler, OGLTexture, OGLVertexArray};

const BLIT_VERTEX_SHADER: &str = r#"
#version 430 core

layout (location = 0) out vec2 frag_tex_coord;

// Fullscreen triangle (no vertex buffer needed)
void main() {
    float x = -1.0 + float((gl_VertexID & 1) << 2);
    float y = -1.0 + float((gl_VertexID & 2) << 1);
    frag_tex_coord = vec2((x + 1.0) * 0.5, (y + 1.0) * 0.5);
    gl_Position = vec4(x, y, 0.0, 1.0);
}
"#;

const BLIT_FRAGMENT_SHADER: &str = r#"
#version 430 core

layout (location = 0) in vec2 frag_tex_coord;
layout (location = 0) out vec4 color;

layout (binding = 0) uniform sampler2D screen_texture;

void main() {
    color = texture(screen_texture, frag_tex_coord);
}
"#;

/// BlitScreen handles the final frame composition to the window.
///
/// Corresponds to zuyu's `BlitScreen` class.
pub struct BlitScreen {
    program: OGLProgram,
    vao: OGLVertexArray,
    texture: OGLTexture,
    sampler: OGLSampler,
    /// Current framebuffer texture dimensions.
    current_width: u32,
    current_height: u32,
}

impl BlitScreen {
    /// Create a new BlitScreen with compiled shaders and GL resources.
    pub fn new() -> Result<Self, String> {
        let program = OGLProgram::create_from_source(BLIT_VERTEX_SHADER, BLIT_FRAGMENT_SHADER)?;

        let mut vao = OGLVertexArray::new();
        vao.create();

        let mut texture = OGLTexture::new();
        texture.create();

        let mut sampler = OGLSampler::new();
        sampler.create();

        // Configure sampler: linear filtering, clamp to edge
        unsafe {
            gl::SamplerParameteri(sampler.handle, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
            gl::SamplerParameteri(sampler.handle, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
            gl::SamplerParameteri(
                sampler.handle,
                gl::TEXTURE_WRAP_S,
                gl::CLAMP_TO_EDGE as i32,
            );
            gl::SamplerParameteri(
                sampler.handle,
                gl::TEXTURE_WRAP_T,
                gl::CLAMP_TO_EDGE as i32,
            );
        }

        info!("BlitScreen: OpenGL blit pipeline created");

        Ok(Self {
            program,
            vao,
            texture,
            sampler,
            current_width: 0,
            current_height: 0,
        })
    }

    /// Draw the given RGBA8 framebuffer to the current viewport.
    ///
    /// `pixels` must be width*height*4 bytes in RGBA8888 format.
    /// `viewport_width`/`viewport_height` is the window size.
    pub fn draw_screen(
        &mut self,
        pixels: &[u8],
        fb_width: u32,
        fb_height: u32,
        viewport_width: u32,
        viewport_height: u32,
    ) {
        if pixels.len() < (fb_width * fb_height * 4) as usize {
            return;
        }

        unsafe {
            // Update texture data
            gl::BindTexture(gl::TEXTURE_2D, self.texture.handle);

            if fb_width != self.current_width || fb_height != self.current_height {
                // Allocate new texture storage
                gl::TexImage2D(
                    gl::TEXTURE_2D,
                    0,
                    gl::RGBA8 as i32,
                    fb_width as i32,
                    fb_height as i32,
                    0,
                    gl::RGBA,
                    gl::UNSIGNED_BYTE,
                    pixels.as_ptr() as *const _,
                );
                self.current_width = fb_width;
                self.current_height = fb_height;
            } else {
                // Update existing texture
                gl::TexSubImage2D(
                    gl::TEXTURE_2D,
                    0,
                    0,
                    0,
                    fb_width as i32,
                    fb_height as i32,
                    gl::RGBA,
                    gl::UNSIGNED_BYTE,
                    pixels.as_ptr() as *const _,
                );
            }

            // Set up rendering state
            gl::Viewport(0, 0, viewport_width as i32, viewport_height as i32);
            gl::Clear(gl::COLOR_BUFFER_BIT);

            // Bind program, VAO, texture, and sampler
            gl::UseProgram(self.program.handle);
            gl::BindVertexArray(self.vao.handle);
            gl::ActiveTexture(gl::TEXTURE0);
            gl::BindTexture(gl::TEXTURE_2D, self.texture.handle);
            gl::BindSampler(0, self.sampler.handle);

            // Disable depth test and blending for final blit
            gl::Disable(gl::DEPTH_TEST);
            gl::Disable(gl::BLEND);
            gl::Disable(gl::CULL_FACE);
            gl::Disable(gl::STENCIL_TEST);
            gl::Disable(gl::SCISSOR_TEST);
            gl::ColorMask(gl::TRUE, gl::TRUE, gl::TRUE, gl::TRUE);

            // Draw fullscreen triangle (3 vertices, no VBO)
            gl::DrawArrays(gl::TRIANGLES, 0, 3);

            // Unbind
            gl::BindSampler(0, 0);
            gl::BindVertexArray(0);
            gl::UseProgram(0);
        }
    }
}
