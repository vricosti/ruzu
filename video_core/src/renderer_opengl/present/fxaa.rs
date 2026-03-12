// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/fxaa.h and fxaa.cpp
//!
//! Fast Approximate Anti-Aliasing (FXAA) post-processing pass for OpenGL.

/// FXAA anti-aliasing pass.
///
/// Corresponds to `OpenGL::FXAA`.
pub struct FXAA {
    vert_shader: u32,
    frag_shader: u32,
    sampler: u32,
    framebuffer: u32,
    texture: u32,
}

impl FXAA {
    /// Create a new FXAA pass with the given dimensions.
    ///
    /// Corresponds to `FXAA::FXAA()`.
    pub fn new(_width: u32, _height: u32) -> Self {
        // TODO: Compile FXAA shaders, create texture and framebuffer
        Self {
            vert_shader: 0,
            frag_shader: 0,
            sampler: 0,
            framebuffer: 0,
            texture: 0,
        }
    }

    /// Execute the FXAA pass and return the output texture handle.
    ///
    /// Corresponds to `FXAA::Draw()`.
    pub fn draw(&self, _input_texture: u32) -> u32 {
        todo!("FXAA::Draw")
    }
}

impl Drop for FXAA {
    fn drop(&mut self) {
        unsafe {
            for &prog in &[self.vert_shader, self.frag_shader] {
                if prog != 0 {
                    gl::DeleteProgram(prog);
                }
            }
            if self.sampler != 0 {
                gl::DeleteSamplers(1, &self.sampler);
            }
            if self.framebuffer != 0 {
                gl::DeleteFramebuffers(1, &self.framebuffer);
            }
            if self.texture != 0 {
                gl::DeleteTextures(1, &self.texture);
            }
        }
    }
}
