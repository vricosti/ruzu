// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/fxaa.h and fxaa.cpp
//!
//! Fast Approximate Anti-Aliasing (FXAA) post-processing pass for OpenGL.

use super::util::create_bilinear_sampler;
use crate::host_shaders::fragment_shaders::FXAA_FRAG;
use crate::host_shaders::vertex_shaders::FXAA_VERT;
use crate::renderer_opengl::gl_shader_manager::ProgramManager;
use crate::renderer_opengl::gl_shader_util::create_program_from_source;

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
    ///
    /// In the full implementation, this compiles the FXAA vertex and fragment
    /// shaders from host shader sources, creates a bilinear sampler, and
    /// allocates a RGBA16F texture + framebuffer at the given dimensions.
    pub fn new(width: u32, height: u32) -> Self {
        let mut framebuffer: u32 = 0;
        let mut texture: u32 = 0;

        let vert_shader = create_program_from_source(FXAA_VERT, gl::VERTEX_SHADER);
        let frag_shader = create_program_from_source(FXAA_FRAG, gl::FRAGMENT_SHADER);
        let sampler = create_bilinear_sampler();

        unsafe {
            // Create output texture
            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut texture);
            gl::TextureStorage2D(texture, 1, gl::RGBA16F, width as i32, height as i32);

            // Create framebuffer
            gl::CreateFramebuffers(1, &mut framebuffer);
            gl::NamedFramebufferTexture(framebuffer, gl::COLOR_ATTACHMENT0, texture, 0);
        }

        Self {
            vert_shader,
            frag_shader,
            sampler,
            framebuffer,
            texture,
        }
    }

    /// Execute the FXAA pass and return the output texture handle.
    ///
    /// Corresponds to `FXAA::Draw()`.
    ///
    /// Binds the FXAA shader pipeline, renders a full-screen triangle with the
    /// input texture, and returns the anti-aliased output texture.
    pub fn draw(&self, program_manager: &mut ProgramManager, input_texture: u32) -> u32 {
        unsafe {
            gl::FrontFace(gl::CCW);

            program_manager.bind_present_programs(self.vert_shader, self.frag_shader);
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, self.framebuffer);
            gl::BindTextureUnit(0, input_texture);
            gl::BindSampler(0, self.sampler);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);
            gl::FrontFace(gl::CW);
        }

        self.texture
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
