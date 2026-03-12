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
    ///
    /// In the full implementation, this compiles the FXAA vertex and fragment
    /// shaders from host shader sources, creates a bilinear sampler, and
    /// allocates a RGBA16F texture + framebuffer at the given dimensions.
    pub fn new(width: u32, height: u32) -> Self {
        let mut sampler: u32 = 0;
        let mut framebuffer: u32 = 0;
        let mut texture: u32 = 0;

        unsafe {
            // Create bilinear sampler
            gl::CreateSamplers(1, &mut sampler);
            gl::SamplerParameteri(sampler, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
            gl::SamplerParameteri(sampler, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
            gl::SamplerParameteri(sampler, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as i32);
            gl::SamplerParameteri(sampler, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as i32);

            // Create output texture
            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut texture);
            gl::TextureStorage2D(texture, 1, gl::RGBA16F, width as i32, height as i32);

            // Create framebuffer
            gl::CreateFramebuffers(1, &mut framebuffer);
            gl::NamedFramebufferTexture(framebuffer, gl::COLOR_ATTACHMENT0, texture, 0);
        }

        // Shader programs require host shader sources (FXAA_VERT, FXAA_FRAG);
        // these are compiled at build time. For now, set to 0 — the draw method
        // guards against zero handles.
        Self {
            vert_shader: 0,
            frag_shader: 0,
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
    pub fn draw(&self, input_texture: u32) -> u32 {
        if self.vert_shader == 0 || self.frag_shader == 0 {
            // Shaders not yet compiled; return input unchanged
            return input_texture;
        }

        unsafe {
            gl::FrontFace(gl::CCW);

            // Bind pipeline (in full impl, uses ProgramManager::BindPresentPrograms)
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
