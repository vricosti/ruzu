// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/fsr.h and fsr.cpp
//!
//! AMD FidelityFX Super Resolution (FSR) upscaling pass for OpenGL.

/// FSR upscaling pass.
///
/// Corresponds to `OpenGL::FSR`.
pub struct FSR {
    pub width: u32,
    pub height: u32,
    framebuffer: u32,
    sampler: u32,
    vert: u32,
    easu_frag: u32,
    rcas_frag: u32,
    easu_tex: u32,
    rcas_tex: u32,
}

impl FSR {
    /// Create a new FSR pass with the given output dimensions.
    ///
    /// Corresponds to `FSR::FSR()`.
    ///
    /// In the full implementation, this compiles the FSR EASU and RCAS fragment
    /// shaders (with included ffx_a.h and ffx_fsr1.h sources), creates the
    /// bilinear sampler, framebuffer, and two RGBA16F intermediate textures
    /// at the output resolution.
    pub fn new(output_width: u32, output_height: u32) -> Self {
        let mut sampler: u32 = 0;
        let mut framebuffer: u32 = 0;
        let mut easu_tex: u32 = 0;
        let mut rcas_tex: u32 = 0;

        unsafe {
            // Create bilinear sampler
            gl::CreateSamplers(1, &mut sampler);
            gl::SamplerParameteri(sampler, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
            gl::SamplerParameteri(sampler, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
            gl::SamplerParameteri(sampler, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as i32);
            gl::SamplerParameteri(sampler, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as i32);

            // Create framebuffer
            gl::CreateFramebuffers(1, &mut framebuffer);

            // Create EASU intermediate texture
            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut easu_tex);
            gl::TextureStorage2D(
                easu_tex,
                1,
                gl::RGBA16F,
                output_width as i32,
                output_height as i32,
            );

            // Create RCAS output texture
            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut rcas_tex);
            gl::TextureStorage2D(
                rcas_tex,
                1,
                gl::RGBA16F,
                output_width as i32,
                output_height as i32,
            );
        }

        // Shader programs require FSR host shader sources compiled at build time.
        // For now, shader handles are set to 0 — the draw method guards against this.
        Self {
            width: output_width,
            height: output_height,
            framebuffer,
            sampler,
            vert: 0,
            easu_frag: 0,
            rcas_frag: 0,
            easu_tex,
            rcas_tex,
        }
    }

    /// Execute the FSR pass and return the output texture handle.
    ///
    /// Corresponds to `FSR::Draw()`.
    ///
    /// The two passes are:
    /// 1. EASU (Edge Adaptive Spatial Upsampling) — scales the input to output resolution
    /// 2. RCAS (Robust Contrast Adaptive Sharpening) — sharpens the upscaled result
    pub fn draw(
        &self,
        input_texture: u32,
        input_image_width: u32,
        input_image_height: u32,
    ) -> u32 {
        if self.vert == 0 || self.easu_frag == 0 || self.rcas_frag == 0 {
            // Shaders not yet compiled; return input unchanged
            return input_texture;
        }

        let output_width = self.width as f32;
        let output_height = self.height as f32;

        unsafe {
            gl::FrontFace(gl::CW);
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, self.framebuffer);

            // Pass 1: EASU upscaling
            gl::NamedFramebufferTexture(
                self.framebuffer,
                gl::COLOR_ATTACHMENT0,
                self.easu_tex,
                0,
            );
            gl::ViewportIndexedf(0, 0.0, 0.0, output_width, output_height);
            // In full impl: program_manager.BindPresentPrograms(vert, easu_frag);
            gl::BindTextureUnit(0, input_texture);
            gl::BindSampler(0, self.sampler);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);

            // Pass 2: RCAS sharpening
            gl::NamedFramebufferTexture(
                self.framebuffer,
                gl::COLOR_ATTACHMENT0,
                self.rcas_tex,
                0,
            );
            // In full impl: program_manager.BindPresentPrograms(vert, rcas_frag);
            gl::BindTextureUnit(0, self.easu_tex);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);
        }

        self.rcas_tex
    }

    /// Check if the FSR pass needs to be recreated for new screen dimensions.
    ///
    /// Corresponds to `FSR::NeedsRecreation()`.
    pub fn needs_recreation(&self, screen_width: u32, screen_height: u32) -> bool {
        screen_width != self.width || screen_height != self.height
    }
}

impl Drop for FSR {
    fn drop(&mut self) {
        unsafe {
            if self.framebuffer != 0 {
                gl::DeleteFramebuffers(1, &self.framebuffer);
            }
            if self.sampler != 0 {
                gl::DeleteSamplers(1, &self.sampler);
            }
            for &prog in &[self.vert, self.easu_frag, self.rcas_frag] {
                if prog != 0 {
                    gl::DeleteProgram(prog);
                }
            }
            for &tex in &[self.easu_tex, self.rcas_tex] {
                if tex != 0 {
                    gl::DeleteTextures(1, &tex);
                }
            }
        }
    }
}
