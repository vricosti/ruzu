// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/smaa.h and smaa.cpp
//!
//! Subpixel Morphological Anti-Aliasing (SMAA) post-processing pass for OpenGL.

/// SMAA anti-aliasing pass.
///
/// Corresponds to `OpenGL::SMAA`.
pub struct SMAA {
    edge_detection_vert: u32,
    blending_weight_calculation_vert: u32,
    neighborhood_blending_vert: u32,
    edge_detection_frag: u32,
    blending_weight_calculation_frag: u32,
    neighborhood_blending_frag: u32,
    area_tex: u32,
    search_tex: u32,
    edges_tex: u32,
    blend_tex: u32,
    sampler: u32,
    framebuffer: u32,
    texture: u32,
}

impl SMAA {
    /// Create a new SMAA pass with the given dimensions.
    ///
    /// Corresponds to `SMAA::SMAA()`.
    ///
    /// In the full implementation, this compiles all six SMAA shaders (3 vert + 3 frag),
    /// uploads the SMAA area and search lookup textures, and creates intermediate
    /// edge/blend textures plus the final output texture + framebuffer.
    pub fn new(width: u32, height: u32) -> Self {
        let mut edges_tex: u32 = 0;
        let mut blend_tex: u32 = 0;
        let mut sampler: u32 = 0;
        let mut framebuffer: u32 = 0;
        let mut texture: u32 = 0;

        unsafe {
            // Create intermediate textures
            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut edges_tex);
            gl::TextureStorage2D(edges_tex, 1, gl::RG16F, width as i32, height as i32);

            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut blend_tex);
            gl::TextureStorage2D(blend_tex, 1, gl::RGBA16F, width as i32, height as i32);

            // Create bilinear sampler
            gl::CreateSamplers(1, &mut sampler);
            gl::SamplerParameteri(sampler, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
            gl::SamplerParameteri(sampler, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
            gl::SamplerParameteri(sampler, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as i32);
            gl::SamplerParameteri(sampler, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as i32);

            // Create framebuffer and output texture
            gl::CreateFramebuffers(1, &mut framebuffer);

            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut texture);
            gl::TextureStorage2D(texture, 1, gl::RGBA16F, width as i32, height as i32);
            gl::NamedFramebufferTexture(framebuffer, gl::COLOR_ATTACHMENT0, texture, 0);
        }

        // Shader programs and area/search textures require host shader sources
        // and embedded texture data that are compiled at build time. For now,
        // shader handles are set to 0 — the draw method guards against zero handles.
        Self {
            edge_detection_vert: 0,
            blending_weight_calculation_vert: 0,
            neighborhood_blending_vert: 0,
            edge_detection_frag: 0,
            blending_weight_calculation_frag: 0,
            neighborhood_blending_frag: 0,
            area_tex: 0,
            search_tex: 0,
            edges_tex,
            blend_tex,
            sampler,
            framebuffer,
            texture,
        }
    }

    /// Execute the three-pass SMAA pipeline and return the output texture handle.
    ///
    /// Corresponds to `SMAA::Draw()`.
    ///
    /// The three passes are:
    /// 1. Edge detection — renders edges into `edges_tex`
    /// 2. Blending weight calculation — uses edges + area/search textures
    /// 3. Neighborhood blending — final composite into output texture
    pub fn draw(&self, input_texture: u32) -> u32 {
        if self.edge_detection_vert == 0 || self.edge_detection_frag == 0 {
            // Shaders not yet compiled; return input unchanged
            return input_texture;
        }

        unsafe {
            gl::ClearColor(0.0, 0.0, 0.0, 0.0);
            gl::FrontFace(gl::CCW);
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, self.framebuffer);
            gl::BindSampler(0, self.sampler);
            gl::BindSampler(1, self.sampler);
            gl::BindSampler(2, self.sampler);

            // Pass 1: Edge detection
            gl::BindTextureUnit(0, input_texture);
            gl::NamedFramebufferTexture(
                self.framebuffer,
                gl::COLOR_ATTACHMENT0,
                self.edges_tex,
                0,
            );
            gl::Clear(gl::COLOR_BUFFER_BIT);
            // In full impl: program_manager.BindPresentPrograms(edge_detection_vert, edge_detection_frag);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);

            // Pass 2: Blending weight calculation
            gl::BindTextureUnit(0, self.edges_tex);
            gl::BindTextureUnit(1, self.area_tex);
            gl::BindTextureUnit(2, self.search_tex);
            gl::NamedFramebufferTexture(
                self.framebuffer,
                gl::COLOR_ATTACHMENT0,
                self.blend_tex,
                0,
            );
            gl::Clear(gl::COLOR_BUFFER_BIT);
            // In full impl: program_manager.BindPresentPrograms(blending_weight_calculation_vert, blending_weight_calculation_frag);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);

            // Pass 3: Neighborhood blending
            gl::BindTextureUnit(0, input_texture);
            gl::BindTextureUnit(1, self.blend_tex);
            gl::NamedFramebufferTexture(
                self.framebuffer,
                gl::COLOR_ATTACHMENT0,
                self.texture,
                0,
            );
            // In full impl: program_manager.BindPresentPrograms(neighborhood_blending_vert, neighborhood_blending_frag);
            gl::Clear(gl::COLOR_BUFFER_BIT);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);
            gl::FrontFace(gl::CW);
        }

        self.texture
    }
}

impl Drop for SMAA {
    fn drop(&mut self) {
        unsafe {
            for &prog in &[
                self.edge_detection_vert,
                self.blending_weight_calculation_vert,
                self.neighborhood_blending_vert,
                self.edge_detection_frag,
                self.blending_weight_calculation_frag,
                self.neighborhood_blending_frag,
            ] {
                if prog != 0 {
                    gl::DeleteProgram(prog);
                }
            }
            for &tex in &[
                self.area_tex,
                self.search_tex,
                self.edges_tex,
                self.blend_tex,
                self.texture,
            ] {
                if tex != 0 {
                    gl::DeleteTextures(1, &tex);
                }
            }
            if self.sampler != 0 {
                gl::DeleteSamplers(1, &self.sampler);
            }
            if self.framebuffer != 0 {
                gl::DeleteFramebuffers(1, &self.framebuffer);
            }
        }
    }
}
