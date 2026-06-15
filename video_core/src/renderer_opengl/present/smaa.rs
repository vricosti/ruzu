// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/smaa.h and smaa.cpp
//!
//! Subpixel Morphological Anti-Aliasing (SMAA) post-processing pass for OpenGL.

use super::util::{create_bilinear_sampler, replace_include};
use crate::host_shaders::fragment_shaders::{
    SMAA_BLENDING_WEIGHT_CALCULATION_FRAG, SMAA_EDGE_DETECTION_FRAG,
    SMAA_NEIGHBORHOOD_BLENDING_FRAG,
};
use crate::host_shaders::glsl_includes::OPENGL_SMAA_GLSL;
use crate::host_shaders::vertex_shaders::{
    SMAA_BLENDING_WEIGHT_CALCULATION_VERT, SMAA_EDGE_DETECTION_VERT,
    SMAA_NEIGHBORHOOD_BLENDING_VERT,
};
use crate::renderer_opengl::gl_shader_manager::ProgramManager;
use crate::renderer_opengl::gl_shader_util::create_program_from_source;
use crate::smaa_area_tex::{AREATEX_HEIGHT, AREATEX_WIDTH, AREA_TEX_BYTES};
use crate::smaa_search_tex::{SEARCHTEX_HEIGHT, SEARCHTEX_WIDTH, SEARCH_TEX_BYTES};

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
        let edge_detection_vert = smaa_shader(SMAA_EDGE_DETECTION_VERT, gl::VERTEX_SHADER);
        let edge_detection_frag = smaa_shader(SMAA_EDGE_DETECTION_FRAG, gl::FRAGMENT_SHADER);
        let blending_weight_calculation_vert =
            smaa_shader(SMAA_BLENDING_WEIGHT_CALCULATION_VERT, gl::VERTEX_SHADER);
        let blending_weight_calculation_frag =
            smaa_shader(SMAA_BLENDING_WEIGHT_CALCULATION_FRAG, gl::FRAGMENT_SHADER);
        let neighborhood_blending_vert =
            smaa_shader(SMAA_NEIGHBORHOOD_BLENDING_VERT, gl::VERTEX_SHADER);
        let neighborhood_blending_frag =
            smaa_shader(SMAA_NEIGHBORHOOD_BLENDING_FRAG, gl::FRAGMENT_SHADER);

        let mut area_tex: u32 = 0;
        let mut search_tex: u32 = 0;
        let mut edges_tex: u32 = 0;
        let mut blend_tex: u32 = 0;
        let mut framebuffer: u32 = 0;
        let mut texture: u32 = 0;

        unsafe {
            gl::BindBuffer(gl::PIXEL_UNPACK_BUFFER, 0);
            gl::PixelStorei(gl::UNPACK_ROW_LENGTH, 0);

            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut area_tex);
            gl::TextureStorage2D(
                area_tex,
                1,
                gl::RG8,
                AREATEX_WIDTH as i32,
                AREATEX_HEIGHT as i32,
            );
            gl::TextureSubImage2D(
                area_tex,
                0,
                0,
                0,
                AREATEX_WIDTH as i32,
                AREATEX_HEIGHT as i32,
                gl::RG,
                gl::UNSIGNED_BYTE,
                AREA_TEX_BYTES.as_ptr().cast(),
            );

            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut search_tex);
            gl::TextureStorage2D(
                search_tex,
                1,
                gl::R8,
                SEARCHTEX_WIDTH as i32,
                SEARCHTEX_HEIGHT as i32,
            );
            gl::TextureSubImage2D(
                search_tex,
                0,
                0,
                0,
                SEARCHTEX_WIDTH as i32,
                SEARCHTEX_HEIGHT as i32,
                gl::RED,
                gl::UNSIGNED_BYTE,
                SEARCH_TEX_BYTES.as_ptr().cast(),
            );

            // Create intermediate textures
            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut edges_tex);
            gl::TextureStorage2D(edges_tex, 1, gl::RG16F, width as i32, height as i32);

            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut blend_tex);
            gl::TextureStorage2D(blend_tex, 1, gl::RGBA16F, width as i32, height as i32);

            // Create framebuffer and output texture
            gl::CreateFramebuffers(1, &mut framebuffer);

            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut texture);
            gl::TextureStorage2D(texture, 1, gl::RGBA16F, width as i32, height as i32);
            gl::NamedFramebufferTexture(framebuffer, gl::COLOR_ATTACHMENT0, texture, 0);
        }

        let sampler = create_bilinear_sampler();

        Self {
            edge_detection_vert,
            blending_weight_calculation_vert,
            neighborhood_blending_vert,
            edge_detection_frag,
            blending_weight_calculation_frag,
            neighborhood_blending_frag,
            area_tex,
            search_tex,
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
    pub fn draw(&self, program_manager: &mut ProgramManager, input_texture: u32) -> u32 {
        unsafe {
            gl::ClearColor(0.0, 0.0, 0.0, 0.0);
            gl::FrontFace(gl::CCW);
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, self.framebuffer);
            gl::BindSampler(0, self.sampler);
            gl::BindSampler(1, self.sampler);
            gl::BindSampler(2, self.sampler);

            // Pass 1: Edge detection
            gl::BindTextureUnit(0, input_texture);
            gl::NamedFramebufferTexture(self.framebuffer, gl::COLOR_ATTACHMENT0, self.edges_tex, 0);
            gl::Clear(gl::COLOR_BUFFER_BIT);
            program_manager
                .bind_present_programs(self.edge_detection_vert, self.edge_detection_frag);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);

            // Pass 2: Blending weight calculation
            gl::BindTextureUnit(0, self.edges_tex);
            gl::BindTextureUnit(1, self.area_tex);
            gl::BindTextureUnit(2, self.search_tex);
            gl::NamedFramebufferTexture(self.framebuffer, gl::COLOR_ATTACHMENT0, self.blend_tex, 0);
            gl::Clear(gl::COLOR_BUFFER_BIT);
            program_manager.bind_present_programs(
                self.blending_weight_calculation_vert,
                self.blending_weight_calculation_frag,
            );
            gl::DrawArrays(gl::TRIANGLES, 0, 3);

            // Pass 3: Neighborhood blending
            gl::BindTextureUnit(0, input_texture);
            gl::BindTextureUnit(1, self.blend_tex);
            gl::NamedFramebufferTexture(self.framebuffer, gl::COLOR_ATTACHMENT0, self.texture, 0);
            program_manager.bind_present_programs(
                self.neighborhood_blending_vert,
                self.neighborhood_blending_frag,
            );
            gl::Clear(gl::COLOR_BUFFER_BIT);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);
            gl::FrontFace(gl::CW);
        }

        self.texture
    }
}

fn smaa_shader(source: &str, stage: u32) -> u32 {
    let mut shader_source = source.to_string();
    replace_include(&mut shader_source, "opengl_smaa.glsl", OPENGL_SMAA_GLSL);
    create_program_from_source(&shader_source, stage)
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
