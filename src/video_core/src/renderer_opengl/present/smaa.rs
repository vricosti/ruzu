// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of Eden `video_core/renderer_opengl/present/smaa.{h,cpp}`.
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
use crate::renderer_opengl::gl_resource_manager::{
    OGLFramebuffer, OGLProgram, OGLSampler, OGLTexture,
};
use crate::renderer_opengl::gl_shader_manager::ProgramManager;
use crate::renderer_opengl::gl_shader_util::create_program_from_source;
use crate::smaa_area_tex::{AREATEX_HEIGHT, AREATEX_WIDTH, AREA_TEX_BYTES};
use crate::smaa_search_tex::{SEARCHTEX_HEIGHT, SEARCHTEX_WIDTH, SEARCH_TEX_BYTES};

/// SMAA anti-aliasing pass.
///
/// Corresponds to `OpenGL::SMAA`.
pub struct SMAA {
    // Rust drops fields in declaration order; this is Eden's reverse-member
    // destruction order.
    texture: OGLTexture,
    framebuffer: OGLFramebuffer,
    sampler: OGLSampler,
    blend_tex: OGLTexture,
    edges_tex: OGLTexture,
    search_tex: OGLTexture,
    area_tex: OGLTexture,
    neighborhood_blending_frag: OGLProgram,
    blending_weight_calculation_frag: OGLProgram,
    edge_detection_frag: OGLProgram,
    neighborhood_blending_vert: OGLProgram,
    blending_weight_calculation_vert: OGLProgram,
    edge_detection_vert: OGLProgram,
}

impl SMAA {
    /// Create a new SMAA pass with the given dimensions.
    ///
    /// Corresponds to `SMAA::SMAA()`.
    ///
    /// Compiles all six SMAA shaders, uploads both lookup textures, and creates
    /// the upstream edge, blend, and output targets.
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

        unsafe {
            gl::BindBuffer(gl::PIXEL_UNPACK_BUFFER, 0);
            gl::PixelStorei(gl::UNPACK_ROW_LENGTH, 0);
        }

        let mut area_tex = OGLTexture::new();
        area_tex.create(gl::TEXTURE_2D);
        unsafe {
            gl::TextureStorage2D(
                area_tex.handle,
                1,
                gl::RG8,
                AREATEX_WIDTH as i32,
                AREATEX_HEIGHT as i32,
            );
            gl::TextureSubImage2D(
                area_tex.handle,
                0,
                0,
                0,
                AREATEX_WIDTH as i32,
                AREATEX_HEIGHT as i32,
                gl::RG,
                gl::UNSIGNED_BYTE,
                AREA_TEX_BYTES.as_ptr().cast(),
            );
        }
        let mut search_tex = OGLTexture::new();
        search_tex.create(gl::TEXTURE_2D);
        unsafe {
            gl::TextureStorage2D(
                search_tex.handle,
                1,
                gl::R8,
                SEARCHTEX_WIDTH as i32,
                SEARCHTEX_HEIGHT as i32,
            );
            gl::TextureSubImage2D(
                search_tex.handle,
                0,
                0,
                0,
                SEARCHTEX_WIDTH as i32,
                SEARCHTEX_HEIGHT as i32,
                gl::RED,
                gl::UNSIGNED_BYTE,
                SEARCH_TEX_BYTES.as_ptr().cast(),
            );
        }

        let mut edges_tex = OGLTexture::new();
        edges_tex.create(gl::TEXTURE_2D);
        unsafe {
            gl::TextureStorage2D(edges_tex.handle, 1, gl::RG16F, width as i32, height as i32)
        };

        let mut blend_tex = OGLTexture::new();
        blend_tex.create(gl::TEXTURE_2D);
        unsafe {
            gl::TextureStorage2D(
                blend_tex.handle,
                1,
                gl::RGBA16F,
                width as i32,
                height as i32,
            )
        };

        let sampler = create_bilinear_sampler();
        let mut framebuffer = OGLFramebuffer::new();
        framebuffer.create();
        let mut texture = OGLTexture::new();
        texture.create(gl::TEXTURE_2D);
        unsafe {
            gl::TextureStorage2D(texture.handle, 1, gl::RGBA16F, width as i32, height as i32);
            gl::NamedFramebufferTexture(
                framebuffer.handle,
                gl::COLOR_ATTACHMENT0,
                texture.handle,
                0,
            );
        }

        Self {
            texture,
            framebuffer,
            sampler,
            blend_tex,
            edges_tex,
            search_tex,
            area_tex,
            neighborhood_blending_frag,
            blending_weight_calculation_frag,
            edge_detection_frag,
            neighborhood_blending_vert,
            blending_weight_calculation_vert,
            edge_detection_vert,
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
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, self.framebuffer.handle);
            gl::BindSampler(0, self.sampler.handle);
            gl::BindSampler(1, self.sampler.handle);
            gl::BindSampler(2, self.sampler.handle);

            // Pass 1: Edge detection
            gl::BindTextureUnit(0, input_texture);
            gl::NamedFramebufferTexture(
                self.framebuffer.handle,
                gl::COLOR_ATTACHMENT0,
                self.edges_tex.handle,
                0,
            );
            gl::Clear(gl::COLOR_BUFFER_BIT);
            program_manager.bind_present_programs(
                self.edge_detection_vert.handle,
                self.edge_detection_frag.handle,
            );
            gl::DrawArrays(gl::TRIANGLES, 0, 3);

            // Pass 2: Blending weight calculation
            gl::BindTextureUnit(0, self.edges_tex.handle);
            gl::BindTextureUnit(1, self.area_tex.handle);
            gl::BindTextureUnit(2, self.search_tex.handle);
            gl::NamedFramebufferTexture(
                self.framebuffer.handle,
                gl::COLOR_ATTACHMENT0,
                self.blend_tex.handle,
                0,
            );
            gl::Clear(gl::COLOR_BUFFER_BIT);
            program_manager.bind_present_programs(
                self.blending_weight_calculation_vert.handle,
                self.blending_weight_calculation_frag.handle,
            );
            gl::DrawArrays(gl::TRIANGLES, 0, 3);

            // Pass 3: Neighborhood blending
            gl::BindTextureUnit(0, input_texture);
            gl::BindTextureUnit(1, self.blend_tex.handle);
            gl::NamedFramebufferTexture(
                self.framebuffer.handle,
                gl::COLOR_ATTACHMENT0,
                self.texture.handle,
                0,
            );
            program_manager.bind_present_programs(
                self.neighborhood_blending_vert.handle,
                self.neighborhood_blending_frag.handle,
            );
            gl::Clear(gl::COLOR_BUFFER_BIT);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);
            gl::FrontFace(gl::CW);
        }

        self.texture.handle
    }
}

fn smaa_shader(source: &str, stage: u32) -> OGLProgram {
    let mut shader_source = source.to_string();
    replace_include(&mut shader_source, "opengl_smaa.glsl", OPENGL_SMAA_GLSL);
    create_program_from_source(&shader_source, stage)
}
