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
    pub fn new(_width: u32, _height: u32) -> Self {
        // TODO: Compile SMAA shaders, create textures and framebuffer
        // TODO: Upload area and search textures
        Self {
            edge_detection_vert: 0,
            blending_weight_calculation_vert: 0,
            neighborhood_blending_vert: 0,
            edge_detection_frag: 0,
            blending_weight_calculation_frag: 0,
            neighborhood_blending_frag: 0,
            area_tex: 0,
            search_tex: 0,
            edges_tex: 0,
            blend_tex: 0,
            sampler: 0,
            framebuffer: 0,
            texture: 0,
        }
    }

    /// Execute the three-pass SMAA pipeline and return the output texture handle.
    ///
    /// Corresponds to `SMAA::Draw()`.
    pub fn draw(&self, _input_texture: u32) -> u32 {
        todo!("SMAA::Draw")
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
