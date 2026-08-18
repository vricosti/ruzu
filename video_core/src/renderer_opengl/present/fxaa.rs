// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of Eden `video_core/renderer_opengl/present/fxaa.{h,cpp}`.
//!
//! Fast Approximate Anti-Aliasing (FXAA) post-processing pass for OpenGL.

use super::util::create_bilinear_sampler;
use crate::host_shaders::fragment_shaders::FXAA_FRAG;
use crate::host_shaders::vertex_shaders::FXAA_VERT;
use crate::renderer_opengl::gl_resource_manager::{
    OGLFramebuffer, OGLProgram, OGLSampler, OGLTexture,
};
use crate::renderer_opengl::gl_shader_manager::ProgramManager;
use crate::renderer_opengl::gl_shader_util::create_program_from_source;

/// FXAA anti-aliasing pass.
///
/// Corresponds to `OpenGL::FXAA`.
pub struct FXAA {
    // Rust drops fields in declaration order; this is Eden's reverse-member
    // destruction order.
    texture: OGLTexture,
    framebuffer: OGLFramebuffer,
    sampler: OGLSampler,
    frag_shader: OGLProgram,
    vert_shader: OGLProgram,
}

impl FXAA {
    /// Create a new FXAA pass with the given dimensions.
    ///
    /// Corresponds to `FXAA::FXAA()`.
    ///
    /// Compiles the host shaders and creates the upstream sampler, RGBA16F
    /// target, and framebuffer.
    pub fn new(width: u32, height: u32) -> Self {
        let vert_shader = create_program_from_source(FXAA_VERT, gl::VERTEX_SHADER);
        let frag_shader = create_program_from_source(FXAA_FRAG, gl::FRAGMENT_SHADER);
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
            frag_shader,
            vert_shader,
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

            program_manager.bind_present_programs(self.vert_shader.handle, self.frag_shader.handle);
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, self.framebuffer.handle);
            gl::BindTextureUnit(0, input_texture);
            gl::BindSampler(0, self.sampler.handle);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);
            gl::FrontFace(gl::CW);
        }

        self.texture.handle
    }
}
