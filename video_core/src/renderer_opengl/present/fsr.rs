// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/fsr.h and fsr.cpp
//!
//! AMD FidelityFX Super Resolution (FSR) upscaling pass for OpenGL.

use super::util::{create_bilinear_sampler, replace_include};
use crate::framebuffer_config::RectF;
use crate::fsr::{fsr_easu_con_offset, fsr_rcas_con};
use crate::host_shaders::fragment_shaders::{
    OPENGL_FIDELITYFX_FSR_EASU_FRAG, OPENGL_FIDELITYFX_FSR_FRAG, OPENGL_FIDELITYFX_FSR_RCAS_FRAG,
};
use crate::host_shaders::glsl_includes::{FFX_A_H, FFX_FSR1_H};
use crate::host_shaders::vertex_shaders::FULL_SCREEN_TRIANGLE_VERT;
use crate::renderer_opengl::gl_shader_manager::ProgramManager;
use crate::renderer_opengl::gl_shader_util::create_program_from_source;

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
        let mut framebuffer: u32 = 0;
        let mut easu_tex: u32 = 0;
        let mut rcas_tex: u32 = 0;

        let mut fsr_source = OPENGL_FIDELITYFX_FSR_FRAG.to_string();
        replace_include(&mut fsr_source, "ffx_a.h", FFX_A_H);
        replace_include(&mut fsr_source, "ffx_fsr1.h", FFX_FSR1_H);

        let mut fsr_easu_source = OPENGL_FIDELITYFX_FSR_EASU_FRAG.to_string();
        let mut fsr_rcas_source = OPENGL_FIDELITYFX_FSR_RCAS_FRAG.to_string();
        replace_include(
            &mut fsr_easu_source,
            "opengl_fidelityfx_fsr.frag",
            &fsr_source,
        );
        replace_include(
            &mut fsr_rcas_source,
            "opengl_fidelityfx_fsr.frag",
            &fsr_source,
        );

        let vert = create_program_from_source(FULL_SCREEN_TRIANGLE_VERT, gl::VERTEX_SHADER);
        let easu_frag = create_program_from_source(&fsr_easu_source, gl::FRAGMENT_SHADER);
        let rcas_frag = create_program_from_source(&fsr_rcas_source, gl::FRAGMENT_SHADER);
        let sampler = create_bilinear_sampler();

        unsafe {
            gl::ProgramUniform2f(vert, 0, 1.0, -1.0);
            gl::ProgramUniform2f(vert, 1, 0.0, 1.0);

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

        Self {
            width: output_width,
            height: output_height,
            framebuffer,
            sampler,
            vert,
            easu_frag,
            rcas_frag,
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
        program_manager: &mut ProgramManager,
        input_texture: u32,
        input_image_width: u32,
        input_image_height: u32,
        crop_rect: RectF,
    ) -> u32 {
        let input_width = input_image_width as f32;
        let input_height = input_image_height as f32;
        let output_width = self.width as f32;
        let output_height = self.height as f32;
        let viewport_width = (crop_rect.right - crop_rect.left) * input_width;
        let viewport_x = crop_rect.left * input_width;
        let viewport_height = (crop_rect.bottom - crop_rect.top) * input_height;
        let viewport_y = crop_rect.top * input_height;

        let mut easu_con = [0u32; 16];
        let mut rcas_con = [0u32; 4];
        {
            let (con0, rest) = easu_con.split_at_mut(4);
            let (con1, rest) = rest.split_at_mut(4);
            let (con2, con3) = rest.split_at_mut(4);
            fsr_easu_con_offset(
                con0.try_into().unwrap(),
                con1.try_into().unwrap(),
                con2.try_into().unwrap(),
                con3.try_into().unwrap(),
                viewport_width,
                viewport_height,
                input_width,
                input_height,
                output_width,
                output_height,
                viewport_x,
                viewport_y,
            );
        }

        let sharpening =
            *common::settings::values().fsr_sharpening_slider.get_value() as f32 / 100.0;
        fsr_rcas_con(&mut rcas_con, sharpening);

        unsafe {
            gl::ProgramUniform4uiv(self.easu_frag, 0, 4, easu_con.as_ptr());
            gl::ProgramUniform4uiv(self.rcas_frag, 0, 1, rcas_con.as_ptr());
            gl::FrontFace(gl::CW);
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, self.framebuffer);

            // Pass 1: EASU upscaling
            gl::NamedFramebufferTexture(self.framebuffer, gl::COLOR_ATTACHMENT0, self.easu_tex, 0);
            gl::ViewportIndexedf(0, 0.0, 0.0, output_width, output_height);
            program_manager.bind_present_programs(self.vert, self.easu_frag);
            gl::BindTextureUnit(0, input_texture);
            gl::BindSampler(0, self.sampler);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);

            // Pass 2: RCAS sharpening
            gl::NamedFramebufferTexture(self.framebuffer, gl::COLOR_ATTACHMENT0, self.rcas_tex, 0);
            program_manager.bind_present_programs(self.vert, self.rcas_frag);
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
