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
    pub fn new(output_width: u32, output_height: u32) -> Self {
        // TODO: Compile FSR shaders, create textures and framebuffer
        Self {
            width: output_width,
            height: output_height,
            framebuffer: 0,
            sampler: 0,
            vert: 0,
            easu_frag: 0,
            rcas_frag: 0,
            easu_tex: 0,
            rcas_tex: 0,
        }
    }

    /// Execute the FSR pass and return the output texture handle.
    ///
    /// Corresponds to `FSR::Draw()`.
    pub fn draw(
        &self,
        _input_texture: u32,
        _input_image_width: u32,
        _input_image_height: u32,
    ) -> u32 {
        todo!("FSR::Draw")
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
