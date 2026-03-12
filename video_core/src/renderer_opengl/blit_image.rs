// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/blit_image.h and blit_image.cpp
//!
//! Blit image helper — performs color blits between framebuffers using a full-screen triangle.

use super::gl_shader_manager::ProgramManager;
use super::gl_shader_util::create_program_from_source;

/// Offset2D for region specification.
#[derive(Clone, Copy, Debug, Default)]
pub struct Offset2D {
    pub x: i32,
    pub y: i32,
}

/// Region2D for blit source/destination.
#[derive(Clone, Copy, Debug, Default)]
pub struct Region2D {
    pub start: Offset2D,
    pub end: Offset2D,
}

/// Extent3D for source image dimensions.
#[derive(Clone, Copy, Debug, Default)]
pub struct Extent3D {
    pub width: u32,
    pub height: u32,
    pub depth: u32,
}

/// Blit image helper.
///
/// Corresponds to `OpenGL::BlitImageHelper`.
pub struct BlitImageHelper {
    full_screen_vert: u32,
    blit_color_to_color_frag: u32,
}

impl BlitImageHelper {
    /// Create a new blit image helper.
    ///
    /// Corresponds to `BlitImageHelper::BlitImageHelper()`.
    pub fn new(_program_manager: &ProgramManager) -> Self {
        // TODO: Load host shaders (FULL_SCREEN_TRIANGLE_VERT, BLIT_COLOR_FLOAT_FRAG)
        Self {
            full_screen_vert: 0,
            blit_color_to_color_frag: 0,
        }
    }

    /// Blit a color image to a framebuffer.
    ///
    /// Corresponds to `BlitImageHelper::BlitColor()`.
    pub fn blit_color(
        &self,
        program_manager: &mut ProgramManager,
        dst_framebuffer: u32,
        src_image_view: u32,
        src_sampler: u32,
        dst_region: &Region2D,
        src_region: &Region2D,
        src_size: &Extent3D,
    ) {
        if self.full_screen_vert == 0 || self.blit_color_to_color_frag == 0 {
            return;
        }

        unsafe {
            gl::Disable(gl::CULL_FACE);
            gl::Disable(gl::COLOR_LOGIC_OP);
            gl::Disable(gl::DEPTH_TEST);
            gl::Disable(gl::STENCIL_TEST);
            gl::Disable(gl::POLYGON_OFFSET_FILL);
            gl::Disable(gl::RASTERIZER_DISCARD);
            gl::Disablei(gl::BLEND, 0);
            gl::PolygonMode(gl::FRONT_AND_BACK, gl::FILL);
            gl::FrontFace(gl::CW);
            gl::ColorMaski(0, gl::TRUE, gl::TRUE, gl::TRUE, gl::TRUE);
            gl::DepthRangeIndexed(0, 0.0, 0.0);

            program_manager.bind_present_programs(
                self.full_screen_vert,
                self.blit_color_to_color_frag,
            );

            let scale_x = (src_region.end.x - src_region.start.x) as f32
                / src_size.width as f32;
            let scale_y = (src_region.end.y - src_region.start.y) as f32
                / src_size.height as f32;
            gl::ProgramUniform2f(self.full_screen_vert, 0, scale_x, scale_y);

            let offset_x = src_region.start.x as f32 / src_size.width as f32;
            let offset_y = src_region.start.y as f32 / src_size.height as f32;
            gl::ProgramUniform2f(self.full_screen_vert, 1, offset_x, offset_y);

            let vp_x = dst_region.start.x.min(dst_region.end.x);
            let vp_y = dst_region.start.y.min(dst_region.end.y);
            let vp_w = (dst_region.end.x - dst_region.start.x).unsigned_abs();
            let vp_h = (dst_region.end.y - dst_region.start.y).unsigned_abs();
            gl::Viewport(vp_x, vp_y, vp_w as i32, vp_h as i32);

            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, dst_framebuffer);
            gl::BindSampler(0, src_sampler);
            gl::BindTextureUnit(0, src_image_view);
            gl::Clear(gl::COLOR_BUFFER_BIT);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);
        }
    }
}

impl Drop for BlitImageHelper {
    fn drop(&mut self) {
        unsafe {
            if self.full_screen_vert != 0 {
                gl::DeleteProgram(self.full_screen_vert);
            }
            if self.blit_color_to_color_frag != 0 {
                gl::DeleteProgram(self.blit_color_to_color_frag);
            }
        }
    }
}
