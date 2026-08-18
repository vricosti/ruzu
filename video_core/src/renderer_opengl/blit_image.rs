// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of Eden `video_core/renderer_opengl/blit_image.{h,cpp}`.
//!
//! Blit image helper — performs color blits between framebuffers using a full-screen triangle.

use super::gl_resource_manager::OGLProgram;
use super::gl_shader_manager::ProgramManagerHandle;
use super::gl_shader_util::create_program_from_source;
use crate::host_shaders::fragment_shaders::BLIT_COLOR_FLOAT_FRAG;
use crate::host_shaders::vertex_shaders::FULL_SCREEN_TRIANGLE_VERT;
use crate::texture_cache::types::{Extent3D, Region2D};

const GL_ALPHA_TEST: u32 = 0x0BC0;

/// Blit image helper.
///
/// Corresponds to `OpenGL::BlitImageHelper`.
pub struct BlitImageHelper {
    // Rust drops fields in declaration order. Keep the two programs in C++
    // reverse-member destruction order, then release the shared owner handle.
    blit_color_to_color_frag: OGLProgram,
    full_screen_vert: OGLProgram,
    program_manager: ProgramManagerHandle,
}

impl BlitImageHelper {
    /// Create a new blit image helper.
    ///
    /// Corresponds to `BlitImageHelper::BlitImageHelper()`.
    pub fn new(program_manager: ProgramManagerHandle) -> Self {
        let full_screen_vert =
            create_program_from_source(FULL_SCREEN_TRIANGLE_VERT, gl::VERTEX_SHADER);
        let blit_color_to_color_frag =
            create_program_from_source(BLIT_COLOR_FLOAT_FRAG, gl::FRAGMENT_SHADER);
        Self {
            blit_color_to_color_frag,
            full_screen_vert,
            program_manager,
        }
    }

    /// Blit a color image to a framebuffer.
    ///
    /// Corresponds to `BlitImageHelper::BlitColor()`.
    pub fn blit_color(
        &self,
        dst_framebuffer: u32,
        src_image_view: u32,
        src_sampler: u32,
        dst_region: &Region2D,
        src_region: &Region2D,
        src_size: &Extent3D,
    ) {
        unsafe {
            gl::Disable(gl::CULL_FACE);
            gl::Disable(gl::COLOR_LOGIC_OP);
            gl::Disable(gl::DEPTH_TEST);
            gl::Disable(gl::STENCIL_TEST);
            gl::Disable(gl::POLYGON_OFFSET_FILL);
            gl::Disable(gl::RASTERIZER_DISCARD);
            gl::Disable(GL_ALPHA_TEST);
            gl::Disablei(gl::BLEND, 0);
            gl::PolygonMode(gl::FRONT_AND_BACK, gl::FILL);
            gl::FrontFace(gl::CW);
            gl::ColorMaski(0, gl::TRUE, gl::TRUE, gl::TRUE, gl::TRUE);
            gl::DepthRangeIndexed(0, 0.0, 0.0);

            self.program_manager.lock().bind_present_programs(
                self.full_screen_vert.handle,
                self.blit_color_to_color_frag.handle,
            );

            let scale_x = (src_region.end.x - src_region.start.x) as f32 / src_size.width as f32;
            let scale_y = (src_region.end.y - src_region.start.y) as f32 / src_size.height as f32;
            gl::ProgramUniform2f(self.full_screen_vert.handle, 0, scale_x, scale_y);

            let offset_x = src_region.start.x as f32 / src_size.width as f32;
            let offset_y = src_region.start.y as f32 / src_size.height as f32;
            gl::ProgramUniform2f(self.full_screen_vert.handle, 1, offset_x, offset_y);

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
