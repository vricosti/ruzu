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
        _program_manager: &mut ProgramManager,
        _dst_framebuffer: u32,
        _src_image_view: u32,
        _src_sampler: u32,
        _dst_region: &Region2D,
        _src_region: &Region2D,
        _src_size: &Extent3D,
    ) {
        todo!("BlitImageHelper::BlitColor")
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
