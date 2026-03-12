// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/window_adapt_pass.h and
//! window_adapt_pass.cpp
//!
//! Window adapt pass — final composition of layers onto the window framebuffer
//! using a configurable fragment shader (filter).

use super::present_uniforms::ScreenRectVertex;

/// Window adapt pass for final framebuffer composition.
///
/// Corresponds to `OpenGL::WindowAdaptPass`.
pub struct WindowAdaptPass {
    sampler: u32,
    vert: u32,
    frag: u32,
    vertex_buffer: u32,
    /// GPU address of the vertex buffer (NV unified memory).
    vertex_buffer_address: u64,
}

impl WindowAdaptPass {
    /// Create a new window adapt pass with the given sampler and fragment shader.
    ///
    /// Corresponds to `WindowAdaptPass::WindowAdaptPass()`.
    pub fn new(_sampler: u32, _frag_source: &str) -> Self {
        // TODO: Compile vertex shader from OPENGL_PRESENT_VERT
        // TODO: Compile fragment shader from frag_source
        // TODO: Create vertex buffer, query GPU address if unified memory
        Self {
            sampler: 0,
            vert: 0,
            frag: 0,
            vertex_buffer: 0,
            vertex_buffer_address: 0,
        }
    }

    /// Draw all layers to the current framebuffer.
    ///
    /// Corresponds to `WindowAdaptPass::DrawToFramebuffer()`.
    pub fn draw_to_framebuffer(
        &self,
        _textures: &[u32],
        _matrices: &[[f32; 6]],
        _vertices: &[[ScreenRectVertex; 4]],
        _layout_width: u32,
        _layout_height: u32,
    ) {
        todo!("WindowAdaptPass::DrawToFramebuffer")
    }
}

impl Drop for WindowAdaptPass {
    fn drop(&mut self) {
        unsafe {
            for &prog in &[self.vert, self.frag] {
                if prog != 0 {
                    gl::DeleteProgram(prog);
                }
            }
            if self.sampler != 0 {
                gl::DeleteSamplers(1, &self.sampler);
            }
            if self.vertex_buffer != 0 {
                gl::DeleteBuffers(1, &self.vertex_buffer);
            }
        }
    }
}
