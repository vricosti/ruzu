// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/layer.h and layer.cpp
//!
//! Presentation layer — manages a single framebuffer texture, applies anti-aliasing
//! and upscaling, and prepares draw data for the window adapt pass.

use super::fsr::FSR;
use super::fxaa::FXAA;
use super::present_uniforms::ScreenRectVertex;
use super::smaa::SMAA;

/// Information about a framebuffer texture for display.
///
/// Corresponds to `OpenGL::FramebufferTextureInfo` (used in gl_blit_screen).
#[derive(Clone, Copy, Debug, Default)]
pub struct FramebufferTextureInfo {
    pub display_texture: u32,
    pub width: u32,
    pub height: u32,
    pub scaled_width: u32,
    pub scaled_height: u32,
}

/// Information about the Switch screen texture.
///
/// Corresponds to `OpenGL::TextureInfo`.
pub struct TextureInfo {
    pub resource: u32,
    pub width: i32,
    pub height: i32,
    pub gl_format: u32,
    pub gl_type: u32,
    pub pixel_format: u32,
}

impl TextureInfo {
    pub fn new() -> Self {
        Self {
            resource: 0,
            width: 0,
            height: 0,
            gl_format: gl::NONE,
            gl_type: gl::NONE,
            pixel_format: 0,
        }
    }
}

/// A presentation layer.
///
/// Corresponds to `OpenGL::Layer`.
pub struct Layer {
    /// Framebuffer texture data on the CPU side.
    gl_framebuffer_data: Vec<u8>,
    /// Display information for the framebuffer texture.
    framebuffer_texture: TextureInfo,

    fsr: Option<FSR>,
    fxaa: Option<FXAA>,
    smaa: Option<SMAA>,
}

impl Layer {
    /// Create a new presentation layer.
    ///
    /// Corresponds to `Layer::Layer()`.
    pub fn new() -> Self {
        // TODO: Create initial 1x1 RGBA8 texture, clear to black
        Self {
            gl_framebuffer_data: Vec::new(),
            framebuffer_texture: TextureInfo::new(),
            fsr: None,
            fxaa: None,
            smaa: None,
        }
    }

    /// Configure the layer for drawing and return the display texture handle.
    ///
    /// Corresponds to `Layer::ConfigureDraw()`.
    pub fn configure_draw(
        &mut self,
        _out_matrix: &mut [f32; 6],
        _out_vertices: &mut [ScreenRectVertex; 4],
        _invert_y: bool,
    ) -> u32 {
        todo!("Layer::ConfigureDraw")
    }

    /// Load the framebuffer from emulated memory into the screen texture.
    fn load_fb_to_screen_info(&mut self) -> FramebufferTextureInfo {
        todo!("Layer::LoadFBToScreenInfo")
    }

    /// Prepare the render target (reallocate if dimensions changed).
    fn prepare_render_target(&mut self) -> FramebufferTextureInfo {
        todo!("Layer::PrepareRenderTarget")
    }

    /// Reconfigure the framebuffer texture for new dimensions/format.
    fn configure_framebuffer_texture(&mut self) {
        todo!("Layer::ConfigureFramebufferTexture")
    }

    /// Create or recreate the FXAA pass.
    fn create_fxaa(&mut self) {
        self.smaa = None;
        if self.fxaa.is_none() {
            // TODO: Use resolution scaling info
            self.fxaa = Some(FXAA::new(
                self.framebuffer_texture.width as u32,
                self.framebuffer_texture.height as u32,
            ));
        }
    }

    /// Create or recreate the SMAA pass.
    fn create_smaa(&mut self) {
        self.fxaa = None;
        if self.smaa.is_none() {
            self.smaa = Some(SMAA::new(
                self.framebuffer_texture.width as u32,
                self.framebuffer_texture.height as u32,
            ));
        }
    }
}
