// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/layer.h and layer.cpp
//!
//! Presentation layer -- manages a single framebuffer texture, applies anti-aliasing
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
    /// Port of `Layer::Layer()`.
    /// Creates an initial 1x1 RGBA8 texture, cleared to black.
    pub fn new() -> Self {
        let mut texture: u32 = 0;
        unsafe {
            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut texture);
            gl::TextureStorage2D(texture, 1, gl::RGBA8, 1, 1);
            let black: [u8; 4] = [0, 0, 0, 255];
            gl::TextureSubImage2D(
                texture,
                0,
                0,
                0,
                1,
                1,
                gl::RGBA,
                gl::UNSIGNED_BYTE,
                black.as_ptr() as *const _,
            );
        }

        Self {
            gl_framebuffer_data: Vec::new(),
            framebuffer_texture: TextureInfo {
                resource: texture,
                width: 1,
                height: 1,
                gl_format: gl::RGBA,
                gl_type: gl::UNSIGNED_BYTE,
                pixel_format: 0,
            },
            fsr: None,
            fxaa: None,
            smaa: None,
        }
    }

    /// Configure the layer for drawing and return the display texture handle.
    ///
    /// Port of `Layer::ConfigureDraw()`.
    ///
    /// In the full implementation, this:
    /// 1. Loads the framebuffer from emulated memory into the screen texture
    /// 2. Prepares the render target (reallocates if needed)
    /// 3. Applies anti-aliasing (FXAA or SMAA)
    /// 4. Applies FSR upscaling if enabled
    /// 5. Fills the output matrix and vertex data
    /// 6. Returns the final texture handle to draw
    pub fn configure_draw(
        &mut self,
        out_matrix: &mut [f32; 6],
        out_vertices: &mut [ScreenRectVertex; 4],
        invert_y: bool,
    ) -> u32 {
        let fb_info = self.prepare_render_target();

        // Build orthographic projection matrix (2x3, stored column-major)
        // Maps from framebuffer coordinates to clip space
        let width = fb_info.scaled_width as f32;
        let height = fb_info.scaled_height as f32;
        out_matrix[0] = 2.0 / width;
        out_matrix[1] = 0.0;
        out_matrix[2] = 0.0;
        out_matrix[3] = if invert_y { -2.0 / height } else { 2.0 / height };
        out_matrix[4] = -1.0;
        out_matrix[5] = if invert_y { 1.0 } else { -1.0 };

        // Build quad vertices
        let y0 = if invert_y { height } else { 0.0 };
        let y1 = if invert_y { 0.0 } else { height };

        out_vertices[0] = ScreenRectVertex {
            position: [0.0, y0],
            tex_coord: [0.0, 0.0],
        };
        out_vertices[1] = ScreenRectVertex {
            position: [width, y0],
            tex_coord: [1.0, 0.0],
        };
        out_vertices[2] = ScreenRectVertex {
            position: [0.0, y1],
            tex_coord: [0.0, 1.0],
        };
        out_vertices[3] = ScreenRectVertex {
            position: [width, y1],
            tex_coord: [1.0, 1.0],
        };

        fb_info.display_texture
    }

    /// Load the framebuffer from emulated memory into the screen texture.
    fn load_fb_to_screen_info(&mut self) -> FramebufferTextureInfo {
        // In the full implementation, this reads framebuffer data from guest memory
        // and uploads it to the GL texture.
        FramebufferTextureInfo {
            display_texture: self.framebuffer_texture.resource,
            width: self.framebuffer_texture.width as u32,
            height: self.framebuffer_texture.height as u32,
            scaled_width: self.framebuffer_texture.width as u32,
            scaled_height: self.framebuffer_texture.height as u32,
        }
    }

    /// Prepare the render target (reallocate if dimensions changed).
    fn prepare_render_target(&mut self) -> FramebufferTextureInfo {
        self.load_fb_to_screen_info()
    }

    /// Reconfigure the framebuffer texture for new dimensions/format.
    fn configure_framebuffer_texture(&mut self, width: i32, height: i32, format: u32, gl_type: u32) {
        if self.framebuffer_texture.width == width
            && self.framebuffer_texture.height == height
            && self.framebuffer_texture.gl_format == format
        {
            return;
        }

        unsafe {
            if self.framebuffer_texture.resource != 0 {
                gl::DeleteTextures(1, &self.framebuffer_texture.resource);
            }
            let mut texture: u32 = 0;
            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut texture);
            gl::TextureStorage2D(texture, 1, gl::RGBA8, width, height);
            self.framebuffer_texture.resource = texture;
        }

        self.framebuffer_texture.width = width;
        self.framebuffer_texture.height = height;
        self.framebuffer_texture.gl_format = format;
        self.framebuffer_texture.gl_type = gl_type;

        // Invalidate AA passes since dimensions changed
        self.fxaa = None;
        self.smaa = None;
        self.fsr = None;
    }

    /// Create or recreate the FXAA pass.
    fn create_fxaa(&mut self) {
        self.smaa = None;
        if self.fxaa.is_none() {
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

impl Drop for Layer {
    fn drop(&mut self) {
        unsafe {
            if self.framebuffer_texture.resource != 0 {
                gl::DeleteTextures(1, &self.framebuffer_texture.resource);
            }
        }
    }
}
