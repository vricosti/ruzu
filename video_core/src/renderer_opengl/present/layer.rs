// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/layer.h and layer.cpp
//!
//! Presentation layer -- manages a single framebuffer texture, applies anti-aliasing
//! and upscaling, and prepares draw data for the window adapt pass.

use super::fsr::FSR;
use super::fxaa::FXAA;
use super::present_uniforms::{make_orthographic_matrix, ScreenRectVertex};
use super::smaa::SMAA;

use crate::framebuffer_config::{self, FramebufferConfig};
use ruzu_core::frontend::framebuffer_layout::FramebufferLayout;

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
            let black: [u8; 4] = [0, 0, 0, 0];
            gl::ClearTexImage(
                texture,
                0,
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
    /// 1. Loads the framebuffer from emulated memory into the screen texture
    /// 2. Applies anti-aliasing (FXAA or SMAA) — skipped for now (AA passes are stubs)
    /// 3. Applies FSR upscaling if enabled — skipped for now
    /// 4. Fills the output matrix and vertex data
    /// 5. Returns the final texture handle to draw
    pub fn configure_draw(
        &mut self,
        out_matrix: &mut [f32; 6],
        out_vertices: &mut [ScreenRectVertex; 4],
        framebuffer: &FramebufferConfig,
        layout: &FramebufferLayout,
        invert_y: bool,
    ) -> u32 {
        let info = self.prepare_render_target(framebuffer);
        let crop = framebuffer_config::normalize_crop(framebuffer, info.width, info.height);
        let texture = info.display_texture;

        // Anti-aliasing would be applied here (upstream checks filters.get_anti_aliasing()).
        // FXAA/SMAA passes are structural stubs — they return input texture unchanged.
        // When AA shaders are ported, the logic goes here matching upstream ConfigureDraw.

        unsafe {
            gl::Disablei(gl::SCISSOR_TEST, 0);
        }

        // FSR upscaling would be applied here (upstream checks filters.get_scaling_filter() == Fsr).
        // FSR pass is a structural stub — skipped until shader is ported.

        // Build orthographic projection matrix.
        *out_matrix = make_orthographic_matrix(layout.width as f32, layout.height as f32);

        // Map the coordinates to the screen.
        let x = layout.screen.left;
        let y = layout.screen.top;
        let w = layout.screen.right - layout.screen.left;
        let h = layout.screen.bottom - layout.screen.top;

        let left = crop.left;
        let right = crop.right;
        let top = if invert_y { crop.bottom } else { crop.top };
        let bottom = if invert_y { crop.top } else { crop.bottom };

        out_vertices[0] = ScreenRectVertex::new(x, y, left, top);
        out_vertices[1] = ScreenRectVertex::new(x + w, y, right, top);
        out_vertices[2] = ScreenRectVertex::new(x, y + h, left, bottom);
        out_vertices[3] = ScreenRectVertex::new(x + w, y + h, right, bottom);

        texture
    }

    /// Prepare the render target: reallocate texture if dimensions/format changed,
    /// then load framebuffer data.
    ///
    /// Port of `Layer::PrepareRenderTarget()`.
    fn prepare_render_target(&mut self, framebuffer: &FramebufferConfig) -> FramebufferTextureInfo {
        if self.framebuffer_texture.width != framebuffer.width as i32
            || self.framebuffer_texture.height != framebuffer.height as i32
            || self.framebuffer_texture.pixel_format != framebuffer.pixel_format.0
            || self.gl_framebuffer_data.is_empty()
        {
            self.configure_framebuffer_texture(framebuffer);
        }

        self.load_fb_to_screen_info(framebuffer)
    }

    /// Load the framebuffer from emulated memory into the screen texture.
    ///
    /// Port of `Layer::LoadFBToScreenInfo()`.
    ///
    /// Upstream flow:
    /// 1. Try rasterizer.AccelerateDisplay() — GPU-cached texture fast path
    /// 2. If miss: read from device_memory, unswizzle, upload via glTextureSubImage2D
    ///
    /// Since we don't yet have device_memory or rasterizer.AccelerateDisplay() wired,
    /// this returns the existing texture (which will show black until GPU memory
    /// integration is complete). The texture is properly allocated by
    /// configure_framebuffer_texture().
    fn load_fb_to_screen_info(
        &mut self,
        framebuffer: &FramebufferConfig,
    ) -> FramebufferTextureInfo {
        // The full implementation would:
        // 1. let framebuffer_addr = framebuffer.address + framebuffer.offset as u64;
        // 2. Try rasterizer.accelerate_display(framebuffer, framebuffer_addr, framebuffer.stride)
        // 3. If miss: read host_ptr from device_memory, unswizzle, upload to texture
        //
        // For now, return the texture info without loading data.
        // The texture exists at the correct dimensions from configure_framebuffer_texture().

        FramebufferTextureInfo {
            display_texture: self.framebuffer_texture.resource,
            width: framebuffer.width,
            height: framebuffer.height,
            scaled_width: framebuffer.width,
            scaled_height: framebuffer.height,
        }
    }

    /// Reconfigure the framebuffer texture for new dimensions/format.
    ///
    /// Port of `Layer::ConfigureFramebufferTexture()`.
    fn configure_framebuffer_texture(&mut self, framebuffer: &FramebufferConfig) {
        self.framebuffer_texture.width = framebuffer.width as i32;
        self.framebuffer_texture.height = framebuffer.height as i32;
        self.framebuffer_texture.pixel_format = framebuffer.pixel_format.0;

        // Map pixel format to GL format.
        // Upstream: PixelFormatFromGPUPixelFormat + BytesPerBlock.
        // Common formats: RGBA8888 = format 1, RGB565 = format 4.
        let (internal_format, gl_format, gl_type, bytes_per_pixel) =
            match framebuffer.pixel_format.0 {
                1 => (gl::RGBA8, gl::RGBA, gl::UNSIGNED_INT_8_8_8_8_REV, 4u32),
                4 => (gl::RGB565, gl::RGB, gl::UNSIGNED_SHORT_5_6_5, 2u32),
                _ => {
                    log::warn!(
                        "Unknown framebuffer pixel format: {}, defaulting to RGBA8",
                        framebuffer.pixel_format.0
                    );
                    (gl::RGBA8, gl::RGBA, gl::UNSIGNED_INT_8_8_8_8_REV, 4u32)
                }
            };

        self.framebuffer_texture.gl_format = gl_format;
        self.framebuffer_texture.gl_type = gl_type;

        // Allocate CPU buffer for unswizzled data.
        self.gl_framebuffer_data.resize(
            (framebuffer.width * framebuffer.height * bytes_per_pixel) as usize,
            0,
        );

        // Recreate GL texture.
        unsafe {
            if self.framebuffer_texture.resource != 0 {
                gl::DeleteTextures(1, &self.framebuffer_texture.resource);
            }
            let mut texture: u32 = 0;
            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut texture);
            gl::TextureStorage2D(
                texture,
                1,
                internal_format,
                framebuffer.width as i32,
                framebuffer.height as i32,
            );
            self.framebuffer_texture.resource = texture;
        }

        // Invalidate post-processors since dimensions changed.
        self.fxaa = None;
        self.smaa = None;
        self.fsr = None;
    }

    /// Create or recreate the FXAA pass.
    #[allow(dead_code)]
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
    #[allow(dead_code)]
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
