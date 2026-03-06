// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/yuzu_cmd/emu_window/emu_window_sdl2_gl.h and emu_window_sdl2_gl.cpp
//! Status: EN COURS
//!
//! SDL2 + OpenGL presentation layer. Creates an OpenGL context, initializes
//! the RendererOpenGL, and handles framebuffer presentation via GL blit.

use std::sync::Arc;

use log::info;
use sdl2::video::{GLContext, Window};
use thiserror::Error;

use ruzu_gpu::renderer_opengl::RendererOpenGL;
use ruzu_gpu::syncpoint::SyncpointManager;

#[derive(Debug, Error)]
pub enum OpenGLPresenterError {
    #[error("OpenGL context creation failed: {0}")]
    ContextCreation(String),
    #[error("Renderer initialization failed: {0}")]
    RendererInit(String),
}

/// OpenGL presenter matching zuyu's `EmuWindow_SDL2_GL`.
///
/// Manages the SDL2 OpenGL context and delegates rendering to `RendererOpenGL`.
pub struct OpenGLPresenter {
    _gl_context: GLContext,
    renderer: RendererOpenGL,
}

impl OpenGLPresenter {
    /// Create a new OpenGL presenter from an SDL2 window.
    ///
    /// The window must have been created with the `.opengl()` flag.
    pub fn new(
        window: &Window,
        syncpoints: Arc<SyncpointManager>,
    ) -> Result<Self, OpenGLPresenterError> {
        // Create GL context
        let gl_context = window
            .gl_create_context()
            .map_err(|e| OpenGLPresenterError::ContextCreation(e))?;

        // Make context current
        window
            .gl_make_current(&gl_context)
            .map_err(|e| OpenGLPresenterError::ContextCreation(e))?;

        // Disable vsync for maximum frame throughput (matching zuyu)
        let video = window.subsystem();
        let _ = video.gl_set_swap_interval(sdl2::video::SwapInterval::Immediate);

        info!("OpenGL context created");

        // Initialize RendererOpenGL (loads GL function pointers and queries device)
        let renderer = RendererOpenGL::new(
            |name| video.gl_get_proc_address(name) as *const _,
            syncpoints,
        )
        .map_err(|e| OpenGLPresenterError::RendererInit(e.to_string()))?;

        Ok(Self {
            _gl_context: gl_context,
            renderer,
        })
    }

    /// Present an RGBA8888 framebuffer to the window via OpenGL blit.
    pub fn present_framebuffer(
        &mut self,
        window: &Window,
        pixels: &[u8],
        width: u32,
        height: u32,
    ) -> Result<(), OpenGLPresenterError> {
        let (win_w, win_h) = window.size();

        self.renderer.composite(pixels, width, height, win_w, win_h);

        // Swap buffers
        window.gl_swap_window();

        Ok(())
    }

    /// Handle window resize (GL viewport is updated on next present).
    pub fn resize(&mut self, _width: u32, _height: u32) -> Result<(), OpenGLPresenterError> {
        // GL viewport is set in composite(), so nothing to do here.
        Ok(())
    }

    /// Get a reference to the underlying renderer.
    pub fn renderer(&self) -> &RendererOpenGL {
        &self.renderer
    }

    /// Get a mutable reference to the underlying renderer.
    #[allow(dead_code)]
    pub fn renderer_mut(&mut self) -> &mut RendererOpenGL {
        &mut self.renderer
    }
}
