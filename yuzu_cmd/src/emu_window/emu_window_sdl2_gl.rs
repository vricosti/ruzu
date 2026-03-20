// SPDX-FileCopyrightText: 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! OpenGL SDL2 emulator window.
//!
//! Port of `yuzu_cmd/emu_window/emu_window_sdl2_gl.h` and
//! `yuzu_cmd/emu_window/emu_window_sdl2_gl.cpp`.
//!
//! `EmuWindowSdl2Gl` creates an SDL2 window with an OpenGL 4.6 compatibility-
//! profile context, checks for required GL extensions, and provides a
//! shared-context factory for the video core renderer.
//!
//! A helper type `SdlGlContext` (maps to C++ `SDLGLContext`) wraps an SDL GL
//! context to implement the `GraphicsContext` interface (swap buffers,
//! make-current / done-current).

use std::ffi::CStr;
use sdl2::sys as sdl;

use super::emu_window_sdl2::EmuWindowSdl2;

// Screen layout constants.
// Maps to C++ `Layout::ScreenUndocked::Width` / `Layout::ScreenUndocked::Height`.
const SCREEN_UNDOCKED_WIDTH: i32 = 1280;
const SCREEN_UNDOCKED_HEIGHT: i32 = 720;

// ---------------------------------------------------------------------------
// SDLGLContext
// ---------------------------------------------------------------------------

/// SDL2 OpenGL context wrapper.
///
/// Maps to C++ class `SDLGLContext` defined inside
/// `yuzu_cmd/emu_window/emu_window_sdl2_gl.cpp`.
pub struct SdlGlContext {
    /// The SDL window this context belongs to.
    window: *mut sdl::SDL_Window,

    /// The raw SDL GL context handle.
    context: sdl::SDL_GLContext,

    /// Whether this context is currently current on this thread.
    is_current: bool,
}

impl SdlGlContext {
    /// Creates a new GL context for `window`.
    ///
    /// Maps to C++ `SDLGLContext::SDLGLContext`.
    pub fn new(window: *mut sdl::SDL_Window) -> Self {
        // Maps to: context = SDL_GL_CreateContext(window)
        let context = unsafe { sdl::SDL_GL_CreateContext(window) };
        SdlGlContext {
            window,
            context,
            is_current: false,
        }
    }

    /// Swaps the front and back buffers.
    ///
    /// Maps to C++ `SDLGLContext::SwapBuffers`.
    pub fn swap_buffers(&self) {
        // Maps to: SDL_GL_SwapWindow(window)
        unsafe { sdl::SDL_GL_SwapWindow(self.window) };
    }

    /// Makes this context current on the calling thread.
    ///
    /// Maps to C++ `SDLGLContext::MakeCurrent`.
    pub fn make_current(&mut self) {
        // Maps to: if (is_current) return; is_current = SDL_GL_MakeCurrent(window, context) == 0
        if self.is_current {
            return;
        }
        let ret = unsafe { sdl::SDL_GL_MakeCurrent(self.window, self.context) };
        self.is_current = ret == 0;
    }

    /// Releases this context from the calling thread.
    ///
    /// Maps to C++ `SDLGLContext::DoneCurrent`.
    pub fn done_current(&mut self) {
        // Maps to: if (!is_current) return; SDL_GL_MakeCurrent(window, nullptr); is_current = false
        if !self.is_current {
            return;
        }
        unsafe { sdl::SDL_GL_MakeCurrent(self.window, std::ptr::null_mut()) };
        self.is_current = false;
    }
}

impl Drop for SdlGlContext {
    /// Releases and destroys the SDL GL context.
    ///
    /// Maps to C++ `SDLGLContext::~SDLGLContext`.
    fn drop(&mut self) {
        // Maps to: DoneCurrent(); SDL_GL_DeleteContext(context)
        self.done_current();
        unsafe { sdl::SDL_GL_DeleteContext(self.context) };
    }
}

// ---------------------------------------------------------------------------
// EmuWindowSdl2Gl
// ---------------------------------------------------------------------------

/// OpenGL-backed SDL2 emulator window.
///
/// Maps to C++ class `EmuWindow_SDL2_GL` in
/// `yuzu_cmd/emu_window/emu_window_sdl2_gl.h`.
pub struct EmuWindowSdl2Gl {
    /// Shared base window state.
    base: EmuWindowSdl2,

    /// The main-thread OpenGL context.
    /// Maps to C++ `window_context`.
    window_context: sdl::SDL_GLContext,

    /// The core (GPU-thread) OpenGL context.
    /// Maps to C++ `core_context`.
    core_context: Option<Box<SdlGlContext>>,
}

impl EmuWindowSdl2Gl {
    /// Creates the window and an OpenGL 4.6 compatibility-profile context.
    /// Loads GL function pointers via `gl` crate (upstream uses GLAD).
    /// Validates required extensions before returning.
    ///
    /// Maps to C++ `EmuWindow_SDL2_GL::EmuWindow_SDL2_GL`.
    pub fn new(fullscreen: bool) -> Self {
        let mut base = EmuWindowSdl2::new();

        // Maps to SDL_GL_SetAttribute calls in upstream constructor.
        unsafe {
            sdl::SDL_GL_SetAttribute(sdl::SDL_GLattr::SDL_GL_CONTEXT_MAJOR_VERSION, 4);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GLattr::SDL_GL_CONTEXT_MINOR_VERSION, 6);
            sdl::SDL_GL_SetAttribute(
                sdl::SDL_GLattr::SDL_GL_CONTEXT_PROFILE_MASK,
                sdl::SDL_GLprofile::SDL_GL_CONTEXT_PROFILE_COMPATIBILITY as i32,
            );
            sdl::SDL_GL_SetAttribute(sdl::SDL_GLattr::SDL_GL_DOUBLEBUFFER, 1);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GLattr::SDL_GL_RED_SIZE, 8);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GLattr::SDL_GL_GREEN_SIZE, 8);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GLattr::SDL_GL_BLUE_SIZE, 8);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GLattr::SDL_GL_ALPHA_SIZE, 0);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GLattr::SDL_GL_SHARE_WITH_CURRENT_CONTEXT, 1);
            if *common::settings::values().renderer_debug.get_value() {
                sdl::SDL_GL_SetAttribute(
                    sdl::SDL_GLattr::SDL_GL_CONTEXT_FLAGS,
                    sdl::SDL_GLcontextFlag::SDL_GL_CONTEXT_DEBUG_FLAG as i32,
                );
            }
            sdl::SDL_GL_SetSwapInterval(0);
        }

        let window_title = b"yuzu-cmd (OpenGL)\0";
        let window_flags = sdl::SDL_WindowFlags::SDL_WINDOW_OPENGL as u32
            | sdl::SDL_WindowFlags::SDL_WINDOW_RESIZABLE as u32
            | sdl::SDL_WindowFlags::SDL_WINDOW_ALLOW_HIGHDPI as u32;

        // Maps to: render_window = SDL_CreateWindow(...)
        let render_window = unsafe {
            sdl::SDL_CreateWindow(
                window_title.as_ptr() as *const _,
                sdl::SDL_WINDOWPOS_UNDEFINED_MASK as i32,
                sdl::SDL_WINDOWPOS_UNDEFINED_MASK as i32,
                SCREEN_UNDOCKED_WIDTH,
                SCREEN_UNDOCKED_HEIGHT,
                window_flags,
            )
        };

        if render_window.is_null() {
            let err = unsafe { CStr::from_ptr(sdl::SDL_GetError()) }.to_string_lossy();
            log::error!("Failed to create SDL2 window! {}", err);
            std::process::exit(1);
        }

        base.render_window = render_window;

        // Maps to: SetWindowIcon()
        base.set_window_icon();

        // Maps to: if (fullscreen) { Fullscreen(); ShowCursor(false); }
        if fullscreen {
            base.fullscreen();
            base.show_cursor(false);
        }

        // Maps to: window_context = SDL_GL_CreateContext(render_window)
        let window_context = unsafe { sdl::SDL_GL_CreateContext(render_window) };
        if window_context.is_null() {
            let err = unsafe { CStr::from_ptr(sdl::SDL_GetError()) }.to_string_lossy();
            log::error!("Failed to create SDL2 GL context: {}", err);
            std::process::exit(1);
        }

        // Maps to: core_context = CreateSharedContext()
        let core_context_raw = SdlGlContext::new(render_window);
        if core_context_raw.context.is_null() {
            let err = unsafe { CStr::from_ptr(sdl::SDL_GetError()) }.to_string_lossy();
            log::error!("Failed to create shared SDL2 GL context: {}", err);
            std::process::exit(1);
        }

        // Maps to: gladLoadGLLoader(SDL_GL_GetProcAddress)
        // Using the `gl` crate's load_with instead of GLAD.
        gl::load_with(|s| {
            let cs = std::ffi::CString::new(s).unwrap();
            unsafe { sdl::SDL_GL_GetProcAddress(cs.as_ptr()) as *const _ }
        });

        let mut instance = EmuWindowSdl2Gl {
            base,
            window_context,
            core_context: Some(Box::new(core_context_raw)),
        };

        if !instance.supports_required_gl_extensions() {
            log::error!("GPU does not support all required OpenGL extensions! Exiting...");
            std::process::exit(1);
        }

        // Maps to: OnResize(); OnMinimalClientAreaChangeRequest(...); SDL_PumpEvents()
        instance.base.on_resize();
        instance.base.on_minimal_client_area_change_request(256, 256);
        unsafe { sdl::SDL_PumpEvents() };

        // Log GL version info.
        unsafe {
            let vendor = gl::GetString(gl::VENDOR);
            let renderer = gl::GetString(gl::RENDERER);
            let version = gl::GetString(gl::VERSION);
            if !vendor.is_null() && !renderer.is_null() && !version.is_null() {
                let vendor = CStr::from_ptr(vendor as *const _).to_string_lossy();
                let renderer_str = CStr::from_ptr(renderer as *const _).to_string_lossy();
                let version = CStr::from_ptr(version as *const _).to_string_lossy();
                log::info!("GL Vendor:   {}", vendor);
                log::info!("GL Renderer: {}", renderer_str);
                log::info!("GL Version:  {}", version);
            }

            // Initial clear to dark blue so the window isn't just garbage.
            gl::ClearColor(0.0, 0.0, 0.2, 1.0);
            gl::Clear(gl::COLOR_BUFFER_BIT);
            sdl::SDL_GL_SwapWindow(render_window);
        }

        log::info!("yuzu-cmd | OpenGL window initialized");

        instance
    }

    /// Creates a new shared GL context for off-thread use.
    ///
    /// Maps to C++ `EmuWindow_SDL2_GL::CreateSharedContext`.
    pub fn create_shared_context(&self) -> Box<SdlGlContext> {
        // Maps to: return std::make_unique<SDLGLContext>(render_window)
        Box::new(SdlGlContext::new(self.base.render_window))
    }

    /// Returns `true` if the current GL driver supports all extensions that
    /// yuzu requires.
    ///
    /// Maps to C++ `EmuWindow_SDL2_GL::SupportsRequiredGLExtensions`.
    fn supports_required_gl_extensions(&self) -> bool {
        // Upstream checks GLAD_GL_EXT_texture_compression_s3tc and
        // GLAD_GL_ARB_texture_compression_rgtc via glad extension flags.
        // The `gl` crate does not provide per-extension availability flags;
        // we check via glGetStringi instead.
        let mut unsupported: Vec<&str> = Vec::new();
        let mut has_s3tc = false;
        let mut has_rgtc = false;

        let num_extensions = {
            let mut n: gl::types::GLint = 0;
            unsafe { gl::GetIntegerv(gl::NUM_EXTENSIONS, &mut n) };
            n
        };

        for i in 0..num_extensions as u32 {
            let ext_ptr = unsafe { gl::GetStringi(gl::EXTENSIONS, i) };
            if ext_ptr.is_null() {
                continue;
            }
            let ext = unsafe { CStr::from_ptr(ext_ptr as *const _) }.to_string_lossy();
            if ext == "GL_EXT_texture_compression_s3tc" {
                has_s3tc = true;
            }
            if ext == "GL_ARB_texture_compression_rgtc" {
                has_rgtc = true;
            }
        }

        if !has_s3tc {
            unsupported.push("EXT_texture_compression_s3tc");
        }
        if !has_rgtc {
            unsupported.push("ARB_texture_compression_rgtc");
        }

        for ext in &unsupported {
            log::error!("Unsupported GL extension: {}", ext);
        }

        unsupported.is_empty()
    }

    /// Returns whether the window is still open.
    pub fn is_open(&self) -> bool {
        self.base.is_open()
    }

    /// Waits for and dispatches the next SDL event.
    pub fn wait_event(&mut self) {
        self.base.wait_event();
    }

    /// Polls all pending events without blocking.
    pub fn poll_events(&mut self) {
        self.base.poll_events();
    }

    /// Swaps the front and back buffers.
    pub fn swap_buffers(&self) {
        unsafe { sdl::SDL_GL_SwapWindow(self.base.render_window) };
    }

    /// Get the window drawable size in pixels.
    pub fn get_drawable_size(&self) -> (i32, i32) {
        self.base.get_drawable_size()
    }
}

impl Drop for EmuWindowSdl2Gl {
    /// Destroys the shared core context, then the window GL context.
    ///
    /// Maps to C++ `EmuWindow_SDL2_GL::~EmuWindow_SDL2_GL`.
    fn drop(&mut self) {
        // Maps to: core_context.reset(); SDL_GL_DeleteContext(window_context)
        self.core_context = None;
        unsafe { sdl::SDL_GL_DeleteContext(self.window_context) };
        // base drop calls SDL_Quit
    }
}
