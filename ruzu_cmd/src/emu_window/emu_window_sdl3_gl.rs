// SPDX-FileCopyrightText: 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! OpenGL SDL3 emulator window.
//!
//! Port of `yuzu_cmd/emu_window/emu_window_sdl3_gl.h` and
//! `yuzu_cmd/emu_window/emu_window_sdl3_gl.cpp`.
//!
//! `EmuWindowSdl3Gl` creates an SDL3 window with an OpenGL 4.6 compatibility-
//! profile context, checks for required GL extensions, and provides a
//! shared-context factory for the video core renderer.
//!
//! A helper type `SdlGlContext` (maps to C++ `SDLGLContext`) wraps an SDL GL
//! context to implement the `GraphicsContext` interface (swap buffers,
//! make-current / done-current).

use sdl3::sys::everything as sdl;
use std::ffi::CStr;
use std::sync::{Arc, Mutex, RwLock};

use super::emu_window_sdl3::EmuWindowSdl3;
use ruzu_core::core::SystemRef;
use ruzu_core::frontend::framebuffer_layout::FramebufferLayout;

// Screen layout constants.
// Maps to C++ `Layout::ScreenUndocked::Width` / `Layout::ScreenUndocked::Height`.
const SCREEN_UNDOCKED_WIDTH: i32 = 1280;
const SCREEN_UNDOCKED_HEIGHT: i32 = 720;

static SHARE_ANCHOR_MUTEX: Mutex<()> = Mutex::new(());
static mut SHARE_ANCHOR_CONTEXT: sdl::SDL_GLContext = std::ptr::null_mut();
static mut SHARE_ANCHOR_WINDOW: *mut sdl::SDL_Window = std::ptr::null_mut();

// ---------------------------------------------------------------------------
// SDLGLContext
// ---------------------------------------------------------------------------

/// SDL3 OpenGL context wrapper.
///
/// Maps to C++ class `SDLGLContext` defined inside
/// `yuzu_cmd/emu_window/emu_window_sdl3_gl.cpp`.
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
        // Maps to upstream SDLGLContext: make the main window context current
        // while creating shared contexts so strict drivers join the same GL
        // share group.
        let _anchor_lock = SHARE_ANCHOR_MUTEX.lock().unwrap();
        let saved_current = unsafe { sdl::SDL_GL_GetCurrentContext() };
        let saved_window = unsafe { sdl::SDL_GL_GetCurrentWindow() };
        unsafe {
            if !SHARE_ANCHOR_CONTEXT.is_null() && saved_current != SHARE_ANCHOR_CONTEXT {
                sdl::SDL_GL_MakeCurrent(SHARE_ANCHOR_WINDOW, SHARE_ANCHOR_CONTEXT);
            }
        }
        let context = unsafe { sdl::SDL_GL_CreateContext(window) };
        unsafe {
            sdl::SDL_GL_MakeCurrent(saved_window, saved_current);
        }
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
        if common::trace::is_enabled(common::trace::cat::PRESENT_COMPOSITE) {
            let _ = common::trace::emit_raw(
                common::trace::cat::PRESENT_COMPOSITE,
                &[5, 0, 0, 0, 0, 0, 0, 0],
            );
        }
        if std::env::var_os("RUZU_TRACE_PRESENT").is_some() {
            log::info!("[PRESENT] SDL_GL_SwapWindow");
        }
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
        self.is_current = ret;
        if !ret {
            let err = unsafe { CStr::from_ptr(sdl::SDL_GetError()) }.to_string_lossy();
            log::error!("SDL_GL_MakeCurrent failed: {}", err);
        } else {
            unsafe {
                sdl::SDL_GL_SetSwapInterval(0);
            }
            if common::trace::is_enabled(common::trace::cat::PRESENT_COMPOSITE) {
                let _ = common::trace::emit_raw(
                    common::trace::cat::PRESENT_COMPOSITE,
                    &[6, 0, 0, 0, 0, 0, 0, 0],
                );
            }
            if std::env::var_os("RUZU_TRACE_PRESENT").is_some() {
                log::info!("[PRESENT] SDL_GL_MakeCurrent");
            }
        }
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
        if common::trace::is_enabled(common::trace::cat::PRESENT_COMPOSITE) {
            let _ = common::trace::emit_raw(
                common::trace::cat::PRESENT_COMPOSITE,
                &[7, 0, 0, 0, 0, 0, 0, 0],
            );
        }
        self.is_current = false;
    }
}

// Safety: SDL GL contexts are designed to be used across threads (the GPU thread
// acquires the context via MakeCurrent). The SDL API guarantees thread-safety for
// GL context operations when properly managed (only one thread current at a time).
unsafe impl Send for SdlGlContext {}

impl ruzu_core::frontend::graphics_context::GraphicsContext for SdlGlContext {
    fn swap_buffers(&mut self) {
        SdlGlContext::swap_buffers(self);
    }

    fn make_current(&mut self) {
        SdlGlContext::make_current(self);
    }

    fn done_current(&mut self) {
        SdlGlContext::done_current(self);
    }
}

impl Drop for SdlGlContext {
    /// Releases and destroys the SDL GL context.
    ///
    /// Maps to C++ `SDLGLContext::~SDLGLContext`.
    fn drop(&mut self) {
        // Maps to: DoneCurrent(); SDL_GL_DestroyContext(context)
        self.done_current();
        unsafe { sdl::SDL_GL_DestroyContext(self.context) };
    }
}

// ---------------------------------------------------------------------------
// EmuWindowSdl3Gl
// ---------------------------------------------------------------------------

/// OpenGL-backed SDL3 emulator window.
///
/// Maps to C++ class `EmuWindow_SDL3_GL` in
/// `yuzu_cmd/emu_window/emu_window_sdl3_gl.h`.
pub struct EmuWindowSdl3Gl {
    /// Shared base window state.
    base: EmuWindowSdl3,

    /// The main-thread OpenGL context.
    /// Maps to C++ `window_context`.
    window_context: sdl::SDL_GLContext,

    /// The core (GPU-thread) OpenGL context.
    /// Maps to C++ `core_context`.
    core_context: Option<Box<SdlGlContext>>,

    /// Whether this SDL video driver requires all GL compilation on one context.
    strict_context_required: bool,
}

impl EmuWindowSdl3Gl {
    /// Creates the window and an OpenGL 4.6 compatibility-profile context.
    /// Loads GL function pointers via `gl` crate (upstream uses GLAD).
    /// Validates required extensions before returning.
    ///
    /// Maps to C++ `EmuWindow_SDL3_GL::EmuWindow_SDL3_GL`.
    pub fn new(system: SystemRef, fullscreen: bool) -> Self {
        let mut base = EmuWindowSdl3::new(system);

        // Maps to SDL_GL_SetAttribute calls in upstream constructor.
        // macOS only supports up to OpenGL 4.1 core profile (no compatibility profile).
        #[cfg(target_os = "macos")]
        let (gl_major, gl_minor, gl_profile) = (4, 1, sdl::SDL_GL_CONTEXT_PROFILE_CORE.0);
        #[cfg(not(target_os = "macos"))]
        let (gl_major, gl_minor, gl_profile) = (4, 6, sdl::SDL_GL_CONTEXT_PROFILE_COMPATIBILITY.0);
        unsafe {
            sdl::SDL_GL_SetAttribute(sdl::SDL_GL_CONTEXT_MAJOR_VERSION, gl_major);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GL_CONTEXT_MINOR_VERSION, gl_minor);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GL_CONTEXT_PROFILE_MASK, gl_profile);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GL_DOUBLEBUFFER, 1);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GL_RED_SIZE, 8);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GL_GREEN_SIZE, 8);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GL_BLUE_SIZE, 8);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GL_ALPHA_SIZE, 0);
            sdl::SDL_GL_SetAttribute(sdl::SDL_GL_SHARE_WITH_CURRENT_CONTEXT, 1);
            if *common::settings::values().renderer_debug.get_value() {
                sdl::SDL_GL_SetAttribute(
                    sdl::SDL_GL_CONTEXT_FLAGS,
                    sdl::SDL_GL_CONTEXT_DEBUG_FLAG.0,
                );
            }
            sdl::SDL_GL_SetSwapInterval(0);
        }

        let window_title = b"ruzu-cmd (OpenGL)\0";
        let window_flags =
            sdl::SDL_WINDOW_OPENGL | sdl::SDL_WINDOW_RESIZABLE | sdl::SDL_WINDOW_HIGH_PIXEL_DENSITY;

        // Maps to: render_window = SDL_CreateWindow(...)
        let render_window = unsafe {
            sdl::SDL_CreateWindow(
                window_title.as_ptr() as *const _,
                SCREEN_UNDOCKED_WIDTH,
                SCREEN_UNDOCKED_HEIGHT,
                window_flags,
            )
        };

        if render_window.is_null() {
            let err = unsafe { CStr::from_ptr(sdl::SDL_GetError()) }.to_string_lossy();
            log::error!("Failed to create SDL3 window! {}", err);
            std::process::exit(1);
        }

        let video_driver = unsafe { sdl::SDL_GetCurrentVideoDriver() };
        let strict_context_required = !video_driver.is_null()
            && unsafe { CStr::from_ptr(video_driver) }.to_bytes() == b"wayland";
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
            log::error!("Failed to create SDL3 GL context: {}", err);
            std::process::exit(1);
        }
        let make_current_ret = unsafe { sdl::SDL_GL_MakeCurrent(render_window, window_context) };
        if !make_current_ret {
            let err = unsafe { CStr::from_ptr(sdl::SDL_GetError()) }.to_string_lossy();
            log::error!("Failed to make SDL3 GL context current: {}", err);
            std::process::exit(1);
        }
        unsafe {
            sdl::SDL_GL_SetSwapInterval(0);
            let _anchor_lock = SHARE_ANCHOR_MUTEX.lock().unwrap();
            SHARE_ANCHOR_WINDOW = render_window;
            SHARE_ANCHOR_CONTEXT = window_context;
        }

        // Maps to: core_context = CreateSharedContext()
        let core_context_raw = SdlGlContext::new(render_window);
        if core_context_raw.context.is_null() {
            let err = unsafe { CStr::from_ptr(sdl::SDL_GetError()) }.to_string_lossy();
            log::error!("Failed to create shared SDL3 GL context: {}", err);
            std::process::exit(1);
        }

        // Maps to: gladLoadGLLoader(SDL_GL_GetProcAddress)
        // Using the `gl` crate's load_with instead of GLAD.
        gl::load_with(|s| {
            let cs = std::ffi::CString::new(s).unwrap();
            unsafe {
                sdl::SDL_GL_GetProcAddress(cs.as_ptr())
                    .map_or(std::ptr::null(), |proc| proc as *const () as *const _)
            }
        });

        let mut instance = EmuWindowSdl3Gl {
            base,
            window_context,
            core_context: Some(Box::new(core_context_raw)),
            strict_context_required,
        };

        if !instance.supports_required_gl_extensions() {
            log::error!("GPU does not support all required OpenGL extensions! Exiting...");
            std::process::exit(1);
        }

        // Maps to: OnResize(); OnMinimalClientAreaChangeRequest(...); SDL_PumpEvents()
        instance.base.on_resize();
        instance
            .base
            .on_minimal_client_area_change_request(256, 256);
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

        log::info!("ruzu-cmd | OpenGL window initialized");

        instance
    }

    /// Creates a new shared GL context for off-thread use.
    ///
    /// Maps to C++ `EmuWindow_SDL3_GL::CreateSharedContext`.
    pub fn create_shared_context(&self) -> Box<SdlGlContext> {
        // Maps to: return std::make_unique<SDLGLContext>(render_window)
        Box::new(SdlGlContext::new(self.base.render_window))
    }

    /// Matches `Core::Frontend::EmuWindow::StrictContextRequired()`.
    pub fn strict_context_required(&self) -> bool {
        self.strict_context_required
    }

    /// Shared framebuffer layout updated by the SDL resize path.
    ///
    /// Upstream passes this `EmuWindow` to `RendererOpenGL`, whose
    /// `Composite` reads `GetFramebufferLayout()` every frame.
    pub fn framebuffer_layout(&self) -> Arc<RwLock<FramebufferLayout>> {
        self.base.framebuffer_layout()
    }

    /// Returns `true` if the current GL driver supports all extensions that
    /// yuzu requires.
    ///
    /// Maps to C++ `EmuWindow_SDL3_GL::SupportsRequiredGLExtensions`.
    fn supports_required_gl_extensions(&self) -> bool {
        // Upstream checks GLAD_GL_EXT_texture_compression_s3tc and
        // GLAD_GL_ARB_texture_compression_rgtc via glad extension flags.
        // The `gl` crate does not provide per-extension availability flags;
        // we check via glGetStringi instead.
        //
        // Note: GL_ARB_texture_compression_rgtc was promoted to core in OpenGL 3.0.
        // On macOS core profile contexts (GL 4.1 core), the functionality is always
        // present but the extension string may not appear — so we also accept it
        // when the GL version is 3.0 or higher.
        let mut unsupported: Vec<&str> = Vec::new();
        let mut has_s3tc = false;
        let mut has_rgtc = false;

        // Check GL version for core-promoted extensions.
        let gl_version = {
            let mut major: gl::types::GLint = 0;
            let mut minor: gl::types::GLint = 0;
            unsafe {
                gl::GetIntegerv(gl::MAJOR_VERSION, &mut major);
                gl::GetIntegerv(gl::MINOR_VERSION, &mut minor);
            }
            (major, minor)
        };

        // RGTC is core since GL 3.0.
        if gl_version.0 > 3 || (gl_version.0 == 3 && gl_version.1 >= 0) {
            has_rgtc = true;
        }

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

    /// Returns the raw SDL window pointer.
    pub fn raw_window(&self) -> *mut sdl::SDL_Window {
        self.base.render_window
    }
}

impl Drop for EmuWindowSdl3Gl {
    /// Destroys the shared core context, then the window GL context.
    ///
    /// Maps to C++ `EmuWindow_SDL3_GL::~EmuWindow_SDL3_GL`.
    fn drop(&mut self) {
        // Maps to: core_context.reset(); SDL_GL_DestroyContext(window_context)
        self.core_context = None;
        unsafe { sdl::SDL_GL_DestroyContext(self.window_context) };
        // base drop calls SDL_Quit
    }
}
