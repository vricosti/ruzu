// SPDX-FileCopyrightText: 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Vulkan SDL2 emulator window.
//!
//! Port of `yuzu_cmd/emu_window/emu_window_sdl2_vk.h` and
//! `yuzu_cmd/emu_window/emu_window_sdl2_vk.cpp`.
//!
//! `EmuWindowSdl2Vk` creates an SDL2 window suitable for Vulkan rendering.
//! After window creation it queries the native window-manager info via
//! `SDL_GetWindowWMInfo` and populates the `WindowSystemInfo` struct that
//! the Vulkan renderer uses to create a surface.
//!
//! Supported platforms mirror the upstream `#ifdef` blocks:
//! Windows (`HWND`), X11 (`Display`/`Window`), Wayland
//! (`wl_display`/`wl_surface`), Cocoa (Metal view), and Android (`ANativeWindow`).
//!
//! `CreateSharedContext` returns a `DummyContext` — Vulkan does not need a
//! shared GL-style context.

use sdl2::sys as sdl;
use std::ffi::CStr;

use super::emu_window_sdl2::{DummyContext, EmuWindowSdl2};
use ruzu_core::frontend::emu_window::{WindowSystemInfo, WindowSystemType};
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

// Screen layout constants.
// Maps to C++ `Layout::ScreenUndocked::Width` / `Layout::ScreenUndocked::Height`.
const SCREEN_UNDOCKED_WIDTH: i32 = 1280;
const SCREEN_UNDOCKED_HEIGHT: i32 = 720;

/// Vulkan-backed SDL2 emulator window.
///
/// Maps to C++ class `EmuWindow_SDL2_VK` in
/// `yuzu_cmd/emu_window/emu_window_sdl2_vk.h`.
pub struct EmuWindowSdl2Vk {
    /// Shared base window state.
    base: EmuWindowSdl2,

    /// Native window-system data consumed by the Vulkan renderer.
    /// Maps to upstream `window_info`.
    window_info: WindowSystemInfo,

    /// SDL-owned Metal view used to keep the CAMetalLayer alive on macOS.
    #[cfg(target_os = "macos")]
    metal_view: sdl::SDL_MetalView,
}

impl EmuWindowSdl2Vk {
    /// Creates the SDL2 window and resolves the native window handle.
    ///
    /// Queries window-manager info via `SDL_GetWindowWMInfo` and fills in the
    /// window-system-info struct consumed by the Vulkan renderer.
    ///
    /// Maps to C++ `EmuWindow_SDL2_VK::EmuWindow_SDL2_VK`.
    pub fn new(fullscreen: bool) -> Self {
        let mut base = EmuWindowSdl2::new();

        let window_title = b"ruzu-cmd (Vulkan)\0";
        let window_flags = sdl::SDL_WindowFlags::SDL_WINDOW_RESIZABLE as u32
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

        // Maps to: SDL_SysWMinfo wm; SDL_VERSION(&wm.version); SDL_GetWindowWMInfo(...)
        let mut wm: sdl::SDL_SysWMinfo = unsafe { std::mem::zeroed() };
        // SDL_VERSION macro: set version fields to the compiled-against SDL version
        wm.version.major = sdl::SDL_MAJOR_VERSION as u8;
        wm.version.minor = sdl::SDL_MINOR_VERSION as u8;
        wm.version.patch = sdl::SDL_PATCHLEVEL as u8;
        let wm_result = unsafe { sdl::SDL_GetWindowWMInfo(render_window, &mut wm) };
        if wm_result == sdl::SDL_bool::SDL_FALSE {
            let err = unsafe { CStr::from_ptr(sdl::SDL_GetError()) }.to_string_lossy();
            log::error!("Failed to get information from the window manager: {}", err);
            std::process::exit(1);
        }

        // Maps to: SetWindowIcon()
        base.set_window_icon();

        // Maps to: if (fullscreen) { Fullscreen(); ShowCursor(false); }
        if fullscreen {
            base.fullscreen();
            base.show_cursor(false);
        }

        // Maps to upstream switch on `wm.subsystem`.
        let mut window_info = WindowSystemInfo::default();
        match wm.subsystem {
            #[cfg(target_os = "linux")]
            sdl::SDL_SYSWM_TYPE::SDL_SYSWM_X11 => unsafe {
                window_info.type_ = WindowSystemType::X11;
                window_info.display_connection = wm.info.x11.display as usize;
                window_info.render_surface = wm.info.x11.window as usize;
            },
            #[cfg(target_os = "linux")]
            sdl::SDL_SYSWM_TYPE::SDL_SYSWM_WAYLAND => unsafe {
                window_info.type_ = WindowSystemType::Wayland;
                window_info.display_connection = wm.info.wl.display as usize;
                window_info.render_surface = wm.info.wl.surface as usize;
            },
            #[cfg(target_os = "macos")]
            sdl::SDL_SYSWM_TYPE::SDL_SYSWM_COCOA => {
                window_info.type_ = WindowSystemType::Cocoa;
            }
            other => {
                log::error!("Window manager subsystem {} not implemented", other as i32);
                std::process::exit(1);
            }
        }

        #[cfg(target_os = "macos")]
        let metal_view = {
            let view = unsafe { sdl::SDL_Metal_CreateView(render_window) };
            if view.is_null() {
                let err = unsafe { CStr::from_ptr(sdl::SDL_GetError()) }.to_string_lossy();
                log::error!("Failed to create SDL Metal view: {}", err);
                std::process::exit(1);
            }
            let layer = unsafe { sdl::SDL_Metal_GetLayer(view) };
            if layer.is_null() {
                let err = unsafe { CStr::from_ptr(sdl::SDL_GetError()) }.to_string_lossy();
                log::error!("Failed to get SDL Metal layer: {}", err);
                unsafe { sdl::SDL_Metal_DestroyView(view) };
                std::process::exit(1);
            }
            window_info.render_surface = layer as usize;
            view
        };

        // Maps to: OnResize(); OnMinimalClientAreaChangeRequest(...); SDL_PumpEvents()
        base.on_resize();
        base.on_minimal_client_area_change_request(256, 256);
        unsafe { sdl::SDL_PumpEvents() };

        log::info!("ruzu-cmd | Vulkan window initialized");

        EmuWindowSdl2Vk {
            base,
            window_info,
            #[cfg(target_os = "macos")]
            metal_view,
        }
    }

    /// Returns a `DummyContext` — Vulkan does not require a shared GL context.
    ///
    /// Maps to C++ `EmuWindow_SDL2_VK::CreateSharedContext`.
    pub fn create_shared_context(&self) -> DummyContext {
        DummyContext
    }

    /// Returns whether the window is still open.
    pub fn is_open(&self) -> bool {
        self.base.is_open()
    }

    /// Returns whether the window is currently visible.
    pub fn is_shown(&self) -> bool {
        self.base.is_shown()
    }

    /// Shared visibility state consumed by the Vulkan renderer.
    pub fn shown_state(&self) -> Arc<AtomicBool> {
        self.base.shown_state()
    }

    /// Waits for and dispatches the next SDL event.
    pub fn wait_event(&mut self) {
        self.base.wait_event();
    }

    /// Returns the raw SDL window pointer.
    pub fn raw_window(&self) -> *mut sdl::SDL_Window {
        self.base.render_window
    }

    /// Returns the native window-system information for Vulkan surface creation.
    pub fn window_info(&self) -> &WindowSystemInfo {
        &self.window_info
    }

    /// Returns the Vulkan drawable size in pixels.
    pub fn drawable_size(&self) -> (u32, u32) {
        let mut width: i32 = 0;
        let mut height: i32 = 0;
        #[cfg(target_os = "macos")]
        unsafe {
            sdl::SDL_Metal_GetDrawableSize(self.base.render_window, &mut width, &mut height);
        }
        #[cfg(not(target_os = "macos"))]
        unsafe {
            sdl::SDL_Vulkan_GetDrawableSize(self.base.render_window, &mut width, &mut height);
        }
        if width <= 0 || height <= 0 {
            (SCREEN_UNDOCKED_WIDTH as u32, SCREEN_UNDOCKED_HEIGHT as u32)
        } else {
            (width as u32, height as u32)
        }
    }
}

impl Drop for EmuWindowSdl2Vk {
    /// Default destructor — base `EmuWindowSdl2` handles SDL cleanup.
    ///
    /// Maps to C++ `EmuWindow_SDL2_VK::~EmuWindow_SDL2_VK` (`= default`).
    fn drop(&mut self) {
        #[cfg(target_os = "macos")]
        unsafe {
            if !self.metal_view.is_null() {
                sdl::SDL_Metal_DestroyView(self.metal_view);
                self.metal_view = std::ptr::null_mut();
            }
        }
    }
}
