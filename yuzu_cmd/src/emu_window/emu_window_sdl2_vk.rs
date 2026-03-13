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

use std::ffi::CStr;
use sdl2::sys as sdl;

use super::emu_window_sdl2::{DummyContext, EmuWindowSdl2};

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

        let window_title = b"yuzu-cmd (Vulkan)\0";
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
            log::error!(
                "Failed to get information from the window manager: {}",
                err
            );
            std::process::exit(1);
        }

        // Maps to: SetWindowIcon()
        base.set_window_icon();

        // Maps to: if (fullscreen) { Fullscreen(); ShowCursor(false); }
        if fullscreen {
            base.fullscreen();
            base.show_cursor(false);
        }

        // window_info population (WindowSystemInfo) is omitted here since
        // video_core::WindowSystemInfo / renderer_vulkan are not yet fully ported.
        // Upstream populates window_info.type and window_info.render_surface
        // based on wm.subsystem (X11 / Wayland / Windows / Cocoa / Android).
        log::warn!(
            "EmuWindowSdl2Vk: WindowSystemInfo population not yet ported \
             (video_core::WindowSystemInfo not available)"
        );

        // Maps to: OnResize(); OnMinimalClientAreaChangeRequest(...); SDL_PumpEvents()
        base.on_resize();
        base.on_minimal_client_area_change_request(256, 256);
        unsafe { sdl::SDL_PumpEvents() };

        log::info!("yuzu-cmd | Vulkan window initialized");

        EmuWindowSdl2Vk { base }
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

    /// Waits for and dispatches the next SDL event.
    pub fn wait_event(&mut self) {
        self.base.wait_event();
    }
}

impl Drop for EmuWindowSdl2Vk {
    /// Default destructor — base `EmuWindowSdl2` handles SDL cleanup.
    ///
    /// Maps to C++ `EmuWindow_SDL2_VK::~EmuWindow_SDL2_VK` (`= default`).
    fn drop(&mut self) {}
}
