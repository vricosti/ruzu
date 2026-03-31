// SPDX-FileCopyrightText: 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Null-renderer SDL2 emulator window.
//!
//! Port of `yuzu_cmd/emu_window/emu_window_sdl2_null.h` and
//! `yuzu_cmd/emu_window/emu_window_sdl2_null.cpp`.
//!
//! `EmuWindowSdl2Null` creates a plain SDL2 window without any graphics API
//! context. It is used with the `RendererNull` backend, which renders nothing
//! and is useful for benchmarking CPU-only performance without GPU overhead.
//!
//! `CreateSharedContext` returns a `DummyContext`, mirroring the Vulkan
//! variant, since there is no real graphics context to share.

use sdl2::sys as sdl;
use std::ffi::CStr;

use super::emu_window_sdl2::{DummyContext, EmuWindowSdl2};

// Screen layout constants.
// Maps to C++ `Layout::ScreenUndocked::Width` / `Layout::ScreenUndocked::Height`.
const SCREEN_UNDOCKED_WIDTH: i32 = 1280;
const SCREEN_UNDOCKED_HEIGHT: i32 = 720;

/// Null-renderer SDL2 emulator window.
///
/// Maps to C++ class `EmuWindow_SDL2_Null` in
/// `yuzu_cmd/emu_window/emu_window_sdl2_null.h`.
pub struct EmuWindowSdl2Null {
    /// Shared base window state.
    base: EmuWindowSdl2,
}

impl EmuWindowSdl2Null {
    /// Creates the SDL2 window for use with the null renderer.
    ///
    /// Maps to C++ `EmuWindow_SDL2_Null::EmuWindow_SDL2_Null`.
    pub fn new(fullscreen: bool) -> Self {
        let mut base = EmuWindowSdl2::new();

        let window_title = b"yuzu-cmd (Null)\0";
        // No OpenGL/Vulkan flags — plain resizable window.
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

        // Maps to: SetWindowIcon()
        base.set_window_icon();

        // Maps to: if (fullscreen) { Fullscreen(); ShowCursor(false); }
        if fullscreen {
            base.fullscreen();
            base.show_cursor(false);
        }

        // Maps to: OnResize(); OnMinimalClientAreaChangeRequest(...); SDL_PumpEvents()
        base.on_resize();
        base.on_minimal_client_area_change_request(256, 256);
        unsafe { sdl::SDL_PumpEvents() };

        log::info!("yuzu-cmd | Null window initialized");

        EmuWindowSdl2Null { base }
    }

    /// Returns a `DummyContext` — no real graphics context is needed.
    ///
    /// Maps to C++ `EmuWindow_SDL2_Null::CreateSharedContext`.
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

    /// Returns the raw SDL window pointer.
    pub fn raw_window(&self) -> *mut sdl::SDL_Window {
        self.base.render_window
    }
}

impl Drop for EmuWindowSdl2Null {
    /// Default destructor — base `EmuWindowSdl2` handles SDL cleanup.
    ///
    /// Maps to C++ `EmuWindow_SDL2_Null::~EmuWindow_SDL2_Null` (`= default`).
    fn drop(&mut self) {}
}
