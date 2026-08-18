// SPDX-FileCopyrightText: 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Vulkan SDL3 emulator window.
//!
//! Port of `yuzu_cmd/emu_window/emu_window_sdl3_vk.h` and
//! `yuzu_cmd/emu_window/emu_window_sdl3_vk.cpp`.
//!
//! `EmuWindowSdl3Vk` creates an SDL3 window suitable for Vulkan rendering.
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

use sdl3::sys::everything as sdl;
use std::ffi::CStr;

use super::emu_window_sdl3::{DummyContext, EmuWindowSdl3};
use ruzu_core::core::SystemRef;
use ruzu_core::frontend::emu_window::{WindowSystemInfo, WindowSystemType};
use ruzu_core::frontend::framebuffer_layout::FramebufferLayout;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, RwLock};

// Screen layout constants.
// Maps to C++ `Layout::ScreenUndocked::Width` / `Layout::ScreenUndocked::Height`.
const SCREEN_UNDOCKED_WIDTH: i32 = 1280;
const SCREEN_UNDOCKED_HEIGHT: i32 = 720;

fn query_vulkan_drawable_size(render_window: *mut sdl::SDL_Window) -> (u32, u32) {
    let mut width: i32 = 0;
    let mut height: i32 = 0;
    unsafe {
        sdl::SDL_GetWindowSizeInPixels(render_window, &mut width, &mut height);
    }
    if width <= 0 || height <= 0 {
        (SCREEN_UNDOCKED_WIDTH as u32, SCREEN_UNDOCKED_HEIGHT as u32)
    } else {
        (width as u32, height as u32)
    }
}

#[cfg(target_os = "macos")]
fn validate_metal_view_and_layer(view: sdl::SDL_MetalView, layer: *mut std::ffi::c_void) {
    use objc::runtime::{Class, Object, BOOL, NO};
    use objc::{msg_send, sel, sel_impl};

    unsafe {
        let Some(metal_layer_class) = Class::get("CAMetalLayer") else {
            return;
        };
        let layer_object = layer as *mut Object;
        if layer_object.is_null() {
            return;
        }
        let is_metal_layer: BOOL = msg_send![layer_object, isKindOfClass: metal_layer_class];
        if is_metal_layer == NO {
            log::error!("SDL_Metal_GetLayer returned a non-CAMetalLayer object");
            std::process::exit(1);
        }

        let view_object = view as *mut Object;
        trace_macos_window_state(view_object, layer_object);
    }
}

#[cfg(target_os = "macos")]
#[repr(C)]
#[derive(Clone, Copy)]
struct CgSize {
    width: f64,
    height: f64,
}

#[cfg(target_os = "macos")]
fn trace_macos_window_state(view: *mut objc::runtime::Object, layer: *mut objc::runtime::Object) {
    if std::env::var_os("RUZU_TRACE_MACOS_WINDOW").is_none() {
        return;
    }

    use objc::runtime::{Object, BOOL};
    use objc::{msg_send, sel, sel_impl};

    unsafe {
        let window: *mut Object = if view.is_null() {
            std::ptr::null_mut()
        } else {
            msg_send![view, window]
        };
        let window_number: i64 = if window.is_null() {
            -1
        } else {
            msg_send![window, windowNumber]
        };
        let is_visible: BOOL = if window.is_null() {
            false
        } else {
            msg_send![window, isVisible]
        };
        let is_miniaturized: BOOL = if window.is_null() {
            false
        } else {
            msg_send![window, isMiniaturized]
        };
        let is_key_window: BOOL = if window.is_null() {
            false
        } else {
            msg_send![window, isKeyWindow]
        };
        let is_main_window: BOOL = if window.is_null() {
            false
        } else {
            msg_send![window, isMainWindow]
        };
        let view_layer: *mut Object = if view.is_null() {
            std::ptr::null_mut()
        } else {
            msg_send![view, layer]
        };
        let drawable_size: CgSize = if layer.is_null() {
            CgSize {
                width: 0.0,
                height: 0.0,
            }
        } else {
            msg_send![layer, drawableSize]
        };
        let contents_scale: f64 = if layer.is_null() {
            0.0
        } else {
            msg_send![layer, contentsScale]
        };
        log::info!(
            "[MACOS_WINDOW] nswindow={:?} window_number={} visible={} miniaturized={} key={} main={} layer={:?} view_layer={:?} layer_matches_view={} drawable={}x{} contents_scale={}",
            window,
            window_number,
            is_visible,
            is_miniaturized,
            is_key_window,
            is_main_window,
            layer,
            view_layer,
            layer == view_layer,
            drawable_size.width,
            drawable_size.height,
            contents_scale
        );
    }
}

/// Vulkan-backed SDL3 emulator window.
///
/// Maps to C++ class `EmuWindow_SDL3_VK` in
/// `yuzu_cmd/emu_window/emu_window_sdl3_vk.h`.
pub struct EmuWindowSdl3Vk {
    /// Shared base window state.
    base: EmuWindowSdl3,

    /// Native window-system data consumed by the Vulkan renderer.
    /// Maps to upstream `window_info`.
    window_info: WindowSystemInfo,

    /// SDL-owned Metal view used to keep the CAMetalLayer alive on macOS.
    #[cfg(target_os = "macos")]
    metal_view: sdl::SDL_MetalView,
}

impl EmuWindowSdl3Vk {
    /// Creates the SDL3 window and resolves the native window handle.
    ///
    /// Queries window-manager info via `SDL_GetWindowWMInfo` and fills in the
    /// window-system-info struct consumed by the Vulkan renderer.
    ///
    /// Maps to C++ `EmuWindow_SDL3_VK::EmuWindow_SDL3_VK`.
    pub fn new(system: SystemRef, fullscreen: bool) -> Self {
        let mut base = EmuWindowSdl3::new(system);

        let window_title = b"ruzu-cmd (Vulkan)\0";
        #[cfg(not(target_os = "macos"))]
        let window_flags = sdl::SDL_WINDOW_RESIZABLE | sdl::SDL_WINDOW_HIGH_PIXEL_DENSITY;
        #[cfg(target_os = "macos")]
        let window_flags =
            sdl::SDL_WINDOW_RESIZABLE | sdl::SDL_WINDOW_HIGH_PIXEL_DENSITY | sdl::SDL_WINDOW_METAL;
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

        base.render_window = render_window;

        let window_props = unsafe { sdl::SDL_GetWindowProperties(render_window) };

        // Maps to: SetWindowIcon()
        base.set_window_icon();

        // Maps to: if (fullscreen) { Fullscreen(); ShowCursor(false); }
        if fullscreen {
            base.fullscreen();
            base.show_cursor(false);
        }

        let mut window_info = WindowSystemInfo::default();
        unsafe {
            let hwnd = sdl::SDL_GetPointerProperty(
                window_props,
                sdl::SDL_PROP_WINDOW_WIN32_HWND_POINTER,
                std::ptr::null_mut(),
            );
            let wl_display = sdl::SDL_GetPointerProperty(
                window_props,
                sdl::SDL_PROP_WINDOW_WAYLAND_DISPLAY_POINTER,
                std::ptr::null_mut(),
            );
            let x11_display = sdl::SDL_GetPointerProperty(
                window_props,
                sdl::SDL_PROP_WINDOW_X11_DISPLAY_POINTER,
                std::ptr::null_mut(),
            );
            if !hwnd.is_null() {
                window_info.type_ = WindowSystemType::Windows;
                window_info.render_surface = hwnd as usize;
            } else if !wl_display.is_null() {
                let wl_surface = sdl::SDL_GetPointerProperty(
                    window_props,
                    sdl::SDL_PROP_WINDOW_WAYLAND_SURFACE_POINTER,
                    std::ptr::null_mut(),
                );
                if wl_surface.is_null() {
                    log::error!("Wayland surface is unavailable");
                    std::process::exit(1);
                }
                window_info.type_ = WindowSystemType::Wayland;
                window_info.display_connection = wl_display as usize;
                window_info.render_surface = wl_surface as usize;
            } else if !x11_display.is_null() {
                let x11_window = sdl::SDL_GetNumberProperty(
                    window_props,
                    sdl::SDL_PROP_WINDOW_X11_WINDOW_NUMBER,
                    0,
                );
                if x11_window == 0 {
                    log::error!("X11 window handle is unavailable");
                    std::process::exit(1);
                }
                window_info.type_ = WindowSystemType::X11;
                window_info.display_connection = x11_display as usize;
                window_info.render_surface = x11_window as usize;
            } else {
                #[cfg(not(any(target_os = "macos", target_os = "android")))]
                {
                    log::error!("Unable to determine native window backend from SDL properties");
                    std::process::exit(1);
                }
            }
        }

        #[cfg(target_os = "macos")]
        let metal_view = {
            // Upstream selects Cocoa before handing the Metal surface to the
            // Vulkan renderer. Leaving the default Headless type makes
            // CreateSurface reject an otherwise valid CAMetalLayer.
            window_info.type_ = WindowSystemType::Cocoa;
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
            validate_metal_view_and_layer(view, layer.cast());
            window_info.render_surface = layer as usize;
            view
        };

        // Maps to: OnResize(); OnMinimalClientAreaChangeRequest(...); SDL_PumpEvents()
        base.on_resize();
        let (drawable_width, drawable_height) = query_vulkan_drawable_size(render_window);
        base.update_current_framebuffer_layout(drawable_width, drawable_height);
        base.on_minimal_client_area_change_request(256, 256);
        unsafe { sdl::SDL_PumpEvents() };

        log::info!("ruzu-cmd | Vulkan window initialized");

        EmuWindowSdl3Vk {
            base,
            window_info,
            #[cfg(target_os = "macos")]
            metal_view,
        }
    }

    /// Returns a `DummyContext` — Vulkan does not require a shared GL context.
    ///
    /// Maps to C++ `EmuWindow_SDL3_VK::CreateSharedContext`.
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

    /// Shared framebuffer layout consumed by Vulkan presentation.
    pub fn framebuffer_layout(&self) -> Arc<RwLock<FramebufferLayout>> {
        self.base.framebuffer_layout()
    }

    /// Waits for and dispatches the next SDL event.
    pub fn wait_event(&mut self) {
        self.base.wait_event();
        let (width, height) = query_vulkan_drawable_size(self.base.render_window);
        self.base.update_current_framebuffer_layout(width, height);
    }

    /// Polls and dispatches pending SDL events without blocking.
    ///
    /// This mirrors the existing OpenGL frontend helper and is used only when
    /// the diagnostic `RUZU_POLL_EVENTS_LOOP` mode is enabled from `main`.
    pub fn poll_events(&mut self) {
        self.base.poll_events();
        let (width, height) = query_vulkan_drawable_size(self.base.render_window);
        self.base.update_current_framebuffer_layout(width, height);
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
        query_vulkan_drawable_size(self.base.render_window)
    }
}

impl Drop for EmuWindowSdl3Vk {
    /// Default destructor — base `EmuWindowSdl3` handles SDL cleanup.
    ///
    /// Maps to C++ `EmuWindow_SDL3_VK::~EmuWindow_SDL3_VK` (`= default`).
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
