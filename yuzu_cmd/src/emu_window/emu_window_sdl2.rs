// SPDX-FileCopyrightText: 2016 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Base SDL2 emulator window.
//!
//! Port of `yuzu_cmd/emu_window/emu_window_sdl2.h` and
//! `yuzu_cmd/emu_window/emu_window_sdl2.cpp`.
//!
//! `EmuWindowSdl2` is the base SDL2-backed window type. It handles SDL event
//! processing, keyboard/mouse/touch input forwarding to `InputSubsystem`, and
//! window lifecycle management. Derived types provide the graphics-context
//! specific initialization (OpenGL, Vulkan, Null).

use sdl2::sys as sdl;
use std::ffi::CStr;

// SDL_TOUCH_MOUSEID is defined in SDL_touch.h as ((Uint32)-1).
// It is not exported by sdl2-sys as a Rust constant, so we define it here.
const SDL_TOUCH_MOUSEID: u32 = u32::MAX;

/// A no-op graphics context used as a placeholder.
/// Maps to C++ `DummyContext` in `emu_window_sdl2.h`.
pub struct DummyContext;

/// SDL2-based emulator window base.
///
/// Maps to C++ class `EmuWindow_SDL2` in
/// `yuzu_cmd/emu_window/emu_window_sdl2.h`.
pub struct EmuWindowSdl2 {
    /// Whether the window is still open (close not yet requested).
    /// Maps to C++ `is_open`.
    pub is_open: bool,

    /// Whether the window is shown (not minimized).
    /// Maps to C++ `is_shown`.
    pub is_shown: bool,

    /// Tracks when the title bar was last updated (SDL ticks).
    /// Maps to C++ `last_time`.
    pub last_time: u32,

    /// Raw SDL2 window pointer.
    /// Maps to C++ `render_window`.
    pub render_window: *mut sdl::SDL_Window,
}

impl EmuWindowSdl2 {
    /// Creates a new SDL2 window, initializing SDL2 subsystems and the input
    /// subsystem.
    ///
    /// Maps to C++ `EmuWindow_SDL2::EmuWindow_SDL2`.
    ///
    /// # Safety
    /// Calls into SDL2 C API. The caller must ensure SDL2 is not already
    /// initialized in an incompatible way. Exits the process on failure,
    /// matching upstream behavior.
    pub fn new() -> Self {
        // Maps to: input_subsystem->Initialize(); (stubbed — InputSubsystem not yet ported)
        // Maps to: SDL_Init(SDL_INIT_VIDEO | SDL_INIT_JOYSTICK | SDL_INIT_GAMECONTROLLER)
        unsafe {
            let num_drivers = sdl::SDL_GetNumVideoDrivers();
            let mut drivers = Vec::new();
            for i in 0..num_drivers {
                let name = sdl::SDL_GetVideoDriver(i);
                if !name.is_null() {
                    drivers.push(CStr::from_ptr(name).to_string_lossy().into_owned());
                }
            }
            log::info!("SDL video drivers available: {:?}", drivers);
        }

        let ret = unsafe {
            sdl::SDL_Init(
                sdl::SDL_INIT_VIDEO | sdl::SDL_INIT_JOYSTICK | sdl::SDL_INIT_GAMECONTROLLER,
            )
        };
        if ret < 0 {
            let err = unsafe { CStr::from_ptr(sdl::SDL_GetError()) }
                .to_string_lossy();
            log::error!("Failed to initialize SDL2: {}, Exiting...", err);
            std::process::exit(1);
        }

        unsafe {
            let current_driver = sdl::SDL_GetCurrentVideoDriver();
            let current_driver = if current_driver.is_null() {
                "<null>".to_string()
            } else {
                CStr::from_ptr(current_driver).to_string_lossy().into_owned()
            };
            let display_count = sdl::SDL_GetNumVideoDisplays();
            log::info!(
                "SDL initialized with video driver {:?}, display_count={}",
                current_driver,
                display_count
            );
            if display_count <= 0 {
                let err = CStr::from_ptr(sdl::SDL_GetError()).to_string_lossy();
                log::error!(
                    "SDL reports no displays after initialization. SDL_GetError={}",
                    err
                );
            }
        }

        // Maps to: SDL_SetMainReady()
        unsafe { sdl::SDL_SetMainReady() };

        EmuWindowSdl2 {
            is_open: true,
            is_shown: true,
            last_time: 0,
            render_window: std::ptr::null_mut(),
        }
    }

    /// Returns whether the window is still open (no close request yet).
    ///
    /// Maps to C++ `EmuWindow_SDL2::IsOpen`.
    pub fn is_open(&self) -> bool {
        self.is_open
    }

    /// Returns whether the window is shown (not minimized).
    ///
    /// Maps to C++ `EmuWindow_SDL2::IsShown`.
    pub fn is_shown(&self) -> bool {
        self.is_shown
    }

    /// Waits for and dispatches the next SDL event.
    /// Called on the main thread.
    ///
    /// Maps to C++ `EmuWindow_SDL2::WaitEvent`.
    pub fn wait_event(&mut self) {
        // Maps to: SDL_WaitEvent dispatch loop
        let mut event: sdl::SDL_Event = unsafe { std::mem::zeroed() };
        let ret = unsafe { sdl::SDL_WaitEvent(&mut event) };
        if ret == 0 {
            let err_ptr = unsafe { sdl::SDL_GetError() };
            let err = unsafe { CStr::from_ptr(err_ptr) }.to_string_lossy();
            if err.is_empty() {
                // SDL spurious wakeup — see upstream comment about SDL issue #5780.
                return;
            }
            log::error!("SDL_WaitEvent failed: {}", err);
            std::process::exit(1);
        }

        self.dispatch_event(&event);
        if self.is_open {
            self.update_title_bar();
        }
    }

    /// Polls and dispatches all pending SDL events without blocking.
    /// Returns true if at least one event was processed.
    ///
    /// Used by the GL render loop which needs to run continuously.
    pub fn poll_events(&mut self) -> bool {
        let mut had_events = false;
        let mut event: sdl::SDL_Event = unsafe { std::mem::zeroed() };
        while unsafe { sdl::SDL_PollEvent(&mut event) } != 0 {
            self.dispatch_event(&event);
            had_events = true;
        }
        if self.is_open {
            self.update_title_bar();
        }
        had_events
    }

    /// Get the window drawable size in pixels.
    pub fn get_drawable_size(&self) -> (i32, i32) {
        let mut w: i32 = 0;
        let mut h: i32 = 0;
        unsafe { sdl::SDL_GL_GetDrawableSize(self.render_window, &mut w, &mut h) };
        (w, h)
    }

    fn update_title_bar(&mut self) {
        // Update window title every ~2 seconds.
        let current_time = unsafe { sdl::SDL_GetTicks() };
        if current_time > self.last_time + 2000 {
            let title = b"yuzu-cmd\0";
            unsafe {
                sdl::SDL_SetWindowTitle(self.render_window, title.as_ptr() as *const _);
            }
            self.last_time = current_time;
        }
    }

    fn dispatch_event(&mut self, event: &sdl::SDL_Event) {
        use sdl::SDL_EventType::*;
        use sdl::SDL_WindowEventID::*;
        let event_type = unsafe { event.type_ };
        match event_type {
            x if x == SDL_WINDOWEVENT as u32 => {
                let window_event = unsafe { event.window.event } as u32;
                match window_event {
                    x if x == SDL_WINDOWEVENT_SIZE_CHANGED as u32
                        || x == SDL_WINDOWEVENT_RESIZED as u32
                        || x == SDL_WINDOWEVENT_MAXIMIZED as u32
                        || x == SDL_WINDOWEVENT_RESTORED as u32 =>
                    {
                        self.on_resize();
                    }
                    x if x == SDL_WINDOWEVENT_MINIMIZED as u32 => {
                        self.is_shown = false;
                        self.on_resize();
                    }
                    x if x == SDL_WINDOWEVENT_EXPOSED as u32 => {
                        self.is_shown = true;
                        self.on_resize();
                    }
                    x if x == SDL_WINDOWEVENT_CLOSE as u32 => {
                        self.is_open = false;
                    }
                    _ => {}
                }
            }
            x if x == SDL_KEYDOWN as u32 || x == SDL_KEYUP as u32 => {
                let scancode = unsafe { event.key.keysym.scancode } as i32;
                let state = unsafe { event.key.state } as u8;
                self.on_key_event(scancode, state);
            }
            x if x == SDL_MOUSEMOTION as u32 => {
                let which = unsafe { event.motion.which };
                if which != SDL_TOUCH_MOUSEID {
                    let x = unsafe { event.motion.x };
                    let y = unsafe { event.motion.y };
                    self.on_mouse_motion(x, y);
                }
            }
            x if x == SDL_MOUSEBUTTONDOWN as u32 || x == SDL_MOUSEBUTTONUP as u32 => {
                let which = unsafe { event.button.which };
                if which != SDL_TOUCH_MOUSEID {
                    let button = unsafe { event.button.button } as u32;
                    let state = unsafe { event.button.state } as u8;
                    let x = unsafe { event.button.x };
                    let y = unsafe { event.button.y };
                    self.on_mouse_button(button, state, x, y);
                }
            }
            x if x == SDL_FINGERDOWN as u32 => {
                let x = unsafe { event.tfinger.x };
                let y = unsafe { event.tfinger.y };
                let id = unsafe { event.tfinger.touchId } as usize;
                self.on_finger_down(x, y, id);
            }
            x if x == SDL_FINGERMOTION as u32 => {
                let x = unsafe { event.tfinger.x };
                let y = unsafe { event.tfinger.y };
                let id = unsafe { event.tfinger.touchId } as usize;
                self.on_finger_motion(x, y, id);
            }
            x if x == SDL_FINGERUP as u32 => {
                self.on_finger_up();
            }
            x if x == SDL_QUIT as u32 => {
                self.is_open = false;
            }
            _ => {}
        }
    }

    /// Loads and sets the window icon from the embedded yuzu.bmp data.
    ///
    /// Maps to C++ `EmuWindow_SDL2::SetWindowIcon`.
    /// Note: The embedded icon data (yuzu_icon / yuzu_icon_size from yuzu_icon.h)
    /// is not ported. This logs a warning and returns early, matching the upstream
    /// graceful-failure path.
    pub fn set_window_icon(&self) {
        // Upstream: SDL_RWFromConstMem((void*)yuzu_icon, yuzu_icon_size)
        // then SDL_LoadBMP_RW / SDL_SetWindowIcon / SDL_FreeSurface.
        // The embedded BMP data from yuzu_icon.h is not ported.
        log::warn!("set_window_icon: embedded icon data not ported, skipping.");
    }

    // -----------------------------------------------------------------------
    // Protected helpers — called from wait_event
    // -----------------------------------------------------------------------

    /// Called when a key is pressed or released.
    ///
    /// Maps to C++ `EmuWindow_SDL2::OnKeyEvent`.
    /// Note: InputSubsystem::GetKeyboard not yet ported — logs and returns.
    pub(crate) fn on_key_event(&mut self, key: i32, state: u8) {
        // Upstream: input_subsystem->GetKeyboard()->PressKey(key) / ReleaseKey(key)
        // InputSubsystem not yet ported.
        let _ = (key, state);
        log::trace!("on_key_event: key={} state={} (InputSubsystem not yet ported)", key, state);
    }

    /// Converts an SDL mouse button constant to the `MouseButton` enum used by
    /// `InputCommon`.
    ///
    /// Maps to C++ `EmuWindow_SDL2::SDLButtonToMouseButton`.
    pub(crate) fn sdl_button_to_mouse_button(&self, button: u32) -> MouseButton {
        // SDL_BUTTON_LEFT=1, SDL_BUTTON_MIDDLE=2, SDL_BUTTON_RIGHT=3,
        // SDL_BUTTON_X1=4, SDL_BUTTON_X2=5
        match button {
            1 => MouseButton::Left,   // SDL_BUTTON_LEFT
            3 => MouseButton::Right,  // SDL_BUTTON_RIGHT
            2 => MouseButton::Wheel,  // SDL_BUTTON_MIDDLE
            4 => MouseButton::Backward, // SDL_BUTTON_X1
            5 => MouseButton::Forward,  // SDL_BUTTON_X2
            _ => MouseButton::Undefined,
        }
    }

    /// Translates a pixel-space position to a normalized touch position.
    ///
    /// Maps to C++ `EmuWindow_SDL2::MouseToTouchPos`.
    pub(crate) fn mouse_to_touch_pos(&self, touch_x: i32, touch_y: i32) -> (f32, f32) {
        // Maps to: int w, h; SDL_GetWindowSize(render_window, &w, &h);
        let mut w: i32 = 1;
        let mut h: i32 = 1;
        if !self.render_window.is_null() {
            unsafe { sdl::SDL_GetWindowSize(self.render_window, &mut w, &mut h) };
        }
        let w = w.max(1);
        let h = h.max(1);
        let fx = (touch_x as f32) / (w as f32);
        let fy = (touch_y as f32) / (h as f32);
        (fx.clamp(0.0, 1.0), fy.clamp(0.0, 1.0))
    }

    /// Called when a mouse button is pressed or released.
    ///
    /// Maps to C++ `EmuWindow_SDL2::OnMouseButton`.
    /// Note: InputSubsystem not yet ported — logs and returns.
    pub(crate) fn on_mouse_button(&mut self, button: u32, state: u8, x: i32, y: i32) {
        // Upstream: SDLButtonToMouseButton + input_subsystem->GetMouse()->PressButton / ReleaseButton
        let _mouse_button = self.sdl_button_to_mouse_button(button);
        let _ = (state, x, y);
        log::trace!("on_mouse_button: button={} state={} (InputSubsystem not yet ported)", button, state);
    }

    /// Called when the mouse cursor moves.
    ///
    /// Maps to C++ `EmuWindow_SDL2::OnMouseMotion`.
    /// Note: InputSubsystem not yet ported — logs and returns.
    pub(crate) fn on_mouse_motion(&mut self, x: i32, y: i32) {
        // Upstream: MouseToTouchPos + input_subsystem->GetMouse()->Move / MouseMove / TouchMove
        let _pos = self.mouse_to_touch_pos(x, y);
        log::trace!("on_mouse_motion: x={} y={} (InputSubsystem not yet ported)", x, y);
    }

    /// Called when a finger starts touching the touchscreen.
    ///
    /// Maps to C++ `EmuWindow_SDL2::OnFingerDown`.
    /// Note: InputSubsystem not yet ported — logs and returns.
    pub(crate) fn on_finger_down(&mut self, x: f32, y: f32, id: usize) {
        // Upstream: input_subsystem->GetTouchScreen()->TouchPressed(x, y, id)
        log::trace!("on_finger_down: x={} y={} id={} (InputSubsystem not yet ported)", x, y, id);
    }

    /// Called when a finger moves on the touchscreen.
    ///
    /// Maps to C++ `EmuWindow_SDL2::OnFingerMotion`.
    /// Note: InputSubsystem not yet ported — logs and returns.
    pub(crate) fn on_finger_motion(&mut self, x: f32, y: f32, id: usize) {
        // Upstream: input_subsystem->GetTouchScreen()->TouchMoved(x, y, id)
        log::trace!("on_finger_motion: x={} y={} id={} (InputSubsystem not yet ported)", x, y, id);
    }

    /// Called when a finger lifts from the touchscreen.
    ///
    /// Maps to C++ `EmuWindow_SDL2::OnFingerUp`.
    /// Note: InputSubsystem not yet ported — logs and returns.
    pub(crate) fn on_finger_up(&mut self) {
        // Upstream: input_subsystem->GetTouchScreen()->ReleaseAllTouch()
        log::trace!("on_finger_up (InputSubsystem not yet ported)");
    }

    /// Called when the window is resized or restored.
    ///
    /// Maps to C++ `EmuWindow_SDL2::OnResize`.
    /// Note: UpdateCurrentFramebufferLayout depends on Core::Frontend not yet ported.
    pub(crate) fn on_resize(&mut self) {
        // Maps to: int width, height; SDL_GL_GetDrawableSize(render_window, &width, &height);
        // then UpdateCurrentFramebufferLayout(width, height).
        if self.render_window.is_null() {
            return;
        }
        let mut width: i32 = 0;
        let mut height: i32 = 0;
        unsafe { sdl::SDL_GL_GetDrawableSize(self.render_window, &mut width, &mut height) };
        // UpdateCurrentFramebufferLayout not yet ported (Core::Frontend).
        log::trace!("on_resize: {}x{} (UpdateCurrentFramebufferLayout not yet ported)", width, height);
    }

    /// Shows or hides the mouse cursor.
    ///
    /// Maps to C++ `EmuWindow_SDL2::ShowCursor`.
    pub(crate) fn show_cursor(&self, show: bool) {
        // Maps to: SDL_ShowCursor(show_cursor ? SDL_ENABLE : SDL_DISABLE)
        let toggle = if show { sdl::SDL_ENABLE as i32 } else { sdl::SDL_DISABLE as i32 };
        unsafe { sdl::SDL_ShowCursor(toggle) };
    }

    /// Applies the current fullscreen mode setting.
    ///
    /// Maps to C++ `EmuWindow_SDL2::Fullscreen`.
    pub(crate) fn fullscreen(&self) {
        if self.render_window.is_null() {
            return;
        }
        // Upstream checks Settings::values.fullscreen_mode.GetValue():
        //   Exclusive  -> SDL_WINDOW_FULLSCREEN (with desktop-mode size first)
        //   Borderless -> SDL_WINDOW_FULLSCREEN_DESKTOP
        //   fallback   -> SDL_MaximizeWindow
        let fullscreen_mode = *common::settings::values().fullscreen_mode.get_value();
        let sdl_flag = match fullscreen_mode {
            common::settings::FullscreenMode::Exclusive => {
                sdl::SDL_WindowFlags::SDL_WINDOW_FULLSCREEN as u32
            }
            _ => sdl::SDL_WindowFlags::SDL_WINDOW_FULLSCREEN_DESKTOP as u32,
        };
        let ret = unsafe {
            sdl::SDL_SetWindowFullscreen(self.render_window, sdl_flag)
        };
        if ret != 0 {
            let err = unsafe { CStr::from_ptr(sdl::SDL_GetError()) }.to_string_lossy();
            log::error!("Borderless fullscreening failed: {}", err);
            log::info!("Falling back on a maximised window...");
            unsafe { sdl::SDL_MaximizeWindow(self.render_window) };
        }
    }

    /// Called when the minimum client area size changes.
    ///
    /// Maps to C++ `EmuWindow_SDL2::OnMinimalClientAreaChangeRequest`.
    pub(crate) fn on_minimal_client_area_change_request(&self, min_width: u32, min_height: u32) {
        // Maps to: SDL_SetWindowMinimumSize(render_window, minimal_size.first, minimal_size.second)
        if !self.render_window.is_null() {
            unsafe {
                sdl::SDL_SetWindowMinimumSize(
                    self.render_window,
                    min_width as i32,
                    min_height as i32,
                )
            };
        }
    }
}

impl Drop for EmuWindowSdl2 {
    /// Shuts down the input subsystem and SDL2.
    ///
    /// Maps to C++ `EmuWindow_SDL2::~EmuWindow_SDL2`.
    fn drop(&mut self) {
        // Upstream: system.HIDCore().UnloadInputDevices(); input_subsystem->Shutdown(); SDL_Quit();
        // HIDCore/InputSubsystem not yet ported.
        log::debug!("EmuWindowSdl2::drop — calling SDL_Quit (HIDCore/InputSubsystem not yet ported)");
        unsafe { sdl::SDL_Quit() };
    }
}

/// Mouse button identifiers forwarded from InputCommon.
///
/// Mirrors `InputCommon::MouseButton` used in the C++ port.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MouseButton {
    Left,
    Right,
    Wheel,
    Backward,
    Forward,
    Undefined,
}
