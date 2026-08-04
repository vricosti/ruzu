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
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, RwLock};
use std::time::Duration;

use hid_core::frontend::emulated_controller::set_simple_npad_button;
use hid_core::hid_types::NpadButton;
use input_common::drivers::mouse::MouseButton;
use input_common::InputSubsystem;
use ruzu_core::core::SystemRef;
use ruzu_core::frontend::framebuffer_layout::{
    default_frame_layout, FramebufferLayout, ScreenUndocked,
};
use ruzu_core::perf_stats::PerfStatsResults;

// SDL_TOUCH_MOUSEID is defined in SDL_touch.h as ((Uint32)-1).
// It is not exported by sdl2-sys as a Rust constant, so we define it here.
const SDL_TOUCH_MOUSEID: u32 = u32::MAX;

/// Schedule an environment-gated L+R press for frontend diagnostics.
///
/// This uses the diagnostic Player1 NPad bridge, avoiding host
/// accessibility/automation permissions while testing title-screen input.
pub fn schedule_auto_lr_if_requested() {
    let Some(delay_ms) = std::env::var("RUZU_AUTO_LR_DELAY_MS")
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
    else {
        return;
    };
    let repeat_count = std::env::var("RUZU_AUTO_LR_REPEAT_COUNT")
        .ok()
        .and_then(|value| value.parse::<u32>().ok())
        .unwrap_or(1)
        .max(1);
    let repeat_ms = std::env::var("RUZU_AUTO_LR_REPEAT_MS")
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
        .unwrap_or(2_000);

    let _ = std::thread::Builder::new()
        .name("AutoInput".to_string())
        .spawn(move || {
            std::thread::sleep(Duration::from_millis(delay_ms));
            for attempt in 1..=repeat_count {
                log::info!("[AUTO_INPUT] pressing L+R attempt={attempt}/{repeat_count}");
                set_simple_npad_button(NpadButton::L, true);
                set_simple_npad_button(NpadButton::R, true);
                std::thread::sleep(Duration::from_millis(350));
                set_simple_npad_button(NpadButton::R, false);
                set_simple_npad_button(NpadButton::L, false);
                if attempt != repeat_count {
                    std::thread::sleep(Duration::from_millis(repeat_ms));
                }
            }
        });
}

/// Schedule environment-gated A presses for frontend diagnostics.
///
/// This is kept separate from the L+R trigger so a test can select the first
/// highlighted Mii after the title-screen input has been accepted.
pub fn schedule_auto_a_if_requested() {
    let Some(delay_ms) = std::env::var("RUZU_AUTO_A_DELAY_MS")
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
    else {
        return;
    };
    let repeat_count = std::env::var("RUZU_AUTO_A_REPEAT_COUNT")
        .ok()
        .and_then(|value| value.parse::<u32>().ok())
        .unwrap_or(1)
        .max(1);
    let repeat_ms = std::env::var("RUZU_AUTO_A_REPEAT_MS")
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
        .unwrap_or(2_000);
    let marker = std::env::var_os("RUZU_AUTO_A_MARKER");
    let marker_attempt = std::env::var("RUZU_AUTO_A_MARKER_ATTEMPT")
        .ok()
        .and_then(|value| value.parse::<u32>().ok())
        .unwrap_or(repeat_count);
    let marker_delay_ms = std::env::var("RUZU_AUTO_A_MARKER_DELAY_MS")
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
        .unwrap_or(500);

    let _ = std::thread::Builder::new()
        .name("AutoInputA".to_string())
        .spawn(move || {
            std::thread::sleep(Duration::from_millis(delay_ms));
            for attempt in 1..=repeat_count {
                log::info!("[AUTO_INPUT] pressing A attempt={attempt}/{repeat_count}");
                set_simple_npad_button(NpadButton::A, true);
                std::thread::sleep(Duration::from_millis(350));
                set_simple_npad_button(NpadButton::A, false);
                if attempt == marker_attempt {
                    if let Some(path) = marker.as_ref() {
                        std::thread::sleep(Duration::from_millis(marker_delay_ms));
                        let _ = std::fs::write(path, b"ready\n");
                    }
                }
                if attempt != repeat_count {
                    std::thread::sleep(Duration::from_millis(repeat_ms));
                }
            }
        });
}

/// Whether the environment-gated benchmark sampler owns the destructive
/// `PerfStats` read. The title bar reuses its last sample while this is set.
static PERF_LOG_ACTIVE: AtomicBool = AtomicBool::new(false);
static PERF_LOG_LAST_FPS_MILLI: AtomicU64 = AtomicU64::new(0);
static PERF_LOG_LAST_SPEED_MILLI: AtomicU64 = AtomicU64::new(0);

fn perf_log_last_results() -> PerfStatsResults {
    PerfStatsResults {
        average_game_fps: PERF_LOG_LAST_FPS_MILLI.load(Ordering::Relaxed) as f64 / 1_000.0,
        emulation_speed: PERF_LOG_LAST_SPEED_MILLI.load(Ordering::Relaxed) as f64 / 1_000.0,
        ..Default::default()
    }
}

/// Starts an optional fixed-interval performance sampler.
///
/// `update_title_bar` only runs when SDL delivers an event. The sampler makes
/// benchmark output independent of event frequency and does nothing unless
/// `RUZU_PERF_LOG` is configured.
pub fn schedule_perf_log_if_requested(system: SystemRef) {
    let Some(path) = std::env::var_os("RUZU_PERF_LOG") else {
        return;
    };
    let interval_ms = std::env::var("RUZU_PERF_LOG_INTERVAL_MS")
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
        .unwrap_or(1_000)
        .max(100);

    PERF_LOG_ACTIVE.store(true, Ordering::Relaxed);
    let spawn_result = std::thread::Builder::new()
        .name("PerfLog".to_string())
        .spawn(move || {
            use std::io::Write;

            let mut file = match std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&path)
            {
                Ok(file) => file,
                Err(error) => {
                    PERF_LOG_ACTIVE.store(false, Ordering::Relaxed);
                    log::error!("[PERF_LOG] cannot open {:?}: {error}", path);
                    return;
                }
            };
            let start = std::time::Instant::now();
            loop {
                std::thread::sleep(Duration::from_millis(interval_ms));
                if system.is_null() {
                    continue;
                }
                let results = system.get().get_and_reset_perf_stats();
                PERF_LOG_LAST_FPS_MILLI.store(
                    (results.average_game_fps * 1_000.0).max(0.0) as u64,
                    Ordering::Relaxed,
                );
                PERF_LOG_LAST_SPEED_MILLI.store(
                    (results.emulation_speed * 1_000.0).max(0.0) as u64,
                    Ordering::Relaxed,
                );
                let _ = writeln!(
                    file,
                    "{:.3} fps={:.2} system_fps={:.2} speed={:.2} frametime_ms={:.3}",
                    start.elapsed().as_secs_f64(),
                    results.average_game_fps,
                    results.system_fps,
                    results.emulation_speed * 100.0,
                    results.frametime * 1_000.0
                );
            }
        });
    if let Err(error) = spawn_result {
        PERF_LOG_ACTIVE.store(false, Ordering::Relaxed);
        log::error!("[PERF_LOG] cannot create sampler thread: {error}");
    }
}

/// A no-op graphics context used as a placeholder.
/// Maps to C++ `DummyContext` in `emu_window_sdl2.h`.
pub struct DummyContext;

/// SDL2-based emulator window base.
///
/// Maps to C++ class `EmuWindow_SDL2` in
/// `yuzu_cmd/emu_window/emu_window_sdl2.h`.
pub struct EmuWindowSdl2 {
    /// Host input drivers and their registered factories.
    /// Maps to C++ `input_subsystem`.
    pub input_subsystem: InputSubsystem,

    /// Whether the window is still open (close not yet requested).
    /// Maps to C++ `is_open`.
    pub is_open: bool,

    /// Whether the window is shown (not minimized).
    /// Maps to C++ `is_shown`.
    pub is_shown: bool,

    /// Shared visibility flag used by render backends running on the GPU thread.
    pub shown_state: Arc<AtomicBool>,

    /// Shared framebuffer layout used by render backends running on the GPU thread.
    pub framebuffer_layout: Arc<RwLock<FramebufferLayout>>,

    /// Tracks when the title bar was last updated (SDL ticks).
    /// Maps to C++ `last_time`.
    pub last_time: u32,

    /// Core instance used by the upstream title-bar performance update.
    pub system: SystemRef,

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
    pub fn new(system: SystemRef) -> Self {
        let mut input_subsystem = InputSubsystem::new();
        input_subsystem.initialize();
        // Rust binaries do not use SDL's SDL_main wrapper, so SDL must be
        // told that the application entry point is ready before SDL_Init().
        unsafe { sdl::SDL_SetMainReady() };

        // Maps to: SDL_Init(SDL_INIT_VIDEO | SDL_INIT_JOYSTICK | SDL_INIT_GAMECONTROLLER)
        let ret = unsafe {
            sdl::SDL_Init(
                sdl::SDL_INIT_VIDEO | sdl::SDL_INIT_JOYSTICK | sdl::SDL_INIT_GAMECONTROLLER,
            )
        };
        if ret < 0 {
            let err = unsafe { CStr::from_ptr(sdl::SDL_GetError()) }.to_string_lossy();
            log::error!("Failed to initialize SDL2: {}, Exiting...", err);
            std::process::exit(1);
        }

        EmuWindowSdl2 {
            input_subsystem,
            is_open: true,
            is_shown: true,
            shown_state: Arc::new(AtomicBool::new(true)),
            framebuffer_layout: Arc::new(RwLock::new(default_frame_layout(
                ScreenUndocked::WIDTH,
                ScreenUndocked::HEIGHT,
            ))),
            last_time: 0,
            system,
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

    pub fn shown_state(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.shown_state)
    }

    pub fn framebuffer_layout(&self) -> Arc<RwLock<FramebufferLayout>> {
        Arc::clone(&self.framebuffer_layout)
    }

    /// Updates the current framebuffer layout.
    ///
    /// Maps to upstream `Core::Frontend::EmuWindow::UpdateCurrentFramebufferLayout`.
    pub(crate) fn update_current_framebuffer_layout(&mut self, width: u32, height: u32) {
        *self.framebuffer_layout.write().unwrap() =
            default_frame_layout(width.max(1), height.max(1));
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
            // Maps to upstream `system.GetAndResetPerfStats()`. The optional
            // sampler owns this destructive read while a benchmark is active.
            let results = if self.system.is_null() {
                PerfStatsResults::default()
            } else if PERF_LOG_ACTIVE.load(Ordering::Relaxed) {
                perf_log_last_results()
            } else {
                self.system.get().get_and_reset_perf_stats()
            };
            let title = format!(
                "ruzu | FPS: {:.0} ({:.0}%)\0",
                results.average_game_fps,
                results.emulation_speed * 100.0
            );
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
                        self.shown_state.store(false, Ordering::Relaxed);
                        self.on_resize();
                    }
                    x if x == SDL_WINDOWEVENT_EXPOSED as u32 => {
                        self.is_shown = true;
                        self.shown_state.store(true, Ordering::Relaxed);
                        self.on_resize();
                    }
                    x if x == SDL_WINDOWEVENT_CLOSE as u32 => {
                        log::info!("SDL window close event received");
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
                log::info!("SDL quit event received");
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
    pub(crate) fn on_key_event(&mut self, key: i32, state: u8) {
        if let Some(keyboard) = self.input_subsystem.get_keyboard_mut() {
            if state == sdl::SDL_PRESSED as u8 {
                keyboard.press_key(key);
            } else if state == sdl::SDL_RELEASED as u8 {
                keyboard.release_key(key);
            }
        }
    }

    /// Converts an SDL mouse button constant to the `MouseButton` enum used by
    /// `InputCommon`.
    ///
    /// Maps to C++ `EmuWindow_SDL2::SDLButtonToMouseButton`.
    pub(crate) fn sdl_button_to_mouse_button(&self, button: u32) -> MouseButton {
        // SDL_BUTTON_LEFT=1, SDL_BUTTON_MIDDLE=2, SDL_BUTTON_RIGHT=3,
        // SDL_BUTTON_X1=4, SDL_BUTTON_X2=5
        match button {
            1 => MouseButton::Left,     // SDL_BUTTON_LEFT
            3 => MouseButton::Right,    // SDL_BUTTON_RIGHT
            2 => MouseButton::Wheel,    // SDL_BUTTON_MIDDLE
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
    pub(crate) fn on_mouse_button(&mut self, button: u32, state: u8, x: i32, y: i32) {
        let mouse_button = self.sdl_button_to_mouse_button(button);
        let touch = self.mouse_to_touch_pos(x, y);
        if let Some(mouse) = self.input_subsystem.get_mouse_mut() {
            if state == sdl::SDL_PRESSED as u8 {
                mouse.press_button(x, y, mouse_button);
                mouse.press_mouse_button(mouse_button);
                mouse.press_touch_button(touch.0, touch.1, mouse_button);
            } else {
                mouse.release_button(mouse_button);
            }
        }
    }

    /// Called when the mouse cursor moves.
    ///
    /// Maps to C++ `EmuWindow_SDL2::OnMouseMotion`.
    pub(crate) fn on_mouse_motion(&mut self, x: i32, y: i32) {
        let touch = self.mouse_to_touch_pos(x, y);
        if let Some(mouse) = self.input_subsystem.get_mouse_mut() {
            mouse.move_cursor(x, y, 0, 0);
            mouse.mouse_move(touch.0, touch.1);
            mouse.touch_move(touch.0, touch.1);
        }
    }

    /// Called when a finger starts touching the touchscreen.
    ///
    /// Maps to C++ `EmuWindow_SDL2::OnFingerDown`.
    pub(crate) fn on_finger_down(&mut self, x: f32, y: f32, id: usize) {
        if let Some(touch_screen) = self.input_subsystem.get_touch_screen_mut() {
            touch_screen.touch_pressed(x, y, id);
        }
    }

    /// Called when a finger moves on the touchscreen.
    ///
    /// Maps to C++ `EmuWindow_SDL2::OnFingerMotion`.
    pub(crate) fn on_finger_motion(&mut self, x: f32, y: f32, id: usize) {
        if let Some(touch_screen) = self.input_subsystem.get_touch_screen_mut() {
            touch_screen.touch_moved(x, y, id);
        }
    }

    /// Called when a finger lifts from the touchscreen.
    ///
    /// Maps to C++ `EmuWindow_SDL2::OnFingerUp`.
    pub(crate) fn on_finger_up(&mut self) {
        if let Some(touch_screen) = self.input_subsystem.get_touch_screen_mut() {
            touch_screen.release_all_touch();
        }
    }

    /// Called when the window is resized or restored.
    ///
    /// Maps to C++ `EmuWindow_SDL2::OnResize`.
    pub(crate) fn on_resize(&mut self) {
        // Maps to: int width, height; SDL_GL_GetDrawableSize(render_window, &width, &height);
        // then UpdateCurrentFramebufferLayout(width, height).
        if self.render_window.is_null() {
            return;
        }
        let mut width: i32 = 0;
        let mut height: i32 = 0;
        unsafe { sdl::SDL_GL_GetDrawableSize(self.render_window, &mut width, &mut height) };
        let width = width.max(1) as u32;
        let height = height.max(1) as u32;
        self.update_current_framebuffer_layout(width, height);
        log::trace!("on_resize: {}x{}", width, height);
    }

    /// Shows or hides the mouse cursor.
    ///
    /// Maps to C++ `EmuWindow_SDL2::ShowCursor`.
    pub(crate) fn show_cursor(&self, show: bool) {
        // Maps to: SDL_ShowCursor(show_cursor ? SDL_ENABLE : SDL_DISABLE)
        let toggle = if show {
            sdl::SDL_ENABLE as i32
        } else {
            sdl::SDL_DISABLE as i32
        };
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
        let ret = unsafe { sdl::SDL_SetWindowFullscreen(self.render_window, sdl_flag) };
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
        self.input_subsystem.shutdown();
        unsafe { sdl::SDL_Quit() };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hid_core::frontend::emulated_controller::get_simple_npad_button_state;

    #[test]
    fn key_events_do_not_update_the_diagnostic_npad_bridge() {
        set_simple_npad_button(NpadButton::ALL, false);

        let mut window = std::mem::ManuallyDrop::new(EmuWindowSdl2 {
            input_subsystem: InputSubsystem::new(),
            is_open: true,
            is_shown: true,
            shown_state: Arc::new(AtomicBool::new(true)),
            framebuffer_layout: Arc::new(RwLock::new(default_frame_layout(
                ScreenUndocked::WIDTH,
                ScreenUndocked::HEIGHT,
            ))),
            last_time: 0,
            system: SystemRef::null(),
            render_window: std::ptr::null_mut(),
        });

        window.on_key_event(4, sdl::SDL_PRESSED as u8);
        assert_eq!(
            get_simple_npad_button_state().raw,
            NpadButton::NONE,
            "SDL keyboard events must only use the configured keyboard engine"
        );
    }
}
