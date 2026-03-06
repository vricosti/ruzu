// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use anyhow::{Context, Result};
use log::info;
use sdl2::event::Event;
use sdl2::keyboard::{Keycode, Scancode};
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use sdl2::video::Window;
use sdl2::EventPump;
use sdl2::Sdl;

use ruzu_service::hid_shared_memory::buttons;

/// Default window dimensions (720p, matching Switch handheld mode).
pub const DEFAULT_WIDTH: u32 = 1280;
pub const DEFAULT_HEIGHT: u32 = 720;

/// Analog stick range (Switch uses ~±32767, we scale keyboard to a reasonable value).
const STICK_MAX: i32 = 30000;

/// Current input state from SDL2 keyboard polling.
pub struct InputState {
    pub buttons: u64,
    pub l_stick: (i32, i32),
    pub r_stick: (i32, i32),
    /// If the window was resized this frame, contains (width, height).
    pub resized: Option<(u32, u32)>,
    /// Touch point from mouse click, scaled to Switch touch screen coords (1280x720).
    /// `None` if mouse button is not pressed.
    pub touch: Option<(u32, u32)>,
}

/// Rendering backend the window was created for.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WindowBackend {
    /// Window created with Vulkan support.
    Vulkan,
    /// Window created with OpenGL support.
    OpenGL,
    /// Plain window (software rendering only).
    Software,
}

/// SDL2 window manager for the emulator.
pub struct EmulatorWindow {
    #[allow(dead_code)]
    pub sdl_context: Sdl,
    pub window: Window,
    pub event_pump: EventPump,
    /// Whether the window was created with Vulkan support.
    #[allow(dead_code)]
    pub vulkan_enabled: bool,
    /// Whether the window was created with OpenGL support.
    #[allow(dead_code)]
    pub opengl_enabled: bool,
    /// Active rendering backend for this window.
    pub backend: WindowBackend,
}

impl EmulatorWindow {
    /// Create a new SDL2 window with the specified rendering backend.
    ///
    /// `preferred_backend` determines what kind of window to create:
    /// - `Vulkan` — try Vulkan first, fall back to OpenGL, then software
    /// - `OpenGL` — try OpenGL first, fall back to software
    /// - `Software` — plain window, no GPU API
    pub fn new(
        title: &str,
        width: u32,
        height: u32,
        preferred_backend: WindowBackend,
    ) -> Result<Self> {
        let sdl_context = sdl2::init().map_err(|e| anyhow::anyhow!("SDL2 init failed: {}", e))?;

        let video_subsystem = sdl_context
            .video()
            .map_err(|e| anyhow::anyhow!("SDL2 video init failed: {}", e))?;

        let (window, vulkan_enabled, opengl_enabled, backend) = match preferred_backend {
            WindowBackend::Vulkan => {
                // Try Vulkan first
                match video_subsystem
                    .window(title, width, height)
                    .position_centered()
                    .resizable()
                    .vulkan()
                    .build()
                {
                    Ok(w) => {
                        info!("Created Vulkan-enabled SDL2 window: {}x{}", width, height);
                        (w, true, false, WindowBackend::Vulkan)
                    }
                    Err(e) => {
                        info!(
                            "Vulkan window not available ({}), trying OpenGL",
                            e
                        );
                        // Fall back to OpenGL
                        Self::try_opengl_or_software(
                            &video_subsystem,
                            title,
                            width,
                            height,
                        )?
                    }
                }
            }
            WindowBackend::OpenGL => {
                Self::try_opengl_or_software(&video_subsystem, title, width, height)?
            }
            WindowBackend::Software => {
                let w = video_subsystem
                    .window(title, width, height)
                    .position_centered()
                    .resizable()
                    .build()
                    .context("Failed to create SDL2 window")?;
                info!("Created software-only SDL2 window: {}x{}", width, height);
                (w, false, false, WindowBackend::Software)
            }
        };

        let event_pump = sdl_context
            .event_pump()
            .map_err(|e| anyhow::anyhow!("SDL2 event pump failed: {}", e))?;

        Ok(Self {
            sdl_context,
            window,
            event_pump,
            vulkan_enabled,
            opengl_enabled,
            backend,
        })
    }

    /// Try to create an OpenGL window, falling back to software.
    fn try_opengl_or_software(
        video: &sdl2::VideoSubsystem,
        title: &str,
        width: u32,
        height: u32,
    ) -> Result<(Window, bool, bool, WindowBackend)> {
        // Set GL attributes (matching zuyu's EmuWindow_SDL2_GL)
        let gl_attr = video.gl_attr();
        gl_attr.set_context_major_version(4);
        gl_attr.set_context_minor_version(6);
        gl_attr.set_context_profile(sdl2::video::GLProfile::Compatibility);
        gl_attr.set_double_buffer(true);
        gl_attr.set_red_size(8);
        gl_attr.set_green_size(8);
        gl_attr.set_blue_size(8);
        gl_attr.set_alpha_size(0);
        gl_attr.set_share_with_current_context(true);

        match video
            .window(title, width, height)
            .position_centered()
            .resizable()
            .opengl()
            .build()
        {
            Ok(w) => {
                info!("Created OpenGL-enabled SDL2 window: {}x{}", width, height);
                Ok((w, false, true, WindowBackend::OpenGL))
            }
            Err(e) => {
                info!(
                    "OpenGL window not available ({}), falling back to software rendering",
                    e
                );
                let w = video
                    .window(title, width, height)
                    .position_centered()
                    .resizable()
                    .build()
                    .context("Failed to create SDL2 window")?;
                Ok((w, false, false, WindowBackend::Software))
            }
        }
    }

    /// Poll SDL2 events. Returns false if the window should close.
    pub fn poll_events(&mut self) -> bool {
        for event in self.event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => return false,
                Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => return false,
                Event::Window {
                    win_event: sdl2::event::WindowEvent::Resized(w, h),
                    ..
                } => {
                    info!("Window resized: {}x{}", w, h);
                }
                _ => {}
            }
        }
        true
    }

    /// Poll SDL2 events and sample keyboard input state.
    /// Returns `None` if the window should close, or `Some(InputState)`.
    pub fn poll_events_with_input(&mut self) -> Option<InputState> {
        let mut resized = None;

        // Drain events first (handles quit/resize).
        for event in self.event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => return None,
                Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => return None,
                Event::Window {
                    win_event: sdl2::event::WindowEvent::Resized(w, h),
                    ..
                } => {
                    info!("Window resized: {}x{}", w, h);
                    resized = Some((w as u32, h as u32));
                }
                _ => {}
            }
        }

        // Sample keyboard state (polling-based, not event-based).
        let kb = self.event_pump.keyboard_state();
        let mut btns: u64 = 0;

        // Button mapping (using scancodes for layout-independence).
        if kb.is_scancode_pressed(Scancode::Z) {
            btns |= buttons::A;
        }
        if kb.is_scancode_pressed(Scancode::X) {
            btns |= buttons::B;
        }
        if kb.is_scancode_pressed(Scancode::C) {
            btns |= buttons::X;
        }
        if kb.is_scancode_pressed(Scancode::V) {
            btns |= buttons::Y;
        }
        if kb.is_scancode_pressed(Scancode::Return) {
            btns |= buttons::PLUS;
        }
        if kb.is_scancode_pressed(Scancode::RShift) {
            btns |= buttons::MINUS;
        }
        if kb.is_scancode_pressed(Scancode::Q) {
            btns |= buttons::L;
        }
        if kb.is_scancode_pressed(Scancode::E) {
            btns |= buttons::R;
        }
        if kb.is_scancode_pressed(Scancode::A) {
            btns |= buttons::ZL;
        }
        if kb.is_scancode_pressed(Scancode::D) {
            btns |= buttons::ZR;
        }

        // D-Pad (arrow keys).
        if kb.is_scancode_pressed(Scancode::Left) {
            btns |= buttons::DPAD_LEFT;
        }
        if kb.is_scancode_pressed(Scancode::Up) {
            btns |= buttons::DPAD_UP;
        }
        if kb.is_scancode_pressed(Scancode::Right) {
            btns |= buttons::DPAD_RIGHT;
        }
        if kb.is_scancode_pressed(Scancode::Down) {
            btns |= buttons::DPAD_DOWN;
        }

        // Left analog stick (IJKL keys).
        let mut lx: i32 = 0;
        let mut ly: i32 = 0;
        if kb.is_scancode_pressed(Scancode::J) {
            lx -= STICK_MAX;
        }
        if kb.is_scancode_pressed(Scancode::L) {
            lx += STICK_MAX;
        }
        if kb.is_scancode_pressed(Scancode::I) {
            ly += STICK_MAX;
        }
        if kb.is_scancode_pressed(Scancode::K) {
            ly -= STICK_MAX;
        }

        // Sample mouse state for touch input.
        let mouse = self.event_pump.mouse_state();
        let touch = if mouse.left() {
            // Scale mouse position from window coordinates to Switch touch screen (1280x720).
            let (win_w, win_h) = self.window.size();
            let tx = if win_w > 0 {
                (mouse.x() as u32).min(win_w - 1) * DEFAULT_WIDTH / win_w
            } else {
                0
            };
            let ty = if win_h > 0 {
                (mouse.y() as u32).min(win_h - 1) * DEFAULT_HEIGHT / win_h
            } else {
                0
            };
            Some((tx, ty))
        } else {
            None
        };

        Some(InputState {
            buttons: btns,
            l_stick: (lx, ly),
            r_stick: (0, 0),
            resized,
            touch,
        })
    }

    /// Present an RGBA8888 framebuffer to the window via software blit.
    pub fn present_framebuffer(&mut self, pixels: &[u8], width: u32, height: u32) {
        let expected_size = (width * height * 4) as usize;
        if pixels.len() < expected_size {
            return;
        }

        let pitch = width * 4;

        // Create a surface from the pixel data and blit to the window surface.
        let surface = sdl2::surface::Surface::from_data(
            // SAFETY: The pixel data lives for the duration of this function call.
            // We need a mutable reference for SDL2 but won't actually modify the data.
            unsafe {
                std::slice::from_raw_parts_mut(pixels.as_ptr() as *mut u8, pixels.len())
            },
            width,
            height,
            pitch,
            PixelFormatEnum::ABGR8888,
        );

        let surface = match surface {
            Ok(s) => s,
            Err(e) => {
                log::warn!("Failed to create surface: {}", e);
                return;
            }
        };

        let mut window_surface = match self.window.surface(&self.event_pump) {
            Ok(s) => s,
            Err(e) => {
                log::warn!("Failed to get window surface: {}", e);
                return;
            }
        };

        let (win_w, win_h) = self.window.size();
        let dst_rect = Rect::new(0, 0, win_w, win_h);

        let _ = surface.blit_scaled(None, &mut window_surface, Some(dst_rect));
        let _ = window_surface.finish();
    }

    /// Get the current window size.
    pub fn size(&self) -> (u32, u32) {
        self.window.size()
    }
}
