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
}

/// SDL2 window manager for the emulator.
pub struct EmulatorWindow {
    pub sdl_context: Sdl,
    pub window: Window,
    pub event_pump: EventPump,
    /// Whether the window was created with Vulkan support.
    pub vulkan_enabled: bool,
}

impl EmulatorWindow {
    /// Create a new SDL2 window with Vulkan support (fallback to software rendering).
    pub fn new(title: &str, width: u32, height: u32) -> Result<Self> {
        let sdl_context = sdl2::init().map_err(|e| anyhow::anyhow!("SDL2 init failed: {}", e))?;

        let video_subsystem = sdl_context
            .video()
            .map_err(|e| anyhow::anyhow!("SDL2 video init failed: {}", e))?;

        // Try creating a Vulkan-enabled window first; fall back to software rendering.
        let (window, vulkan_enabled) = match video_subsystem
            .window(title, width, height)
            .position_centered()
            .resizable()
            .vulkan()
            .build()
        {
            Ok(w) => {
                info!("Created Vulkan-enabled SDL2 window: {}x{}", width, height);
                (w, true)
            }
            Err(e) => {
                info!(
                    "Vulkan window not available ({}), falling back to software rendering",
                    e
                );
                let w = video_subsystem
                    .window(title, width, height)
                    .position_centered()
                    .resizable()
                    .build()
                    .context("Failed to create SDL2 window")?;
                (w, false)
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
        })
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

        Some(InputState {
            buttons: btns,
            l_stick: (lx, ly),
            r_stick: (0, 0),
            resized,
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
