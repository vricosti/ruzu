// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use anyhow::{Context, Result};
use log::info;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::video::Window;
use sdl2::EventPump;
use sdl2::Sdl;

/// Default window dimensions (720p, matching Switch handheld mode).
pub const DEFAULT_WIDTH: u32 = 1280;
pub const DEFAULT_HEIGHT: u32 = 720;

/// SDL2 window manager for the emulator.
pub struct EmulatorWindow {
    pub sdl_context: Sdl,
    pub window: Window,
    pub event_pump: EventPump,
}

impl EmulatorWindow {
    /// Create a new SDL2 window with Vulkan support.
    pub fn new(title: &str, width: u32, height: u32) -> Result<Self> {
        let sdl_context = sdl2::init().map_err(|e| anyhow::anyhow!("SDL2 init failed: {}", e))?;

        let video_subsystem = sdl_context
            .video()
            .map_err(|e| anyhow::anyhow!("SDL2 video init failed: {}", e))?;

        let window = video_subsystem
            .window(title, width, height)
            .vulkan()
            .position_centered()
            .resizable()
            .build()
            .context("Failed to create SDL2 window")?;

        let event_pump = sdl_context
            .event_pump()
            .map_err(|e| anyhow::anyhow!("SDL2 event pump failed: {}", e))?;

        info!(
            "Created SDL2 window: {}x{} with Vulkan support",
            width, height
        );

        Ok(Self {
            sdl_context,
            window,
            event_pump,
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

    /// Get the current window size.
    pub fn size(&self) -> (u32, u32) {
        self.window.size()
    }

    /// Get the Vulkan instance extensions required by SDL2.
    pub fn vulkan_instance_extensions(&self) -> Result<Vec<String>> {
        self.window
            .vulkan_instance_extensions()
            .map(|exts| exts.into_iter().map(String::from).collect())
            .map_err(|e| anyhow::anyhow!("Failed to get Vulkan extensions: {}", e))
    }
}
