// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/window_system.h
//! Port of zuyu/src/core/hle/service/am/window_system.cpp

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ButtonPressDuration {
    ShortPressing,
    MiddlePressing,
    LongPressing,
}

/// Port of WindowSystem
///
/// Manages the foreground/background state of applets and dispatches
/// lifecycle events. Stubbed: requires full Applet, EventObserver, and
/// System integration.
pub struct WindowSystem {
    home_menu_foreground_locked: bool,
    // TODO: System reference, EventObserver, applet map, foreground roots
}

impl WindowSystem {
    pub fn new() -> Self {
        Self {
            home_menu_foreground_locked: false,
        }
    }

    pub fn update(&self) {
        // TODO: prune terminated applets, update state
        log::warn!("(STUBBED) WindowSystem::update called");
    }

    pub fn request_home_menu_to_get_foreground(&self) {
        // TODO
        log::warn!("(STUBBED) WindowSystem::request_home_menu_to_get_foreground called");
    }

    pub fn request_application_to_get_foreground(&self) {
        // TODO
        log::warn!("(STUBBED) WindowSystem::request_application_to_get_foreground called");
    }

    pub fn request_lock_home_menu_into_foreground(&mut self) {
        self.home_menu_foreground_locked = true;
        // TODO
    }

    pub fn request_unlock_home_menu_into_foreground(&mut self) {
        self.home_menu_foreground_locked = false;
        // TODO
    }

    pub fn on_operation_mode_changed(&self) {
        // TODO
        log::warn!("(STUBBED) WindowSystem::on_operation_mode_changed called");
    }

    pub fn on_exit_requested(&self) {
        // TODO
        log::warn!("(STUBBED) WindowSystem::on_exit_requested called");
    }

    pub fn on_home_button_pressed(&self, _duration: ButtonPressDuration) {
        // TODO
        log::warn!("(STUBBED) WindowSystem::on_home_button_pressed called");
    }

    pub fn on_capture_button_pressed(&self, _duration: ButtonPressDuration) {
        // No-op, matching upstream
    }

    pub fn on_power_button_pressed(&self, _duration: ButtonPressDuration) {
        // No-op, matching upstream
    }
}
