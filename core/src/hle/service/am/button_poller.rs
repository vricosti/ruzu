// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/button_poller.h
//! Port of zuyu/src/core/hle/service/am/button_poller.cpp

use std::time::Instant;

use super::window_system::ButtonPressDuration;

/// Port of ButtonPoller
///
/// Polls HID button state and translates press durations into
/// ButtonPressDuration events for the WindowSystem.
/// Stubbed: requires HID core integration.
pub struct ButtonPoller {
    _home_button_press_start: Option<Instant>,
    _capture_button_press_start: Option<Instant>,
    _power_button_press_start: Option<Instant>,
}

impl ButtonPoller {
    pub fn new() -> Self {
        Self {
            _home_button_press_start: None,
            _capture_button_press_start: None,
            _power_button_press_start: None,
        }
    }
}

/// Classify how long a button was held.
pub fn classify_press_duration(start: Instant) -> ButtonPressDuration {
    let dur = start.elapsed();
    if dur.as_millis() < 500 {
        ButtonPressDuration::ShortPressing
    } else if dur.as_millis() < 1000 {
        ButtonPressDuration::MiddlePressing
    } else {
        ButtonPressDuration::LongPressing
    }
}
