// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/shader_notify.h and video_core/shader_notify.cpp
//!
//! Shader compilation notification system for reporting build progress to the UI.

use std::sync::atomic::{AtomicI32, Ordering};
use std::time::{Duration, Instant};

/// Duration after which we stop reporting "shaders building".
const TIME_TO_STOP_REPORTING: Duration = Duration::from_secs(2);

/// Tracks shader compilation progress.
pub struct ShaderNotify {
    num_building: AtomicI32,
    num_complete: AtomicI32,
    report_base: i32,
    completed: bool,
    num_when_completed: i32,
    complete_time: Option<Instant>,
}

impl ShaderNotify {
    pub fn new() -> Self {
        Self {
            num_building: AtomicI32::new(0),
            num_complete: AtomicI32::new(0),
            report_base: 0,
            completed: false,
            num_when_completed: 0,
            complete_time: None,
        }
    }

    /// Returns the number of shaders currently being built (relative to report base).
    ///
    /// After all shaders complete and a timeout passes, the report resets to zero.
    pub fn shaders_building(&mut self) -> i32 {
        let now_complete = self.num_complete.load(Ordering::Relaxed);
        let now_building = self.num_building.load(Ordering::Relaxed);

        if now_complete == now_building {
            let now = Instant::now();
            if self.completed && now_complete == self.num_when_completed {
                if let Some(complete_time) = self.complete_time {
                    if now.duration_since(complete_time) > TIME_TO_STOP_REPORTING {
                        self.report_base = now_complete;
                        self.completed = false;
                    }
                }
            } else {
                self.completed = true;
                self.num_when_completed = now_complete;
                self.complete_time = Some(now);
            }
        }

        now_building - self.report_base
    }

    /// Mark a shader as completed.
    pub fn mark_shader_complete(&self) {
        self.num_complete.fetch_add(1, Ordering::Relaxed);
    }

    /// Mark a shader as building.
    pub fn mark_shader_building(&self) {
        self.num_building.fetch_add(1, Ordering::Relaxed);
    }
}

impl Default for ShaderNotify {
    fn default() -> Self {
        Self::new()
    }
}
