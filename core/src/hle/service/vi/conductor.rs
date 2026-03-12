// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/conductor.h
//! Port of zuyu/src/core/hle/service/vi/conductor.cpp
//!
//! The Conductor manages vsync timing and display composition.
//! It maintains a VsyncManager per display and drives the composition loop.

use std::collections::HashMap;

use super::vsync_manager::VsyncManager;

/// Frame interval in nanoseconds (60 FPS).
pub const FRAME_NS: i64 = 1_000_000_000 / 60;

/// Conductor drives display composition and vsync signaling.
///
/// In upstream C++, the Conductor:
/// - Creates a CoreTiming event for periodic composition
/// - Optionally runs a dedicated vsync thread in multicore mode
/// - Manages VsyncManager instances per display
///
/// Full implementation depends on Core::Timing and threading infrastructure.
pub struct Conductor {
    vsync_managers: HashMap<u64, VsyncManager>,
    swap_interval: i32,
    compose_speed_scale: f32,
}

impl Conductor {
    pub fn new(display_ids: &[u64]) -> Self {
        let mut vsync_managers = HashMap::new();
        for &id in display_ids {
            vsync_managers.insert(id, VsyncManager::new());
        }

        Self {
            vsync_managers,
            swap_interval: 1,
            compose_speed_scale: 1.0,
        }
    }

    pub fn link_vsync_event(&mut self, display_id: u64, event_handle: u64) {
        if let Some(manager) = self.vsync_managers.get_mut(&display_id) {
            manager.link_vsync_event(event_handle);
        }
    }

    pub fn unlink_vsync_event(&mut self, display_id: u64, event_handle: u64) {
        if let Some(manager) = self.vsync_managers.get_mut(&display_id) {
            manager.unlink_vsync_event(event_handle);
        }
    }

    /// Process a vsync tick: compose each display and signal vsync events.
    pub fn process_vsync<F>(&mut self, mut compose_fn: F)
    where
        F: FnMut(&mut i32, &mut f32, u64) -> bool,
    {
        for (&display_id, manager) in self.vsync_managers.iter() {
            compose_fn(&mut self.swap_interval, &mut self.compose_speed_scale, display_id);
            manager.signal_vsync();
        }
    }

    /// Calculate the next tick interval in nanoseconds.
    ///
    /// In upstream this reads from Settings to determine speed scaling.
    /// This simplified version uses the swap interval and compose speed scale.
    pub fn get_next_ticks(&self) -> i64 {
        let speed_scale = 1.0f32 / self.compose_speed_scale;
        let effective_fps = 60.0f32 / self.swap_interval as f32;
        (speed_scale * (1_000_000_000.0f32 / effective_fps)) as i64
    }
}
