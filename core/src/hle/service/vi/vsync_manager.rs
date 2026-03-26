// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/vsync_manager.h
//! Port of zuyu/src/core/hle/service/vi/vsync_manager.cpp

use std::sync::Arc;

use crate::hle::service::os::event::Event;

/// VsyncManager tracks a set of vsync events and signals them on vsync.
///
/// Upstream stores `std::set<Event*>` and calls `event->Signal()` on each.
/// We store `Arc<Event>` and call `event.signal()`.
pub struct VsyncManager {
    vsync_events: Vec<Arc<Event>>,
}

impl VsyncManager {
    pub fn new() -> Self {
        Self {
            vsync_events: Vec::new(),
        }
    }

    /// Signal all registered vsync events.
    /// Port of upstream `VsyncManager::SignalVsync`.
    pub fn signal_vsync(&self) {
        for event in &self.vsync_events {
            event.signal();
        }
    }

    /// Register a vsync event.
    /// Port of upstream `VsyncManager::LinkVsyncEvent`.
    pub fn link_vsync_event(&mut self, event: Arc<Event>) {
        self.vsync_events.push(event);
    }

    /// Unregister a vsync event.
    /// Port of upstream `VsyncManager::UnlinkVsyncEvent`.
    pub fn unlink_vsync_event(&mut self, event: &Arc<Event>) {
        self.vsync_events.retain(|e| !Arc::ptr_eq(e, event));
    }
}

impl Default for VsyncManager {
    fn default() -> Self {
        Self::new()
    }
}
