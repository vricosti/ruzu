// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/vsync_manager.h
//! Port of zuyu/src/core/hle/service/vi/vsync_manager.cpp

use std::collections::BTreeSet;

/// VsyncManager tracks a set of vsync event handles and signals them on vsync.
///
/// In upstream C++, the Event pointers are raw pointers to Service::Event objects.
/// Here we use u64 handles as identifiers for the events, since the actual
/// event signaling depends on the kernel event infrastructure.
pub struct VsyncManager {
    /// Set of registered vsync event handles.
    vsync_events: BTreeSet<u64>,
}

impl VsyncManager {
    pub fn new() -> Self {
        Self {
            vsync_events: BTreeSet::new(),
        }
    }

    /// Signal all registered vsync events.
    ///
    /// In upstream, this calls event->Signal() on each event.
    /// The actual signaling depends on the kernel event infrastructure.
    pub fn signal_vsync(&self) {
        for _event_handle in &self.vsync_events {
            // In a full port, this would signal the kernel event:
            // event.signal();
            log::trace!("VsyncManager: signal_vsync event={}", _event_handle);
        }
    }

    /// Register a vsync event handle.
    pub fn link_vsync_event(&mut self, event_handle: u64) {
        self.vsync_events.insert(event_handle);
    }

    /// Unregister a vsync event handle.
    pub fn unlink_vsync_event(&mut self, event_handle: u64) {
        self.vsync_events.remove(&event_handle);
    }
}

impl Default for VsyncManager {
    fn default() -> Self {
        Self::new()
    }
}
