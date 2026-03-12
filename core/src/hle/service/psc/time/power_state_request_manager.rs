// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/power_state_request_manager.h/.cpp
//!
//! PowerStateRequestManager: manages power state change requests with priority tracking.

use std::sync::Mutex;

/// PowerStateRequestManager tracks pending and available power state requests.
///
/// Upstream uses a KEvent for signaling; here we track the state and will wire
/// the event once the kernel layer is integrated.
pub struct PowerStateRequestManager {
    inner: Mutex<PowerStateRequestManagerInner>,
    // TODO: Kernel::KEvent* m_event when wired to kernel
}

struct PowerStateRequestManagerInner {
    has_pending_request: bool,
    pending_request_priority: u32,
    has_available_request: bool,
    available_request_priority: u32,
}

impl PowerStateRequestManager {
    pub fn new() -> Self {
        Self {
            inner: Mutex::new(PowerStateRequestManagerInner {
                has_pending_request: false,
                pending_request_priority: 0,
                has_available_request: false,
                available_request_priority: 0,
            }),
        }
    }

    /// Update the pending request priority.
    /// If a request is already pending, takes the max of old and new priorities.
    pub fn update_pending_power_state_request_priority(&self, priority: u32) {
        let mut inner = self.inner.lock().unwrap();
        if inner.has_pending_request {
            inner.pending_request_priority = inner.pending_request_priority.max(priority);
        } else {
            inner.pending_request_priority = priority;
            inner.has_pending_request = true;
        }
    }

    /// Signal that a power state request is available.
    /// Moves the pending request to available and signals the event.
    pub fn signal_power_state_request_availability(&self) {
        let mut inner = self.inner.lock().unwrap();
        if inner.has_pending_request {
            if !inner.has_available_request {
                inner.has_available_request = true;
            }
            inner.has_pending_request = false;
            inner.available_request_priority = inner.pending_request_priority;
            // TODO: m_event->Signal()
        }
    }

    /// Get and clear the current power state request.
    /// Returns (had_request, priority).
    pub fn get_and_clear_power_state_request(&self) -> (bool, u32) {
        let mut inner = self.inner.lock().unwrap();
        let had_request = inner.has_available_request;
        let mut priority = 0;
        if inner.has_available_request {
            priority = inner.available_request_priority;
            inner.has_available_request = false;
            // TODO: m_event->Clear()
        }
        (had_request, priority)
    }
}
