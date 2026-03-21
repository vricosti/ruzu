// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/power_state_request_manager.h/.cpp
//!
//! PowerStateRequestManager: manages power state change requests with priority
//! tracking. Uses a kernel event for signaling availability.

use std::sync::{Arc, Mutex};

use crate::hle::service::os::event::Event;

/// PowerStateRequestManager tracks pending and available power state requests.
///
/// Corresponds to `PSC::Time::PowerStateRequestManager` in upstream.
///
/// State machine:
///   1. `UpdatePendingPowerStateRequestPriority(priority)` — accumulates the
///      max priority among pending requests.
///   2. `SignalPowerStateRequestAvailability()` — moves pending → available and
///      signals the kernel event.
///   3. `GetAndClearPowerStateRequest()` — consumes the available request and
///      clears the kernel event.
pub struct PowerStateRequestManager {
    inner: Mutex<PowerStateRequestManagerInner>,
    /// Kernel event for signaling power state request availability.
    /// Upstream: `Kernel::KEvent* m_event`.
    event: Arc<Event>,
}

struct PowerStateRequestManagerInner {
    has_pending_request: bool,
    pending_request_priority: u32,
    has_available_request: bool,
    available_request_priority: u32,
}

impl PowerStateRequestManager {
    /// Create a new PowerStateRequestManager.
    ///
    /// Corresponds to upstream constructor which creates a KEvent named
    /// "Psc:PowerStateRequestManager:Event".
    pub fn new() -> Self {
        Self {
            inner: Mutex::new(PowerStateRequestManagerInner {
                has_pending_request: false,
                pending_request_priority: 0,
                has_available_request: false,
                available_request_priority: 0,
            }),
            event: Arc::new(Event::new()),
        }
    }

    /// Update the pending request priority.
    ///
    /// If a request is already pending, takes the max of old and new priorities.
    /// Corresponds to `PowerStateRequestManager::UpdatePendingPowerStateRequestPriority`
    /// in upstream.
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
    ///
    /// Moves the pending request to available and signals the event.
    /// Corresponds to `PowerStateRequestManager::SignalPowerStateRequestAvailability`
    /// in upstream.
    pub fn signal_power_state_request_availability(&self) {
        let mut inner = self.inner.lock().unwrap();
        if inner.has_pending_request {
            if !inner.has_available_request {
                inner.has_available_request = true;
            }
            inner.has_pending_request = false;
            inner.available_request_priority = inner.pending_request_priority;
            self.event.signal();
        }
    }

    /// Get and clear the current power state request.
    ///
    /// Returns `(had_request, priority)`. If there was an available request,
    /// clears the event.
    /// Corresponds to `PowerStateRequestManager::GetAndClearPowerStateRequest`
    /// in upstream.
    pub fn get_and_clear_power_state_request(&self) -> (bool, u32) {
        let mut inner = self.inner.lock().unwrap();
        let had_request = inner.has_available_request;
        let mut priority = 0;
        if inner.has_available_request {
            priority = inner.available_request_priority;
            inner.has_available_request = false;
            self.event.clear();
        }
        (had_request, priority)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_request_initially() {
        let manager = PowerStateRequestManager::new();
        let (had, _priority) = manager.get_and_clear_power_state_request();
        assert!(!had);
    }

    #[test]
    fn pending_becomes_available_after_signal() {
        let manager = PowerStateRequestManager::new();
        manager.update_pending_power_state_request_priority(5);

        // Not available yet — only pending
        let (had, _) = manager.get_and_clear_power_state_request();
        assert!(!had);

        // Signal availability
        manager.signal_power_state_request_availability();

        let (had, priority) = manager.get_and_clear_power_state_request();
        assert!(had);
        assert_eq!(priority, 5);

        // Cleared now
        let (had, _) = manager.get_and_clear_power_state_request();
        assert!(!had);
    }

    #[test]
    fn max_priority_accumulation() {
        let manager = PowerStateRequestManager::new();
        manager.update_pending_power_state_request_priority(3);
        manager.update_pending_power_state_request_priority(7);
        manager.update_pending_power_state_request_priority(1);
        manager.signal_power_state_request_availability();

        let (had, priority) = manager.get_and_clear_power_state_request();
        assert!(had);
        assert_eq!(priority, 7);
    }

    #[test]
    fn signal_without_pending_is_noop() {
        let manager = PowerStateRequestManager::new();
        manager.signal_power_state_request_availability();
        let (had, _) = manager.get_and_clear_power_state_request();
        assert!(!had);
    }
}
