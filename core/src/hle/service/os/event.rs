// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/event.h
//! Port of zuyu/src/core/hle/service/os/event.cpp
//!
//! Event wrapper for kernel KEvent.

use std::sync::{Arc, Condvar, Mutex};

/// Event — wraps a kernel event for service use.
///
/// Upstream stores a `KEvent*` and calls `m_event->Signal()` / `m_event->Clear()`.
/// Since HLE services don't always have a full KEvent wired, we use a
/// Condvar-based signaling mechanism that provides the same behavioral contract:
/// - Signal: wakes all waiters
/// - Clear: resets the signaled state
/// - Wait: blocks until signaled (not exposed here, done via kernel wait)
pub struct Event {
    signaled: Arc<Mutex<bool>>,
    cv: Arc<Condvar>,
}

impl Event {
    pub fn new() -> Self {
        Self {
            signaled: Arc::new(Mutex::new(false)),
            cv: Arc::new(Condvar::new()),
        }
    }

    /// Signal the event. Wakes all waiters.
    /// Port of upstream `Event::Signal()` → `m_event->Signal()`.
    pub fn signal(&self) {
        let mut signaled = self.signaled.lock().unwrap();
        *signaled = true;
        self.cv.notify_all();
    }

    /// Clear the event (reset signaled state).
    /// Port of upstream `Event::Clear()` → `m_event->Clear()`.
    pub fn clear(&self) {
        let mut signaled = self.signaled.lock().unwrap();
        *signaled = false;
    }

    /// Check if the event is currently signaled.
    pub fn is_signaled(&self) -> bool {
        *self.signaled.lock().unwrap()
    }

    /// Wait for the event to be signaled.
    pub fn wait(&self) {
        let guard = self.signaled.lock().unwrap();
        let _guard = self.cv.wait_while(guard, |s| !*s).unwrap();
    }

    /// Wait for the event with a timeout. Returns true if signaled, false if timed out.
    pub fn wait_timeout(&self, timeout: std::time::Duration) -> bool {
        let guard = self.signaled.lock().unwrap();
        let (guard, result) = self.cv.wait_timeout_while(guard, timeout, |s| !*s).unwrap();
        *guard
    }
}

impl Default for Event {
    fn default() -> Self {
        Self::new()
    }
}
