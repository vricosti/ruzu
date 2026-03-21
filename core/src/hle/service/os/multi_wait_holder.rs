// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/multi_wait_holder.h
//! Port of zuyu/src/core/hle/service/os/multi_wait_holder.cpp
//!
//! MultiWaitHolder — holds a synchronization object for MultiWait.

use super::event::Event;
use std::sync::Arc;

/// MultiWaitHolder — holds a native synchronization object handle.
///
/// Upstream stores a native kernel handle and linkage into the parent
/// MultiWait's intrusive list. We store an optional Event reference
/// and a linked flag.
pub struct MultiWaitHolder {
    user_data: usize,
    /// The event this holder is waiting on (if any).
    event: Option<Arc<Event>>,
    /// Whether this holder is currently linked to a MultiWait.
    is_linked: bool,
}

impl MultiWaitHolder {
    pub fn new() -> Self {
        Self {
            user_data: 0,
            event: None,
            is_linked: false,
        }
    }

    /// Create a holder wrapping an Event.
    pub fn from_event(event: Arc<Event>) -> Self {
        Self {
            user_data: 0,
            event: Some(event),
            is_linked: false,
        }
    }

    /// Set user data associated with this holder.
    pub fn set_user_data(&mut self, data: usize) {
        self.user_data = data;
    }

    /// Get user data associated with this holder.
    pub fn get_user_data(&self) -> usize {
        self.user_data
    }

    /// Check if the held object is signaled.
    pub fn is_signaled(&self) -> bool {
        if let Some(ref event) = self.event {
            event.is_signaled()
        } else {
            false
        }
    }

    /// Link this holder to a MultiWait.
    pub fn link_to_multi_wait(&mut self) {
        self.is_linked = true;
    }

    /// Unlink this holder from its MultiWait.
    pub fn unlink_from_multi_wait(&mut self) {
        self.is_linked = false;
    }

    /// Check if currently linked.
    pub fn is_linked(&self) -> bool {
        self.is_linked
    }
}

impl Default for MultiWaitHolder {
    fn default() -> Self {
        Self::new()
    }
}
