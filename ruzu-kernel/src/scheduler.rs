// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Simple single-core scheduler for Phase 1.
//! Just runs the first runnable thread. No preemption.

use crate::thread::ThreadState;
use ruzu_common::Handle;

/// Simple round-robin scheduler for single-core Phase 1.
pub struct Scheduler {
    /// Handle of the currently running thread.
    pub current_thread: Option<Handle>,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            current_thread: None,
        }
    }

    /// Select the next runnable thread handle from a list of thread states.
    pub fn schedule(&mut self, threads: &[(Handle, ThreadState)]) -> Option<Handle> {
        // Simple: pick the first runnable thread
        let next = threads
            .iter()
            .find(|(_, state)| *state == ThreadState::Runnable)
            .map(|(handle, _)| *handle);

        self.current_thread = next;
        next
    }
}

impl Default for Scheduler {
    fn default() -> Self {
        Self::new()
    }
}
