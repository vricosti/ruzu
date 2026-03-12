// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_turbo_mode.h` / `vk_turbo_mode.cpp`.
//!
//! Keeps the GPU clocked at maximum frequency by submitting periodic
//! no-op work to prevent idle clock-down.

use std::sync::{Condvar, Mutex};
use std::time::Instant;

use ash::vk;

// ---------------------------------------------------------------------------
// TurboMode
// ---------------------------------------------------------------------------

/// Port of `TurboMode` class.
///
/// On non-Android platforms, creates a secondary Vulkan device and
/// periodically submits trivial compute dispatches to prevent the GPU
/// from reducing its clock speed.
pub struct TurboMode {
    _submission_lock: Mutex<()>,
    _submission_cv: Condvar,
    _submission_time: Mutex<Option<Instant>>,
}

impl TurboMode {
    /// Port of `TurboMode::TurboMode`.
    pub fn new(_instance: vk::Instance) -> Self {
        todo!("TurboMode::new")
    }

    /// Port of `TurboMode::QueueSubmitted`.
    ///
    /// Called after each GPU queue submission to reset the idle timer.
    pub fn queue_submitted(&self) {
        todo!("TurboMode::queue_submitted")
    }

    // --- Private ---

    fn run(&self) {
        todo!("TurboMode::run")
    }
}
