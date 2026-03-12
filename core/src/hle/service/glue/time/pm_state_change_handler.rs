// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/pm_state_change_handler.h
//! Port of zuyu/src/core/hle/service/glue/time/pm_state_change_handler.cpp
//!
//! PmStateChangeHandler: handles power management state changes for time.

/// PmStateChangeHandler tracks power state transitions for the alarm worker.
///
/// Corresponds to `PmStateChangeHandler` in upstream pm_state_change_handler.h.
pub struct PmStateChangeHandler {
    /// Priority of the current PM state change. Used by TimeWorker to decide
    /// which events to wait on.
    ///
    /// Corresponds to `m_priority` in upstream.
    pub priority: i32,
    // NOTE: upstream holds a reference to AlarmWorker, but in Rust we manage
    // ownership through the TimeWorker which owns both.
}

impl PmStateChangeHandler {
    /// Create a new PmStateChangeHandler.
    ///
    /// Corresponds to `PmStateChangeHandler::PmStateChangeHandler(AlarmWorker&)` in upstream.
    /// The AlarmWorker reference is managed externally (by TimeWorker).
    pub fn new() -> Self {
        // TODO: Initialize IPmModule, dependent on Rtc and Fs
        Self { priority: 0 }
    }
}
