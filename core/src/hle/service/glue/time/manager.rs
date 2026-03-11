// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/manager.h
//! Port of zuyu/src/core/hle/service/glue/time/manager.cpp
//!
//! TimeManager: orchestrates time service initialization.
//! Status: Stub

/// TimeManager corresponds to upstream `Time::TimeManager`.
pub struct TimeManager {
    // TODO: workers, alarm_worker, file_timestamp_worker, pm_state_change_handler
}

impl TimeManager {
    pub fn new() -> Self {
        log::debug!("TimeManager::new called");
        Self {}
    }
}
