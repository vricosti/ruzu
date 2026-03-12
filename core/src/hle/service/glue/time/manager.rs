// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/manager.h
//! Port of zuyu/src/core/hle/service/glue/time/manager.cpp
//!
//! TimeManager: orchestrates time service initialization for the glue layer.
//! Owns the time worker, file timestamp worker, steady clock resource,
//! and time zone binary data.

use super::file_timestamp_worker::FileTimestampWorker;
use super::standard_steady_clock_resource::StandardSteadyClockResource;
use super::worker::TimeWorker;

/// TimeManager corresponds to upstream `Glue::Time::TimeManager`.
///
/// Owns all time-related worker objects and coordinates initialization.
pub struct TimeManager {
    /// The background time worker thread.
    ///
    /// Corresponds to `m_worker` in upstream.
    pub worker: TimeWorker,

    /// The file timestamp worker.
    ///
    /// Corresponds to `m_file_timestamp_worker` in upstream.
    pub file_timestamp_worker: FileTimestampWorker,

    /// The steady clock resource (RTC management).
    ///
    /// Corresponds to `m_steady_clock_resource` in upstream.
    pub steady_clock_resource: StandardSteadyClockResource,

    // TODO: m_time_m (shared_ptr<PSC::Time::ServiceManager>)
    // TODO: m_time_sm (shared_ptr<PSC::Time::StaticService>)
    // TODO: m_time_zone_binary (TimeZoneBinary)
}

impl TimeManager {
    /// Create a new TimeManager.
    ///
    /// Corresponds to `TimeManager::TimeManager(System&)` in upstream manager.cpp.
    pub fn new() -> Self {
        log::debug!("Glue::Time::TimeManager::new called");
        Self {
            worker: TimeWorker::new(),
            file_timestamp_worker: FileTimestampWorker::new(),
            steady_clock_resource: StandardSteadyClockResource::new(),
        }
    }

    /// Initialize the time manager.
    ///
    /// In upstream, this is done during system boot to set up the time service
    /// with proper references to PSC::Time services and settings.
    pub fn initialize(&mut self) {
        log::debug!("Glue::Time::TimeManager::Initialize called");

        // TODO: Full initialization sequence:
        // 1. Get PSC::Time::ServiceManager from SM
        // 2. Get PSC::Time::StaticService
        // 3. Initialize steady_clock_resource
        // 4. Initialize time_zone_binary
        // 5. Initialize worker
        // 6. Start worker thread

        self.worker.initialize();
    }
}
