// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/file_timestamp_worker.h
//! Port of zuyu/src/core/hle/service/glue/time/file_timestamp_worker.cpp
//!
//! FileTimestampWorker: manages filesystem POSIX time updates from the system clock.

use std::sync::Arc;

use crate::hle::service::psc::time::system_clock::SystemClock;
use crate::hle::service::psc::time::time_zone_service::TimeZoneService;

/// FileTimestampWorker updates the filesystem's POSIX time.
///
/// Corresponds to `FileTimestampWorker` in upstream file_timestamp_worker.h.
///
/// Upstream holds:
/// - `m_system_clock` (shared_ptr<PSC::Time::SystemClock>)
/// - `m_time_zone` (shared_ptr<PSC::Time::TimeZoneService>)
///
/// These are set during `TimeWorker::initialize` from the `time:sm` service.
pub struct FileTimestampWorker {
    /// Upstream: `m_system_clock`.
    pub system_clock: Option<Arc<SystemClock>>,
    /// Upstream: `m_time_zone`.
    pub time_zone: Option<Arc<TimeZoneService>>,
    /// Whether this worker has been initialized with clock references.
    ///
    /// Corresponds to `m_initialized` in upstream.
    pub initialized: bool,
}

impl FileTimestampWorker {
    pub fn new() -> Self {
        Self {
            system_clock: None,
            time_zone: None,
            initialized: false,
        }
    }

    /// Update the filesystem's POSIX time from the system clock.
    ///
    /// Corresponds to `FileTimestampWorker::SetFilesystemPosixTime` in upstream.
    ///
    /// Upstream implementation:
    ///   1. m_system_clock->GetCurrentTime(&time)
    ///   2. m_time_zone->ToCalendarTimeWithMyRule(&cal, &info, time)
    ///   3. IFileSystemProxy::SetCurrentPosixTime (upstream also has a TODO here)
    ///
    /// Step 3 is unimplemented even in upstream (marked as a TODO).
    pub fn set_filesystem_posix_time(&self) {
        if !self.initialized {
            return;
        }

        let Some(system_clock) = self.system_clock.as_ref() else {
            return;
        };
        let Some(time_zone) = self.time_zone.as_ref() else {
            return;
        };
        let Ok(time) = system_clock.get_current_time() else {
            return;
        };
        let Ok((_calendar_time, _additional_info)) = time_zone.to_calendar_time_with_my_rule(time)
        else {
            return;
        };

        // TODO IFileSystemProxy::SetCurrentPosixTime (also unimplemented in Eden).
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::service::psc::time::common::SystemClockContext;

    #[test]
    fn retains_configured_services_for_worker_lifetime() {
        let system_clock = Arc::new(SystemClock::with_state(
            false,
            false,
            true,
            SystemClockContext::default(),
            123,
        ));
        let time_zone = Arc::new(TimeZoneService::new(false));
        let mut worker = FileTimestampWorker::new();
        worker.system_clock = Some(Arc::clone(&system_clock));
        worker.time_zone = Some(Arc::clone(&time_zone));
        worker.initialized = true;

        assert_eq!(Arc::strong_count(&system_clock), 2);
        assert_eq!(Arc::strong_count(&time_zone), 2);
        worker.set_filesystem_posix_time();
        drop(worker);
        assert_eq!(Arc::strong_count(&system_clock), 1);
        assert_eq!(Arc::strong_count(&time_zone), 1);
    }
}
