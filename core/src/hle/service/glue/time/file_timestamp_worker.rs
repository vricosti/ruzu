// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/file_timestamp_worker.h
//! Port of zuyu/src/core/hle/service/glue/time/file_timestamp_worker.cpp
//!
//! FileTimestampWorker: manages filesystem POSIX time updates from the system clock.

/// FileTimestampWorker updates the filesystem's POSIX time.
///
/// Corresponds to `FileTimestampWorker` in upstream file_timestamp_worker.h.
///
/// Upstream holds:
/// - `m_system_clock` (shared_ptr<PSC::Time::SystemClock>)
/// - `m_time_zone` (shared_ptr<PSC::Time::TimeZoneService>)
///
/// These are set during TimeWorker::Initialize via the time:sm service.
/// The clock references cannot be stored here until the time service
/// manager is fully wired (time:sm provides GetStandardLocalSystemClock
/// and GetTimeZoneService).
pub struct FileTimestampWorker {
    /// Whether this worker has been initialized with clock references.
    ///
    /// Corresponds to `m_initialized` in upstream.
    pub initialized: bool,
}

impl FileTimestampWorker {
    pub fn new() -> Self {
        Self { initialized: false }
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
    /// Steps 1-2 require the SystemClock and TimeZoneService references from
    /// time:sm. Step 3 is unimplemented even in upstream (marked as a TODO).
    pub fn set_filesystem_posix_time(&self) {
        if !self.initialized {
            return;
        }

        log::debug!("FileTimestampWorker::SetFilesystemPosixTime called (no-op: upstream also leaves IFileSystemProxy::SetCurrentPosixTime unimplemented)");
    }
}
