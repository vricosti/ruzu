// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/read_only_application_record_interface.h
//! Port of zuyu/src/core/hle/service/ns/read_only_application_record_interface.cpp
//!
//! IReadOnlyApplicationRecordInterface — read-only access to application records.

use crate::hle::result::ResultCode;

/// IPC command table for IReadOnlyApplicationRecordInterface.
///
/// Corresponds to the function table in upstream read_only_application_record_interface.cpp.
pub mod commands {
    pub const HAS_APPLICATION_RECORD: u32 = 0;
    pub const NOTIFY_APPLICATION_FAILURE: u32 = 1;
    pub const IS_DATA_CORRUPTED_RESULT: u32 = 2;
}

/// IReadOnlyApplicationRecordInterface.
///
/// Corresponds to `IReadOnlyApplicationRecordInterface` in upstream.
pub struct IReadOnlyApplicationRecordInterface;

impl IReadOnlyApplicationRecordInterface {
    pub fn new() -> Self {
        Self
    }

    /// HasApplicationRecord (cmd 0).
    ///
    /// Corresponds to upstream `IReadOnlyApplicationRecordInterface::HasApplicationRecord`.
    pub fn has_application_record(&self, program_id: u64) -> Result<bool, ResultCode> {
        log::warn!(
            "(STUBBED) HasApplicationRecord called, program_id={:016x}",
            program_id,
        );
        Ok(true)
    }

    /// IsDataCorruptedResult (cmd 2).
    ///
    /// Corresponds to upstream `IReadOnlyApplicationRecordInterface::IsDataCorruptedResult`.
    pub fn is_data_corrupted_result(&self, result: u32) -> Result<bool, ResultCode> {
        log::warn!(
            "(STUBBED) IsDataCorruptedResult called, result={:#x}",
            result,
        );
        Ok(false)
    }
}
