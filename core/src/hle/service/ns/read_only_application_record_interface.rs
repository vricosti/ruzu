// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/read_only_application_record_interface.cpp/.h

pub const IREAD_ONLY_APPLICATION_RECORD_INTERFACE_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "HasApplicationRecord"),
    (1, false, "NotifyApplicationFailure"),
    (2, true, "IsDataCorruptedResult"),
];

/// Stub: HasApplicationRecord always returns true upstream.
pub fn has_application_record(_program_id: u64) -> bool {
    log::warn!("(STUBBED) IReadOnlyApplicationRecordInterface::HasApplicationRecord called");
    true
}

/// Stub: IsDataCorruptedResult always returns false upstream.
pub fn is_data_corrupted_result(_result: u32) -> bool {
    log::warn!("(STUBBED) IReadOnlyApplicationRecordInterface::IsDataCorruptedResult called");
    false
}
