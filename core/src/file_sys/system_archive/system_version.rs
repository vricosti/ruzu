// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/system_archive/system_version.h / .cpp

use crate::file_sys::vfs::vfs_types::VirtualDir;

/// Get the long display version string.
/// Corresponds to upstream `GetLongDisplayVersion`.
pub fn get_long_display_version() -> String {
    // TODO: return proper version string matching upstream.
    String::from("0.0.0")
}

/// Synthesize the SystemVersion archive.
/// Stub: returns None.
pub fn system_version() -> Option<VirtualDir> {
    None
}
