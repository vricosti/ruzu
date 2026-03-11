// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/system_archive/system_archive.h / .cpp

use crate::file_sys::vfs::vfs_types::VirtualFile;

/// Synthesize a system archive for the given title ID.
/// Corresponds to upstream `FileSys::SystemArchive::SynthesizeSystemArchive`.
///
/// Stub: returns None for all title IDs.
/// TODO: implement archive synthesis for known system title IDs.
pub fn synthesize_system_archive(_title_id: u64) -> Option<VirtualFile> {
    None
}
