// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/document_interface.cpp/.h

use super::ns_types::ContentPath;

pub const IDOCUMENT_INTERFACE_COMMANDS: &[(u32, bool, &str)] = &[
    (21, false, "GetApplicationContentPath"),
    (23, true, "ResolveApplicationContentPath"),
    (92, true, "GetRunningApplicationProgramId"),
];

/// Stub: ResolveApplicationContentPath does nothing upstream.
pub fn resolve_application_content_path(content_path: &ContentPath) {
    log::warn!(
        "(STUBBED) IDocumentInterface::ResolveApplicationContentPath called, \
         file_system_proxy_type={}, program_id={:016X}",
        content_path.file_system_proxy_type,
        content_path.program_id
    );
}
