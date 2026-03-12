// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/document_interface.h
//! Port of zuyu/src/core/hle/service/ns/document_interface.cpp
//!
//! IDocumentInterface — document-related operations for NS.

use crate::hle::result::ResultCode;

/// IPC command table for IDocumentInterface.
///
/// Corresponds to the function table in upstream document_interface.cpp.
pub mod commands {
    pub const GET_APPLICATION_CONTENT_PATH: u32 = 21;
    pub const RESOLVE_APPLICATION_CONTENT_PATH: u32 = 23;
    pub const GET_RUNNING_APPLICATION_PROGRAM_ID: u32 = 92;
}

/// IDocumentInterface.
///
/// Corresponds to `IDocumentInterface` in upstream.
pub struct IDocumentInterface;

impl IDocumentInterface {
    pub fn new() -> Self {
        Self
    }

    /// ResolveApplicationContentPath (cmd 23).
    ///
    /// Corresponds to upstream `IDocumentInterface::ResolveApplicationContentPath`.
    pub fn resolve_application_content_path(
        &self,
        file_system_proxy_type: u8,
        program_id: u64,
    ) -> Result<(), ResultCode> {
        log::warn!(
            "(STUBBED) ResolveApplicationContentPath called, file_system_proxy_type={}, program_id={:016x}",
            file_system_proxy_type,
            program_id,
        );
        Ok(())
    }

    /// GetRunningApplicationProgramId (cmd 92).
    ///
    /// Corresponds to upstream `IDocumentInterface::GetRunningApplicationProgramId`.
    pub fn get_running_application_program_id(
        &self,
        _caller_program_id: u64,
    ) -> Result<u64, ResultCode> {
        log::warn!("(STUBBED) GetRunningApplicationProgramId called");
        // TODO: return system.get_application_process_program_id()
        Ok(0)
    }
}
