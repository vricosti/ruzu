// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/document_interface.h
//! Port of zuyu/src/core/hle/service/ns/document_interface.cpp
//!
//! IDocumentInterface — document-related operations for NS.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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
pub struct IDocumentInterface {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    system: crate::core::SystemRef,
}

impl IDocumentInterface {
    pub fn new(system: crate::core::SystemRef) -> Self {
        let handlers = build_handler_map(&[
            (
                commands::GET_APPLICATION_CONTENT_PATH,
                None,
                "GetApplicationContentPath",
            ),
            (
                commands::RESOLVE_APPLICATION_CONTENT_PATH,
                None,
                "ResolveApplicationContentPath",
            ),
            (
                commands::GET_RUNNING_APPLICATION_PROGRAM_ID,
                None,
                "GetRunningApplicationProgramId",
            ),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            system,
        }
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
        Ok(self.system.get().runtime_program_id())
    }
}

impl SessionRequestHandler for IDocumentInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ns::IDocumentInterface"
    }
}

impl ServiceFramework for IDocumentInterface {
    fn get_service_name(&self) -> &str {
        "ns::IDocumentInterface"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
