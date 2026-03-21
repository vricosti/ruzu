// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/read_only_application_record_interface.h
//! Port of zuyu/src/core/hle/service/ns/read_only_application_record_interface.cpp
//!
//! IReadOnlyApplicationRecordInterface — read-only access to application records.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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
pub struct IReadOnlyApplicationRecordInterface {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IReadOnlyApplicationRecordInterface {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (commands::HAS_APPLICATION_RECORD, None, "HasApplicationRecord"),
            (commands::NOTIFY_APPLICATION_FAILURE, None, "NotifyApplicationFailure"),
            (commands::IS_DATA_CORRUPTED_RESULT, None, "IsDataCorruptedResult"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
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

impl SessionRequestHandler for IReadOnlyApplicationRecordInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ns::IReadOnlyApplicationRecordInterface"
    }
}

impl ServiceFramework for IReadOnlyApplicationRecordInterface {
    fn get_service_name(&self) -> &str {
        "ns::IReadOnlyApplicationRecordInterface"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
