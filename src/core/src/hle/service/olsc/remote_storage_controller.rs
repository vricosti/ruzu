// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/remote_storage_controller.h
//! Port of zuyu/src/core/hle/service/olsc/remote_storage_controller.cpp
//!
//! IRemoteStorageController: manages remote save data storage operations.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IRemoteStorageController.
///
/// Corresponds to `IRemoteStorageController` in upstream remote_storage_controller.cpp.
pub struct IRemoteStorageController {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    system: crate::core::SystemRef,
}

impl IRemoteStorageController {
    pub fn new(system: crate::core::SystemRef) -> Self {
        let s: Option<fn(&dyn ServiceFramework, &mut HLERequestContext)> = Some(Self::stub_handler);
        let handlers = build_handler_map(&[
            (0, s, "Unknown0"),
            (1, s, "Unknown1"),
            (2, s, "Unknown2"),
            (3, s, "Unknown3"),
            (4, s, "Unknown4"),
            (5, s, "Unknown5"),
            (6, s, "Unknown6"),
            (7, s, "Unknown7"),
            (8, s, "Unknown8"),
            (9, s, "Unknown9"),
            (10, s, "Unknown10"),
            (11, s, "Unknown11"),
            (12, s, "Unknown12"),
            (13, s, "Unknown13"),
            (14, s, "Unknown14"),
            (19, s, "Unknown19"),
            (22, s, "GetSecondarySave"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            system,
        }
    }

    fn stub_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        log::warn!("(STUBBED) IRemoteStorageController command {}", cmd);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Cmd 22: GetSecondarySave
    ///
    /// Returns whether a secondary save exists for the given application.
    /// Upstream always returns false with zeroed output (stubbed).
    pub fn get_secondary_save(&self, application_id: u64) -> (ResultCode, bool, [u64; 3]) {
        log::error!(
            "(STUBBED) IRemoteStorageController::get_secondary_save called, application_id={:016X}",
            application_id
        );
        (RESULT_SUCCESS, false, [0u64; 3])
    }
}

impl SessionRequestHandler for IRemoteStorageController {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IRemoteStorageController"
    }
}

impl ServiceFramework for IRemoteStorageController {
    fn get_service_name(&self) -> &str {
        "IRemoteStorageController"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
