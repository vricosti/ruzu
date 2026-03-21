// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/daemon_controller.h
//! Port of zuyu/src/core/hle/service/olsc/daemon_controller.cpp
//!
//! IDaemonController: manages auto-transfer settings for accounts.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IDaemonController.
///
/// Corresponds to `IDaemonController` in upstream daemon_controller.cpp.
pub struct IDaemonController {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IDaemonController {
    pub fn new() -> Self {
        let s: Option<fn(&dyn ServiceFramework, &mut HLERequestContext)> =
            Some(Self::stub_handler);
        let handlers = build_handler_map(&[
            (0, s, "GetAutoTransferEnabledForAccountAndApplication"),
            (1, s, "SetAutoTransferEnabledForAccountAndApplication"),
            (2, s, "GetGlobalUploadEnabledForAccount"),
            (3, s, "SetGlobalUploadEnabledForAccount"),
            (4, s, "TouchAccount"),
            (5, s, "GetGlobalDownloadEnabledForAccount"),
            (6, s, "SetGlobalDownloadEnabledForAccount"),
            (10, s, "GetForbiddenSaveDataIndication"),
            (11, s, "GetStopperObject"),
            (12, s, "GetState"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn stub_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        log::warn!("(STUBBED) IDaemonController command {}", cmd);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Cmd 0: GetAutoTransferEnabledForAccountAndApplication
    ///
    /// Returns whether auto-transfer is enabled for the given user and application.
    /// Upstream always returns false (stubbed).
    pub fn get_auto_transfer_enabled_for_account_and_application(
        &self,
        user_id: u128,
        application_id: u64,
    ) -> (ResultCode, bool) {
        log::warn!(
            "(STUBBED) IDaemonController::get_auto_transfer_enabled_for_account_and_application called, user_id={:032X}, application_id={:016X}",
            user_id,
            application_id
        );
        (RESULT_SUCCESS, false)
    }
}

impl SessionRequestHandler for IDaemonController {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IDaemonController"
    }
}

impl ServiceFramework for IDaemonController {
    fn get_service_name(&self) -> &str {
        "IDaemonController"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
