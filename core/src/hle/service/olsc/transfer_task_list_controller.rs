// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/transfer_task_list_controller.h
//! Port of zuyu/src/core/hle/service/olsc/transfer_task_list_controller.cpp
//!
//! ITransferTaskListController: manages transfer task lists.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::native_handle_holder::INativeHandleHolder;

/// ITransferTaskListController.
///
/// Corresponds to `ITransferTaskListController` in upstream transfer_task_list_controller.cpp.
pub struct ITransferTaskListController {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ITransferTaskListController {
    pub fn new() -> Self {
        let s: Option<fn(&dyn ServiceFramework, &mut HLERequestContext)> =
            Some(Self::stub_handler);
        let handlers = build_handler_map(&[
            (0, s, "Unknown0"),
            (1, s, "Unknown1"),
            (2, s, "Unknown2"),
            (3, s, "Unknown3"),
            (4, s, "Unknown4"),
            (5, s, "GetNativeHandleHolder"),
            (6, s, "Unknown6"),
            (7, s, "Unknown7"),
            (8, s, "Unknown8"),
            (9, s, "GetNativeHandleHolder2"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn stub_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        log::warn!("(STUBBED) ITransferTaskListController command {}", cmd);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Cmd 5 / Cmd 9: GetNativeHandleHolder
    ///
    /// Returns a new INativeHandleHolder instance.
    pub fn get_native_handle_holder(&self) -> (ResultCode, INativeHandleHolder) {
        log::warn!("(STUBBED) ITransferTaskListController::get_native_handle_holder called");
        (RESULT_SUCCESS, INativeHandleHolder::new())
    }
}

impl SessionRequestHandler for ITransferTaskListController {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ITransferTaskListController"
    }
}

impl ServiceFramework for ITransferTaskListController {
    fn get_service_name(&self) -> &str {
        "ITransferTaskListController"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
