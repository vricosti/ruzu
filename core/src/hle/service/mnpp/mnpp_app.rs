// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mnpp/mnpp_app.cpp
//!
//! MNPP_APP service ("mnpp:app").

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use std::collections::BTreeMap;

/// MNPP_APP service ("mnpp:app").
pub struct MnppApp {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl MnppApp {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(MnppApp::unknown0_handler), "unknown0"),
            (1, Some(MnppApp::unknown1_handler), "unknown1"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn unknown0_handler(
        _this: &dyn crate::hle::service::service::ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("MNPP_APP::unknown0 (STUBBED) called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(crate::hle::result::RESULT_SUCCESS);
    }

    fn unknown1_handler(
        _this: &dyn crate::hle::service::service::ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("MNPP_APP::unknown1 (STUBBED) called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(crate::hle::result::RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for MnppApp {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "mnpp:app"
    }
}

impl ServiceFramework for MnppApp {
    fn get_service_name(&self) -> &str {
        "mnpp:app"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "mnpp:app" service.
///
/// Corresponds to `LoopProcess` in upstream `mnpp_app.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let server_manager = ServerManager::new_shared(system);
    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "mnpp:app",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(MnppApp::new()) }),
            64,
        );
    }

    ServerManager::run_server_shared(server_manager);
}
