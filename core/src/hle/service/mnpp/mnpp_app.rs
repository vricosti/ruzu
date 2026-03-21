// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mnpp/mnpp_app.cpp
//!
//! MNPP_APP service ("mnpp:app").

use std::collections::BTreeMap;
use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// MNPP_APP service ("mnpp:app").
pub struct MnppApp {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl MnppApp {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, None, "unknown0"),
            (1, None, "unknown1"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn unknown0(&self) {
        log::warn!("(STUBBED) MnppApp::unknown0 called");
    }

    pub fn unknown1(&self) {
        log::warn!("(STUBBED) MnppApp::unknown1 called");
    }
}

impl SessionRequestHandler for MnppApp {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str { "mnpp:app" }
}

impl ServiceFramework for MnppApp {
    fn get_service_name(&self) -> &str { "mnpp:app" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

/// Registers "mnpp:app" service.
///
/// Corresponds to `LoopProcess` in upstream `mnpp_app.cpp`.
pub fn loop_process() {
    // TODO: register "mnpp:app" -> MnppApp with ServerManager
}
