// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/grc/grc.cpp
//!
//! GRC service ("grc:c"). All commands are unimplemented stubs.

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use std::collections::BTreeMap;

/// GRC service ("grc:c"). All stubs.
pub struct GRC {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl GRC {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (1, None, "OpenContinuousRecorder"),
            (2, None, "OpenGameMovieTrimmer"),
            (3, None, "OpenOffscreenRecorder"),
            (101, None, "CreateMovieMaker"),
            (9903, None, "SetOffscreenRecordingMarker"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for GRC {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "grc:c"
    }
}

impl ServiceFramework for GRC {
    fn get_service_name(&self) -> &str {
        "grc:c"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "grc:c" service.
///
/// Corresponds to `LoopProcess` in upstream `grc.cpp`:
/// ```cpp
/// server_manager->RegisterNamedService("grc:c", std::make_shared<GRC>(system));
/// ```
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let mut server_manager = ServerManager::new(system);

    server_manager.register_named_service(
        "grc:c",
        Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(GRC::new()) }),
        16,
    );

    ServerManager::run_server(server_manager);
}
