// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mig/mig.cpp
//!
//! MIG_USR service registered as "mig:user". All commands are unimplemented stubs.

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use std::collections::BTreeMap;

/// MIG_USR service object. Upstream names the object "mig:usr" and registers
/// it under the named service "mig:user".
pub struct MigUsr {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl MigUsr {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (10, None, "TryGetLastMigrationInfo"),
            (100, None, "CreateServer"),
            (101, None, "ResumeServer"),
            (200, None, "CreateClient"),
            (201, None, "ResumeClient"),
            (1001, None, "Unknown1001"),
            (1010, None, "Unknown1010"),
            (1100, None, "Unknown1100"),
            (1101, None, "Unknown1101"),
            (1200, None, "Unknown1200"),
            (1201, None, "Unknown1201"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for MigUsr {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "mig:usr"
    }
}

impl ServiceFramework for MigUsr {
    fn get_service_name(&self) -> &str {
        "mig:usr"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "mig:user" service.
///
/// Corresponds to `LoopProcess` in upstream `mig.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let server_manager = ServerManager::new_shared(system);

    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "mig:user",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(MigUsr::new()) }),
            64,
        );
    }
    ServerManager::run_server_shared(server_manager);
}
