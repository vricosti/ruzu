// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mig/mig.cpp
//!
//! MIG_USR service ("mig:user"). All commands are unimplemented stubs.

use std::collections::BTreeMap;
use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// MIG_USR service ("mig:user"). All stubs.
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
    fn service_name(&self) -> &str { "mig:user" }
}

impl ServiceFramework for MigUsr {
    fn get_service_name(&self) -> &str { "mig:user" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

/// Registers "mig:user" service.
///
/// Corresponds to `LoopProcess` in upstream `mig.cpp`.
pub fn loop_process() {
    // TODO: register "mig:user" -> MigUsr with ServerManager
}
