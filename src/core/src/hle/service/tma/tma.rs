// SPDX-FileCopyrightText: Copyright 2026 Eden Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of Eden `src/core/hle/service/tma/tma.{h,cpp}`.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

pub struct HtcTenv {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl HtcTenv {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[(0, None, "GetServiceInterface")]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for HtcTenv {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "htc:tenv"
    }
}
impl ServiceFramework for HtcTenv {
    fn get_service_name(&self) -> &str {
        "htc:tenv"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;
    let server_manager = ServerManager::new_shared(system);
    server_manager.lock().unwrap().register_named_service(
        "htc:tenv",
        Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(HtcTenv::new()) }),
        64,
    );
    ServerManager::run_server_shared(server_manager);
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn service_table_matches_upstream() {
        assert_eq!(HtcTenv::new().handlers().len(), 1);
    }
}
