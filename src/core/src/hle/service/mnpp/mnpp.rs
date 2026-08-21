// SPDX-FileCopyrightText: Copyright 2026 Eden Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of Eden `src/core/hle/service/mnpp/mnpp.{h,cpp}`.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

pub struct MnppApp {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl MnppApp {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, Some(Self::cmd0_handler), "Cmd0"),
                (1, Some(Self::cmd1_handler), "Cmd1"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn cmd0_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) MnppApp::cmd0 called");
        let mut response = ResponseBuilder::new(ctx, 2, 0, 0);
        response.push_result(RESULT_SUCCESS);
    }

    fn cmd1_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) MnppApp::cmd1 called");
        let mut response = ResponseBuilder::new(ctx, 2, 0, 0);
        response.push_result(RESULT_SUCCESS);
    }
}

macro_rules! impl_framework {
    ($type:ty, $name:literal) => {
        impl SessionRequestHandler for $type {
            fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
                ServiceFramework::handle_sync_request_impl(self, ctx)
            }

            fn service_name(&self) -> &str {
                $name
            }
        }

        impl ServiceFramework for $type {
            fn get_service_name(&self) -> &str {
                $name
            }

            fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
                &self.handlers
            }

            fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
                &self.handlers_tipc
            }
        }
    };
}

impl_framework!(MnppApp, "mnpp:app");

pub struct MnppSys {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl MnppSys {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, None, "Cmd0"),
                (10, None, "Cmd10"),
                (100, None, "Cmd100"),
                (200, None, "Cmd200"),
                (300, None, "Cmd300"),
                (400, None, "Cmd400"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl_framework!(MnppSys, "mnpp:sys");

pub struct MnppWeb {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl MnppWeb {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, None, "Cmd0"),
                (1, None, "Cmd1"),
                (10, None, "Cmd10"),
                (20, None, "Cmd20"),
                (100, None, "Cmd100"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl_framework!(MnppWeb, "mnpp:web");

pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;

    let server_manager = ServerManager::new_shared(system);
    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "mnpp:app",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(MnppApp::new()) }),
            64,
        );
        server_manager.register_named_service(
            "mnpp:sys",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(MnppSys::new()) }),
            64,
        );
        server_manager.register_named_service(
            "mnpp:web",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(MnppWeb::new()) }),
            64,
        );
    }
    ServerManager::run_server_shared(server_manager);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn service_tables_match_upstream() {
        assert_eq!(MnppApp::new().handlers().len(), 2);
        assert_eq!(MnppSys::new().handlers().len(), 6);
        assert_eq!(MnppWeb::new().handlers().len(), 5);
    }
}
