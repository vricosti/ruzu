// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/olsc.h
//! Port of zuyu/src/core/hle/service/olsc/olsc.cpp
//!
//! LoopProcess: Registers "olsc:u" and "olsc:s" services.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// Service names registered by OLSC.
pub const SERVICE_NAME_APPLICATION: &str = "olsc:u";
pub const SERVICE_NAME_SYSTEM: &str = "olsc:s";
pub const SERVICE_NAME_PROFILE_BG_AGENT: &str = "spbg:sp";

pub struct ISProfileBgAgentForSystemProcess {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISProfileBgAgentForSystemProcess {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[(100, None, "OpenBgAgentController")]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for ISProfileBgAgentForSystemProcess {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        SERVICE_NAME_PROFILE_BG_AGENT
    }
}
impl ServiceFramework for ISProfileBgAgentForSystemProcess {
    fn get_service_name(&self) -> &str {
        SERVICE_NAME_PROFILE_BG_AGENT
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Entry point for the OLSC service module.
///
/// Registers "olsc:u" and "olsc:s" services with a ServerManager.
///
/// Corresponds to `LoopProcess` in upstream `olsc.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let server_manager = ServerManager::new_shared(system);

    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            SERVICE_NAME_APPLICATION,
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(
                    crate::hle::service::olsc::olsc_service_for_application::IOlscServiceForApplication::new(),
                )
            }),
            16,
        );
        server_manager.register_named_service(
            SERVICE_NAME_SYSTEM,
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(
                    crate::hle::service::olsc::olsc_service_for_system_service::IOlscServiceForSystemService::new(system),
                )
            }),
            16,
        );
        server_manager.register_named_service(
            SERVICE_NAME_PROFILE_BG_AGENT,
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(ISProfileBgAgentForSystemProcess::new())
            }),
            16,
        );
    }

    ServerManager::run_server_shared(server_manager);
}
