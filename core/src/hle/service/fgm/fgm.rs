// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/fgm/fgm.cpp
//!
//! FGM service and FGM_DBG service.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for IRequest (returned by FGM::Initialize)
pub mod request_commands {
    pub const INITIALIZE: u32 = 0;
    pub const SET: u32 = 1;
    pub const GET: u32 = 2;
    pub const CANCEL: u32 = 3;
}

/// IPC command IDs for FGM
pub mod fgm_commands {
    pub const INITIALIZE: u32 = 0;
}

/// IPC command IDs for FGM_DBG
pub mod fgm_dbg_commands {
    pub const INITIALIZE: u32 = 0;
    pub const READ: u32 = 1;
    pub const CANCEL: u32 = 2;
}

/// IRequest interface returned by FGM::Initialize. All stubs.
pub struct IRequest {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IRequest {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (request_commands::INITIALIZE, None, "Initialize"),
                (request_commands::SET, None, "Set"),
                (request_commands::GET, None, "Get"),
                (request_commands::CANCEL, None, "Cancel"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IRequest {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IRequest"
    }
}

impl ServiceFramework for IRequest {
    fn get_service_name(&self) -> &str {
        "IRequest"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// FGM service ("fgm", "fgm:0", "fgm:9").
pub struct FGM {
    name: String,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl FGM {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            handlers: build_handler_map(&[(
                fgm_commands::INITIALIZE,
                Some(FGM::initialize_handler),
                "Initialize",
            )]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Initialize (cmd 0) - creates an IRequest interface
    pub fn initialize(&self) -> IRequest {
        log::debug!("FGM({})::initialize called", self.name);
        IRequest::new()
    }

    fn initialize_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("FGM::Initialize called");
        let request: std::sync::Arc<dyn SessionRequestHandler> =
            std::sync::Arc::new(IRequest::new());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(crate::hle::result::RESULT_SUCCESS);
        rb.push_ipc_interface(request);
    }
}

impl SessionRequestHandler for FGM {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        &self.name
    }
}

impl ServiceFramework for FGM {
    fn get_service_name(&self) -> &str {
        &self.name
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// FGM_DBG service ("fgm:dbg"). All stubs.
pub struct FgmDbg {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl FgmDbg {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (fgm_dbg_commands::INITIALIZE, None, "Initialize"),
                (fgm_dbg_commands::READ, None, "Read"),
                (fgm_dbg_commands::CANCEL, None, "Cancel"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for FgmDbg {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "fgm:dbg"
    }
}

impl ServiceFramework for FgmDbg {
    fn get_service_name(&self) -> &str {
        "fgm:dbg"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "fgm", "fgm:0", "fgm:9", "fgm:dbg" services.
///
/// Corresponds to `LoopProcess` in upstream `fgm.cpp`:
/// ```cpp
/// server_manager->RegisterNamedService("fgm", std::make_shared<FGM>(system, "fgm"));
/// server_manager->RegisterNamedService("fgm:0", std::make_shared<FGM>(system, "fgm:0"));
/// server_manager->RegisterNamedService("fgm:9", std::make_shared<FGM>(system, "fgm:9"));
/// server_manager->RegisterNamedService("fgm:dbg", std::make_shared<FGM_DBG>(system));
/// ```
pub fn loop_process(system: crate::core::SystemRef) {
    let server_manager = crate::hle::service::server_manager::ServerManager::new_shared(system);
    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "fgm",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(FGM::new("fgm")) }),
            64,
        );
        server_manager.register_named_service(
            "fgm:0",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(FGM::new("fgm:0")) }),
            64,
        );
        server_manager.register_named_service(
            "fgm:9",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(FGM::new("fgm:9")) }),
            64,
        );
        server_manager.register_named_service(
            "fgm:dbg",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(FgmDbg::new()) }),
            64,
        );
    }
    crate::hle::service::server_manager::ServerManager::run_server_shared(server_manager);
}
