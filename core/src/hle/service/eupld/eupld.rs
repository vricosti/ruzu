// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/eupld/eupld.cpp
//!
//! ErrorUploadContext ("eupld:c") and ErrorUploadRequest ("eupld:r") services.
//! All commands are unimplemented stubs in upstream.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for ErrorUploadContext
pub mod context_commands {
    pub const SET_URL: u32 = 0;
    pub const IMPORT_CRT: u32 = 1;
    pub const IMPORT_PKI: u32 = 2;
    pub const SET_AUTO_UPLOAD: u32 = 3;
    pub const GET_AUTO_UPLOAD: u32 = 4;
}

/// IPC command IDs for ErrorUploadRequest
pub mod request_commands {
    pub const INITIALIZE: u32 = 0;
    pub const UPLOAD_ALL: u32 = 1;
    pub const UPLOAD_SELECTED: u32 = 2;
    pub const GET_UPLOAD_STATUS: u32 = 3;
    pub const CANCEL_UPLOAD: u32 = 4;
    pub const GET_RESULT: u32 = 5;
}

/// ErrorUploadContext service ("eupld:c"). All stubs.
pub struct ErrorUploadContext {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ErrorUploadContext {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (context_commands::SET_URL, None, "SetUrl"),
                (context_commands::IMPORT_CRT, None, "ImportCrt"),
                (context_commands::IMPORT_PKI, None, "ImportPki"),
                (context_commands::SET_AUTO_UPLOAD, None, "SetAutoUpload"),
                (context_commands::GET_AUTO_UPLOAD, None, "GetAutoUpload"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for ErrorUploadContext {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "eupld:c"
    }
}

impl ServiceFramework for ErrorUploadContext {
    fn get_service_name(&self) -> &str {
        "eupld:c"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// ErrorUploadRequest service ("eupld:r"). All stubs.
pub struct ErrorUploadRequest {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ErrorUploadRequest {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (request_commands::INITIALIZE, None, "Initialize"),
                (request_commands::UPLOAD_ALL, None, "UploadAll"),
                (request_commands::UPLOAD_SELECTED, None, "UploadSelected"),
                (request_commands::GET_UPLOAD_STATUS, None, "GetUploadStatus"),
                (request_commands::CANCEL_UPLOAD, None, "CancelUpload"),
                (request_commands::GET_RESULT, None, "GetResult"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for ErrorUploadRequest {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "eupld:r"
    }
}

impl ServiceFramework for ErrorUploadRequest {
    fn get_service_name(&self) -> &str {
        "eupld:r"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "eupld:c" and "eupld:r" services.
///
/// Corresponds to `LoopProcess` in upstream `eupld.cpp`:
/// ```cpp
/// server_manager->RegisterNamedService("eupld:c", std::make_shared<ErrorUploadContext>(system));
/// server_manager->RegisterNamedService("eupld:r", std::make_shared<ErrorUploadRequest>(system));
/// ```
pub fn loop_process(system: crate::core::SystemRef) {
    let server_manager = crate::hle::service::server_manager::ServerManager::new_shared(system);
    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "eupld:c",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(ErrorUploadContext::new())
            }),
            64,
        );
        server_manager.register_named_service(
            "eupld:r",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(ErrorUploadRequest::new())
            }),
            64,
        );
    }
    crate::hle::service::server_manager::ServerManager::run_server_shared(server_manager);
}
