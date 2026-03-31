// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/account_proxy_interface.h
//! Port of zuyu/src/core/hle/service/ns/account_proxy_interface.cpp
//!
//! IAccountProxyInterface — account proxy operations for NS.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAccountProxyInterface.
///
/// Corresponds to the function table in upstream account_proxy_interface.cpp.
pub mod commands {
    pub const CREATE_USER_ACCOUNT: u32 = 0;
}

/// IAccountProxyInterface.
///
/// Corresponds to `IAccountProxyInterface` in upstream.
pub struct IAccountProxyInterface {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IAccountProxyInterface {
    pub fn new() -> Self {
        let handlers =
            build_handler_map(&[(commands::CREATE_USER_ACCOUNT, None, "CreateUserAccount")]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IAccountProxyInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ns::IAccountProxyInterface"
    }
}

impl ServiceFramework for IAccountProxyInterface {
    fn get_service_name(&self) -> &str {
        "ns::IAccountProxyInterface"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
