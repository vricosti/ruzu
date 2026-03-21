// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/acc_aa.h
//! Port of zuyu/src/core/hle/service/acc/acc_aa.cpp
//!
//! ACC_AA service ("acc:aa").

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for ACC_AA
pub mod commands {
    pub const ENSURE_CACHE_ASYNC: u32 = 0;
    pub const LOAD_CACHE: u32 = 1;
    pub const GET_DEVICE_ACCOUNT_ID: u32 = 2;
    pub const REGISTER_NOTIFICATION_TOKEN_ASYNC: u32 = 50;  // 1.0.0 - 6.2.0
    pub const UNREGISTER_NOTIFICATION_TOKEN_ASYNC: u32 = 51; // 1.0.0 - 6.2.0
}

/// ACC_AA service.
///
/// Corresponds to `ACC_AA` in upstream `acc_aa.h`.
/// All handlers are nullptr (unimplemented) in upstream.
pub struct AccAA {
    pub interface: super::acc::Interface,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl AccAA {
    /// Matches upstream `AccAA(shared_ptr<Module>, shared_ptr<ProfileManager>, System&)`.
    pub fn new(
        module: std::sync::Arc<super::acc::Module>,
        profile_manager: std::sync::Arc<std::sync::Mutex<super::profile_manager::ProfileManager>>,
        system: crate::core::SystemRef,
    ) -> Self {
        let handlers = build_handler_map(&[
            (0, None, "EnsureCacheAsync"),
            (1, None, "LoadCache"),
            (2, None, "GetDeviceAccountId"),
            (50, None, "RegisterNotificationTokenAsync"),
            (51, None, "UnregisterNotificationTokenAsync"),
        ]);

        Self {
            interface: super::acc::Interface::new(module, profile_manager, system, "acc:aa"),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for AccAA {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "acc:aa"
    }
}

impl ServiceFramework for AccAA {
    fn get_service_name(&self) -> &str {
        "acc:aa"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
