// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/application_version_interface.h
//! Port of zuyu/src/core/hle/service/ns/application_version_interface.cpp
//!
//! IApplicationVersionInterface — application version management for NS.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IApplicationVersionInterface.
///
/// Corresponds to the function table in upstream application_version_interface.cpp.
pub mod commands {
    pub const GET_LAUNCH_REQUIRED_VERSION: u32 = 0;
    pub const UPGRADE_LAUNCH_REQUIRED_VERSION: u32 = 1;
    pub const UPDATE_VERSION_LIST: u32 = 35;
    pub const PUSH_LAUNCH_VERSION: u32 = 36;
    pub const LIST_REQUIRED_VERSION: u32 = 37;
    pub const REQUEST_VERSION_LIST: u32 = 800;
    pub const LIST_VERSION_LIST: u32 = 801;
    pub const REQUEST_VERSION_LIST_DATA: u32 = 802;
    pub const IMPORT_AUTO_UPDATE_POLICY_JSON_FOR_DEBUG: u32 = 900;
    pub const LIST_DEFAULT_AUTO_UPDATE_POLICY: u32 = 901;
    pub const LIST_AUTO_UPDATE_POLICY_FOR_SPECIFIC_APPLICATION: u32 = 902;
    pub const PERFORM_AUTO_UPDATE: u32 = 1000;
    pub const LIST_AUTO_UPDATE_SCHEDULE: u32 = 1001;
}

/// IApplicationVersionInterface.
///
/// Corresponds to `IApplicationVersionInterface` in upstream.
pub struct IApplicationVersionInterface {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IApplicationVersionInterface {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (
                commands::GET_LAUNCH_REQUIRED_VERSION,
                None,
                "GetLaunchRequiredVersion",
            ),
            (
                commands::UPGRADE_LAUNCH_REQUIRED_VERSION,
                None,
                "UpgradeLaunchRequiredVersion",
            ),
            (commands::UPDATE_VERSION_LIST, None, "UpdateVersionList"),
            (commands::PUSH_LAUNCH_VERSION, None, "PushLaunchVersion"),
            (commands::LIST_REQUIRED_VERSION, None, "ListRequiredVersion"),
            (commands::REQUEST_VERSION_LIST, None, "RequestVersionList"),
            (commands::LIST_VERSION_LIST, None, "ListVersionList"),
            (
                commands::REQUEST_VERSION_LIST_DATA,
                None,
                "RequestVersionListData",
            ),
            (
                commands::IMPORT_AUTO_UPDATE_POLICY_JSON_FOR_DEBUG,
                None,
                "ImportAutoUpdatePolicyJsonForDebug",
            ),
            (
                commands::LIST_DEFAULT_AUTO_UPDATE_POLICY,
                None,
                "ListDefaultAutoUpdatePolicy",
            ),
            (
                commands::LIST_AUTO_UPDATE_POLICY_FOR_SPECIFIC_APPLICATION,
                None,
                "ListAutoUpdatePolicyForSpecificApplication",
            ),
            (commands::PERFORM_AUTO_UPDATE, None, "PerformAutoUpdate"),
            (
                commands::LIST_AUTO_UPDATE_SCHEDULE,
                None,
                "ListAutoUpdateSchedule",
            ),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IApplicationVersionInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ns::IApplicationVersionInterface"
    }
}

impl ServiceFramework for IApplicationVersionInterface {
    fn get_service_name(&self) -> &str {
        "ns::IApplicationVersionInterface"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
