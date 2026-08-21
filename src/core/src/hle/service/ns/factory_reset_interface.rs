// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/factory_reset_interface.h
//! Port of zuyu/src/core/hle/service/ns/factory_reset_interface.cpp
//!
//! IFactoryResetInterface — factory reset operations for NS.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IFactoryResetInterface.
///
/// Corresponds to the function table in upstream factory_reset_interface.cpp.
pub mod commands {
    pub const RESET_TO_FACTORY_SETTINGS: u32 = 100;
    pub const RESET_TO_FACTORY_SETTINGS_WITHOUT_USER_SAVE_DATA: u32 = 101;
    pub const RESET_TO_FACTORY_SETTINGS_FOR_REFURBISHMENT: u32 = 102;
    pub const RESET_TO_FACTORY_SETTINGS_WITH_PLATFORM_REGION: u32 = 103;
    pub const RESET_TO_FACTORY_SETTINGS_WITH_PLATFORM_REGION_AUTHENTICATION: u32 = 104;
    pub const REQUEST_RESET_TO_FACTORY_SETTINGS_SECURELY: u32 = 105;
    pub const REQUEST_RESET_TO_FACTORY_SETTINGS_WITH_PLATFORM_REGION_AUTHENTICATION_SECURELY: u32 =
        106;
}

/// IFactoryResetInterface.
///
/// Corresponds to `IFactoryResetInterface` in upstream.
pub struct IFactoryResetInterface {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IFactoryResetInterface {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (commands::RESET_TO_FACTORY_SETTINGS, None, "ResetToFactorySettings"),
            (commands::RESET_TO_FACTORY_SETTINGS_WITHOUT_USER_SAVE_DATA, None, "ResetToFactorySettingsWithoutUserSaveData"),
            (commands::RESET_TO_FACTORY_SETTINGS_FOR_REFURBISHMENT, None, "ResetToFactorySettingsForRefurbishment"),
            (commands::RESET_TO_FACTORY_SETTINGS_WITH_PLATFORM_REGION, None, "ResetToFactorySettingsWithPlatformRegion"),
            (commands::RESET_TO_FACTORY_SETTINGS_WITH_PLATFORM_REGION_AUTHENTICATION, None, "ResetToFactorySettingsWithPlatformRegionAuthentication"),
            (commands::REQUEST_RESET_TO_FACTORY_SETTINGS_SECURELY, None, "RequestResetToFactorySettingsSecurely"),
            (commands::REQUEST_RESET_TO_FACTORY_SETTINGS_WITH_PLATFORM_REGION_AUTHENTICATION_SECURELY, None, "RequestResetToFactorySettingsWithPlatformRegionAuthenticationSecurely"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IFactoryResetInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ns::IFactoryResetInterface"
    }
}

impl ServiceFramework for IFactoryResetInterface {
    fn get_service_name(&self) -> &str {
        "ns::IFactoryResetInterface"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
