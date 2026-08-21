// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/omm/policy_manager_system.h
//! Port of zuyu/src/core/hle/service/omm/policy_manager_system.cpp

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

pub mod commands {
    pub const GET_AUTO_POWER_DOWN_EVENT: u32 = 0;
    pub const IS_AUTO_POWER_DOWN_REQUESTED: u32 = 1;
    pub const UNKNOWN2: u32 = 2;
    pub const SET_HANDLING_CONTEXT: u32 = 3;
    pub const LOAD_AND_APPLY_SETTINGS: u32 = 4;
    pub const REPORT_USER_IS_ACTIVE: u32 = 5;
}

/// IPolicyManagerSystem service ("idle:sys").
///
/// All commands are `nullptr` in upstream.
pub struct IPolicyManagerSystem {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IPolicyManagerSystem {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    commands::GET_AUTO_POWER_DOWN_EVENT,
                    None,
                    "GetAutoPowerDownEvent",
                ),
                (
                    commands::IS_AUTO_POWER_DOWN_REQUESTED,
                    None,
                    "IsAutoPowerDownRequested",
                ),
                (commands::UNKNOWN2, None, "Unknown2"),
                (commands::SET_HANDLING_CONTEXT, None, "SetHandlingContext"),
                (
                    commands::LOAD_AND_APPLY_SETTINGS,
                    None,
                    "LoadAndApplySettings",
                ),
                (commands::REPORT_USER_IS_ACTIVE, None, "ReportUserIsActive"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IPolicyManagerSystem {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "idle:sys"
    }
}

impl ServiceFramework for IPolicyManagerSystem {
    fn get_service_name(&self) -> &str {
        "idle:sys"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn policy_manager_system_table_matches_upstream_command_count() {
        assert_eq!(IPolicyManagerSystem::new().handlers.len(), 6);
    }
}
