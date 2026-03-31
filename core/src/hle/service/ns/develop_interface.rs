// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/develop_interface.h
//! Port of zuyu/src/core/hle/service/ns/develop_interface.cpp
//!
//! IDevelopInterface — "ns:dev" service.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IDevelopInterface.
///
/// Corresponds to the function table in upstream develop_interface.cpp.
pub mod commands {
    pub const LAUNCH_PROGRAM: u32 = 0;
    pub const TERMINATE_PROCESS: u32 = 1;
    pub const TERMINATE_PROGRAM: u32 = 2;
    pub const GET_SHELL_EVENT: u32 = 4;
    pub const GET_SHELL_EVENT_INFO: u32 = 5;
    pub const TERMINATE_APPLICATION: u32 = 6;
    pub const PREPARE_LAUNCH_PROGRAM_FROM_HOST: u32 = 7;
    pub const LAUNCH_APPLICATION_FROM_HOST: u32 = 8;
    pub const LAUNCH_APPLICATION_WITH_STORAGE_ID_FOR_DEVELOP: u32 = 9;
    pub const IS_SYSTEM_MEMORY_RESOURCE_LIMIT_BOOSTED: u32 = 10;
    pub const GET_RUNNING_APPLICATION_PROCESS_ID_FOR_DEVELOP: u32 = 11;
    pub const SET_CURRENT_APPLICATION_RIGHTS_ENVIRONMENT_CAN_BE_ACTIVE_FOR_DEVELOP: u32 = 12;
    pub const CREATE_APPLICATION_RESOURCE_FOR_DEVELOP: u32 = 13;
    pub const IS_PREOMIA_FOR_DEVELOP: u32 = 14;
    pub const GET_APPLICATION_PROGRAM_ID_FROM_HOST: u32 = 15;
    pub const REFRESH_CACHED_DEBUG_VALUES: u32 = 16;
    pub const PREPARE_LAUNCH_APPLICATION_FROM_HOST: u32 = 17;
    pub const GET_LAUNCH_EVENT: u32 = 18;
    pub const GET_LAUNCH_RESULT: u32 = 19;
}

/// IDevelopInterface.
///
/// Corresponds to `IDevelopInterface` in upstream.
pub struct IDevelopInterface {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IDevelopInterface {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (commands::LAUNCH_PROGRAM, None, "LaunchProgram"),
            (commands::TERMINATE_PROCESS, None, "TerminateProcess"),
            (commands::TERMINATE_PROGRAM, None, "TerminateProgram"),
            (commands::GET_SHELL_EVENT, None, "GetShellEvent"),
            (commands::GET_SHELL_EVENT_INFO, None, "GetShellEventInfo"),
            (
                commands::TERMINATE_APPLICATION,
                None,
                "TerminateApplication",
            ),
            (
                commands::PREPARE_LAUNCH_PROGRAM_FROM_HOST,
                None,
                "PrepareLaunchProgramFromHost",
            ),
            (
                commands::LAUNCH_APPLICATION_FROM_HOST,
                None,
                "LaunchApplicationFromHost",
            ),
            (
                commands::LAUNCH_APPLICATION_WITH_STORAGE_ID_FOR_DEVELOP,
                None,
                "LaunchApplicationWithStorageIdForDevelop",
            ),
            (
                commands::IS_SYSTEM_MEMORY_RESOURCE_LIMIT_BOOSTED,
                None,
                "IsSystemMemoryResourceLimitBoosted",
            ),
            (
                commands::GET_RUNNING_APPLICATION_PROCESS_ID_FOR_DEVELOP,
                None,
                "GetRunningApplicationProcessIdForDevelop",
            ),
            (
                commands::SET_CURRENT_APPLICATION_RIGHTS_ENVIRONMENT_CAN_BE_ACTIVE_FOR_DEVELOP,
                None,
                "SetCurrentApplicationRightsEnvironmentCanBeActiveForDevelop",
            ),
            (
                commands::CREATE_APPLICATION_RESOURCE_FOR_DEVELOP,
                None,
                "CreateApplicationResourceForDevelop",
            ),
            (
                commands::IS_PREOMIA_FOR_DEVELOP,
                None,
                "IsPreomiaForDevelop",
            ),
            (
                commands::GET_APPLICATION_PROGRAM_ID_FROM_HOST,
                None,
                "GetApplicationProgramIdFromHost",
            ),
            (
                commands::REFRESH_CACHED_DEBUG_VALUES,
                None,
                "RefreshCachedDebugValues",
            ),
            (
                commands::PREPARE_LAUNCH_APPLICATION_FROM_HOST,
                None,
                "PrepareLaunchApplicationFromHost",
            ),
            (commands::GET_LAUNCH_EVENT, None, "GetLaunchEvent"),
            (commands::GET_LAUNCH_RESULT, None, "GetLaunchResult"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IDevelopInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ns::IDevelopInterface"
    }
}

impl ServiceFramework for IDevelopInterface {
    fn get_service_name(&self) -> &str {
        "ns::IDevelopInterface"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
