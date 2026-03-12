// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/develop_interface.h
//! Port of zuyu/src/core/hle/service/ns/develop_interface.cpp
//!
//! IDevelopInterface — "ns:dev" service.

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
pub struct IDevelopInterface;

impl IDevelopInterface {
    pub fn new() -> Self {
        Self
    }
}
