// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/application_version_interface.h
//! Port of zuyu/src/core/hle/service/ns/application_version_interface.cpp
//!
//! IApplicationVersionInterface — application version management for NS.

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
pub struct IApplicationVersionInterface;

impl IApplicationVersionInterface {
    pub fn new() -> Self {
        Self
    }
}
