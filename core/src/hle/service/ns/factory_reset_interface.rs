// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/factory_reset_interface.h
//! Port of zuyu/src/core/hle/service/ns/factory_reset_interface.cpp
//!
//! IFactoryResetInterface — factory reset operations for NS.

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
    pub const REQUEST_RESET_TO_FACTORY_SETTINGS_WITH_PLATFORM_REGION_AUTHENTICATION_SECURELY: u32 = 106;
}

/// IFactoryResetInterface.
///
/// Corresponds to `IFactoryResetInterface` in upstream.
pub struct IFactoryResetInterface;

impl IFactoryResetInterface {
    pub fn new() -> Self {
        Self
    }
}
