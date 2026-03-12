// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/set/firmware_debug_settings_server.h and .cpp
//!
//! IFirmwareDebugSettingsServer service ("set:fd").

/// IPC command table for IFirmwareDebugSettingsServer ("set:fd").
///
/// Corresponds to the function table in upstream firmware_debug_settings_server.cpp.
/// All commands are unimplemented stubs in upstream (nullptr handlers).
pub mod commands {
    pub const SET_SETTINGS_ITEM_VALUE: u32 = 2;
    pub const RESET_SETTINGS_ITEM_VALUE: u32 = 3;
    pub const CREATE_SETTINGS_ITEM_KEY_ITERATOR: u32 = 4;
    pub const READ_SETTINGS: u32 = 10;
    pub const RESET_SETTINGS: u32 = 11;
    pub const SET_WEB_INSPECTOR_FLAG: u32 = 20;
    pub const SET_ALLOWED_SSL_HOSTS: u32 = 21;
    pub const SET_HOST_FS_MOUNT_POINT: u32 = 22;
    pub const SET_MEMORY_USAGE_RATE_FLAG: u32 = 23;
}

/// IFirmwareDebugSettingsServer — "set:fd" service.
///
/// Corresponds to `IFirmwareDebugSettingsServer` in upstream.
/// All IPC commands are unimplemented (nullptr) in upstream, matching here.
pub struct IFirmwareDebugSettingsServer;

impl IFirmwareDebugSettingsServer {
    pub fn new() -> Self {
        Self
    }

    /// SetSettingsItemValue (cmd 2) - not implemented upstream.
    pub fn set_settings_item_value(&self, _category: &[u8], _name: &[u8], _value: &[u8]) {
        log::warn!("IFirmwareDebugSettingsServer::SetSettingsItemValue not implemented");
    }

    /// ResetSettingsItemValue (cmd 3) - not implemented upstream.
    pub fn reset_settings_item_value(&self, _category: &[u8], _name: &[u8]) {
        log::warn!("IFirmwareDebugSettingsServer::ResetSettingsItemValue not implemented");
    }

    /// CreateSettingsItemKeyIterator (cmd 4) - not implemented upstream.
    pub fn create_settings_item_key_iterator(&self, _category: &[u8]) {
        log::warn!("IFirmwareDebugSettingsServer::CreateSettingsItemKeyIterator not implemented");
    }

    /// ReadSettings (cmd 10) - not implemented upstream.
    pub fn read_settings(&self) {
        log::warn!("IFirmwareDebugSettingsServer::ReadSettings not implemented");
    }

    /// ResetSettings (cmd 11) - not implemented upstream.
    pub fn reset_settings(&self) {
        log::warn!("IFirmwareDebugSettingsServer::ResetSettings not implemented");
    }

    /// SetWebInspectorFlag (cmd 20) - not implemented upstream.
    pub fn set_web_inspector_flag(&self, _flag: bool) {
        log::warn!("IFirmwareDebugSettingsServer::SetWebInspectorFlag not implemented");
    }

    /// SetAllowedSslHosts (cmd 21) - not implemented upstream.
    pub fn set_allowed_ssl_hosts(&self, _hosts: &[u8]) {
        log::warn!("IFirmwareDebugSettingsServer::SetAllowedSslHosts not implemented");
    }

    /// SetHostFsMountPoint (cmd 22) - not implemented upstream.
    pub fn set_host_fs_mount_point(&self, _path: &[u8]) {
        log::warn!("IFirmwareDebugSettingsServer::SetHostFsMountPoint not implemented");
    }

    /// SetMemoryUsageRateFlag (cmd 23) - not implemented upstream.
    pub fn set_memory_usage_rate_flag(&self, _flag: bool) {
        log::warn!("IFirmwareDebugSettingsServer::SetMemoryUsageRateFlag not implemented");
    }
}
