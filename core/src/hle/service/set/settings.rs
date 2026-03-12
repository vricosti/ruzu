// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/set/settings.h and settings.cpp
//!
//! Entry point for the settings service module.
//!
//! Registers the following named services:
//! - "set"     -> ISettingsServer
//! - "set:cal" -> IFactorySettingsServer
//! - "set:fd"  -> IFirmwareDebugSettingsServer
//! - "set:sys" -> ISystemSettingsServer

use super::factory_settings_server::IFactorySettingsServer;
use super::firmware_debug_settings_server::IFirmwareDebugSettingsServer;
use super::settings_server::ISettingsServer;
use super::system_settings_server::ISystemSettingsServer;

/// Registers "set", "set:cal", "set:fd", "set:sys" services.
///
/// Corresponds to `Set::LoopProcess` in upstream settings.cpp.
pub fn loop_process() {
    // Create service instances (matching upstream LoopProcess).
    let _settings_server = ISettingsServer::new();
    let _factory_settings_server = IFactorySettingsServer::new();
    let _firmware_debug_settings_server = IFirmwareDebugSettingsServer::new();
    let _system_settings_server = ISystemSettingsServer::new();

    // TODO: Register with ServerManager when server infrastructure is available:
    // server_manager.register_named_service("set", settings_server);
    // server_manager.register_named_service("set:cal", factory_settings_server);
    // server_manager.register_named_service("set:fd", firmware_debug_settings_server);
    // server_manager.register_named_service("set:sys", system_settings_server);
    // server_manager.run_server();
}
