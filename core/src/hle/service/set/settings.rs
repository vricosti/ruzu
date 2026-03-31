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
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;
    use std::sync::Arc;

    let mut server_manager = ServerManager::new(system);

    // "set" -> ISettingsServer
    server_manager.register_named_service(
        "set",
        Box::new(|| -> SessionRequestHandlerPtr { Arc::new(ISettingsServer::new()) }),
        64,
    );

    // "set:cal" -> IFactorySettingsServer
    server_manager.register_named_service(
        "set:cal",
        Box::new(|| -> SessionRequestHandlerPtr { Arc::new(IFactorySettingsServer::new()) }),
        64,
    );

    // "set:fd" -> IFirmwareDebugSettingsServer
    server_manager.register_named_service(
        "set:fd",
        Box::new(|| -> SessionRequestHandlerPtr { Arc::new(IFirmwareDebugSettingsServer::new()) }),
        64,
    );

    // "set:sys" -> ISystemSettingsServer (via SystemSettingsService wrapper)
    server_manager.register_named_service(
        "set:sys",
        Box::new(|| -> SessionRequestHandlerPtr {
            Arc::new(super::system_settings_server::SystemSettingsService::new())
        }),
        64,
    );

    ServerManager::run_server(server_manager);
}
