// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/olsc.h
//! Port of zuyu/src/core/hle/service/olsc/olsc.cpp
//!
//! LoopProcess: Registers "olsc:u" and "olsc:s" services.

/// Service names registered by OLSC.
pub const SERVICE_NAME_APPLICATION: &str = "olsc:u";
pub const SERVICE_NAME_SYSTEM: &str = "olsc:s";

/// Entry point for the OLSC service module.
///
/// Registers "olsc:u" and "olsc:s" services with a ServerManager.
///
/// Corresponds to `LoopProcess` in upstream `olsc.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;

    let mut server_manager = ServerManager::new(system);

    let stub_names = &[SERVICE_NAME_APPLICATION, SERVICE_NAME_SYSTEM];
    for &name in stub_names {
        let svc_name = name.to_string();
        server_manager.register_named_service(
            name,
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(crate::hle::service::services::GenericStubService::new(&svc_name))
            }),
            16,
        );
    }

    ServerManager::run_server(server_manager);
}
