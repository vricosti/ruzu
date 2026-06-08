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
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let server_manager = ServerManager::new_shared(system);

    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            SERVICE_NAME_APPLICATION,
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(
                    crate::hle::service::olsc::olsc_service_for_application::IOlscServiceForApplication::new(),
                )
            }),
            16,
        );
        server_manager.register_named_service(
            SERVICE_NAME_SYSTEM,
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(
                    crate::hle::service::olsc::olsc_service_for_system_service::IOlscServiceForSystemService::new(system),
                )
            }),
            16,
        );
    }

    ServerManager::run_server_shared(server_manager);
}
