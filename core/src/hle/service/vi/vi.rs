// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/vi.cpp/.h
//!
//! VI service registration: creates a shared Container and registers
//! vi:m, vi:s, vi:u root services.

use std::sync::Arc;

use crate::hle::service::hle_ipc::SessionRequestHandler;

use super::application_root_service::IApplicationRootService;
use super::container::Container;
use super::manager_root_service::IManagerRootService;
use super::system_root_service::ISystemRootService;

pub const VI_SERVICE_NAMES: &[&str] = &["vi:m", "vi:s", "vi:u"];

// -- Registration --

/// Launches VI services.
///
/// Matches upstream `void VI::LoopProcess(Core::System& system, std::stop_token token)`.
/// Creates a shared Container, then registers vi:m, vi:s, vi:u.
pub fn loop_process(system: crate::core::SystemRef) {
    let container = Container::new(system);

    let server_manager = crate::hle::service::server_manager::ServerManager::new_shared(system);

    {
        let mut server_manager = server_manager.lock().unwrap();

        // vi:m — IManagerRootService (cmd 2 = GetDisplayService)
        let container_m = Arc::clone(&container);
        server_manager.register_named_service(
            "vi:m",
            Box::new(move || -> Arc<dyn SessionRequestHandler> {
                Arc::new(IManagerRootService::new(Arc::clone(&container_m)))
            }),
            64,
        );

        // vi:s — ISystemRootService (cmd 1 = GetDisplayService)
        let container_s = Arc::clone(&container);
        server_manager.register_named_service(
            "vi:s",
            Box::new(move || -> Arc<dyn SessionRequestHandler> {
                Arc::new(ISystemRootService::new(Arc::clone(&container_s)))
            }),
            64,
        );

        // vi:u — IApplicationRootService (cmd 0 = GetDisplayService)
        let container_u = Arc::clone(&container);
        server_manager.register_named_service(
            "vi:u",
            Box::new(move || -> Arc<dyn SessionRequestHandler> {
                Arc::new(IApplicationRootService::new(Arc::clone(&container_u)))
            }),
            64,
        );
    }

    crate::hle::service::server_manager::ServerManager::run_server_shared(server_manager);
}

/// Backward-compatible alias.
pub fn register_services(system: crate::core::SystemRef) {
    loop_process(system);
}
