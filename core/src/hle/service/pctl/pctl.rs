// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pctl/pctl.h
//! Port of zuyu/src/core/hle/service/pctl/pctl.cpp
//!
//! Parental controls service registration.

use std::sync::{Arc, Mutex};

use crate::hle::service::hle_ipc::{SessionRequestHandlerFactory, SessionRequestHandlerPtr};
use crate::hle::service::server_manager::ServerManager;
use crate::hle::service::sm::sm::ServiceManager;

/// Launches PCTL services.
///
/// Matches upstream `void PCTL::LoopProcess(Core::System& system)`:
/// ```cpp
/// void LoopProcess(Core::System& system) {
///     auto server_manager = std::make_unique<ServerManager>(system);
///     server_manager->RegisterNamedService("pctl", ...);
///     server_manager->RegisterNamedService("pctl:a", ...);
///     server_manager->RegisterNamedService("pctl:r", ...);
///     server_manager->RegisterNamedService("pctl:s", ...);
///     ServerManager::RunServer(std::move(server_manager));
/// }
/// ```
pub fn loop_process(service_manager: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
    let mut server_manager = ServerManager::new(system);

    register_named_service(
        &mut server_manager,
        "pctl",
        super::pctl_types::Capability::APPLICATION
            | super::pctl_types::Capability::SNS_POST
            | super::pctl_types::Capability::STATUS
            | super::pctl_types::Capability::STEREO_VISION,
    );
    register_named_service(
        &mut server_manager,
        "pctl:a",
        super::pctl_types::Capability::NONE,
    );
    register_named_service(
        &mut server_manager,
        "pctl:r",
        super::pctl_types::Capability::NONE,
    );
    register_named_service(
        &mut server_manager,
        "pctl:s",
        super::pctl_types::Capability::NONE,
    );

    ServerManager::run_server(server_manager);
}

fn register_named_service(
    server_manager: &mut ServerManager,
    name: &str,
    capability: super::pctl_types::Capability,
) {
    let name_owned = name.to_string();
    let factory: SessionRequestHandlerFactory = Box::new(move || -> SessionRequestHandlerPtr {
        Arc::new(
            super::parental_control_service_factory::IParentalControlServiceFactory::new(
                &name_owned,
                capability,
            ),
        )
    });
    server_manager.register_named_service(name, factory, 64);
}
