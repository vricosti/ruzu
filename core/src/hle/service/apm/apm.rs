// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/apm/apm.h
//! Port of zuzu/src/core/hle/service/apm/apm.cpp
//!
//! APM Module and LoopProcess.

use std::sync::{Arc, Mutex};

use crate::hle::service::hle_ipc::{SessionRequestHandlerFactory, SessionRequestHandlerPtr};
use crate::hle::service::server_manager::ServerManager;
use crate::hle::service::sm::sm::ServiceManager;

/// APM Module.
///
/// Corresponds to `Module` class in upstream `apm.h`.
pub struct Module;

impl Module {
    pub fn new() -> Self {
        Self
    }
}

/// Launches APM services.
///
/// Matches upstream `void APM::LoopProcess(Core::System& system)`:
/// ```cpp
/// void LoopProcess(Core::System& system) {
///     auto module = std::make_shared<Module>();
///     auto server_manager = std::make_unique<ServerManager>(system);
///     server_manager->RegisterNamedService("apm", ...);
///     server_manager->RegisterNamedService("apm:am", ...);
///     server_manager->RegisterNamedService("apm:sys", ...);
///     ServerManager::RunServer(std::move(server_manager));
/// }
/// ```
pub fn loop_process(service_manager: &Arc<Mutex<ServiceManager>>) {
    let module = Arc::new(Module::new());
    let controller = Arc::new(Mutex::new(super::apm_controller::Controller::new()));
    let mut server_manager = ServerManager::new(service_manager.clone());

    let factory: SessionRequestHandlerFactory = {
        let module = module.clone();
        let controller = controller.clone();
        Box::new(move || -> SessionRequestHandlerPtr {
            Arc::new(super::apm_interface::APM::new(module.clone(), controller.clone(), "apm"))
        })
    };
    server_manager.register_named_service("apm", factory, 64);

    let factory: SessionRequestHandlerFactory = {
        let module = module.clone();
        let controller = controller.clone();
        Box::new(move || -> SessionRequestHandlerPtr {
            Arc::new(super::apm_interface::APM::new(
                module.clone(),
                controller.clone(),
                "apm:am",
            ))
        })
    };
    server_manager.register_named_service("apm:am", factory, 64);

    let factory: SessionRequestHandlerFactory = {
        let controller = controller.clone();
        Box::new(move || -> SessionRequestHandlerPtr {
            Arc::new(super::apm_interface::ApmSys::new(controller.clone()))
        })
    };
    server_manager.register_named_service("apm:sys", factory, 64);

    ServerManager::run_server(server_manager);
}
