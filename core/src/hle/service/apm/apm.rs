// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/apm/apm.h
//! Port of zuyu/src/core/hle/service/apm/apm.cpp
//!
//! APM Module and LoopProcess.

use std::sync::{Arc, Mutex};

use crate::hle::service::hle_ipc::{SessionRequestHandlerFactory, SessionRequestHandlerPtr};
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

/// Registers "apm", "apm:am", "apm:sys" services.
///
/// Corresponds to `LoopProcess` in upstream `apm.cpp`.
pub fn loop_process(service_manager: &Arc<Mutex<ServiceManager>>) {
    register_services(service_manager);
}

pub fn register_services(service_manager: &Arc<Mutex<ServiceManager>>) {
    let module = Arc::new(Module::new());
    let controller = Arc::new(Mutex::new(super::apm_controller::Controller::new()));

    register_named_service(service_manager, "apm", {
        let module = module.clone();
        let controller = controller.clone();
        move || -> SessionRequestHandlerPtr {
            Arc::new(super::apm_interface::APM::new(module.clone(), controller.clone(), "apm"))
        }
    });

    register_named_service(service_manager, "apm:am", {
        let module = module.clone();
        let controller = controller.clone();
        move || -> SessionRequestHandlerPtr {
            Arc::new(super::apm_interface::APM::new(
                module.clone(),
                controller.clone(),
                "apm:am",
            ))
        }
    });

    register_named_service(service_manager, "apm:sys", {
        let controller = controller.clone();
        move || -> SessionRequestHandlerPtr {
            Arc::new(super::apm_interface::ApmSys::new(controller.clone()))
        }
    });
}

fn register_named_service<F>(
    service_manager: &Arc<Mutex<ServiceManager>>,
    name: &str,
    factory: F,
) where
    F: Fn() -> SessionRequestHandlerPtr + Send + Sync + 'static,
{
    let boxed: SessionRequestHandlerFactory = Box::new(factory);
    let result = service_manager
        .lock()
        .unwrap()
        .register_service(name.to_string(), 64, boxed);
    if result.is_error() {
        log::warn!(
            "Failed to register service '{}': {:#x}",
            name,
            result.get_inner_value()
        );
    }
}
