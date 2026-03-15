// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pctl/pctl.h
//! Port of zuyu/src/core/hle/service/pctl/pctl.cpp
//!
//! Parental controls service registration.

use std::sync::{Arc, Mutex};

use crate::hle::service::hle_ipc::{SessionRequestHandlerFactory, SessionRequestHandlerPtr};
use crate::hle::service::sm::sm::ServiceManager;

/// LoopProcess — registers "pctl", "pctl:a", "pctl:s", "pctl:r" services.
///
/// Corresponds to `Service::PCTL::LoopProcess` in upstream pctl.cpp.
pub fn loop_process(service_manager: &Arc<Mutex<ServiceManager>>) {
    register_services(service_manager);
}

pub fn register_services(service_manager: &Arc<Mutex<ServiceManager>>) {
    register_named_service(
        service_manager,
        "pctl",
        super::pctl_types::Capability::APPLICATION
            | super::pctl_types::Capability::SNS_POST
            | super::pctl_types::Capability::STATUS
            | super::pctl_types::Capability::STEREO_VISION,
    );
    register_named_service(
        service_manager,
        "pctl:a",
        super::pctl_types::Capability::NONE,
    );
    register_named_service(
        service_manager,
        "pctl:r",
        super::pctl_types::Capability::NONE,
    );
    register_named_service(
        service_manager,
        "pctl:s",
        super::pctl_types::Capability::NONE,
    );
}

fn register_named_service(
    service_manager: &Arc<Mutex<ServiceManager>>,
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

    let result = service_manager
        .lock()
        .unwrap()
        .register_service(name.to_string(), 64, factory);
    if result.is_error() {
        log::warn!(
            "Failed to register service '{}': {:#x}",
            name,
            result.get_inner_value()
        );
    }
}
