// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/am.h
//! Port of zuyu/src/core/hle/service/am/am.cpp
//!
//! Entry point for the AM service. Registers "appletAE" and "appletOE"
//! named services.

use std::sync::{Arc, Mutex};

use crate::hle::service::hle_ipc::{SessionRequestHandlerFactory, SessionRequestHandlerPtr};
use crate::hle::service::sm::sm::ServiceManager;

/// Port of Service::AM::LoopProcess
///
/// In the C++ version, this creates a WindowSystem, ButtonPoller,
/// EventObserver, and registers the two named services. Stubbed until
/// ServerManager and related infrastructure are wired.
pub fn loop_process(service_manager: &Arc<Mutex<ServiceManager>>) {
    register_services(service_manager);
}

pub fn register_services(service_manager: &Arc<Mutex<ServiceManager>>) {
    register_named_service(service_manager, "appletOE", || -> SessionRequestHandlerPtr {
        Arc::new(super::service::application_proxy_service::IApplicationProxyService::new())
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
