// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/am.h
//! Port of zuyu/src/core/hle/service/am/am.cpp
//!
//! Entry point for the AM service. Registers "appletAE" and "appletOE"
//! named services.

use std::sync::{Arc, Mutex};

use crate::hle::service::hle_ipc::{SessionRequestHandlerFactory, SessionRequestHandlerPtr};
use crate::hle::service::server_manager::ServerManager;
use crate::hle::service::sm::sm::ServiceManager;

/// Launches AM services.
///
/// Matches upstream `void AM::LoopProcess(Core::System& system)`.
/// In the C++ version, this creates a WindowSystem, ButtonPoller,
/// EventObserver, and registers the two named services.
pub fn loop_process(service_manager: &Arc<Mutex<ServiceManager>>) {
    let mut server_manager = ServerManager::new(crate::core::SystemRef::null());

    let factory: SessionRequestHandlerFactory = Box::new(|| -> SessionRequestHandlerPtr {
        Arc::new(super::service::application_proxy_service::IApplicationProxyService::new())
    });
    server_manager.register_named_service("appletOE", factory, 64);

    // TODO: upstream also registers "appletAE"
    // server_manager.register_named_service("appletAE", ...);

    ServerManager::run_server(server_manager);
}
