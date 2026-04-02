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

use super::event_observer::EventObserver;
use super::window_system::WindowSystem;

/// Launches AM services.
///
/// Matches upstream `void AM::LoopProcess(Core::System& system)`.
/// In the C++ version, this creates a WindowSystem, ButtonPoller,
/// EventObserver, and registers the two named services.
pub fn loop_process(service_manager: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
    let mut server_manager = ServerManager::new(system);

    // Create a shared WindowSystem, matching upstream which creates it on the stack
    // and passes references to both appletOE and appletAE.
    let window_system = Arc::new(Mutex::new(WindowSystem::new()));
    let window_system_ptr = {
        let mut guard = window_system.lock().unwrap();
        &mut *guard as *mut WindowSystem
    };
    let event_observer = Box::new(EventObserver::new(window_system_ptr as *const WindowSystem));
    {
        let mut guard = window_system.lock().unwrap();
        guard.set_event_observer(event_observer);
    }
    // Spawn set_window_system on a dedicated background thread so it can block on
    // the condition variable while this thread proceeds to run_server.
    // Matches upstream: AM::LoopProcess calls SetWindowSystem inside LoopProcess, which
    // runs on a guest kernel thread via RunOnGuestCoreProcess, allowing the setup thread
    // to call CreateAndInsertByFrontendAppletParameters concurrently.
    let ws_for_thread = window_system.clone();
    let system_for_thread = system;
    std::thread::Builder::new()
        .name("am:SetWindowSystem".to_string())
        .spawn(move || {
            system_for_thread
                .get()
                .get_applet_manager()
                .set_window_system(Some(ws_for_thread));
        })
        .expect("failed to spawn am:SetWindowSystem thread");

    let ws = window_system.clone();
    let system_oe = system;
    let factory_oe: SessionRequestHandlerFactory = Box::new(move || -> SessionRequestHandlerPtr {
        Arc::new(
            super::service::application_proxy_service::IApplicationProxyService::new(
                system_oe,
                ws.clone(),
            ),
        )
    });
    server_manager.register_named_service("appletOE", factory_oe, 64);

    let ws = window_system.clone();
    let system_ae = system;
    let factory_ae: SessionRequestHandlerFactory = Box::new(move || -> SessionRequestHandlerPtr {
        Arc::new(
            super::service::all_system_applet_proxies_service::IAllSystemAppletProxiesService::new(
                system_ae,
                ws.clone(),
            ),
        )
    });
    server_manager.register_named_service("appletAE", factory_ae, 64);

    ServerManager::run_server(server_manager);
}
