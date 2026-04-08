// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/vi.cpp/.h
//!
//! VI service registration: creates a shared Container and registers
//! vi:u, vi:s, vi:m root services.

use std::sync::{Arc, LazyLock, Mutex, Weak};

use crate::hle::service::hle_ipc::SessionRequestHandler;

use super::application_root_service::IApplicationRootService;
use super::container::Container;
use super::manager_root_service::IManagerRootService;
use super::system_root_service::ISystemRootService;

pub const VI_SERVICE_NAMES: &[&str] = &["vi:u", "vi:s", "vi:m"];

static SHARED_CONTAINER: LazyLock<Mutex<Option<Weak<Container>>>> =
    LazyLock::new(|| Mutex::new(None));

pub fn get_shared_container() -> Option<Arc<Container>> {
    SHARED_CONTAINER
        .lock()
        .unwrap()
        .as_ref()
        .and_then(Weak::upgrade)
}

// -- Registration --

/// Launches VI services.
///
/// Matches upstream `void VI::LoopProcess(Core::System& system, std::stop_token token)`.
/// Creates a shared Container, then registers vi:u, vi:s, vi:m.
pub fn loop_process(system: crate::core::SystemRef) {
    let container = Container::new(system);
    *SHARED_CONTAINER.lock().unwrap() = Some(Arc::downgrade(&container));

    let mut server_manager = crate::hle::service::server_manager::ServerManager::new(system);

    // vi:u — IApplicationRootService (cmd 0 = GetDisplayService)
    let container_u = Arc::clone(&container);
    server_manager.register_named_service(
        "vi:u",
        Box::new(move || -> Arc<dyn SessionRequestHandler> {
            Arc::new(IApplicationRootService::new(Arc::clone(&container_u)))
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

    // vi:m — IManagerRootService (cmd 2 = GetDisplayService)
    let container_m = Arc::clone(&container);
    server_manager.register_named_service(
        "vi:m",
        Box::new(move || -> Arc<dyn SessionRequestHandler> {
            Arc::new(IManagerRootService::new(Arc::clone(&container_m)))
        }),
        64,
    );

    crate::hle::service::server_manager::ServerManager::run_server(server_manager);
}

/// Backward-compatible alias.
pub fn register_services(system: crate::core::SystemRef) {
    loop_process(system);
}
