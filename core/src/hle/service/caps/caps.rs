// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps.h
//! Port of zuyu/src/core/hle/service/caps/caps.cpp
//!
//! Screenshot/album service registration.

/// LoopProcess — registers "caps:a", "caps:c", "caps:u", "caps:ss", "caps:sc", "caps:su".
///
/// Corresponds to `Service::Capture::LoopProcess` in upstream caps.cpp.
pub fn loop_process(system: crate::core::SystemRef) {
    use super::caps_manager::AlbumManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;
    use std::sync::{Arc, Mutex};

    log::debug!("Capture::LoopProcess called");

    let mut server_manager = ServerManager::new(system);
    let album_manager = Arc::new(Mutex::new(AlbumManager::new()));

    // caps:a -> IAlbumAccessorService
    let mgr = Arc::clone(&album_manager);
    server_manager.register_named_service(
        "caps:a",
        Box::new(move || -> SessionRequestHandlerPtr {
            Arc::new(super::caps_a::IAlbumAccessorService::new(Arc::clone(&mgr)))
        }),
        64,
    );

    // caps:c -> IAlbumControlService
    let mgr = Arc::clone(&album_manager);
    server_manager.register_named_service(
        "caps:c",
        Box::new(move || -> SessionRequestHandlerPtr {
            Arc::new(super::caps_c::IAlbumControlService::new(Arc::clone(&mgr)))
        }),
        64,
    );

    // caps:u -> IAlbumApplicationService
    let mgr = Arc::clone(&album_manager);
    server_manager.register_named_service(
        "caps:u",
        Box::new(move || -> SessionRequestHandlerPtr {
            Arc::new(super::caps_u::IAlbumApplicationService::new(Arc::clone(
                &mgr,
            )))
        }),
        64,
    );

    // caps:ss -> IScreenShotService
    let mgr = Arc::clone(&album_manager);
    server_manager.register_named_service(
        "caps:ss",
        Box::new(move || -> SessionRequestHandlerPtr {
            Arc::new(super::caps_ss::IScreenShotService::new(Arc::clone(&mgr)))
        }),
        64,
    );

    // caps:sc -> IScreenShotControlService
    server_manager.register_named_service(
        "caps:sc",
        Box::new(|| -> SessionRequestHandlerPtr {
            Arc::new(super::caps_sc::IScreenShotControlService::new())
        }),
        64,
    );

    // caps:su -> IScreenShotApplicationService
    let mgr = Arc::clone(&album_manager);
    server_manager.register_named_service(
        "caps:su",
        Box::new(move || -> SessionRequestHandlerPtr {
            Arc::new(super::caps_su::IScreenShotApplicationService::new(
                Arc::clone(&mgr),
            ))
        }),
        64,
    );

    ServerManager::run_server(server_manager);
}
