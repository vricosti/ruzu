// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps.h
//! Port of zuyu/src/core/hle/service/caps/caps.cpp
//!
//! Screenshot/album service registration.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// Decoder control endpoint. Upstream intentionally gives the object the
/// internal service identity `grc:d` while registering it as `caps:dc`.
pub struct IDecoderControlService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IDecoderControlService {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (3001, None, "DecodeJpeg"),
                (4001, None, "ShrinkJpeg"),
                (4002, None, "ShrinkJpegEx"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IDecoderControlService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "grc:d"
    }
}

impl ServiceFramework for IDecoderControlService {
    fn get_service_name(&self) -> &str {
        "grc:d"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// LoopProcess — registers "caps:a", "caps:c", "caps:u", "caps:ss", "caps:sc", "caps:su".
///
/// Corresponds to `Service::Capture::LoopProcess` in upstream caps.cpp.
pub fn loop_process(system: crate::core::SystemRef) {
    use super::caps_manager::AlbumManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;
    use std::sync::{Arc, Mutex};

    log::debug!("Capture::LoopProcess called");

    let server_manager = ServerManager::new_shared(system);
    let album_manager = Arc::new(Mutex::new(AlbumManager::new()));

    {
        let mut server_manager = server_manager.lock().unwrap();

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

        let firmware_major =
            crate::hle::service::set::system_settings_server::get_firmware_version_impl(
                crate::hle::service::set::settings_types::GetFirmwareVersionType::Version1,
            )
            .major;
        if firmware_major >= 4 {
            server_manager.register_named_service(
                "caps:dc",
                Box::new(|| -> SessionRequestHandlerPtr {
                    Arc::new(IDecoderControlService::new())
                }),
                64,
            );
        }
    }

    ServerManager::run_server_shared(server_manager);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decoder_control_identity_and_commands_match_upstream() {
        let service = IDecoderControlService::new();
        assert_eq!(SessionRequestHandler::service_name(&service), "grc:d");
        assert_eq!(
            service.handlers().keys().copied().collect::<Vec<_>>(),
            [3001, 4001, 4002]
        );
    }
}
