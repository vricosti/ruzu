// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ptm/ptm.h
//! Port of zuyu/src/core/hle/service/ptm/ptm.cpp
//!
//! PTM service registration — registers psm, ts services.

/// LoopProcess — registers "psm" and "ts" services.
///
/// Corresponds to `Service::PTM::LoopProcess` in upstream ptm.cpp.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    use super::psm::PSM;
    use super::ts::TS;

    let server_manager = ServerManager::new_shared(system);

    {
        let mut server_manager = server_manager.lock().unwrap();
        let psm_system = system.clone();
        server_manager.register_named_service(
            "psm",
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(PSM::new(psm_system.clone()))
            }),
            16,
        );
        server_manager.register_named_service(
            "ts",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(TS::new()) }),
            16,
        );
    }

    ServerManager::run_server_shared(server_manager);
}
