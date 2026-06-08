// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/omm/omm.h
//! Port of zuyu/src/core/hle/service/omm/omm.cpp
//!
//! OMM service registration. Registers:
//! - "idle:sys" -> IPolicyManagerSystem
//! - "omm" -> IOperationModeManager
//! - "spsm" -> IPowerStateInterface

/// Register all OMM services.
///
/// Corresponds to upstream `OMM::LoopProcess` in `omm.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    use super::operation_mode_manager::IOperationModeManager;
    use super::policy_manager_system::IPolicyManagerSystem;
    use super::power_state_interface::IPowerStateInterface;

    log::debug!("OMM::LoopProcess called");

    let server_manager = ServerManager::new_shared(system);
    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "idle:sys",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IPolicyManagerSystem::new())
            }),
            64,
        );
        server_manager.register_named_service(
            "omm",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IOperationModeManager::new())
            }),
            64,
        );
        server_manager.register_named_service(
            "spsm",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IPowerStateInterface::new())
            }),
            64,
        );
    }

    ServerManager::run_server_shared(server_manager);
}
