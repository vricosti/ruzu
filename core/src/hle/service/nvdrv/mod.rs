// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/

pub mod core;
pub mod devices;
pub mod nvdata;
pub mod nvdrv;
pub mod nvdrv_interface;
pub mod nvmemp;

use std::sync::{Arc, Mutex};

use crate::hle::service::sm::sm::ServiceManager;
use crate::hle::service::nvdrv::nvdrv::Module;
use crate::hle::service::nvdrv::nvdrv_interface::NvdrvService;

const NVDRV_SERVICE_NAMES: &[&str] = &["nvdrv", "nvdrv:a", "nvdrv:s", "nvdrv:t"];

/// Launches Nvidia services.
///
/// Matches upstream `void Nvidia::LoopProcess(Core::System& system)`.
pub fn loop_process(service_manager: &Arc<Mutex<ServiceManager>>) {
    let module = Module::new();
    let mut server_manager =
        crate::hle::service::server_manager::ServerManager::new(service_manager.clone());
    for name in NVDRV_SERVICE_NAMES {
        let svc_name = name.to_string();
        let module = Arc::clone(&module);
        server_manager.register_named_service(
            name,
            Box::new(move || {
                Arc::new(NvdrvService::new(Arc::clone(&module), &svc_name))
            }),
            64,
        );
    }
    crate::hle::service::server_manager::ServerManager::run_server(server_manager);
}

/// Backward-compatible alias.
pub fn register_services(service_manager: &Arc<Mutex<ServiceManager>>) {
    loop_process(service_manager);
}
