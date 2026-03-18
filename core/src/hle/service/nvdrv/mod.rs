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

/// Register nvdrv services with the service manager.
pub fn register_services(service_manager: &Arc<Mutex<ServiceManager>>) {
    let module = Module::new();
    for name in NVDRV_SERVICE_NAMES {
        let svc_name = name.to_string();
        let module = Arc::clone(&module);
        let result = service_manager.lock().unwrap().register_service(
            svc_name.clone(),
            64,
            Box::new(move || {
                Arc::new(NvdrvService::new(Arc::clone(&module), &svc_name))
            }),
        );
        if result.is_error() {
            log::warn!("Failed to register NVDRV service '{}'", name);
        }
    }
}
