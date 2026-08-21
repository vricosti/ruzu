// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ptm/ptm.h
//! Port of zuyu/src/core/hle/service/ptm/ptm.cpp
//!
//! PTM service registration — registers psm, ts services.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

macro_rules! define_stub_service {
    ($type:ident, $service:literal, [$(($id:expr, $command:literal)),* $(,)?]) => {
        pub struct $type { handlers: BTreeMap<u32, FunctionInfo>, handlers_tipc: BTreeMap<u32, FunctionInfo> }
        impl $type { pub fn new() -> Self { Self { handlers: build_handler_map(&[$(($id, None, $command)),*]), handlers_tipc: BTreeMap::new() } } }
        impl SessionRequestHandler for $type {
            fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode { ServiceFramework::handle_sync_request_impl(self, ctx) }
            fn service_name(&self) -> &str { $service }
        }
        impl ServiceFramework for $type {
            fn get_service_name(&self) -> &str { $service }
            fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
            fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
        }
    };
}

define_stub_service!(
    PsmManu,
    "psm:manu",
    [
        (0, "EnableVdd50StateControl"),
        (1, "DisableVdd50StateControl"),
        (2, "SetVdd50State")
    ]
);
define_stub_service!(Powctl, "powctl", [(0, "OpenSession")]);

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
        // Upstream deliberately uses `if (1 /* not retail */)` here.
        server_manager.register_named_service(
            "psm:manu",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(PsmManu::new()) }),
            16,
        );
        server_manager.register_named_service(
            "powctl",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(Powctl::new()) }),
            16,
        );
    }

    ServerManager::run_server_shared(server_manager);
}
