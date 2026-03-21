// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/ns.h
//! Port of zuyu/src/core/hle/service/ns/ns.cpp
//!
//! NS LoopProcess registers the following named services:
//!   ns:am2, ns:ec, ns:rid, ns:rt, ns:web, ns:ro -> IServiceGetterInterface
//!   ns:dev                                        -> IDevelopInterface
//!   ns:su                                         -> ISystemUpdateInterface
//!   ns:vm                                         -> IVulnerabilityManagerInterface
//!   pdm:qry                                       -> IQueryService
//!   pl:s, pl:u                                    -> IPlatformServiceManager

/// Service names registered by NS LoopProcess.
///
/// Corresponds to the registrations in upstream ns.cpp `LoopProcess`.
pub const NS_SERVICE_GETTER_NAMES: &[&str] =
    &["ns:am2", "ns:ec", "ns:rid", "ns:rt", "ns:web", "ns:ro"];

/// LoopProcess — registers all NS services.
///
/// Corresponds to `Service::NS::LoopProcess` in upstream ns.cpp.
pub fn loop_process() {
    use std::sync::Arc;
    use crate::hle::service::server_manager::ServerManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;

    log::debug!("NS::LoopProcess called");

    let mut server_manager = ServerManager::new(crate::core::SystemRef::null());

    // ns:am2, ns:ec, ns:rid, ns:rt, ns:web, ns:ro -> IServiceGetterInterface
    for &name in NS_SERVICE_GETTER_NAMES {
        server_manager.register_named_service(
            name,
            Box::new(|| -> SessionRequestHandlerPtr {
                Arc::new(super::service_getter_interface::IServiceGetterInterface::new())
            }),
            64,
        );
    }

    // ns:dev -> IDevelopInterface
    server_manager.register_named_service(
        "ns:dev",
        Box::new(|| -> SessionRequestHandlerPtr {
            Arc::new(super::develop_interface::IDevelopInterface::new())
        }),
        64,
    );

    // ns:su -> ISystemUpdateInterface
    server_manager.register_named_service(
        "ns:su",
        Box::new(|| -> SessionRequestHandlerPtr {
            Arc::new(super::system_update_interface::ISystemUpdateInterface::new())
        }),
        64,
    );

    // ns:vm -> IVulnerabilityManagerInterface
    server_manager.register_named_service(
        "ns:vm",
        Box::new(|| -> SessionRequestHandlerPtr {
            Arc::new(super::vulnerability_manager_interface::IVulnerabilityManagerInterface::new())
        }),
        64,
    );

    // pdm:qry -> IQueryService (stub — no SessionRequestHandler impl yet)
    {
        let svc_name = "pdm:qry".to_string();
        server_manager.register_named_service(
            "pdm:qry",
            Box::new(move || -> SessionRequestHandlerPtr {
                Arc::new(crate::hle::service::services::GenericStubService::new(&svc_name))
            }),
            64,
        );
    }

    // pl:u -> IPlatformServiceManager
    {
        let pl_u: Arc<dyn crate::hle::service::hle_ipc::SessionRequestHandler> =
            Arc::new(super::platform_service_manager::IPlatformServiceManager::new());
        let pl_u_clone = Arc::clone(&pl_u);
        server_manager.register_named_service(
            "pl:u",
            Box::new(move || pl_u_clone.clone()),
            64,
        );
    }

    // pl:s -> IPlatformServiceManager
    {
        let pl_s: Arc<dyn crate::hle::service::hle_ipc::SessionRequestHandler> =
            Arc::new(super::platform_service_manager::IPlatformServiceManager::new());
        let pl_s_clone = Arc::clone(&pl_s);
        server_manager.register_named_service(
            "pl:s",
            Box::new(move || pl_s_clone.clone()),
            64,
        );
    }

    ServerManager::run_server(server_manager);
}
