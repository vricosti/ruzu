// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/psc.cpp/.h
//!
//! LoopProcess registers the following named services:
//!   psc:c     -> IPmControl
//!   psc:m     -> IPmService
//!   ovln:rcv  -> IReceiverService
//!   ovln:snd  -> ISenderService
//!   time:m    -> Time::ServiceManager
//!   time:su   -> Time::StaticService
//!   time:al   -> Time::IAlarmService

pub const PSC_SERVICE_NAMES: &[&str] = &[
    "psc:c", "psc:m", "ovln:rcv", "ovln:snd", "time:m", "time:su", "time:al",
];

/// Register all PSC services.
///
/// Corresponds to upstream `PSC::LoopProcess` in `psc.cpp`.
pub fn loop_process(
    system: crate::core::SystemRef,
    device_memory: *const crate::device_memory::DeviceMemory,
    memory_manager: *mut crate::hle::kernel::k_memory_manager::KMemoryManager,
) {
    use std::sync::Arc;

    use crate::hle::service::hle_ipc::SessionRequestHandler;
    use crate::hle::service::server_manager::ServerManager;

    let server_manager = ServerManager::new_shared(system.clone());
    {
        let mut server_manager = server_manager.lock().unwrap();

        let stub = |sm: &mut ServerManager, name: &str| {
            let svc_name = name.to_string();
            sm.register_named_service(
                name,
                Box::new(move || -> Arc<dyn SessionRequestHandler> {
                    Arc::new(crate::hle::service::services::GenericStubService::new(
                        &svc_name,
                    ))
                }),
                64,
            );
        };

        stub(&mut server_manager, "psc:c");
        stub(&mut server_manager, "psc:m");
        stub(&mut server_manager, "ovln:rcv");
        stub(&mut server_manager, "ovln:snd");

        let time_sm: Arc<dyn SessionRequestHandler> = Arc::new(
            crate::hle::service::psc::time::service_manager::TimeServiceManager::new(
                system,
                device_memory,
                memory_manager,
            ),
        );
        let time_sm_factory = {
            let shared = Arc::clone(&time_sm);
            Box::new(move || -> Arc<dyn SessionRequestHandler> { Arc::clone(&shared) })
        };
        server_manager.register_named_service("time:m", time_sm_factory, 64);

        stub(&mut server_manager, "time:su");
        stub(&mut server_manager, "time:al");
    }

    ServerManager::run_server_shared(server_manager);
}
