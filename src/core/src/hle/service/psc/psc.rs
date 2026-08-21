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

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

macro_rules! define_stub_service {
    ($type:ident, $service:literal, [$(($id:expr, $command:literal)),* $(,)?]) => {
        pub struct $type { handlers: BTreeMap<u32, FunctionInfo>, handlers_tipc: BTreeMap<u32, FunctionInfo> }
        impl $type {
            pub fn new() -> Self { Self { handlers: build_handler_map(&[$(($id, None, $command)),*]), handlers_tipc: BTreeMap::new() } }
        }
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
    PscL,
    "psc:l",
    [
        (0, "Initialize_3"),
        (1, "Lock"),
        (2, "Unlock"),
        (3, "IsLocked"),
        (4, "GetRelatedState")
    ]
);
define_stub_service!(
    InsR,
    "ins:r",
    [(0, "GetInputSourceState"), (1, "GetTriggerTargetEvent")]
);
define_stub_service!(InsS, "ins:s", [(0, "GetNotifyEvent")]);
define_stub_service!(
    HshlSys,
    "hshl:sys",
    [
        (0, "GetBatteryPercentage"),
        (1, "GetChargerType"),
        (2, "OpenChargeSession"),
        (3, "GetRawBatteryPercentage"),
        (4, "GetBatteryVoltageLevel"),
        (5, "OpenThermalSession"),
        (6, "GetAbnormalTemperatureSet"),
        (7, "OpenClockSession"),
        (8, "GetClockRate"),
        (9, "OpenBridgeSession"),
        (10, "GetBridgePowerSupply"),
        (11, "OpenVsysVoltageSession"),
        (12, "GetIsBatteryEnoughForFullAwake"),
        (13, "GetIsCharging"),
        (14, "Cmd14"),
        (15, "Cmd15")
    ]
);
define_stub_service!(
    HshlSet,
    "hshl:set",
    [
        (0, "OpenChargeSession_2"),
        (1, "OpenThermalSession_2"),
        (2, "SetClockRate"),
        (3, "SetBridgePowerSupply"),
        (4, "Cmd4"),
        (5, "Cmd5")
    ]
);

pub const PSC_SERVICE_NAMES: &[&str] = &[
    "psc:c", "psc:m", "psc:l", "ins:r", "ins:s", "hshl:sys", "hshl:set", "ovln:rcv", "ovln:snd",
    "time:m", "time:su", "time:al",
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
        server_manager.register_named_service(
            "psc:l",
            Box::new(|| -> Arc<dyn SessionRequestHandler> { Arc::new(PscL::new()) }),
            64,
        );
        server_manager.register_named_service(
            "ins:r",
            Box::new(|| -> Arc<dyn SessionRequestHandler> { Arc::new(InsR::new()) }),
            64,
        );
        server_manager.register_named_service(
            "ins:s",
            Box::new(|| -> Arc<dyn SessionRequestHandler> { Arc::new(InsS::new()) }),
            64,
        );
        server_manager.register_named_service(
            "hshl:sys",
            Box::new(|| -> Arc<dyn SessionRequestHandler> { Arc::new(HshlSys::new()) }),
            64,
        );
        server_manager.register_named_service(
            "hshl:set",
            Box::new(|| -> Arc<dyn SessionRequestHandler> { Arc::new(HshlSet::new()) }),
            64,
        );
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
