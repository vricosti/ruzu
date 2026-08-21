// SPDX-FileCopyrightText: Copyright 2026 Eden Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of Eden `src/core/hle/service/wlan/wlan.{h,cpp}`.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
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
    ILocalManager,
    "wlan:lcl",
    [
        (0, "OpenMasterMode"),
        (0, "OpenMode_2"),
        (1, "CloseMasterMode"),
        (1, "CloseMode_2"),
        (2, "OpenClientMode"),
        (2, "GetMacAddress_2"),
        (3, "CloseClientMode"),
        (3, "CreateBss"),
        (4, "OpenSpectatorMode"),
        (4, "DestroyBss"),
        (5, "CloseSpectatorMode"),
        (5, "StartScan_2"),
        (6, "GetMacAddress_2"),
        (6, "StopScan_2"),
        (7, "CreateBss"),
        (7, "Connect_2"),
        (8, "DestroyBss"),
        (8, "CancelConnect_2"),
        (9, "StartScan_2"),
        (9, "Join"),
        (10, "StopScan_2"),
        (10, "CancelJoin"),
        (11, "Connect_2"),
        (11, "Disconnect_2"),
        (12, "CancelConnect_2"),
        (12, "SetBeaconLostCount"),
        (13, "Join"),
        (13, "GetSystemEvent_2"),
        (14, "CancelJoin"),
        (14, "GetConnectionStatus_2"),
        (15, "Disconnect_2"),
        (15, "GetClientStatus"),
        (16, "SetBeaconLostCount"),
        (16, "GetBssIndicationEvent"),
        (17, "GetSystemEvent_2"),
        (17, "GetBssIndicationInfo"),
        (18, "GetConnectionStatus_2"),
        (18, "GetState_2"),
        (19, "GetClientStatus"),
        (19, "GetAllowedChannels"),
        (20, "GetBssIndicationEvent"),
        (20, "AddIe"),
        (21, "GetBssIndicationInfo"),
        (21, "DeleteIe"),
        (22, "GetState_2"),
        (22, "PutFrameRaw"),
        (23, "GetAllowedChannels"),
        (23, "CancelGetFrame"),
        (24, "AddIe"),
        (24, "CreateRxEntry"),
        (25, "DeleteIe"),
        (25, "DeleteRxEntry"),
        (26, "PutFrameRaw"),
        (26, "AddEthertypeToRxEntry"),
        (27, "CancelGetFrame"),
        (27, "DeleteEthertypeFromRxEntry"),
        (28, "CreateRxEntry"),
        (28, "AddMatchingDataToRxEntry"),
        (29, "DeleteRxEntry"),
        (29, "RemoveMatchingDataFromRxEntry"),
        (30, "AddEthertypeToRxEntry"),
        (30, "GetScanResult_2"),
        (31, "DeleteEthertypeFromRxEntry"),
        (31, "PutActionFrameOneShot"),
        (32, "AddMatchingDataToRxEntry"),
        (32, "SetActionFrameWithBeacon"),
        (33, "RemoveMatchingDataFromRxEntry"),
        (33, "CancelActionFrameWithBeacon"),
        (34, "GetScanResult_2"),
        (34, "CreateRxEntryForActionFrame"),
        (35, "PutActionFrameOneShot"),
        (35, "DeleteRxEntryForActionFrame"),
        (36, "SetActionFrameWithBeacon"),
        (36, "AddSubtypeToRxEntryForActionFrame"),
        (37, "CancelActionFrameWithBeacon"),
        (37, "DeleteSubtypeFromRxEntryForActionFrame"),
        (38, "CreateRxEntryForActionFrame"),
        (38, "CancelGetActionFrame"),
        (39, "DeleteRxEntryForActionFrame"),
        (39, "GetRssi_2"),
        (40, "AddSubtypeToRxEntryForActionFrame"),
        (40, "SetMaxAssociationNumber"),
        (41, "DeleteSubtypeFromRxEntryForActionFrame"),
        (41, "Cmd41"),
        (42, "CancelGetActionFrame"),
        (42, "Cmd42"),
        (43, "GetRssi_2"),
        (43, "Cmd43"),
        (44, "SetMaxAssociationNumber"),
        (45, "OpenLcsMasterMode"),
        (46, "CloseLcsMasterMode"),
        (47, "OpenLcsClientMode"),
        (48, "CloseLcsClientMode"),
        (49, "GetChannelStats"),
        (50, "Cmd50"),
        (51, "Cmd51"),
        (52, "Cmd52"),
    ]
);
define_stub_service!(ILocalGetFrame, "wlan:lg", [(0, "GetFrameRaw")]);
define_stub_service!(ILocalGetActionFrame, "wlan:lga", [(0, "GetActionFrame")]);
define_stub_service!(ISocketGetFrame, "wlan:sg", [(0, "GetFrameRaw")]);
define_stub_service!(
    ISocketManager,
    "wlan:soc",
    [
        (0, "PutFrameRaw_2"),
        (1, "CancelGetFrame_2"),
        (2, "CreateRxEntry_2"),
        (3, "DeleteRxEntry_2"),
        (4, "AddEthertypeToRxEntry_2"),
        (5, "DeleteEthertypeFromRxEntry_2"),
        (6, "GetMacAddress_3"),
        (7, "SwitchTsfTimerFunction"),
        (8, "GetDeltaTimeBetweenSystemAndTsf"),
        (9, "RegisterSharedMemory"),
        (10, "UnregisterSharedMemory"),
        (11, "EnableSharedMemory"),
        (12, "SetMulticastFilter")
    ]
);
define_stub_service!(
    IDetectManager,
    "wlan:dtc",
    [
        (0, "Cmd0"),
        (1, "Cmd1"),
        (2, "Cmd2"),
        (3, "Cmd3"),
        (4, "Cmd4"),
        (5, "Cmd5"),
        (6, "Cmd6"),
        (7, "Cmd7"),
        (8, "Cmd8"),
        (9, "Cmd9"),
        (10, "Cmd10"),
        (11, "Cmd11"),
        (12, "Cmd12"),
        (13, "Cmd13"),
        (14, "Cmd14"),
        (15, "Cmd15"),
        (16, "Cmd16"),
        (17, "Cmd17"),
        (18, "Cmd18"),
        (19, "Cmd19"),
        (20, "Cmd20"),
        (21, "Cmd21"),
        (22, "Cmd22"),
        (23, "Cmd23"),
        (24, "Cmd24"),
        (25, "Cmd25"),
        (26, "Cmd26"),
        (27, "Cmd27")
    ]
);
define_stub_service!(
    IPrivateServiceCreator,
    "wlan:p",
    [
        (0, "CreateWirelessCommunicationService"),
        (1, "CreatePrivateWirelessCommunicationService")
    ]
);
define_stub_service!(
    ISfDriverServiceCreator,
    "wlan:nd",
    [(0, "CreateDriverService")]
);

pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;
    let server_manager = ServerManager::new_shared(system);
    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "wlan:lcl",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(ILocalManager::new()) }),
            64,
        );
        server_manager.register_named_service(
            "wlan:lg",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(ILocalGetFrame::new()) }),
            64,
        );
        server_manager.register_named_service(
            "wlan:lga",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(ILocalGetActionFrame::new())
            }),
            64,
        );
        server_manager.register_named_service(
            "wlan:sg",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(ISocketGetFrame::new())
            }),
            64,
        );
        server_manager.register_named_service(
            "wlan:soc",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(ISocketManager::new()) }),
            64,
        );
        server_manager.register_named_service(
            "wlan:dtc",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(IDetectManager::new()) }),
            64,
        );
        server_manager.register_named_service(
            "wlan:p",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IPrivateServiceCreator::new())
            }),
            64,
        );
        server_manager.register_named_service(
            "wlan:nd",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(ISfDriverServiceCreator::new())
            }),
            64,
        );
    }
    ServerManager::run_server_shared(server_manager);
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn service_tables_match_upstream() {
        assert_eq!(ILocalManager::new().handlers().len(), 53);
        assert_eq!(ISocketManager::new().handlers().len(), 13);
        assert_eq!(IDetectManager::new().handlers().len(), 28);
    }
}
