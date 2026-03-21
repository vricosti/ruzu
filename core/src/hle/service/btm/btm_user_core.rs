// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm_user_core.h
//! Port of zuyu/src/core/hle/service/btm/btm_user_core.cpp
//!
//! IBtmUserCore — BLE user core interface.

use std::collections::BTreeMap;
use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IBtmUserCore.
pub struct IBtmUserCore {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    // TODO: service_context, scan_event, connection_event, etc.
}

impl IBtmUserCore {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, None, "AcquireBleScanEvent"),
            (1, None, "GetBleScanFilterParameter"),
            (2, None, "GetBleScanFilterParameter2"),
            (3, None, "StartBleScanForGeneral"),
            (4, None, "StopBleScanForGeneral"),
            (5, None, "GetBleScanResultsForGeneral"),
            (6, None, "StartBleScanForPaired"),
            (7, None, "StopBleScanForPaired"),
            (8, None, "StartBleScanForSmartDevice"),
            (9, None, "StopBleScanForSmartDevice"),
            (10, None, "GetBleScanResultsForSmartDevice"),
            (17, None, "AcquireBleConnectionEvent"),
            (18, None, "BleConnect"),
            (19, None, "BleDisconnect"),
            (20, None, "BleGetConnectionState"),
            (21, None, "AcquireBlePairingEvent"),
            (22, None, "BlePairDevice"),
            (23, None, "BleUnPairDevice"),
            (24, None, "BleUnPairDevice2"),
            (25, None, "BleGetPairedDevices"),
            (26, None, "AcquireBleServiceDiscoveryEvent"),
            (27, None, "GetGattServices"),
            (28, None, "GetGattService"),
            (29, None, "GetGattIncludedServices"),
            (30, None, "GetBelongingGattService"),
            (31, None, "GetGattCharacteristics"),
            (32, None, "GetGattDescriptors"),
            (33, None, "AcquireBleMtuConfigEvent"),
            (34, None, "ConfigureBleMtu"),
            (35, None, "GetBleMtu"),
            (36, None, "RegisterBleGattDataPath"),
            (37, None, "UnregisterBleGattDataPath"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IBtmUserCore {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str { "IBtmUserCore" }
}

impl ServiceFramework for IBtmUserCore {
    fn get_service_name(&self) -> &str { "IBtmUserCore" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}
