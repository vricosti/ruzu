// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm_user_core.h
//! Port of zuyu/src/core/hle/service/btm/btm_user_core.cpp
//!
//! IBtmUserCore — BLE user core interface.

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::kernel_helpers::ServiceContext;
use crate::hle::service::os::event::Event;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use std::collections::BTreeMap;
use std::sync::Arc;

/// IBtmUserCore.
///
/// Upstream fields:
/// - `service_context`: provides kernel event creation/destruction
/// - `scan_event`: KEvent signaled on BLE scan results
/// - `connection_event`: KEvent signaled on BLE connection state changes
/// - `service_discovery_event`: KEvent signaled on BLE service discovery completion
/// - `config_event`: KEvent signaled on BLE MTU configuration changes
pub struct IBtmUserCore {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    service_context: ServiceContext,
    scan_event: Arc<Event>,
    connection_event: Arc<Event>,
    service_discovery_event: Arc<Event>,
    config_event: Arc<Event>,
}

impl IBtmUserCore {
    pub fn new() -> Self {
        let mut service_context = ServiceContext::new("IBtmUserCore".to_string());

        let scan_handle = service_context.create_event("IBtmUserCore:ScanEvent".to_string());
        let scan_event = service_context.get_event(scan_handle).unwrap();

        let conn_handle = service_context.create_event("IBtmUserCore:ConnectionEvent".to_string());
        let connection_event = service_context.get_event(conn_handle).unwrap();

        let disc_handle = service_context.create_event("IBtmUserCore:DiscoveryEvent".to_string());
        let service_discovery_event = service_context.get_event(disc_handle).unwrap();

        let cfg_handle = service_context.create_event("IBtmUserCore:ConfigEvent".to_string());
        let config_event = service_context.get_event(cfg_handle).unwrap();

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
            service_context,
            scan_event,
            connection_event,
            service_discovery_event,
            config_event,
        }
    }
}

impl SessionRequestHandler for IBtmUserCore {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "IBtmUserCore"
    }
}

impl ServiceFramework for IBtmUserCore {
    fn get_service_name(&self) -> &str {
        "IBtmUserCore"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
