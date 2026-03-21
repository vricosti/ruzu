// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm.h
//! Port of zuyu/src/core/hle/service/btm/btm.cpp
//!
//! Bluetooth management service registration and IBtm.

use std::collections::BTreeMap;
use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IBtm — main Bluetooth management service.
pub struct IBtm {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IBtm {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, None, "GetState"),
            (1, None, "GetHostDeviceProperty"),
            (2, None, "AcquireDeviceConditionEvent"),
            (3, None, "GetDeviceCondition"),
            (4, None, "SetBurstMode"),
            (5, None, "SetSlotMode"),
            (6, None, "SetBluetoothMode"),
            (7, None, "SetWlanMode"),
            (8, None, "AcquireDeviceInfoEvent"),
            (9, None, "GetDeviceInfo"),
            (10, None, "AddDeviceInfo"),
            (11, None, "RemoveDeviceInfo"),
            (12, None, "IncreaseDeviceInfoOrder"),
            (13, None, "LlrNotify"),
            (14, None, "EnableRadio"),
            (15, None, "DisableRadio"),
            (16, None, "HidDisconnect"),
            (17, None, "HidSetRetransmissionMode"),
            (18, None, "AcquireAwakeReqEvent"),
            (19, None, "AcquireLlrStateEvent"),
            (20, None, "IsLlrStarted"),
            (21, None, "EnableSlotSaving"),
            (22, None, "ProtectDeviceInfo"),
            (23, None, "AcquireBleScanEvent"),
            (24, None, "GetBleScanParameterGeneral"),
            (25, None, "GetBleScanParameterSmartDevice"),
            (26, None, "StartBleScanForGeneral"),
            (27, None, "StopBleScanForGeneral"),
            (28, None, "GetBleScanResultsForGeneral"),
            (29, None, "StartBleScanForPairedDevice"),
            (30, None, "StopBleScanForPairedDevice"),
            (31, None, "StartBleScanForSmartDevice"),
            (32, None, "StopBleScanForSmartDevice"),
            (33, None, "GetBleScanResultsForSmartDevice"),
            (34, None, "AcquireBleConnectionEvent"),
            (35, None, "BleConnect"),
            (36, None, "BleOverrideConnection"),
            (37, None, "BleDisconnect"),
            (38, None, "BleGetConnectionState"),
            (39, None, "BleGetGattClientConditionList"),
            (40, None, "AcquireBlePairingEvent"),
            (41, None, "BlePairDevice"),
            (42, None, "BleUnpairDeviceOnBoth"),
            (43, None, "BleUnpairDevice"),
            (44, None, "BleGetPairedAddresses"),
            (45, None, "AcquireBleServiceDiscoveryEvent"),
            (46, None, "GetGattServices"),
            (47, None, "GetGattService"),
            (48, None, "GetGattIncludedServices"),
            (49, None, "GetBelongingService"),
            (50, None, "GetGattCharacteristics"),
            (51, None, "GetGattDescriptors"),
            (52, None, "AcquireBleMtuConfigEvent"),
            (53, None, "ConfigureBleMtu"),
            (54, None, "GetBleMtu"),
            (55, None, "RegisterBleGattDataPath"),
            (56, None, "UnregisterBleGattDataPath"),
            (57, None, "RegisterAppletResourceUserId"),
            (58, None, "UnregisterAppletResourceUserId"),
            (59, None, "SetAppletResourceUserId"),
            (60, None, "Unknown60"),
            (61, None, "Unknown61"),
            (62, None, "Unknown62"),
            (63, None, "Unknown63"),
            (64, None, "Unknown64"),
            (65, None, "Unknown65"),
            (66, None, "Unknown66"),
            (67, None, "Unknown67"),
            (68, None, "Unknown68"),
            (69, None, "Unknown69"),
            (70, None, "Unknown70"),
            (71, None, "Unknown71"),
            (72, None, "Unknown72"),
            (73, None, "Unknown73"),
            (74, None, "Unknown74"),
            (75, None, "Unknown75"),
            (76, None, "Unknown76"),
            (100, None, "Unknown100"),
            (101, None, "Unknown101"),
            (110, None, "Unknown110"),
            (111, None, "Unknown111"),
            (112, None, "Unknown112"),
            (113, None, "Unknown113"),
            (114, None, "Unknown114"),
            (115, None, "Unknown115"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IBtm {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str { "btm" }
}

impl ServiceFramework for IBtm {
    fn get_service_name(&self) -> &str { "btm" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

/// LoopProcess — registers "btm", "btm:dbg", "btm:sys", "btm:u".
pub fn loop_process() {
    log::debug!("BTM::LoopProcess called");
    // TODO: Register btm services with ServerManager
}
