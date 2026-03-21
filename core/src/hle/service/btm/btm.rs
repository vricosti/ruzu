// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm.h
//! Port of zuyu/src/core/hle/service/btm/btm.cpp
//!
//! Bluetooth management service registration and IBtm.

use std::collections::BTreeMap;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IBtm — main Bluetooth management service.
pub struct IBtm {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IBtm {
    /// Stub handler for all nullptr entries -- logs STUBBED and returns RESULT_SUCCESS.
    fn stub_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        log::warn!("(STUBBED) IBtm command {}", cmd);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    pub fn new() -> Self {
        let s = Some(Self::stub_handler as fn(&dyn ServiceFramework, &mut HLERequestContext));
        let handlers = build_handler_map(&[
            (0, s, "GetState"),
            (1, s, "GetHostDeviceProperty"),
            (2, s, "AcquireDeviceConditionEvent"),
            (3, s, "GetDeviceCondition"),
            (4, s, "SetBurstMode"),
            (5, s, "SetSlotMode"),
            (6, s, "SetBluetoothMode"),
            (7, s, "SetWlanMode"),
            (8, s, "AcquireDeviceInfoEvent"),
            (9, s, "GetDeviceInfo"),
            (10, s, "AddDeviceInfo"),
            (11, s, "RemoveDeviceInfo"),
            (12, s, "IncreaseDeviceInfoOrder"),
            (13, s, "LlrNotify"),
            (14, s, "EnableRadio"),
            (15, s, "DisableRadio"),
            (16, s, "HidDisconnect"),
            (17, s, "HidSetRetransmissionMode"),
            (18, s, "AcquireAwakeReqEvent"),
            (19, s, "AcquireLlrStateEvent"),
            (20, s, "IsLlrStarted"),
            (21, s, "EnableSlotSaving"),
            (22, s, "ProtectDeviceInfo"),
            (23, s, "AcquireBleScanEvent"),
            (24, s, "GetBleScanParameterGeneral"),
            (25, s, "GetBleScanParameterSmartDevice"),
            (26, s, "StartBleScanForGeneral"),
            (27, s, "StopBleScanForGeneral"),
            (28, s, "GetBleScanResultsForGeneral"),
            (29, s, "StartBleScanForPairedDevice"),
            (30, s, "StopBleScanForPairedDevice"),
            (31, s, "StartBleScanForSmartDevice"),
            (32, s, "StopBleScanForSmartDevice"),
            (33, s, "GetBleScanResultsForSmartDevice"),
            (34, s, "AcquireBleConnectionEvent"),
            (35, s, "BleConnect"),
            (36, s, "BleOverrideConnection"),
            (37, s, "BleDisconnect"),
            (38, s, "BleGetConnectionState"),
            (39, s, "BleGetGattClientConditionList"),
            (40, s, "AcquireBlePairingEvent"),
            (41, s, "BlePairDevice"),
            (42, s, "BleUnpairDeviceOnBoth"),
            (43, s, "BleUnpairDevice"),
            (44, s, "BleGetPairedAddresses"),
            (45, s, "AcquireBleServiceDiscoveryEvent"),
            (46, s, "GetGattServices"),
            (47, s, "GetGattService"),
            (48, s, "GetGattIncludedServices"),
            (49, s, "GetBelongingService"),
            (50, s, "GetGattCharacteristics"),
            (51, s, "GetGattDescriptors"),
            (52, s, "AcquireBleMtuConfigEvent"),
            (53, s, "ConfigureBleMtu"),
            (54, s, "GetBleMtu"),
            (55, s, "RegisterBleGattDataPath"),
            (56, s, "UnregisterBleGattDataPath"),
            (57, s, "RegisterAppletResourceUserId"),
            (58, s, "UnregisterAppletResourceUserId"),
            (59, s, "SetAppletResourceUserId"),
            (60, s, "Unknown60"),
            (61, s, "Unknown61"),
            (62, s, "Unknown62"),
            (63, s, "Unknown63"),
            (64, s, "Unknown64"),
            (65, s, "Unknown65"),
            (66, s, "Unknown66"),
            (67, s, "Unknown67"),
            (68, s, "Unknown68"),
            (69, s, "Unknown69"),
            (70, s, "Unknown70"),
            (71, s, "Unknown71"),
            (72, s, "Unknown72"),
            (73, s, "Unknown73"),
            (74, s, "Unknown74"),
            (75, s, "Unknown75"),
            (76, s, "Unknown76"),
            (100, s, "Unknown100"),
            (101, s, "Unknown101"),
            (110, s, "Unknown110"),
            (111, s, "Unknown111"),
            (112, s, "Unknown112"),
            (113, s, "Unknown113"),
            (114, s, "Unknown114"),
            (115, s, "Unknown115"),
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
