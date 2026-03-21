// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm_debug.h
//! Port of zuyu/src/core/hle/service/btm/btm_debug.cpp
//!
//! IBtmDebug — "btm:dbg".

use std::collections::BTreeMap;
use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IBtmDebug.
pub struct IBtmDebug {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IBtmDebug {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, None, "AcquireDiscoveryEvent"),
            (1, None, "StartDiscovery"),
            (2, None, "CancelDiscovery"),
            (3, None, "GetDeviceProperty"),
            (4, None, "CreateBond"),
            (5, None, "CancelBond"),
            (6, None, "SetTsiMode"),
            (7, None, "GeneralTest"),
            (8, None, "HidConnect"),
            (9, None, "GeneralGet"),
            (10, None, "GetGattClientDisconnectionReason"),
            (11, None, "GetBleConnectionParameter"),
            (12, None, "GetBleConnectionParameterRequest"),
            (13, None, "Unknown13"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IBtmDebug {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str { "btm:dbg" }
}

impl ServiceFramework for IBtmDebug {
    fn get_service_name(&self) -> &str { "btm:dbg" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}
