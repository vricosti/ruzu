// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/ldn/monitor_service.h
//! Port of zuyu/src/core/hle/service/ldn/monitor_service.cpp
//!
//! IMonitorService: LDN monitoring service.

use std::collections::BTreeMap;

use super::ldn_types::State;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IMonitorService.
///
/// | Cmd | Handler              | Name                          |
/// |-----|---------------------|-------------------------------|
/// | 0   | GetStateForMonitor  | GetStateForMonitor            |
/// | 1   | nullptr             | GetNetworkInfoForMonitor      |
/// | 2   | nullptr             | GetIpv4AddressForMonitor      |
/// | 3   | nullptr             | GetDisconnectReasonForMonitor |
/// | 4   | nullptr             | GetSecurityParameterForMonitor|
/// | 5   | nullptr             | GetNetworkConfigForMonitor    |
/// | 100 | InitializeMonitor   | InitializeMonitor             |
/// | 101 | FinalizeMonitor     | FinalizeMonitor               |
pub struct IMonitorService {
    state: State,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IMonitorService {
    pub fn new() -> Self {
        Self {
            state: State::None,
            handlers: build_handler_map(&[
                (
                    0,
                    Some(Self::get_state_for_monitor_handler),
                    "GetStateForMonitor",
                ),
                (1, None, "GetNetworkInfoForMonitor"),
                (2, None, "GetIpv4AddressForMonitor"),
                (3, None, "GetDisconnectReasonForMonitor"),
                (4, None, "GetSecurityParameterForMonitor"),
                (5, None, "GetNetworkConfigForMonitor"),
                (
                    100,
                    Some(Self::initialize_monitor_handler),
                    "InitializeMonitor",
                ),
                (101, Some(Self::finalize_monitor_handler), "FinalizeMonitor"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Cmd 0: GetStateForMonitor
    pub fn get_state_for_monitor(&self) -> (ResultCode, State) {
        log::warn!("(STUBBED) IMonitorService::get_state_for_monitor called");
        (RESULT_SUCCESS, State::None)
    }

    /// Cmd 100: InitializeMonitor
    pub fn initialize_monitor(&self) -> ResultCode {
        log::info!("IMonitorService::initialize_monitor called");
        RESULT_SUCCESS
    }

    /// Cmd 101: FinalizeMonitor
    pub fn finalize_monitor(&self) -> ResultCode {
        log::info!("IMonitorService::finalize_monitor called");
        RESULT_SUCCESS
    }

    fn get_state_for_monitor_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IMonitorService) };
        let (result, state) = service.get_state_for_monitor();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(result);
        rb.push_u32(state as u32);
    }

    fn initialize_monitor_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IMonitorService) };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(service.initialize_monitor());
    }

    fn finalize_monitor_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IMonitorService) };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(service.finalize_monitor());
    }
}

impl SessionRequestHandler for IMonitorService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IMonitorService"
    }
}

impl ServiceFramework for IMonitorService {
    fn get_service_name(&self) -> &str {
        "IMonitorService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
