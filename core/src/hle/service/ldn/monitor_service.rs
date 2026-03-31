// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/ldn/monitor_service.h
//! Port of zuyu/src/core/hle/service/ldn/monitor_service.cpp
//!
//! IMonitorService: LDN monitoring service.

use super::ldn_types::State;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

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
}

impl IMonitorService {
    pub fn new() -> Self {
        Self { state: State::None }
    }

    /// Cmd 0: GetStateForMonitor
    pub fn get_state_for_monitor(&self) -> (ResultCode, State) {
        log::warn!("(STUBBED) IMonitorService::get_state_for_monitor called");
        (RESULT_SUCCESS, State::None)
    }

    /// Cmd 100: InitializeMonitor
    pub fn initialize_monitor(&mut self) -> ResultCode {
        log::info!("IMonitorService::initialize_monitor called");
        RESULT_SUCCESS
    }

    /// Cmd 101: FinalizeMonitor
    pub fn finalize_monitor(&mut self) -> ResultCode {
        log::info!("IMonitorService::finalize_monitor called");
        RESULT_SUCCESS
    }
}
