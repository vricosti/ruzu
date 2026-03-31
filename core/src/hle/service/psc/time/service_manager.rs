// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/service_manager.h/.cpp
//!
//! PSC::Time::ServiceManager — the "time:m" service.
//! Accepts SetupStandard*ClockCore calls from the Glue TimeManager during init.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// PSC::Time::ServiceManager — handles clock core setup.
pub struct TimeServiceManager {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    // Clock state is stored here; for now just track that setup was called
    steady_clock_setup: bool,
    local_clock_setup: bool,
    network_clock_setup: bool,
    user_clock_setup: bool,
    timezone_setup: bool,
    ephemeral_clock_setup: bool,
}

impl TimeServiceManager {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, Some(Self::stub_ok), "GetStaticServiceAsUser"),
                (5, Some(Self::stub_ok), "GetStaticServiceAsAdmin"),
                (6, Some(Self::stub_ok), "GetStaticServiceAsRepair"),
                (7, Some(Self::stub_ok), "GetStaticServiceAsServiceManager"),
                (
                    10,
                    Some(Self::setup_standard_steady_clock_core),
                    "SetupStandardSteadyClockCore",
                ),
                (
                    11,
                    Some(Self::setup_standard_local_system_clock_core),
                    "SetupStandardLocalSystemClockCore",
                ),
                (
                    12,
                    Some(Self::setup_standard_network_system_clock_core),
                    "SetupStandardNetworkSystemClockCore",
                ),
                (
                    13,
                    Some(Self::setup_standard_user_system_clock_core),
                    "SetupStandardUserSystemClockCore",
                ),
                (
                    14,
                    Some(Self::setup_time_zone_service_core),
                    "SetupTimeZoneServiceCore",
                ),
                (
                    15,
                    Some(Self::setup_ephemeral_network_system_clock_core),
                    "SetupEphemeralNetworkSystemClockCore",
                ),
                (
                    50,
                    Some(Self::stub_ok),
                    "GetStandardLocalClockOperationEvent",
                ),
                (
                    51,
                    Some(Self::stub_ok),
                    "GetStandardNetworkClockOperationEventForServiceManager",
                ),
                (
                    52,
                    Some(Self::stub_ok),
                    "GetEphemeralNetworkClockOperationEventForServiceManager",
                ),
                (
                    53,
                    Some(Self::stub_ok),
                    "GetStandardUserSystemClockAutomaticCorrectionUpdatedEvent",
                ),
                (60, Some(Self::stub_ok), "SetStandardSteadyClockBaseTime"),
                (200, Some(Self::stub_ok), "GetClosestAlarmUpdatedEvent"),
                (201, Some(Self::stub_ok), "CheckAndSignalAlarms"),
                (202, Some(Self::stub_ok), "GetClosestAlarmInfo"),
            ]),
            handlers_tipc: BTreeMap::new(),
            steady_clock_setup: false,
            local_clock_setup: false,
            network_clock_setup: false,
            user_clock_setup: false,
            timezone_setup: false,
            ephemeral_clock_setup: false,
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn setup_standard_steady_clock_core(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("PSC::Time::ServiceManager::SetupStandardSteadyClockCore called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn setup_standard_local_system_clock_core(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::info!("PSC::Time::ServiceManager::SetupStandardLocalSystemClockCore called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn setup_standard_network_system_clock_core(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::info!("PSC::Time::ServiceManager::SetupStandardNetworkSystemClockCore called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn setup_standard_user_system_clock_core(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::info!("PSC::Time::ServiceManager::SetupStandardUserSystemClockCore called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn setup_time_zone_service_core(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("PSC::Time::ServiceManager::SetupTimeZoneServiceCore called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn setup_ephemeral_network_system_clock_core(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::info!("PSC::Time::ServiceManager::SetupEphemeralNetworkSystemClockCore called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn stub_ok(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for TimeServiceManager {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        ServiceFramework::get_service_name(self)
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl ServiceFramework for TimeServiceManager {
    fn get_service_name(&self) -> &str {
        "time:m"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
