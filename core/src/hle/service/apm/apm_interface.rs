// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/apm/apm_interface.h
//! Port of zuyu/src/core/hle/service/apm/apm_interface.cpp
//!
//! APM and APM_Sys service interfaces, and ISession.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use super::apm::Module;
use super::apm_controller::{Controller, CpuBoostMode, PerformanceConfiguration, PerformanceMode};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for APM
pub mod apm_commands {
    pub const OPEN_SESSION: u32 = 0;
    pub const GET_PERFORMANCE_MODE: u32 = 1;
    pub const IS_CPU_OVERCLOCK_ENABLED: u32 = 6;
}

/// IPC command IDs for APM_Sys
pub mod apm_sys_commands {
    pub const REQUEST_PERFORMANCE_MODE: u32 = 0;
    pub const GET_PERFORMANCE_EVENT: u32 = 1;
    pub const GET_THROTTLING_STATE: u32 = 2;
    pub const GET_LAST_THROTTLING_STATE: u32 = 3;
    pub const CLEAR_LAST_THROTTLING_STATE: u32 = 4;
    pub const LOAD_AND_APPLY_SETTINGS: u32 = 5;
    pub const SET_CPU_BOOST_MODE: u32 = 6;
    pub const GET_CURRENT_PERFORMANCE_CONFIGURATION: u32 = 7;
}

/// IPC command IDs for ISession
pub mod session_commands {
    pub const SET_PERFORMANCE_CONFIGURATION: u32 = 0;
    pub const GET_PERFORMANCE_CONFIGURATION: u32 = 1;
    pub const SET_CPU_OVERCLOCK_ENABLED: u32 = 2;
}

/// ISession interface.
///
/// Corresponds to `ISession` in upstream `apm_interface.cpp`.
pub struct ISession {
    controller: Arc<Mutex<Controller>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISession {
    pub fn new(controller: Arc<Mutex<Controller>>) -> Self {
        let handlers = build_handler_map(&[
            (
                session_commands::SET_PERFORMANCE_CONFIGURATION,
                Some(ISession::set_performance_configuration_handler),
                "SetPerformanceConfiguration",
            ),
            (
                session_commands::GET_PERFORMANCE_CONFIGURATION,
                Some(ISession::get_performance_configuration_handler),
                "GetPerformanceConfiguration",
            ),
            (
                session_commands::SET_CPU_OVERCLOCK_ENABLED,
                Some(ISession::set_cpu_overclock_enabled_handler),
                "SetCpuOverclockEnabled",
            ),
        ]);

        Self {
            controller,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn set_performance_configuration(
        &self,
        mode: PerformanceMode,
        config: PerformanceConfiguration,
    ) {
        log::debug!(
            "ISession::set_performance_configuration called, mode={:?}, config={:?}",
            mode,
            config
        );
        self.controller
            .lock()
            .unwrap()
            .set_performance_configuration(mode, config);
    }

    pub fn get_performance_configuration(&self, mode: PerformanceMode) -> PerformanceConfiguration {
        log::debug!(
            "ISession::get_performance_configuration called, mode={:?}",
            mode
        );
        self.controller
            .lock()
            .unwrap()
            .get_current_performance_configuration(mode)
    }

    pub fn set_cpu_overclock_enabled(&self, enabled: bool) {
        log::warn!(
            "(STUBBED) ISession::set_cpu_overclock_enabled called, enabled={}",
            enabled
        );
    }

    fn set_performance_configuration_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let session = unsafe { &*(this as *const dyn ServiceFramework as *const ISession) };
        let mut rp = RequestParser::new(ctx);
        let mode = match rp.pop_u32() {
            0 => PerformanceMode::Normal,
            1 => PerformanceMode::Boost,
            _ => PerformanceMode::Normal,
        };
        let config = match rp.pop_u32() {
            0 => PerformanceConfiguration::Config1,
            1 => PerformanceConfiguration::Config2,
            2 => PerformanceConfiguration::Config3,
            3 => PerformanceConfiguration::Config4,
            _ => PerformanceConfiguration::Config1,
        };
        session.set_performance_configuration(mode, config);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_performance_configuration_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let session = unsafe { &*(this as *const dyn ServiceFramework as *const ISession) };
        let mut rp = RequestParser::new(ctx);
        let mode = match rp.pop_u32() {
            0 => PerformanceMode::Normal,
            1 => PerformanceMode::Boost,
            _ => PerformanceMode::Normal,
        };
        let config = session.get_performance_configuration(mode);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(config as u32);
    }

    fn set_cpu_overclock_enabled_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let session = unsafe { &*(this as *const dyn ServiceFramework as *const ISession) };
        let mut rp = RequestParser::new(ctx);
        session.set_cpu_overclock_enabled(rp.pop_bool());

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for ISession {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "apm::ISession"
    }
}

impl ServiceFramework for ISession {
    fn get_service_name(&self) -> &str {
        "apm::ISession"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// APM service ("apm", "apm:am").
///
/// Corresponds to `APM` class in upstream `apm_interface.h`.
pub struct APM {
    module: Arc<Module>,
    controller: Arc<Mutex<Controller>>,
    name: String,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl APM {
    pub fn new(module: Arc<Module>, controller: Arc<Mutex<Controller>>, name: &str) -> Self {
        let handlers = build_handler_map(&[
            (
                apm_commands::OPEN_SESSION,
                Some(APM::open_session_handler),
                "OpenSession",
            ),
            (
                apm_commands::GET_PERFORMANCE_MODE,
                Some(APM::get_performance_mode_handler),
                "GetPerformanceMode",
            ),
            (
                apm_commands::IS_CPU_OVERCLOCK_ENABLED,
                Some(APM::is_cpu_overclock_enabled_handler),
                "IsCpuOverclockEnabled",
            ),
        ]);
        Self {
            module,
            controller,
            name: name.to_string(),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn open_session(&self) -> ISession {
        log::debug!("APM({})::open_session called", self.name);
        ISession::new(self.controller.clone())
    }

    pub fn get_performance_mode(&self) -> PerformanceMode {
        log::debug!("APM({})::get_performance_mode called", self.name);
        self.controller
            .lock()
            .unwrap()
            .get_current_performance_mode()
    }

    pub fn is_cpu_overclock_enabled(&self) -> bool {
        log::warn!(
            "(STUBBED) APM({})::is_cpu_overclock_enabled called",
            self.name
        );
        false
    }

    fn open_session_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let apm = unsafe { &*(this as *const dyn ServiceFramework as *const APM) };
        let session = Arc::new(apm.open_session());
        let handle = ctx.create_session_for_service(session).unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_move_objects(handle);
    }

    fn get_performance_mode_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let apm = unsafe { &*(this as *const dyn ServiceFramework as *const APM) };
        let mode = apm.get_performance_mode();

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(mode as u32);
    }

    fn is_cpu_overclock_enabled_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let apm = unsafe { &*(this as *const dyn ServiceFramework as *const APM) };
        let enabled = apm.is_cpu_overclock_enabled();

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(enabled);
    }
}

impl SessionRequestHandler for APM {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        &self.name
    }
}

impl ServiceFramework for APM {
    fn get_service_name(&self) -> &str {
        &self.name
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// APM_Sys service ("apm:sys").
///
/// Corresponds to `APM_Sys` class in upstream `apm_interface.h`.
pub struct ApmSys {
    controller: Arc<Mutex<Controller>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ApmSys {
    pub fn new(controller: Arc<Mutex<Controller>>) -> Self {
        let handlers = build_handler_map(&[
            (0, None, "RequestPerformanceMode"),
            (
                apm_sys_commands::GET_PERFORMANCE_EVENT,
                Some(ApmSys::get_performance_event_handler),
                "GetPerformanceEvent",
            ),
            (2, None, "GetThrottlingState"),
            (3, None, "GetLastThrottlingState"),
            (4, None, "ClearLastThrottlingState"),
            (5, None, "LoadAndApplySettings"),
            (
                apm_sys_commands::SET_CPU_BOOST_MODE,
                Some(ApmSys::set_cpu_boost_mode_handler),
                "SetCpuBoostMode",
            ),
            (
                apm_sys_commands::GET_CURRENT_PERFORMANCE_CONFIGURATION,
                Some(ApmSys::get_current_performance_configuration_handler),
                "GetCurrentPerformanceConfiguration",
            ),
        ]);

        Self {
            controller,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn get_performance_event(&self) -> ISession {
        log::debug!("ApmSys::get_performance_event called");
        ISession::new(self.controller.clone())
    }

    pub fn set_cpu_boost_mode(&self, mode: CpuBoostMode) {
        log::debug!("ApmSys::set_cpu_boost_mode called, mode={:?}", mode);
        self.controller
            .lock()
            .unwrap()
            .set_from_cpu_boost_mode(mode);
    }

    pub fn get_current_performance_configuration(&self) -> PerformanceConfiguration {
        log::debug!("ApmSys::get_current_performance_configuration called");
        let mut ctrl = self.controller.lock().unwrap();
        let mode = ctrl.get_current_performance_mode();
        ctrl.get_current_performance_configuration(mode)
    }

    fn get_performance_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ApmSys) };
        let session = Arc::new(service.get_performance_event());
        let handle = ctx.create_session_for_service(session).unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_move_objects(handle);
    }

    fn set_cpu_boost_mode_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ApmSys) };
        let mut rp = RequestParser::new(ctx);
        let mode = match rp.pop_u32() {
            0 => CpuBoostMode::Normal,
            1 => CpuBoostMode::FastLoad,
            2 => CpuBoostMode::Partial,
            _ => CpuBoostMode::Normal,
        };
        service.set_cpu_boost_mode(mode);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_current_performance_configuration_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ApmSys) };
        let config = service.get_current_performance_configuration();

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(config as u32);
    }
}

impl SessionRequestHandler for ApmSys {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "apm:sys"
    }
}

impl ServiceFramework for ApmSys {
    fn get_service_name(&self) -> &str {
        "apm:sys"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
