// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/ptm/ts.h
//! Port of zuyu/src/core/hle/service/ptm/ts.cpp
//!
//! TS service ("ts") and ISession -- temperature sensor service.

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// Temperature sensor locations.
///
/// Corresponds to `Location` enum in upstream ts.cpp.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Internal = 0,
    External = 1,
}

/// IPC command IDs for TS service.
///
/// Corresponds to the function table in `TS` constructor (upstream ts.cpp).
pub mod commands {
    pub const GET_TEMPERATURE_RANGE: u32 = 0;
    pub const GET_TEMPERATURE: u32 = 1;
    pub const SET_MEASUREMENT_MODE: u32 = 2;
    pub const GET_TEMPERATURE_MILLI_C: u32 = 3;
    pub const OPEN_SESSION: u32 = 4;
}

/// IPC command IDs for ISession.
///
/// Corresponds to the function table in `ISession` constructor (upstream ts.cpp).
pub mod session_commands {
    pub const GET_TEMPERATURE_RANGE: u32 = 0;
    pub const SET_MEASUREMENT_MODE: u32 = 2;
    pub const GET_TEMPERATURE: u32 = 4;
}

/// TS service ("ts").
///
/// Corresponds to `TS` in upstream ts.h / ts.cpp.
pub struct TS {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl TS {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (commands::GET_TEMPERATURE_RANGE, None, "GetTemperatureRange"),
            (commands::GET_TEMPERATURE, Some(TS::get_temperature_handler), "GetTemperature"),
            (commands::SET_MEASUREMENT_MODE, None, "SetMeasurementMode"),
            (commands::GET_TEMPERATURE_MILLI_C, Some(TS::get_temperature_milli_c_handler), "GetTemperatureMilliC"),
            (commands::OPEN_SESSION, Some(TS::open_session_handler), "OpenSession"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// GetTemperature (cmd 1).
    ///
    /// Corresponds to `TS::GetTemperature` in upstream ts.cpp.
    /// Returns 35 degrees for internal, 20 for external.
    pub fn get_temperature(&self, location: Location) -> i32 {
        match location {
            Location::Internal => 35,
            Location::External => 20,
        }
    }

    /// GetTemperatureMilliC (cmd 3).
    ///
    /// Corresponds to `TS::GetTemperatureMilliC` in upstream ts.cpp.
    /// Returns 35000 mC for internal, 20000 for external.
    pub fn get_temperature_milli_c(&self, location: Location) -> i32 {
        match location {
            Location::Internal => 35000,
            Location::External => 20000,
        }
    }

    /// OpenSession (cmd 4).
    ///
    /// Corresponds to `TS::OpenSession` in upstream ts.cpp.
    pub fn open_session(&self, _device_code: u32) -> ISession {
        ISession::new()
    }

    // --- Handler bridge functions ---

    fn get_temperature_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const TS) };
        let mut rp = RequestParser::new(ctx);
        let location_raw = rp.pop_u32() as u8;
        let location = if location_raw == 0 { Location::Internal } else { Location::External };
        let temp = service.get_temperature(location);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_i32(temp);
    }

    fn get_temperature_milli_c_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const TS) };
        let mut rp = RequestParser::new(ctx);
        let location_raw = rp.pop_u32() as u8;
        let location = if location_raw == 0 { Location::Internal } else { Location::External };
        let temp = service.get_temperature_milli_c(location);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_i32(temp);
    }

    fn open_session_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const TS) };
        let mut rp = RequestParser::new(ctx);
        let device_code = rp.pop_u32();
        let session = Arc::new(service.open_session(device_code));
        let handle = ctx.create_session_for_service(session).unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_move_objects(handle);
    }
}

impl SessionRequestHandler for TS {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ts"
    }
}

impl ServiceFramework for TS {
    fn get_service_name(&self) -> &str {
        "ts"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// ISession -- per-device temperature query interface.
///
/// Corresponds to `ISession` in upstream ts.cpp.
pub struct ISession {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISession {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (session_commands::GET_TEMPERATURE_RANGE, None, "GetTemperatureRange"),
            (session_commands::SET_MEASUREMENT_MODE, None, "SetMeasurementMode"),
            (session_commands::GET_TEMPERATURE, Some(ISession::get_temperature_handler), "GetTemperature"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// GetTemperature (cmd 4).
    ///
    /// Corresponds to `ISession::GetTemperature` in upstream ts.cpp.
    /// Returns a constant 35 degrees.
    pub fn get_temperature(&self) -> f32 {
        35.0
    }

    // --- Handler bridge functions ---

    fn get_temperature_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISession) };
        let temp = service.get_temperature();

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(temp);
    }
}

impl SessionRequestHandler for ISession {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ts::ISession"
    }
}

impl ServiceFramework for ISession {
    fn get_service_name(&self) -> &str {
        "ts::ISession"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
