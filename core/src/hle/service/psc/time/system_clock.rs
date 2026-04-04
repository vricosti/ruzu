// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/system_clock.h
//! Port of zuyu/src/core/hle/service/psc/time/system_clock.cpp
//!
//! ISystemClock: provides system clock time queries and modifications.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use super::common::SystemClockContext;
use super::errors::{RESULT_CLOCK_UNINITIALIZED, RESULT_FAILED, RESULT_PERMISSION_DENIED};
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for ISystemClock.
///
/// Corresponds to the function table in upstream system_clock.cpp constructor.
pub mod commands {
    pub const GET_CURRENT_TIME: u32 = 0;
    pub const SET_CURRENT_TIME: u32 = 1;
    pub const GET_SYSTEM_CLOCK_CONTEXT: u32 = 2;
    pub const SET_SYSTEM_CLOCK_CONTEXT: u32 = 3;
    pub const GET_OPERATION_EVENT_READABLE_HANDLE: u32 = 4;
}

struct SystemClockState {
    initialized: bool,
    context: SystemClockContext,
    current_time: i64,
    operation_event: Option<Arc<Mutex<KReadableEvent>>>,
}

/// SystemClock service interface.
///
/// Corresponds to `SystemClock` in upstream system_clock.h.
pub struct SystemClock {
    can_write_clock: bool,
    can_write_uninitialized_clock: bool,
    state: Mutex<SystemClockState>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl SystemClock {
    pub fn new(can_write_clock: bool, can_write_uninitialized_clock: bool) -> Self {
        let handlers = build_handler_map(&[
            (
                commands::GET_CURRENT_TIME,
                Some(Self::get_current_time_handler),
                "GetCurrentTime",
            ),
            (
                commands::SET_CURRENT_TIME,
                Some(Self::set_current_time_handler),
                "SetCurrentTime",
            ),
            (
                commands::GET_SYSTEM_CLOCK_CONTEXT,
                Some(Self::get_system_clock_context_handler),
                "GetSystemClockContext",
            ),
            (
                commands::SET_SYSTEM_CLOCK_CONTEXT,
                Some(Self::set_system_clock_context_handler),
                "SetSystemClockContext",
            ),
            (
                commands::GET_OPERATION_EVENT_READABLE_HANDLE,
                Some(Self::get_operation_event_readable_handle_handler),
                "GetOperationEventReadableHandle",
            ),
        ]);
        Self {
            can_write_clock,
            can_write_uninitialized_clock,
            state: Mutex::new(SystemClockState {
                initialized: false,
                context: SystemClockContext::default(),
                current_time: 0,
                operation_event: None,
            }),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    /// Mark this clock as initialized.
    pub fn set_initialized(&mut self, initialized: bool) {
        self.state.lock().unwrap().initialized = initialized;
    }

    fn check_initialized(&self) -> ResultCode {
        if !self.can_write_uninitialized_clock && !self.state.lock().unwrap().initialized {
            return RESULT_CLOCK_UNINITIALIZED;
        }
        RESULT_SUCCESS
    }

    /// GetCurrentTime (cmd 0).
    ///
    /// Corresponds to `SystemClock::GetCurrentTime` in upstream system_clock.cpp.
    pub fn get_current_time(&self) -> Result<i64, ResultCode> {
        let check = self.check_initialized();
        if check.is_error() {
            return Err(check);
        }
        let current_time = self.state.lock().unwrap().current_time;
        log::debug!("SystemClock::GetCurrentTime: time={}", current_time);
        Ok(current_time)
    }

    /// SetCurrentTime (cmd 1).
    ///
    /// Corresponds to `SystemClock::SetCurrentTime` in upstream system_clock.cpp.
    pub fn set_current_time(&self, time: i64) -> ResultCode {
        log::debug!("SystemClock::SetCurrentTime: time={}", time);
        if !self.can_write_clock {
            return RESULT_PERMISSION_DENIED;
        }
        let check = self.check_initialized();
        if check.is_error() {
            return check;
        }
        self.state.lock().unwrap().current_time = time;
        RESULT_SUCCESS
    }

    /// GetSystemClockContext (cmd 2).
    ///
    /// Corresponds to `SystemClock::GetSystemClockContext` in upstream system_clock.cpp.
    pub fn get_system_clock_context(&self) -> Result<SystemClockContext, ResultCode> {
        let check = self.check_initialized();
        if check.is_error() {
            return Err(check);
        }
        let context = self.state.lock().unwrap().context;
        log::debug!("SystemClock::GetSystemClockContext: offset={}", context.offset);
        Ok(context)
    }

    /// SetSystemClockContext (cmd 3).
    ///
    /// Corresponds to `SystemClock::SetSystemClockContext` in upstream system_clock.cpp.
    pub fn set_system_clock_context(&self, context: &SystemClockContext) -> ResultCode {
        log::debug!(
            "SystemClock::SetSystemClockContext: offset={}",
            context.offset
        );
        if !self.can_write_clock {
            return RESULT_PERMISSION_DENIED;
        }
        let check = self.check_initialized();
        if check.is_error() {
            return check;
        }
        self.state.lock().unwrap().context = *context;
        RESULT_SUCCESS
    }

    /// GetOperationEventReadableHandle (cmd 4).
    ///
    /// Corresponds to `SystemClock::GetOperationEventReadableHandle` in upstream system_clock.cpp.
    /// Upstream lazily creates an OperationEvent, links it to the clock core,
    /// and returns the readable event handle. This port keeps a persistent
    /// readable-event owner and returns additional copy handles to it.
    pub fn get_operation_event_readable_handle(
        &self,
        ctx: &HLERequestContext,
    ) -> Result<u32, ResultCode> {
        log::debug!("SystemClock::GetOperationEventReadableHandle called");
        let mut state = self.state.lock().unwrap();
        let readable_event = if let Some(event) = state.operation_event.as_ref() {
            Arc::clone(event)
        } else {
            let Some((handle, readable_event)) = ctx.create_readable_event(false) else {
                return Err(RESULT_FAILED);
            };
            state.operation_event = Some(Arc::clone(&readable_event));
            return Ok(handle);
        };
        ctx.copy_handle_for_readable_event(readable_event)
            .ok_or(RESULT_FAILED)
    }

    fn get_current_time_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        match service.get_current_time() {
            Ok(time) => {
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_i64(time);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }

    fn set_current_time_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let time = rp.pop_i64();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(service.set_current_time(time));
    }

    fn get_system_clock_context_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        match service.get_system_clock_context() {
            Ok(context) => {
                let mut rb = ResponseBuilder::new(
                    ctx,
                    2 + (core::mem::size_of::<SystemClockContext>() / 4) as u32,
                    0,
                    0,
                );
                rb.push_result(RESULT_SUCCESS);
                rb.push_raw(&context);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }

    fn set_system_clock_context_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let context: SystemClockContext = rp.pop_raw();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(service.set_system_clock_context(&context));
    }

    fn get_operation_event_readable_handle_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        match service.get_operation_event_readable_handle(ctx) {
            Ok(handle) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_copy_objects(handle);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }
}

impl SessionRequestHandler for SystemClock {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ISystemClock"
    }
}

impl ServiceFramework for SystemClock {
    fn get_service_name(&self) -> &str {
        "ISystemClock"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exercised_handlers_are_registered() {
        let service = SystemClock::new(false, true);
        assert!(service
            .handlers()
            .get(&commands::GET_CURRENT_TIME)
            .and_then(|f| f.handler_callback)
            .is_some());
        assert!(service
            .handlers()
            .get(&commands::GET_SYSTEM_CLOCK_CONTEXT)
            .and_then(|f| f.handler_callback)
            .is_some());
    }
}
