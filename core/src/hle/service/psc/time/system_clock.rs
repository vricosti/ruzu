// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/system_clock.h
//! Port of zuyu/src/core/hle/service/psc/time/system_clock.cpp
//!
//! ISystemClock: provides system clock time queries and modifications.

use std::collections::BTreeMap;
use std::sync::Arc;

use super::common::SystemClockContext;
use super::errors::{RESULT_CLOCK_UNINITIALIZED, RESULT_PERMISSION_DENIED};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::os::event::Event;
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

/// SystemClock service interface.
///
/// Corresponds to `SystemClock` in upstream system_clock.h.
pub struct SystemClock {
    can_write_clock: bool,
    can_write_uninitialized_clock: bool,
    // State for the clock core
    initialized: bool,
    context: SystemClockContext,
    current_time: i64,
    /// Lazily created operation event.
    /// Upstream: `std::unique_ptr<OperationEvent> m_operation_event`.
    operation_event: Option<Arc<Event>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl SystemClock {
    pub fn new(can_write_clock: bool, can_write_uninitialized_clock: bool) -> Self {
        let handlers = build_handler_map(&[
            (
                commands::GET_CURRENT_TIME,
                Some(SystemClock::get_current_time_handler),
                "GetCurrentTime",
            ),
            (commands::SET_CURRENT_TIME, None, "SetCurrentTime"),
            (
                commands::GET_SYSTEM_CLOCK_CONTEXT,
                Some(SystemClock::get_system_clock_context_handler),
                "GetSystemClockContext",
            ),
            (
                commands::SET_SYSTEM_CLOCK_CONTEXT,
                None,
                "SetSystemClockContext",
            ),
            (
                commands::GET_OPERATION_EVENT_READABLE_HANDLE,
                None,
                "GetOperationEventReadableHandle",
            ),
        ]);
        Self {
            can_write_clock,
            can_write_uninitialized_clock,
            initialized: false,
            context: SystemClockContext::default(),
            current_time: 0,
            operation_event: None,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const SystemClock) }
    }

    /// Mark this clock as initialized.
    pub fn set_initialized(&mut self, initialized: bool) {
        self.initialized = initialized;
    }

    fn check_initialized(&self) -> ResultCode {
        if !self.can_write_uninitialized_clock && !self.initialized {
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
        log::debug!("SystemClock::GetCurrentTime: time={}", self.current_time);
        Ok(self.current_time)
    }

    /// SetCurrentTime (cmd 1).
    ///
    /// Corresponds to `SystemClock::SetCurrentTime` in upstream system_clock.cpp.
    pub fn set_current_time(&mut self, time: i64) -> ResultCode {
        log::debug!("SystemClock::SetCurrentTime: time={}", time);
        if !self.can_write_clock {
            return RESULT_PERMISSION_DENIED;
        }
        let check = self.check_initialized();
        if check.is_error() {
            return check;
        }
        self.current_time = time;
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
        log::debug!(
            "SystemClock::GetSystemClockContext: offset={}",
            self.context.offset
        );
        Ok(self.context)
    }

    /// SetSystemClockContext (cmd 3).
    ///
    /// Corresponds to `SystemClock::SetSystemClockContext` in upstream system_clock.cpp.
    pub fn set_system_clock_context(&mut self, context: &SystemClockContext) -> ResultCode {
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
        self.context = *context;
        RESULT_SUCCESS
    }

    /// GetOperationEventReadableHandle (cmd 4).
    ///
    /// Corresponds to `SystemClock::GetOperationEventReadableHandle` in upstream system_clock.cpp.
    /// Upstream lazily creates an OperationEvent, links it to the clock core,
    /// and returns the readable event handle. We lazily create an Event and
    /// return a reference to it.
    pub fn get_operation_event_readable_handle(&mut self) -> Arc<Event> {
        log::debug!("SystemClock::GetOperationEventReadableHandle called");
        let event = self
            .operation_event
            .get_or_insert_with(|| Arc::new(Event::new()));
        Arc::clone(event)
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

    fn get_system_clock_context_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
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
        let service = SystemClock::new(false, false);
        assert!(
            service
                .handlers()
                .get(&commands::GET_CURRENT_TIME)
                .and_then(|info| info.handler_callback)
                .is_some()
        );
        assert!(
            service
                .handlers()
                .get(&commands::GET_SYSTEM_CLOCK_CONTEXT)
                .and_then(|info| info.handler_callback)
                .is_some()
        );
    }
}
