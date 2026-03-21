// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/system_clock.h
//! Port of zuyu/src/core/hle/service/psc/time/system_clock.cpp
//!
//! ISystemClock: provides system clock time queries and modifications.

use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::os::event::Event;
use super::common::SystemClockContext;
use super::errors::{RESULT_CLOCK_UNINITIALIZED, RESULT_PERMISSION_DENIED};

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
}

impl SystemClock {
    pub fn new(can_write_clock: bool, can_write_uninitialized_clock: bool) -> Self {
        Self {
            can_write_clock,
            can_write_uninitialized_clock,
            initialized: false,
            context: SystemClockContext::default(),
            current_time: 0,
            operation_event: None,
        }
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
        log::debug!("SystemClock::GetSystemClockContext: offset={}", self.context.offset);
        Ok(self.context)
    }

    /// SetSystemClockContext (cmd 3).
    ///
    /// Corresponds to `SystemClock::SetSystemClockContext` in upstream system_clock.cpp.
    pub fn set_system_clock_context(&mut self, context: &SystemClockContext) -> ResultCode {
        log::debug!("SystemClock::SetSystemClockContext: offset={}", context.offset);
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
}
