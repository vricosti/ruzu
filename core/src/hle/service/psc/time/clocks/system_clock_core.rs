// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/clocks/system_clock_core.h/.cpp
//!
//! SystemClockCore: base system clock that combines a steady clock with a context.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::psc::time::common::{SteadyClockTimePoint, SystemClockContext};
use crate::hle::service::psc::time::errors::RESULT_CLOCK_MISMATCH;

/// SystemClockCore holds a SystemClockContext and references a SteadyClockCore.
///
/// In upstream C++, this holds a reference to SteadyClockCore and a pointer to
/// ContextWriter. Here we store the context directly and use callbacks for the
/// steady clock and context writer operations.
pub struct SystemClockCore {
    initialized: bool,
    context: SystemClockContext,
    /// Callback to get current time point from the steady clock.
    get_time_point: Box<dyn Fn() -> Result<SteadyClockTimePoint, ResultCode> + Send + Sync>,
    /// Optional callback to write context (replaces ContextWriter pointer).
    context_writer: Option<Box<dyn Fn(&SystemClockContext) -> ResultCode + Send + Sync>>,
    /// Optional callback to signal operation events through context writer.
    signal_fn: Option<Box<dyn Fn() + Send + Sync>>,
}

impl SystemClockCore {
    pub fn new(
        get_time_point: Box<dyn Fn() -> Result<SteadyClockTimePoint, ResultCode> + Send + Sync>,
    ) -> Self {
        Self {
            initialized: false,
            context: SystemClockContext::default(),
            get_time_point,
            context_writer: None,
            signal_fn: None,
        }
    }

    pub fn is_initialized(&self) -> bool {
        self.initialized
    }

    pub fn set_initialized(&mut self) {
        self.initialized = true;
    }

    pub fn set_context_writer(
        &mut self,
        writer: Box<dyn Fn(&SystemClockContext) -> ResultCode + Send + Sync>,
        signal: Box<dyn Fn() + Send + Sync>,
    ) {
        self.context_writer = Some(writer);
        self.signal_fn = Some(signal);
    }

    /// Check if the current steady clock source matches the context's steady time point.
    pub fn check_clock_source_matches(&self) -> bool {
        let context = match self.get_context() {
            Ok(c) => c,
            Err(_) => return false,
        };
        let time_point = match (self.get_time_point)() {
            Ok(tp) => tp,
            Err(_) => return false,
        };
        context.steady_time_point.id_matches(&time_point)
    }

    /// Get the current time in seconds.
    pub fn get_current_time(&self) -> Result<i64, ResultCode> {
        let time_point = (self.get_time_point)().map_err(|e| e)?;
        let context = self.get_context()?;

        if !context.steady_time_point.id_matches(&time_point) {
            return Err(RESULT_CLOCK_MISMATCH);
        }

        Ok(context.offset + time_point.time_point)
    }

    /// Set the current time, deriving a new context from the steady clock.
    pub fn set_current_time(&mut self, time: i64) -> ResultCode {
        let time_point = match (self.get_time_point)() {
            Ok(tp) => tp,
            Err(e) => return e,
        };

        let context = SystemClockContext {
            offset: time - time_point.time_point,
            steady_time_point: time_point,
        };
        self.set_context_and_write(&context)
    }

    pub fn get_current_time_point(&self) -> Result<SteadyClockTimePoint, ResultCode> {
        (self.get_time_point)()
    }

    pub fn get_context(&self) -> Result<SystemClockContext, ResultCode> {
        Ok(self.context)
    }

    pub fn set_context(&mut self, context: &SystemClockContext) -> ResultCode {
        self.context = *context;
        RESULT_SUCCESS
    }

    pub fn set_context_and_write(&mut self, context: &SystemClockContext) -> ResultCode {
        let rc = self.set_context(context);
        if rc != RESULT_SUCCESS {
            return rc;
        }

        if let Some(ref writer) = self.context_writer {
            let rc = writer(context);
            if rc != RESULT_SUCCESS {
                return rc;
            }
        }

        RESULT_SUCCESS
    }
}
