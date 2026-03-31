// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/steady_clock.h
//! Port of zuyu/src/core/hle/service/psc/time/steady_clock.cpp
//!
//! ISteadyClock: provides steady clock time point queries.

use std::collections::BTreeMap;

use super::common::SteadyClockTimePoint;
use super::errors::{RESULT_CLOCK_UNINITIALIZED, RESULT_NOT_IMPLEMENTED, RESULT_PERMISSION_DENIED};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for ISteadyClock.
///
/// Corresponds to the function table in upstream steady_clock.cpp constructor.
pub mod commands {
    pub const GET_CURRENT_TIME_POINT: u32 = 0;
    pub const GET_TEST_OFFSET: u32 = 2;
    pub const SET_TEST_OFFSET: u32 = 3;
    pub const GET_RTC_VALUE: u32 = 100;
    pub const IS_RTC_RESET_DETECTED: u32 = 101;
    pub const GET_SETUP_RESULT_VALUE: u32 = 102;
    pub const GET_INTERNAL_OFFSET: u32 = 200;
}

/// SteadyClock service interface.
///
/// Corresponds to `SteadyClock` in upstream steady_clock.h.
pub struct SteadyClock {
    can_write_steady_clock: bool,
    can_write_uninitialized_clock: bool,
    // State for the clock core
    initialized: bool,
    test_offset: i64,
    internal_offset: i64,
    current_time_point: SteadyClockTimePoint,
    setup_result: ResultCode,
    rtc_reset_detected: bool,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl SteadyClock {
    pub fn new(can_write_steady_clock: bool, can_write_uninitialized_clock: bool) -> Self {
        let handlers = build_handler_map(&[
            (
                commands::GET_CURRENT_TIME_POINT,
                None,
                "GetCurrentTimePoint",
            ),
            (commands::GET_TEST_OFFSET, None, "GetTestOffset"),
            (commands::SET_TEST_OFFSET, None, "SetTestOffset"),
            (commands::GET_RTC_VALUE, None, "GetRtcValue"),
            (commands::IS_RTC_RESET_DETECTED, None, "IsRtcResetDetected"),
            (
                commands::GET_SETUP_RESULT_VALUE,
                None,
                "GetSetupResultValue",
            ),
            (commands::GET_INTERNAL_OFFSET, None, "GetInternalOffset"),
        ]);
        Self {
            can_write_steady_clock,
            can_write_uninitialized_clock,
            initialized: false,
            test_offset: 0,
            internal_offset: 0,
            current_time_point: SteadyClockTimePoint::default(),
            setup_result: RESULT_SUCCESS,
            rtc_reset_detected: false,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Mark this clock as initialized (called during system setup).
    pub fn set_initialized(&mut self, initialized: bool) {
        self.initialized = initialized;
    }

    fn check_initialized(&self) -> ResultCode {
        if !self.can_write_uninitialized_clock && !self.initialized {
            return RESULT_CLOCK_UNINITIALIZED;
        }
        RESULT_SUCCESS
    }

    /// GetCurrentTimePoint (cmd 0).
    ///
    /// Corresponds to `SteadyClock::GetCurrentTimePoint` in upstream steady_clock.cpp.
    pub fn get_current_time_point(&self) -> Result<SteadyClockTimePoint, ResultCode> {
        let check = self.check_initialized();
        if check.is_error() {
            return Err(check);
        }
        log::debug!(
            "SteadyClock::GetCurrentTimePoint: time_point={}",
            self.current_time_point.time_point
        );
        Ok(self.current_time_point)
    }

    /// GetTestOffset (cmd 2).
    ///
    /// Corresponds to `SteadyClock::GetTestOffset` in upstream steady_clock.cpp.
    pub fn get_test_offset(&self) -> Result<i64, ResultCode> {
        let check = self.check_initialized();
        if check.is_error() {
            return Err(check);
        }
        log::debug!(
            "SteadyClock::GetTestOffset: test_offset={}",
            self.test_offset
        );
        Ok(self.test_offset)
    }

    /// SetTestOffset (cmd 3).
    ///
    /// Corresponds to `SteadyClock::SetTestOffset` in upstream steady_clock.cpp.
    pub fn set_test_offset(&mut self, test_offset: i64) -> ResultCode {
        log::debug!("SteadyClock::SetTestOffset: test_offset={}", test_offset);
        if !self.can_write_steady_clock {
            return RESULT_PERMISSION_DENIED;
        }
        let check = self.check_initialized();
        if check.is_error() {
            return check;
        }
        self.test_offset = test_offset;
        RESULT_SUCCESS
    }

    /// GetRtcValue (cmd 100).
    ///
    /// Corresponds to `SteadyClock::GetRtcValue` in upstream steady_clock.cpp.
    pub fn get_rtc_value(&self) -> Result<i64, ResultCode> {
        let check = self.check_initialized();
        if check.is_error() {
            return Err(check);
        }
        // In upstream this calls m_clock_core.GetRtcValue()
        // For now, return current system time in seconds
        let time_s = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs() as i64;
        log::debug!("SteadyClock::GetRtcValue: rtc_value={}", time_s);
        Ok(time_s)
    }

    /// IsRtcResetDetected (cmd 101).
    ///
    /// Corresponds to `SteadyClock::IsRtcResetDetected` in upstream steady_clock.cpp.
    pub fn is_rtc_reset_detected(&self) -> Result<bool, ResultCode> {
        let check = self.check_initialized();
        if check.is_error() {
            return Err(check);
        }
        log::debug!(
            "SteadyClock::IsRtcResetDetected: is_detected={}",
            self.rtc_reset_detected
        );
        Ok(self.rtc_reset_detected)
    }

    /// GetSetupResultValue (cmd 102).
    ///
    /// Corresponds to `SteadyClock::GetSetupResultValue` in upstream steady_clock.cpp.
    pub fn get_setup_result_value(&self) -> Result<ResultCode, ResultCode> {
        let check = self.check_initialized();
        if check.is_error() {
            return Err(check);
        }
        log::debug!(
            "SteadyClock::GetSetupResultValue: result={:08X}",
            self.setup_result.get_inner_value()
        );
        Ok(self.setup_result)
    }

    /// GetInternalOffset (cmd 200).
    ///
    /// Corresponds to `SteadyClock::GetInternalOffset` in upstream steady_clock.cpp.
    pub fn get_internal_offset(&self) -> Result<i64, ResultCode> {
        let check = self.check_initialized();
        if check.is_error() {
            return Err(check);
        }
        log::debug!(
            "SteadyClock::GetInternalOffset: internal_offset={}",
            self.internal_offset
        );
        Ok(self.internal_offset)
    }
}

impl SessionRequestHandler for SteadyClock {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ISteadyClock"
    }
}

impl ServiceFramework for SteadyClock {
    fn get_service_name(&self) -> &str {
        "ISteadyClock"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
