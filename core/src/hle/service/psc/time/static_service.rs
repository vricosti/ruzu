// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/static.h
//! Port of zuyu/src/core/hle/service/psc/time/static.cpp
//!
//! StaticService: provides time service interfaces (time:u / time:a / time:r / time:su).
//! Note: file named static_service.rs to avoid Rust keyword conflict with "static".

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::common::{
    ClockSnapshot, StaticServiceSetupInfo, SteadyClockTimePoint, SystemClockContext, TimeType,
    get_span_between_time_points,
};
use super::errors::{
    RESULT_CLOCK_MISMATCH, RESULT_CLOCK_UNINITIALIZED, RESULT_NOT_IMPLEMENTED,
    RESULT_PERMISSION_DENIED, RESULT_TIME_NOT_FOUND,
};

/// IPC command IDs for StaticService.
///
/// Corresponds to the function table in upstream static.cpp constructor.
pub mod commands {
    pub const GET_STANDARD_USER_SYSTEM_CLOCK: u32 = 0;
    pub const GET_STANDARD_NETWORK_SYSTEM_CLOCK: u32 = 1;
    pub const GET_STANDARD_STEADY_CLOCK: u32 = 2;
    pub const GET_TIME_ZONE_SERVICE: u32 = 3;
    pub const GET_STANDARD_LOCAL_SYSTEM_CLOCK: u32 = 4;
    pub const GET_EPHEMERAL_NETWORK_SYSTEM_CLOCK: u32 = 5;
    pub const GET_SHARED_MEMORY_NATIVE_HANDLE: u32 = 20;
    pub const SET_STANDARD_STEADY_CLOCK_INTERNAL_OFFSET: u32 = 50;
    pub const GET_STANDARD_STEADY_CLOCK_RTC_VALUE: u32 = 51;
    pub const IS_STANDARD_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_ENABLED: u32 = 100;
    pub const SET_STANDARD_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_ENABLED: u32 = 101;
    pub const GET_STANDARD_USER_SYSTEM_CLOCK_INITIAL_YEAR: u32 = 102;
    pub const IS_STANDARD_NETWORK_SYSTEM_CLOCK_ACCURACY_SUFFICIENT: u32 = 200;
    pub const GET_STANDARD_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_UPDATED_TIME: u32 = 201;
    pub const CALCULATE_MONOTONIC_SYSTEM_CLOCK_BASE_TIME_POINT: u32 = 300;
    pub const GET_CLOCK_SNAPSHOT: u32 = 400;
    pub const GET_CLOCK_SNAPSHOT_FROM_SYSTEM_CLOCK_CONTEXT: u32 = 401;
    pub const CALCULATE_STANDARD_USER_SYSTEM_CLOCK_DIFFERENCE_BY_USER: u32 = 500;
    pub const CALCULATE_SPAN_BETWEEN: u32 = 501;
}

/// Default setup info for time:su
pub const TIME_SU_SETUP_INFO: StaticServiceSetupInfo = StaticServiceSetupInfo {
    can_write_local_clock: false,
    can_write_user_clock: false,
    can_write_network_clock: false,
    can_write_timezone_device_location: false,
    can_write_steady_clock: false,
    can_write_uninitialized_clock: true,
};

/// Helper: compute time from a time point and clock context.
///
/// Corresponds to the anonymous `GetTimeFromTimePointAndContext` in upstream static.cpp.
fn get_time_from_time_point_and_context(
    time_point: &SteadyClockTimePoint,
    context: &SystemClockContext,
) -> Result<i64, ResultCode> {
    if !time_point.id_matches(&context.steady_time_point) {
        return Err(RESULT_CLOCK_MISMATCH);
    }
    Ok(context.offset + time_point.time_point)
}

/// StaticService corresponds to upstream `PSC::Time::StaticService`.
pub struct StaticService {
    pub setup_info: StaticServiceSetupInfo,
    /// Whether the user system clock's automatic correction is enabled.
    automatic_correction_enabled: bool,
    /// The time point when automatic correction was last updated.
    automatic_correction_time_point: SteadyClockTimePoint,
    /// Whether the user system clock is initialized.
    user_clock_initialized: bool,
    /// Whether the steady clock is initialized.
    steady_clock_initialized: bool,
    /// Whether the network clock is sufficiently accurate.
    network_accuracy_sufficient: bool,
}

impl StaticService {
    pub fn new(setup_info: StaticServiceSetupInfo) -> Self {
        Self {
            setup_info,
            automatic_correction_enabled: false,
            automatic_correction_time_point: SteadyClockTimePoint::default(),
            user_clock_initialized: false,
            steady_clock_initialized: false,
            network_accuracy_sufficient: false,
        }
    }

    pub fn set_user_clock_initialized(&mut self, initialized: bool) {
        self.user_clock_initialized = initialized;
    }

    pub fn set_steady_clock_initialized(&mut self, initialized: bool) {
        self.steady_clock_initialized = initialized;
    }

    /// SetStandardSteadyClockInternalOffset (cmd 50).
    ///
    /// Corresponds to `StaticService::SetStandardSteadyClockInternalOffset` in upstream.
    pub fn set_standard_steady_clock_internal_offset(&self, offset_ns: i64) -> ResultCode {
        log::debug!(
            "StaticService::SetStandardSteadyClockInternalOffset: offset_ns={}. Not implemented!",
            offset_ns
        );
        if !self.setup_info.can_write_steady_clock {
            return RESULT_PERMISSION_DENIED;
        }
        RESULT_NOT_IMPLEMENTED
    }

    /// GetStandardSteadyClockRtcValue (cmd 51).
    ///
    /// Corresponds to `StaticService::GetStandardSteadyClockRtcValue` in upstream.
    pub fn get_standard_steady_clock_rtc_value(&self) -> Result<i64, ResultCode> {
        log::debug!(
            "StaticService::GetStandardSteadyClockRtcValue: Not implemented!"
        );
        Err(RESULT_NOT_IMPLEMENTED)
    }

    /// IsStandardUserSystemClockAutomaticCorrectionEnabled (cmd 100).
    ///
    /// Corresponds to upstream static.cpp.
    pub fn is_standard_user_system_clock_automatic_correction_enabled(
        &self,
    ) -> Result<bool, ResultCode> {
        if !self.user_clock_initialized {
            return Err(RESULT_CLOCK_UNINITIALIZED);
        }
        log::debug!(
            "StaticService::IsStandardUserSystemClockAutomaticCorrectionEnabled: {}",
            self.automatic_correction_enabled
        );
        Ok(self.automatic_correction_enabled)
    }

    /// SetStandardUserSystemClockAutomaticCorrectionEnabled (cmd 101).
    ///
    /// Corresponds to upstream static.cpp.
    pub fn set_standard_user_system_clock_automatic_correction_enabled(
        &mut self,
        automatic_correction: bool,
    ) -> ResultCode {
        log::debug!(
            "StaticService::SetStandardUserSystemClockAutomaticCorrectionEnabled: {}",
            automatic_correction
        );
        if !self.user_clock_initialized || !self.steady_clock_initialized {
            return RESULT_CLOCK_UNINITIALIZED;
        }
        if !self.setup_info.can_write_user_clock {
            return RESULT_PERMISSION_DENIED;
        }
        self.automatic_correction_enabled = automatic_correction;
        RESULT_SUCCESS
    }

    /// GetStandardUserSystemClockInitialYear (cmd 102).
    ///
    /// Corresponds to upstream static.cpp.
    pub fn get_standard_user_system_clock_initial_year(&self) -> Result<i32, ResultCode> {
        log::debug!(
            "StaticService::GetStandardUserSystemClockInitialYear: Not implemented!"
        );
        Err(RESULT_NOT_IMPLEMENTED)
    }

    /// IsStandardNetworkSystemClockAccuracySufficient (cmd 200).
    ///
    /// Corresponds to upstream static.cpp.
    pub fn is_standard_network_system_clock_accuracy_sufficient(&self) -> bool {
        log::debug!(
            "StaticService::IsStandardNetworkSystemClockAccuracySufficient: {}",
            self.network_accuracy_sufficient
        );
        self.network_accuracy_sufficient
    }

    /// GetStandardUserSystemClockAutomaticCorrectionUpdatedTime (cmd 201).
    ///
    /// Corresponds to upstream static.cpp.
    pub fn get_standard_user_system_clock_automatic_correction_updated_time(
        &self,
    ) -> Result<SteadyClockTimePoint, ResultCode> {
        if !self.user_clock_initialized {
            return Err(RESULT_CLOCK_UNINITIALIZED);
        }
        log::debug!(
            "StaticService::GetStandardUserSystemClockAutomaticCorrectionUpdatedTime: time_point={}",
            self.automatic_correction_time_point.time_point
        );
        Ok(self.automatic_correction_time_point)
    }

    /// CalculateStandardUserSystemClockDifferenceByUser (cmd 500).
    ///
    /// Corresponds to upstream static.cpp.
    pub fn calculate_standard_user_system_clock_difference_by_user(
        &self,
        a: &ClockSnapshot,
        b: &ClockSnapshot,
    ) -> i64 {
        let diff_s = b.user_context.offset - a.user_context.offset;

        if a.user_context == b.user_context
            || !a
                .user_context
                .steady_time_point
                .id_matches(&b.user_context.steady_time_point)
        {
            return 0;
        }

        if !a.is_automatic_correction_enabled || !b.is_automatic_correction_enabled {
            // Convert seconds to nanoseconds
            return diff_s.saturating_mul(1_000_000_000);
        }

        if a.network_context
            .steady_time_point
            .id_matches(&a.steady_clock_time_point)
            || b.network_context
                .steady_time_point
                .id_matches(&b.steady_clock_time_point)
        {
            return 0;
        }

        diff_s.saturating_mul(1_000_000_000)
    }

    /// CalculateSpanBetween (cmd 501).
    ///
    /// Corresponds to upstream static.cpp.
    pub fn calculate_span_between(
        &self,
        a: &ClockSnapshot,
        b: &ClockSnapshot,
    ) -> Result<i64, ResultCode> {
        let time_s =
            get_span_between_time_points(&a.steady_clock_time_point, &b.steady_clock_time_point);

        let time_s = match time_s {
            Some(t) => t,
            None => {
                if a.network_time == 0 || b.network_time == 0 {
                    return Err(RESULT_TIME_NOT_FOUND);
                }
                b.network_time - a.network_time
            }
        };

        // Convert seconds to nanoseconds
        Ok(time_s.saturating_mul(1_000_000_000))
    }
}
