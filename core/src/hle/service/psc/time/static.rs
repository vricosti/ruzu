// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/static.h
//! Port of zuyu/src/core/hle/service/psc/time/static.cpp

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

use super::common::{
    get_span_between_time_points, ClockSnapshot, StaticServiceSetupInfo, SteadyClockTimePoint,
    SystemClockContext, TimeType,
};
use super::errors::{
    RESULT_CLOCK_MISMATCH, RESULT_CLOCK_UNINITIALIZED, RESULT_NOT_IMPLEMENTED,
    RESULT_PERMISSION_DENIED, RESULT_TIME_NOT_FOUND,
};
use super::steady_clock::SteadyClock;
use super::system_clock::SystemClock;
use super::time_zone::TimeZone;
use super::time_zone_service::TimeZoneService;

/// IPC command IDs for StaticService.
///
/// Corresponds to the function table in upstream `static.cpp`.
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

/// Default setup info for `time:su`.
pub const TIME_SU_SETUP_INFO: StaticServiceSetupInfo = StaticServiceSetupInfo {
    can_write_local_clock: false,
    can_write_user_clock: false,
    can_write_network_clock: false,
    can_write_timezone_device_location: false,
    can_write_steady_clock: false,
    can_write_uninitialized_clock: true,
};

/// Corresponds to the anonymous `GetTimeFromTimePointAndContext` helper in
/// upstream `static.cpp`.
pub fn get_time_from_time_point_and_context(
    time_point: &SteadyClockTimePoint,
    context: &SystemClockContext,
) -> Result<i64, ResultCode> {
    if !time_point.id_matches(&context.steady_time_point) {
        return Err(RESULT_CLOCK_MISMATCH);
    }
    Ok(context.offset + time_point.time_point)
}

/// `PSC::Time::StaticService`.
///
/// Corresponds to `PSC::Time::StaticService` in upstream static.h.
/// In upstream, this holds references to clock cores from TimeManager.
/// Here we maintain local state for the clock queries that don't
/// require sub-service creation, and create standalone sub-services
/// for the "Get" methods.
pub struct StaticService {
    pub setup_info: StaticServiceSetupInfo,
    automatic_correction_enabled: bool,
    automatic_correction_time_point: SteadyClockTimePoint,
    user_clock_initialized: bool,
    steady_clock_initialized: bool,
    network_accuracy_sufficient: bool,
    /// User system clock context (for GetClockSnapshot).
    user_context: SystemClockContext,
    /// Network system clock context (for GetClockSnapshot).
    network_context: SystemClockContext,
    /// Current steady clock time point (for GetClockSnapshot).
    steady_clock_time_point: SteadyClockTimePoint,
    /// TimeZone reference for calendar conversions in GetClockSnapshotImpl.
    /// Matches upstream `m_time_zone` (reference to `m_time->m_time_zone`).
    time_zone: TimeZone,
    /// Boot instant for CalculateMonotonicSystemClockBaseTimePoint.
    /// Matches upstream `m_system.CoreTiming().GetClockTicks()` converted to time.
    boot_instant: std::time::Instant,
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
            user_context: SystemClockContext::default(),
            network_context: SystemClockContext::default(),
            steady_clock_time_point: SteadyClockTimePoint::default(),
            time_zone: TimeZone::new(),
            boot_instant: std::time::Instant::now(),
        }
    }

    pub fn set_user_clock_initialized(&mut self, initialized: bool) {
        self.user_clock_initialized = initialized;
    }

    pub fn set_steady_clock_initialized(&mut self, initialized: bool) {
        self.steady_clock_initialized = initialized;
    }

    /// Set the user system clock context (for snapshot queries).
    pub fn set_user_context(&mut self, context: &SystemClockContext) {
        self.user_context = *context;
    }

    /// Set the network system clock context (for snapshot queries).
    pub fn set_network_context(&mut self, context: &SystemClockContext) {
        self.network_context = *context;
    }

    /// Set the current steady clock time point.
    pub fn set_steady_clock_time_point(&mut self, time_point: &SteadyClockTimePoint) {
        self.steady_clock_time_point = *time_point;
    }

    // =========================================================================
    // Commands 0-5: Sub-service creation
    //
    // In upstream, these create new IPC service objects referencing the clock
    // cores from TimeManager. Here we create standalone instances with the
    // appropriate permission flags from setup_info.
    // =========================================================================

    /// GetStandardUserSystemClock (cmd 0).
    ///
    /// Corresponds to `StaticService::GetStandardUserSystemClock` in upstream.
    /// Creates a SystemClock sub-service for the user system clock.
    pub fn get_standard_user_system_clock(&self) -> SystemClock {
        log::debug!("PSC::Time::StaticService::GetStandardUserSystemClock called");
        SystemClock::new(
            self.setup_info.can_write_user_clock,
            self.setup_info.can_write_uninitialized_clock,
        )
    }

    /// GetStandardNetworkSystemClock (cmd 1).
    ///
    /// Corresponds to `StaticService::GetStandardNetworkSystemClock` in upstream.
    /// Creates a SystemClock sub-service for the network system clock.
    pub fn get_standard_network_system_clock(&self) -> SystemClock {
        log::debug!("PSC::Time::StaticService::GetStandardNetworkSystemClock called");
        SystemClock::new(
            self.setup_info.can_write_network_clock,
            self.setup_info.can_write_uninitialized_clock,
        )
    }

    /// GetStandardSteadyClock (cmd 2).
    ///
    /// Corresponds to `StaticService::GetStandardSteadyClock` in upstream.
    /// Creates a SteadyClock sub-service.
    pub fn get_standard_steady_clock(&self) -> SteadyClock {
        log::debug!("PSC::Time::StaticService::GetStandardSteadyClock called");
        SteadyClock::new(
            self.setup_info.can_write_steady_clock,
            self.setup_info.can_write_uninitialized_clock,
        )
    }

    /// GetTimeZoneService (cmd 3).
    ///
    /// Corresponds to `StaticService::GetTimeZoneService` in upstream.
    /// Creates a TimeZoneService sub-service.
    pub fn get_time_zone_service(&self) -> TimeZoneService {
        log::debug!("PSC::Time::StaticService::GetTimeZoneService called");
        TimeZoneService::new(self.setup_info.can_write_timezone_device_location)
    }

    /// GetStandardLocalSystemClock (cmd 4).
    ///
    /// Corresponds to `StaticService::GetStandardLocalSystemClock` in upstream.
    /// Creates a SystemClock sub-service for the local system clock.
    pub fn get_standard_local_system_clock(&self) -> SystemClock {
        log::debug!("PSC::Time::StaticService::GetStandardLocalSystemClock called");
        SystemClock::new(
            self.setup_info.can_write_local_clock,
            self.setup_info.can_write_uninitialized_clock,
        )
    }

    /// GetEphemeralNetworkSystemClock (cmd 5).
    ///
    /// Corresponds to `StaticService::GetEphemeralNetworkSystemClock` in upstream.
    /// Creates a SystemClock sub-service for the ephemeral network clock.
    pub fn get_ephemeral_network_system_clock(&self) -> SystemClock {
        log::debug!("PSC::Time::StaticService::GetEphemeralNetworkSystemClock called");
        SystemClock::new(
            self.setup_info.can_write_network_clock,
            self.setup_info.can_write_uninitialized_clock,
        )
    }

    // =========================================================================
    // Command 20: Shared memory
    // =========================================================================

    /// GetSharedMemoryNativeHandle (cmd 20).
    ///
    /// Corresponds to `StaticService::GetSharedMemoryNativeHandle` in upstream.
    /// Returns the kernel shared memory handle for lock-free time reads.
    ///
    /// Upstream returns `&m_shared_memory.GetKSharedMemory()`. The
    /// Glue::Time::StaticService handles this at the IPC handler level
    /// (registering KSharedMemory in the process handle table). This PSC-level
    /// wrapper is not called directly; the Glue layer bypasses it.
    pub fn get_shared_memory_native_handle(&self) -> ResultCode {
        log::debug!(
            "PSC::Time::StaticService::GetSharedMemoryNativeHandle called. Not implemented!"
        );
        RESULT_NOT_IMPLEMENTED
    }

    // =========================================================================
    // Commands 50-51: Steady clock operations
    // =========================================================================

    pub fn set_standard_steady_clock_internal_offset(&self, offset_ns: i64) -> ResultCode {
        log::debug!(
            "StaticService::SetStandardSteadyClockInternalOffset: offset_ns={offset_ns}. Not implemented!"
        );
        if !self.setup_info.can_write_steady_clock {
            return RESULT_PERMISSION_DENIED;
        }
        RESULT_NOT_IMPLEMENTED
    }

    pub fn get_standard_steady_clock_rtc_value(&self) -> Result<i64, ResultCode> {
        log::debug!("StaticService::GetStandardSteadyClockRtcValue: Not implemented!");
        Err(RESULT_NOT_IMPLEMENTED)
    }

    // =========================================================================
    // Commands 100-102: User system clock
    // =========================================================================

    pub fn is_standard_user_system_clock_automatic_correction_enabled(
        &self,
    ) -> Result<bool, ResultCode> {
        if !self.user_clock_initialized {
            return Err(RESULT_CLOCK_UNINITIALIZED);
        }
        Ok(self.automatic_correction_enabled)
    }

    pub fn set_standard_user_system_clock_automatic_correction_enabled(
        &mut self,
        automatic_correction: bool,
    ) -> ResultCode {
        if !self.user_clock_initialized || !self.steady_clock_initialized {
            return RESULT_CLOCK_UNINITIALIZED;
        }
        if !self.setup_info.can_write_user_clock {
            return RESULT_PERMISSION_DENIED;
        }
        self.automatic_correction_enabled = automatic_correction;
        // Upstream calls m_shared_memory.SetAutomaticCorrection(automatic_correction)
        // and then gets the current steady clock time point to call
        // m_user_system_clock.SetTimePointAndSignal(time_point) followed by
        // m_user_system_clock.GetEvent().Signal().
        //
        // The SharedMemory and clock core references are held by
        // Glue::Time::StaticService (which wraps this PSC service). The Glue
        // layer is responsible for calling shared_memory.set_automatic_correction()
        // and updating the time point via the TimeManager. Here we only update
        // the local automatic_correction_time_point with the current steady
        // clock time point since that is the data this service owns.
        self.automatic_correction_time_point = self.steady_clock_time_point;
        RESULT_SUCCESS
    }

    /// GetStandardUserSystemClockInitialYear (cmd 102).
    ///
    /// Corresponds to `StaticService::GetStandardUserSystemClockInitialYear` in upstream.
    /// Upstream returns ResultNotImplemented.
    pub fn get_standard_user_system_clock_initial_year(&self) -> Result<i32, ResultCode> {
        log::debug!("StaticService::GetStandardUserSystemClockInitialYear: Not implemented!");
        Err(RESULT_NOT_IMPLEMENTED)
    }

    // =========================================================================
    // Commands 200-201: Network accuracy
    // =========================================================================

    pub fn is_standard_network_system_clock_accuracy_sufficient(&self) -> bool {
        self.network_accuracy_sufficient
    }

    pub fn get_standard_user_system_clock_automatic_correction_updated_time(
        &self,
    ) -> Result<SteadyClockTimePoint, ResultCode> {
        if !self.user_clock_initialized {
            return Err(RESULT_CLOCK_UNINITIALIZED);
        }
        Ok(self.automatic_correction_time_point)
    }

    // =========================================================================
    // Command 300: Monotonic base time point
    // =========================================================================

    /// CalculateMonotonicSystemClockBaseTimePoint (cmd 300).
    ///
    /// Corresponds to `StaticService::CalculateMonotonicSystemClockBaseTimePoint` in upstream.
    /// Calculates: (context.offset + time_point.time_point) - (current_time_ns / 1e9)
    ///
    /// Requires steady clock initialization and matching clock source IDs.
    pub fn calculate_monotonic_system_clock_base_time_point(
        &self,
        context: &SystemClockContext,
    ) -> Result<i64, ResultCode> {
        if !self.steady_clock_initialized {
            return Err(RESULT_CLOCK_UNINITIALIZED);
        }

        let time_point = self.steady_clock_time_point;

        if !time_point.id_matches(&context.steady_time_point) {
            return Err(RESULT_CLOCK_MISMATCH);
        }

        // Upstream:
        //   auto ticks = m_system.CoreTiming().GetClockTicks();
        //   auto current_time_ns = ConvertToTimeSpan(ticks).count();
        //   *out_time = (context.offset + time_point.time_point) - (current_time_ns / 1e9);
        //
        // We use elapsed wall time since boot as the CoreTiming approximation.
        let elapsed_ns = self.boot_instant.elapsed().as_nanos() as i64;
        let current_time_s = elapsed_ns / 1_000_000_000;
        let out_time = (context.offset + time_point.time_point) - current_time_s;
        log::debug!(
            "StaticService::CalculateMonotonicSystemClockBaseTimePoint: context_offset={}, elapsed_s={}, out_time={}",
            context.offset, current_time_s, out_time
        );
        Ok(out_time)
    }

    // =========================================================================
    // Commands 400-401: Clock snapshots
    // =========================================================================

    /// GetClockSnapshot (cmd 400).
    ///
    /// Corresponds to `StaticService::GetClockSnapshot` in upstream.
    /// Gets clock snapshot from the current user and network contexts.
    pub fn get_clock_snapshot(&self, type_: TimeType) -> Result<ClockSnapshot, ResultCode> {
        log::debug!("StaticService::GetClockSnapshot: type={:?}", type_);
        self.get_clock_snapshot_impl(&self.user_context, &self.network_context, type_)
    }

    /// GetClockSnapshotFromSystemClockContext (cmd 401).
    ///
    /// Corresponds to `StaticService::GetClockSnapshotFromSystemClockContext` in upstream.
    /// Gets clock snapshot from the provided user and network contexts.
    pub fn get_clock_snapshot_from_system_clock_context(
        &self,
        type_: TimeType,
        user_context: &SystemClockContext,
        network_context: &SystemClockContext,
    ) -> Result<ClockSnapshot, ResultCode> {
        log::debug!(
            "StaticService::GetClockSnapshotFromSystemClockContext: type={:?}",
            type_
        );
        self.get_clock_snapshot_impl(user_context, network_context, type_)
    }

    // =========================================================================
    // Commands 500-501: Calculations
    // =========================================================================

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

        Ok(time_s.saturating_mul(1_000_000_000))
    }

    // =========================================================================
    // Private helpers
    // =========================================================================

    /// GetClockSnapshotImpl (private).
    ///
    /// Corresponds to `StaticService::GetClockSnapshotImpl` in upstream.
    /// Fills a ClockSnapshot from the provided contexts.
    fn get_clock_snapshot_impl(
        &self,
        user_context: &SystemClockContext,
        network_context: &SystemClockContext,
        type_: TimeType,
    ) -> Result<ClockSnapshot, ResultCode> {
        let mut snapshot = ClockSnapshot::default();

        snapshot.user_context = *user_context;
        snapshot.network_context = *network_context;
        snapshot.steady_clock_time_point = self.steady_clock_time_point;
        snapshot.is_automatic_correction_enabled = self.automatic_correction_enabled;

        // Get location name from timezone.
        // Matches upstream: m_time_zone.GetLocationName(out_snapshot->location_name)
        snapshot.location_name = self.time_zone.get_location_name().unwrap_or([0u8; 0x24]);

        // Compute user_time from time_point and user_context.
        // Matches upstream: GetTimeFromTimePointAndContext(&out_snapshot->user_time, ...)
        snapshot.user_time = get_time_from_time_point_and_context(
            &snapshot.steady_clock_time_point,
            &snapshot.user_context,
        )?;

        // Compute user calendar time via TimeZone.
        // Matches upstream: m_time_zone.ToCalendarTimeWithMyRule(user_calendar_time, ..., user_time)
        let (cal, cal_info) = self.time_zone.to_calendar_time_with_my_rule(snapshot.user_time)?;
        snapshot.user_calendar_time = cal;
        snapshot.user_calendar_additional_time = cal_info;

        // Compute network_time, defaulting to 0 on mismatch (matching upstream).
        match get_time_from_time_point_and_context(
            &snapshot.steady_clock_time_point,
            &snapshot.network_context,
        ) {
            Ok(time) => snapshot.network_time = time,
            Err(_) => snapshot.network_time = 0,
        }

        // Compute network calendar time via TimeZone.
        // Matches upstream: m_time_zone.ToCalendarTimeWithMyRule(network_calendar_time, ..., network_time)
        let (cal, cal_info) = self.time_zone.to_calendar_time_with_my_rule(snapshot.network_time)?;
        snapshot.network_calendar_time = cal;
        snapshot.network_calendar_additional_time = cal_info;

        snapshot.time_type = type_ as u8;
        snapshot.unk_ce = 0;

        Ok(snapshot)
    }
}
