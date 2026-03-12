// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/static.h
//! Port of zuyu/src/core/hle/service/glue/time/static.cpp
//!
//! StaticService: glue-layer time service that wraps PSC::Time::StaticService.
//! Provides time:u / time:a / time:r interfaces with additional functionality
//! like timezone binary management and file timestamp worker integration.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::psc::time::common::{
    ClockSnapshot, StaticServiceSetupInfo, SteadyClockTimePoint, SystemClockContext, TimeType,
};

/// IPC command IDs for Glue::Time::StaticService.
///
/// Corresponds to the function table in upstream glue/time/static.cpp constructor.
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
    pub const CALCULATE_SPAN_BETWEEN_STANDARD_USER_SYSTEM_CLOCKS: u32 = 501;
}

/// Glue-layer StaticService.
///
/// Corresponds to `Glue::Time::StaticService` in upstream glue/time/static.h.
/// Wraps `PSC::Time::StaticService` and adds timezone binary and file timestamp support.
pub struct StaticService {
    pub service_name: String,
    pub setup_info: StaticServiceSetupInfo,
    // TODO: m_set_sys (shared_ptr<ISystemSettingsServer>)
    // TODO: m_time_m (shared_ptr<PSC::Time::ServiceManager>)
    // TODO: m_wrapped_service (shared_ptr<PSC::Time::StaticService>)
    // TODO: m_time_zone (shared_ptr<PSC::Time::TimeZoneService>)
    // TODO: references to FileTimestampWorker, StandardSteadyClockResource, TimeZoneBinary
}

impl StaticService {
    /// Create a new glue-layer StaticService.
    ///
    /// Corresponds to `StaticService::StaticService(System&, StaticServiceSetupInfo,
    /// shared_ptr<TimeManager>, const char*)` in upstream.
    ///
    /// The constructor determines which PSC::Time::StaticService variant to use
    /// based on the setup_info flags:
    /// - admin: can_write_local + can_write_user + can_write_timezone
    /// - user: no write permissions
    /// - repair: can_write_steady only
    pub fn new(setup_info: StaticServiceSetupInfo, name: &str) -> Self {
        log::debug!("Glue::Time::StaticService::new called for '{}'", name);

        // Determine which wrapped service variant to create
        if setup_info.can_write_local_clock
            && setup_info.can_write_user_clock
            && !setup_info.can_write_network_clock
            && setup_info.can_write_timezone_device_location
            && !setup_info.can_write_steady_clock
            && !setup_info.can_write_uninitialized_clock
        {
            log::debug!("  -> Admin variant");
        } else if !setup_info.can_write_local_clock
            && !setup_info.can_write_user_clock
            && !setup_info.can_write_network_clock
            && !setup_info.can_write_timezone_device_location
            && !setup_info.can_write_steady_clock
            && !setup_info.can_write_uninitialized_clock
        {
            log::debug!("  -> User variant");
        } else if !setup_info.can_write_local_clock
            && !setup_info.can_write_user_clock
            && !setup_info.can_write_network_clock
            && !setup_info.can_write_timezone_device_location
            && setup_info.can_write_steady_clock
            && !setup_info.can_write_uninitialized_clock
        {
            log::debug!("  -> Repair variant");
        } else {
            log::error!("  -> Unknown setup_info variant!");
        }

        Self {
            service_name: name.to_string(),
            setup_info,
        }
    }

    /// GetStandardUserSystemClock (cmd 0).
    ///
    /// Delegates to the wrapped PSC::Time::StaticService.
    pub fn get_standard_user_system_clock(&self) -> ResultCode {
        log::debug!("Glue::Time::StaticService::GetStandardUserSystemClock called");
        // TODO: Delegate to m_wrapped_service->GetStandardUserSystemClock()
        RESULT_SUCCESS
    }

    /// GetStandardNetworkSystemClock (cmd 1).
    pub fn get_standard_network_system_clock(&self) -> ResultCode {
        log::debug!("Glue::Time::StaticService::GetStandardNetworkSystemClock called");
        RESULT_SUCCESS
    }

    /// GetStandardSteadyClock (cmd 2).
    pub fn get_standard_steady_clock(&self) -> ResultCode {
        log::debug!("Glue::Time::StaticService::GetStandardSteadyClock called");
        RESULT_SUCCESS
    }

    /// GetTimeZoneService (cmd 3).
    ///
    /// Creates a new Glue::Time::TimeZoneService wrapping the PSC timezone.
    pub fn get_time_zone_service(&self) -> ResultCode {
        log::debug!("Glue::Time::StaticService::GetTimeZoneService called");
        // TODO: Create TimeZoneService with file_timestamp_worker, timezone_binary, etc.
        RESULT_SUCCESS
    }

    /// GetStandardLocalSystemClock (cmd 4).
    pub fn get_standard_local_system_clock(&self) -> ResultCode {
        log::debug!("Glue::Time::StaticService::GetStandardLocalSystemClock called");
        RESULT_SUCCESS
    }

    /// GetEphemeralNetworkSystemClock (cmd 5).
    pub fn get_ephemeral_network_system_clock(&self) -> ResultCode {
        log::debug!("Glue::Time::StaticService::GetEphemeralNetworkSystemClock called");
        RESULT_SUCCESS
    }

    /// SetStandardSteadyClockInternalOffset (cmd 50).
    ///
    /// Converts offset_ns to seconds and saves via set:sys.
    pub fn set_standard_steady_clock_internal_offset(&self, offset_ns: i64) -> ResultCode {
        log::debug!(
            "Glue::Time::StaticService::SetStandardSteadyClockInternalOffset: offset_ns={}",
            offset_ns
        );
        use crate::hle::service::psc::time::errors::RESULT_PERMISSION_DENIED;
        if !self.setup_info.can_write_steady_clock {
            return RESULT_PERMISSION_DENIED;
        }
        let _offset_s = offset_ns / 1_000_000_000;
        // TODO: m_set_sys->SetExternalSteadyClockInternalOffset(offset_s)
        RESULT_SUCCESS
    }

    /// GetStandardSteadyClockRtcValue (cmd 51).
    pub fn get_standard_steady_clock_rtc_value(&self) -> Result<i64, ResultCode> {
        log::debug!("Glue::Time::StaticService::GetStandardSteadyClockRtcValue called");
        // TODO: m_standard_steady_clock_resource.GetRtcTimeInSeconds()
        let time_s = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs() as i64;
        Ok(time_s)
    }
}
