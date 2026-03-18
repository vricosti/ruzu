// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/static.h
//! Port of zuyu/src/core/hle/service/glue/time/static.cpp

use std::sync::Mutex;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::psc::time::common::{
    ClockSnapshot, StaticServiceSetupInfo, SteadyClockTimePoint, SystemClockContext, TimeType,
};
use crate::hle::service::psc::time::r#static as psc_static;

use super::file_timestamp_worker::FileTimestampWorker;
use super::standard_steady_clock_resource::StandardSteadyClockResource;
use super::time_zone::TimeZoneService;
use super::time_zone_binary::TimeZoneBinary;

/// IPC command IDs for `Glue::Time::StaticService`.
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

/// `Glue::Time::StaticService`.
pub struct StaticService {
    pub service_name: String,
    pub setup_info: StaticServiceSetupInfo,
    wrapped_service: Mutex<psc_static::StaticService>,
    file_timestamp_worker: FileTimestampWorker,
    standard_steady_clock_resource: StandardSteadyClockResource,
    time_zone_binary: Mutex<TimeZoneBinary>,
}

impl StaticService {
    pub fn new(setup_info: StaticServiceSetupInfo, name: &str) -> Self {
        log::debug!("Glue::Time::StaticService::new called for '{name}'");

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
            log::error!("  -> Unknown setup_info variant");
        }

        let mut time_zone_binary = TimeZoneBinary::new();
        let _ = time_zone_binary.mount();

        Self {
            service_name: name.to_string(),
            setup_info,
            wrapped_service: Mutex::new(psc_static::StaticService::new(setup_info)),
            file_timestamp_worker: FileTimestampWorker::new(),
            standard_steady_clock_resource: StandardSteadyClockResource::new(),
            time_zone_binary: Mutex::new(time_zone_binary),
        }
    }

    pub fn get_standard_user_system_clock(&self) -> ResultCode {
        log::debug!("Glue::Time::StaticService::GetStandardUserSystemClock called");
        RESULT_SUCCESS
    }

    pub fn get_standard_network_system_clock(&self) -> ResultCode {
        log::debug!("Glue::Time::StaticService::GetStandardNetworkSystemClock called");
        RESULT_SUCCESS
    }

    pub fn get_standard_steady_clock(&self) -> ResultCode {
        log::debug!("Glue::Time::StaticService::GetStandardSteadyClock called");
        RESULT_SUCCESS
    }

    pub fn get_time_zone_service(&self) -> Result<TimeZoneService, ResultCode> {
        log::debug!("Glue::Time::StaticService::GetTimeZoneService called");

        let _binary = self.time_zone_binary.lock().unwrap();
        Ok(TimeZoneService::new(
            self.setup_info.can_write_timezone_device_location,
        ))
    }

    pub fn get_standard_local_system_clock(&self) -> ResultCode {
        log::debug!("Glue::Time::StaticService::GetStandardLocalSystemClock called");
        RESULT_SUCCESS
    }

    pub fn get_ephemeral_network_system_clock(&self) -> ResultCode {
        log::debug!("Glue::Time::StaticService::GetEphemeralNetworkSystemClock called");
        RESULT_SUCCESS
    }

    pub fn set_standard_steady_clock_internal_offset(&self, offset_ns: i64) -> ResultCode {
        log::debug!(
            "Glue::Time::StaticService::SetStandardSteadyClockInternalOffset: offset_ns={offset_ns}"
        );
        use crate::hle::service::psc::time::errors::RESULT_PERMISSION_DENIED;
        if !self.setup_info.can_write_steady_clock {
            return RESULT_PERMISSION_DENIED;
        }
        let _offset_s = offset_ns / 1_000_000_000;
        RESULT_SUCCESS
    }

    pub fn get_standard_steady_clock_rtc_value(&self) -> Result<i64, ResultCode> {
        log::debug!("Glue::Time::StaticService::GetStandardSteadyClockRtcValue called");
        self.standard_steady_clock_resource.get_rtc_time_in_seconds()
    }

    pub fn is_standard_user_system_clock_automatic_correction_enabled(
        &self,
    ) -> Result<bool, ResultCode> {
        self.wrapped_service
            .lock()
            .unwrap()
            .is_standard_user_system_clock_automatic_correction_enabled()
    }

    pub fn set_standard_user_system_clock_automatic_correction_enabled(
        &self,
        automatic_correction: bool,
    ) -> ResultCode {
        self.wrapped_service
            .lock()
            .unwrap()
            .set_standard_user_system_clock_automatic_correction_enabled(automatic_correction)
    }

    /// GetStandardUserSystemClockInitialYear (cmd 102).
    ///
    /// Corresponds to `StaticService::GetStandardUserSystemClockInitialYear` in upstream.
    /// In upstream, delegates to m_set_sys->GetSettingsItemValueImpl<s32>(
    ///     "time", "standard_user_clock_initial_year").
    pub fn get_standard_user_system_clock_initial_year(&self) -> Result<i32, ResultCode> {
        log::debug!(
            "Glue::Time::StaticService::GetStandardUserSystemClockInitialYear called"
        );
        // TODO: Delegate to set:sys GetSettingsItemValueImpl once ISystemSettingsServer
        // is wired. Default initial year in upstream settings is 2019.
        Ok(2019)
    }

    pub fn is_standard_network_system_clock_accuracy_sufficient(&self) -> Result<bool, ResultCode> {
        Ok(self
            .wrapped_service
            .lock()
            .unwrap()
            .is_standard_network_system_clock_accuracy_sufficient())
    }

    pub fn get_clock_snapshot(&self, type_: TimeType) -> Result<ClockSnapshot, ResultCode> {
        self.wrapped_service.lock().unwrap().get_clock_snapshot(type_)
    }

    pub fn get_clock_snapshot_from_system_clock_context(
        &self,
        type_: TimeType,
        user_context: &SystemClockContext,
        network_context: &SystemClockContext,
    ) -> Result<ClockSnapshot, ResultCode> {
        self.wrapped_service
            .lock()
            .unwrap()
            .get_clock_snapshot_from_system_clock_context(type_, user_context, network_context)
    }

    /// CalculateMonotonicSystemClockBaseTimePoint (cmd 300).
    ///
    /// Corresponds to `StaticService::CalculateMonotonicSystemClockBaseTimePoint` in upstream.
    /// Delegates to the wrapped PSC static service.
    pub fn calculate_monotonic_system_clock_base_time_point(
        &self,
        context: &SystemClockContext,
    ) -> Result<i64, ResultCode> {
        log::debug!(
            "Glue::Time::StaticService::CalculateMonotonicSystemClockBaseTimePoint called"
        );
        self.wrapped_service
            .lock()
            .unwrap()
            .calculate_monotonic_system_clock_base_time_point(context)
    }

    pub fn get_standard_user_system_clock_automatic_correction_updated_time(
        &self,
    ) -> Result<SteadyClockTimePoint, ResultCode> {
        self.wrapped_service
            .lock()
            .unwrap()
            .get_standard_user_system_clock_automatic_correction_updated_time()
    }

    pub fn calculate_standard_user_system_clock_difference_by_user(
        &self,
        a: &ClockSnapshot,
        b: &ClockSnapshot,
    ) -> Result<i64, ResultCode> {
        Ok(self
            .wrapped_service
            .lock()
            .unwrap()
            .calculate_standard_user_system_clock_difference_by_user(a, b))
    }

    pub fn calculate_span_between(
        &self,
        a: &ClockSnapshot,
        b: &ClockSnapshot,
    ) -> Result<i64, ResultCode> {
        self.wrapped_service
            .lock()
            .unwrap()
            .calculate_span_between(a, b)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::service::psc::time::common::SteadyClockTimePoint;

    fn user_setup() -> StaticServiceSetupInfo {
        StaticServiceSetupInfo {
            can_write_local_clock: false,
            can_write_user_clock: false,
            can_write_network_clock: false,
            can_write_timezone_device_location: false,
            can_write_steady_clock: false,
            can_write_uninitialized_clock: false,
        }
    }

    #[test]
    fn get_time_zone_service_returns_glue_service_object() {
        let service = StaticService::new(user_setup(), "time:u");
        let time_zone_service = service.get_time_zone_service().unwrap();

        let name = time_zone_service.get_device_location_name().unwrap();
        assert_eq!(&name[..3], b"UTC");
    }

    fn admin_setup() -> StaticServiceSetupInfo {
        StaticServiceSetupInfo {
            can_write_local_clock: true,
            can_write_user_clock: true,
            can_write_network_clock: false,
            can_write_timezone_device_location: true,
            can_write_steady_clock: false,
            can_write_uninitialized_clock: false,
        }
    }

    #[test]
    fn delegated_correction_queries_follow_wrapped_psc_static_service() {
        // Use admin setup so we have can_write_user_clock permission
        let service = StaticService::new(admin_setup(), "time:a");
        {
            let mut wrapped = service.wrapped_service.lock().unwrap();
            wrapped.set_user_clock_initialized(true);
            wrapped.set_steady_clock_initialized(true);
            let rc = wrapped.set_standard_user_system_clock_automatic_correction_enabled(true);
            assert!(rc.is_success(), "Failed to enable automatic correction: {:?}", rc);
        }

        assert_eq!(
            service
                .is_standard_user_system_clock_automatic_correction_enabled()
                .unwrap(),
            true
        );
        assert_eq!(
            service
                .get_standard_user_system_clock_automatic_correction_updated_time()
                .unwrap(),
            SteadyClockTimePoint::default()
        );
    }
}
