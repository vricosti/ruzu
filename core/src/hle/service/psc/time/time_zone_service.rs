// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/time_zone_service.h/.cpp
//!
//! ITimeZoneService: provides timezone queries and conversions.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::common::{
    CalendarAdditionalInfo, CalendarTime, LocationName, RuleVersion, SteadyClockTimePoint,
};
use super::errors::{RESULT_NOT_IMPLEMENTED, RESULT_PERMISSION_DENIED};
use super::time_zone::{TimeZone, TzRule};

/// IPC command IDs for ITimeZoneService.
///
/// Corresponds to the function table in upstream time_zone_service.cpp constructor.
pub mod commands {
    pub const GET_DEVICE_LOCATION_NAME: u32 = 0;
    pub const SET_DEVICE_LOCATION_NAME: u32 = 1;
    pub const GET_TOTAL_LOCATION_NAME_COUNT: u32 = 2;
    pub const LOAD_LOCATION_NAME_LIST: u32 = 3;
    pub const LOAD_TIME_ZONE_RULE: u32 = 4;
    pub const GET_TIME_ZONE_RULE_VERSION: u32 = 5;
    pub const GET_DEVICE_LOCATION_NAME_AND_UPDATED_TIME: u32 = 6;
    pub const SET_DEVICE_LOCATION_NAME_WITH_TIME_ZONE_RULE: u32 = 7;
    pub const PARSE_TIME_ZONE_BINARY: u32 = 8;
    pub const GET_DEVICE_LOCATION_NAME_OPERATION_EVENT_READABLE_HANDLE: u32 = 20;
    pub const TO_CALENDAR_TIME: u32 = 100;
    pub const TO_CALENDAR_TIME_WITH_MY_RULE: u32 = 101;
    pub const TO_POSIX_TIME: u32 = 201;
    pub const TO_POSIX_TIME_WITH_MY_RULE: u32 = 202;
}

/// PSC TimeZoneService.
///
/// Corresponds to `PSC::Time::TimeZoneService` in upstream time_zone_service.h.
pub struct TimeZoneService {
    can_write_timezone_device_location: bool,
    // In upstream, holds references to StandardSteadyClockCore and TimeZone.
    // Here we keep local state until clock core wiring is complete.
    time_zone: TimeZone,
}

impl TimeZoneService {
    pub fn new(can_write_timezone_device_location: bool) -> Self {
        Self {
            can_write_timezone_device_location,
            time_zone: TimeZone::new(),
        }
    }

    /// Create with an existing TimeZone reference (for when wired to TimeManager).
    pub fn with_time_zone(
        can_write_timezone_device_location: bool,
        time_zone: TimeZone,
    ) -> Self {
        Self {
            can_write_timezone_device_location,
            time_zone,
        }
    }

    /// GetDeviceLocationName (cmd 0).
    pub fn get_device_location_name(&self) -> Result<LocationName, ResultCode> {
        log::debug!("PSC::Time::TimeZoneService::GetDeviceLocationName called");
        self.time_zone.get_location_name()
    }

    /// SetDeviceLocationName (cmd 1).
    pub fn set_device_location_name(&self, _location_name: &LocationName) -> ResultCode {
        log::debug!("PSC::Time::TimeZoneService::SetDeviceLocationName called. Not implemented!");
        if !self.can_write_timezone_device_location {
            return RESULT_PERMISSION_DENIED;
        }
        RESULT_NOT_IMPLEMENTED
    }

    /// GetTotalLocationNameCount (cmd 2).
    pub fn get_total_location_name_count(&self) -> Result<u32, ResultCode> {
        log::debug!("PSC::Time::TimeZoneService::GetTotalLocationNameCount called");
        self.time_zone.get_total_location_count()
    }

    /// LoadLocationNameList (cmd 3).
    pub fn load_location_name_list(&self) -> ResultCode {
        log::debug!(
            "PSC::Time::TimeZoneService::LoadLocationNameList called. Not implemented!"
        );
        RESULT_NOT_IMPLEMENTED
    }

    /// LoadTimeZoneRule (cmd 4).
    pub fn load_time_zone_rule(&self) -> ResultCode {
        log::debug!("PSC::Time::TimeZoneService::LoadTimeZoneRule called. Not implemented!");
        RESULT_NOT_IMPLEMENTED
    }

    /// GetTimeZoneRuleVersion (cmd 5).
    pub fn get_time_zone_rule_version(&self) -> Result<RuleVersion, ResultCode> {
        log::debug!("PSC::Time::TimeZoneService::GetTimeZoneRuleVersion called");
        self.time_zone.get_rule_version()
    }

    /// GetDeviceLocationNameAndUpdatedTime (cmd 6).
    pub fn get_device_location_name_and_updated_time(
        &self,
    ) -> Result<(LocationName, SteadyClockTimePoint), ResultCode> {
        log::debug!(
            "PSC::Time::TimeZoneService::GetDeviceLocationNameAndUpdatedTime called"
        );
        let name = self.time_zone.get_location_name()?;
        let time_point = self.time_zone.get_time_point()?;
        Ok((name, time_point))
    }

    /// SetDeviceLocationNameWithTimeZoneRule (cmd 7).
    pub fn set_device_location_name_with_time_zone_rule(
        &mut self,
        location_name: &LocationName,
        binary: &[u8],
    ) -> ResultCode {
        log::debug!(
            "PSC::Time::TimeZoneService::SetDeviceLocationNameWithTimeZoneRule called"
        );
        if !self.can_write_timezone_device_location {
            return RESULT_PERMISSION_DENIED;
        }
        let rc = self.time_zone.parse_binary(location_name, binary);
        if rc.is_error() {
            return rc;
        }
        // Upstream gets the current time point from m_clock_core (StandardSteadyClockCore)
        // and calls m_time_zone.SetTimePoint(time_point). The steady clock core
        // reference is not wired into this service yet; the Glue layer's
        // TimeZoneService is the one that actually sets the time point after
        // calling SetDeviceLocationNameWithTimeZoneRule (see upstream
        // glue/time/time_zone.cpp). When the PSC service is called directly,
        // the time point remains unchanged.
        RESULT_SUCCESS
    }

    /// ParseTimeZoneBinary (cmd 8).
    pub fn parse_time_zone_binary(&self, rule: &mut TzRule, binary: &[u8]) -> ResultCode {
        log::debug!("PSC::Time::TimeZoneService::ParseTimeZoneBinary called");
        self.time_zone.parse_binary_into(rule, binary)
    }

    /// GetDeviceLocationNameOperationEventReadableHandle (cmd 20).
    pub fn get_device_location_name_operation_event_readable_handle(&self) -> ResultCode {
        log::debug!("PSC::Time::TimeZoneService::GetDeviceLocationNameOperationEventReadableHandle called. Not implemented!");
        RESULT_NOT_IMPLEMENTED
    }

    /// ToCalendarTime (cmd 100).
    pub fn to_calendar_time(
        &self,
        time: i64,
        rule: &TzRule,
    ) -> Result<(CalendarTime, CalendarAdditionalInfo), ResultCode> {
        log::debug!("PSC::Time::TimeZoneService::ToCalendarTime: time={}", time);
        self.time_zone.to_calendar_time(time, rule)
    }

    /// ToCalendarTimeWithMyRule (cmd 101).
    pub fn to_calendar_time_with_my_rule(
        &self,
        time: i64,
    ) -> Result<(CalendarTime, CalendarAdditionalInfo), ResultCode> {
        log::debug!(
            "PSC::Time::TimeZoneService::ToCalendarTimeWithMyRule: time={}",
            time
        );
        self.time_zone.to_calendar_time_with_my_rule(time)
    }

    /// ToPosixTime (cmd 201).
    pub fn to_posix_time(
        &self,
        out_times: &mut [i64],
        calendar_time: &CalendarTime,
        rule: &TzRule,
    ) -> Result<u32, ResultCode> {
        log::debug!("PSC::Time::TimeZoneService::ToPosixTime called");
        self.time_zone.to_posix_time(out_times, calendar_time, rule)
    }

    /// ToPosixTimeWithMyRule (cmd 202).
    pub fn to_posix_time_with_my_rule(
        &self,
        out_times: &mut [i64],
        calendar_time: &CalendarTime,
    ) -> Result<u32, ResultCode> {
        log::debug!("PSC::Time::TimeZoneService::ToPosixTimeWithMyRule called");
        self.time_zone
            .to_posix_time_with_my_rule(out_times, calendar_time)
    }
}
