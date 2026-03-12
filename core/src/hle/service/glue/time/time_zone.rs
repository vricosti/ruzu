// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/time_zone.h
//! Port of zuyu/src/core/hle/service/glue/time/time_zone.cpp
//!
//! TimeZoneService: glue-layer timezone service that wraps PSC::Time::TimeZoneService
//! and adds timezone binary file loading support.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::psc::time::common::{
    CalendarAdditionalInfo, CalendarTime, LocationName, RuleVersion,
};
use crate::hle::service::psc::time::errors::RESULT_PERMISSION_DENIED;

/// IPC command IDs for Glue::Time::TimeZoneService.
///
/// Corresponds to the function table in upstream glue/time/time_zone.cpp constructor.
pub mod commands {
    pub const GET_DEVICE_LOCATION_NAME: u32 = 0;
    pub const SET_DEVICE_LOCATION_NAME: u32 = 1;
    pub const GET_TOTAL_LOCATION_NAME_COUNT: u32 = 2;
    pub const LOAD_LOCATION_NAME_LIST: u32 = 3;
    pub const LOAD_TIME_ZONE_RULE: u32 = 4;
    pub const GET_TIME_ZONE_RULE_VERSION: u32 = 5;
    pub const GET_DEVICE_LOCATION_NAME_AND_UPDATED_TIME: u32 = 6;
    pub const SET_DEVICE_LOCATION_NAME_WITH_TIME_ZONE_BINARY: u32 = 7;
    pub const PARSE_TIME_ZONE_BINARY: u32 = 8;
    pub const GET_DEVICE_LOCATION_NAME_OPERATION_EVENT_READ_ONLY: u32 = 9;
    pub const TO_CALENDAR_TIME: u32 = 20;
    pub const TO_CALENDAR_TIME_WITH_MY_RULE: u32 = 21;
    pub const TO_POSIX_TIME: u32 = 100;
    pub const TO_POSIX_TIME_WITH_MY_RULE: u32 = 101;
}

/// Glue-layer TimeZoneService.
///
/// Corresponds to `Glue::Time::TimeZoneService` in upstream glue/time/time_zone.h.
/// Wraps `PSC::Time::TimeZoneService` and adds timezone binary support.
pub struct TimeZoneService {
    can_write_timezone_device_location: bool,
    // TODO: references to FileTimestampWorker, TimeZoneBinary, PSC::Time::TimeZoneService
}

impl TimeZoneService {
    pub fn new(can_write_timezone_device_location: bool) -> Self {
        Self {
            can_write_timezone_device_location,
        }
    }

    /// GetDeviceLocationName (cmd 0).
    ///
    /// Corresponds to `TimeZoneService::GetDeviceLocationName` in upstream.
    pub fn get_device_location_name(&self) -> Result<LocationName, ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::GetDeviceLocationName called");
        // Default location: "UTC"
        let mut name: LocationName = [0u8; 0x24];
        name[0] = b'U';
        name[1] = b'T';
        name[2] = b'C';
        Ok(name)
    }

    /// SetDeviceLocationName (cmd 1).
    ///
    /// Corresponds to `TimeZoneService::SetDeviceLocationName` in upstream.
    pub fn set_device_location_name(&self, _name: &LocationName) -> ResultCode {
        log::debug!("Glue::Time::TimeZoneService::SetDeviceLocationName called");
        if !self.can_write_timezone_device_location {
            return RESULT_PERMISSION_DENIED;
        }
        // TODO: Load timezone binary for the new location, update PSC timezone service
        RESULT_SUCCESS
    }

    /// GetTotalLocationNameCount (cmd 2).
    ///
    /// Corresponds to `TimeZoneService::GetTotalLocationNameCount` in upstream.
    pub fn get_total_location_name_count(&self) -> Result<i32, ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::GetTotalLocationNameCount called");
        // TODO: Delegate to m_wrapped_service
        Ok(1) // At least UTC
    }

    /// LoadLocationNameList (cmd 3).
    ///
    /// Corresponds to `TimeZoneService::LoadLocationNameList` in upstream.
    pub fn load_location_name_list(
        &self,
        _index: i32,
    ) -> Result<Vec<LocationName>, ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::LoadLocationNameList called");
        // TODO: Load from timezone binary
        let mut utc: LocationName = [0u8; 0x24];
        utc[0] = b'U';
        utc[1] = b'T';
        utc[2] = b'C';
        Ok(vec![utc])
    }

    /// GetTimeZoneRuleVersion (cmd 5).
    ///
    /// Corresponds to `TimeZoneService::GetTimeZoneRuleVersion` in upstream.
    pub fn get_time_zone_rule_version(&self) -> Result<RuleVersion, ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::GetTimeZoneRuleVersion called");
        // TODO: Delegate to m_wrapped_service
        Ok([0u8; 0x10])
    }

    /// ToCalendarTime (cmd 20).
    ///
    /// Corresponds to `TimeZoneService::ToCalendarTime` in upstream.
    pub fn to_calendar_time(
        &self,
        time: i64,
        _rule: &[u8],
    ) -> Result<(CalendarTime, CalendarAdditionalInfo), ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::ToCalendarTime: time={}", time);
        // TODO: Delegate to m_wrapped_service
        Ok((CalendarTime::default(), CalendarAdditionalInfo::default()))
    }

    /// ToCalendarTimeWithMyRule (cmd 21).
    ///
    /// Corresponds to `TimeZoneService::ToCalendarTimeWithMyRule` in upstream.
    pub fn to_calendar_time_with_my_rule(
        &self,
        time: i64,
    ) -> Result<(CalendarTime, CalendarAdditionalInfo), ResultCode> {
        log::debug!(
            "Glue::Time::TimeZoneService::ToCalendarTimeWithMyRule: time={}",
            time
        );
        // Simple UTC conversion
        // seconds since epoch -> CalendarTime
        let secs = time;
        let days = secs / 86400;
        let day_secs = secs % 86400;

        let hours = (day_secs / 3600) as i8;
        let minutes = ((day_secs % 3600) / 60) as i8;
        let seconds = (day_secs % 60) as i8;

        // Simplified date calculation (not handling all edge cases)
        // This is a rough approximation for UTC
        let mut year = 1970i16;
        let mut remaining_days = days;
        loop {
            let days_in_year = if is_leap_year(year as i32) { 366 } else { 365 };
            if remaining_days < days_in_year {
                break;
            }
            remaining_days -= days_in_year;
            year += 1;
        }

        let leap = is_leap_year(year as i32);
        let month_days: [i64; 12] = if leap {
            [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        } else {
            [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        };

        let mut month = 0i8;
        for (i, &mdays) in month_days.iter().enumerate() {
            if remaining_days < mdays {
                month = (i + 1) as i8;
                break;
            }
            remaining_days -= mdays;
        }
        if month == 0 {
            month = 12;
        }
        let day = (remaining_days + 1) as i8;

        let calendar = CalendarTime {
            year,
            month,
            day,
            hour: hours,
            minute: minutes,
            second: seconds,
        };

        let day_of_week = ((days + 4) % 7) as i32; // Jan 1, 1970 was Thursday (4)
        let mut day_of_year = 0i32;
        for i in 0..(month as usize - 1) {
            day_of_year += month_days[i] as i32;
        }
        day_of_year += day as i32 - 1;

        let mut tz_name = [0u8; 8];
        tz_name[0] = b'U';
        tz_name[1] = b'T';
        tz_name[2] = b'C';

        let additional = CalendarAdditionalInfo {
            day_of_week,
            day_of_year,
            name: tz_name,
            is_dst: 0,
            ut_offset: 0,
        };

        Ok((calendar, additional))
    }

    /// ToPosixTime (cmd 100).
    ///
    /// Corresponds to `TimeZoneService::ToPosixTime` in upstream.
    pub fn to_posix_time(
        &self,
        _calendar: &CalendarTime,
        _rule: &[u8],
    ) -> Result<i64, ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::ToPosixTime called");
        // TODO: Delegate to m_wrapped_service
        Ok(0)
    }

    /// ToPosixTimeWithMyRule (cmd 101).
    ///
    /// Corresponds to `TimeZoneService::ToPosixTimeWithMyRule` in upstream.
    pub fn to_posix_time_with_my_rule(&self, calendar: &CalendarTime) -> Result<i64, ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::ToPosixTimeWithMyRule called");
        // Simple UTC conversion: CalendarTime -> seconds since epoch
        let year = calendar.year as i32;
        let mut days: i64 = 0;
        for y in 1970..year {
            days += if is_leap_year(y) { 366 } else { 365 };
        }

        let leap = is_leap_year(year);
        let month_days: [i64; 12] = if leap {
            [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        } else {
            [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        };
        for i in 0..(calendar.month as usize).saturating_sub(1) {
            days += month_days[i];
        }
        days += (calendar.day as i64) - 1;

        let secs = days * 86400
            + calendar.hour as i64 * 3600
            + calendar.minute as i64 * 60
            + calendar.second as i64;
        Ok(secs)
    }
}

fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}
