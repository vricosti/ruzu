// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/time_zone.h/.cpp
//!
//! TimeZone: manages timezone rules and conversions.
//! TZif binary parsing is in the `tzif` sibling module.

use std::sync::Mutex;

use super::common::{
    CalendarAdditionalInfo, CalendarTime, LocationName, RuleVersion, SteadyClockTimePoint,
};
use super::errors::RESULT_TIME_ZONE_NOT_FOUND;
pub use super::tzif::{
    detzcode, detzcode64, is_leap, localsub, parse_posix_tz, time1, CalendarTimeInternal, TtInfo,
    TzRule, TM_YEAR_BASE,
};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// TimeZone manages timezone location, rules, and time conversions.
pub struct TimeZone {
    initialized: bool,
    mutex: Mutex<()>,
    location: LocationName,
    my_rule: TzRule,
    steady_clock_time_point: SteadyClockTimePoint,
    total_location_name_count: u32,
    rule_version: RuleVersion,
}

impl TimeZone {
    pub fn new() -> Self {
        Self {
            initialized: false,
            mutex: Mutex::new(()),
            location: [0u8; 0x24],
            my_rule: TzRule::default(),
            steady_clock_time_point: SteadyClockTimePoint::default(),
            total_location_name_count: 0,
            rule_version: [0u8; 0x10],
        }
    }

    pub fn is_initialized(&self) -> bool {
        self.initialized
    }

    pub fn set_initialized(&mut self) {
        self.initialized = true;
    }

    pub fn set_time_point(&mut self, time_point: &SteadyClockTimePoint) {
        let _lock = self.mutex.lock().unwrap();
        self.steady_clock_time_point = *time_point;
    }

    pub fn set_total_location_name_count(&mut self, count: u32) {
        let _lock = self.mutex.lock().unwrap();
        self.total_location_name_count = count;
    }

    pub fn set_rule_version(&mut self, rule_version: &RuleVersion) {
        let _lock = self.mutex.lock().unwrap();
        self.rule_version = *rule_version;
    }

    pub fn snapshot(&self) -> Self {
        let _lock = self.mutex.lock().unwrap();
        Self {
            initialized: self.initialized,
            mutex: Mutex::new(()),
            location: self.location,
            my_rule: self.my_rule.clone(),
            steady_clock_time_point: self.steady_clock_time_point,
            total_location_name_count: self.total_location_name_count,
            rule_version: self.rule_version,
        }
    }

    pub fn get_location_name(&self) -> Result<LocationName, ResultCode> {
        let _lock = self.mutex.lock().unwrap();
        Ok(self.location)
    }

    pub fn get_total_location_count(&self) -> Result<u32, ResultCode> {
        let _lock = self.mutex.lock().unwrap();
        Ok(self.total_location_name_count)
    }

    pub fn get_rule_version(&self) -> Result<RuleVersion, ResultCode> {
        let _lock = self.mutex.lock().unwrap();
        Ok(self.rule_version)
    }

    pub fn get_time_point(&self) -> Result<SteadyClockTimePoint, ResultCode> {
        let _lock = self.mutex.lock().unwrap();
        Ok(self.steady_clock_time_point)
    }

    /// Convert a POSIX time to calendar time using the given rule.
    ///
    /// Matches upstream `localtime_rz` -> `localsub` -> `timesub`.
    pub fn to_calendar_time(
        &self,
        time: i64,
        rule: &TzRule,
    ) -> Result<(CalendarTime, CalendarAdditionalInfo), ResultCode> {
        let _lock = self.mutex.lock().unwrap();

        let internal = localsub(rule, time).ok_or(RESULT_TIME_ZONE_NOT_FOUND)?;

        let calendar = CalendarTime {
            year: (internal.tm_year + TM_YEAR_BASE as i32) as i16,
            month: (internal.tm_mon + 1) as i8, // tm_mon is 0-based, CalendarTime month is 1-based
            day: internal.tm_mday as i8,
            hour: internal.tm_hour as i8,
            minute: internal.tm_min as i8,
            second: internal.tm_sec as i8,
        };

        let mut name = [0u8; 8];
        let abbr_len = internal
            .tm_zone
            .iter()
            .position(|&c| c == 0)
            .unwrap_or(internal.tm_zone.len());
        let copy_len = abbr_len.min(name.len() - 1);
        name[..copy_len].copy_from_slice(&internal.tm_zone[..copy_len]);

        let additional = CalendarAdditionalInfo {
            day_of_week: internal.tm_wday,
            day_of_year: internal.tm_yday,
            name,
            is_dst: internal.tm_isdst,
            ut_offset: internal.tm_utoff,
        };

        Ok((calendar, additional))
    }

    /// Convert using the device's own rule.
    pub fn to_calendar_time_with_my_rule(
        &self,
        time: i64,
    ) -> Result<(CalendarTime, CalendarAdditionalInfo), ResultCode> {
        let rule = self.my_rule.clone();
        self.to_calendar_time(time, &rule)
    }

    /// Parse a timezone binary for the given location name.
    ///
    /// Calls `TzRule::parse` to parse the TZif binary into a proper rule
    /// with transition times, type info, and abbreviations (matching upstream
    /// `ParseTimeZoneBinary` -> `tzloadbody`).
    pub fn parse_binary(&mut self, name: &LocationName, binary: &[u8]) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        self.location = *name;
        match TzRule::parse(binary) {
            Some(rule) => {
                self.my_rule = rule;
                RESULT_SUCCESS
            }
            None => {
                // Fall back to default (UTC) rule if parsing fails
                self.my_rule = TzRule::default();
                RESULT_SUCCESS
            }
        }
    }

    /// Parse a timezone binary into an output rule.
    pub fn parse_binary_into(&self, rule: &mut TzRule, binary: &[u8]) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        match TzRule::parse(binary) {
            Some(parsed) => {
                *rule = parsed;
                RESULT_SUCCESS
            }
            None => {
                *rule = TzRule::default();
                RESULT_SUCCESS
            }
        }
    }

    /// Convert calendar time to POSIX time using the given rule.
    ///
    /// Matches upstream `mktime_tzname` -> `time1`.
    pub fn to_posix_time(
        &self,
        out_times: &mut [i64],
        calendar: &CalendarTime,
        rule: &TzRule,
    ) -> Result<u32, ResultCode> {
        let _lock = self.mutex.lock().unwrap();
        if out_times.is_empty() {
            return Ok(0);
        }

        // Convert CalendarTime to CalendarTimeInternal
        let internal = CalendarTimeInternal {
            tm_sec: calendar.second as i32,
            tm_min: calendar.minute as i32,
            tm_hour: calendar.hour as i32,
            tm_mday: calendar.day as i32,
            tm_mon: calendar.month as i32 - 1, // CalendarTime month is 1-based, tm_mon is 0-based
            tm_year: calendar.year as i32 - TM_YEAR_BASE as i32,
            tm_wday: 0,
            tm_yday: 0,
            tm_isdst: -1, // Let the library figure out DST
            tm_zone: [0u8; 16],
            tm_utoff: 0,
            time_index: 0,
        };

        match time1(rule, &internal) {
            Some(t) => {
                out_times[0] = t;
                Ok(1)
            }
            None => {
                // Fall back to simple UTC conversion if timezone search fails
                let y = calendar.year as i64;
                let m = calendar.month as i64;
                let d = calendar.day as i64;

                let mut days: i64 = 0;
                if y >= 1970 {
                    for yr in 1970..y {
                        days += if is_leap(yr) { 366 } else { 365 };
                    }
                } else {
                    for yr in y..1970 {
                        days -= if is_leap(yr) { 366 } else { 365 };
                    }
                }

                let leap = is_leap(y);
                let month_days: [i64; 12] = if leap {
                    [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                } else {
                    [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                };
                for i in 0..(m - 1) as usize {
                    if i < 12 {
                        days += month_days[i];
                    }
                }
                days += d - 1;

                let seconds = days * 86400
                    + calendar.hour as i64 * 3600
                    + calendar.minute as i64 * 60
                    + calendar.second as i64;

                out_times[0] = seconds;
                Ok(1)
            }
        }
    }

    /// Convert calendar time to POSIX time using the device's own rule.
    pub fn to_posix_time_with_my_rule(
        &self,
        out_times: &mut [i64],
        calendar: &CalendarTime,
    ) -> Result<u32, ResultCode> {
        let rule = self.my_rule.clone();
        self.to_posix_time(out_times, calendar, &rule)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_rule_is_utc() {
        let rule = TzRule::default();
        assert_eq!(rule.timecnt, 0);
        assert_eq!(rule.typecnt, 1);
        assert_eq!(rule.ttis[0].tt_utoff, 0);
        assert!(!rule.ttis[0].tt_isdst);
    }

    #[test]
    fn test_utc_epoch_conversion() {
        let tz = TimeZone::new();
        let rule = TzRule::default();
        let (cal, info) = tz.to_calendar_time(0, &rule).unwrap();
        assert_eq!(cal.year, 1970);
        assert_eq!(cal.month, 1);
        assert_eq!(cal.day, 1);
        assert_eq!(cal.hour, 0);
        assert_eq!(cal.minute, 0);
        assert_eq!(cal.second, 0);
        assert_eq!(info.day_of_week, 4); // Thursday
        assert_eq!(info.day_of_year, 0);
        assert_eq!(info.ut_offset, 0);
        assert_eq!(info.is_dst, 0);
    }

    #[test]
    fn test_utc_known_date() {
        // 2023-06-15 13:40:00 UTC = 1686836400
        let tz = TimeZone::new();
        let rule = TzRule::default();
        let (cal, info) = tz.to_calendar_time(1686836400, &rule).unwrap();
        assert_eq!(cal.year, 2023);
        assert_eq!(cal.month, 6);
        assert_eq!(cal.day, 15);
        assert_eq!(cal.hour, 13);
        assert_eq!(cal.minute, 40);
        assert_eq!(cal.second, 0);
        assert_eq!(info.day_of_week, 4); // Thursday
        assert_eq!(info.ut_offset, 0);
    }

    #[test]
    fn test_utc_roundtrip() {
        let tz = TimeZone::new();
        let rule = TzRule::default();

        // Forward conversion
        let (cal, _) = tz.to_calendar_time(1686836400, &rule).unwrap();

        // Reverse conversion
        let mut out = [0i64; 1];
        let count = tz.to_posix_time(&mut out, &cal, &rule).unwrap();
        assert_eq!(count, 1);
        assert_eq!(out[0], 1686836400);
    }

    #[test]
    fn test_detzcode() {
        // Big-endian 0x00000000 = 0
        assert_eq!(detzcode(&[0, 0, 0, 0]), 0);
        // Big-endian 0x00000001 = 1
        assert_eq!(detzcode(&[0, 0, 0, 1]), 1);
        // Big-endian 0xFFFFFFFF = -1
        assert_eq!(detzcode(&[0xFF, 0xFF, 0xFF, 0xFF]), -1);
        // Big-endian 0x80000000 = i32::MIN
        assert_eq!(detzcode(&[0x80, 0, 0, 0]), i32::MIN);
    }

    #[test]
    fn test_detzcode64() {
        assert_eq!(detzcode64(&[0, 0, 0, 0, 0, 0, 0, 0]), 0);
        assert_eq!(detzcode64(&[0, 0, 0, 0, 0, 0, 0, 1]), 1);
        assert_eq!(
            detzcode64(&[0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
            -1
        );
    }

    #[test]
    fn test_tzif_magic_validation() {
        // Invalid magic
        assert!(TzRule::parse(b"XXXX").is_none());
        // Too short
        assert!(TzRule::parse(b"TZif").is_none());
    }

    #[test]
    fn test_parse_posix_tz_simple() {
        // Simple fixed offset
        let rule = parse_posix_tz(b"UTC0").unwrap();
        assert_eq!(rule.typecnt, 1);
        assert_eq!(rule.ttis[0].tt_utoff, 0);
        assert!(!rule.ttis[0].tt_isdst);
    }

    #[test]
    fn test_parse_posix_tz_with_dst() {
        // US Eastern
        let rule = parse_posix_tz(b"EST5EDT,M3.2.0,M11.1.0").unwrap();
        assert_eq!(rule.typecnt, 2);
        assert_eq!(rule.ttis[0].tt_utoff, -5 * 3600); // EST = UTC-5
        assert!(!rule.ttis[0].tt_isdst);
        assert_eq!(rule.ttis[1].tt_utoff, -4 * 3600); // EDT = UTC-4
        assert!(rule.ttis[1].tt_isdst);
    }
}
