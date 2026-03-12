// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/time_zone.h/.cpp
//!
//! TimeZone: manages timezone rules and conversions.
//! Note: Upstream depends on the external `tz` library (Tz::Rule).
//! The Rust port uses a placeholder rule type until a Rust tz library
//! equivalent is integrated.

use std::sync::Mutex;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::common::{
    CalendarAdditionalInfo, CalendarTime, LocationName, RuleVersion, SteadyClockTimePoint,
};
use super::errors::RESULT_TIME_ZONE_NOT_FOUND;

/// Placeholder for Tz::Rule. Will be replaced with actual timezone rule data.
#[derive(Debug, Clone, Default)]
pub struct TzRule {
    // TODO: Port Tz::Rule structure from upstream tz library
    _data: Vec<u8>,
}

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
    /// TODO: Implement using a Rust tz library when available.
    pub fn to_calendar_time(
        &self,
        time: i64,
        _rule: &TzRule,
    ) -> Result<(CalendarTime, CalendarAdditionalInfo), ResultCode> {
        let _lock = self.mutex.lock().unwrap();
        // TODO: Implement actual timezone conversion using tz rules
        // For now, return a basic UTC conversion
        let secs = time;
        let days = secs / 86400;
        let remaining = secs % 86400;
        let hour = (remaining / 3600) as i8;
        let minute = ((remaining % 3600) / 60) as i8;
        let second = (remaining % 60) as i8;

        // Simple days-to-date conversion (not accounting for leap years properly yet)
        // This is a placeholder; real implementation needs tz rule application
        let mut y = 1970i64;
        let mut d = days;
        loop {
            let days_in_year = if y % 4 == 0 && (y % 100 != 0 || y % 400 == 0) {
                366
            } else {
                365
            };
            if d < days_in_year {
                break;
            }
            d -= days_in_year;
            y += 1;
        }
        let day_of_year = d as i32;

        let leap = y % 4 == 0 && (y % 100 != 0 || y % 400 == 0);
        let month_days: [i64; 12] = if leap {
            [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        } else {
            [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        };
        let mut month = 0i8;
        for (i, &md) in month_days.iter().enumerate() {
            if d < md {
                month = (i + 1) as i8;
                break;
            }
            d -= md;
        }
        if month == 0 {
            month = 12;
        }
        let day = (d + 1) as i8;

        let calendar = CalendarTime {
            year: y as i16,
            month,
            day,
            hour,
            minute,
            second,
        };
        let additional = CalendarAdditionalInfo {
            day_of_week: ((days + 4) % 7) as i32, // Jan 1 1970 was Thursday (4)
            day_of_year,
            name: [0u8; 8],
            is_dst: 0,
            ut_offset: 0,
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
    pub fn parse_binary(&mut self, name: &LocationName, binary: &[u8]) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        self.location = *name;
        // TODO: Parse the binary into self.my_rule using tz library
        self.my_rule._data = binary.to_vec();
        RESULT_SUCCESS
    }

    /// Parse a timezone binary into an output rule.
    pub fn parse_binary_into(&self, rule: &mut TzRule, binary: &[u8]) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        // TODO: Parse the binary into rule using tz library
        rule._data = binary.to_vec();
        RESULT_SUCCESS
    }

    /// Convert calendar time to POSIX time using the given rule.
    /// TODO: Implement using a Rust tz library when available.
    pub fn to_posix_time(
        &self,
        out_times: &mut [i64],
        calendar: &CalendarTime,
        _rule: &TzRule,
    ) -> Result<u32, ResultCode> {
        let _lock = self.mutex.lock().unwrap();
        // Basic UTC-only conversion (placeholder)
        if out_times.is_empty() {
            return Ok(0);
        }

        // Simple calendar to epoch conversion (UTC only, placeholder)
        let y = calendar.year as i64;
        let m = calendar.month as i64;
        let d = calendar.day as i64;

        // Days from epoch to start of year
        let mut days: i64 = 0;
        for yr in 1970..y {
            days += if yr % 4 == 0 && (yr % 100 != 0 || yr % 400 == 0) {
                366
            } else {
                365
            };
        }

        let leap = y % 4 == 0 && (y % 100 != 0 || y % 400 == 0);
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
