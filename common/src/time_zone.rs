// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

use std::collections::HashMap;
use std::sync::LazyLock;
use std::time::SystemTime;

// Time zone strings — matches upstream constexpr array exactly
const TIMEZONES: [&str; 46] = [
    "GMT",
    "GMT",
    "CET",
    "CST6CDT",
    "Cuba",
    "EET",
    "Egypt",
    "Eire",
    "EST",
    "EST5EDT",
    "GB",
    "GB-Eire",
    "GMT",
    "GMT+0",
    "GMT-0",
    "GMT0",
    "Greenwich",
    "Hongkong",
    "HST",
    "Iceland",
    "Iran",
    "Israel",
    "Jamaica",
    "Japan",
    "Kwajalein",
    "Libya",
    "MET",
    "MST",
    "MST7MDT",
    "Navajo",
    "NZ",
    "NZ-CHAT",
    "Poland",
    "Portugal",
    "PRC",
    "PST8PDT",
    "ROC",
    "ROK",
    "Singapore",
    "Turkey",
    "UCT",
    "Universal",
    "UTC",
    "W-SU",
    "WET",
    "Zulu",
];

pub fn get_time_zone_strings() -> &'static [&'static str; 46] {
    &TIMEZONES
}

pub fn get_default_time_zone() -> String {
    "GMT".to_string()
}

/// Converts a broken-down time spec to cumulative seconds.
/// Results are not comparable to seconds since Epoch — matching upstream.
fn tm_spec_to_seconds(tm_year: i32, tm_yday: i32, tm_hour: i32, tm_min: i32, tm_sec: i32) -> i64 {
    let year = tm_year - 1; // Years up to now
    let leap_years = year / 4 - year / 100;
    let mut cumulative = tm_year as i64;
    cumulative = cumulative * 365 + leap_years as i64 + tm_yday as i64; // Years to days
    cumulative = cumulative * 24 + tm_hour as i64; // Days to hours
    cumulative = cumulative * 60 + tm_min as i64; // Hours to minutes
    cumulative = cumulative * 60 + tm_sec as i64; // Minutes to seconds
    cumulative
}

/// Gets the offset of the current timezone (from GMT), in seconds.
pub fn get_current_offset_seconds() -> i64 {
    // Use libc to get local and gmt time, matching upstream behavior
    #[cfg(unix)]
    {
        unsafe {
            let t = libc::time(std::ptr::null_mut());
            let local = *libc::localtime(&t);
            let gmt = *libc::gmtime(&t);

            let gmt_seconds = tm_spec_to_seconds(
                gmt.tm_year,
                gmt.tm_yday,
                gmt.tm_hour,
                gmt.tm_min,
                gmt.tm_sec,
            );
            let local_seconds = tm_spec_to_seconds(
                local.tm_year,
                local.tm_yday,
                local.tm_hour,
                local.tm_min,
                local.tm_sec,
            );

            local_seconds - gmt_seconds
        }
    }

    #[cfg(not(unix))]
    {
        // Fallback: return 0 (GMT)
        0
    }
}

// Key is [Hours * 100 + Minutes], multiplied by 100 if DST
static OFF_TIMEZONES: LazyLock<HashMap<i64, &'static str>> = LazyLock::new(|| {
    let mut m = HashMap::new();
    m.insert(530, "Asia/Calcutta");
    m.insert(930, "Australia/Darwin");
    m.insert(845, "Australia/Eucla");
    m.insert(103000, "Australia/Adelaide");
    m.insert(1030, "Australia/Lord_Howe");
    m.insert(630, "Indian/Cocos");
    m.insert(1245, "Pacific/Chatham");
    m.insert(134500, "Pacific/Chatham");
    m.insert(-330, "Canada/Newfoundland");
    m.insert(-23000, "Canada/Newfoundland");
    m.insert(430, "Asia/Kabul");
    m.insert(330, "Asia/Tehran");
    m.insert(43000, "Asia/Tehran");
    m.insert(545, "Asia/Kathmandu");
    m.insert(-930, "Asia/Marquesas");
    m
});

/// Searches time zone offsets for the closest offset to the system time zone.
pub fn find_system_time_zone() -> String {
    let seconds = get_current_offset_seconds();

    let minutes = seconds / 60;
    let hours = minutes / 60;
    let minutes_off = minutes - hours * 60;

    if minutes_off != 0 {
        #[cfg(unix)]
        let is_dst = unsafe {
            let t = libc::time(std::ptr::null_mut());
            let local = *libc::localtime(&t);
            local.tm_isdst != 0
        };

        #[cfg(not(unix))]
        let is_dst = false;

        let tz_index = (hours * 100 + minutes_off) * if is_dst { 100 } else { 1 };

        if let Some(&tz) = OFF_TIMEZONES.get(&tz_index) {
            return tz.to_string();
        }
        log::error!(
            "Time zone {} not handled, defaulting to hour offset.",
            tz_index
        );
    }

    // For some reason the Etc/GMT times are reversed. GMT+6 contains -21600 as its offset,
    // -6 hours instead of +6 hours, so these signs are purposefully reversed to fix it.
    let postfix = if hours > 0 {
        format!("-{}", hours.abs())
    } else if hours < 0 {
        format!("+{}", hours.abs())
    } else {
        String::new()
    };

    format!("Etc/GMT{}", postfix)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_time_zone_strings() {
        let tz = get_time_zone_strings();
        assert_eq!(tz.len(), 46);
        assert_eq!(tz[0], "GMT");
        assert_eq!(tz[45], "Zulu");
    }

    #[test]
    fn test_get_default_time_zone() {
        assert_eq!(get_default_time_zone(), "GMT");
    }

    #[test]
    fn test_find_system_time_zone_returns_string() {
        let tz = find_system_time_zone();
        assert!(!tz.is_empty());
    }

    #[test]
    fn test_tm_spec_to_seconds_deterministic() {
        let s1 = tm_spec_to_seconds(120, 100, 12, 30, 45);
        let s2 = tm_spec_to_seconds(120, 100, 12, 30, 45);
        assert_eq!(s1, s2);
    }
}
