// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/time_zone.h
//! Port of zuyu/src/core/hle/service/glue/time/time_zone.cpp
//!
//! TimeZoneService: time zone operations.
//! Status: Stub

/// IPC command IDs for TimeZoneService
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

pub struct TimeZoneService;

impl TimeZoneService {
    pub fn new() -> Self { Self }
}
