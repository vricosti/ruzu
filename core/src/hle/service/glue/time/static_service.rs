// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/static.h
//! Port of zuyu/src/core/hle/service/glue/time/static.cpp
//!
//! StaticService: time:u / time:a / time:r service.
//! Status: Stub

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// IPC command IDs for StaticService (time:u/a/r)
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

/// StaticService corresponds to upstream `Time::StaticService`.
pub struct StaticService {
    pub service_name: String,
}

impl StaticService {
    pub fn new(name: &str) -> Self {
        Self {
            service_name: name.to_string(),
        }
    }
}
