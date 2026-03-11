// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/time_zone_binary.h
//! Port of zuyu/src/core/hle/service/glue/time/time_zone_binary.cpp
//!
//! TimeZoneBinary: mount/read operations for timezone data.
//! Status: Stub

pub struct TimeZoneBinary;

impl TimeZoneBinary {
    pub fn new() -> Self { Self }
}

/// Get the timezone binary data for a location name.
pub fn get_time_zone_binary(_location_name: &str) -> Option<Vec<u8>> {
    log::warn!("(STUBBED) time_zone_binary::get_time_zone_binary");
    None
}
