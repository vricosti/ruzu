// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ptm/ts.h
//! Port of zuyu/src/core/hle/service/ptm/ts.cpp
//!
//! ITs — Temperature sensor service ("ts").

/// IPC command table for ITs.
pub mod commands {
    pub const GET_TEMPERATURE_RANGE: u32 = 0;
    pub const GET_TEMPERATURE: u32 = 1;
    pub const SET_MEASURING_MODE: u32 = 2;
    pub const GET_TEMPERATURE_MILLISECONDS: u32 = 3;
    pub const OPEN_SESSION: u32 = 4;
}

/// Temperature sensor locations, matching upstream enum.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Internal = 0,
    External = 1,
}

/// ITs service.
///
/// Corresponds to `ITs` in upstream ts.h / ts.cpp.
pub struct ITs;

impl ITs {
    pub fn new() -> Self {
        Self
    }

    /// GetTemperature (cmd 1).
    ///
    /// Stubbed: returns 35 degrees for internal, 20 for external.
    pub fn get_temperature(&self, location: Location) -> i32 {
        log::debug!("ITs::get_temperature called, location={:?}", location);
        match location {
            Location::Internal => 35,
            Location::External => 20,
        }
    }

    /// GetTemperatureMilliC (cmd 3).
    ///
    /// Stubbed: returns 35000 mC for internal, 20000 for external.
    pub fn get_temperature_milliseconds(&self, location: Location) -> i32 {
        log::debug!(
            "ITs::get_temperature_milliseconds called, location={:?}",
            location
        );
        match location {
            Location::Internal => 35000,
            Location::External => 20000,
        }
    }
}
