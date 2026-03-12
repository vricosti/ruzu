// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/ptm/ts.h
//! Port of zuyu/src/core/hle/service/ptm/ts.cpp
//!
//! TS service ("ts") and ISession -- temperature sensor service.

/// Temperature sensor locations.
///
/// Corresponds to `Location` enum in upstream ts.cpp.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Internal = 0,
    External = 1,
}

/// IPC command IDs for TS service.
///
/// Corresponds to the function table in `TS` constructor (upstream ts.cpp).
pub mod commands {
    pub const GET_TEMPERATURE_RANGE: u32 = 0;
    pub const GET_TEMPERATURE: u32 = 1;
    pub const SET_MEASUREMENT_MODE: u32 = 2;
    pub const GET_TEMPERATURE_MILLI_C: u32 = 3;
    pub const OPEN_SESSION: u32 = 4;
}

/// IPC command IDs for ISession.
///
/// Corresponds to the function table in `ISession` constructor (upstream ts.cpp).
pub mod session_commands {
    pub const GET_TEMPERATURE_RANGE: u32 = 0;
    pub const SET_MEASUREMENT_MODE: u32 = 2;
    pub const GET_TEMPERATURE: u32 = 4;
}

/// TS service ("ts").
///
/// Corresponds to `TS` in upstream ts.h / ts.cpp.
pub struct TS;

impl TS {
    pub fn new() -> Self {
        Self
    }

    /// GetTemperature (cmd 1).
    ///
    /// Corresponds to `TS::GetTemperature` in upstream ts.cpp.
    /// Returns 35 degrees for internal, 20 for external.
    pub fn get_temperature(&self, location: Location) -> i32 {
        match location {
            Location::Internal => 35,
            Location::External => 20,
        }
    }

    /// GetTemperatureMilliC (cmd 3).
    ///
    /// Corresponds to `TS::GetTemperatureMilliC` in upstream ts.cpp.
    /// Returns 35000 mC for internal, 20000 for external.
    pub fn get_temperature_milli_c(&self, location: Location) -> i32 {
        match location {
            Location::Internal => 35000,
            Location::External => 20000,
        }
    }

    /// OpenSession (cmd 4).
    ///
    /// Corresponds to `TS::OpenSession` in upstream ts.cpp.
    pub fn open_session(&self, _device_code: u32) -> ISession {
        ISession::new()
    }
}

/// ISession -- per-device temperature query interface.
///
/// Corresponds to `ISession` in upstream ts.cpp.
pub struct ISession;

impl ISession {
    pub fn new() -> Self {
        Self
    }

    /// GetTemperature (cmd 4).
    ///
    /// Corresponds to `ISession::GetTemperature` in upstream ts.cpp.
    /// Returns a constant 35 degrees.
    pub fn get_temperature(&self) -> f32 {
        35.0
    }
}
