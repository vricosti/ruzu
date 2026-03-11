// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ptm/psm.h
//! Port of zuyu/src/core/hle/service/ptm/psm.cpp
//!
//! IPsmServer — Power/battery management service ("psm").

/// IPC command table for IPsmServer.
pub mod commands {
    pub const GET_BATTERY_CHARGE_PERCENTAGE: u32 = 0;
    pub const GET_CHARGER_TYPE: u32 = 1;
    pub const ENABLE_BATTERY_CHARGING: u32 = 2;
    pub const DISABLE_BATTERY_CHARGING: u32 = 3;
    pub const IS_BATTERY_CHARGING_ENABLED: u32 = 4;
    pub const ACQUIRE_BATTERY_VOLTAGE_STATE_CHANGED_EVENT: u32 = 5;
    pub const GET_BATTERY_VOLTAGE_STATE: u32 = 6;
    pub const GET_BATTERY_AGE_PERCENTAGE: u32 = 7;
    pub const GET_RAW_BATTERY_CHARGE_PERCENTAGE: u32 = 8;
    pub const IS_ENOUGH_POWER_SUPPLIED: u32 = 9;
    pub const GET_BATTERY_VOLTAGE_STATE2: u32 = 10;
    pub const GET_RAW_BATTERY_AGE_PERCENTAGE: u32 = 11;
    pub const SET_BATTERY_AGE_RATE: u32 = 12;
    pub const GET_BATTERY_CYCLE_COUNT: u32 = 13;
    pub const RESET_BATTERY_CYCLE_COUNT: u32 = 14;
    pub const GET_BATTERY_CHARGE_INFO_FIELDS: u32 = 15;
    pub const GET_BATTERY_CHARGING_INFO_FIELDS: u32 = 16;
    pub const GET_BATTERY_CHARGE_INFO_EVENT: u32 = 17;
    pub const OPEN_SESSION: u32 = 18;
}

/// IPsmServer service.
///
/// Corresponds to `IPsmServer` in upstream psm.h / psm.cpp.
pub struct IPsmServer {
    /// Battery charge percentage (stubbed to 100%).
    battery_charge_percentage: u32,
    /// Charger type (stubbed to ChargerType::Charger = 1).
    charger_type: u32,
}

impl IPsmServer {
    pub fn new() -> Self {
        Self {
            battery_charge_percentage: 100,
            charger_type: 1,
        }
    }

    /// GetBatteryChargePercentage (cmd 0).
    pub fn get_battery_charge_percentage(&self) -> u32 {
        log::debug!("IPsmServer::get_battery_charge_percentage called");
        self.battery_charge_percentage
    }

    /// GetChargerType (cmd 1).
    pub fn get_charger_type(&self) -> u32 {
        log::debug!("IPsmServer::get_charger_type called");
        self.charger_type
    }
}

/// IPsmSession — per-session battery event interface.
///
/// Corresponds to `IPsmSession` in upstream psm.cpp.
pub struct IPsmSession;

impl IPsmSession {
    pub fn new() -> Self {
        Self
    }
}
