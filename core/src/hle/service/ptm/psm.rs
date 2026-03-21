// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ptm/psm.h
//! Port of zuyu/src/core/hle/service/ptm/psm.cpp
//!
//! PSM service ("psm") and IPsmSession.

/// Charger type enum.
///
/// Corresponds to charger_type values used in upstream psm.cpp.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChargerType {
    None = 0,
    Charger = 1,
    UsbC = 2,
}

/// IPC command IDs for PSM service.
///
/// Corresponds to the function table in `PSM` constructor (upstream psm.cpp).
pub mod commands {
    pub const GET_BATTERY_CHARGE_PERCENTAGE: u32 = 0;
    pub const GET_CHARGER_TYPE: u32 = 1;
    pub const ENABLE_BATTERY_CHARGING: u32 = 2;
    pub const DISABLE_BATTERY_CHARGING: u32 = 3;
    pub const IS_BATTERY_CHARGING_ENABLED: u32 = 4;
    pub const ACQUIRE_CONTROLLER_POWER_SUPPLY: u32 = 5;
    pub const RELEASE_CONTROLLER_POWER_SUPPLY: u32 = 6;
    pub const OPEN_SESSION: u32 = 7;
    pub const ENABLE_ENOUGH_POWER_CHARGE_EMULATION: u32 = 8;
    pub const DISABLE_ENOUGH_POWER_CHARGE_EMULATION: u32 = 9;
    pub const ENABLE_FAST_BATTERY_CHARGING: u32 = 10;
    pub const DISABLE_FAST_BATTERY_CHARGING: u32 = 11;
    pub const GET_BATTERY_VOLTAGE_STATE: u32 = 12;
    pub const GET_RAW_BATTERY_CHARGE_PERCENTAGE: u32 = 13;
    pub const IS_ENOUGH_POWER_SUPPLIED: u32 = 14;
    pub const GET_BATTERY_AGE_PERCENTAGE: u32 = 15;
    pub const GET_BATTERY_CHARGE_INFO_EVENT: u32 = 16;
    pub const GET_BATTERY_CHARGE_INFO_FIELDS: u32 = 17;
    pub const GET_BATTERY_CHARGE_CALIBRATED_EVENT: u32 = 18;
}

/// IPC command IDs for IPsmSession.
///
/// Corresponds to the function table in `IPsmSession` constructor (upstream psm.cpp).
pub mod session_commands {
    pub const BIND_STATE_CHANGE_EVENT: u32 = 0;
    pub const UNBIND_STATE_CHANGE_EVENT: u32 = 1;
    pub const SET_CHARGER_TYPE_CHANGE_EVENT_ENABLED: u32 = 2;
    pub const SET_POWER_SUPPLY_CHANGE_EVENT_ENABLED: u32 = 3;
    pub const SET_BATTERY_VOLTAGE_STATE_CHANGE_EVENT_ENABLED: u32 = 4;
}

/// PSM service ("psm").
///
/// Corresponds to `PSM` in upstream psm.h / psm.cpp.
pub struct PSM {
    system: crate::core::SystemRef,
    /// Battery charge percentage (stubbed to 100%).
    battery_charge_percentage: u32,
    /// Charger type.
    charger_type: ChargerType,
}

impl PSM {
    pub fn new(system: crate::core::SystemRef) -> Self {
        Self {
            system,
            battery_charge_percentage: 100,
            charger_type: ChargerType::Charger,
        }
    }

    /// GetBatteryChargePercentage (cmd 0).
    ///
    /// Corresponds to `PSM::GetBatteryChargePercentage` in upstream psm.cpp.
    pub fn get_battery_charge_percentage(&self) -> u32 {
        log::debug!("PSM::get_battery_charge_percentage called");
        self.battery_charge_percentage
    }

    /// GetChargerType (cmd 1).
    ///
    /// Corresponds to `PSM::GetChargerType` in upstream psm.cpp.
    pub fn get_charger_type(&self) -> ChargerType {
        log::debug!("PSM::get_charger_type called");
        self.charger_type
    }

    /// OpenSession (cmd 7).
    ///
    /// Corresponds to `PSM::OpenSession` in upstream psm.cpp.
    pub fn open_session(&self) -> IPsmSession {
        log::debug!("PSM::open_session called");
        IPsmSession::new()
    }
}

/// IPsmSession -- per-session battery/charger event interface.
///
/// Corresponds to `IPsmSession` in upstream psm.cpp.
pub struct IPsmSession {
    should_signal_charger_type: bool,
    should_signal_power_supply: bool,
    should_signal_battery_voltage: bool,
    should_signal: bool,
    // In upstream: service_context and state_change_event (KEvent)
    // TODO: add kernel event support
}

impl IPsmSession {
    pub fn new() -> Self {
        // Upstream: state_change_event = service_context.CreateEvent("IPsmSession::state_change_event")
        Self {
            should_signal_charger_type: false,
            should_signal_power_supply: false,
            should_signal_battery_voltage: false,
            should_signal: false,
        }
    }

    /// BindStateChangeEvent (cmd 0).
    ///
    /// Corresponds to `IPsmSession::BindStateChangeEvent` in upstream psm.cpp.
    /// Sets should_signal = true and returns the state_change_event handle.
    pub fn bind_state_change_event(&mut self) {
        log::debug!("IPsmSession::bind_state_change_event called");
        self.should_signal = true;
        // TODO: return state_change_event->GetReadableEvent()
    }

    /// UnbindStateChangeEvent (cmd 1).
    ///
    /// Corresponds to `IPsmSession::UnbindStateChangeEvent` in upstream psm.cpp.
    pub fn unbind_state_change_event(&mut self) {
        log::debug!("IPsmSession::unbind_state_change_event called");
        self.should_signal = false;
    }

    /// SetChargerTypeChangeEventEnabled (cmd 2).
    ///
    /// Corresponds to `IPsmSession::SetChargerTypeChangeEventEnabled` in upstream psm.cpp.
    pub fn set_charger_type_change_event_enabled(&mut self, state: bool) {
        log::debug!(
            "IPsmSession::set_charger_type_change_event_enabled called, state={}",
            state
        );
        self.should_signal_charger_type = state;
    }

    /// SetPowerSupplyChangeEventEnabled (cmd 3).
    ///
    /// Corresponds to `IPsmSession::SetPowerSupplyChangeEventEnabled` in upstream psm.cpp.
    pub fn set_power_supply_change_event_enabled(&mut self, state: bool) {
        log::debug!(
            "IPsmSession::set_power_supply_change_event_enabled called, state={}",
            state
        );
        self.should_signal_power_supply = state;
    }

    /// SetBatteryVoltageStateChangeEventEnabled (cmd 4).
    ///
    /// Corresponds to `IPsmSession::SetBatteryVoltageStateChangeEventEnabled` in upstream psm.cpp.
    pub fn set_battery_voltage_state_change_event_enabled(&mut self, state: bool) {
        log::debug!(
            "IPsmSession::set_battery_voltage_state_change_event_enabled called, state={}",
            state
        );
        self.should_signal_battery_voltage = state;
    }

    /// Signal charger type changed (internal helper).
    ///
    /// Corresponds to `IPsmSession::SignalChargerTypeChanged` in upstream psm.cpp.
    pub fn signal_charger_type_changed(&self) {
        if self.should_signal && self.should_signal_charger_type {
            // TODO: state_change_event->Signal()
        }
    }

    /// Signal power supply changed (internal helper).
    ///
    /// Corresponds to `IPsmSession::SignalPowerSupplyChanged` in upstream psm.cpp.
    pub fn signal_power_supply_changed(&self) {
        if self.should_signal && self.should_signal_power_supply {
            // TODO: state_change_event->Signal()
        }
    }

    /// Signal battery voltage state changed (internal helper).
    ///
    /// Corresponds to `IPsmSession::SignalBatteryVoltageStateChanged` in upstream psm.cpp.
    pub fn signal_battery_voltage_state_changed(&self) {
        if self.should_signal && self.should_signal_battery_voltage {
            // TODO: state_change_event->Signal()
        }
    }
}
