// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ptm/psm.h
//! Port of zuyu/src/core/hle/service/ptm/psm.cpp
//!
//! PSM service ("psm") and IPsmSession.

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl PSM {
    pub fn new(system: crate::core::SystemRef) -> Self {
        let handlers = build_handler_map(&[
            (
                commands::GET_BATTERY_CHARGE_PERCENTAGE,
                Some(PSM::get_battery_charge_percentage_handler),
                "GetBatteryChargePercentage",
            ),
            (
                commands::GET_CHARGER_TYPE,
                Some(PSM::get_charger_type_handler),
                "GetChargerType",
            ),
            (
                commands::ENABLE_BATTERY_CHARGING,
                None,
                "EnableBatteryCharging",
            ),
            (
                commands::DISABLE_BATTERY_CHARGING,
                None,
                "DisableBatteryCharging",
            ),
            (
                commands::IS_BATTERY_CHARGING_ENABLED,
                None,
                "IsBatteryChargingEnabled",
            ),
            (
                commands::ACQUIRE_CONTROLLER_POWER_SUPPLY,
                None,
                "AcquireControllerPowerSupply",
            ),
            (
                commands::RELEASE_CONTROLLER_POWER_SUPPLY,
                None,
                "ReleaseControllerPowerSupply",
            ),
            (
                commands::OPEN_SESSION,
                Some(PSM::open_session_handler),
                "OpenSession",
            ),
            (
                commands::ENABLE_ENOUGH_POWER_CHARGE_EMULATION,
                None,
                "EnableEnoughPowerChargeEmulation",
            ),
            (
                commands::DISABLE_ENOUGH_POWER_CHARGE_EMULATION,
                None,
                "DisableEnoughPowerChargeEmulation",
            ),
            (
                commands::ENABLE_FAST_BATTERY_CHARGING,
                None,
                "EnableFastBatteryCharging",
            ),
            (
                commands::DISABLE_FAST_BATTERY_CHARGING,
                None,
                "DisableFastBatteryCharging",
            ),
            (
                commands::GET_BATTERY_VOLTAGE_STATE,
                None,
                "GetBatteryVoltageState",
            ),
            (
                commands::GET_RAW_BATTERY_CHARGE_PERCENTAGE,
                None,
                "GetRawBatteryChargePercentage",
            ),
            (
                commands::IS_ENOUGH_POWER_SUPPLIED,
                None,
                "IsEnoughPowerSupplied",
            ),
            (
                commands::GET_BATTERY_AGE_PERCENTAGE,
                None,
                "GetBatteryAgePercentage",
            ),
            (
                commands::GET_BATTERY_CHARGE_INFO_EVENT,
                None,
                "GetBatteryChargeInfoEvent",
            ),
            (
                commands::GET_BATTERY_CHARGE_INFO_FIELDS,
                None,
                "GetBatteryChargeInfoFields",
            ),
            (
                commands::GET_BATTERY_CHARGE_CALIBRATED_EVENT,
                None,
                "GetBatteryChargeCalibratedEvent",
            ),
        ]);
        Self {
            system,
            battery_charge_percentage: 100,
            charger_type: ChargerType::Charger,
            handlers,
            handlers_tipc: BTreeMap::new(),
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

    // --- Handler bridge functions ---

    fn get_battery_charge_percentage_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PSM) };
        let percentage = service.get_battery_charge_percentage();

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(percentage);
    }

    fn get_charger_type_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PSM) };
        let charger = service.get_charger_type();

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(charger as u32);
    }

    fn open_session_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PSM) };
        let session = Arc::new(service.open_session());
        let handle = ctx.create_session_for_service(session).unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_move_objects(handle);
    }
}

impl SessionRequestHandler for PSM {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "psm"
    }
}

impl ServiceFramework for PSM {
    fn get_service_name(&self) -> &str {
        "psm"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// IPsmSession -- per-session battery/charger event interface.
///
/// Corresponds to `IPsmSession` in upstream psm.cpp.
pub struct IPsmSession {
    should_signal_charger_type: AtomicBool,
    should_signal_power_supply: AtomicBool,
    should_signal_battery_voltage: AtomicBool,
    should_signal: AtomicBool,
    service_context: crate::hle::service::kernel_helpers::ServiceContext,
    state_change_event_handle: u32,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IPsmSession {
    pub fn new() -> Self {
        let mut service_context =
            crate::hle::service::kernel_helpers::ServiceContext::new("IPsmSession".to_string());
        let state_change_event_handle =
            service_context.create_event("IPsmSession::state_change_event".to_string());
        let handlers = build_handler_map(&[
            (
                session_commands::BIND_STATE_CHANGE_EVENT,
                Some(IPsmSession::bind_state_change_event_handler),
                "BindStateChangeEvent",
            ),
            (
                session_commands::UNBIND_STATE_CHANGE_EVENT,
                Some(IPsmSession::unbind_state_change_event_handler),
                "UnbindStateChangeEvent",
            ),
            (
                session_commands::SET_CHARGER_TYPE_CHANGE_EVENT_ENABLED,
                Some(IPsmSession::set_charger_type_change_event_enabled_handler),
                "SetChargerTypeChangeEventEnabled",
            ),
            (
                session_commands::SET_POWER_SUPPLY_CHANGE_EVENT_ENABLED,
                Some(IPsmSession::set_power_supply_change_event_enabled_handler),
                "SetPowerSupplyChangeEventEnabled",
            ),
            (
                session_commands::SET_BATTERY_VOLTAGE_STATE_CHANGE_EVENT_ENABLED,
                Some(IPsmSession::set_battery_voltage_state_change_event_enabled_handler),
                "SetBatteryVoltageStateChangeEventEnabled",
            ),
        ]);
        Self {
            should_signal_charger_type: AtomicBool::new(false),
            should_signal_power_supply: AtomicBool::new(false),
            should_signal_battery_voltage: AtomicBool::new(false),
            should_signal: AtomicBool::new(false),
            service_context,
            state_change_event_handle,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// BindStateChangeEvent (cmd 0).
    ///
    /// Corresponds to `IPsmSession::BindStateChangeEvent` in upstream psm.cpp.
    /// Sets should_signal = true and returns the state_change_event handle.
    pub fn bind_state_change_event(&self) {
        log::debug!("IPsmSession::bind_state_change_event called");
        self.should_signal.store(true, Ordering::Relaxed);
    }

    /// UnbindStateChangeEvent (cmd 1).
    ///
    /// Corresponds to `IPsmSession::UnbindStateChangeEvent` in upstream psm.cpp.
    pub fn unbind_state_change_event(&self) {
        log::debug!("IPsmSession::unbind_state_change_event called");
        self.should_signal.store(false, Ordering::Relaxed);
    }

    /// SetChargerTypeChangeEventEnabled (cmd 2).
    ///
    /// Corresponds to `IPsmSession::SetChargerTypeChangeEventEnabled` in upstream psm.cpp.
    pub fn set_charger_type_change_event_enabled(&self, state: bool) {
        log::debug!(
            "IPsmSession::set_charger_type_change_event_enabled called, state={}",
            state
        );
        self.should_signal_charger_type
            .store(state, Ordering::Relaxed);
    }

    /// SetPowerSupplyChangeEventEnabled (cmd 3).
    ///
    /// Corresponds to `IPsmSession::SetPowerSupplyChangeEventEnabled` in upstream psm.cpp.
    pub fn set_power_supply_change_event_enabled(&self, state: bool) {
        log::debug!(
            "IPsmSession::set_power_supply_change_event_enabled called, state={}",
            state
        );
        self.should_signal_power_supply
            .store(state, Ordering::Relaxed);
    }

    /// SetBatteryVoltageStateChangeEventEnabled (cmd 4).
    ///
    /// Corresponds to `IPsmSession::SetBatteryVoltageStateChangeEventEnabled` in upstream psm.cpp.
    pub fn set_battery_voltage_state_change_event_enabled(&self, state: bool) {
        log::debug!(
            "IPsmSession::set_battery_voltage_state_change_event_enabled called, state={}",
            state
        );
        self.should_signal_battery_voltage
            .store(state, Ordering::Relaxed);
    }

    /// Signal charger type changed (internal helper).
    ///
    /// Corresponds to `IPsmSession::SignalChargerTypeChanged` in upstream psm.cpp.
    pub fn signal_charger_type_changed(&self) {
        if self.should_signal.load(Ordering::Relaxed)
            && self.should_signal_charger_type.load(Ordering::Relaxed)
        {
            if let Some(event) = self
                .service_context
                .get_event(self.state_change_event_handle)
            {
                event.signal();
            }
        }
    }

    /// Signal power supply changed (internal helper).
    ///
    /// Corresponds to `IPsmSession::SignalPowerSupplyChanged` in upstream psm.cpp.
    pub fn signal_power_supply_changed(&self) {
        if self.should_signal.load(Ordering::Relaxed)
            && self.should_signal_power_supply.load(Ordering::Relaxed)
        {
            if let Some(event) = self
                .service_context
                .get_event(self.state_change_event_handle)
            {
                event.signal();
            }
        }
    }

    /// Signal battery voltage state changed (internal helper).
    ///
    /// Corresponds to `IPsmSession::SignalBatteryVoltageStateChanged` in upstream psm.cpp.
    pub fn signal_battery_voltage_state_changed(&self) {
        if self.should_signal.load(Ordering::Relaxed)
            && self.should_signal_battery_voltage.load(Ordering::Relaxed)
        {
            if let Some(event) = self
                .service_context
                .get_event(self.state_change_event_handle)
            {
                event.signal();
            }
        }
    }

    // --- Handler bridge functions ---

    fn bind_state_change_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IPsmSession) };
        service.bind_state_change_event();

        if let Some(handle) = ctx.create_readable_event_handle(false) {
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(handle);
        } else {
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(0);
        }
    }

    fn unbind_state_change_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IPsmSession) };
        service.unbind_state_change_event();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_charger_type_change_event_enabled_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IPsmSession) };
        let mut rp = RequestParser::new(ctx);
        let state = rp.pop_bool();
        service.set_charger_type_change_event_enabled(state);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_power_supply_change_event_enabled_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IPsmSession) };
        let mut rp = RequestParser::new(ctx);
        let state = rp.pop_bool();
        service.set_power_supply_change_event_enabled(state);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_battery_voltage_state_change_event_enabled_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IPsmSession) };
        let mut rp = RequestParser::new(ctx);
        let state = rp.pop_bool();
        service.set_battery_voltage_state_change_event_enabled(state);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for IPsmSession {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "psm::IPsmSession"
    }
}

impl ServiceFramework for IPsmSession {
    fn get_service_name(&self) -> &str {
        "psm::IPsmSession"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
