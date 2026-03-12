// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/btdrv/btdrv.cpp
//!
//! IBluetoothDriver ("btdrv") and IBluetoothUser ("bt") services.

/// IPC command IDs for IBluetoothUser ("bt").
///
/// Corresponds to the function table in `IBluetoothUser` constructor (upstream btdrv.cpp).
pub mod bt_commands {
    pub const LE_CLIENT_READ_CHARACTERISTIC: u32 = 0;
    pub const LE_CLIENT_READ_DESCRIPTOR: u32 = 1;
    pub const LE_CLIENT_WRITE_CHARACTERISTIC: u32 = 2;
    pub const LE_CLIENT_WRITE_DESCRIPTOR: u32 = 3;
    pub const LE_CLIENT_REGISTER_NOTIFICATION: u32 = 4;
    pub const LE_CLIENT_DEREGISTER_NOTIFICATION: u32 = 5;
    pub const SET_LE_RESPONSE: u32 = 6;
    pub const LE_SEND_INDICATION: u32 = 7;
    pub const GET_LE_EVENT_INFO: u32 = 8;
    pub const REGISTER_BLE_EVENT: u32 = 9;
}

/// IPC command IDs for IBluetoothDriver ("btdrv").
///
/// Corresponds to the function table in `IBluetoothDriver` constructor (upstream btdrv.cpp).
/// Only the key implemented/notable commands are listed; the rest are nullptr stubs.
pub mod btdrv_commands {
    pub const INITIALIZE_BLUETOOTH_DRIVER: u32 = 0;
    pub const INITIALIZE_BLUETOOTH: u32 = 1;
    pub const ENABLE_BLUETOOTH: u32 = 2;
    pub const DISABLE_BLUETOOTH: u32 = 3;
    pub const FINALIZE_BLUETOOTH: u32 = 4;
    pub const GET_ADAPTER_PROPERTIES: u32 = 5;
    pub const GET_ADAPTER_PROPERTY: u32 = 6;
    pub const SET_ADAPTER_PROPERTY: u32 = 7;
    pub const START_INQUIRY: u32 = 8;
    pub const STOP_INQUIRY: u32 = 9;
    pub const CREATE_BOND: u32 = 10;
    pub const REMOVE_BOND: u32 = 11;
    pub const CANCEL_BOND: u32 = 12;
    pub const RESPOND_TO_PIN_REQUEST: u32 = 13;
    pub const RESPOND_TO_SSP_REQUEST: u32 = 14;
    pub const GET_EVENT_INFO: u32 = 15;
    pub const INITIALIZE_HID: u32 = 16;
    pub const OPEN_HID_CONNECTION: u32 = 17;
    pub const CLOSE_HID_CONNECTION: u32 = 18;
    pub const WRITE_HID_DATA: u32 = 19;
    pub const WRITE_HID_DATA2: u32 = 20;
    pub const SET_HID_REPORT: u32 = 21;
    pub const GET_HID_REPORT: u32 = 22;
    pub const TRIGGER_CONNECTION: u32 = 23;
    pub const ADD_PAIRED_DEVICE_INFO: u32 = 24;
    pub const GET_PAIRED_DEVICE_INFO: u32 = 25;
    pub const FINALIZE_HID: u32 = 26;
    pub const GET_HID_EVENT_INFO: u32 = 27;
    pub const SET_TSI: u32 = 28;
    pub const ENABLE_BURST_MODE: u32 = 29;
    pub const SET_ZERO_RETRANSMISSION: u32 = 30;
    pub const ENABLE_MC_MODE: u32 = 31;
    pub const ENABLE_LLR_SCAN: u32 = 32;
    pub const DISABLE_LLR_SCAN: u32 = 33;
    pub const ENABLE_RADIO: u32 = 34;
    pub const SET_VISIBILITY: u32 = 35;
    pub const ENABLE_TBFC_SCAN: u32 = 36;
    pub const REGISTER_HID_REPORT_EVENT: u32 = 37;
    pub const GET_HID_REPORT_EVENT_INFO: u32 = 38;
    pub const GET_LATEST_PLR: u32 = 39;
    pub const GET_PENDING_CONNECTIONS: u32 = 40;
    pub const GET_CHANNEL_MAP: u32 = 41;
    pub const ENABLE_TX_POWER_BOOST_SETTING: u32 = 42;
    pub const IS_TX_POWER_BOOST_SETTING_ENABLED: u32 = 43;
    pub const ENABLE_AFH_SETTING: u32 = 44;
    pub const IS_AFH_SETTING_ENABLED: u32 = 45;
    pub const INITIALIZE_BLE: u32 = 46;
    pub const ENABLE_BLE: u32 = 47;
    pub const DISABLE_BLE: u32 = 48;
    pub const FINALIZE_BLE: u32 = 49;
    pub const SET_BLE_VISIBILITY: u32 = 50;
    pub const IS_BLUETOOTH_ENABLED: u32 = 100;
    pub const ACQUIRE_AUDIO_EVENT: u32 = 128;
    pub const IS_MANUFACTURING_MODE: u32 = 256;
    pub const EMULATE_BLUETOOTH_CRASH: u32 = 257;
    pub const GET_BLE_CHANNEL_MAP: u32 = 258;
}

/// IBluetoothUser service ("bt").
///
/// Corresponds to `IBluetoothUser` in upstream btdrv.cpp.
pub struct IBluetoothUser {
    // In upstream: service_context and register_event (KEvent)
    // TODO: add when kernel event support is available
}

impl IBluetoothUser {
    pub fn new() -> Self {
        // Upstream creates register_event = service_context.CreateEvent("BT:RegisterEvent")
        Self {}
    }

    /// RegisterBleEvent (cmd 9).
    ///
    /// Corresponds to `IBluetoothUser::RegisterBleEvent` in upstream btdrv.cpp.
    /// Returns the register_event's readable event handle.
    pub fn register_ble_event(&self) {
        log::warn!("(STUBBED) IBluetoothUser::register_ble_event called");
        // TODO: return register_event->GetReadableEvent()
    }
}

/// IBluetoothDriver service ("btdrv").
///
/// Corresponds to `IBluetoothDriver` in upstream btdrv.cpp.
/// Most commands are unimplemented (nullptr) in upstream.
pub struct IBluetoothDriver;

impl IBluetoothDriver {
    pub fn new() -> Self {
        Self
    }

    /// EnableRadio (cmd 34).
    ///
    /// Corresponds to `IBluetoothDriver::EnableRadio` in upstream btdrv.cpp.
    pub fn enable_radio(&self) {
        log::warn!("(STUBBED) IBluetoothDriver::enable_radio called");
    }
}

/// Registers "btdrv" and "bt" services.
///
/// Corresponds to `LoopProcess` in upstream btdrv.cpp.
pub fn loop_process() {
    log::debug!("BtDrv::LoopProcess called");
    // TODO: register "btdrv" -> IBluetoothDriver and "bt" -> IBluetoothUser with ServerManager
}
