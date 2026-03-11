// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/btdrv/btdrv.cpp
//!
//! IBluetoothDriver ("btdrv") and IBluetoothUser ("bt") services.

/// IPC command IDs for IBluetoothUser ("bt")
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

/// IPC command IDs for IBluetoothDriver ("btdrv") - selected key commands
pub mod btdrv_commands {
    pub const INITIALIZE_BLUETOOTH_DRIVER: u32 = 0;
    pub const ENABLE_RADIO: u32 = 34;
    pub const IS_BLUETOOTH_ENABLED: u32 = 100;
    pub const IS_MANUFACTURING_MODE: u32 = 256;
    pub const EMULATE_BLUETOOTH_CRASH: u32 = 257;
    pub const GET_BLE_CHANNEL_MAP: u32 = 258;
}

/// IBluetoothUser service ("bt").
pub struct IBluetoothUser {
    // TODO: service_context, register_event
}

impl IBluetoothUser {
    pub fn new() -> Self {
        Self {}
    }

    /// Stubbed: RegisterBleEvent (cmd 9)
    pub fn register_ble_event(&self) {
        log::warn!("(STUBBED) IBluetoothUser::register_ble_event called");
        // TODO: return event handle
    }
}

/// IBluetoothDriver service ("btdrv").
/// Most commands are unimplemented stubs in upstream.
pub struct IBluetoothDriver;

impl IBluetoothDriver {
    pub fn new() -> Self {
        Self
    }

    /// Stubbed: EnableRadio (cmd 34)
    pub fn enable_radio(&self) {
        log::warn!("(STUBBED) IBluetoothDriver::enable_radio called");
    }
}

/// Registers "btdrv" and "bt" services.
///
/// Corresponds to `LoopProcess` in upstream `btdrv.cpp`.
pub fn loop_process() {
    // TODO: register "btdrv" -> IBluetoothDriver and "bt" -> IBluetoothUser with ServerManager
}
