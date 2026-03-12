// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/set/factory_settings_server.h and .cpp
//!
//! IFactorySettingsServer service ("set:cal").
//!
//! All IPC commands are unimplemented (nullptr) in upstream.

/// IPC command table for IFactorySettingsServer ("set:cal").
///
/// Corresponds to the function table in upstream factory_settings_server.cpp.
pub mod commands {
    pub const GET_BLUETOOTH_BD_ADDRESS: u32 = 0;
    pub const GET_CONFIGURATION_ID1: u32 = 1;
    pub const GET_ACCELEROMETER_OFFSET: u32 = 2;
    pub const GET_ACCELEROMETER_SCALE: u32 = 3;
    pub const GET_GYROSCOPE_OFFSET: u32 = 4;
    pub const GET_GYROSCOPE_SCALE: u32 = 5;
    pub const GET_WIRELESS_LAN_MAC_ADDRESS: u32 = 6;
    pub const GET_WIRELESS_LAN_COUNTRY_CODE_COUNT: u32 = 7;
    pub const GET_WIRELESS_LAN_COUNTRY_CODES: u32 = 8;
    pub const GET_SERIAL_NUMBER: u32 = 9;
    pub const SET_INITIAL_SYSTEM_APPLET_PROGRAM_ID: u32 = 10;
    pub const SET_OVERLAY_DISP_PROGRAM_ID: u32 = 11;
    pub const GET_BATTERY_LOT: u32 = 12;
    pub const GET_ECI_DEVICE_CERTIFICATE: u32 = 14;
    pub const GET_ETICKET_DEVICE_CERTIFICATE: u32 = 15;
    pub const GET_SSL_KEY: u32 = 16;
    pub const GET_SSL_CERTIFICATE: u32 = 17;
    pub const GET_GAME_CARD_KEY: u32 = 18;
    pub const GET_GAME_CARD_CERTIFICATE: u32 = 19;
    pub const GET_ECI_DEVICE_KEY: u32 = 20;
    pub const GET_ETICKET_DEVICE_KEY: u32 = 21;
    pub const GET_SPEAKER_PARAMETER: u32 = 22;
    pub const GET_LCD_VENDOR_ID: u32 = 23;
    pub const GET_ECI_DEVICE_CERTIFICATE2: u32 = 24;
    pub const GET_ECI_DEVICE_KEY2: u32 = 25;
    pub const GET_AMIIBO_KEY: u32 = 26;
    pub const GET_AMIIBO_ECQV_CERTIFICATE: u32 = 27;
    pub const GET_AMIIBO_ECDSA_CERTIFICATE: u32 = 28;
    pub const GET_AMIIBO_ECQV_BLS_KEY: u32 = 29;
    pub const GET_AMIIBO_ECQV_BLS_CERTIFICATE: u32 = 30;
    pub const GET_AMIIBO_ECQV_BLS_ROOT_CERTIFICATE: u32 = 31;
    pub const GET_USB_TYPE_C_POWER_SOURCE_CIRCUIT_VERSION: u32 = 32;
    pub const GET_ANALOG_STICK_MODULE_TYPE_L: u32 = 33;
    pub const GET_ANALOG_STICK_MODEL_PARAMETER_L: u32 = 34;
    pub const GET_ANALOG_STICK_FACTORY_CALIBRATION_L: u32 = 35;
    pub const GET_ANALOG_STICK_MODULE_TYPE_R: u32 = 36;
    pub const GET_ANALOG_STICK_MODEL_PARAMETER_R: u32 = 37;
    pub const GET_ANALOG_STICK_FACTORY_CALIBRATION_R: u32 = 38;
    pub const GET_CONSOLE_SIX_AXIS_SENSOR_MODULE_TYPE: u32 = 39;
    pub const GET_CONSOLE_SIX_AXIS_SENSOR_HORIZONTAL_OFFSET: u32 = 40;
    pub const GET_BATTERY_VERSION: u32 = 41;
    pub const GET_DEVICE_ID: u32 = 42;
    pub const GET_CONSOLE_SIX_AXIS_SENSOR_MOUNT_TYPE: u32 = 43;
}

/// IFactorySettingsServer -- "set:cal" service.
///
/// Corresponds to `IFactorySettingsServer` in upstream.
/// All commands are unimplemented stubs in upstream (nullptr handlers).
pub struct IFactorySettingsServer;

impl IFactorySettingsServer {
    pub fn new() -> Self {
        Self
    }
}
