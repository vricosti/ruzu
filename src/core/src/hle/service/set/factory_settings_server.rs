// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/set/factory_settings_server.h and .cpp
//!
//! IFactorySettingsServer service ("set:cal").
//!
//! All IPC commands are unimplemented (nullptr) in upstream.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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
pub struct IFactorySettingsServer {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IFactorySettingsServer {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (
                commands::GET_BLUETOOTH_BD_ADDRESS,
                None,
                "GetBluetoothBdAddress",
            ),
            (commands::GET_CONFIGURATION_ID1, None, "GetConfigurationId1"),
            (
                commands::GET_ACCELEROMETER_OFFSET,
                None,
                "GetAccelerometerOffset",
            ),
            (
                commands::GET_ACCELEROMETER_SCALE,
                None,
                "GetAccelerometerScale",
            ),
            (commands::GET_GYROSCOPE_OFFSET, None, "GetGyroscopeOffset"),
            (commands::GET_GYROSCOPE_SCALE, None, "GetGyroscopeScale"),
            (
                commands::GET_WIRELESS_LAN_MAC_ADDRESS,
                None,
                "GetWirelessLanMacAddress",
            ),
            (
                commands::GET_WIRELESS_LAN_COUNTRY_CODE_COUNT,
                None,
                "GetWirelessLanCountryCodeCount",
            ),
            (
                commands::GET_WIRELESS_LAN_COUNTRY_CODES,
                None,
                "GetWirelessLanCountryCodes",
            ),
            (commands::GET_SERIAL_NUMBER, None, "GetSerialNumber"),
            (
                commands::SET_INITIAL_SYSTEM_APPLET_PROGRAM_ID,
                None,
                "SetInitialSystemAppletProgramId",
            ),
            (
                commands::SET_OVERLAY_DISP_PROGRAM_ID,
                None,
                "SetOverlayDispProgramId",
            ),
            (commands::GET_BATTERY_LOT, None, "GetBatteryLot"),
            (
                commands::GET_ECI_DEVICE_CERTIFICATE,
                None,
                "GetEciDeviceCertificate",
            ),
            (
                commands::GET_ETICKET_DEVICE_CERTIFICATE,
                None,
                "GetEticketDeviceCertificate",
            ),
            (commands::GET_SSL_KEY, None, "GetSslKey"),
            (commands::GET_SSL_CERTIFICATE, None, "GetSslCertificate"),
            (commands::GET_GAME_CARD_KEY, None, "GetGameCardKey"),
            (
                commands::GET_GAME_CARD_CERTIFICATE,
                None,
                "GetGameCardCertificate",
            ),
            (commands::GET_ECI_DEVICE_KEY, None, "GetEciDeviceKey"),
            (
                commands::GET_ETICKET_DEVICE_KEY,
                None,
                "GetEticketDeviceKey",
            ),
            (commands::GET_SPEAKER_PARAMETER, None, "GetSpeakerParameter"),
            (commands::GET_LCD_VENDOR_ID, None, "GetLcdVendorId"),
            (
                commands::GET_ECI_DEVICE_CERTIFICATE2,
                None,
                "GetEciDeviceCertificate2",
            ),
            (commands::GET_ECI_DEVICE_KEY2, None, "GetEciDeviceKey2"),
            (commands::GET_AMIIBO_KEY, None, "GetAmiiboKey"),
            (
                commands::GET_AMIIBO_ECQV_CERTIFICATE,
                None,
                "GetAmiiboEcqvCertificate",
            ),
            (
                commands::GET_AMIIBO_ECDSA_CERTIFICATE,
                None,
                "GetAmiiboEcdsaCertificate",
            ),
            (
                commands::GET_AMIIBO_ECQV_BLS_KEY,
                None,
                "GetAmiiboEcqvBlsKey",
            ),
            (
                commands::GET_AMIIBO_ECQV_BLS_CERTIFICATE,
                None,
                "GetAmiiboEcqvBlsCertificate",
            ),
            (
                commands::GET_AMIIBO_ECQV_BLS_ROOT_CERTIFICATE,
                None,
                "GetAmiiboEcqvBlsRootCertificate",
            ),
            (
                commands::GET_USB_TYPE_C_POWER_SOURCE_CIRCUIT_VERSION,
                None,
                "GetUsbTypeCPowerSourceCircuitVersion",
            ),
            (
                commands::GET_ANALOG_STICK_MODULE_TYPE_L,
                None,
                "GetAnalogStickModuleTypeL",
            ),
            (
                commands::GET_ANALOG_STICK_MODEL_PARAMETER_L,
                None,
                "GetAnalogStickModelParameterL",
            ),
            (
                commands::GET_ANALOG_STICK_FACTORY_CALIBRATION_L,
                None,
                "GetAnalogStickFactoryCalibrationL",
            ),
            (
                commands::GET_ANALOG_STICK_MODULE_TYPE_R,
                None,
                "GetAnalogStickModuleTypeR",
            ),
            (
                commands::GET_ANALOG_STICK_MODEL_PARAMETER_R,
                None,
                "GetAnalogStickModelParameterR",
            ),
            (
                commands::GET_ANALOG_STICK_FACTORY_CALIBRATION_R,
                None,
                "GetAnalogStickFactoryCalibrationR",
            ),
            (
                commands::GET_CONSOLE_SIX_AXIS_SENSOR_MODULE_TYPE,
                None,
                "GetConsoleSixAxisSensorModuleType",
            ),
            (
                commands::GET_CONSOLE_SIX_AXIS_SENSOR_HORIZONTAL_OFFSET,
                None,
                "GetConsoleSixAxisSensorHorizontalOffset",
            ),
            (commands::GET_BATTERY_VERSION, None, "GetBatteryVersion"),
            (commands::GET_DEVICE_ID, None, "GetDeviceId"),
            (
                commands::GET_CONSOLE_SIX_AXIS_SENSOR_MOUNT_TYPE,
                None,
                "GetConsoleSixAxisSensorMountType",
            ),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IFactorySettingsServer {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "set:cal"
    }
}

impl ServiceFramework for IFactorySettingsServer {
    fn get_service_name(&self) -> &str {
        "set:cal"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
