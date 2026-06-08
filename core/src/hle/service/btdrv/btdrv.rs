// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/btdrv/btdrv.cpp
//!
//! IBluetoothDriver ("btdrv") and IBluetoothUser ("bt") services.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    service_context: crate::hle::service::kernel_helpers::ServiceContext,
    /// Handle for the register event. Returned by RegisterBleEvent.
    register_event_handle: u32,
}

impl IBluetoothUser {
    pub fn new() -> Self {
        let mut service_context =
            crate::hle::service::kernel_helpers::ServiceContext::new("bt".to_string());
        let register_event_handle = service_context.create_event("BT:RegisterEvent".to_string());
        Self {
            handlers: build_handler_map(&[
                (
                    bt_commands::LE_CLIENT_READ_CHARACTERISTIC,
                    None,
                    "LeClientReadCharacteristic",
                ),
                (
                    bt_commands::LE_CLIENT_READ_DESCRIPTOR,
                    None,
                    "LeClientReadDescriptor",
                ),
                (
                    bt_commands::LE_CLIENT_WRITE_CHARACTERISTIC,
                    None,
                    "LeClientWriteCharacteristic",
                ),
                (
                    bt_commands::LE_CLIENT_WRITE_DESCRIPTOR,
                    None,
                    "LeClientWriteDescriptor",
                ),
                (
                    bt_commands::LE_CLIENT_REGISTER_NOTIFICATION,
                    None,
                    "LeClientRegisterNotification",
                ),
                (
                    bt_commands::LE_CLIENT_DEREGISTER_NOTIFICATION,
                    None,
                    "LeClientDeregisterNotification",
                ),
                (bt_commands::SET_LE_RESPONSE, None, "SetLeResponse"),
                (bt_commands::LE_SEND_INDICATION, None, "LeSendIndication"),
                (bt_commands::GET_LE_EVENT_INFO, None, "GetLeEventInfo"),
                (
                    bt_commands::REGISTER_BLE_EVENT,
                    Some(IBluetoothUser::register_ble_event_handler),
                    "RegisterBleEvent",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
            service_context,
            register_event_handle,
        }
    }

    /// RegisterBleEvent (cmd 9).
    ///
    /// Corresponds to `IBluetoothUser::RegisterBleEvent` in upstream btdrv.cpp.
    /// Returns the register_event's readable event handle.
    pub fn register_ble_event(
        &self,
    ) -> Option<std::sync::Arc<crate::hle::service::os::event::Event>> {
        log::warn!("(STUBBED) IBluetoothUser::register_ble_event called");
        self.service_context.get_event(self.register_event_handle)
    }

    fn register_ble_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IBluetoothUser) };
        let handle = service
            .register_ble_event()
            .and_then(|event| event.copy_handle(ctx))
            .unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }
}

impl SessionRequestHandler for IBluetoothUser {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "bt"
    }
}

impl ServiceFramework for IBluetoothUser {
    fn get_service_name(&self) -> &str {
        "bt"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// IBluetoothDriver service ("btdrv").
///
/// Corresponds to `IBluetoothDriver` in upstream btdrv.cpp.
/// Most commands are unimplemented (nullptr) in upstream.
pub struct IBluetoothDriver {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IBluetoothDriver {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    btdrv_commands::INITIALIZE_BLUETOOTH_DRIVER,
                    None,
                    "InitializeBluetoothDriver",
                ),
                (
                    btdrv_commands::INITIALIZE_BLUETOOTH,
                    None,
                    "InitializeBluetooth",
                ),
                (btdrv_commands::ENABLE_BLUETOOTH, None, "EnableBluetooth"),
                (btdrv_commands::DISABLE_BLUETOOTH, None, "DisableBluetooth"),
                (
                    btdrv_commands::FINALIZE_BLUETOOTH,
                    None,
                    "FinalizeBluetooth",
                ),
                (
                    btdrv_commands::GET_ADAPTER_PROPERTIES,
                    None,
                    "GetAdapterProperties",
                ),
                (
                    btdrv_commands::GET_ADAPTER_PROPERTY,
                    None,
                    "GetAdapterProperty",
                ),
                (
                    btdrv_commands::SET_ADAPTER_PROPERTY,
                    None,
                    "SetAdapterProperty",
                ),
                (btdrv_commands::START_INQUIRY, None, "StartInquiry"),
                (btdrv_commands::STOP_INQUIRY, None, "StopInquiry"),
                (btdrv_commands::CREATE_BOND, None, "CreateBond"),
                (btdrv_commands::REMOVE_BOND, None, "RemoveBond"),
                (btdrv_commands::CANCEL_BOND, None, "CancelBond"),
                (
                    btdrv_commands::RESPOND_TO_PIN_REQUEST,
                    None,
                    "RespondToPinRequest",
                ),
                (
                    btdrv_commands::RESPOND_TO_SSP_REQUEST,
                    None,
                    "RespondToSspRequest",
                ),
                (btdrv_commands::GET_EVENT_INFO, None, "GetEventInfo"),
                (btdrv_commands::INITIALIZE_HID, None, "InitializeHid"),
                (
                    btdrv_commands::OPEN_HID_CONNECTION,
                    None,
                    "OpenHidConnection",
                ),
                (
                    btdrv_commands::CLOSE_HID_CONNECTION,
                    None,
                    "CloseHidConnection",
                ),
                (btdrv_commands::WRITE_HID_DATA, None, "WriteHidData"),
                (btdrv_commands::WRITE_HID_DATA2, None, "WriteHidData2"),
                (btdrv_commands::SET_HID_REPORT, None, "SetHidReport"),
                (btdrv_commands::GET_HID_REPORT, None, "GetHidReport"),
                (
                    btdrv_commands::TRIGGER_CONNECTION,
                    None,
                    "TriggerConnection",
                ),
                (
                    btdrv_commands::ADD_PAIRED_DEVICE_INFO,
                    None,
                    "AddPairedDeviceInfo",
                ),
                (
                    btdrv_commands::GET_PAIRED_DEVICE_INFO,
                    None,
                    "GetPairedDeviceInfo",
                ),
                (btdrv_commands::FINALIZE_HID, None, "FinalizeHid"),
                (btdrv_commands::GET_HID_EVENT_INFO, None, "GetHidEventInfo"),
                (btdrv_commands::SET_TSI, None, "SetTsi"),
                (btdrv_commands::ENABLE_BURST_MODE, None, "EnableBurstMode"),
                (
                    btdrv_commands::SET_ZERO_RETRANSMISSION,
                    None,
                    "SetZeroRetransmission",
                ),
                (btdrv_commands::ENABLE_MC_MODE, None, "EnableMcMode"),
                (btdrv_commands::ENABLE_LLR_SCAN, None, "EnableLlrScan"),
                (btdrv_commands::DISABLE_LLR_SCAN, None, "DisableLlrScan"),
                (
                    btdrv_commands::ENABLE_RADIO,
                    Some(IBluetoothDriver::enable_radio_handler),
                    "EnableRadio",
                ),
                (btdrv_commands::SET_VISIBILITY, None, "SetVisibility"),
                (btdrv_commands::ENABLE_TBFC_SCAN, None, "EnableTbfcScan"),
                (
                    btdrv_commands::REGISTER_HID_REPORT_EVENT,
                    None,
                    "RegisterHidReportEvent",
                ),
                (
                    btdrv_commands::GET_HID_REPORT_EVENT_INFO,
                    None,
                    "GetHidReportEventInfo",
                ),
                (btdrv_commands::GET_LATEST_PLR, None, "GetLatestPlr"),
                (
                    btdrv_commands::GET_PENDING_CONNECTIONS,
                    None,
                    "GetPendingConnections",
                ),
                (btdrv_commands::GET_CHANNEL_MAP, None, "GetChannelMap"),
                (
                    btdrv_commands::ENABLE_TX_POWER_BOOST_SETTING,
                    None,
                    "EnableTxPowerBoostSetting",
                ),
                (
                    btdrv_commands::IS_TX_POWER_BOOST_SETTING_ENABLED,
                    None,
                    "IsTxPowerBoostSettingEnabled",
                ),
                (btdrv_commands::ENABLE_AFH_SETTING, None, "EnableAfhSetting"),
                (
                    btdrv_commands::IS_AFH_SETTING_ENABLED,
                    None,
                    "IsAfhSettingEnabled",
                ),
                (btdrv_commands::INITIALIZE_BLE, None, "InitializeBle"),
                (btdrv_commands::ENABLE_BLE, None, "EnableBle"),
                (btdrv_commands::DISABLE_BLE, None, "DisableBle"),
                (btdrv_commands::FINALIZE_BLE, None, "FinalizeBle"),
                (btdrv_commands::SET_BLE_VISIBILITY, None, "SetBleVisibility"),
                (51, None, "SetBleConnectionParameter"),
                (52, None, "SetBleDefaultConnectionParameter"),
                (53, None, "SetBleAdvertiseData"),
                (54, None, "SetBleAdvertiseParameter"),
                (55, None, "StartBleScan"),
                (56, None, "StopBleScan"),
                (57, None, "AddBleScanFilterCondition"),
                (58, None, "DeleteBleScanFilterCondition"),
                (59, None, "DeleteBleScanFilter"),
                (60, None, "ClearBleScanFilters"),
                (61, None, "EnableBleScanFilter"),
                (62, None, "RegisterGattClient"),
                (63, None, "UnregisterGattClient"),
                (64, None, "UnregisterAllGattClients"),
                (65, None, "ConnectGattServer"),
                (66, None, "CancelConnectGattServer"),
                (67, None, "DisconnectGattServer"),
                (68, None, "GetGattAttribute"),
                (69, None, "GetGattService"),
                (70, None, "ConfigureAttMtu"),
                (71, None, "RegisterGattServer"),
                (72, None, "UnregisterGattServer"),
                (73, None, "ConnectGattClient"),
                (74, None, "DisconnectGattClient"),
                (75, None, "AddGattService"),
                (76, None, "EnableGattService"),
                (77, None, "AddGattCharacteristic"),
                (78, None, "AddGattDescriptor"),
                (79, None, "GetBleManagedEventInfo"),
                (80, None, "GetGattFirstCharacteristic"),
                (81, None, "GetGattNextCharacteristic"),
                (82, None, "GetGattFirstDescriptor"),
                (83, None, "GetGattNextDescriptor"),
                (84, None, "RegisterGattManagedDataPath"),
                (85, None, "UnregisterGattManagedDataPath"),
                (86, None, "RegisterGattHidDataPath"),
                (87, None, "UnregisterGattHidDataPath"),
                (88, None, "RegisterGattDataPath"),
                (89, None, "UnregisterGattDataPath"),
                (90, None, "ReadGattCharacteristic"),
                (91, None, "ReadGattDescriptor"),
                (92, None, "WriteGattCharacteristic"),
                (93, None, "WriteGattDescriptor"),
                (94, None, "RegisterGattNotification"),
                (95, None, "UnregisterGattNotification"),
                (96, None, "GetLeHidEventInfo"),
                (97, None, "RegisterBleHidEvent"),
                (98, None, "SetBleScanParameter"),
                (99, None, "MoveToSecondaryPiconet"),
                (
                    btdrv_commands::IS_BLUETOOTH_ENABLED,
                    None,
                    "IsBluetoothEnabled",
                ),
                (
                    btdrv_commands::ACQUIRE_AUDIO_EVENT,
                    None,
                    "AcquireAudioEvent",
                ),
                (129, None, "GetAudioEventInfo"),
                (130, None, "OpenAudioConnection"),
                (131, None, "CloseAudioConnection"),
                (132, None, "OpenAudioOut"),
                (133, None, "CloseAudioOut"),
                (134, None, "AcquireAudioOutStateChangedEvent"),
                (135, None, "StartAudioOut"),
                (136, None, "StopAudioOut"),
                (137, None, "GetAudioOutState"),
                (138, None, "GetAudioOutFeedingCodec"),
                (139, None, "GetAudioOutFeedingParameter"),
                (140, None, "AcquireAudioOutBufferAvailableEvent"),
                (141, None, "SendAudioData"),
                (142, None, "AcquireAudioControlInputStateChangedEvent"),
                (143, None, "GetAudioControlInputState"),
                (144, None, "AcquireAudioConnectionStateChangedEvent"),
                (145, None, "GetConnectedAudioDevice"),
                (146, None, "CloseAudioControlInput"),
                (147, None, "RegisterAudioControlNotification"),
                (148, None, "SendAudioControlPassthroughCommand"),
                (149, None, "SendAudioControlSetAbsoluteVolumeCommand"),
                (150, None, "AcquireAudioSinkVolumeLocallyChangedEvent"),
                (
                    151,
                    None,
                    "AcquireAudioSinkVolumeUpdateRequestCompletedEvent",
                ),
                (152, None, "GetAudioSinkVolume"),
                (153, None, "RequestUpdateAudioSinkVolume"),
                (154, None, "IsAudioSinkVolumeSupported"),
                (
                    btdrv_commands::IS_MANUFACTURING_MODE,
                    None,
                    "IsManufacturingMode",
                ),
                (
                    btdrv_commands::EMULATE_BLUETOOTH_CRASH,
                    None,
                    "EmulateBluetoothCrash",
                ),
                (
                    btdrv_commands::GET_BLE_CHANNEL_MAP,
                    None,
                    "GetBleChannelMap",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// EnableRadio (cmd 34).
    ///
    /// Corresponds to `IBluetoothDriver::EnableRadio` in upstream btdrv.cpp.
    pub fn enable_radio(&self) {
        log::warn!("(STUBBED) IBluetoothDriver::enable_radio called");
    }

    fn enable_radio_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IBluetoothDriver) };
        service.enable_radio();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for IBluetoothDriver {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "btdrv"
    }
}

impl ServiceFramework for IBluetoothDriver {
    fn get_service_name(&self) -> &str {
        "btdrv"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "btdrv" and "bt" services.
///
/// Corresponds to `LoopProcess` in upstream btdrv.cpp.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;

    log::debug!("BtDrv::LoopProcess called");

    let server_manager = ServerManager::new_shared(system);
    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "btdrv",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IBluetoothDriver::new())
            }),
            64,
        );
        server_manager.register_named_service(
            "bt",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(IBluetoothUser::new()) }),
            64,
        );
    }
    ServerManager::run_server_shared(server_manager);
}
