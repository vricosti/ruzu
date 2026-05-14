// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nifm/nifm.cpp
//!
//! Network Interface Manager services.

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::{ErrorModule, ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use crate::internal_network::network::get_host_ipv4_address;

/// Upstream: `ResultPendingConnection{ErrorModule::NIFM, 111}`
pub const RESULT_PENDING_CONNECTION: ResultCode =
    ResultCode::from_module_description(ErrorModule::NIFM, 111);

/// Upstream: `ResultNetworkCommunicationDisabled{ErrorModule::NIFM, 1111}`
pub const RESULT_NETWORK_COMMUNICATION_DISABLED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NIFM, 1111);

/// RequestState enum. Upstream: `RequestState` in `nifm.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum RequestState {
    NotSubmitted = 1,
    // Invalid = 1, // intentional duplicate in upstream
    OnHold = 2,
    Accepted = 3,
    Blocking = 4,
}

/// NetworkInterfaceType enum. Upstream: `NetworkInterfaceType` in `nifm.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum NetworkInterfaceType {
    Invalid = 0,
    WiFiIeee80211 = 1,
    Ethernet = 2,
}

/// InternetConnectionStatus enum. Upstream: `InternetConnectionStatus` in `nifm.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum InternetConnectionStatus {
    ConnectingUnknown1 = 0,
    ConnectingUnknown2 = 1,
    ConnectingUnknown3 = 2,
    ConnectingUnknown4 = 3,
    Connected = 4,
}

#[derive(Clone, Copy)]
#[repr(C)]
struct InternetConnectionStatusOutput {
    interface_type: u8,
    wifi_strength: u8,
    state: u8,
}

const _: () = assert!(std::mem::size_of::<InternetConnectionStatusOutput>() == 0x3);

/// IPC command IDs for IGeneralService
pub mod general_service_commands {
    pub const GET_CLIENT_ID: u32 = 1;
    pub const CREATE_SCAN_REQUEST: u32 = 2;
    pub const CREATE_REQUEST: u32 = 4;
    pub const GET_CURRENT_NETWORK_PROFILE: u32 = 5;
    pub const REMOVE_NETWORK_PROFILE: u32 = 10;
    pub const GET_CURRENT_IP_ADDRESS: u32 = 12;
    pub const CREATE_TEMPORARY_NETWORK_PROFILE: u32 = 14;
    pub const GET_CURRENT_IP_CONFIG_INFO: u32 = 15;
    pub const IS_WIRELESS_COMMUNICATION_ENABLED: u32 = 17;
    pub const GET_INTERNET_CONNECTION_STATUS: u32 = 18;
    pub const IS_ETHERNET_COMMUNICATION_ENABLED: u32 = 20;
    pub const IS_ANY_INTERNET_REQUEST_ACCEPTED: u32 = 21;
    pub const IS_ANY_FOREGROUND_REQUEST_ACCEPTED: u32 = 22;
}

/// IPC command IDs for IRequest
pub mod request_commands {
    pub const GET_REQUEST_STATE: u32 = 0;
    pub const GET_RESULT: u32 = 1;
    pub const GET_SYSTEM_EVENT_READABLE_HANDLES: u32 = 2;
    pub const CANCEL: u32 = 3;
    pub const SUBMIT: u32 = 4;
    pub const SET_REQUIREMENT_PRESET: u32 = 6;
    pub const SET_CONNECTION_CONFIRMATION_OPTION: u32 = 11;
    pub const GET_APPLET_INFO: u32 = 21;
}

/// IPC command IDs for NetworkInterface ("nifm:a", "nifm:s", "nifm:u")
pub mod interface_commands {
    pub const CREATE_GENERAL_SERVICE_OLD: u32 = 4;
    pub const CREATE_GENERAL_SERVICE: u32 = 5;
}

fn push_interface_response(ctx: &mut HLERequestContext, object: SessionRequestHandlerPtr) {
    let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
    rb.push_result(RESULT_SUCCESS);
    rb.push_ipc_interface(object);
}

/// IScanRequest.
pub struct IScanRequest {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IScanRequest {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, None, "Submit"),
            (1, None, "IsProcessing"),
            (2, None, "GetResult"),
            (3, None, "GetSystemEventReadableHandle"),
            (4, None, "SetChannels"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IScanRequest {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IScanRequest"
    }
}

impl ServiceFramework for IScanRequest {
    fn get_service_name(&self) -> &str {
        "IScanRequest"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// IRequest.
pub struct IRequest {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    state: RequestState,
    service_context: crate::hle::service::kernel_helpers::ServiceContext,
    event1_handle: u32,
    event2_handle: u32,
}

impl IRequest {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (
                request_commands::GET_REQUEST_STATE,
                Some(IRequest::get_request_state_handler),
                "GetRequestState",
            ),
            (
                request_commands::GET_RESULT,
                Some(IRequest::get_result_handler),
                "GetResult",
            ),
            (
                request_commands::GET_SYSTEM_EVENT_READABLE_HANDLES,
                Some(IRequest::get_system_event_readable_handles_handler),
                "GetSystemEventReadableHandles",
            ),
            (
                request_commands::CANCEL,
                Some(IRequest::cancel_handler),
                "Cancel",
            ),
            (
                request_commands::SUBMIT,
                Some(IRequest::submit_handler),
                "Submit",
            ),
            (5, None, "SetRequirement"),
            (
                request_commands::SET_REQUIREMENT_PRESET,
                Some(IRequest::set_requirement_preset_handler),
                "SetRequirementPreset",
            ),
            (8, None, "SetPriority"),
            (9, None, "SetNetworkProfileId"),
            (10, None, "SetRejectable"),
            (
                request_commands::SET_CONNECTION_CONFIRMATION_OPTION,
                Some(IRequest::set_connection_confirmation_option_handler),
                "SetConnectionConfirmationOption",
            ),
            (12, None, "SetPersistent"),
            (13, None, "SetInstant"),
            (14, None, "SetSustainable"),
            (15, None, "SetRawPriority"),
            (16, None, "SetGreedy"),
            (17, None, "SetSharable"),
            (18, None, "SetRequirementByRevision"),
            (19, None, "GetRequirement"),
            (20, None, "GetRevision"),
            (
                request_commands::GET_APPLET_INFO,
                Some(IRequest::get_applet_info_handler),
                "GetAppletInfo",
            ),
            (22, None, "GetAdditionalInfo"),
            (23, None, "SetKeptInSleep"),
            (24, None, "RegisterSocketDescriptor"),
            (25, None, "UnregisterSocketDescriptor"),
        ]);

        let mut service_context =
            crate::hle::service::kernel_helpers::ServiceContext::new("IRequest".to_string());
        let event1_handle = service_context.create_event("IRequest:Event1".to_string());
        let event2_handle = service_context.create_event("IRequest:Event2".to_string());

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            state: RequestState::NotSubmitted,
            service_context,
            event1_handle,
            event2_handle,
        }
    }

    fn update_state(&mut self, new_state: RequestState) {
        self.state = new_state;
        if let Some(event) = self.service_context.get_event(self.event1_handle) {
            event.signal();
        }
    }

    fn submit_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &mut *(std::ptr::addr_of!(*this).cast::<IRequest>().cast_mut()) };
        log::debug!("(STUBBED) IRequest::Submit called");

        if this.state == RequestState::NotSubmitted {
            this.update_state(RequestState::OnHold);
        }

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_request_state_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const IRequest) };
        log::debug!("(STUBBED) IRequest::GetRequestState called");

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(this.state as u32);
    }

    fn get_result_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &mut *(std::ptr::addr_of!(*this).cast::<IRequest>().cast_mut()) };
        log::debug!("(STUBBED) IRequest::GetResult called");

        let has_connection = get_host_ipv4_address().is_some();
        let result = match this.state {
            RequestState::NotSubmitted => {
                if has_connection {
                    RESULT_SUCCESS
                } else {
                    RESULT_NETWORK_COMMUNICATION_DISABLED
                }
            }
            RequestState::OnHold => {
                if has_connection {
                    this.update_state(RequestState::Accepted);
                } else {
                    this.update_state(RequestState::NotSubmitted);
                }
                RESULT_PENDING_CONNECTION
            }
            _ => RESULT_SUCCESS,
        };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    fn get_system_event_readable_handles_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const IRequest) };
        log::warn!("(STUBBED) IRequest::GetSystemEventReadableHandles called");

        let event1 = this
            .service_context
            .get_event(this.event1_handle)
            .and_then(|event| event.copy_handle(ctx))
            .unwrap_or(0);
        let event2 = this
            .service_context
            .get_event(this.event2_handle)
            .and_then(|event| event.copy_handle(ctx))
            .unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 2, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(event1);
        rb.push_copy_objects(event2);
    }

    fn cancel_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) IRequest::Cancel called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_requirement_preset_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let param = rp.pop_u32();
        log::warn!(
            "(STUBBED) IRequest::SetRequirementPreset called, param={}",
            param
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_connection_confirmation_option_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) IRequest::SetConnectionConfirmationOption called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_applet_info_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) IRequest::GetAppletInfo called");
        let out = vec![0u8; ctx.get_write_buffer_size(0)];
        ctx.write_buffer(&out, 0);

        let mut rb = ResponseBuilder::new(ctx, 5, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0);
        rb.push_u32(0);
        rb.push_u32(0);
    }
}

impl Drop for IRequest {
    fn drop(&mut self) {
        self.service_context.close_event(self.event1_handle);
        self.service_context.close_event(self.event2_handle);
    }
}

impl SessionRequestHandler for IRequest {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IRequest"
    }
}

impl ServiceFramework for IRequest {
    fn get_service_name(&self) -> &str {
        "IRequest"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// INetworkProfile.
pub struct INetworkProfile {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl INetworkProfile {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, None, "Update"),
            (1, None, "PersistOld"),
            (2, None, "Persist"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for INetworkProfile {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "INetworkProfile"
    }
}

impl ServiceFramework for INetworkProfile {
    fn get_service_name(&self) -> &str {
        "INetworkProfile"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// IGeneralService.
pub struct IGeneralService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IGeneralService {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (
                general_service_commands::GET_CLIENT_ID,
                Some(IGeneralService::get_client_id_handler),
                "GetClientId",
            ),
            (
                general_service_commands::CREATE_SCAN_REQUEST,
                Some(IGeneralService::create_scan_request_handler),
                "CreateScanRequest",
            ),
            (
                general_service_commands::CREATE_REQUEST,
                Some(IGeneralService::create_request_handler),
                "CreateRequest",
            ),
            (
                general_service_commands::GET_CURRENT_NETWORK_PROFILE,
                Some(IGeneralService::get_current_network_profile_handler),
                "GetCurrentNetworkProfile",
            ),
            (6, None, "EnumerateNetworkInterfaces"),
            (7, None, "EnumerateNetworkProfiles"),
            (8, None, "GetNetworkProfile"),
            (9, None, "SetNetworkProfile"),
            (
                general_service_commands::REMOVE_NETWORK_PROFILE,
                Some(IGeneralService::remove_network_profile_handler),
                "RemoveNetworkProfile",
            ),
            (11, None, "GetScanDataOld"),
            (
                general_service_commands::GET_CURRENT_IP_ADDRESS,
                Some(IGeneralService::get_current_ip_address_handler),
                "GetCurrentIpAddress",
            ),
            (13, None, "GetCurrentAccessPointOld"),
            (
                general_service_commands::CREATE_TEMPORARY_NETWORK_PROFILE,
                Some(IGeneralService::create_temporary_network_profile_handler),
                "CreateTemporaryNetworkProfile",
            ),
            (
                general_service_commands::GET_CURRENT_IP_CONFIG_INFO,
                Some(IGeneralService::get_current_ip_config_info_handler),
                "GetCurrentIpConfigInfo",
            ),
            (16, None, "SetWirelessCommunicationEnabled"),
            (
                general_service_commands::IS_WIRELESS_COMMUNICATION_ENABLED,
                Some(IGeneralService::is_wireless_communication_enabled_handler),
                "IsWirelessCommunicationEnabled",
            ),
            (
                general_service_commands::GET_INTERNET_CONNECTION_STATUS,
                Some(IGeneralService::get_internet_connection_status_handler),
                "GetInternetConnectionStatus",
            ),
            (19, None, "SetEthernetCommunicationEnabled"),
            (
                general_service_commands::IS_ETHERNET_COMMUNICATION_ENABLED,
                Some(IGeneralService::is_ethernet_communication_enabled_handler),
                "IsEthernetCommunicationEnabled",
            ),
            (
                general_service_commands::IS_ANY_INTERNET_REQUEST_ACCEPTED,
                Some(IGeneralService::is_any_internet_request_accepted_handler),
                "IsAnyInternetRequestAccepted",
            ),
            (
                general_service_commands::IS_ANY_FOREGROUND_REQUEST_ACCEPTED,
                Some(IGeneralService::is_any_foreground_request_accepted_handler),
                "IsAnyForegroundRequestAccepted",
            ),
            (23, None, "PutToSleep"),
            (24, None, "WakeUp"),
            (25, None, "GetSsidListVersion"),
            (26, None, "SetExclusiveClient"),
            (27, None, "GetDefaultIpSetting"),
            (28, None, "SetDefaultIpSetting"),
            (29, None, "SetWirelessCommunicationEnabledForTest"),
            (30, None, "SetEthernetCommunicationEnabledForTest"),
            (31, None, "GetTelemetorySystemEventReadableHandle"),
            (32, None, "GetTelemetryInfo"),
            (33, None, "ConfirmSystemAvailability"),
            (34, None, "SetBackgroundRequestEnabled"),
            (35, None, "GetScanData"),
            (36, None, "GetCurrentAccessPoint"),
            (37, None, "Shutdown"),
            (38, None, "GetAllowedChannels"),
            (39, None, "NotifyApplicationSuspended"),
            (40, None, "SetAcceptableNetworkTypeFlag"),
            (41, None, "GetAcceptableNetworkTypeFlag"),
            (42, None, "NotifyConnectionStateChanged"),
            (43, None, "SetWowlDelayedWakeTime"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_client_id_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) IGeneralService::GetClientId called");
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(1);
    }

    fn create_scan_request_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IGeneralService::CreateScanRequest called");
        push_interface_response(ctx, Arc::new(IScanRequest::new()));
    }

    fn create_request_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IGeneralService::CreateRequest called");
        push_interface_response(ctx, Arc::new(IRequest::new()));
    }

    fn get_current_network_profile_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) IGeneralService::GetCurrentNetworkProfile called");
        let out = vec![0u8; ctx.get_write_buffer_size(0)];
        ctx.write_buffer(&out, 0);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn remove_network_profile_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) IGeneralService::RemoveNetworkProfile called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_current_ip_address_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) IGeneralService::GetCurrentIpAddress called");
        let ipv4 = get_host_ipv4_address().unwrap_or([0, 0, 0, 0]);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&ipv4);
    }

    fn create_temporary_network_profile_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IGeneralService::CreateTemporaryNetworkProfile called");

        let mut uuid = [0u8; 16];
        let buffer = ctx.read_buffer(0);
        if buffer.len() >= 24 {
            uuid.copy_from_slice(&buffer[8..24]);
        }

        let mut rb = ResponseBuilder::new(ctx, 6, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(Arc::new(INetworkProfile::new()));
        rb.push_raw(&uuid);
    }

    fn get_current_ip_config_info_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) IGeneralService::GetCurrentIpConfigInfo called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        let zero = [0u8; 0x16];
        rb.push_raw_bytes(&zero);
    }

    fn is_wireless_communication_enabled_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) IGeneralService::IsWirelessCommunicationEnabled called");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u8(1);
    }

    fn get_internet_connection_status_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) IGeneralService::GetInternetConnectionStatus called");
        let out = InternetConnectionStatusOutput {
            interface_type: NetworkInterfaceType::WiFiIeee80211 as u8,
            wifi_strength: 3,
            state: InternetConnectionStatus::Connected as u8,
        };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&out);
    }

    fn is_ethernet_communication_enabled_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) IGeneralService::IsEthernetCommunicationEnabled called");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u8(if get_host_ipv4_address().is_some() {
            1
        } else {
            0
        });
    }

    fn is_any_internet_request_accepted_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::error!("(STUBBED) IGeneralService::IsAnyInternetRequestAccepted called");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u8(if get_host_ipv4_address().is_some() {
            1
        } else {
            0
        });
    }

    fn is_any_foreground_request_accepted_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) IGeneralService::IsAnyForegroundRequestAccepted called");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u8(0);
    }
}

impl SessionRequestHandler for IGeneralService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IGeneralService"
    }
}

impl ServiceFramework for IGeneralService {
    fn get_service_name(&self) -> &str {
        "IGeneralService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// NetworkInterface ("nifm:a", "nifm:s", "nifm:u").
pub struct NetworkInterface {
    name: String,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NetworkInterface {
    pub fn new(name: &str) -> Self {
        let handlers = build_handler_map(&[
            (
                interface_commands::CREATE_GENERAL_SERVICE_OLD,
                Some(NetworkInterface::create_general_service_old_handler),
                "CreateGeneralServiceOld",
            ),
            (
                interface_commands::CREATE_GENERAL_SERVICE,
                Some(NetworkInterface::create_general_service_handler),
                "CreateGeneralService",
            ),
        ]);
        Self {
            name: name.to_string(),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn create_general_service_old_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("NetworkInterface::CreateGeneralServiceOld called");
        push_interface_response(ctx, Arc::new(IGeneralService::new()));
    }

    fn create_general_service_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("NetworkInterface::CreateGeneralService called");
        push_interface_response(ctx, Arc::new(IGeneralService::new()));
    }
}

impl SessionRequestHandler for NetworkInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        &self.name
    }
}

impl ServiceFramework for NetworkInterface {
    fn get_service_name(&self) -> &str {
        &self.name
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "nifm:u", "nifm:a", "nifm:s" services.
///
/// Corresponds to `LoopProcess` in upstream `nifm.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let mut server_manager = ServerManager::new(system);

    server_manager.register_named_service(
        "nifm:a",
        Box::new(|| -> SessionRequestHandlerPtr { Arc::new(NetworkInterface::new("nifm:a")) }),
        16,
    );
    server_manager.register_named_service(
        "nifm:s",
        Box::new(|| -> SessionRequestHandlerPtr { Arc::new(NetworkInterface::new("nifm:s")) }),
        16,
    );
    server_manager.register_named_service(
        "nifm:u",
        Box::new(|| -> SessionRequestHandlerPtr { Arc::new(NetworkInterface::new("nifm:u")) }),
        16,
    );

    ServerManager::run_server(server_manager);
}
