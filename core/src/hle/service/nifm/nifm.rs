// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nifm/nifm.cpp
//!
//! Network Interface Manager services.

use crate::hle::result::{ErrorModule, ResultCode};

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

/// IGeneralService.
pub struct IGeneralService;

impl IGeneralService {
    pub fn new() -> Self {
        Self
    }

    pub fn get_client_id(&self) -> u64 {
        log::warn!("(STUBBED) IGeneralService::get_client_id called");
        1 // Non-zero client ID
    }

    pub fn create_scan_request(&self) -> IScanRequest {
        log::debug!("IGeneralService::create_scan_request called");
        IScanRequest::new()
    }

    pub fn create_request(&self) -> IRequest {
        log::debug!("IGeneralService::create_request called");
        IRequest::new()
    }

    pub fn remove_network_profile(&self) {
        log::warn!("(STUBBED) IGeneralService::remove_network_profile called");
    }

    pub fn get_current_ip_address(&self) -> [u8; 4] {
        log::warn!("(STUBBED) IGeneralService::get_current_ip_address called");
        [127, 0, 0, 1]
    }

    pub fn is_wireless_communication_enabled(&self) -> bool {
        log::warn!("(STUBBED) IGeneralService::is_wireless_communication_enabled called");
        true
    }

    pub fn is_ethernet_communication_enabled(&self) -> bool {
        log::warn!("(STUBBED) IGeneralService::is_ethernet_communication_enabled called");
        true
    }

    pub fn is_any_internet_request_accepted(&self) -> bool {
        log::error!("(STUBBED) IGeneralService::is_any_internet_request_accepted called");
        true
    }

    pub fn is_any_foreground_request_accepted(&self) -> bool {
        log::warn!("(STUBBED) IGeneralService::is_any_foreground_request_accepted called");
        false
    }
}

/// IScanRequest. All stubs.
pub struct IScanRequest;
impl IScanRequest {
    pub fn new() -> Self { Self }
}

/// IRequest.
pub struct IRequest {
    state: RequestState,
    // TODO: service_context, event1, event2
}

impl IRequest {
    pub fn new() -> Self {
        Self {
            state: RequestState::NotSubmitted,
        }
    }

    pub fn submit(&mut self) {
        log::debug!("(STUBBED) IRequest::submit called");
        if self.state == RequestState::NotSubmitted {
            self.state = RequestState::OnHold;
        }
    }

    pub fn get_request_state(&self) -> RequestState {
        self.state
    }

    pub fn get_result(&mut self) -> ResultCode {
        log::debug!("(STUBBED) IRequest::get_result called");
        // Simplified: assume network available
        match self.state {
            RequestState::OnHold => {
                self.state = RequestState::Accepted;
                RESULT_PENDING_CONNECTION
            }
            _ => crate::hle::result::RESULT_SUCCESS,
        }
    }

    pub fn cancel(&self) {
        log::warn!("(STUBBED) IRequest::cancel called");
    }

    pub fn set_requirement_preset(&self, _param: u32) {
        log::warn!("(STUBBED) IRequest::set_requirement_preset called");
    }

    pub fn set_connection_confirmation_option(&self) {
        log::warn!("(STUBBED) IRequest::set_connection_confirmation_option called");
    }
}

/// INetworkProfile. All stubs.
pub struct INetworkProfile;
impl INetworkProfile {
    pub fn new() -> Self { Self }
}

/// NetworkInterface ("nifm:a", "nifm:s", "nifm:u").
pub struct NetworkInterface {
    name: String,
}

impl NetworkInterface {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }

    pub fn create_general_service_old(&self) -> IGeneralService {
        log::debug!("NetworkInterface({})::create_general_service_old called", self.name);
        IGeneralService::new()
    }

    pub fn create_general_service(&self) -> IGeneralService {
        log::debug!("NetworkInterface({})::create_general_service called", self.name);
        IGeneralService::new()
    }
}

/// Registers "nifm:a", "nifm:s", "nifm:u" services.
///
/// Corresponds to `LoopProcess` in upstream `nifm.cpp`.
pub fn loop_process() {
    // TODO: register services with ServerManager
}
