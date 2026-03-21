// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/ldn/user_local_communication_service.h
//! Port of zuyu/src/core/hle/service/ldn/user_local_communication_service.cpp
//!
//! IUserLocalCommunicationService: user-facing LDN communication service.
//! This is the most complex LDN service, handling network scanning, access point
//! management, station connections, and LAN discovery.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::lan_discovery::LANDiscovery;
use super::ldn_types::{State, DisconnectReason};

/// IPC command table for IUserLocalCommunicationService.
///
/// | Cmd | Handler                          | Name                              |
/// |-----|----------------------------------|-----------------------------------|
/// | 0   | GetState                         | GetState                          |
/// | 1   | GetNetworkInfo                   | GetNetworkInfo                    |
/// | 2   | GetIpv4Address                   | GetIpv4Address                    |
/// | 3   | GetDisconnectReason              | GetDisconnectReason               |
/// | 4   | GetSecurityParameter             | GetSecurityParameter              |
/// | 5   | GetNetworkConfig                 | GetNetworkConfig                  |
/// | 100 | AttachStateChangeEvent           | AttachStateChangeEvent            |
/// | 101 | GetNetworkInfoLatestUpdate       | GetNetworkInfoLatestUpdate        |
/// | 102 | Scan                             | Scan                              |
/// | 103 | ScanPrivate                      | ScanPrivate                       |
/// | 104 | SetWirelessControllerRestriction | SetWirelessControllerRestriction  |
/// | 200 | OpenAccessPoint                  | OpenAccessPoint                   |
/// | 201 | CloseAccessPoint                 | CloseAccessPoint                  |
/// | 202 | CreateNetwork                    | CreateNetwork                     |
/// | 203 | CreateNetworkPrivate             | CreateNetworkPrivate              |
/// | 204 | DestroyNetwork                   | DestroyNetwork                    |
/// | 205 | nullptr                          | Reject                            |
/// | 206 | SetAdvertiseData                 | SetAdvertiseData                  |
/// | 207 | SetStationAcceptPolicy           | SetStationAcceptPolicy            |
/// | 208 | AddAcceptFilterEntry             | AddAcceptFilterEntry              |
/// | 209 | nullptr                          | ClearAcceptFilter                 |
/// | 300 | OpenStation                      | OpenStation                       |
/// | 301 | CloseStation                     | CloseStation                      |
/// | 302 | Connect                          | Connect                           |
/// | 303 | nullptr                          | ConnectPrivate                    |
/// | 304 | Disconnect                       | Disconnect                        |
/// | 400 | Initialize                       | Initialize                        |
/// | 401 | Finalize                         | Finalize                          |
/// | 402 | Initialize2                      | Initialize2                       |
pub struct IUserLocalCommunicationService {
    is_initialized: bool,
    lan_discovery: LANDiscovery,
}

impl IUserLocalCommunicationService {
    pub fn new() -> Self {
        Self {
            is_initialized: false,
            lan_discovery: LANDiscovery::new(),
        }
    }

    /// Cmd 0: GetState
    pub fn get_state(&self) -> (ResultCode, State) {
        let state = if self.is_initialized {
            self.lan_discovery.get_state()
        } else {
            State::Error
        };
        log::info!("IUserLocalCommunicationService::get_state called, state={:?}", state);
        (RESULT_SUCCESS, state)
    }

    /// Cmd 3: GetDisconnectReason
    pub fn get_disconnect_reason(&self) -> (ResultCode, DisconnectReason) {
        log::info!("IUserLocalCommunicationService::get_disconnect_reason called");
        (RESULT_SUCCESS, self.lan_discovery.get_disconnect_reason())
    }

    /// Cmd 104: SetWirelessControllerRestriction
    pub fn set_wireless_controller_restriction(&self) -> ResultCode {
        log::warn!("(STUBBED) IUserLocalCommunicationService::set_wireless_controller_restriction called");
        RESULT_SUCCESS
    }

    /// Cmd 207: SetStationAcceptPolicy
    pub fn set_station_accept_policy(&self) -> ResultCode {
        log::warn!("(STUBBED) IUserLocalCommunicationService::set_station_accept_policy called");
        RESULT_SUCCESS
    }

    /// Cmd 208: AddAcceptFilterEntry
    pub fn add_accept_filter_entry(&self) -> ResultCode {
        log::warn!("(STUBBED) IUserLocalCommunicationService::add_accept_filter_entry called");
        RESULT_SUCCESS
    }

    /// Cmd 400: Initialize
    ///
    /// Upstream checks Network::GetSelectedNetworkInterface() and binds a room_member
    /// callback before calling lan_discovery.Initialize(). The network interface check
    /// and room_member binding are not yet wired (depends on internal_network and
    /// Network::RoomNetwork integration). For now we initialize the LAN discovery
    /// directly.
    pub fn initialize(&mut self, _aruid: u64) -> ResultCode {
        log::info!("IUserLocalCommunicationService::initialize called");
        // Upstream: checks Network::GetSelectedNetworkInterface(), returns
        // ResultAirplaneModeEnabled if unavailable, then binds room_member callback.
        // Those dependencies are not yet available; proceed with LAN discovery init.
        self.lan_discovery.initialize();
        self.is_initialized = true;
        RESULT_SUCCESS
    }

    /// Cmd 401: Finalize
    pub fn finalize(&mut self) -> ResultCode {
        log::info!("IUserLocalCommunicationService::finalize called");
        // Upstream: unbinds room_member callback here. Not yet wired.
        self.is_initialized = false;
        self.lan_discovery.finalize()
    }

    /// Cmd 402: Initialize2
    pub fn initialize2(&mut self, version: u32, process_id: u64) -> ResultCode {
        log::info!(
            "IUserLocalCommunicationService::initialize2 called, version={}, process_id={}",
            version,
            process_id
        );
        self.initialize(process_id)
    }
}
