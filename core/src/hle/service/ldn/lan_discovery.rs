// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ldn/lan_discovery.h
//! Port of zuyu/src/core/hle/service/ldn/lan_discovery.cpp
//!
//! LANDiscovery: manages LAN-based local communication discovery, network creation,
//! scanning, and station management.
//!
//! Note: This is a complex networking subsystem. The full implementation depends on
//! the internal network layer (RoomNetwork, RoomMember). Core data structures and
//! state management are ported here; network I/O will be wired when the network
//! layer is available.

use std::collections::HashMap;
use std::sync::Mutex;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::ldn_results::*;
use super::ldn_types::*;

/// Fake SSID used for LAN discovery (matches upstream `fake_ssid`).
pub const FAKE_SSID: &str = "YuzuFakeSsidForLdn";

/// LanStation represents a single station connected to the LAN network.
pub struct LanStation {
    pub node_id: i8,
    pub status: NodeStatus,
}

impl LanStation {
    pub fn new(node_id: i8) -> Self {
        Self {
            node_id,
            status: NodeStatus::Disconnected,
        }
    }

    pub fn reset(&mut self) {
        self.status = NodeStatus::Disconnected;
    }

    pub fn get_status(&self) -> NodeStatus {
        self.status
    }
}

/// LANDiscovery manages the state machine for local area network communication.
pub struct LANDiscovery {
    inited: bool,
    packet_mutex: Mutex<()>,
    stations: [LanStation; STATION_COUNT_MAX],
    node_changes: [NodeLatestUpdate; NODE_COUNT_MAX],
    node_last_states: [u8; NODE_COUNT_MAX],
    scan_results: HashMap<MacAddress, NetworkInfo>,
    node_info: NodeInfo,
    network_info: NetworkInfo,
    state: State,
    disconnect_reason: DisconnectReason,
    connected_clients: Vec<Ipv4Address>,
    host_ip: Option<Ipv4Address>,
}

impl LANDiscovery {
    pub fn new() -> Self {
        Self {
            inited: false,
            packet_mutex: Mutex::new(()),
            stations: core::array::from_fn(|i| LanStation::new(i as i8)),
            node_changes: [NodeLatestUpdate::default(); NODE_COUNT_MAX],
            node_last_states: [0u8; NODE_COUNT_MAX],
            scan_results: HashMap::new(),
            node_info: NodeInfo::default(),
            network_info: NetworkInfo::default(),
            state: State::None,
            disconnect_reason: DisconnectReason::None,
            connected_clients: Vec::new(),
            host_ip: None,
        }
    }

    pub fn get_state(&self) -> State {
        self.state
    }

    pub fn set_state(&mut self, new_state: State) {
        self.state = new_state;
    }

    pub fn get_disconnect_reason(&self) -> DisconnectReason {
        self.disconnect_reason
    }

    pub fn get_network_info(&self) -> Result<NetworkInfo, ResultCode> {
        Ok(self.network_info.clone())
    }

    pub fn open_access_point(&mut self) -> ResultCode {
        self.reset_stations();
        self.state = State::AccessPointOpened;
        RESULT_SUCCESS
    }

    pub fn close_access_point(&mut self) -> ResultCode {
        self.state = State::Initialized;
        RESULT_SUCCESS
    }

    pub fn open_station(&mut self) -> ResultCode {
        self.reset_stations();
        self.state = State::StationOpened;
        RESULT_SUCCESS
    }

    pub fn close_station(&mut self) -> ResultCode {
        self.disconnect_reason = DisconnectReason::None;
        self.state = State::Initialized;
        RESULT_SUCCESS
    }

    pub fn destroy_network(&mut self) -> ResultCode {
        // TODO: send disconnect packets
        self.state = State::AccessPointOpened;
        self.reset_stations();
        RESULT_SUCCESS
    }

    pub fn disconnect(&mut self) -> ResultCode {
        self.state = State::StationOpened;
        self.reset_stations();
        RESULT_SUCCESS
    }

    pub fn initialize(&mut self) -> ResultCode {
        if self.inited {
            return RESULT_SUCCESS;
        }
        self.state = State::Initialized;
        self.inited = true;
        RESULT_SUCCESS
    }

    pub fn finalize(&mut self) -> ResultCode {
        self.reset_stations();
        self.state = State::None;
        self.inited = false;
        RESULT_SUCCESS
    }

    fn reset_stations(&mut self) {
        for station in self.stations.iter_mut() {
            station.reset();
        }
        self.connected_clients.clear();
    }

    fn init_node_state_change(&mut self) {
        for change in self.node_changes.iter_mut() {
            *change = NodeLatestUpdate::default();
        }
        for state in self.node_last_states.iter_mut() {
            *state = 0;
        }
    }
}
