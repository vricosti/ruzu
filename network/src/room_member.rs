// SPDX-FileCopyrightText: Copyright 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/network/room_member.h and room_member.cpp
//!
//! Implements the RoomMember (client) for network multiplayer games.

use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::Arc;

use parking_lot::Mutex;

use common::announce_multiplayer_room::{GameInfo, IPv4Address, RoomInformation};

use crate::room::{BanList, RoomMessageTypes, StatusMessageTypes, NO_PREFERRED_IP};

// ---------------------------------------------------------------------------
// Constants (from room_member.cpp)
// ---------------------------------------------------------------------------

/// Connection timeout in milliseconds.
pub const CONNECTION_TIMEOUT_MS: u32 = 5000;

// ---------------------------------------------------------------------------
// LDN packet types (from room_member.h)
// ---------------------------------------------------------------------------

/// LDN packet type.
/// Maps to C++ `Network::LDNPacketType`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum LdnPacketType {
    Scan = 0,
    ScanResp,
    Connect,
    SyncNetwork,
    Disconnect,
    DestroyNetwork,
}

/// An LDN packet.
/// Maps to C++ `Network::LDNPacket`.
#[derive(Clone, Debug)]
pub struct LdnPacket {
    pub packet_type: LdnPacketType,
    pub local_ip: IPv4Address,
    pub remote_ip: IPv4Address,
    pub broadcast: bool,
    pub data: Vec<u8>,
}

// ---------------------------------------------------------------------------
// SockAddrIn / Domain / Protocol  (from common/socket_types.h, used here)
// ---------------------------------------------------------------------------

/// Network address domain.
/// Maps to C++ `Network::Domain`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
#[repr(u8)]
pub enum Domain {
    #[default]
    Inet = 0,
    Inet6,
}

/// Network protocol.
/// Maps to C++ `Network::Protocol`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
#[repr(u8)]
pub enum Protocol {
    #[default]
    Tcp = 0,
    Udp,
}

/// Socket address.
/// Maps to C++ `Network::SockAddrIn`.
#[derive(Clone, Debug, Default)]
pub struct SockAddrIn {
    pub family: Domain,
    pub ip: IPv4Address,
    pub portno: u16,
}

// ---------------------------------------------------------------------------
// ProxyPacket
// ---------------------------------------------------------------------------

/// Information about received proxy packets.
/// Maps to C++ `Network::ProxyPacket`.
#[derive(Clone, Debug, Default)]
pub struct ProxyPacket {
    pub local_endpoint: SockAddrIn,
    pub remote_endpoint: SockAddrIn,
    pub protocol: Protocol,
    pub broadcast: bool,
    pub data: Vec<u8>,
}

// ---------------------------------------------------------------------------
// ChatEntry / StatusMessageEntry
// ---------------------------------------------------------------------------

/// Represents a chat message.
/// Maps to C++ `Network::ChatEntry`.
#[derive(Clone, Debug, Default)]
pub struct ChatEntry {
    /// Nickname of the client who sent this message.
    pub nickname: String,
    /// Web services username of the client who sent this message, can be empty.
    pub username: String,
    /// Body of the message.
    pub message: String,
}

/// Represents a system status message.
/// Maps to C++ `Network::StatusMessageEntry`.
#[derive(Clone, Debug)]
pub struct StatusMessageEntry {
    /// Type of the message.
    pub message_type: StatusMessageTypes,
    /// Subject of the message (the user who is joining/leaving/being banned, etc.).
    pub nickname: String,
    pub username: String,
}

// ---------------------------------------------------------------------------
// RoomMember::State and RoomMember::Error
// ---------------------------------------------------------------------------

/// Connection state of a RoomMember.
/// Maps to C++ `Network::RoomMember::State`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum RoomMemberState {
    /// Not initialized.
    Uninitialized = 0,
    /// Default state (not connected).
    Idle,
    /// The client is attempting to join a room.
    Joining,
    /// The client is connected to the room and ready to send/receive packets.
    Joined,
    /// The client is connected to the room and is granted mod permissions.
    Moderator,
}

/// Errors that can occur for a RoomMember.
/// Maps to C++ `Network::RoomMember::Error`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum RoomMemberError {
    /// Connection closed.
    LostConnection = 0,
    /// Kicked by the host.
    HostKicked,
    /// Some error (permissions to network device missing or something).
    UnknownError,
    /// Somebody is already using this name.
    NameCollision,
    /// Somebody is already using that fake IP address.
    IpCollision,
    /// The room version is not the same as for this RoomMember.
    WrongVersion,
    /// The password doesn't match.
    WrongPassword,
    /// The room is not responding to a connection attempt.
    CouldNotConnect,
    /// Room is already at the maximum number of players.
    RoomIsFull,
    /// The user is banned by the host.
    HostBanned,
    /// The user does not have mod permissions.
    PermissionDenied,
    /// The nickname the user attempts to kick/ban does not exist.
    NoSuchUser,
}

/// Returns a string representation of the state.
/// Maps to C++ `Network::GetStateStr`.
pub fn get_state_str(state: RoomMemberState) -> &'static str {
    match state {
        RoomMemberState::Uninitialized => "Uninitialized",
        RoomMemberState::Idle => "Idle",
        RoomMemberState::Joining => "Joining",
        RoomMemberState::Joined => "Joined",
        RoomMemberState::Moderator => "Moderator",
    }
}

/// Returns a string representation of the error.
/// Maps to C++ `Network::GetErrorStr`.
pub fn get_error_str(error: RoomMemberError) -> &'static str {
    match error {
        RoomMemberError::LostConnection => "LostConnection",
        RoomMemberError::HostKicked => "HostKicked",
        RoomMemberError::UnknownError => "UnknownError",
        RoomMemberError::NameCollision => "NameCollision",
        RoomMemberError::IpCollision => "IpCollision",
        RoomMemberError::WrongVersion => "WrongVersion",
        RoomMemberError::WrongPassword => "WrongPassword",
        RoomMemberError::CouldNotConnect => "CouldNotConnect",
        RoomMemberError::RoomIsFull => "RoomIsFull",
        RoomMemberError::HostBanned => "HostBanned",
        RoomMemberError::PermissionDenied => "PermissionDenied",
        RoomMemberError::NoSuchUser => "NoSuchUser",
    }
}

// ---------------------------------------------------------------------------
// MemberInformation
// ---------------------------------------------------------------------------

/// Information about a member as seen by the client.
/// Maps to C++ `Network::RoomMember::MemberInformation`.
#[derive(Clone, Debug, Default)]
pub struct MemberInformation {
    pub nickname: String,
    pub username: String,
    pub display_name: String,
    pub avatar_url: String,
    pub game_info: GameInfo,
    pub fake_ip: IPv4Address,
}

pub type MemberList = Vec<MemberInformation>;

// ---------------------------------------------------------------------------
// Callback types
// ---------------------------------------------------------------------------

/// A handle for a registered callback.
pub type CallbackHandle<T> = Arc<Box<dyn Fn(&T) + Send + Sync>>;

// ---------------------------------------------------------------------------
// RoomMemberImpl (internal state)
// ---------------------------------------------------------------------------

struct RoomMemberImpl {
    state: AtomicU8,
    member_information: Mutex<MemberList>,
    room_information: Mutex<RoomInformation>,
    current_game_info: Mutex<GameInfo>,
    nickname: Mutex<String>,
    username: Mutex<String>,
    fake_ip: Mutex<IPv4Address>,

    // Callbacks
    callbacks_state: Mutex<Vec<CallbackHandle<RoomMemberState>>>,
    callbacks_error: Mutex<Vec<CallbackHandle<RoomMemberError>>>,
    callbacks_proxy_packet: Mutex<Vec<CallbackHandle<ProxyPacket>>>,
    callbacks_ldn_packet: Mutex<Vec<CallbackHandle<LdnPacket>>>,
    callbacks_room_information: Mutex<Vec<CallbackHandle<RoomInformation>>>,
    callbacks_chat_message: Mutex<Vec<CallbackHandle<ChatEntry>>>,
    callbacks_status_message: Mutex<Vec<CallbackHandle<StatusMessageEntry>>>,
    callbacks_ban_list: Mutex<Vec<CallbackHandle<BanList>>>,
}

impl RoomMemberImpl {
    fn new() -> Self {
        Self {
            state: AtomicU8::new(RoomMemberState::Idle as u8),
            member_information: Mutex::new(Vec::new()),
            room_information: Mutex::new(RoomInformation::default()),
            current_game_info: Mutex::new(GameInfo::default()),
            nickname: Mutex::new(String::new()),
            username: Mutex::new(String::new()),
            fake_ip: Mutex::new([0; 4]),
            callbacks_state: Mutex::new(Vec::new()),
            callbacks_error: Mutex::new(Vec::new()),
            callbacks_proxy_packet: Mutex::new(Vec::new()),
            callbacks_ldn_packet: Mutex::new(Vec::new()),
            callbacks_room_information: Mutex::new(Vec::new()),
            callbacks_chat_message: Mutex::new(Vec::new()),
            callbacks_status_message: Mutex::new(Vec::new()),
            callbacks_ban_list: Mutex::new(Vec::new()),
        }
    }

    fn get_state(&self) -> RoomMemberState {
        match self.state.load(Ordering::SeqCst) {
            0 => RoomMemberState::Uninitialized,
            1 => RoomMemberState::Idle,
            2 => RoomMemberState::Joining,
            3 => RoomMemberState::Joined,
            4 => RoomMemberState::Moderator,
            _ => RoomMemberState::Idle,
        }
    }

    fn set_state(&self, state: RoomMemberState) {
        let old = self.get_state();
        if old != state {
            self.state.store(state as u8, Ordering::SeqCst);
            let callbacks = self.callbacks_state.lock();
            for cb in callbacks.iter() {
                cb(&state);
            }
        }
    }

    fn set_error(&self, error: RoomMemberError) {
        let callbacks = self.callbacks_error.lock();
        for cb in callbacks.iter() {
            cb(&error);
        }
    }

    fn is_connected(&self) -> bool {
        let state = self.get_state();
        state == RoomMemberState::Joining
            || state == RoomMemberState::Joined
            || state == RoomMemberState::Moderator
    }
}

// ---------------------------------------------------------------------------
// RoomMember (public API)
// ---------------------------------------------------------------------------

/// This is what a client (person joining a server) would use.
/// Maps to C++ `Network::RoomMember`.
pub struct RoomMember {
    room_member_impl: RoomMemberImpl,
}

impl RoomMember {
    pub fn new() -> Self {
        Self {
            room_member_impl: RoomMemberImpl::new(),
        }
    }

    /// Returns the status of our connection to the room.
    pub fn get_state(&self) -> RoomMemberState {
        self.room_member_impl.get_state()
    }

    /// Returns information about the members in the room we're connected to.
    pub fn get_member_information(&self) -> MemberList {
        self.room_member_impl.member_information.lock().clone()
    }

    /// Returns the nickname of the RoomMember.
    pub fn get_nickname(&self) -> String {
        self.room_member_impl.nickname.lock().clone()
    }

    /// Returns the username of the RoomMember.
    pub fn get_username(&self) -> String {
        self.room_member_impl.username.lock().clone()
    }

    /// Returns the fake IP address of the RoomMember.
    pub fn get_fake_ip_address(&self) -> IPv4Address {
        assert!(
            self.is_connected(),
            "Tried to get fake ip address while not connected"
        );
        *self.room_member_impl.fake_ip.lock()
    }

    /// Returns information about the room we're connected to.
    pub fn get_room_information(&self) -> RoomInformation {
        self.room_member_impl.room_information.lock().clone()
    }

    /// Returns whether we're connected to a server or not.
    pub fn is_connected(&self) -> bool {
        self.room_member_impl.is_connected()
    }

    /// Attempts to join a room at the specified address and port.
    ///
    /// NOTE: ENet networking is not ported. This method is stubbed.
    pub fn join(
        &self,
        _nickname: &str,
        _server_addr: &str,
        _server_port: u16,
        _client_port: u16,
        _preferred_fake_ip: &IPv4Address,
        _password: &str,
        _token: &str,
    ) {
        log::warn!("RoomMember::join: ENet networking layer not ported; setting state to Error");
        self.room_member_impl.set_error(RoomMemberError::CouldNotConnect);
    }

    /// Sends a proxy packet to the room.
    pub fn send_proxy_packet(&self, _packet: &ProxyPacket) {
        log::warn!("RoomMember::send_proxy_packet: ENet networking layer not ported; packet dropped");
    }

    /// Sends an LDN packet to the room.
    pub fn send_ldn_packet(&self, _packet: &LdnPacket) {
        log::warn!("RoomMember::send_ldn_packet: ENet networking layer not ported; packet dropped");
    }

    /// Sends a chat message to the room.
    pub fn send_chat_message(&self, _message: &str) {
        log::warn!("RoomMember::send_chat_message: ENet networking layer not ported; message dropped");
    }

    /// Sends the current game info to the room.
    pub fn send_game_info(&self, game_info: &GameInfo) {
        *self.room_member_impl.current_game_info.lock() = game_info.clone();
        if !self.is_connected() {
            return;
        }
        log::warn!("RoomMember::send_game_info: ENet networking layer not ported; game info not sent");
    }

    /// Sends a moderation request to the room.
    pub fn send_moderation_request(&self, msg_type: RoomMessageTypes, _nickname: &str) {
        assert!(
            msg_type == RoomMessageTypes::IdModKick
                || msg_type == RoomMessageTypes::IdModBan
                || msg_type == RoomMessageTypes::IdModUnban,
            "type is not a moderation request"
        );
        if !self.is_connected() {
            return;
        }
        log::warn!("RoomMember::send_moderation_request: ENet networking layer not ported; request dropped");
    }

    /// Attempts to retrieve ban list from the room.
    pub fn request_ban_list(&self) {
        if !self.is_connected() {
            return;
        }
        log::warn!("RoomMember::request_ban_list: ENet networking layer not ported; request dropped");
    }

    // -----------------------------------------------------------------------
    // Callback binding
    // -----------------------------------------------------------------------

    pub fn bind_on_state_changed(
        &self,
        callback: impl Fn(&RoomMemberState) + Send + Sync + 'static,
    ) -> CallbackHandle<RoomMemberState> {
        let handle: CallbackHandle<RoomMemberState> = Arc::new(Box::new(callback));
        self.room_member_impl
            .callbacks_state
            .lock()
            .push(handle.clone());
        handle
    }

    pub fn bind_on_error(
        &self,
        callback: impl Fn(&RoomMemberError) + Send + Sync + 'static,
    ) -> CallbackHandle<RoomMemberError> {
        let handle: CallbackHandle<RoomMemberError> = Arc::new(Box::new(callback));
        self.room_member_impl
            .callbacks_error
            .lock()
            .push(handle.clone());
        handle
    }

    pub fn bind_on_proxy_packet_received(
        &self,
        callback: impl Fn(&ProxyPacket) + Send + Sync + 'static,
    ) -> CallbackHandle<ProxyPacket> {
        let handle: CallbackHandle<ProxyPacket> = Arc::new(Box::new(callback));
        self.room_member_impl
            .callbacks_proxy_packet
            .lock()
            .push(handle.clone());
        handle
    }

    pub fn bind_on_ldn_packet_received(
        &self,
        callback: impl Fn(&LdnPacket) + Send + Sync + 'static,
    ) -> CallbackHandle<LdnPacket> {
        let handle: CallbackHandle<LdnPacket> = Arc::new(Box::new(callback));
        self.room_member_impl
            .callbacks_ldn_packet
            .lock()
            .push(handle.clone());
        handle
    }

    pub fn bind_on_room_information_changed(
        &self,
        callback: impl Fn(&RoomInformation) + Send + Sync + 'static,
    ) -> CallbackHandle<RoomInformation> {
        let handle: CallbackHandle<RoomInformation> = Arc::new(Box::new(callback));
        self.room_member_impl
            .callbacks_room_information
            .lock()
            .push(handle.clone());
        handle
    }

    pub fn bind_on_chat_message_received(
        &self,
        callback: impl Fn(&ChatEntry) + Send + Sync + 'static,
    ) -> CallbackHandle<ChatEntry> {
        let handle: CallbackHandle<ChatEntry> = Arc::new(Box::new(callback));
        self.room_member_impl
            .callbacks_chat_message
            .lock()
            .push(handle.clone());
        handle
    }

    pub fn bind_on_status_message_received(
        &self,
        callback: impl Fn(&StatusMessageEntry) + Send + Sync + 'static,
    ) -> CallbackHandle<StatusMessageEntry> {
        let handle: CallbackHandle<StatusMessageEntry> = Arc::new(Box::new(callback));
        self.room_member_impl
            .callbacks_status_message
            .lock()
            .push(handle.clone());
        handle
    }

    pub fn bind_on_ban_list_received(
        &self,
        callback: impl Fn(&BanList) + Send + Sync + 'static,
    ) -> CallbackHandle<BanList> {
        let handle: CallbackHandle<BanList> = Arc::new(Box::new(callback));
        self.room_member_impl
            .callbacks_ban_list
            .lock()
            .push(handle.clone());
        handle
    }

    /// Leaves the current room.
    pub fn leave(&self) {
        self.room_member_impl
            .set_state(RoomMemberState::Idle);
        // NOTE: loop_thread join and enet_host_destroy not ported.
    }
}

impl Default for RoomMember {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_room_member_default_state() {
        let member = RoomMember::new();
        assert_eq!(member.get_state(), RoomMemberState::Idle);
        assert!(!member.is_connected());
    }

    #[test]
    fn test_get_state_str() {
        assert_eq!(get_state_str(RoomMemberState::Joined), "Joined");
        assert_eq!(get_state_str(RoomMemberState::Moderator), "Moderator");
    }

    #[test]
    fn test_get_error_str() {
        assert_eq!(get_error_str(RoomMemberError::RoomIsFull), "RoomIsFull");
        assert_eq!(
            get_error_str(RoomMemberError::PermissionDenied),
            "PermissionDenied"
        );
    }
}
