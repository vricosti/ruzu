// SPDX-FileCopyrightText: Copyright 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/network/room_member.h and room_member.cpp
//!
//! Implements the RoomMember (client) for network multiplayer games.

use std::net::{Ipv4Addr, SocketAddr, ToSocketAddrs, UdpSocket};
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::Arc;
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

use parking_lot::Mutex;
use rusty_enet as enet;

use common::announce_multiplayer_room::{GameInfo, IPv4Address, RoomInformation};

use crate::packet::Packet;
use crate::room::{BanList, RoomMessageTypes, StatusMessageTypes, NETWORK_VERSION, NUM_CHANNELS};

// ---------------------------------------------------------------------------
// Constants (from room_member.cpp)
// ---------------------------------------------------------------------------

/// Connection timeout in milliseconds.
pub const CONNECTION_TIMEOUT_MS: u32 = 5000;

/// How long the receive loop sleeps between two `service` calls.
///
/// Upstream passes a timeout straight to `enet_host_service`, which blocks
/// until an event arrives or the timeout expires. `rusty_enet::Host::service`
/// never blocks, so the wait is expressed here instead. 5 ms matches the
/// timeout upstream uses inside its loop.
const SERVICE_POLL_INTERVAL: Duration = Duration::from_millis(5);

/// Resolves the room address the way `enet_address_set_host` does: a literal
/// address if possible, otherwise a DNS lookup.
fn resolve_server_address(server_addr: &str, server_port: u16) -> Option<SocketAddr> {
    match (server_addr, server_port).to_socket_addrs() {
        Ok(mut addresses) => addresses.next(),
        Err(error) => {
            log::error!("RoomMember: could not resolve {server_addr}:{server_port}: {error}");
            None
        }
    }
}

/// Body of `RoomMember::SendProxyPacket`, after the message id.
fn write_proxy_packet(packet: &mut Packet, proxy_packet: &ProxyPacket) {
    packet.write_u8(proxy_packet.local_endpoint.family as u8);
    packet.write_array(&proxy_packet.local_endpoint.ip);
    packet.write_u16(proxy_packet.local_endpoint.portno);

    packet.write_u8(proxy_packet.remote_endpoint.family as u8);
    packet.write_array(&proxy_packet.remote_endpoint.ip);
    packet.write_u16(proxy_packet.remote_endpoint.portno);

    packet.write_u8(proxy_packet.protocol as u8);
    packet.write_bool(proxy_packet.broadcast);
    packet.write_vec_u8(&proxy_packet.data);
}

/// Body of `RoomMember::SendLdnPacket`, after the message id.
fn write_ldn_packet(packet: &mut Packet, ldn_packet: &LdnPacket) {
    packet.write_u8(ldn_packet.packet_type as u8);
    packet.write_array(&ldn_packet.local_ip);
    packet.write_array(&ldn_packet.remote_ip);
    packet.write_bool(ldn_packet.broadcast);
    packet.write_vec_u8(&ldn_packet.data);
}

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

impl LdnPacketType {
    /// Maps to the `static_cast<LDNPacketType>` upstream performs on the wire byte.
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(Self::Scan),
            1 => Some(Self::ScanResp),
            2 => Some(Self::Connect),
            3 => Some(Self::SyncNetwork),
            4 => Some(Self::Disconnect),
            5 => Some(Self::DestroyNetwork),
            _ => None,
        }
    }
}

impl Domain {
    /// Maps to the `static_cast<Domain>` upstream performs on the wire byte.
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(Self::Inet),
            1 => Some(Self::Inet6),
            _ => None,
        }
    }
}

impl Protocol {
    /// Maps to the `static_cast<Protocol>` upstream performs on the wire byte.
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(Self::Tcp),
            1 => Some(Self::Udp),
            _ => None,
        }
    }
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

    /// Packets queued by the caller thread and flushed by the receive loop.
    /// Maps to C++ `send_list` guarded by `send_list_mutex`.
    send_list: Mutex<Vec<Packet>>,
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
            send_list: Mutex::new(Vec::new()),
        }
    }

    /// Maps to C++ `RoomMemberImpl::Send`.
    fn send(&self, packet: Packet) {
        self.send_list.lock().push(packet);
    }

    /// Maps to C++ `RoomMemberImpl::SendJoinRequest`.
    fn send_join_request(
        &self,
        nickname: &str,
        preferred_fake_ip: &IPv4Address,
        password: &str,
        token: &str,
    ) {
        let mut packet = Packet::new();
        packet.write_u8(RoomMessageTypes::IdJoinRequest as u8);
        packet.write_string(nickname);
        packet.write_array(preferred_fake_ip);
        packet.write_u32(NETWORK_VERSION);
        packet.write_string(password);
        packet.write_string(token);
        self.send(packet);
    }

    /// Dispatches one received packet. Maps to the `ENET_EVENT_TYPE_RECEIVE`
    /// arm of C++ `RoomMemberImpl::StartLoop`.
    fn handle_packet(&self, data: &[u8]) {
        let Some(&id) = data.first() else {
            return;
        };
        let Some(message) = RoomMessageTypes::from_u8(id) else {
            return;
        };

        match message {
            RoomMessageTypes::IdProxyPacket => self.handle_proxy_packet(data),
            RoomMessageTypes::IdLdnPacket => self.handle_ldn_packet(data),
            RoomMessageTypes::IdChatMessage => self.handle_chat_packet(data),
            RoomMessageTypes::IdStatusMessage => self.handle_status_message_packet(data),
            RoomMessageTypes::IdRoomInformation => self.handle_room_information_packet(data),
            RoomMessageTypes::IdJoinSuccess | RoomMessageTypes::IdJoinSuccessAsMod => {
                // If we joined successfully there must be at least one member
                // in the room: us.
                debug_assert!(
                    !self.member_information.lock().is_empty(),
                    "we have not yet received member information"
                );
                self.handle_join_packet(data);
                if message == RoomMessageTypes::IdJoinSuccessAsMod {
                    self.set_state(RoomMemberState::Moderator);
                } else {
                    self.set_state(RoomMemberState::Joined);
                }
            }
            RoomMessageTypes::IdModBanListResponse => self.handle_mod_ban_list_response(data),
            RoomMessageTypes::IdRoomIsFull => self.fail(RoomMemberError::RoomIsFull),
            RoomMessageTypes::IdNameCollision => self.fail(RoomMemberError::NameCollision),
            RoomMessageTypes::IdIpCollision => self.fail(RoomMemberError::IpCollision),
            RoomMessageTypes::IdVersionMismatch => self.fail(RoomMemberError::WrongVersion),
            RoomMessageTypes::IdWrongPassword => self.fail(RoomMemberError::WrongPassword),
            RoomMessageTypes::IdCloseRoom => self.fail(RoomMemberError::LostConnection),
            RoomMessageTypes::IdHostKicked => self.fail(RoomMemberError::HostKicked),
            RoomMessageTypes::IdHostBanned => self.fail(RoomMemberError::HostBanned),
            RoomMessageTypes::IdModPermissionDenied => {
                self.set_error(RoomMemberError::PermissionDenied)
            }
            RoomMessageTypes::IdModNoSuchUser => self.set_error(RoomMemberError::NoSuchUser),
            _ => {}
        }
    }

    /// The `SetState(Idle); SetError(e);` pair upstream repeats for every
    /// terminal room message.
    fn fail(&self, error: RoomMemberError) {
        self.set_state(RoomMemberState::Idle);
        self.set_error(error);
    }

    /// Maps to C++ `RoomMemberImpl::HandleRoomInformationPacket`.
    fn handle_room_information_packet(&self, data: &[u8]) {
        let mut packet = Packet::new();
        packet.append(data);
        packet.ignore_bytes(1);

        let mut info = RoomInformation::default();
        info.name = packet.read_string().unwrap_or_default();
        info.description = packet.read_string().unwrap_or_default();
        info.member_slots = packet.read_u32().unwrap_or_default();
        info.port = packet.read_u16().unwrap_or_default();
        info.preferred_game.name = packet.read_string().unwrap_or_default();
        info.host_username = packet.read_string().unwrap_or_default();
        *self.room_information.lock() = info.clone();

        let num_members = packet.read_u32().unwrap_or_default();
        let mut members = Vec::with_capacity(num_members as usize);
        let nickname = self.nickname.lock().clone();
        for _ in 0..num_members {
            let member = MemberInformation {
                nickname: packet.read_string().unwrap_or_default(),
                fake_ip: packet.read_array::<4>().unwrap_or_default(),
                game_info: GameInfo {
                    name: packet.read_string().unwrap_or_default(),
                    id: packet.read_u64().unwrap_or_default(),
                    version: packet.read_string().unwrap_or_default(),
                },
                username: packet.read_string().unwrap_or_default(),
                display_name: packet.read_string().unwrap_or_default(),
                avatar_url: packet.read_string().unwrap_or_default(),
            };
            if member.nickname == nickname {
                *self.username.lock() = member.username.clone();
            }
            members.push(member);
        }
        *self.member_information.lock() = members;

        let callbacks = self.callbacks_room_information.lock();
        for callback in callbacks.iter() {
            callback(&info);
        }
    }

    /// Maps to C++ `RoomMemberImpl::HandleJoinPacket`.
    fn handle_join_packet(&self, data: &[u8]) {
        let mut packet = Packet::new();
        packet.append(data);
        packet.ignore_bytes(1);
        if let Some(fake_ip) = packet.read_array::<4>() {
            *self.fake_ip.lock() = fake_ip;
        }
    }

    /// Maps to C++ `RoomMemberImpl::HandleChatPacket`.
    fn handle_chat_packet(&self, data: &[u8]) {
        let mut packet = Packet::new();
        packet.append(data);
        packet.ignore_bytes(1);

        let entry = ChatEntry {
            nickname: packet.read_string().unwrap_or_default(),
            username: packet.read_string().unwrap_or_default(),
            message: packet.read_string().unwrap_or_default(),
        };
        let callbacks = self.callbacks_chat_message.lock();
        for callback in callbacks.iter() {
            callback(&entry);
        }
    }

    /// Maps to C++ `RoomMemberImpl::HandleStatusMessagePacket`.
    fn handle_status_message_packet(&self, data: &[u8]) {
        let mut packet = Packet::new();
        packet.append(data);
        packet.ignore_bytes(1);

        let raw_type = packet.read_u8().unwrap_or_default();
        let entry = StatusMessageEntry {
            message_type: StatusMessageTypes::from_u8(raw_type)
                .unwrap_or(StatusMessageTypes::IdMemberJoin),
            nickname: packet.read_string().unwrap_or_default(),
            username: packet.read_string().unwrap_or_default(),
        };
        let callbacks = self.callbacks_status_message.lock();
        for callback in callbacks.iter() {
            callback(&entry);
        }
    }

    /// Maps to C++ `RoomMemberImpl::HandleModBanListResponsePacket`.
    fn handle_mod_ban_list_response(&self, data: &[u8]) {
        let mut packet = Packet::new();
        packet.append(data);
        packet.ignore_bytes(1);

        let ban_list: BanList = (
            packet.read_vec_string().unwrap_or_default(),
            packet.read_vec_string().unwrap_or_default(),
        );
        let callbacks = self.callbacks_ban_list.lock();
        for callback in callbacks.iter() {
            callback(&ban_list);
        }
    }

    /// Maps to C++ `RoomMemberImpl::HandleProxyPackets`.
    fn handle_proxy_packet(&self, data: &[u8]) {
        let mut packet = Packet::new();
        packet.append(data);
        packet.ignore_bytes(1);

        let proxy_packet = ProxyPacket {
            local_endpoint: SockAddrIn {
                family: Domain::from_u8(packet.read_u8().unwrap_or_default())
                    .unwrap_or(Domain::Inet),
                ip: packet.read_array::<4>().unwrap_or_default(),
                portno: packet.read_u16().unwrap_or_default(),
            },
            remote_endpoint: SockAddrIn {
                family: Domain::from_u8(packet.read_u8().unwrap_or_default())
                    .unwrap_or(Domain::Inet),
                ip: packet.read_array::<4>().unwrap_or_default(),
                portno: packet.read_u16().unwrap_or_default(),
            },
            protocol: Protocol::from_u8(packet.read_u8().unwrap_or_default())
                .unwrap_or(Protocol::Udp),
            broadcast: packet.read_bool().unwrap_or_default(),
            data: packet.read_vec_u8().unwrap_or_default(),
        };
        let callbacks = self.callbacks_proxy_packet.lock();
        for callback in callbacks.iter() {
            callback(&proxy_packet);
        }
    }

    /// Maps to C++ `RoomMemberImpl::HandleLdnPackets`.
    fn handle_ldn_packet(&self, data: &[u8]) {
        let mut packet = Packet::new();
        packet.append(data);
        packet.ignore_bytes(1);

        let ldn_packet = LdnPacket {
            packet_type: LdnPacketType::from_u8(packet.read_u8().unwrap_or_default())
                .unwrap_or(LdnPacketType::Scan),
            local_ip: packet.read_array::<4>().unwrap_or_default(),
            remote_ip: packet.read_array::<4>().unwrap_or_default(),
            broadcast: packet.read_bool().unwrap_or_default(),
            data: packet.read_vec_u8().unwrap_or_default(),
        };
        let callbacks = self.callbacks_ldn_packet.lock();
        for callback in callbacks.iter() {
            callback(&ldn_packet);
        }
    }

    /// Maps to C++ `RoomMemberImpl::Disconnect`.
    fn disconnect(&self, host: &mut enet::Host<UdpSocket>, peer_id: enet::PeerID) {
        self.member_information.lock().clear();
        {
            let mut info = self.room_information.lock();
            info.member_slots = 0;
            info.name.clear();
        }

        host.peer_mut(peer_id).disconnect(0);

        let deadline = Instant::now() + Duration::from_millis(CONNECTION_TIMEOUT_MS as u64);
        while Instant::now() < deadline {
            match host.service() {
                // Ignore all incoming data.
                Ok(Some(enet::Event::Receive { .. })) | Ok(Some(enet::Event::Connect { .. })) => {}
                Ok(Some(enet::Event::Disconnect { .. })) => return,
                Ok(None) => {}
                Err(_) => break,
            }
            std::thread::sleep(SERVICE_POLL_INTERVAL);
        }

        // Did not disconnect gracefully: force it.
        host.peer_mut(peer_id).reset();
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
    /// Shared with the receive loop, which is why this is an `Arc` where
    /// upstream can use a `unique_ptr`: the C++ loop captures `this` and is
    /// joined in the destructor before the impl dies.
    room_member_impl: Arc<RoomMemberImpl>,
    /// Maps to C++ `loop_thread`.
    loop_thread: Mutex<Option<JoinHandle<()>>>,
}

impl RoomMember {
    pub fn new() -> Self {
        Self {
            room_member_impl: Arc::new(RoomMemberImpl::new()),
            loop_thread: Mutex::new(None),
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
    /// Maps to C++ `RoomMember::Join`.
    pub fn join(
        &self,
        nickname: &str,
        server_addr: &str,
        server_port: u16,
        client_port: u16,
        preferred_fake_ip: &IPv4Address,
        password: &str,
        token: &str,
    ) {
        // If the member is connected, kill the connection first.
        if self.loop_thread.lock().is_some() {
            self.leave();
        }

        self.room_member_impl.set_state(RoomMemberState::Joining);

        let Some(address) = resolve_server_address(server_addr, server_port) else {
            self.room_member_impl.set_state(RoomMemberState::Idle);
            self.room_member_impl
                .set_error(RoomMemberError::CouldNotConnect);
            return;
        };

        // Upstream lets ENet own the socket through `enet_host_create(nullptr,
        // ...)`. rusty_enet takes an already-bound socket instead, so the
        // client port that upstream passes to `Join` is bound here. Port 0
        // keeps upstream's "any port" behaviour.
        let bind_addr = match address {
            SocketAddr::V4(_) => SocketAddr::from((Ipv4Addr::UNSPECIFIED, client_port)),
            SocketAddr::V6(_) => SocketAddr::from((std::net::Ipv6Addr::UNSPECIFIED, client_port)),
        };
        let socket = match UdpSocket::bind(bind_addr) {
            Ok(socket) => socket,
            Err(error) => {
                log::error!("RoomMember::join: could not bind client socket: {error}");
                self.room_member_impl.set_state(RoomMemberState::Idle);
                self.room_member_impl
                    .set_error(RoomMemberError::UnknownError);
                return;
            }
        };

        let mut host = match enet::Host::new(
            socket,
            enet::HostSettings {
                peer_limit: 1,
                channel_limit: NUM_CHANNELS,
                ..Default::default()
            },
        ) {
            Ok(host) => host,
            Err(error) => {
                log::error!("RoomMember::join: could not create client: {error}");
                self.room_member_impl.set_state(RoomMemberState::Idle);
                self.room_member_impl
                    .set_error(RoomMemberError::UnknownError);
                return;
            }
        };

        let peer_id = match host.connect(address, NUM_CHANNELS, 0) {
            Ok(peer) => peer.id(),
            Err(_) => {
                self.room_member_impl.set_state(RoomMemberState::Idle);
                self.room_member_impl
                    .set_error(RoomMemberError::UnknownError);
                return;
            }
        };

        // Upstream blocks inside `enet_host_service(client, &event,
        // ConnectionTimeoutMs)`. rusty_enet's `service` never blocks, so the
        // same bounded wait is expressed as a poll until the deadline.
        let deadline = Instant::now() + Duration::from_millis(CONNECTION_TIMEOUT_MS as u64);
        let mut connected = false;
        while Instant::now() < deadline {
            match host.service() {
                Ok(Some(enet::Event::Connect { .. })) => {
                    connected = true;
                    break;
                }
                Ok(_) => {}
                Err(error) => {
                    log::error!("RoomMember::join: socket error while connecting: {error}");
                    break;
                }
            }
            std::thread::sleep(SERVICE_POLL_INTERVAL);
        }

        if !connected {
            host.peer_mut(peer_id).disconnect(0);
            self.room_member_impl.set_state(RoomMemberState::Idle);
            self.room_member_impl
                .set_error(RoomMemberError::CouldNotConnect);
            return;
        }

        *self.room_member_impl.nickname.lock() = nickname.to_string();
        self.start_loop(host, peer_id);
        self.room_member_impl
            .send_join_request(nickname, preferred_fake_ip, password, token);
        let game_info = self.room_member_impl.current_game_info.lock().clone();
        self.send_game_info(&game_info);
    }

    /// Spawns the receive loop. Maps to C++ `RoomMemberImpl::StartLoop`.
    fn start_loop(&self, mut host: enet::Host<UdpSocket>, peer_id: enet::PeerID) {
        let member = Arc::clone(&self.room_member_impl);
        let handle = std::thread::Builder::new()
            .name("RoomMember".to_string())
            .spawn(move || {
                while member.is_connected() {
                    match host.service() {
                        Ok(Some(enet::Event::Receive { packet, .. })) => {
                            member.handle_packet(packet.data());
                        }
                        Ok(Some(enet::Event::Disconnect { .. })) => {
                            let state = member.get_state();
                            if state == RoomMemberState::Joined
                                || state == RoomMemberState::Moderator
                            {
                                member.set_state(RoomMemberState::Idle);
                                member.set_error(RoomMemberError::LostConnection);
                            }
                        }
                        Ok(Some(enet::Event::Connect { .. })) => {
                            // Cannot happen: the connection is already established.
                            debug_assert!(
                                false,
                                "unexpected connect event while already connected"
                            );
                        }
                        Ok(None) => {}
                        Err(error) => {
                            log::error!("RoomMember: socket error: {error}");
                            member.set_state(RoomMemberState::Idle);
                            member.set_error(RoomMemberError::LostConnection);
                        }
                    }

                    let outgoing: Vec<Packet> = std::mem::take(&mut *member.send_list.lock());
                    for packet in &outgoing {
                        let enet_packet = enet::Packet::reliable(packet.get_data());
                        if let Err(error) = host.peer_mut(peer_id).send(0, &enet_packet) {
                            log::error!("RoomMember: could not send packet: {error:?}");
                        }
                    }
                    host.flush();

                    std::thread::sleep(SERVICE_POLL_INTERVAL);
                }

                member.disconnect(&mut host, peer_id);
            })
            .expect("failed to spawn the RoomMember thread");

        *self.loop_thread.lock() = Some(handle);
    }

    /// Sends a proxy packet to the room.
    pub fn send_proxy_packet(&self, proxy_packet: &ProxyPacket) {
        let mut packet = Packet::new();
        packet.write_u8(RoomMessageTypes::IdProxyPacket as u8);
        write_proxy_packet(&mut packet, proxy_packet);
        self.room_member_impl.send(packet);
    }

    /// Sends an LDN packet to the room.
    pub fn send_ldn_packet(&self, ldn_packet: &LdnPacket) {
        let mut packet = Packet::new();
        packet.write_u8(RoomMessageTypes::IdLdnPacket as u8);
        write_ldn_packet(&mut packet, ldn_packet);
        self.room_member_impl.send(packet);
    }

    /// Sends a chat message to the room.
    pub fn send_chat_message(&self, message: &str) {
        let mut packet = Packet::new();
        packet.write_u8(RoomMessageTypes::IdChatMessage as u8);
        packet.write_string(message);
        self.room_member_impl.send(packet);
    }

    /// Sends the current game info to the room.
    pub fn send_game_info(&self, game_info: &GameInfo) {
        *self.room_member_impl.current_game_info.lock() = game_info.clone();
        if !self.is_connected() {
            return;
        }
        let mut packet = Packet::new();
        packet.write_u8(RoomMessageTypes::IdSetGameInfo as u8);
        packet.write_string(&game_info.name);
        packet.write_u64(game_info.id);
        packet.write_string(&game_info.version);
        self.room_member_impl.send(packet);
    }

    /// Sends a moderation request to the room.
    pub fn send_moderation_request(&self, msg_type: RoomMessageTypes, nickname: &str) {
        assert!(
            msg_type == RoomMessageTypes::IdModKick
                || msg_type == RoomMessageTypes::IdModBan
                || msg_type == RoomMessageTypes::IdModUnban,
            "type is not a moderation request"
        );
        if !self.is_connected() {
            return;
        }
        let mut packet = Packet::new();
        packet.write_u8(msg_type as u8);
        packet.write_string(nickname);
        self.room_member_impl.send(packet);
    }

    /// Attempts to retrieve ban list from the room.
    pub fn request_ban_list(&self) {
        if !self.is_connected() {
            return;
        }
        let mut packet = Packet::new();
        packet.write_u8(RoomMessageTypes::IdModGetBanList as u8);
        self.room_member_impl.send(packet);
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

    /// Rust counterpart of upstream's templated `RoomMember::Unbind` for the
    /// chat callback set.
    pub fn unbind_on_chat_message_received(&self, handle: &CallbackHandle<ChatEntry>) {
        self.room_member_impl
            .callbacks_chat_message
            .lock()
            .retain(|registered| !Arc::ptr_eq(registered, handle));
    }

    /// Rust counterpart of upstream's templated `RoomMember::Unbind` for the
    /// error callback set.
    pub fn unbind_on_error(&self, handle: &CallbackHandle<RoomMemberError>) {
        self.room_member_impl
            .callbacks_error
            .lock()
            .retain(|registered| !Arc::ptr_eq(registered, handle));
    }

    /// Rust counterpart of upstream's templated `RoomMember::Unbind` for the
    /// proxy-packet callback set.
    pub fn unbind_on_proxy_packet_received(&self, handle: &CallbackHandle<ProxyPacket>) {
        self.room_member_impl
            .callbacks_proxy_packet
            .lock()
            .retain(|registered| !Arc::ptr_eq(registered, handle));
    }

    /// Rust counterpart of upstream's templated `RoomMember::Unbind` for the
    /// LDN-packet callback set.
    pub fn unbind_on_ldn_packet_received(&self, handle: &CallbackHandle<LdnPacket>) {
        self.room_member_impl
            .callbacks_ldn_packet
            .lock()
            .retain(|registered| !Arc::ptr_eq(registered, handle));
    }

    /// Rust counterpart of upstream's templated `RoomMember::Unbind` for the
    /// status callback set.
    pub fn unbind_on_status_message_received(&self, handle: &CallbackHandle<StatusMessageEntry>) {
        self.room_member_impl
            .callbacks_status_message
            .lock()
            .retain(|registered| !Arc::ptr_eq(registered, handle));
    }

    /// Rust counterpart of upstream's templated `RoomMember::Unbind` for the
    /// room-information callback set.
    pub fn unbind_on_room_information_changed(&self, handle: &CallbackHandle<RoomInformation>) {
        self.room_member_impl
            .callbacks_room_information
            .lock()
            .retain(|registered| !Arc::ptr_eq(registered, handle));
    }

    /// Rust counterpart of upstream's templated `RoomMember::Unbind` for the
    /// state callback set.
    pub fn unbind_on_state_changed(&self, handle: &CallbackHandle<RoomMemberState>) {
        self.room_member_impl
            .callbacks_state
            .lock()
            .retain(|registered| !Arc::ptr_eq(registered, handle));
    }

    /// Rust counterpart of upstream's templated `RoomMember::Unbind` for the
    /// ban-list callback set.
    pub fn unbind_on_ban_list_received(&self, handle: &CallbackHandle<BanList>) {
        self.room_member_impl
            .callbacks_ban_list
            .lock()
            .retain(|registered| !Arc::ptr_eq(registered, handle));
    }

    /// Leaves the current room. Maps to C++ `RoomMember::Leave`.
    pub fn leave(&self) {
        // Clearing the state is what stops the receive loop: it polls
        // `IsConnected`, exactly like upstream. The loop then runs `Disconnect`
        // itself, so the ENet teardown stays on the thread that owns the host.
        self.room_member_impl.set_state(RoomMemberState::Idle);

        let handle = self.loop_thread.lock().take();
        if let Some(handle) = handle {
            if handle.join().is_err() {
                log::error!("RoomMember::leave: the receive thread panicked");
            }
        }
    }
}

impl Default for RoomMember {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for RoomMember {
    fn drop(&mut self) {
        // Upstream's destructor requires the receive loop to be gone before
        // destroying the ENet host. `leave` also joins the Rust owner thread.
        self.leave();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The join request is the first thing a room sees; its field order is the
    /// wire contract with every yuzu/eden room. Mirrors
    /// `RoomMemberImpl::SendJoinRequest`.
    #[test]
    fn join_request_matches_upstream_field_order() {
        let member = RoomMemberImpl::new();
        member.send_join_request("nick", &[192, 168, 0, 2], "pw", "tok");

        let queued = member.send_list.lock();
        assert_eq!(queued.len(), 1);

        let mut packet = Packet::new();
        packet.append(queued[0].get_data());

        assert_eq!(
            packet.read_u8(),
            Some(RoomMessageTypes::IdJoinRequest as u8)
        );
        assert_eq!(packet.read_string().as_deref(), Some("nick"));
        assert_eq!(packet.read_array::<4>(), Some([192, 168, 0, 2]));
        assert_eq!(packet.read_u32(), Some(NETWORK_VERSION));
        assert_eq!(packet.read_string().as_deref(), Some("pw"));
        assert_eq!(packet.read_string().as_deref(), Some("tok"));
        assert!(packet.end_of_packet());
    }

    /// A chat packet built by the room must be decoded into the entry the
    /// frontend renders. Mirrors `RoomMemberImpl::HandleChatPacket`.
    #[test]
    fn chat_packet_round_trips_through_the_handler() {
        let member = Arc::new(RoomMemberImpl::new());
        let received = Arc::new(Mutex::new(Vec::<ChatEntry>::new()));
        {
            let sink = Arc::clone(&received);
            member.callbacks_chat_message.lock().push(Arc::new(Box::new(
                move |entry: &ChatEntry| sink.lock().push(entry.clone()),
            )));
        }

        let mut wire = Packet::new();
        wire.write_u8(RoomMessageTypes::IdChatMessage as u8);
        wire.write_string("nick");
        wire.write_string("user");
        wire.write_string("hello");

        member.handle_packet(wire.get_data());

        let entries = received.lock();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].nickname, "nick");
        assert_eq!(entries[0].username, "user");
        assert_eq!(entries[0].message, "hello");
    }

    /// Terminal room messages must both reset the state and report the error,
    /// the `SetState(Idle); SetError(...)` pair upstream repeats.
    #[test]
    fn wrong_password_resets_state_and_reports_the_error() {
        let member = Arc::new(RoomMemberImpl::new());
        member.set_state(RoomMemberState::Joining);

        let errors = Arc::new(Mutex::new(Vec::<RoomMemberError>::new()));
        {
            let sink = Arc::clone(&errors);
            member.callbacks_error.lock().push(Arc::new(Box::new(
                move |error: &RoomMemberError| sink.lock().push(*error),
            )));
        }

        let mut wire = Packet::new();
        wire.write_u8(RoomMessageTypes::IdWrongPassword as u8);
        member.handle_packet(wire.get_data());

        assert_eq!(member.get_state(), RoomMemberState::Idle);
        assert_eq!(*errors.lock(), vec![RoomMemberError::WrongPassword]);
    }

    #[test]
    fn unbind_removes_only_the_requested_callback() {
        let member = RoomMember::new();
        let first = member.bind_on_chat_message_received(|_| {});
        let second = member.bind_on_chat_message_received(|_| {});

        member.unbind_on_chat_message_received(&first);

        let callbacks = member.room_member_impl.callbacks_chat_message.lock();
        assert_eq!(callbacks.len(), 1);
        assert!(Arc::ptr_eq(&callbacks[0], &second));
    }

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
