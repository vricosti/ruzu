// SPDX-FileCopyrightText: Copyright 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/network/room.h and room.cpp
//!
//! Implements the Room (server) for network multiplayer games.

use std::sync::atomic::{AtomicU8, Ordering};

use parking_lot::{Mutex, RwLock};

use common::announce_multiplayer_room::{GameInfo, IPv4Address, Member, RoomInformation};

use crate::verify_user;

// ---------------------------------------------------------------------------
// Constants (from room.h)
// ---------------------------------------------------------------------------

/// The version of this Room and RoomMember.
pub const NETWORK_VERSION: u32 = 1;

/// Default port for room connections.
pub const DEFAULT_ROOM_PORT: u16 = 24872;

/// Maximum chat message size.
pub const MAX_MESSAGE_SIZE: u32 = 500;

/// Maximum number of concurrent connections allowed to this room.
pub const MAX_CONCURRENT_CONNECTIONS: u32 = 254;

/// Number of channels used for the connection.
pub const NUM_CHANNELS: usize = 1;

/// A special IP address that tells the room to assign one automatically.
pub const NO_PREFERRED_IP: IPv4Address = [0xFF, 0xFF, 0xFF, 0xFF];

// ---------------------------------------------------------------------------
// Room message types (from room.h)
// ---------------------------------------------------------------------------

/// The different types of messages that can be sent. The first byte of each
/// packet defines the type.
/// Maps to C++ `Network::RoomMessageTypes`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum RoomMessageTypes {
    IdJoinRequest = 1,
    IdJoinSuccess,
    IdRoomInformation,
    IdSetGameInfo,
    IdProxyPacket,
    IdLdnPacket,
    IdChatMessage,
    IdNameCollision,
    IdIpCollision,
    IdVersionMismatch,
    IdWrongPassword,
    IdCloseRoom,
    IdRoomIsFull,
    IdStatusMessage,
    IdHostKicked,
    IdHostBanned,
    /// Moderation requests
    IdModKick,
    IdModBan,
    IdModUnban,
    IdModGetBanList,
    /// Moderation responses
    IdModBanListResponse,
    IdModPermissionDenied,
    IdModNoSuchUser,
    IdJoinSuccessAsMod,
}

impl RoomMessageTypes {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            1 => Some(Self::IdJoinRequest),
            2 => Some(Self::IdJoinSuccess),
            3 => Some(Self::IdRoomInformation),
            4 => Some(Self::IdSetGameInfo),
            5 => Some(Self::IdProxyPacket),
            6 => Some(Self::IdLdnPacket),
            7 => Some(Self::IdChatMessage),
            8 => Some(Self::IdNameCollision),
            9 => Some(Self::IdIpCollision),
            10 => Some(Self::IdVersionMismatch),
            11 => Some(Self::IdWrongPassword),
            12 => Some(Self::IdCloseRoom),
            13 => Some(Self::IdRoomIsFull),
            14 => Some(Self::IdStatusMessage),
            15 => Some(Self::IdHostKicked),
            16 => Some(Self::IdHostBanned),
            17 => Some(Self::IdModKick),
            18 => Some(Self::IdModBan),
            19 => Some(Self::IdModUnban),
            20 => Some(Self::IdModGetBanList),
            21 => Some(Self::IdModBanListResponse),
            22 => Some(Self::IdModPermissionDenied),
            23 => Some(Self::IdModNoSuchUser),
            24 => Some(Self::IdJoinSuccessAsMod),
            _ => None,
        }
    }
}

/// Types of system status messages.
/// Maps to C++ `Network::StatusMessageTypes`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum StatusMessageTypes {
    /// Member joining.
    IdMemberJoin = 1,
    /// Member leaving.
    IdMemberLeave,
    /// A member is kicked from the room.
    IdMemberKicked,
    /// A member is banned from the room.
    IdMemberBanned,
    /// A username / ip address is unbanned from the room.
    IdAddressUnbanned,
}

impl StatusMessageTypes {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            1 => Some(Self::IdMemberJoin),
            2 => Some(Self::IdMemberLeave),
            3 => Some(Self::IdMemberKicked),
            4 => Some(Self::IdMemberBanned),
            5 => Some(Self::IdAddressUnbanned),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
// Ban list types
// ---------------------------------------------------------------------------

pub type UsernameBanList = Vec<String>;
pub type IpBanList = Vec<String>;
pub type BanList = (UsernameBanList, IpBanList);

// ---------------------------------------------------------------------------
// Room::State
// ---------------------------------------------------------------------------

/// The state of a Room.
/// Maps to C++ `Network::Room::State`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum RoomState {
    /// The room is open and ready to accept connections.
    Open = 0,
    /// The room is not opened and can not accept connections.
    Closed = 1,
}

// ---------------------------------------------------------------------------
// RoomImpl (internal state)
// ---------------------------------------------------------------------------

/// Internal member data within the room implementation.
/// Maps to C++ `Room::RoomImpl::Member`.
struct RoomMemberEntry {
    nickname: String,
    game_info: GameInfo,
    fake_ip: IPv4Address,
    user_data: verify_user::UserData,
    // NOTE: ENetPeer* is not ported; networking layer is stubbed.
}

/// Internal room implementation.
/// Maps to C++ `Room::RoomImpl`.
struct RoomImpl {
    state: AtomicU8,
    room_information: RwLock<RoomInformation>,

    verify_uid: Mutex<String>,
    password: Mutex<String>,

    members: RwLock<Vec<RoomMemberEntry>>,
    username_ban_list: Mutex<UsernameBanList>,
    ip_ban_list: Mutex<IpBanList>,

    verify_backend: Mutex<Option<Box<dyn verify_user::Backend>>>,
}

impl RoomImpl {
    fn new() -> Self {
        Self {
            state: AtomicU8::new(RoomState::Closed as u8),
            room_information: RwLock::new(RoomInformation::default()),
            verify_uid: Mutex::new(String::new()),
            password: Mutex::new(String::new()),
            members: RwLock::new(Vec::new()),
            username_ban_list: Mutex::new(Vec::new()),
            ip_ban_list: Mutex::new(Vec::new()),
            verify_backend: Mutex::new(None),
        }
    }

    fn get_state(&self) -> RoomState {
        match self.state.load(Ordering::SeqCst) {
            0 => RoomState::Open,
            _ => RoomState::Closed,
        }
    }

    fn set_state(&self, state: RoomState) {
        self.state.store(state as u8, Ordering::SeqCst);
    }
}

// ---------------------------------------------------------------------------
// Room (public API)
// ---------------------------------------------------------------------------

/// This is what a server (person creating a server) would use.
/// Maps to C++ `Network::Room`.
pub struct Room {
    room_impl: RoomImpl,
}

impl Room {
    pub fn new() -> Self {
        Self {
            room_impl: RoomImpl::new(),
        }
    }

    /// Gets the current state of the room.
    pub fn get_state(&self) -> RoomState {
        self.room_impl.get_state()
    }

    /// Gets the room information of the room.
    pub fn get_room_information(&self) -> RoomInformation {
        self.room_impl.room_information.read().clone()
    }

    /// Gets the verify UID of this room.
    pub fn get_verify_uid(&self) -> String {
        self.room_impl.verify_uid.lock().clone()
    }

    /// Gets a list of the members connected to the room.
    pub fn get_room_member_list(&self) -> Vec<Member> {
        let members = self.room_impl.members.read();
        members
            .iter()
            .map(|m| Member {
                nickname: m.nickname.clone(),
                username: m.user_data.username.clone(),
                display_name: m.user_data.display_name.clone(),
                avatar_url: m.user_data.avatar_url.clone(),
                fake_ip: m.fake_ip,
                game: m.game_info.clone(),
            })
            .collect()
    }

    /// Checks if the room is password protected.
    pub fn has_password(&self) -> bool {
        !self.room_impl.password.lock().is_empty()
    }

    /// Creates the socket for this room.
    ///
    /// NOTE: The ENet networking layer is not ported. This method sets up room
    /// state but does not actually bind a network socket.
    #[allow(clippy::too_many_arguments)]
    pub fn create(
        &self,
        name: &str,
        description: &str,
        _server: &str,
        server_port: u16,
        password: &str,
        max_connections: u32,
        host_username: &str,
        preferred_game: GameInfo,
        verify_backend: Option<Box<dyn verify_user::Backend>>,
        ban_list: &BanList,
        enable_yuzu_mods: bool,
    ) -> bool {
        // NOTE: enet_host_create is not ported; stubbed out.
        self.room_impl.set_state(RoomState::Open);

        {
            let mut info = self.room_impl.room_information.write();
            info.name = name.to_string();
            info.description = description.to_string();
            info.member_slots = max_connections;
            info.port = server_port;
            info.preferred_game = preferred_game;
            info.host_username = host_username.to_string();
            info.enable_yuzu_mods = enable_yuzu_mods;
        }

        *self.room_impl.password.lock() = password.to_string();
        *self.room_impl.verify_backend.lock() = verify_backend;
        *self.room_impl.username_ban_list.lock() = ban_list.0.clone();
        *self.room_impl.ip_ban_list.lock() = ban_list.1.clone();

        // NOTE: StartLoop() not called because networking is stubbed.
        true
    }

    /// Sets the verification GUID of the room.
    pub fn set_verify_uid(&self, uid: &str) {
        *self.room_impl.verify_uid.lock() = uid.to_string();
    }

    /// Gets the ban list (including banned forum usernames and IPs) of the room.
    pub fn get_ban_list(&self) -> BanList {
        let usernames = self.room_impl.username_ban_list.lock().clone();
        let ips = self.room_impl.ip_ban_list.lock().clone();
        (usernames, ips)
    }

    /// Destroys the room.
    pub fn destroy(&self) {
        self.room_impl.set_state(RoomState::Closed);
        // NOTE: room_thread join and enet_host_destroy not ported.
        {
            let mut info = self.room_impl.room_information.write();
            *info = RoomInformation::default();
        }
        self.room_impl.members.write().clear();
    }
}

impl Default for Room {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_room_default_state_is_closed() {
        let room = Room::new();
        assert_eq!(room.get_state(), RoomState::Closed);
    }

    #[test]
    fn test_room_create_and_destroy() {
        let room = Room::new();
        let result = room.create(
            "Test Room",
            "A test room",
            "",
            DEFAULT_ROOM_PORT,
            "secret",
            4,
            "host",
            GameInfo::default(),
            None,
            &(vec![], vec![]),
            false,
        );
        assert!(result);
        assert_eq!(room.get_state(), RoomState::Open);
        assert!(room.has_password());

        let info = room.get_room_information();
        assert_eq!(info.name, "Test Room");
        assert_eq!(info.member_slots, 4);
        assert_eq!(info.port, DEFAULT_ROOM_PORT);

        room.destroy();
        assert_eq!(room.get_state(), RoomState::Closed);
    }

    #[test]
    fn test_room_message_types_roundtrip() {
        assert_eq!(
            RoomMessageTypes::from_u8(RoomMessageTypes::IdJoinRequest as u8),
            Some(RoomMessageTypes::IdJoinRequest)
        );
        assert_eq!(
            RoomMessageTypes::from_u8(RoomMessageTypes::IdJoinSuccessAsMod as u8),
            Some(RoomMessageTypes::IdJoinSuccessAsMod)
        );
        assert_eq!(RoomMessageTypes::from_u8(0), None);
        assert_eq!(RoomMessageTypes::from_u8(255), None);
    }

    #[test]
    fn test_status_message_types_roundtrip() {
        assert_eq!(
            StatusMessageTypes::from_u8(1),
            Some(StatusMessageTypes::IdMemberJoin)
        );
        assert_eq!(
            StatusMessageTypes::from_u8(5),
            Some(StatusMessageTypes::IdAddressUnbanned)
        );
        assert_eq!(StatusMessageTypes::from_u8(0), None);
    }

    #[test]
    fn test_no_preferred_ip() {
        assert_eq!(NO_PREFERRED_IP, [0xFF, 0xFF, 0xFF, 0xFF]);
    }
}
