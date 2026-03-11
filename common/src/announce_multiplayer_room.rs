// SPDX-FileCopyrightText: Copyright 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/common/announce_multiplayer_room.h
//!
//! Defines the data structures and backend trait for announcing and
//! discovering multiplayer rooms via a web service.

// ---------------------------------------------------------------------------
// Data structures
// ---------------------------------------------------------------------------

/// Information about the game being played by a room member.
/// Maps to C++ `AnnounceMultiplayerRoom::GameInfo`.
#[derive(Clone, Debug, Default)]
pub struct GameInfo {
    pub name: String,
    pub id: u64,
    pub version: String,
}

/// An IPv4 address as 4 bytes.
pub type IPv4Address = [u8; 4];

/// A member (player) in a multiplayer room.
/// Maps to C++ `AnnounceMultiplayerRoom::Member`.
#[derive(Clone, Debug, Default)]
pub struct Member {
    pub username: String,
    pub nickname: String,
    pub display_name: String,
    pub avatar_url: String,
    pub fake_ip: IPv4Address,
    pub game: GameInfo,
}

/// Static information about a multiplayer room.
/// Maps to C++ `AnnounceMultiplayerRoom::RoomInformation`.
#[derive(Clone, Debug, Default)]
pub struct RoomInformation {
    /// Name of the server.
    pub name: String,
    /// Server description.
    pub description: String,
    /// Maximum number of members in this room.
    pub member_slots: u32,
    /// The port of this room.
    pub port: u16,
    /// Game to advertise that you want to play.
    pub preferred_game: GameInfo,
    /// Forum username of the host.
    pub host_username: String,
    /// Allow yuzu Moderators to moderate on this room.
    pub enable_yuzu_mods: bool,
}

/// A fully described multiplayer room including members and connection info.
/// Maps to C++ `AnnounceMultiplayerRoom::Room`.
#[derive(Clone, Debug, Default)]
pub struct Room {
    pub information: RoomInformation,
    pub id: String,
    /// UID used for verification.
    pub verify_uid: String,
    pub ip: String,
    pub net_version: u32,
    pub has_password: bool,
    pub members: Vec<Member>,
}

/// A list of rooms.
pub type RoomList = Vec<Room>;

// ---------------------------------------------------------------------------
// WebResult (inline definition matching web_service/web_result.h)
// ---------------------------------------------------------------------------

/// Result of a web service operation.
/// Maps to C++ `WebService::WebResult`.
#[derive(Clone, Debug)]
pub struct WebResult {
    pub result_code: WebResultCode,
    pub result_string: String,
    pub returned_data: String,
}

/// Status codes for web service operations.
/// Maps to C++ `WebService::WebResult::Code`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WebResultCode {
    Success,
    InvalidURL,
    CredentialsMissing,
    LibError,
    HttpError,
    WrongContent,
    NoWebservice,
}

// ---------------------------------------------------------------------------
// Backend trait
// ---------------------------------------------------------------------------

/// Trait for announce multiplayer room backends.
/// Maps to C++ `AnnounceMultiplayerRoom::Backend`.
///
/// A backend submits room information to and retrieves room lists from a web
/// service.
pub trait Backend: Send + Sync {
    /// Sets the room information used for announcements.
    fn set_room_information(
        &mut self,
        name: &str,
        description: &str,
        port: u16,
        max_player: u32,
        net_version: u32,
        has_password: bool,
        preferred_game: &GameInfo,
    );

    /// Adds a player to the data that gets announced.
    fn add_player(&mut self, member: &Member);

    /// Updates the data in the announce service. Re-registers when required.
    fn update(&mut self) -> WebResult;

    /// Registers the room in the announce service.
    /// On success, `returned_data` contains a global GUID for verification.
    fn register(&mut self) -> WebResult;

    /// Empties the stored player list.
    fn clear_players(&mut self);

    /// Retrieves the list of all rooms from the announce service.
    fn get_room_list(&self) -> RoomList;

    /// Sends a delete message to the announce service.
    fn delete(&mut self);
}

// ---------------------------------------------------------------------------
// NullBackend
// ---------------------------------------------------------------------------

/// A no-op backend that drops all data. Used when no functional backend is
/// available. Maps to C++ `AnnounceMultiplayerRoom::NullBackend`.
pub struct NullBackend;

impl Backend for NullBackend {
    fn set_room_information(
        &mut self,
        _name: &str,
        _description: &str,
        _port: u16,
        _max_player: u32,
        _net_version: u32,
        _has_password: bool,
        _preferred_game: &GameInfo,
    ) {
    }

    fn add_player(&mut self, _member: &Member) {}

    fn update(&mut self) -> WebResult {
        WebResult {
            result_code: WebResultCode::NoWebservice,
            result_string: "WebService is missing".to_string(),
            returned_data: String::new(),
        }
    }

    fn register(&mut self) -> WebResult {
        WebResult {
            result_code: WebResultCode::NoWebservice,
            result_string: "WebService is missing".to_string(),
            returned_data: String::new(),
        }
    }

    fn clear_players(&mut self) {}

    fn get_room_list(&self) -> RoomList {
        RoomList::new()
    }

    fn delete(&mut self) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_null_backend_update_returns_no_webservice() {
        let mut backend = NullBackend;
        let result = backend.update();
        assert_eq!(result.result_code, WebResultCode::NoWebservice);
        assert_eq!(result.result_string, "WebService is missing");
        assert!(result.returned_data.is_empty());
    }

    #[test]
    fn test_null_backend_register_returns_no_webservice() {
        let mut backend = NullBackend;
        let result = backend.register();
        assert_eq!(result.result_code, WebResultCode::NoWebservice);
    }

    #[test]
    fn test_null_backend_get_room_list_empty() {
        let backend = NullBackend;
        let rooms = backend.get_room_list();
        assert!(rooms.is_empty());
    }

    #[test]
    fn test_default_structs() {
        let game = GameInfo::default();
        assert_eq!(game.id, 0);
        assert!(game.name.is_empty());

        let member = Member::default();
        assert_eq!(member.fake_ip, [0, 0, 0, 0]);

        let info = RoomInformation::default();
        assert_eq!(info.port, 0);
        assert_eq!(info.member_slots, 0);

        let room = Room::default();
        assert!(!room.has_password);
        assert!(room.members.is_empty());
    }
}
