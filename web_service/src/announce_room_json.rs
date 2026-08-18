// SPDX-FileCopyrightText: Copyright 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/web_service/announce_room_json.h and announce_room_json.cpp
//!
//! Implementation of `AnnounceMultiplayerRoom::Backend` that (de)serializes
//! room information into/from JSON, and submits/gets it to/from the web
//! service.

use common::announce_multiplayer_room::{
    self, Backend, GameInfo, Member, Room, RoomList, WebResult, WebResultCode,
};

use crate::web_backend::Client;

/// Implementation of `AnnounceMultiplayerRoom::Backend` that serializes room
/// information as JSON and communicates with the web service.
/// Maps to C++ `WebService::RoomJson`.
pub struct RoomJson {
    room: Room,
    client: Client,
    host: String,
    username: String,
    token: String,
    room_id: String,
}

impl RoomJson {
    pub fn new(host: &str, username: &str, token: &str) -> Self {
        Self {
            room: Room::default(),
            client: Client::new(host.to_string(), username.to_string(), token.to_string()),
            host: host.to_string(),
            username: username.to_string(),
            token: token.to_string(),
            room_id: String::new(),
        }
    }
}

impl Backend for RoomJson {
    fn set_room_information(
        &mut self,
        name: &str,
        description: &str,
        port: u16,
        max_player: u32,
        net_version: u32,
        has_password: bool,
        preferred_game: &GameInfo,
    ) {
        self.room.information.name = name.to_string();
        self.room.information.description = description.to_string();
        self.room.information.port = port;
        self.room.information.member_slots = max_player;
        self.room.net_version = net_version;
        self.room.has_password = has_password;
        self.room.information.preferred_game = preferred_game.clone();
    }

    fn add_player(&mut self, member: &Member) {
        self.room.members.push(member.clone());
    }

    fn update(&mut self) -> WebResult {
        if self.room_id.is_empty() {
            log::error!("Room must be registered to be updated");
            return WebResult {
                result_code: WebResultCode::LibError,
                result_string: "Room is not registered".to_string(),
                returned_data: String::new(),
            };
        }
        // NOTE: Would serialize room.members to JSON and POST to /lobby/{room_id}.
        // Stubbed because HTTP client is not implemented.
        let path = format!("/lobby/{}", self.room_id);
        self.client.post_json(&path, "{}", false)
    }

    fn register(&mut self) -> WebResult {
        // NOTE: Would serialize room to JSON and POST to /lobby.
        // Stubbed because HTTP client is not implemented.
        let result = self.client.post_json("/lobby", "{}", false);
        if result.result_code != WebResultCode::Success {
            return result;
        }
        // NOTE: Would parse reply JSON for room_id and verify_uid.
        WebResult {
            result_code: WebResultCode::Success,
            result_string: String::new(),
            returned_data: self.room.verify_uid.clone(),
        }
    }

    fn clear_players(&mut self) {
        self.room.members.clear();
    }

    fn get_room_list(&mut self) -> RoomList {
        let reply = self.client.get_json("/lobby", true).returned_data;
        if reply.is_empty() {
            return Vec::new();
        }
        parse_room_list(&reply)
    }

    fn delete(&mut self) {
        if self.room_id.is_empty() {
            log::error!("Room must be registered to be deleted");
            return;
        }
        // NOTE: Would DELETE /lobby/{room_id} in a detached task. Stubbed.
        let path = format!("/lobby/{}", self.room_id);
        let mut client = Client::new(self.host.clone(), self.username.clone(), self.token.clone());
        let _ = client.delete_json(&path, "", false);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_room_json_set_room_information() {
        let mut rj = RoomJson::new("https://example.com", "user", "token");
        rj.set_room_information("Test", "Desc", 1234, 4, 1, true, &GameInfo::default());
        assert_eq!(rj.room.information.name, "Test");
        assert_eq!(rj.room.information.port, 1234);
        assert!(rj.room.has_password);
    }

    #[test]
    fn test_room_json_clear_players() {
        let mut rj = RoomJson::new("https://example.com", "", "");
        rj.add_player(&Member::default());
        assert_eq!(rj.room.members.len(), 1);
        rj.clear_players();
        assert!(rj.room.members.is_empty());
    }
}

/// Parses the `GET /lobby` body.
///
/// Mirrors `from_json(const nlohmann::json&, Room&)` and its `Member`
/// counterpart in `web_service/announce_room_json.cpp`: the required keys are
/// `externalGuid`, `address`, `name`, `owner`, `port`, `preferredGameName`,
/// `preferredGameId`, `maxPlayers`, `netVersion` and `hasPassword`, while
/// `description` and `players` are optional — upstream reads them inside a
/// try block and swallows an out-of-range error. A room missing a required key
/// is skipped rather than aborting the whole list.
pub fn parse_room_list(body: &str) -> RoomList {
    let Ok(document) = serde_json::from_str::<serde_json::Value>(body) else {
        log::error!("Could not parse the lobby response");
        return Vec::new();
    };
    let Some(rooms) = document.get("rooms").and_then(|rooms| rooms.as_array()) else {
        log::error!("Lobby response has no `rooms` array");
        return Vec::new();
    };

    rooms.iter().filter_map(parse_room).collect()
}

fn parse_room(value: &serde_json::Value) -> Option<Room> {
    let mut room = Room {
        verify_uid: value.get("externalGuid")?.as_str()?.to_string(),
        ip: value.get("address")?.as_str()?.to_string(),
        net_version: value.get("netVersion")?.as_u64()? as u32,
        has_password: value.get("hasPassword")?.as_bool()?,
        ..Room::default()
    };
    room.information.name = value.get("name")?.as_str()?.to_string();
    room.information.host_username = value.get("owner")?.as_str()?.to_string();
    room.information.port = value.get("port")?.as_u64()? as u16;
    room.information.preferred_game.name = value.get("preferredGameName")?.as_str()?.to_string();
    room.information.preferred_game.id = value.get("preferredGameId")?.as_u64()?;
    room.information.member_slots = value.get("maxPlayers")?.as_u64()? as u32;

    // Optional upstream-side.
    if let Some(description) = value.get("description").and_then(|d| d.as_str()) {
        room.information.description = description.to_string();
    }
    if let Some(players) = value.get("players").and_then(|p| p.as_array()) {
        room.members = players.iter().filter_map(parse_member).collect();
    }

    Some(room)
}

fn parse_member(value: &serde_json::Value) -> Option<Member> {
    let mut member = Member {
        nickname: value.get("nickname")?.as_str()?.to_string(),
        ..Member::default()
    };
    member.game.name = value.get("gameName")?.as_str()?.to_string();
    member.game.id = value.get("gameId")?.as_u64()?;
    // Upstream reads both inside one try block and clears both if either key is
    // absent.
    if let (Some(username), Some(avatar_url)) = (
        value.get("username").and_then(|u| u.as_str()),
        value.get("avatarUrl").and_then(|a| a.as_str()),
    ) {
        member.username = username.to_string();
        member.avatar_url = avatar_url.to_string();
    }
    Some(member)
}

#[cfg(test)]
mod lobby_tests {
    use super::*;

    /// Field names and optionality copied from upstream
    /// `web_service/announce_room_json.cpp` `from_json`.
    const LOBBY_BODY: &str = r#"{
      "rooms": [
        {
          "externalGuid": "guid-1",
          "address": "203.0.113.7",
          "name": "Eden room",
          "description": "a description",
          "owner": "host",
          "port": 24872,
          "preferredGameName": "Example Racing Game",
          "preferredGameId": 72899532488839168,
          "maxPlayers": 8,
          "netVersion": 1,
          "hasPassword": true,
          "players": [
            {"nickname": "nick", "gameName": "Racing Game", "gameId": 72899532488839168,
             "username": "web", "avatarUrl": "http://example/a.png"},
            {"nickname": "anon", "gameName": "Racing Game", "gameId": 72899532488839168}
          ]
        }
      ]
    }"#;

    #[test]
    fn parses_the_upstream_lobby_schema() {
        let rooms = parse_room_list(LOBBY_BODY);
        assert_eq!(rooms.len(), 1);
        let room = &rooms[0];
        assert_eq!(room.verify_uid, "guid-1");
        assert_eq!(room.ip, "203.0.113.7");
        assert_eq!(room.information.name, "Eden room");
        assert_eq!(room.information.description, "a description");
        assert_eq!(room.information.host_username, "host");
        assert_eq!(room.information.port, 24872);
        assert_eq!(room.information.preferred_game.name, "Example Racing Game");
        assert_eq!(room.information.preferred_game.id, 72899532488839168);
        assert_eq!(room.information.member_slots, 8);
        assert_eq!(room.net_version, 1);
        assert!(room.has_password);

        assert_eq!(room.members.len(), 2);
        assert_eq!(room.members[0].username, "web");
        assert_eq!(room.members[0].avatar_url, "http://example/a.png");
        // Upstream leaves these empty when the keys are absent.
        assert!(room.members[1].username.is_empty());
        assert!(room.members[1].avatar_url.is_empty());
    }

    #[test]
    fn optional_keys_may_be_absent() {
        let body = r#"{"rooms":[{"externalGuid":"g","address":"a","name":"n","owner":"o",
            "port":1,"preferredGameName":"p","preferredGameId":2,"maxPlayers":3,
            "netVersion":4,"hasPassword":false}]}"#;
        let rooms = parse_room_list(body);
        assert_eq!(rooms.len(), 1);
        assert!(rooms[0].information.description.is_empty());
        assert!(rooms[0].members.is_empty());
    }

    #[test]
    fn partial_member_identity_is_cleared_like_upstreams_single_try_block() {
        let body = r#"{"rooms":[{"externalGuid":"g","address":"a","name":"n","owner":"o",
            "port":1,"preferredGameName":"p","preferredGameId":2,"maxPlayers":3,
            "netVersion":4,"hasPassword":false,"players":[
            {"nickname":"nick","gameName":"game","gameId":2,"username":"web"}]}]}"#;
        let rooms = parse_room_list(body);
        assert_eq!(rooms.len(), 1);
        assert!(rooms[0].members[0].username.is_empty());
        assert!(rooms[0].members[0].avatar_url.is_empty());
    }

    /// A malformed entry must not take the whole listing down with it.
    #[test]
    fn a_room_missing_a_required_key_is_skipped() {
        let body = r#"{"rooms":[{"name":"incomplete"},
            {"externalGuid":"g","address":"a","name":"ok","owner":"o","port":1,
             "preferredGameName":"p","preferredGameId":2,"maxPlayers":3,
             "netVersion":4,"hasPassword":false}]}"#;
        let rooms = parse_room_list(body);
        assert_eq!(rooms.len(), 1);
        assert_eq!(rooms[0].information.name, "ok");
    }

    #[test]
    fn a_body_without_a_rooms_array_yields_nothing() {
        assert!(parse_room_list("{}").is_empty());
        assert!(parse_room_list("not json").is_empty());
    }
}
