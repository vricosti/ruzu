// SPDX-FileCopyrightText: Copyright 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of Eden src/web_service/announce_room_json.h and announce_room_json.cpp
//!
//! Implementation of `AnnounceMultiplayerRoom::Backend` that (de)serializes
//! room information into/from JSON, and submits/gets it to/from the web
//! service.

use std::thread::JoinHandle;

use common::announce_multiplayer_room::{
    Backend, GameInfo, Member, Room, RoomList, WebResult, WebResultCode,
};

use crate::web_backend::Client;

/// Implementation of `AnnounceMultiplayerRoom::Backend` that serializes room
/// information as JSON and communicates with the web service.
/// Maps to C++ `WebService::RoomJson`.
pub struct RoomJson {
    detached_tasks: Vec<JoinHandle<()>>,
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
            detached_tasks: Vec::new(),
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
        let path = format!("/lobby/{}", self.room_id);
        let players: Vec<_> = self.room.members.iter().map(member_to_json).collect();
        let data = serde_json::json!({"players": players}).to_string();
        self.client.post_json(&path, &data, false)
    }

    fn register(&mut self) -> WebResult {
        let data = room_to_json(&self.room).to_string();
        let result = self.client.post_json("/lobby", &data, false);
        if result.result_code != WebResultCode::Success {
            return result;
        }
        let reply: serde_json::Value = serde_json::from_str(&result.returned_data)
            .expect("RoomJson::register received invalid JSON");
        self.room = parse_room(&reply).expect("RoomJson::register response is missing room fields");
        self.room_id = reply
            .get("id")
            .and_then(serde_json::Value::as_str)
            .expect("RoomJson::register response is missing id")
            .to_string();
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
        let path = format!("/lobby/{}", self.room_id);
        let mut client = Client::new(self.host.clone(), self.username.clone(), self.token.clone());
        self.detached_tasks.push(std::thread::spawn(move || {
            let _ = client.delete_json(&path, "", false);
        }));
    }
}

impl Drop for RoomJson {
    fn drop(&mut self) {
        for task in self.detached_tasks.drain(..) {
            let _ = task.join();
        }
    }
}

fn member_to_json(member: &Member) -> serde_json::Value {
    let mut value = serde_json::Map::new();
    if !member.username.is_empty() {
        value.insert("username".to_string(), member.username.clone().into());
    }
    value.insert("nickname".to_string(), member.nickname.clone().into());
    if !member.avatar_url.is_empty() {
        value.insert("avatarUrl".to_string(), member.avatar_url.clone().into());
    }
    value.insert("gameName".to_string(), member.game.name.clone().into());
    value.insert("gameId".to_string(), member.game.id.into());
    serde_json::Value::Object(value)
}

fn room_to_json(room: &Room) -> serde_json::Value {
    let mut value = serde_json::Map::new();
    value.insert("port".to_string(), room.information.port.into());
    value.insert("name".to_string(), room.information.name.clone().into());
    if !room.information.description.is_empty() {
        value.insert(
            "description".to_string(),
            room.information.description.clone().into(),
        );
    }
    value.insert(
        "preferredGameName".to_string(),
        room.information.preferred_game.name.clone().into(),
    );
    value.insert(
        "preferredGameId".to_string(),
        room.information.preferred_game.id.into(),
    );
    value.insert(
        "maxPlayers".to_string(),
        room.information.member_slots.into(),
    );
    value.insert("netVersion".to_string(), room.net_version.into());
    value.insert("hasPassword".to_string(), room.has_password.into());
    if !room.members.is_empty() {
        value.insert(
            "players".to_string(),
            room.members.iter().map(member_to_json).collect(),
        );
    }
    serde_json::Value::Object(value)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{Read, Write};
    use std::time::Duration;

    fn read_http_request(stream: &mut std::net::TcpStream) -> String {
        stream
            .set_read_timeout(Some(Duration::from_secs(2)))
            .unwrap();
        let mut bytes = Vec::new();
        let mut buffer = [0_u8; 2048];
        let (header_end, content_length) = loop {
            let count = stream.read(&mut buffer).unwrap();
            assert_ne!(count, 0);
            bytes.extend_from_slice(&buffer[..count]);
            if let Some(header_end) = bytes.windows(4).position(|part| part == b"\r\n\r\n") {
                let headers = String::from_utf8_lossy(&bytes[..header_end]);
                let content_length = headers
                    .lines()
                    .find_map(|line| {
                        let (name, value) = line.split_once(':')?;
                        name.eq_ignore_ascii_case("content-length")
                            .then(|| value.trim().parse::<usize>().unwrap())
                    })
                    .unwrap_or(0);
                break (header_end + 4, content_length);
            }
        };
        while bytes.len() < header_end + content_length {
            let count = stream.read(&mut buffer).unwrap();
            assert_ne!(count, 0);
            bytes.extend_from_slice(&buffer[..count]);
        }
        String::from_utf8(bytes).unwrap()
    }

    fn write_http_response(stream: &mut std::net::TcpStream, content_type: &str, body: &str) {
        write!(
            stream,
            "HTTP/1.1 200 OK\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
            body.len()
        )
        .unwrap();
    }

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

    #[test]
    fn register_update_and_delete_use_edens_json_payloads() {
        const ROOM_REPLY: &str = r#"{"id":"free-room-id","externalGuid":"verification-id",
            "address":"127.0.0.1","name":"Free Room","owner":"FreeHost","port":24872,
            "preferredGameName":"OpenArenaNX","preferredGameId":0,"maxPlayers":4,
            "netVersion":1,"hasPassword":false,"players":[
            {"nickname":"FreePlayer","gameName":"OpenArenaNX","gameId":0}]}"#;
        let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let address = listener.local_addr().unwrap();
        let server = std::thread::spawn(move || loop {
            let (mut stream, _) = listener.accept().unwrap();
            let request = read_http_request(&mut stream);
            let request_line = request.lines().next().unwrap();
            if request_line.starts_with("POST /jwt/internal ") {
                write_http_response(&mut stream, "text/html", "free-jwt");
            } else if request_line.starts_with("POST /lobby ") {
                let body = request.split_once("\r\n\r\n").unwrap().1;
                let json: serde_json::Value = serde_json::from_str(body).unwrap();
                assert_eq!(json["name"], "Free Room");
                assert_eq!(json["preferredGameName"], "OpenArenaNX");
                assert_eq!(json["players"][0]["nickname"], "FreePlayer");
                assert!(json.get("description").is_none());
                write_http_response(&mut stream, "application/json", ROOM_REPLY);
            } else if request_line.starts_with("POST /lobby/free-room-id ") {
                let body = request.split_once("\r\n\r\n").unwrap().1;
                let json: serde_json::Value = serde_json::from_str(body).unwrap();
                assert_eq!(json.as_object().unwrap().len(), 1);
                assert_eq!(json["players"][0]["nickname"], "FreePlayer");
                write_http_response(&mut stream, "application/json", "{}");
            } else if request_line.starts_with("DELETE /lobby/free-room-id ") {
                write_http_response(&mut stream, "application/json", "{}");
                break;
            } else {
                panic!("unexpected request: {request_line}");
            }
        });

        {
            let mut room = RoomJson::new(&format!("http://{address}"), "FreeHost", "free-token");
            room.set_room_information(
                "Free Room",
                "",
                24872,
                4,
                1,
                false,
                &GameInfo {
                    name: "OpenArenaNX".to_string(),
                    id: 0,
                    version: "1.0".to_string(),
                },
            );
            room.add_player(&Member {
                nickname: "FreePlayer".to_string(),
                game: GameInfo {
                    name: "OpenArenaNX".to_string(),
                    id: 0,
                    version: "1.0".to_string(),
                },
                ..Member::default()
            });
            let result = room.register();
            assert_eq!(result.result_code, WebResultCode::Success);
            assert_eq!(result.returned_data, "verification-id");
            assert_eq!(room.room_id, "free-room-id");
            assert_eq!(room.update().result_code, WebResultCode::Success);
            room.delete();
        }
        server.join().unwrap();
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
/// aborts parsing just as nlohmann's unchecked conversion does upstream.
pub fn parse_room_list(body: &str) -> RoomList {
    let document = serde_json::from_str::<serde_json::Value>(body)
        .expect("RoomJson::get_room_list received invalid JSON");
    let rooms = document
        .get("rooms")
        .and_then(serde_json::Value::as_array)
        .expect("RoomJson::get_room_list response has no rooms array");

    rooms
        .iter()
        .map(|room| parse_room(room).expect("RoomJson room is missing a required field"))
        .collect()
}

fn parse_room(value: &serde_json::Value) -> Option<Room> {
    let mut room = Room {
        verify_uid: value.get("externalGuid")?.as_str()?.to_string(),
        ip: value.get("address")?.as_str()?.to_string(),
        net_version: json_unsigned(value, "netVersion")? as u32,
        has_password: value.get("hasPassword")?.as_bool()?,
        ..Room::default()
    };
    room.information.name = value.get("name")?.as_str()?.to_string();
    room.information.host_username = value.get("owner")?.as_str()?.to_string();
    room.information.port = json_unsigned(value, "port")? as u16;
    room.information.preferred_game.name = value.get("preferredGameName")?.as_str()?.to_string();
    room.information.preferred_game.id = json_unsigned(value, "preferredGameId")?;
    room.information.member_slots = json_unsigned(value, "maxPlayers")? as u32;

    // Optional upstream-side.
    if let Some(description) = value.get("description") {
        room.information.description = description
            .as_str()
            .expect("RoomJson description has the wrong type")
            .to_string();
    } else {
        log::debug!(
            "Room '{}' doesn't contain a description",
            room.information.name
        );
    }
    if let Some(players) = value.get("players") {
        let players = players
            .as_array()
            .expect("RoomJson players has the wrong type");
        match players.iter().map(parse_member).collect() {
            Ok(parsed) => room.members = parsed,
            Err(MemberParseError::MissingRequiredField) => {
                log::debug!("Out of range: player is missing a required field");
            }
            Err(MemberParseError::InvalidFieldType) => {
                panic!("RoomJson player field has the wrong type")
            }
        }
    } else {
        log::debug!("Out of range: room doesn't contain players");
    }

    Some(room)
}

fn json_unsigned(value: &serde_json::Value, key: &str) -> Option<u64> {
    let number = value.get(key)?;
    number
        .as_u64()
        .or_else(|| number.as_i64().map(|number| number as u64))
}

enum MemberParseError {
    MissingRequiredField,
    InvalidFieldType,
}

fn member_string(value: &serde_json::Value, key: &str) -> Result<String, MemberParseError> {
    value
        .get(key)
        .ok_or(MemberParseError::MissingRequiredField)?
        .as_str()
        .map(str::to_string)
        .ok_or(MemberParseError::InvalidFieldType)
}

fn member_u64(value: &serde_json::Value, key: &str) -> Result<u64, MemberParseError> {
    let number = value
        .get(key)
        .ok_or(MemberParseError::MissingRequiredField)?;
    number
        .as_u64()
        .or_else(|| number.as_i64().map(|number| number as u64))
        .ok_or(MemberParseError::InvalidFieldType)
}

fn parse_member(value: &serde_json::Value) -> Result<Member, MemberParseError> {
    let mut member = Member {
        nickname: member_string(value, "nickname")?,
        ..Member::default()
    };
    member.game.name = member_string(value, "gameName")?;
    member.game.id = member_u64(value, "gameId")?;
    // Upstream reads both inside one try block and clears both if either key is
    // absent.
    if value.get("username").is_some() && value.get("avatarUrl").is_some() {
        member.username = member_string(value, "username")?;
        member.avatar_url = member_string(value, "avatarUrl")?;
    } else {
        log::debug!("Member '{}' isn't authenticated", member.nickname);
    }
    Ok(member)
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
          "preferredGameName": "OpenArenaNX",
          "preferredGameId": 0,
          "maxPlayers": 8,
          "netVersion": 1,
          "hasPassword": true,
          "players": [
            {"nickname": "nick", "gameName": "OpenArenaNX", "gameId": 0,
             "username": "web", "avatarUrl": "http://example/a.png"},
            {"nickname": "anon", "gameName": "OpenArenaNX", "gameId": 0}
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
        assert_eq!(room.information.preferred_game.name, "OpenArenaNX");
        assert_eq!(room.information.preferred_game.id, 0);
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
            "port":1,"preferredGameName":"OpenArenaNX","preferredGameId":0,"maxPlayers":3,
            "netVersion":4,"hasPassword":false}]}"#;
        let rooms = parse_room_list(body);
        assert_eq!(rooms.len(), 1);
        assert!(rooms[0].information.description.is_empty());
        assert!(rooms[0].members.is_empty());
    }

    #[test]
    fn signed_json_numbers_preserve_upstreams_unsigned_casts() {
        let body = r#"{"rooms":[{"externalGuid":"g","address":"a","name":"n","owner":"o",
            "port":-1,"preferredGameName":"OpenArenaNX","preferredGameId":-1,
            "maxPlayers":-1,"netVersion":-1,"hasPassword":false}]}"#;
        let rooms = parse_room_list(body);
        assert_eq!(rooms[0].information.port, u16::MAX);
        assert_eq!(rooms[0].information.preferred_game.id, u64::MAX);
        assert_eq!(rooms[0].information.member_slots, u32::MAX);
        assert_eq!(rooms[0].net_version, u32::MAX);
    }

    #[test]
    fn partial_member_identity_is_cleared_like_upstreams_single_try_block() {
        let body = r#"{"rooms":[{"externalGuid":"g","address":"a","name":"n","owner":"o",
            "port":1,"preferredGameName":"OpenArenaNX","preferredGameId":0,"maxPlayers":3,
            "netVersion":4,"hasPassword":false,"players":[
            {"nickname":"nick","gameName":"OpenArenaNX","gameId":0,"username":"web"}]}]}"#;
        let rooms = parse_room_list(body);
        assert_eq!(rooms.len(), 1);
        assert!(rooms[0].members[0].username.is_empty());
        assert!(rooms[0].members[0].avatar_url.is_empty());
    }

    #[test]
    fn a_missing_required_player_field_clears_the_whole_list_like_upstreams_try_block() {
        let body = r#"{"rooms":[{"externalGuid":"g","address":"a","name":"n","owner":"o",
            "port":1,"preferredGameName":"OpenArenaNX","preferredGameId":0,"maxPlayers":3,
            "netVersion":4,"hasPassword":false,"players":[
            {"nickname":"complete","gameName":"OpenArenaNX","gameId":0},
            {"nickname":"incomplete","gameName":"OpenArenaNX"}]}]}"#;
        let rooms = parse_room_list(body);
        assert!(rooms[0].members.is_empty());
    }

    #[test]
    #[should_panic(expected = "description has the wrong type")]
    fn an_optional_field_with_the_wrong_type_fails_like_upstream() {
        let body = r#"{"rooms":[{"externalGuid":"g","address":"a","name":"n","owner":"o",
            "description":1,"port":1,"preferredGameName":"OpenArenaNX",
            "preferredGameId":0,"maxPlayers":3,"netVersion":4,"hasPassword":false}]}"#;
        let _ = parse_room_list(body);
    }

    #[test]
    #[should_panic(expected = "RoomJson room is missing a required field")]
    fn a_room_missing_a_required_key_fails_like_upstream() {
        let body = r#"{"rooms":[{"name":"incomplete"},
            {"externalGuid":"g","address":"a","name":"ok","owner":"o","port":1,
             "preferredGameName":"OpenArenaNX","preferredGameId":0,"maxPlayers":3,
             "netVersion":4,"hasPassword":false}]}"#;
        let _ = parse_room_list(body);
    }

    #[test]
    #[should_panic(expected = "response has no rooms array")]
    fn a_body_without_a_rooms_array_fails_like_upstream() {
        let _ = parse_room_list("{}");
    }

    #[test]
    #[should_panic(expected = "received invalid JSON")]
    fn invalid_json_fails_like_upstream() {
        let _ = parse_room_list("not json");
    }
}
