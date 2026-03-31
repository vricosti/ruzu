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

    fn get_room_list(&self) -> RoomList {
        // NOTE: Would GET /lobby and parse JSON. Stubbed.
        Vec::new()
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
