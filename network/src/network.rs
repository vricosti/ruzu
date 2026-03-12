// SPDX-FileCopyrightText: Copyright 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/network/network.h and network.cpp
//!
//! Provides the `RoomNetwork` struct which owns and coordinates the Room
//! (server) and RoomMember (client) for network games.

use std::sync::{Arc, Weak};

use crate::room::{Room, RoomState};
use crate::room_member::RoomMember;

/// Owns the Room and RoomMember handles for the network subsystem.
/// Maps to C++ `Network::RoomNetwork`.
pub struct RoomNetwork {
    /// RoomMember (Client) for network games.
    m_room_member: Arc<RoomMember>,
    /// Room (Server) for network games.
    m_room: Arc<Room>,
}

impl RoomNetwork {
    pub fn new() -> Self {
        Self {
            m_room_member: Arc::new(RoomMember::new()),
            m_room: Arc::new(Room::new()),
        }
    }

    /// Initializes and registers the network device, the room, and the room
    /// member.
    ///
    /// NOTE: `enet_initialize()` is not ported; ENet is not used in Rust.
    /// This method re-creates the Room and RoomMember instances.
    pub fn init(&mut self) -> bool {
        // NOTE: enet_initialize() call omitted; no ENet in Rust port.
        self.m_room = Arc::new(Room::new());
        self.m_room_member = Arc::new(RoomMember::new());
        log::debug!("initialized OK");
        true
    }

    /// Returns a weak pointer to the room handle.
    pub fn get_room(&self) -> Weak<Room> {
        Arc::downgrade(&self.m_room)
    }

    /// Returns a weak pointer to the room member handle.
    pub fn get_room_member(&self) -> Weak<RoomMember> {
        Arc::downgrade(&self.m_room_member)
    }

    /// Unregisters the network device, the room, and the room member and shuts
    /// them down.
    pub fn shutdown(&mut self) {
        if self.m_room_member.is_connected() {
            self.m_room_member.leave();
        }

        if self.m_room.get_state() == RoomState::Open {
            self.m_room.destroy();
        }

        // NOTE: enet_deinitialize() call omitted.
        log::debug!("shutdown OK");
    }
}

impl Default for RoomNetwork {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_room_network_init_and_shutdown() {
        let mut rn = RoomNetwork::new();
        assert!(rn.init());
        assert!(rn.get_room().upgrade().is_some());
        assert!(rn.get_room_member().upgrade().is_some());
        rn.shutdown();
    }
}
