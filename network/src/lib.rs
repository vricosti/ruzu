// SPDX-FileCopyrightText: Copyright 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/network/
//!
//! Network multiplayer room system: rooms, room members, packet
//! serialization, user verification, and session announcement.

pub mod announce_multiplayer_session;
pub mod network;
pub mod packet;
pub mod room;
pub mod room_member;
pub mod verify_user;
