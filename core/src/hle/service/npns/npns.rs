// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/npns/npns.cpp
//!
//! INpnsSystem ("npns:s") and INpnsUser ("npns:u") services.

/// IPC command IDs for INpnsSystem (selected implemented commands)
pub mod system_commands {
    pub const LISTEN_ALL: u32 = 1;
    pub const LISTEN_TO: u32 = 2;
    pub const RECEIVE: u32 = 3;
    pub const RECEIVE_RAW: u32 = 4;
    pub const GET_RECEIVE_EVENT: u32 = 5;
    pub const LISTEN_UNDELIVERED: u32 = 6;
    pub const GET_STATE_CHANGE_EVENT: u32 = 7;
    pub const SUBSCRIBE_TOPIC: u32 = 11;
    pub const UNSUBSCRIBE_TOPIC: u32 = 12;
    pub const QUERY_IS_TOPIC_EXIST: u32 = 13;
    pub const CREATE_TOKEN: u32 = 21;
    pub const DESTROY_TOKEN: u32 = 23;
    pub const SUSPEND: u32 = 101;
    pub const RESUME: u32 = 102;
    pub const GET_STATE: u32 = 103;
}

/// INpnsSystem ("npns:s").
pub struct INpnsSystem {
    // TODO: service_context, get_receive_event
}

impl INpnsSystem {
    pub fn new() -> Self {
        Self {}
    }

    pub fn listen_to(&self, program_id: u32) {
        log::warn!("(STUBBED) INpnsSystem::listen_to called, program_id={}", program_id);
    }

    pub fn get_receive_event(&self) {
        log::warn!("(STUBBED) INpnsSystem::get_receive_event called");
        // TODO: return event handle
    }
}

/// INpnsUser ("npns:u"). All commands are unimplemented stubs.
pub struct INpnsUser;
impl INpnsUser {
    pub fn new() -> Self { Self }
}

/// Registers "npns:s" and "npns:u" services.
///
/// Corresponds to `LoopProcess` in upstream `npns.cpp`.
pub fn loop_process() {
    // TODO: register services with ServerManager
}
