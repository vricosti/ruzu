// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/npns/npns.cpp
//!
//! INpnsSystem ("npns:s") and INpnsUser ("npns:u") services.

/// IPC command IDs for INpnsSystem ("npns:s").
///
/// Corresponds to the function table in `INpnsSystem` constructor (upstream npns.cpp).
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
    pub const CREATE_TOKEN_WITH_APPLICATION_ID: u32 = 22;
    pub const DESTROY_TOKEN: u32 = 23;
    pub const DESTROY_TOKEN_WITH_APPLICATION_ID: u32 = 24;
    pub const QUERY_IS_TOKEN_VALID: u32 = 25;
    pub const LISTEN_TO_MY_APPLICATION_ID: u32 = 26;
    pub const DESTROY_TOKEN_ALL: u32 = 27;
    pub const UPLOAD_TOKEN_TO_BAAS: u32 = 31;
    pub const DESTROY_TOKEN_FOR_BAAS: u32 = 32;
    pub const CREATE_TOKEN_FOR_BAAS: u32 = 33;
    pub const SET_BAAS_DEVICE_ACCOUNT_ID_LIST: u32 = 34;
    pub const SUSPEND: u32 = 101;
    pub const RESUME: u32 = 102;
    pub const GET_STATE: u32 = 103;
    pub const GET_STATISTICS: u32 = 104;
    pub const GET_PLAY_REPORT_REQUEST_EVENT: u32 = 105;
    pub const GET_JID: u32 = 111;
    pub const CREATE_JID: u32 = 112;
    pub const DESTROY_JID: u32 = 113;
    pub const ATTACH_JID: u32 = 114;
    pub const DETACH_JID: u32 = 115;
    pub const CREATE_NOTIFICATION_RECEIVER: u32 = 120;
    pub const GET_STATE_WITH_HANDOVER: u32 = 151;
    pub const GET_STATE_CHANGE_EVENT_WITH_HANDOVER: u32 = 152;
    pub const GET_DROP_EVENT_WITH_HANDOVER: u32 = 153;
    pub const CREATE_TOKEN_ASYNC: u32 = 154;
    pub const CREATE_TOKEN_ASYNC_WITH_APPLICATION_ID: u32 = 155;
    pub const GET_REQUEST_CHANGE_STATE_CANCEL_EVENT: u32 = 161;
    pub const REQUEST_CHANGE_STATE_FORCE_TIMED_WITH_CANCEL_EVENT: u32 = 162;
    pub const REQUEST_CHANGE_STATE_FORCE_TIMED: u32 = 201;
    pub const REQUEST_CHANGE_STATE_FORCE_ASYNC: u32 = 202;
}

/// IPC command IDs for INpnsUser ("npns:u").
///
/// Corresponds to the function table in `INpnsUser` constructor (upstream npns.cpp).
pub mod user_commands {
    pub const LISTEN_ALL: u32 = 1;
    pub const LISTEN_TO: u32 = 2;
    pub const RECEIVE: u32 = 3;
    pub const RECEIVE_RAW: u32 = 4;
    pub const GET_RECEIVE_EVENT: u32 = 5;
    pub const GET_STATE_CHANGE_EVENT: u32 = 7;
    pub const CREATE_TOKEN: u32 = 21;
    pub const DESTROY_TOKEN: u32 = 23;
    pub const QUERY_IS_TOKEN_VALID: u32 = 25;
    pub const LISTEN_TO_MY_APPLICATION_ID: u32 = 26;
    pub const SUSPEND: u32 = 101;
    pub const RESUME: u32 = 102;
    pub const GET_STATE: u32 = 103;
    pub const GET_STATISTICS: u32 = 104;
    pub const GET_JID: u32 = 111;
    pub const CREATE_NOTIFICATION_RECEIVER: u32 = 120;
    pub const GET_STATE_WITH_HANDOVER: u32 = 151;
    pub const GET_STATE_CHANGE_EVENT_WITH_HANDOVER: u32 = 152;
    pub const GET_DROP_EVENT_WITH_HANDOVER: u32 = 153;
    pub const CREATE_TOKEN_ASYNC: u32 = 154;
}

/// INpnsSystem service ("npns:s").
///
/// Corresponds to `INpnsSystem` in upstream npns.cpp.
pub struct INpnsSystem {
    // In upstream: service_context and get_receive_event (KEvent)
    // TODO: add when kernel event support is available
}

impl INpnsSystem {
    pub fn new() -> Self {
        // Upstream creates get_receive_event = service_context.CreateEvent("npns:s:GetReceiveEvent")
        Self {}
    }

    /// ListenTo (cmd 2).
    ///
    /// Corresponds to `INpnsSystem::ListenTo` in upstream npns.cpp.
    pub fn listen_to(&self, program_id: u32) {
        log::warn!(
            "(STUBBED) INpnsSystem::listen_to called, program_id={}",
            program_id
        );
    }

    /// GetReceiveEvent (cmd 5).
    ///
    /// Corresponds to `INpnsSystem::GetReceiveEvent` in upstream npns.cpp.
    /// Returns the get_receive_event's readable event handle.
    pub fn get_receive_event(&self) {
        log::warn!("(STUBBED) INpnsSystem::get_receive_event called");
        // TODO: return get_receive_event->GetReadableEvent()
    }
}

/// INpnsUser service ("npns:u").
///
/// Corresponds to `INpnsUser` in upstream npns.cpp.
/// All commands are nullptr (unimplemented) in upstream.
pub struct INpnsUser;

impl INpnsUser {
    pub fn new() -> Self {
        Self
    }
}

/// Registers "npns:s" and "npns:u" services.
///
/// Corresponds to `LoopProcess` in upstream npns.cpp.
pub fn loop_process() {
    log::debug!("NPNS::LoopProcess called");
    // TODO: register services with ServerManager
}
