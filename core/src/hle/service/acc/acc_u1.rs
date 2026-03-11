// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/acc_u1.h
//! Port of zuyu/src/core/hle/service/acc/acc_u1.cpp
//!
//! ACC_U1 service ("acc:u1").

/// IPC command IDs for ACC_U1
pub mod commands {
    pub const GET_USER_COUNT: u32 = 0;
    pub const GET_USER_EXISTENCE: u32 = 1;
    pub const LIST_ALL_USERS: u32 = 2;
    pub const LIST_OPEN_USERS: u32 = 3;
    pub const GET_LAST_OPENED_USER: u32 = 4;
    pub const GET_PROFILE: u32 = 5;
    pub const GET_PROFILE_DIGEST: u32 = 6;
    pub const IS_USER_REGISTRATION_REQUEST_PERMITTED: u32 = 50;
    pub const TRY_SELECT_USER_WITHOUT_INTERACTION: u32 = 51;
    pub const LIST_OPEN_CONTEXT_STORED_USERS: u32 = 60;
    pub const DEBUG_ACTIVATE_OPEN_CONTEXT_RETENTION: u32 = 99;
    pub const GET_USER_REGISTRATION_NOTIFIER: u32 = 100;
    pub const GET_USER_STATE_CHANGE_NOTIFIER: u32 = 101;
    pub const GET_BAAS_ACCOUNT_MANAGER_FOR_SYSTEM_SERVICE: u32 = 102;
    pub const GET_BAAS_USER_AVAILABILITY_CHANGE_NOTIFIER: u32 = 103;
    pub const GET_PROFILE_UPDATE_NOTIFIER: u32 = 104;
    pub const CHECK_NETWORK_SERVICE_AVAILABILITY_ASYNC: u32 = 105;
    pub const GET_PROFILE_SYNC_NOTIFIER: u32 = 106;
    pub const STORE_SAVE_DATA_THUMBNAIL: u32 = 110;
    pub const CLEAR_SAVE_DATA_THUMBNAIL: u32 = 111;
    pub const LOAD_SAVE_DATA_THUMBNAIL: u32 = 112;
    pub const GET_SAVE_DATA_THUMBNAIL_EXISTENCE: u32 = 113;
    pub const LIST_OPEN_USERS_IN_APPLICATION: u32 = 120;
    pub const ACTIVATE_OPEN_CONTEXT_RETENTION: u32 = 130;
    pub const LIST_QUALIFIED_USERS: u32 = 140;
    pub const AUTHENTICATE_APPLICATION_ASYNC: u32 = 150;
    pub const ENSURE_SIGNED_DEVICE_IDENTIFIER_CACHE: u32 = 151;
    pub const LOAD_SIGNED_DEVICE_IDENTIFIER_CACHE: u32 = 152;
    pub const GET_USER_LAST_OPENED_APPLICATION: u32 = 190;
    pub const ACTIVATE_OPEN_CONTEXT_HOLDER: u32 = 191;
    pub const DEBUG_INVALIDATE_TOKEN_CACHE: u32 = 997;
    pub const DEBUG_SET_USER_STATE_CLOSE: u32 = 998;
    pub const DEBUG_SET_USER_STATE_OPEN: u32 = 999;
}

/// ACC_U1 service.
///
/// Corresponds to `ACC_U1` in upstream `acc_u1.h`.
pub struct AccU1 {
    pub interface: super::acc::Interface,
}

impl AccU1 {
    pub fn new() -> Self {
        Self {
            interface: super::acc::Interface::new("acc:u1"),
        }
    }
}
