// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/acc_su.h
//! Port of zuyu/src/core/hle/service/acc/acc_su.cpp
//!
//! ACC_SU service ("acc:su").

/// IPC command IDs for ACC_SU
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
    pub const BEGIN_USER_REGISTRATION: u32 = 200;
    pub const COMPLETE_USER_REGISTRATION: u32 = 201;
    pub const CANCEL_USER_REGISTRATION: u32 = 202;
    pub const DELETE_USER: u32 = 203;
    pub const SET_USER_POSITION: u32 = 204;
    pub const GET_PROFILE_EDITOR: u32 = 205;
    pub const COMPLETE_USER_REGISTRATION_FORCIBLY: u32 = 206;
    pub const CREATE_FLOATING_REGISTRATION_REQUEST: u32 = 210;
    pub const CREATE_PROCEDURE_TO_REGISTER_USER: u32 = 211;
    pub const RESUME_PROCEDURE_TO_REGISTER_USER: u32 = 212;
    pub const AUTHENTICATE_SERVICE_ASYNC: u32 = 230;
    pub const GET_BAAS_ACCOUNT_ADMINISTRATOR: u32 = 250;
    pub const PROXY_PROCEDURE_FOR_GUEST_LOGIN: u32 = 290;
    pub const PROXY_PROCEDURE_FOR_FLOATING_REGISTRATION: u32 = 291;
    pub const SUSPEND_BACKGROUND_DAEMON: u32 = 299;
    pub const SET_USER_UNQUALIFIED_FOR_DEBUG: u32 = 900;
    pub const UNSET_USER_UNQUALIFIED_FOR_DEBUG: u32 = 901;
    pub const LIST_USERS_UNQUALIFIED_FOR_DEBUG: u32 = 902;
    pub const REFRESH_FIRMWARE_SETTINGS_FOR_DEBUG: u32 = 910;
    pub const DEBUG_INVALIDATE_TOKEN_CACHE: u32 = 997;
    pub const DEBUG_SET_USER_STATE_CLOSE: u32 = 998;
    pub const DEBUG_SET_USER_STATE_OPEN: u32 = 999;
}

/// ACC_SU service.
///
/// Corresponds to `ACC_SU` in upstream `acc_su.h`.
pub struct AccSU {
    pub interface: super::acc::Interface,
}

impl AccSU {
    pub fn new() -> Self {
        Self {
            interface: super::acc::Interface::new("acc:su"),
        }
    }
}
