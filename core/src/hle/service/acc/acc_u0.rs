// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/acc_u0.h
//! Port of zuyu/src/core/hle/service/acc/acc_u0.cpp
//!
//! ACC_U0 service ("acc:u0").

/// IPC command IDs for ACC_U0
pub mod commands {
    pub const GET_USER_COUNT: u32 = 0;
    pub const GET_USER_EXISTENCE: u32 = 1;
    pub const LIST_ALL_USERS: u32 = 2;
    pub const LIST_OPEN_USERS: u32 = 3;
    pub const GET_LAST_OPENED_USER: u32 = 4;
    pub const GET_PROFILE: u32 = 5;
    pub const GET_PROFILE_DIGEST: u32 = 6;        // 3.0.0+
    pub const IS_USER_REGISTRATION_REQUEST_PERMITTED: u32 = 50;
    pub const TRY_SELECT_USER_WITHOUT_INTERACTION: u32 = 51;
    pub const LIST_OPEN_CONTEXT_STORED_USERS: u32 = 60; // 5.0.0 - 5.1.0
    pub const DEBUG_ACTIVATE_OPEN_CONTEXT_RETENTION: u32 = 99; // 6.0.0+
    pub const INITIALIZE_APPLICATION_INFO: u32 = 100;
    pub const GET_BAAS_ACCOUNT_MANAGER_FOR_APPLICATION: u32 = 101;
    pub const AUTHENTICATE_APPLICATION_ASYNC: u32 = 102;
    pub const CHECK_NETWORK_SERVICE_AVAILABILITY_ASYNC: u32 = 103; // 4.0.0+
    pub const STORE_SAVE_DATA_THUMBNAIL: u32 = 110;
    pub const CLEAR_SAVE_DATA_THUMBNAIL: u32 = 111;
    pub const CREATE_GUEST_LOGIN_REQUEST: u32 = 120;
    pub const LOAD_OPEN_CONTEXT: u32 = 130;        // 5.0.0+
    pub const LIST_OPEN_CONTEXT_STORED_USERS_V2: u32 = 131; // 6.0.0+
    pub const INITIALIZE_APPLICATION_INFO_RESTRICTED: u32 = 140; // 6.0.0+
    pub const LIST_QUALIFIED_USERS: u32 = 141;     // 6.0.0+
    pub const IS_USER_ACCOUNT_SWITCH_LOCKED: u32 = 150; // 6.0.0+
    pub const INITIALIZE_APPLICATION_INFO_V2: u32 = 160;
}

/// ACC_U0 service.
///
/// Corresponds to `ACC_U0` in upstream `acc_u0.h`.
pub struct AccU0 {
    pub interface: super::acc::Interface,
}

impl AccU0 {
    /// Matches upstream `ACC_U0(shared_ptr<Module>, shared_ptr<ProfileManager>, System&)`.
    pub fn new(
        module: std::sync::Arc<super::acc::Module>,
        profile_manager: std::sync::Arc<std::sync::Mutex<super::profile_manager::ProfileManager>>,
        system: crate::core::SystemRef,
    ) -> Self {
        Self {
            interface: super::acc::Interface::new(module, profile_manager, system, "acc:u0"),
        }
    }
}
