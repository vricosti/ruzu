// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/acc.h
//! Port of zuyu/src/core/hle/service/acc/acc.cpp
//!
//! Account module and Interface base class.

use std::sync::{Arc, Mutex};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::profile_manager::{ProfileManager, ProfileBase, UserData, UserIdArray, MAX_USERS};

/// ApplicationType enum from upstream `acc.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ApplicationType {
    GameCard = 0,
    Digital = 1,
    Unknown = 3,
}

/// ApplicationInfo from upstream `acc.h` (Module::Interface private).
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct ApplicationInfo {
    pub title_id: u64,
    pub application_type: ApplicationType,
}

impl Default for ApplicationInfo {
    fn default() -> Self {
        Self {
            title_id: 0,
            application_type: ApplicationType::Unknown,
        }
    }
}

/// Module corresponds to upstream `Module` in `acc.h`.
pub struct Module;

/// Module::Interface base methods shared across ACC_U0, ACC_U1, ACC_SU, ACC_AA.
///
/// Corresponds to `Module::Interface` in upstream `acc.h`.
pub struct Interface {
    pub system: crate::core::SystemRef,
    pub module: Arc<Module>,
    pub profile_manager: Arc<Mutex<ProfileManager>>,
    pub service_name: String,
    pub application_info: ApplicationInfo,
}

impl Interface {
    /// Matches upstream `Module::Interface(shared_ptr<Module>, shared_ptr<ProfileManager>, System&, const char*)`.
    pub fn new(
        module: Arc<Module>,
        profile_manager: Arc<Mutex<ProfileManager>>,
        system: crate::core::SystemRef,
        name: &str,
    ) -> Self {
        Self {
            system,
            module,
            profile_manager,
            service_name: name.to_string(),
            application_info: ApplicationInfo::default(),
        }
    }

    pub fn get_user_count(&self, profile_manager: &ProfileManager) -> (ResultCode, u32) {
        log::debug!("Account::GetUserCount called");
        (RESULT_SUCCESS, profile_manager.get_user_count() as u32)
    }

    pub fn get_user_existence(&self, profile_manager: &ProfileManager, uuid: u128) -> (ResultCode, bool) {
        log::debug!("Account::GetUserExistence called");
        (RESULT_SUCCESS, profile_manager.user_exists(uuid))
    }

    pub fn list_all_users(&self, profile_manager: &ProfileManager) -> (ResultCode, UserIdArray) {
        log::debug!("Account::ListAllUsers called");
        (RESULT_SUCCESS, profile_manager.get_all_users())
    }

    pub fn list_open_users(&self, profile_manager: &ProfileManager) -> (ResultCode, UserIdArray) {
        log::debug!("Account::ListOpenUsers called");
        (RESULT_SUCCESS, profile_manager.get_open_users())
    }

    pub fn get_last_opened_user(&self, profile_manager: &ProfileManager) -> (ResultCode, u128) {
        log::debug!("Account::GetLastOpenedUser called");
        (RESULT_SUCCESS, profile_manager.get_last_opened_user())
    }

    pub fn get_profile(&self, _uuid: u128) -> ResultCode {
        log::debug!("Account::GetProfile called");
        // TODO: create IProfile interface
        RESULT_SUCCESS
    }

    pub fn is_user_registration_request_permitted(&self) -> (ResultCode, bool) {
        log::debug!("Account::IsUserRegistrationRequestPermitted called");
        (RESULT_SUCCESS, true)
    }

    pub fn try_select_user_without_interaction(&self, profile_manager: &ProfileManager) -> (ResultCode, u128) {
        log::debug!("Account::TrySelectUserWithoutInteraction called");
        if profile_manager.get_user_count() != 1 {
            return (RESULT_SUCCESS, 0);
        }
        (RESULT_SUCCESS, profile_manager.get_last_opened_user())
    }

    pub fn is_user_account_switch_locked(&self) -> (ResultCode, bool) {
        log::debug!("Account::IsUserAccountSwitchLocked called");
        (RESULT_SUCCESS, false)
    }

    pub fn list_open_context_stored_users(&self, profile_manager: &ProfileManager) -> (ResultCode, UserIdArray) {
        log::debug!("Account::ListOpenContextStoredUsers called");
        (RESULT_SUCCESS, profile_manager.get_stored_opened_users())
    }

    pub fn list_qualified_users(&self, profile_manager: &ProfileManager) -> (ResultCode, UserIdArray) {
        log::debug!("Account::ListQualifiedUsers called");
        (RESULT_SUCCESS, profile_manager.get_all_users())
    }

    pub fn begin_user_registration(&self, _profile_manager: &mut ProfileManager) -> (ResultCode, u128) {
        log::debug!("Account::BeginUserRegistration called");
        // TODO: generate new UUID
        (RESULT_SUCCESS, 0)
    }

    pub fn complete_user_registration(&self, _profile_manager: &mut ProfileManager, _uuid: u128) -> ResultCode {
        log::debug!("Account::CompleteUserRegistration called");
        RESULT_SUCCESS
    }

    pub fn get_profile_editor(&self, _uuid: u128) -> ResultCode {
        log::debug!("Account::GetProfileEditor called");
        // TODO: create IProfileEditor
        RESULT_SUCCESS
    }

    pub fn initialize_application_info(&mut self) -> ResultCode {
        log::debug!("Account::InitializeApplicationInfo called");
        // TODO: set application_info from process
        RESULT_SUCCESS
    }

    pub fn initialize_application_info_restricted(&mut self) -> ResultCode {
        log::debug!("Account::InitializeApplicationInfoRestricted called");
        RESULT_SUCCESS
    }

    pub fn initialize_application_info_v2(&mut self) -> ResultCode {
        log::debug!("Account::InitializeApplicationInfoV2 called");
        RESULT_SUCCESS
    }
}

/// Registers account services with the server manager.
///
/// Corresponds to `LoopProcess` in upstream `acc.cpp`.
/// Services registered: acc:u0, acc:u1, acc:su, acc:aa
pub fn loop_process() {
    log::debug!("Account::LoopProcess - registering acc:u0, acc:u1, acc:su, acc:aa");
    // TODO: create ServerManager, register named services, run server
}
