// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/frontend/applets/profile_select.h and profile_select.cpp
//! Profile selection applet interface.

use super::applet::Applet;

/// UUID type (128-bit).
///
/// Corresponds to upstream `Common::UUID`.
/// TODO: Import from common::uuid when available.
pub type Uuid = u128;

/// UI mode for profile selection.
///
/// Corresponds to upstream `Service::AM::Frontend::UiMode`.
/// TODO: Import from hle::service::am when available.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum UiMode {
    #[default]
    UserSelector = 0,
    UserCreator = 1,
    EnsureNetworkServiceAccountAvailable = 2,
    UserIconEditor = 3,
    UserNicknameEditor = 4,
    UserCreatorForStarter = 5,
    NintendoAccountAuthorizationRequestContext = 6,
    IntroduceExternalNetworkServiceAccount = 7,
    IntroduceExternalNetworkServiceAccountForRegistration = 8,
    NintendoAccountNnidLinker = 9,
    LicenseRequirementsForNetworkService = 10,
    LicenseRequirementsForNetworkServiceWithUserContextImpl = 11,
    UserCreatorForImmediateNa498AccountNsaLinking = 12,
    UserQualificationPromoter = 13,
}

/// Display options for user selection.
///
/// Corresponds to upstream `Service::AM::Frontend::UiSettingsDisplayOptions`.
/// TODO: Import from hle::service::am when available.
#[derive(Debug, Clone, Copy, Default)]
pub struct UiSettingsDisplayOptions {
    pub raw: u32,
}

/// Purpose of user selection.
///
/// Corresponds to upstream `Service::AM::Frontend::UserSelectionPurpose`.
/// TODO: Import from hle::service::am when available.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum UserSelectionPurpose {
    #[default]
    General = 0,
    GameCardRegistration = 1,
    EShopLaunch = 2,
    EShopItemShow = 3,
    PicturePost = 4,
    NintendoAccountLinkage = 5,
    SettingsUpdate = 6,
    SaveDataDeletion = 7,
    UserMigration = 8,
    SaveDataTransfer = 9,
}

/// Parameters for profile selection.
///
/// Corresponds to upstream `Core::Frontend::ProfileSelectParameters`.
#[derive(Debug, Clone, Default)]
pub struct ProfileSelectParameters {
    pub mode: UiMode,
    pub invalid_uid_list: [Uuid; 8],
    pub display_options: UiSettingsDisplayOptions,
    pub purpose: UserSelectionPurpose,
}

/// Callback type for profile selection results.
///
/// Corresponds to upstream `ProfileSelectApplet::SelectProfileCallback`.
pub type SelectProfileCallback = Box<dyn FnOnce(Option<Uuid>) + Send>;

/// Profile selection applet trait.
///
/// Corresponds to upstream `Core::Frontend::ProfileSelectApplet`.
pub trait ProfileSelectApplet: Applet {
    fn select_profile(
        &self,
        callback: SelectProfileCallback,
        parameters: &ProfileSelectParameters,
    );
}

/// Default (stub) profile selection applet implementation.
///
/// Corresponds to upstream `Core::Frontend::DefaultProfileSelectApplet`.
/// NOTE: Upstream creates a ProfileManager and queries the current user.
/// This stub returns a default UUID.
pub struct DefaultProfileSelectApplet;

impl Applet for DefaultProfileSelectApplet {
    fn close(&self) {}
}

impl ProfileSelectApplet for DefaultProfileSelectApplet {
    fn select_profile(
        &self,
        callback: SelectProfileCallback,
        _parameters: &ProfileSelectParameters,
    ) {
        // TODO: Upstream creates a ProfileManager and returns the current user.
        // For now, return a default UUID.
        log::info!("called, selecting current user instead of prompting...");
        callback(Some(0u128));
    }
}
