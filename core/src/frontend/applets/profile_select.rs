// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/frontend/applets/profile_select.h and profile_select.cpp
//! Profile selection applet interface.

use super::applet::Applet;

/// UUID type (128-bit).
///
/// Corresponds to upstream `Common::UUID`.
/// Placeholder alias: when `common::uuid::Uuid` is ported, replace this type alias.
pub type Uuid = u128;

/// UI mode for profile selection.
///
/// Corresponds to upstream `Service::AM::Frontend::UiMode`.
/// Local definition until hle::service::am::frontend types are ported.
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
/// Local definition until hle::service::am::frontend types are ported.
#[derive(Debug, Clone, Copy, Default)]
pub struct UiSettingsDisplayOptions {
    pub raw: u32,
}

/// Purpose of user selection.
///
/// Corresponds to upstream `Service::AM::Frontend::UserSelectionPurpose`.
/// Local definition until hle::service::am::frontend types are ported.
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
    fn select_profile(&self, callback: SelectProfileCallback, parameters: &ProfileSelectParameters);
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
        // Upstream: creates a Service::Account::ProfileManager and returns
        // manager.GetUser(Settings::values.current_user.GetValue()).
        // Settings::values.current_user is available via common::settings::values(),
        // but ProfileManager is not yet ported. Returning a default UUID matches
        // upstream's fallback when the user index yields no profile (value_or(Common::UUID{})).
        let _current_user = *common::settings::values().current_user.get_value();
        log::info!(
            "called, selecting current user {} instead of prompting...",
            _current_user
        );
        callback(Some(0u128));
    }
}
