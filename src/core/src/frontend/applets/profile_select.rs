// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `core/frontend/applets/profile_select.{h,cpp}`.
//! Profile selection applet interface.

use super::applet::Applet;
pub use crate::hle::service::am::frontend::applet_profile_select::{
    UiMode, UiSettingsDisplayOptions, UserSelectionPurpose,
};

/// UUID type used by the profile-select frontend.
pub type Uuid = common::uuid::UUID;

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

/// Default profile selection applet implementation.
///
/// Corresponds to upstream `Core::Frontend::DefaultProfileSelectApplet`.
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
        let manager = crate::hle::service::acc::profile_manager::ProfileManager::new();
        let current_user = *common::settings::values().current_user.get_value() as usize;
        let uuid = manager
            .get_user(current_user)
            .map(|uuid| common::uuid::UUID::from_bytes(uuid.to_le_bytes()))
            .unwrap_or_default();
        callback(Some(uuid));
        log::info!("called, selecting current user instead of prompting...");
    }
}
