// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pctl/parental_control_service.h
//! Port of zuyu/src/core/hle/service/pctl/parental_control_service.cpp
//!
//! IParentalControlService — actual parental control operations.

use crate::hle::result::ResultCode;

/// IPC command table for IParentalControlService.
pub mod commands {
    pub const INITIALIZE: u32 = 1;
    pub const CHECK_FREE_COMMUNICATION_PERMISSION: u32 = 1001;
    pub const CONFIRM_LAUNCH_APPLICATION_PERMISSION: u32 = 1002;
    pub const CONFIRM_RESUME_APPLICATION_PERMISSION: u32 = 1003;
    pub const CONFIRM_SNS_POST_PERMISSION: u32 = 1004;
    pub const CONFIRM_SYSTEM_SETTINGS_PERMISSION: u32 = 1005;
    pub const IS_RESTRICTION_ENABLED: u32 = 1006;
    pub const GET_SAFETY_LEVEL: u32 = 1007;
    pub const SET_SAFETY_LEVEL: u32 = 1008;
    pub const GET_SAFETY_LEVEL_SETTINGS: u32 = 1009;
    pub const GET_CURRENT_SETTINGS: u32 = 1010;
    pub const SET_CUSTOM_SAFETY_LEVEL_SETTINGS: u32 = 1011;
    pub const GET_DEFAULT_SAFETY_LEVEL_SETTINGS: u32 = 1012;
    pub const SET_DEFAULT_RATING_ORGANIZATION: u32 = 1013;
    pub const GET_DEFAULT_RATING_ORGANIZATION: u32 = 1014;
    pub const IS_FREE_COMMUNICATION_AVAILABLE: u32 = 1031;
    pub const IS_STEREO_VISION_PERMITTED: u32 = 1061;
    pub const IS_PAIRING_ACTIVE: u32 = 1403;
    pub const GET_SETTINGS_LAST_UPDATED_TICK: u32 = 1411;
    pub const IS_ALL_FEATURES_DISABLED: u32 = 1431;
    pub const GET_SYNCHRONIZED_NS_SETTINGS_HASH: u32 = 1432;
    pub const CONFIRM_STEREO_VISION_PERMISSION: u32 = 1433;
    pub const RESET_CONFIRMED_STEREO_VISION_PERMISSION: u32 = 1941;
    pub const IS_STEREO_VISION_DENIED: u32 = 2001;
    pub const SET_STEREO_VISION_RESTRICTION: u32 = 2002;
    pub const GET_STEREO_VISION_RESTRICTION: u32 = 2003;
    pub const RESET_CONFIRMED_STEREO_VISION_RESTRICTION: u32 = 2004;
}

/// IParentalControlService.
///
/// Corresponds to `IParentalControlService` in upstream.
pub struct IParentalControlService {
    capability: u32,
    stereo_vision_permitted: bool,
    features_restriction: bool,
}

impl IParentalControlService {
    pub fn new(capability: u32) -> Self {
        Self {
            capability,
            stereo_vision_permitted: true,
            features_restriction: false,
        }
    }

    /// CheckFreeCommunicationPermission (cmd 1001).
    pub fn check_free_communication_permission(&self) -> ResultCode {
        log::debug!("IParentalControlService::check_free_communication_permission (STUBBED) called");
        ResultCode::new(0) // Success
    }

    /// IsStereoVisionPermitted (cmd 1061).
    pub fn is_stereo_vision_permitted(&self) -> bool {
        log::debug!("IParentalControlService::is_stereo_vision_permitted (STUBBED) called");
        self.stereo_vision_permitted
    }

    /// IsRestrictionEnabled (cmd 1006).
    pub fn is_restriction_enabled(&self) -> bool {
        log::debug!("IParentalControlService::is_restriction_enabled (STUBBED) called");
        false
    }
}
