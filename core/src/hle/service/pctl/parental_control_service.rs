// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pctl/parental_control_service.h
//! Port of zuyu/src/core/hle/service/pctl/parental_control_service.cpp
//!
//! IParentalControlService — actual parental control operations.

use crate::hle::result::ResultCode;
use super::pctl_results::*;
use super::pctl_types::{
    ApplicationInfo, Capability, PlayTimerSettings, RestrictionSettings,
};

/// IPC command table for IParentalControlService.
///
/// Corresponds to the function table in upstream parental_control_service.cpp.
pub mod commands {
    pub const INITIALIZE: u32 = 1;
    pub const CHECK_FREE_COMMUNICATION_PERMISSION: u32 = 1001;
    pub const CONFIRM_LAUNCH_APPLICATION_PERMISSION: u32 = 1002;
    pub const CONFIRM_RESUME_APPLICATION_PERMISSION: u32 = 1003;
    pub const CONFIRM_SNS_POST_PERMISSION: u32 = 1004;
    pub const CONFIRM_SYSTEM_SETTINGS_PERMISSION: u32 = 1005;
    pub const IS_RESTRICTION_TEMPORARY_UNLOCKED: u32 = 1006;
    pub const REVERT_RESTRICTION_TEMPORARY_UNLOCKED: u32 = 1007;
    pub const ENTER_RESTRICTED_SYSTEM_SETTINGS: u32 = 1008;
    pub const LEAVE_RESTRICTED_SYSTEM_SETTINGS: u32 = 1009;
    pub const IS_RESTRICTED_SYSTEM_SETTINGS_ENTERED: u32 = 1010;
    pub const REVERT_RESTRICTED_SYSTEM_SETTINGS_ENTERED: u32 = 1011;
    pub const GET_RESTRICTED_FEATURES: u32 = 1012;
    pub const CONFIRM_STEREO_VISION_PERMISSION: u32 = 1013;
    pub const CONFIRM_PLAYABLE_APPLICATION_VIDEO_OLD: u32 = 1014;
    pub const CONFIRM_PLAYABLE_APPLICATION_VIDEO: u32 = 1015;
    pub const CONFIRM_SHOW_NEWS_PERMISSION: u32 = 1016;
    pub const END_FREE_COMMUNICATION: u32 = 1017;
    pub const IS_FREE_COMMUNICATION_AVAILABLE: u32 = 1018;
    pub const IS_RESTRICTION_ENABLED: u32 = 1031;
    pub const GET_SAFETY_LEVEL: u32 = 1032;
    pub const SET_SAFETY_LEVEL: u32 = 1033;
    pub const GET_SAFETY_LEVEL_SETTINGS: u32 = 1034;
    pub const GET_CURRENT_SETTINGS: u32 = 1035;
    pub const SET_CUSTOM_SAFETY_LEVEL_SETTINGS: u32 = 1036;
    pub const GET_DEFAULT_RATING_ORGANIZATION: u32 = 1037;
    pub const SET_DEFAULT_RATING_ORGANIZATION: u32 = 1038;
    pub const GET_FREE_COMMUNICATION_APPLICATION_LIST_COUNT: u32 = 1039;
    pub const ADD_TO_FREE_COMMUNICATION_APPLICATION_LIST: u32 = 1042;
    pub const DELETE_SETTINGS: u32 = 1043;
    pub const GET_FREE_COMMUNICATION_APPLICATION_LIST: u32 = 1044;
    pub const UPDATE_FREE_COMMUNICATION_APPLICATION_LIST: u32 = 1045;
    pub const DISABLE_FEATURES_FOR_RESET: u32 = 1046;
    pub const NOTIFY_APPLICATION_DOWNLOAD_STARTED: u32 = 1047;
    pub const NOTIFY_NETWORK_PROFILE_CREATED: u32 = 1048;
    pub const RESET_FREE_COMMUNICATION_APPLICATION_LIST: u32 = 1049;
    pub const CONFIRM_STEREO_VISION_RESTRICTION_CONFIGURABLE: u32 = 1061;
    pub const GET_STEREO_VISION_RESTRICTION: u32 = 1062;
    pub const SET_STEREO_VISION_RESTRICTION: u32 = 1063;
    pub const RESET_CONFIRMED_STEREO_VISION_PERMISSION: u32 = 1064;
    pub const IS_STEREO_VISION_PERMITTED: u32 = 1065;
    pub const UNLOCK_RESTRICTION_TEMPORARILY: u32 = 1201;
    pub const UNLOCK_SYSTEM_SETTINGS_RESTRICTION: u32 = 1202;
    pub const SET_PIN_CODE: u32 = 1203;
    pub const GENERATE_INQUIRY_CODE: u32 = 1204;
    pub const CHECK_MASTER_KEY: u32 = 1205;
    pub const GET_PIN_CODE_LENGTH: u32 = 1206;
    pub const GET_PIN_CODE_CHANGED_EVENT: u32 = 1207;
    pub const GET_PIN_CODE: u32 = 1208;
    pub const IS_PAIRING_ACTIVE: u32 = 1403;
    pub const GET_SETTINGS_LAST_UPDATED: u32 = 1406;
    pub const GET_PAIRING_ACCOUNT_INFO: u32 = 1411;
    pub const GET_ACCOUNT_NICKNAME: u32 = 1421;
    pub const GET_ACCOUNT_STATE: u32 = 1424;
    pub const REQUEST_POST_EVENTS: u32 = 1425;
    pub const GET_POST_EVENT_INTERVAL: u32 = 1426;
    pub const SET_POST_EVENT_INTERVAL: u32 = 1427;
    pub const GET_SYNCHRONIZATION_EVENT: u32 = 1432;
    pub const START_PLAY_TIMER: u32 = 1451;
    pub const STOP_PLAY_TIMER: u32 = 1452;
    pub const IS_PLAY_TIMER_ENABLED: u32 = 1453;
    pub const GET_PLAY_TIMER_REMAINING_TIME: u32 = 1454;
    pub const IS_RESTRICTED_BY_PLAY_TIMER: u32 = 1455;
    pub const GET_PLAY_TIMER_SETTINGS: u32 = 1456;
    pub const GET_PLAY_TIMER_EVENT_TO_REQUEST_SUSPENSION: u32 = 1457;
    pub const IS_PLAY_TIMER_ALARM_DISABLED: u32 = 1458;
    pub const NOTIFY_WRONG_PIN_CODE_INPUT_MANY_TIMES: u32 = 1471;
    pub const CANCEL_NETWORK_REQUEST: u32 = 1472;
    pub const GET_UNLINKED_EVENT: u32 = 1473;
    pub const CLEAR_UNLINKED_EVENT: u32 = 1474;
    pub const DISABLE_ALL_FEATURES: u32 = 1601;
    pub const POST_ENABLE_ALL_FEATURES: u32 = 1602;
    pub const IS_ALL_FEATURES_DISABLED: u32 = 1603;
    pub const DELETE_FROM_FREE_COMMUNICATION_APPLICATION_LIST_FOR_DEBUG: u32 = 1901;
    pub const CLEAR_FREE_COMMUNICATION_APPLICATION_LIST_FOR_DEBUG: u32 = 1902;
    pub const REQUEST_UPDATE_EXEMPTION_LIST_ASYNC: u32 = 2016;
}

/// Internal states for the parental control service.
///
/// Corresponds to `States` in upstream parental_control_service.h.
#[derive(Debug, Clone, Default)]
struct States {
    current_tid: u64,
    application_info: ApplicationInfo,
    tid_from_event: u64,
    launch_time_valid: bool,
    is_suspended: bool,
    temporary_unlocked: bool,
    free_communication: bool,
    stereo_vision: bool,
}

/// Internal settings for parental control.
///
/// Corresponds to `ParentalControlSettings` in upstream parental_control_service.h.
#[derive(Debug, Clone, Default)]
struct ParentalControlSettings {
    is_stero_vision_restricted: bool,
    is_free_communication_default_on: bool,
    disabled: bool,
}

/// IParentalControlService.
///
/// Corresponds to `IParentalControlService` in upstream parental_control_service.h.
pub struct IParentalControlService {
    states: States,
    settings: ParentalControlSettings,
    restriction_settings: RestrictionSettings,
    pin_code: [u8; 8],
    capability: Capability,
}

impl IParentalControlService {
    pub fn new(capability: Capability) -> Self {
        Self {
            states: States::default(),
            settings: ParentalControlSettings::default(),
            restriction_settings: RestrictionSettings::default(),
            pin_code: [0u8; 8],
            capability,
        }
    }

    /// CheckFreeCommunicationPermissionImpl — internal helper.
    ///
    /// Corresponds to upstream `CheckFreeCommunicationPermissionImpl`.
    fn check_free_communication_permission_impl(&self) -> bool {
        if self.states.temporary_unlocked {
            return true;
        }
        if (self.states.application_info.parental_control_flag & 1) == 0 {
            return true;
        }
        if self.pin_code[0] == 0 {
            return true;
        }
        if !self.settings.is_free_communication_default_on {
            return true;
        }
        // TODO: Check for blacklisted/exempted applications.
        true
    }

    /// ConfirmStereoVisionPermissionImpl — internal helper.
    ///
    /// Corresponds to upstream `ConfirmStereoVisionPermissionImpl`.
    fn confirm_stereo_vision_permission_impl(&self) -> bool {
        if self.states.temporary_unlocked {
            return true;
        }
        if self.pin_code[0] == 0 {
            return true;
        }
        if !self.settings.is_stero_vision_restricted {
            return false;
        }
        true
    }

    /// SetStereoVisionRestrictionImpl — internal helper.
    ///
    /// Corresponds to upstream `SetStereoVisionRestrictionImpl`.
    fn set_stereo_vision_restriction_impl(&mut self, is_restricted: bool) {
        if self.settings.disabled {
            return;
        }
        if self.pin_code[0] == 0 {
            return;
        }
        self.settings.is_stero_vision_restricted = is_restricted;
    }

    /// Initialize (cmd 1).
    ///
    /// Corresponds to upstream `IParentalControlService::Initialize`.
    pub fn initialize(&mut self) -> Result<(), ResultCode> {
        log::debug!("IParentalControlService::Initialize called");

        if !self.capability.intersects(Capability::APPLICATION | Capability::SYSTEM) {
            log::error!(
                "Invalid capability! capability={:#x}",
                self.capability.bits()
            );
            return Err(RESULT_NO_CAPABILITY);
        }

        // TODO: Recovery flag initialization for pctl:r
        // TODO: Get program_id from system, read control metadata, fill application_info
        self.states.tid_from_event = 0;
        self.states.launch_time_valid = false;
        self.states.is_suspended = false;
        self.states.free_communication = false;
        self.states.stereo_vision = false;

        Ok(())
    }

    /// CheckFreeCommunicationPermission (cmd 1001).
    ///
    /// Corresponds to upstream `IParentalControlService::CheckFreeCommunicationPermission`.
    pub fn check_free_communication_permission(&mut self) -> Result<(), ResultCode> {
        log::debug!("IParentalControlService::CheckFreeCommunicationPermission called");

        if !self.check_free_communication_permission_impl() {
            return Err(RESULT_NO_FREE_COMMUNICATION);
        }
        self.states.free_communication = true;
        Ok(())
    }

    /// ConfirmLaunchApplicationPermission (cmd 1002).
    ///
    /// Corresponds to upstream `IParentalControlService::ConfirmLaunchApplicationPermission`.
    pub fn confirm_launch_application_permission(
        &self,
        _restriction_bitset: &[u8],
        nacp_flag: u64,
        application_id: u64,
    ) -> Result<(), ResultCode> {
        log::warn!(
            "(STUBBED) ConfirmLaunchApplicationPermission called, nacp_flag={:#x} application_id={:016x}",
            nacp_flag,
            application_id,
        );
        Ok(())
    }

    /// ConfirmResumeApplicationPermission (cmd 1003).
    ///
    /// Corresponds to upstream `IParentalControlService::ConfirmResumeApplicationPermission`.
    pub fn confirm_resume_application_permission(
        &self,
        _restriction_bitset: &[u8],
        nacp_flag: u64,
        application_id: u64,
    ) -> Result<(), ResultCode> {
        log::warn!(
            "(STUBBED) ConfirmResumeApplicationPermission called, nacp_flag={:#x} application_id={:016x}",
            nacp_flag,
            application_id,
        );
        Ok(())
    }

    /// ConfirmSnsPostPermission (cmd 1004).
    ///
    /// Corresponds to upstream `IParentalControlService::ConfirmSnsPostPermission`.
    pub fn confirm_sns_post_permission(&self) -> Result<(), ResultCode> {
        log::warn!("(STUBBED) ConfirmSnsPostPermission called");
        Err(RESULT_NO_FREE_COMMUNICATION)
    }

    /// IsRestrictionTemporaryUnlocked (cmd 1006).
    ///
    /// Corresponds to upstream `IParentalControlService::IsRestrictionTemporaryUnlocked`.
    pub fn is_restriction_temporary_unlocked(&self) -> Result<bool, ResultCode> {
        log::warn!("(STUBBED) IsRestrictionTemporaryUnlocked called");
        Ok(false)
    }

    /// IsRestrictedSystemSettingsEntered (cmd 1010).
    ///
    /// Corresponds to upstream `IParentalControlService::IsRestrictedSystemSettingsEntered`.
    pub fn is_restricted_system_settings_entered(&self) -> Result<bool, ResultCode> {
        log::warn!("(STUBBED) IsRestrictedSystemSettingsEntered called");
        Ok(false)
    }

    /// ConfirmStereoVisionPermission (cmd 1013).
    ///
    /// Corresponds to upstream `IParentalControlService::ConfirmStereoVisionPermission`.
    pub fn confirm_stereo_vision_permission(&mut self) -> Result<(), ResultCode> {
        log::debug!("IParentalControlService::ConfirmStereoVisionPermission called");
        self.states.stereo_vision = true;
        Ok(())
    }

    /// EndFreeCommunication (cmd 1017).
    ///
    /// Corresponds to upstream `IParentalControlService::EndFreeCommunication`.
    pub fn end_free_communication(&self) -> Result<(), ResultCode> {
        log::warn!("(STUBBED) EndFreeCommunication called");
        Ok(())
    }

    /// IsFreeCommunicationAvailable (cmd 1018).
    ///
    /// Corresponds to upstream `IParentalControlService::IsFreeCommunicationAvailable`.
    pub fn is_free_communication_available(&self) -> Result<(), ResultCode> {
        log::warn!("(STUBBED) IsFreeCommunicationAvailable called");
        if !self.check_free_communication_permission_impl() {
            return Err(RESULT_NO_FREE_COMMUNICATION);
        }
        Ok(())
    }

    /// IsRestrictionEnabled (cmd 1031).
    ///
    /// Corresponds to upstream `IParentalControlService::IsRestrictionEnabled`.
    pub fn is_restriction_enabled(&self) -> Result<bool, ResultCode> {
        log::debug!("IParentalControlService::IsRestrictionEnabled called");

        if !self.capability.intersects(Capability::STATUS | Capability::RECOVERY) {
            log::error!("Application does not have Status or Recovery capabilities!");
            return Err(RESULT_NO_CAPABILITY);
        }

        Ok(self.pin_code[0] != 0)
    }

    /// GetSafetyLevel (cmd 1032).
    ///
    /// Corresponds to upstream `IParentalControlService::GetSafetyLevel`.
    pub fn get_safety_level(&self) -> Result<u32, ResultCode> {
        log::warn!("(STUBBED) GetSafetyLevel called");
        Ok(0)
    }

    /// GetCurrentSettings (cmd 1035).
    ///
    /// Corresponds to upstream `IParentalControlService::GetCurrentSettings`.
    pub fn get_current_settings(&self) -> Result<RestrictionSettings, ResultCode> {
        log::info!("IParentalControlService::GetCurrentSettings called");
        Ok(self.restriction_settings)
    }

    /// GetFreeCommunicationApplicationListCount (cmd 1039).
    ///
    /// Corresponds to upstream `IParentalControlService::GetFreeCommunicationApplicationListCount`.
    pub fn get_free_communication_application_list_count(&self) -> Result<i32, ResultCode> {
        log::warn!("(STUBBED) GetFreeCommunicationApplicationListCount called");
        Ok(4)
    }

    /// ConfirmStereoVisionRestrictionConfigurable (cmd 1061).
    ///
    /// Corresponds to upstream `IParentalControlService::ConfirmStereoVisionRestrictionConfigurable`.
    pub fn confirm_stereo_vision_restriction_configurable(&self) -> Result<(), ResultCode> {
        log::debug!("IParentalControlService::ConfirmStereoVisionRestrictionConfigurable called");

        if !self.capability.contains(Capability::STEREO_VISION) {
            log::error!("Application does not have StereoVision capability!");
            return Err(RESULT_NO_CAPABILITY);
        }

        if self.pin_code[0] == 0 {
            return Err(RESULT_NO_RESTRICTION_ENABLED);
        }

        Ok(())
    }

    /// GetStereoVisionRestriction (cmd 1062).
    ///
    /// Corresponds to upstream `IParentalControlService::GetStereoVisionRestriction`.
    pub fn get_stereo_vision_restriction(&self) -> Result<bool, ResultCode> {
        log::debug!("IParentalControlService::GetStereoVisionRestriction called");

        if !self.capability.contains(Capability::STEREO_VISION) {
            log::error!("Application does not have StereoVision capability!");
            return Err(RESULT_NO_CAPABILITY);
        }

        Ok(self.settings.is_stero_vision_restricted)
    }

    /// SetStereoVisionRestriction (cmd 1063).
    ///
    /// Corresponds to upstream `IParentalControlService::SetStereoVisionRestriction`.
    pub fn set_stereo_vision_restriction(
        &mut self,
        stereo_vision_restriction: bool,
    ) -> Result<(), ResultCode> {
        log::debug!(
            "IParentalControlService::SetStereoVisionRestriction called, can_use={}",
            stereo_vision_restriction,
        );

        if !self.capability.contains(Capability::STEREO_VISION) {
            log::error!("Application does not have StereoVision capability!");
            return Err(RESULT_NO_CAPABILITY);
        }

        self.set_stereo_vision_restriction_impl(stereo_vision_restriction);
        Ok(())
    }

    /// ResetConfirmedStereoVisionPermission (cmd 1064).
    ///
    /// Corresponds to upstream `IParentalControlService::ResetConfirmedStereoVisionPermission`.
    pub fn reset_confirmed_stereo_vision_permission(&mut self) -> Result<(), ResultCode> {
        log::debug!("IParentalControlService::ResetConfirmedStereoVisionPermission called");
        self.states.stereo_vision = false;
        Ok(())
    }

    /// IsStereoVisionPermitted (cmd 1065).
    ///
    /// Corresponds to upstream `IParentalControlService::IsStereoVisionPermitted`.
    pub fn is_stereo_vision_permitted(&self) -> Result<bool, ResultCode> {
        log::debug!("IParentalControlService::IsStereoVisionPermitted called");

        if !self.confirm_stereo_vision_permission_impl() {
            return Err(RESULT_STEREO_VISION_RESTRICTED);
        }
        Ok(true)
    }

    /// GetPinCodeLength (cmd 1206).
    ///
    /// Corresponds to upstream `IParentalControlService::GetPinCodeLength`.
    pub fn get_pin_code_length(&self) -> Result<i32, ResultCode> {
        log::warn!("(STUBBED) GetPinCodeLength called");
        Ok(0)
    }

    /// IsPairingActive (cmd 1403).
    ///
    /// Corresponds to upstream `IParentalControlService::IsPairingActive`.
    pub fn is_pairing_active(&self) -> Result<bool, ResultCode> {
        log::warn!("(STUBBED) IsPairingActive called");
        Ok(false)
    }

    /// StartPlayTimer (cmd 1451).
    ///
    /// Corresponds to upstream `IParentalControlService::StartPlayTimer`.
    pub fn start_play_timer(&self) -> Result<(), ResultCode> {
        log::warn!("(STUBBED) StartPlayTimer called");
        Ok(())
    }

    /// StopPlayTimer (cmd 1452).
    ///
    /// Corresponds to upstream `IParentalControlService::StopPlayTimer`.
    pub fn stop_play_timer(&self) -> Result<(), ResultCode> {
        log::warn!("(STUBBED) StopPlayTimer called");
        Ok(())
    }

    /// IsPlayTimerEnabled (cmd 1453).
    ///
    /// Corresponds to upstream `IParentalControlService::IsPlayTimerEnabled`.
    pub fn is_play_timer_enabled(&self) -> Result<bool, ResultCode> {
        log::warn!("(STUBBED) IsPlayTimerEnabled called");
        Ok(false)
    }

    /// IsRestrictedByPlayTimer (cmd 1455).
    ///
    /// Corresponds to upstream `IParentalControlService::IsRestrictedByPlayTimer`.
    pub fn is_restricted_by_play_timer(&self) -> Result<bool, ResultCode> {
        log::warn!("(STUBBED) IsRestrictedByPlayTimer called");
        Ok(false)
    }

    /// GetPlayTimerSettings (cmd 1456).
    ///
    /// Corresponds to upstream `IParentalControlService::GetPlayTimerSettings`.
    pub fn get_play_timer_settings(&self) -> Result<PlayTimerSettings, ResultCode> {
        log::warn!("(STUBBED) GetPlayTimerSettings called");
        Ok(PlayTimerSettings::default())
    }

    /// IsPlayTimerAlarmDisabled (cmd 1458).
    ///
    /// Corresponds to upstream `IParentalControlService::IsPlayTimerAlarmDisabled`.
    pub fn is_play_timer_alarm_disabled(&self) -> Result<bool, ResultCode> {
        log::info!("IsPlayTimerAlarmDisabled called");
        Ok(false)
    }
}
