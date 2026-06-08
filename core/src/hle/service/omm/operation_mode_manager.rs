// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/omm/operation_mode_manager.h
//! Port of zuyu/src/core/hle/service/omm/operation_mode_manager.cpp

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

pub mod commands {
    pub const GET_OPERATION_MODE: u32 = 0;
    pub const GET_OPERATION_MODE_CHANGE_EVENT: u32 = 1;
    pub const ENABLE_AUDIO_VISUAL: u32 = 2;
    pub const DISABLE_AUDIO_VISUAL: u32 = 3;
    pub const ENTER_SLEEP_AND_WAIT: u32 = 4;
    pub const GET_CRADLE_STATUS: u32 = 5;
    pub const FADE_IN_DISPLAY: u32 = 6;
    pub const FADE_OUT_DISPLAY: u32 = 7;
    pub const GET_CRADLE_FW_VERSION: u32 = 8;
    pub const NOTIFY_CEC_SETTINGS_CHANGED: u32 = 9;
    pub const SET_OPERATION_MODE_POLICY: u32 = 10;
    pub const GET_DEFAULT_DISPLAY_RESOLUTION: u32 = 11;
    pub const GET_DEFAULT_DISPLAY_RESOLUTION_CHANGE_EVENT: u32 = 12;
    pub const UPDATE_DEFAULT_DISPLAY_RESOLUTION: u32 = 13;
    pub const SHOULD_SLEEP_ON_BOOT: u32 = 14;
    pub const NOTIFY_HDCP_APPLICATION_EXECUTION_STARTED: u32 = 15;
    pub const NOTIFY_HDCP_APPLICATION_EXECUTION_FINISHED: u32 = 16;
    pub const NOTIFY_HDCP_APPLICATION_DRAWING_STARTED: u32 = 17;
    pub const NOTIFY_HDCP_APPLICATION_DRAWING_FINISHED: u32 = 18;
    pub const GET_HDCP_AUTHENTICATION_FAILED_EVENT: u32 = 19;
    pub const GET_HDCP_AUTHENTICATION_FAILED_EMULATION_ENABLED: u32 = 20;
    pub const SET_HDCP_AUTHENTICATION_FAILED_EMULATION: u32 = 21;
    pub const GET_HDCP_STATE_CHANGE_EVENT: u32 = 22;
    pub const GET_HDCP_STATE: u32 = 23;
    pub const SHOW_CARD_UPDATE_PROCESSING: u32 = 24;
    pub const SET_APPLICATION_CEC_SETTINGS_AND_NOTIFY_CHANGED: u32 = 25;
    pub const GET_OPERATION_MODE_SYSTEM_INFO: u32 = 26;
    pub const GET_APPLET_FULL_AWAKING_SYSTEM_EVENT: u32 = 27;
    pub const CREATE_CRADLE_FIRMWARE_UPDATER: u32 = 28;
}

/// IOperationModeManager service ("omm").
///
/// All commands are `nullptr` in upstream.
pub struct IOperationModeManager {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IOperationModeManager {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (commands::GET_OPERATION_MODE, None, "GetOperationMode"),
                (
                    commands::GET_OPERATION_MODE_CHANGE_EVENT,
                    None,
                    "GetOperationModeChangeEvent",
                ),
                (commands::ENABLE_AUDIO_VISUAL, None, "EnableAudioVisual"),
                (commands::DISABLE_AUDIO_VISUAL, None, "DisableAudioVisual"),
                (commands::ENTER_SLEEP_AND_WAIT, None, "EnterSleepAndWait"),
                (commands::GET_CRADLE_STATUS, None, "GetCradleStatus"),
                (commands::FADE_IN_DISPLAY, None, "FadeInDisplay"),
                (commands::FADE_OUT_DISPLAY, None, "FadeOutDisplay"),
                (commands::GET_CRADLE_FW_VERSION, None, "GetCradleFwVersion"),
                (
                    commands::NOTIFY_CEC_SETTINGS_CHANGED,
                    None,
                    "NotifyCecSettingsChanged",
                ),
                (
                    commands::SET_OPERATION_MODE_POLICY,
                    None,
                    "SetOperationModePolicy",
                ),
                (
                    commands::GET_DEFAULT_DISPLAY_RESOLUTION,
                    None,
                    "GetDefaultDisplayResolution",
                ),
                (
                    commands::GET_DEFAULT_DISPLAY_RESOLUTION_CHANGE_EVENT,
                    None,
                    "GetDefaultDisplayResolutionChangeEvent",
                ),
                (
                    commands::UPDATE_DEFAULT_DISPLAY_RESOLUTION,
                    None,
                    "UpdateDefaultDisplayResolution",
                ),
                (commands::SHOULD_SLEEP_ON_BOOT, None, "ShouldSleepOnBoot"),
                (
                    commands::NOTIFY_HDCP_APPLICATION_EXECUTION_STARTED,
                    None,
                    "NotifyHdcpApplicationExecutionStarted",
                ),
                (
                    commands::NOTIFY_HDCP_APPLICATION_EXECUTION_FINISHED,
                    None,
                    "NotifyHdcpApplicationExecutionFinished",
                ),
                (
                    commands::NOTIFY_HDCP_APPLICATION_DRAWING_STARTED,
                    None,
                    "NotifyHdcpApplicationDrawingStarted",
                ),
                (
                    commands::NOTIFY_HDCP_APPLICATION_DRAWING_FINISHED,
                    None,
                    "NotifyHdcpApplicationDrawingFinished",
                ),
                (
                    commands::GET_HDCP_AUTHENTICATION_FAILED_EVENT,
                    None,
                    "GetHdcpAuthenticationFailedEvent",
                ),
                (
                    commands::GET_HDCP_AUTHENTICATION_FAILED_EMULATION_ENABLED,
                    None,
                    "GetHdcpAuthenticationFailedEmulationEnabled",
                ),
                (
                    commands::SET_HDCP_AUTHENTICATION_FAILED_EMULATION,
                    None,
                    "SetHdcpAuthenticationFailedEmulation",
                ),
                (
                    commands::GET_HDCP_STATE_CHANGE_EVENT,
                    None,
                    "GetHdcpStateChangeEvent",
                ),
                (commands::GET_HDCP_STATE, None, "GetHdcpState"),
                (
                    commands::SHOW_CARD_UPDATE_PROCESSING,
                    None,
                    "ShowCardUpdateProcessing",
                ),
                (
                    commands::SET_APPLICATION_CEC_SETTINGS_AND_NOTIFY_CHANGED,
                    None,
                    "SetApplicationCecSettingsAndNotifyChanged",
                ),
                (
                    commands::GET_OPERATION_MODE_SYSTEM_INFO,
                    None,
                    "GetOperationModeSystemInfo",
                ),
                (
                    commands::GET_APPLET_FULL_AWAKING_SYSTEM_EVENT,
                    None,
                    "GetAppletFullAwakingSystemEvent",
                ),
                (
                    commands::CREATE_CRADLE_FIRMWARE_UPDATER,
                    None,
                    "CreateCradleFirmwareUpdater",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IOperationModeManager {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "omm"
    }
}

impl ServiceFramework for IOperationModeManager {
    fn get_service_name(&self) -> &str {
        "omm"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operation_mode_manager_table_matches_upstream_command_count() {
        assert_eq!(IOperationModeManager::new().handlers.len(), 29);
    }
}
