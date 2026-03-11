// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/common_state_getter.h
//! Port of zuyu/src/core/hle/service/am/service/common_state_getter.cpp

use crate::hle::service::am::am_types::{AppletId, FocusState, OperationMode, SystemButtonType};

/// IPC command table for ICommonStateGetter:
/// - 0: GetEventHandle
/// - 1: ReceiveMessage
/// - 2: GetThisAppletKind (unimplemented)
/// - 3: AllowToEnterSleep (unimplemented)
/// - 4: DisallowToEnterSleep (unimplemented)
/// - 5: GetOperationMode
/// - 6: GetPerformanceMode
/// - 7: GetCradleStatus (unimplemented)
/// - 8: GetBootMode
/// - 9: GetCurrentFocusState
/// - 10: RequestToAcquireSleepLock
/// - 11: ReleaseSleepLock (unimplemented)
/// - 12: ReleaseSleepLockTransiently (unimplemented)
/// - 13: GetAcquiredSleepLockEvent
/// - 14: GetWakeupCount (unimplemented)
/// - 20: PushToGeneralChannel (unimplemented)
/// - 30: GetHomeButtonReaderLockAccessor (unimplemented)
/// - 31: GetReaderLockAccessorEx
/// - 32: GetWriterLockAccessorEx
/// - 40: GetCradleFwVersion (unimplemented)
/// - 50: IsVrModeEnabled
/// - 51: SetVrModeEnabled
/// - 52: SetLcdBacklighOffEnabled
/// - 53: BeginVrModeEx
/// - 54: EndVrModeEx
/// - 55: IsInControllerFirmwareUpdateSection
/// - 59: SetVrPositionForDebug (unimplemented)
/// - 60: GetDefaultDisplayResolution
/// - 61: GetDefaultDisplayResolutionChangeEvent
/// - 62: GetHdcpAuthenticationState (unimplemented)
/// - 63: GetHdcpAuthenticationStateChangeEvent (unimplemented)
/// - 64: SetTvPowerStateMatchingMode (unimplemented)
/// - 65: GetApplicationIdByContentActionName (unimplemented)
/// - 66: SetCpuBoostMode
/// - 67: CancelCpuBoostMode (unimplemented)
/// - 68: GetBuiltInDisplayType
/// - 80: PerformSystemButtonPressingIfInFocus
/// - 90: SetPerformanceConfigurationChangedNotification (unimplemented)
/// - 91: GetCurrentPerformanceConfiguration (unimplemented)
/// - 100: SetHandlingHomeButtonShortPressedEnabled (unimplemented)
/// - 110: OpenMyGpuErrorHandler (unimplemented)
/// - 120: GetAppletLaunchedHistory
/// - 200: GetOperationModeSystemInfo
/// - 300: GetSettingsPlatformRegion
/// - 400: ActivateMigrationService (unimplemented)
/// - 401: DeactivateMigrationService (unimplemented)
/// - 500: DisableSleepTillShutdown (unimplemented)
/// - 501: SuppressDisablingSleepTemporarily (unimplemented)
/// - 502: IsSleepEnabled (unimplemented)
/// - 503: IsDisablingSleepSuppressed (unimplemented)
/// - 900: SetRequestExitToLibraryAppletAtExecuteNextProgramEnabled
pub struct ICommonStateGetter {
    // TODO: Applet reference
}

impl ICommonStateGetter {
    pub fn new() -> Self {
        Self {}
    }

    /// Port of ICommonStateGetter::GetCurrentFocusState
    pub fn get_current_focus_state(&self) -> FocusState {
        log::debug!("GetCurrentFocusState called");
        // TODO: lock applet, return lifecycle_manager.GetAndClearFocusState()
        FocusState::InFocus
    }

    /// Port of ICommonStateGetter::GetOperationMode
    pub fn get_operation_mode(&self) -> OperationMode {
        log::debug!("GetOperationMode called");
        // TODO: Settings::IsDockedMode()
        OperationMode::Handheld
    }

    /// Port of ICommonStateGetter::IsVrModeEnabled
    pub fn is_vr_mode_enabled(&self) -> bool {
        log::debug!("IsVrModeEnabled called");
        false
    }

    /// Port of ICommonStateGetter::SetVrModeEnabled
    pub fn set_vr_mode_enabled(&self, _enabled: bool) {
        log::warn!("(STUBBED) SetVrModeEnabled called");
    }

    /// Port of ICommonStateGetter::IsInControllerFirmwareUpdateSection
    pub fn is_in_controller_firmware_update_section(&self) -> bool {
        log::info!("IsInControllerFirmwareUpdateSection called");
        false
    }

    /// Port of ICommonStateGetter::GetDefaultDisplayResolution
    pub fn get_default_display_resolution(&self) -> (i32, i32) {
        log::debug!("GetDefaultDisplayResolution called");
        // TODO: check Settings::IsDockedMode()
        // Undocked: 1280x720, Docked: 1920x1080
        (1280, 720)
    }

    /// Port of ICommonStateGetter::GetBuiltInDisplayType
    pub fn get_built_in_display_type(&self) -> i32 {
        log::warn!("(STUBBED) GetBuiltInDisplayType called");
        0
    }

    /// Port of ICommonStateGetter::PerformSystemButtonPressingIfInFocus
    pub fn perform_system_button_pressing_if_in_focus(&self, _button_type: SystemButtonType) {
        log::warn!("(STUBBED) PerformSystemButtonPressingIfInFocus called");
    }

    /// Port of ICommonStateGetter::GetOperationModeSystemInfo
    pub fn get_operation_mode_system_info(&self) -> u32 {
        log::warn!("(STUBBED) GetOperationModeSystemInfo called");
        0
    }

    /// Port of ICommonStateGetter::SetRequestExitToLibraryAppletAtExecuteNextProgramEnabled
    pub fn set_request_exit_to_library_applet_at_execute_next_program_enabled(&self) {
        log::warn!("(STUBBED) SetRequestExitToLibraryAppletAtExecuteNextProgramEnabled called");
    }
}
