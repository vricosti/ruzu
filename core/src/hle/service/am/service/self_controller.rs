// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/self_controller.h
//! Port of zuyu/src/core/hle/service/am/service/self_controller.cpp

/// IPC command table for ISelfController:
/// - 0: Exit
/// - 1: LockExit
/// - 2: UnlockExit
/// - 3: EnterFatalSection
/// - 4: LeaveFatalSection
/// - 9: GetLibraryAppletLaunchableEvent
/// - 10: SetScreenShotPermission
/// - 11: SetOperationModeChangedNotification
/// - 12: SetPerformanceModeChangedNotification
/// - 13: SetFocusHandlingMode
/// - 14: SetRestartMessageEnabled
/// - 15: SetScreenShotAppletIdentityInfo
/// - 16: SetOutOfFocusSuspendingEnabled
/// - 19: SetAlbumImageOrientation
/// - 40: CreateManagedDisplayLayer
/// - 41: IsSystemBufferSharingEnabled
/// - 42: GetSystemSharedLayerHandle
/// - 43: GetSystemSharedBufferHandle
/// - 44: CreateManagedDisplaySeparableLayer
/// - 50: SetHandlesRequestToDisplay
/// - 51: ApproveToDisplay
/// - 60: OverrideAutoSleepTimeAndDimmingTime
/// - 61: SetMediaPlaybackState
/// - 62: SetIdleTimeDetectionExtension
/// - 63: GetIdleTimeDetectionExtension
/// - 65: ReportUserIsActive
/// - 68: SetAutoSleepDisabled
/// - 69: IsAutoSleepDisabled
/// - 72: SetInputDetectionPolicy
/// - 90: GetAccumulatedSuspendedTickValue
/// - 91: GetAccumulatedSuspendedTickChangedEvent
/// - 100: SetAlbumImageTakenNotificationEnabled
/// - 120: SaveCurrentScreenshot
/// - 130: SetRecordVolumeMuted
pub struct ISelfController {
    // TODO: KProcess reference, Applet reference
}

impl ISelfController {
    pub fn new() -> Self {
        Self {}
    }

    /// Port of ISelfController::Exit
    pub fn exit(&self) {
        log::debug!("Exit called");
    }

    /// Port of ISelfController::LockExit
    pub fn lock_exit(&self) {
        log::debug!("LockExit called");
    }

    /// Port of ISelfController::UnlockExit
    pub fn unlock_exit(&self) {
        log::debug!("UnlockExit called");
    }

    /// Port of ISelfController::SetOperationModeChangedNotification
    pub fn set_operation_mode_changed_notification(&self, enabled: bool) {
        log::info!("SetOperationModeChangedNotification called, enabled={}", enabled);
    }

    /// Port of ISelfController::SetPerformanceModeChangedNotification
    pub fn set_performance_mode_changed_notification(&self, enabled: bool) {
        log::info!("SetPerformanceModeChangedNotification called, enabled={}", enabled);
    }

    /// Port of ISelfController::SetFocusHandlingMode
    pub fn set_focus_handling_mode(&self, notify: bool, background: bool, suspend: bool) {
        log::info!(
            "SetFocusHandlingMode called, notify={} background={} suspend={}",
            notify, background, suspend
        );
    }

    /// Port of ISelfController::SetOutOfFocusSuspendingEnabled
    pub fn set_out_of_focus_suspending_enabled(&self, enabled: bool) {
        log::info!("SetOutOfFocusSuspendingEnabled called, enabled={}", enabled);
    }

    /// Port of ISelfController::SetHandlesRequestToDisplay
    pub fn set_handles_request_to_display(&self, enable: bool) {
        log::warn!("(STUBBED) SetHandlesRequestToDisplay called, enable={}", enable);
    }

    /// Port of ISelfController::ApproveToDisplay
    pub fn approve_to_display(&self) {
        log::warn!("(STUBBED) ApproveToDisplay called");
    }

    /// Port of ISelfController::SetAutoSleepDisabled
    pub fn set_auto_sleep_disabled(&self, is_auto_sleep_disabled: bool) {
        log::debug!("SetAutoSleepDisabled called, is_auto_sleep_disabled={}", is_auto_sleep_disabled);
    }

    /// Port of ISelfController::IsAutoSleepDisabled
    pub fn is_auto_sleep_disabled(&self) -> bool {
        log::debug!("IsAutoSleepDisabled called");
        false
    }

    /// Port of ISelfController::ReportUserIsActive
    pub fn report_user_is_active(&self) {
        log::warn!("(STUBBED) ReportUserIsActive called");
    }
}
