// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/self_controller.h
//! Port of zuyu/src/core/hle/service/am/service/self_controller.cpp

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISelfController {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::exit_handler), "Exit"),
            (1, Some(Self::lock_exit_handler), "LockExit"),
            (2, Some(Self::unlock_exit_handler), "UnlockExit"),
            (9, Some(Self::get_library_applet_launchable_event_handler), "GetLibraryAppletLaunchableEvent"),
            (10, Some(Self::set_screen_shot_permission_handler), "SetScreenShotPermission"),
            (
                11,
                Some(Self::set_operation_mode_changed_notification_handler),
                "SetOperationModeChangedNotification",
            ),
            (
                12,
                Some(Self::set_performance_mode_changed_notification_handler),
                "SetPerformanceModeChangedNotification",
            ),
            (13, Some(Self::set_focus_handling_mode_handler), "SetFocusHandlingMode"),
            (14, Some(Self::set_restart_message_enabled_handler), "SetRestartMessageEnabled"),
            (16, Some(Self::set_out_of_focus_suspending_enabled_handler), "SetOutOfFocusSuspendingEnabled"),
            (40, Some(Self::create_managed_display_layer_handler), "CreateManagedDisplayLayer"),
            (62, Some(Self::set_idle_time_detection_extension_handler), "SetIdleTimeDetectionExtension"),
            (63, Some(Self::get_idle_time_detection_extension_handler), "GetIdleTimeDetectionExtension"),
            (68, Some(Self::set_auto_sleep_disabled_handler), "SetAutoSleepDisabled"),
            (91, Some(Self::get_accumulated_suspended_tick_changed_event_handler), "GetAccumulatedSuspendedTickChangedEvent"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
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

    fn exit_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        service.exit();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn lock_exit_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        service.lock_exit();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn unlock_exit_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        service.unlock_exit();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_operation_mode_changed_notification_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        let mut rp = RequestParser::new(ctx);
        service.set_operation_mode_changed_notification(rp.pop_bool());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_performance_mode_changed_notification_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        let mut rp = RequestParser::new(ctx);
        service.set_performance_mode_changed_notification(rp.pop_bool());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_focus_handling_mode_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        let mut rp = RequestParser::new(ctx);
        service.set_focus_handling_mode(rp.pop_bool(), rp.pop_bool(), rp.pop_bool());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_library_applet_launchable_event_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) GetLibraryAppletLaunchableEvent called");
        let handle = ctx.create_readable_event_handle(true).unwrap_or(0);
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }

    fn set_screen_shot_permission_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) SetScreenShotPermission called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_restart_message_enabled_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) SetRestartMessageEnabled called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_out_of_focus_suspending_enabled_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) SetOutOfFocusSuspendingEnabled called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// CreateManagedDisplayLayer (cmd 40): returns a layer ID.
    /// Matches upstream: display_layer_manager.CreateManagedDisplayLayer(out_layer_id)
    fn create_managed_display_layer_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::info!("ISelfController::CreateManagedDisplayLayer called");
        // Return a dummy layer ID. Upstream creates a real display layer
        // via DisplayLayerManager.
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(1); // layer_id = 1
    }

    fn set_idle_time_detection_extension_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) SetIdleTimeDetectionExtension called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_idle_time_detection_extension_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) GetIdleTimeDetectionExtension called");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0); // IdleTimeDetectionExtension::Disabled = 0
    }

    fn set_auto_sleep_disabled_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) SetAutoSleepDisabled called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_accumulated_suspended_tick_changed_event_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) GetAccumulatedSuspendedTickChangedEvent called");
        let handle = ctx.create_readable_event_handle(false).unwrap_or(0);
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }
}

impl SessionRequestHandler for ISelfController {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for ISelfController {
    fn get_service_name(&self) -> &str {
        "am::ISelfController"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
