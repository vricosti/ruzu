// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/self_controller.h
//! Port of zuyu/src/core/hle/service/am/service/self_controller.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_process::KProcess;
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
    /// Reference to the applet.
    /// Matches upstream `const std::shared_ptr<Applet> m_applet`.
    applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>,
    /// Matches upstream `Kernel::KProcess* m_process`.
    process: Option<Arc<Mutex<KProcess>>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISelfController {
    pub fn new(
        applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>,
        process: Option<Arc<Mutex<KProcess>>>,
    ) -> Self {
        {
            let mut applet_guard = applet.lock().unwrap();
            let applet_id = applet_guard.applet_id;
            let mode = applet_guard.library_applet_mode;
            let process_for_display = process
                .clone()
                .or_else(|| applet_guard.process.get_process());
            if let Some(process_for_display) = process_for_display {
                applet_guard
                    .display_layer_manager
                    .initialize(process_for_display, applet_id, mode);
            }
        }
        let handlers = build_handler_map(&[
            (0, Some(Self::exit_handler), "Exit"),
            (1, Some(Self::lock_exit_handler), "LockExit"),
            (2, Some(Self::unlock_exit_handler), "UnlockExit"),
            (
                9,
                Some(Self::get_library_applet_launchable_event_handler),
                "GetLibraryAppletLaunchableEvent",
            ),
            (
                10,
                Some(Self::set_screen_shot_permission_handler),
                "SetScreenShotPermission",
            ),
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
            (
                13,
                Some(Self::set_focus_handling_mode_handler),
                "SetFocusHandlingMode",
            ),
            (
                14,
                Some(Self::set_restart_message_enabled_handler),
                "SetRestartMessageEnabled",
            ),
            (
                16,
                Some(Self::set_out_of_focus_suspending_enabled_handler),
                "SetOutOfFocusSuspendingEnabled",
            ),
            (
                40,
                Some(Self::create_managed_display_layer_handler),
                "CreateManagedDisplayLayer",
            ),
            (
                62,
                Some(Self::set_idle_time_detection_extension_handler),
                "SetIdleTimeDetectionExtension",
            ),
            (
                63,
                Some(Self::get_idle_time_detection_extension_handler),
                "GetIdleTimeDetectionExtension",
            ),
            (
                68,
                Some(Self::set_auto_sleep_disabled_handler),
                "SetAutoSleepDisabled",
            ),
            (
                91,
                Some(Self::get_accumulated_suspended_tick_changed_event_handler),
                "GetAccumulatedSuspendedTickChangedEvent",
            ),
        ]);
        Self {
            applet,
            process,
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
        log::info!(
            "SetOperationModeChangedNotification called, enabled={}",
            enabled
        );
        let mut applet = self.applet.lock().unwrap();
        applet
            .lifecycle_manager
            .set_operation_mode_changed_notification_enabled(enabled);
    }

    /// Port of ISelfController::SetPerformanceModeChangedNotification
    pub fn set_performance_mode_changed_notification(&self, enabled: bool) {
        log::info!(
            "SetPerformanceModeChangedNotification called, enabled={}",
            enabled
        );
        let mut applet = self.applet.lock().unwrap();
        applet
            .lifecycle_manager
            .set_performance_mode_changed_notification_enabled(enabled);
    }

    /// Port of ISelfController::SetFocusHandlingMode
    pub fn set_focus_handling_mode(&self, notify: bool, background: bool, suspend: bool) {
        log::info!(
            "SetFocusHandlingMode called, notify={} background={} suspend={}",
            notify,
            background,
            suspend
        );
        let mut applet = self.applet.lock().unwrap();
        applet
            .lifecycle_manager
            .set_focus_state_changed_notification_enabled(notify);
        applet.lifecycle_manager.set_focus_handling_mode(suspend);
        applet.update_suspension_state_locked(true);
    }

    /// Port of ISelfController::SetOutOfFocusSuspendingEnabled
    pub fn set_out_of_focus_suspending_enabled(&self, enabled: bool) {
        log::info!("SetOutOfFocusSuspendingEnabled called, enabled={}", enabled);
        let mut applet = self.applet.lock().unwrap();
        applet
            .lifecycle_manager
            .set_out_of_focus_suspending_enabled(enabled);
        applet.update_suspension_state_locked(false);
    }

    /// Port of ISelfController::SetHandlesRequestToDisplay
    pub fn set_handles_request_to_display(&self, enable: bool) {
        log::warn!(
            "(STUBBED) SetHandlesRequestToDisplay called, enable={}",
            enable
        );
    }

    /// Port of ISelfController::ApproveToDisplay
    pub fn approve_to_display(&self) {
        log::warn!("(STUBBED) ApproveToDisplay called");
    }

    /// Port of ISelfController::SetAutoSleepDisabled
    pub fn set_auto_sleep_disabled(&self, is_auto_sleep_disabled: bool) {
        log::debug!(
            "SetAutoSleepDisabled called, is_auto_sleep_disabled={}",
            is_auto_sleep_disabled
        );
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
        let enabled = rp.pop_bool();
        service.set_operation_mode_changed_notification(enabled);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_performance_mode_changed_notification_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        let mut rp = RequestParser::new(ctx);
        let enabled = rp.pop_bool();
        service.set_performance_mode_changed_notification(enabled);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// SetFocusHandlingMode (cmd 13).
    /// Matches upstream: locks applet, sets notification enabled + handling mode,
    /// calls UpdateSuspensionStateLocked.
    fn set_focus_handling_mode_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        let mut rp = RequestParser::new(ctx);
        // CMIF packs consecutive bools as bytes in one u32 word.
        // Upstream D<> macro reads them byte-level via CmifReplyWrap.
        let packed = rp.pop_u32();
        let notify = (packed & 0xFF) != 0;
        let _background = ((packed >> 8) & 0xFF) != 0;
        let suspend = ((packed >> 16) & 0xFF) != 0;
        log::info!(
            "SetFocusHandlingMode: notify={} background={} suspend={}",
            notify,
            _background,
            suspend
        );

        service.set_focus_handling_mode(notify, _background, suspend);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// GetLibraryAppletLaunchableEvent (cmd 9).
    /// Matches upstream: signals event, returns handle.
    fn get_library_applet_launchable_event_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        log::info!("GetLibraryAppletLaunchableEvent called");
        let handle = {
            let mut applet = service.applet.lock().unwrap();
            applet
                .ensure_library_applet_launchable_event(ctx)
                .unwrap_or(0)
        };
        if let Some(thread) = ctx.get_thread() {
            if let Some(process) = thread
                .lock()
                .unwrap()
                .parent
                .as_ref()
                .and_then(|parent| parent.upgrade())
            {
                let mut process = process.lock().unwrap();
                service
                    .applet
                    .lock()
                    .unwrap()
                    .signal_library_applet_launchable_event(&mut process);
            }
        }
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }

    /// SetScreenShotPermission (cmd 10).
    /// Matches upstream: locks applet, stores permission.
    fn set_screen_shot_permission_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        let mut rp = RequestParser::new(ctx);
        let permission = rp.pop_u32();
        log::debug!("SetScreenShotPermission: permission={}", permission);

        let mut applet = service.applet.lock().unwrap();
        applet.screenshot_permission = match permission {
            0 => crate::hle::service::am::am_types::ScreenshotPermission::Inherit,
            1 => crate::hle::service::am::am_types::ScreenshotPermission::Enable,
            2 => crate::hle::service::am::am_types::ScreenshotPermission::Disable,
            _ => crate::hle::service::am::am_types::ScreenshotPermission::Inherit,
        };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// SetRestartMessageEnabled (cmd 14).
    /// Matches upstream: locks applet, sets resume notification.
    fn set_restart_message_enabled_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        let mut rp = RequestParser::new(ctx);
        let enabled = rp.pop_bool();
        log::info!("SetRestartMessageEnabled: enabled={}", enabled);

        let mut applet = service.applet.lock().unwrap();
        applet
            .lifecycle_manager
            .set_resume_notification_enabled(enabled);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// SetOutOfFocusSuspendingEnabled (cmd 16).
    /// Matches upstream: locks applet, sets out-of-focus suspending.
    fn set_out_of_focus_suspending_enabled_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        let mut rp = RequestParser::new(ctx);
        let enabled = rp.pop_bool();
        log::info!("SetOutOfFocusSuspendingEnabled: enabled={}", enabled);

        service.set_out_of_focus_suspending_enabled(enabled);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// CreateManagedDisplayLayer (cmd 40).
    /// Upstream calls `m_applet->display_layer_manager.CreateManagedDisplayLayer(&layer_id)`.
    /// DisplayLayerManager is not yet implemented; returns a dummy layer_id=1 to allow
    /// applets to proceed past initialization. This is sufficient for games that do not
    /// depend on actual display layer management.
    fn create_managed_display_layer_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        let result = service
            .applet
            .lock()
            .unwrap()
            .display_layer_manager
            .create_managed_display_layer();

        match result {
            Ok(layer_id) => {
                log::info!("CreateManagedDisplayLayer: layer_id={}", layer_id);
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u64(layer_id);
            }
            Err(err) => {
                log::error!("CreateManagedDisplayLayer failed: {:?}", err);
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(err);
            }
        }
    }

    /// SetIdleTimeDetectionExtension (cmd 62).
    /// Matches upstream: locks applet, stores value.
    fn set_idle_time_detection_extension_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        let mut rp = RequestParser::new(ctx);
        let extension = rp.pop_u32();
        log::debug!("SetIdleTimeDetectionExtension: extension={}", extension);

        let mut applet = service.applet.lock().unwrap();
        applet.idle_time_detection_extension = match extension {
            0 => crate::hle::service::am::am_types::IdleTimeDetectionExtension::Disabled,
            1 => crate::hle::service::am::am_types::IdleTimeDetectionExtension::Extended,
            2 => crate::hle::service::am::am_types::IdleTimeDetectionExtension::ExtendedUnsafe,
            _ => crate::hle::service::am::am_types::IdleTimeDetectionExtension::Disabled,
        };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// GetIdleTimeDetectionExtension (cmd 63).
    /// Matches upstream: locks applet, reads value.
    fn get_idle_time_detection_extension_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        let applet = service.applet.lock().unwrap();
        let extension = applet.idle_time_detection_extension as u32;
        log::debug!("GetIdleTimeDetectionExtension: extension={}", extension);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(extension);
    }

    /// SetAutoSleepDisabled (cmd 68).
    /// Matches upstream: locks applet, stores bool.
    fn set_auto_sleep_disabled_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISelfController) };
        let mut rp = RequestParser::new(ctx);
        let disabled = rp.pop_bool();
        log::debug!("SetAutoSleepDisabled: disabled={}", disabled);

        let mut applet = service.applet.lock().unwrap();
        applet.auto_sleep_disabled = disabled;

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// GetAccumulatedSuspendedTickChangedEvent (cmd 91).
    /// Matches upstream: returns event handle from applet.
    fn get_accumulated_suspended_tick_changed_event_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("GetAccumulatedSuspendedTickChangedEvent called");
        let handle = ctx.create_readable_event_handle(false).unwrap_or(0);
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }
}

impl Drop for ISelfController {
    fn drop(&mut self) {
        self.applet.lock().unwrap().display_layer_manager.finalize();
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
