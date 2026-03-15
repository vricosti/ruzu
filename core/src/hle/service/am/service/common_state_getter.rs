// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/common_state_getter.h
//! Port of zuyu/src/core/hle/service/am/service/common_state_getter.cpp

use crate::hle::service::am::am_types::{AppletId, FocusState, OperationMode, SystemButtonType};
use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ICommonStateGetter {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (5, Some(Self::get_operation_mode_handler), "GetOperationMode"),
            (8, Some(Self::get_boot_mode_handler), "GetBootMode"),
            (9, Some(Self::get_current_focus_state_handler), "GetCurrentFocusState"),
            (50, Some(Self::is_vr_mode_enabled_handler), "IsVrModeEnabled"),
            (51, Some(Self::set_vr_mode_enabled_handler), "SetVrModeEnabled"),
            (
                55,
                Some(Self::is_in_controller_firmware_update_section_handler),
                "IsInControllerFirmwareUpdateSection",
            ),
            (
                60,
                Some(Self::get_default_display_resolution_handler),
                "GetDefaultDisplayResolution",
            ),
            (68, Some(Self::get_built_in_display_type_handler), "GetBuiltInDisplayType"),
            (
                80,
                Some(Self::perform_system_button_pressing_if_in_focus_handler),
                "PerformSystemButtonPressingIfInFocus",
            ),
            (
                200,
                Some(Self::get_operation_mode_system_info_handler),
                "GetOperationModeSystemInfo",
            ),
            (
                900,
                Some(Self::set_request_exit_to_library_applet_at_execute_next_program_enabled_handler),
                "SetRequestExitToLibraryAppletAtExecuteNextProgramEnabled",
            ),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
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

    fn get_operation_mode_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ICommonStateGetter) };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u8(service.get_operation_mode() as u8);
    }

    fn get_boot_mode_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u8(0);
    }

    fn get_current_focus_state_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ICommonStateGetter) };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u8(service.get_current_focus_state() as u8);
    }

    fn is_vr_mode_enabled_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ICommonStateGetter) };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(service.is_vr_mode_enabled());
    }

    fn set_vr_mode_enabled_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ICommonStateGetter) };
        let mut rp = RequestParser::new(ctx);
        service.set_vr_mode_enabled(rp.pop_bool());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn is_in_controller_firmware_update_section_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ICommonStateGetter) };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(service.is_in_controller_firmware_update_section());
    }

    fn get_default_display_resolution_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ICommonStateGetter) };
        let (w, h) = service.get_default_display_resolution();
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_i32(w);
        rb.push_i32(h);
    }

    fn get_built_in_display_type_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ICommonStateGetter) };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_i32(service.get_built_in_display_type());
    }

    fn perform_system_button_pressing_if_in_focus_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ICommonStateGetter) };
        let mut rp = RequestParser::new(ctx);
        let button = match rp.pop_i32() {
            1 => SystemButtonType::HomeButtonShortPressing,
            2 => SystemButtonType::HomeButtonLongPressing,
            6 => SystemButtonType::CaptureButtonShortPressing,
            7 => SystemButtonType::CaptureButtonLongPressing,
            _ => SystemButtonType::None,
        };
        service.perform_system_button_pressing_if_in_focus(button);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_operation_mode_system_info_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ICommonStateGetter) };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(service.get_operation_mode_system_info());
    }

    fn set_request_exit_to_library_applet_at_execute_next_program_enabled_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ICommonStateGetter) };
        service.set_request_exit_to_library_applet_at_execute_next_program_enabled();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for ICommonStateGetter {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for ICommonStateGetter {
    fn get_service_name(&self) -> &str {
        "am::ICommonStateGetter"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
