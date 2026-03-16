// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/application_functions.h
//! Port of zuyu/src/core/hle/service/am/service/application_functions.cpp

use crate::hle::service::am::am_types::{
    GamePlayRecordingState, ProgramSpecifyKind, WindowOriginMode,
};
use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IApplicationFunctions:
/// - 1: PopLaunchParameter
/// - 10: CreateApplicationAndPushAndRequestToStart (unimplemented)
/// - 11: CreateApplicationAndPushAndRequestToStartForQuest (unimplemented)
/// - 12: CreateApplicationAndRequestToStart (unimplemented)
/// - 13: CreateApplicationAndRequestToStartForQuest (unimplemented)
/// - 14: CreateApplicationWithAttributeAndPushAndRequestToStartForQuest (unimplemented)
/// - 15: CreateApplicationWithAttributeAndRequestToStartForQuest (unimplemented)
/// - 20: EnsureSaveData
/// - 21: GetDesiredLanguage
/// - 22: SetTerminateResult
/// - 23: GetDisplayVersion
/// - 24: GetLaunchStorageInfoForDebug (unimplemented)
/// - 25: ExtendSaveData
/// - 26: GetSaveDataSize
/// - 27: CreateCacheStorage
/// - 28: GetSaveDataSizeMax
/// - 29: GetCacheStorageMax
/// - 30: BeginBlockingHomeButtonShortAndLongPressed
/// - 31: EndBlockingHomeButtonShortAndLongPressed
/// - 32: BeginBlockingHomeButton
/// - 33: EndBlockingHomeButton
/// - 34: SelectApplicationLicense (unimplemented)
/// - 35: GetDeviceSaveDataSizeMax (unimplemented)
/// - 36: GetLimitedApplicationLicense (unimplemented)
/// - 37: GetLimitedApplicationLicenseUpgradableEvent (unimplemented)
/// - 40: NotifyRunning
/// - 50: GetPseudoDeviceId
/// - 60: SetMediaPlaybackStateForApplication (unimplemented)
/// - 65: IsGamePlayRecordingSupported
/// - 66: InitializeGamePlayRecording
/// - 67: SetGamePlayRecordingState
/// - 68: RequestFlushGamePlayingMovieForDebug (unimplemented)
/// - 70: RequestToShutdown (unimplemented)
/// - 71: RequestToReboot (unimplemented)
/// - 72: RequestToSleep (unimplemented)
/// - 80: ExitAndRequestToShowThanksMessage (unimplemented)
/// - 90: EnableApplicationCrashReport
/// - 100: InitializeApplicationCopyrightFrameBuffer
/// - 101: SetApplicationCopyrightImage
/// - 102: SetApplicationCopyrightVisibility
/// - 110: QueryApplicationPlayStatistics (unimplemented)
/// - 111: QueryApplicationPlayStatisticsByUid (unimplemented)
/// - 120: ExecuteProgram (unimplemented)
/// - 121: ClearUserChannel (unimplemented)
/// - 122: UnpopToUserChannel (unimplemented)
/// - 123: GetPreviousProgramIndex
/// - 124: EnableApplicationAllThreadDumpOnCrash (unimplemented)
/// - 130: GetGpuErrorDetectedSystemEvent
/// - 131: SetDelayTimeToAbortOnGpuError (unimplemented)
/// - 140: GetFriendInvitationStorageChannelEvent
/// - 141: TryPopFromFriendInvitationStorageChannel (unimplemented)
/// - 150: GetNotificationStorageChannelEvent (unimplemented)
/// - 151: TryPopFromNotificationStorageChannel (unimplemented)
/// - 160: GetHealthWarningDisappearedSystemEvent
/// - 170: SetHdcpAuthenticationActivated (unimplemented)
/// - 180: GetLaunchRequiredVersion (unimplemented)
/// - 181: UpgradeLaunchRequiredVersion (unimplemented)
/// - 190: SendServerMaintenanceOverlayNotification (unimplemented)
/// - 200: GetLastApplicationExitReason (unimplemented)
/// - 500: StartContinuousRecordingFlushForDebug (unimplemented)
/// - 1000: CreateMovieMaker (unimplemented)
/// - 1001: PrepareForJit
pub struct IApplicationFunctions {
    /// Reference to the applet.
    /// Matches upstream `const std::shared_ptr<Applet> m_applet`.
    applet: std::sync::Arc<std::sync::Mutex<crate::hle::service::am::applet::Applet>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IApplicationFunctions {
    pub fn new(applet: std::sync::Arc<std::sync::Mutex<crate::hle::service::am::applet::Applet>>) -> Self {
        let handlers = build_handler_map(&[
            (1, Some(Self::pop_launch_parameter_handler), "PopLaunchParameter"),
            (20, Some(Self::ensure_save_data_handler), "EnsureSaveData"),
            (21, Some(Self::get_desired_language_handler), "GetDesiredLanguage"),
            (22, Some(Self::set_terminate_result_handler), "SetTerminateResult"),
            (40, Some(Self::notify_running_handler), "NotifyRunning"),
            (50, Some(Self::get_pseudo_device_id_handler), "GetPseudoDeviceId"),
            (65, Some(Self::is_game_play_recording_supported_handler), "IsGamePlayRecordingSupported"),
            (66, Some(Self::initialize_game_play_recording_handler), "InitializeGamePlayRecording"),
            (67, Some(Self::set_game_play_recording_state_handler), "SetGamePlayRecordingState"),
            (90, Some(Self::enable_application_crash_report_handler), "EnableApplicationCrashReport"),
            (100, Some(Self::initialize_application_copyright_frame_buffer_handler), "InitializeApplicationCopyrightFrameBuffer"),
            (123, Some(Self::get_previous_program_index_handler), "GetPreviousProgramIndex"),
            (130, Some(Self::get_gpu_error_detected_system_event_handler), "GetGpuErrorDetectedSystemEvent"),
            (140, Some(Self::get_friend_invitation_storage_channel_event_handler), "GetFriendInvitationStorageChannelEvent"),
            (160, Some(Self::get_health_warning_disappeared_system_event_handler), "GetHealthWarningDisappearedSystemEvent"),
            (1001, Some(Self::prepare_for_jit_handler), "PrepareForJit"),
        ]);
        Self {
            applet,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of IApplicationFunctions::NotifyRunning
    pub fn notify_running(&self) -> bool {
        log::warn!("(STUBBED) NotifyRunning called");
        true
    }

    /// Port of IApplicationFunctions::GetSaveDataSizeMax
    pub fn get_save_data_size_max(&self) -> (u64, u64) {
        log::warn!("(STUBBED) GetSaveDataSizeMax called");
        (0xFFFFFFF, 0xFFFFFFF)
    }

    /// Port of IApplicationFunctions::CreateCacheStorage
    pub fn create_cache_storage(&self, _index: u16, _normal_size: u64, _journal_size: u64) -> (u32, u64) {
        log::warn!("(STUBBED) CreateCacheStorage called");
        (1, 0) // target_media=Nand, required_size=0
    }

    /// Port of IApplicationFunctions::BeginBlockingHomeButtonShortAndLongPressed
    pub fn begin_blocking_home_button_short_and_long_pressed(&self, _unused: i64) {
        log::warn!("(STUBBED) BeginBlockingHomeButtonShortAndLongPressed called");
        // TODO: lock applet, set home_button_long_pressed_blocked/short_pressed_blocked = true
    }

    /// Port of IApplicationFunctions::EndBlockingHomeButtonShortAndLongPressed
    pub fn end_blocking_home_button_short_and_long_pressed(&self) {
        log::warn!("(STUBBED) EndBlockingHomeButtonShortAndLongPressed called");
        // TODO: lock applet, set home_button_long_pressed_blocked/short_pressed_blocked = false
    }

    /// Port of IApplicationFunctions::IsGamePlayRecordingSupported
    pub fn is_game_play_recording_supported(&self) -> bool {
        log::warn!("(STUBBED) IsGamePlayRecordingSupported called");
        false
    }

    /// Port of IApplicationFunctions::SetGamePlayRecordingState
    pub fn set_game_play_recording_state(&self, _state: GamePlayRecordingState) {
        log::warn!("(STUBBED) SetGamePlayRecordingState called");
    }

    /// Port of IApplicationFunctions::EnableApplicationCrashReport
    pub fn enable_application_crash_report(&self, _enabled: bool) {
        log::warn!("(STUBBED) EnableApplicationCrashReport called");
    }

    /// Port of IApplicationFunctions::SetApplicationCopyrightVisibility
    pub fn set_application_copyright_visibility(&self, _visible: bool) {
        log::warn!("(STUBBED) SetApplicationCopyrightVisibility called");
    }

    /// Port of IApplicationFunctions::ExecuteProgram
    pub fn execute_program(&self, _kind: ProgramSpecifyKind, _value: u64) {
        log::warn!("(STUBBED) ExecuteProgram called");
        // TODO: copy user channel, system.ExecuteProgram(value)
    }

    /// Port of IApplicationFunctions::GetPreviousProgramIndex
    pub fn get_previous_program_index(&self) -> i32 {
        log::warn!("(STUBBED) GetPreviousProgramIndex called");
        0
    }

    /// Port of IApplicationFunctions::PrepareForJit
    pub fn prepare_for_jit(&self) {
        log::warn!("(STUBBED) PrepareForJit called");
        // TODO: lock applet, set jit_service_launched = true
    }

    fn notify_running_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationFunctions) };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(service.notify_running());
    }

    fn is_game_play_recording_supported_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationFunctions) };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(service.is_game_play_recording_supported());
    }

    fn enable_application_crash_report_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationFunctions) };
        let mut rp = RequestParser::new(ctx);
        service.enable_application_crash_report(rp.pop_bool());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_previous_program_index_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationFunctions) };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_i32(service.get_previous_program_index());
    }

    fn prepare_for_jit_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationFunctions) };
        service.prepare_for_jit();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// PopLaunchParameter (cmd 1): returns empty launch parameter
    fn pop_launch_parameter_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) PopLaunchParameter called");
        // Upstream returns LaunchParameterData via IStorage.
        // For bring-up, return ResultNoDataInChannel.
        let result = ResultCode::from_module_description(crate::hle::result::ErrorModule::AM, 2);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// EnsureSaveData (cmd 20): ensures save data exists for the given user.
    /// Upstream creates save data via filesystem controller, returns Out<u64> = 0.
    fn ensure_save_data_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) EnsureSaveData called");
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(0); // size = 0
    }

    /// GetDesiredLanguage (cmd 21): returns the desired language code.
    fn get_desired_language_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) GetDesiredLanguage called");
        // "en" language code
        let language_code: u64 = u64::from_le_bytes([b'e', b'n', 0, 0, 0, 0, 0, 0]);
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(language_code);
    }

    /// SetTerminateResult (cmd 22).
    /// Matches upstream: locks applet, stores result.
    fn set_terminate_result_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationFunctions) };
        let mut rp = RequestParser::new(ctx);
        let result = rp.pop_u32();
        log::info!("SetTerminateResult: result={:#x}", result);

        let mut applet = service.applet.lock().unwrap();
        applet.terminate_result = result;

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// GetPseudoDeviceId (cmd 50): returns a pseudo device ID (UUID).
    fn get_pseudo_device_id_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) GetPseudoDeviceId called");
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        // Push 128-bit UUID (all zeros for stub)
        rb.push_u64(0);
        rb.push_u64(0);
    }

    /// InitializeGamePlayRecording (cmd 66)
    fn initialize_game_play_recording_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) InitializeGamePlayRecording called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// SetGamePlayRecordingState (cmd 67).
    /// Matches upstream: locks applet, stores state.
    fn set_game_play_recording_state_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationFunctions) };
        let mut rp = RequestParser::new(ctx);
        let state = rp.pop_u32();
        log::warn!("(STUBBED) SetGamePlayRecordingState: state={}", state);

        let mut applet = service.applet.lock().unwrap();
        applet.game_play_recording_state = match state {
            0 => crate::hle::service::am::am_types::GamePlayRecordingState::Disabled,
            1 => crate::hle::service::am::am_types::GamePlayRecordingState::Enabled,
            _ => crate::hle::service::am::am_types::GamePlayRecordingState::Disabled,
        };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// InitializeApplicationCopyrightFrameBuffer (cmd 100)
    fn initialize_application_copyright_frame_buffer_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) InitializeApplicationCopyrightFrameBuffer called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// GetGpuErrorDetectedSystemEvent (cmd 110): returns an event handle
    fn get_gpu_error_detected_system_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) GetGpuErrorDetectedSystemEvent called");
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationFunctions) };
        let handle = service
            .applet
            .lock()
            .unwrap()
            .ensure_gpu_error_detected_system_event(ctx)
            .unwrap_or(0);
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }

    /// GetFriendInvitationStorageChannelEvent (cmd 120): returns an event handle
    fn get_friend_invitation_storage_channel_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) GetFriendInvitationStorageChannelEvent called");
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationFunctions) };
        let handle = service
            .applet
            .lock()
            .unwrap()
            .ensure_friend_invitation_storage_channel_event(ctx)
            .unwrap_or(0);
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }

    /// GetHealthWarningDisappearedSystemEvent (cmd 160): returns an event handle
    fn get_health_warning_disappeared_system_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) GetHealthWarningDisappearedSystemEvent called");
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationFunctions) };
        let handle = service
            .applet
            .lock()
            .unwrap()
            .ensure_health_warning_disappeared_system_event(ctx)
            .unwrap_or(0);
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }
}

impl SessionRequestHandler for IApplicationFunctions {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IApplicationFunctions {
    fn get_service_name(&self) -> &str {
        "am::IApplicationFunctions"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
