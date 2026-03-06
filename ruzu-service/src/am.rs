// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/am/ (applet manager services)
//! Status: EN COURS
//! Derniere synchro: 2026-03-05
//!
//! Service hierarchy:
//!   appletOE -> IApplicationProxy -> ICommonStateGetter
//!                                 -> ISelfController
//!                                 -> IWindowController
//!                                 -> IAudioController
//!                                 -> IDisplayController
//!                                 -> ILibraryAppletCreator
//!                                 -> IApplicationFunctions
//!                                 -> IProcessWindingController
//!                                 -> IDebugFunctions

use std::collections::VecDeque;

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};
use common::ResultCode;

// ── AM result codes (from am_results.h) ─────────────────────────────────────

/// No messages available in the message queue.
const AM_NO_MESSAGES: ResultCode = ResultCode(0x680);

/// No data in channel (used by PopLaunchParameter, TryPopFrom*).
const AM_NO_DATA_IN_CHANNEL: ResultCode = ResultCode(0x680);

/// Fatal section count imbalance.
const AM_FATAL_SECTION_COUNT_IMBALANCE: ResultCode = ResultCode(0xA80);

// ── AM types (from am_types.h) ──────────────────────────────────────────────

/// nn::am::AppletMessage
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AppletMessage {
    None = 0,
    ChangeIntoForeground = 1,
    ChangeIntoBackground = 2,
    Exit = 4,
    ApplicationExited = 6,
    FocusStateChanged = 15,
    Resume = 16,
    DetectShortPressingHomeButton = 20,
    DetectLongPressingHomeButton = 21,
    DetectShortPressingPowerButton = 22,
    DetectMiddlePressingPowerButton = 23,
    DetectLongPressingPowerButton = 24,
    RequestToPrepareSleep = 25,
    FinishedSleepSequence = 26,
    SleepRequiredByHighTemperature = 27,
    SleepRequiredByLowBattery = 28,
    AutoPowerDown = 29,
    OperationModeChanged = 30,
    PerformanceModeChanged = 31,
    DetectReceivingCecSystemStandby = 32,
    SdCardRemoved = 33,
    LaunchApplicationRequested = 50,
    RequestToDisplay = 51,
    ShowApplicationLogo = 55,
    HideApplicationLogo = 56,
    ForceHideApplicationLogo = 57,
    FloatingApplicationDetected = 60,
    DetectShortPressingCaptureButton = 90,
    AlbumScreenShotTaken = 92,
    AlbumRecordingSaved = 93,
}

/// nn::oe::FocusState
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FocusState {
    InFocus = 1,
    NotInFocus = 2,
    Background = 3,
}

/// nn::oe::OperationMode
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperationMode {
    Handheld = 0,
    Docked = 1,
}

/// nn::am::service::ScreenshotPermission
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScreenshotPermission {
    Inherit = 0,
    Enable = 1,
    Disable = 2,
}

/// nn::am::service::IdleTimeDetectionExtension
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdleTimeDetectionExtension {
    Disabled = 0,
    Extended = 1,
    ExtendedUnsafe = 2,
}

/// nn::am::service::GamePlayRecordingState
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GamePlayRecordingState {
    Disabled = 0,
    Enabled = 1,
}

// ── Root interface: appletOE ─────────────────────────────────────────────────

pub struct AmService;

impl AmService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for AmService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for AmService {
    fn service_name(&self) -> &str {
        "appletOE"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("appletOE: cmd_id={}", cmd_id);
        match cmd_id {
            0 => {
                log::info!("appletOE: OpenApplicationProxy");
                IpcResponse::success().with_move_handle(0)
            }
            _ => {
                log::warn!("appletOE: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── Alternative entry: appletAE ──────────────────────────────────────────────

pub struct AppletAeService;

impl AppletAeService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for AppletAeService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for AppletAeService {
    fn service_name(&self) -> &str {
        "appletAE"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("appletAE: cmd_id={}", cmd_id);
        match cmd_id {
            // OpenSystemAppletProxy / OpenLibraryAppletProxy /
            // OpenOverlayAppletProxy / OpenApplicationProxy
            0 | 100 | 200 | 300 => {
                log::info!("appletAE: OpenProxy (cmd_id={})", cmd_id);
                IpcResponse::success().with_move_handle(0)
            }
            _ => {
                log::warn!("appletAE: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── IApplicationProxy ────────────────────────────────────────────────────────

pub struct ApplicationProxyService;

impl ApplicationProxyService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ApplicationProxyService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for ApplicationProxyService {
    fn service_name(&self) -> &str {
        "IApplicationProxy"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("IApplicationProxy: cmd_id={}", cmd_id);
        match cmd_id {
            0 => {
                log::info!("IApplicationProxy: GetCommonStateGetter");
                IpcResponse::success().with_move_handle(0)
            }
            1 => {
                log::info!("IApplicationProxy: GetSelfController");
                IpcResponse::success().with_move_handle(0)
            }
            2 => {
                log::info!("IApplicationProxy: GetWindowController");
                IpcResponse::success().with_move_handle(0)
            }
            3 => {
                log::info!("IApplicationProxy: GetAudioController");
                IpcResponse::success().with_move_handle(0)
            }
            4 => {
                log::info!("IApplicationProxy: GetDisplayController");
                IpcResponse::success().with_move_handle(0)
            }
            // GetProcessWindingController
            10 => {
                log::info!("IApplicationProxy: GetProcessWindingController");
                IpcResponse::success().with_move_handle(0)
            }
            11 => {
                log::info!("IApplicationProxy: GetLibraryAppletCreator");
                IpcResponse::success().with_move_handle(0)
            }
            20 => {
                log::info!("IApplicationProxy: GetApplicationFunctions");
                IpcResponse::success().with_move_handle(0)
            }
            // GetDebugFunctions
            1000 => {
                log::info!("IApplicationProxy: GetDebugFunctions");
                IpcResponse::success().with_move_handle(0)
            }
            _ => {
                log::warn!("IApplicationProxy: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── ICommonStateGetter ───────────────────────────────────────────────────────

pub struct CommonStateGetterService {
    /// Kernel event handle returned for GetEventHandle.
    event_handle: u32,
    /// Message queue. Populated with initial messages at creation.
    message_queue: VecDeque<u32>,
    /// VR mode state.
    vr_mode_enabled: bool,
}

impl CommonStateGetterService {
    /// Create with a dummy handle (for tests / headless mode).
    pub fn new() -> Self {
        Self::new_with_event(0)
    }

    /// Create with a real kernel event handle allocated by the caller.
    pub fn new_with_event(event_handle: u32) -> Self {
        let mut queue = VecDeque::new();
        // Games expect a FocusStateChanged message on first ReceiveMessage call.
        queue.push_back(AppletMessage::FocusStateChanged as u32);
        Self {
            event_handle,
            message_queue: queue,
            vr_mode_enabled: false,
        }
    }
}

impl Default for CommonStateGetterService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for CommonStateGetterService {
    fn service_name(&self) -> &str {
        "am:ICommonStateGetter"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("am:ICommonStateGetter: cmd_id={}", cmd_id);
        match cmd_id {
            // GetEventHandle (0) — focus state event
            0 => {
                log::info!(
                    "am:ICommonStateGetter: GetEventHandle (handle={})",
                    self.event_handle
                );
                IpcResponse::success().with_copy_handle(self.event_handle)
            }
            // ReceiveMessage (1)
            1 => {
                if let Some(msg) = self.message_queue.pop_front() {
                    log::info!("am:ICommonStateGetter: ReceiveMessage -> msg={}", msg);
                    IpcResponse::success_with_data(vec![msg])
                } else {
                    log::info!("am:ICommonStateGetter: ReceiveMessage (no messages)");
                    IpcResponse::error(AM_NO_MESSAGES)
                }
            }
            // GetOperationMode (5) — handheld
            5 => {
                log::info!("am:ICommonStateGetter: GetOperationMode (handheld)");
                IpcResponse::success_with_data(vec![OperationMode::Handheld as u32])
            }
            // GetPerformanceMode (6) — normal
            6 => {
                log::info!("am:ICommonStateGetter: GetPerformanceMode (normal)");
                IpcResponse::success_with_data(vec![0])
            }
            // GetBootMode (8) — normal
            8 => {
                log::info!("am:ICommonStateGetter: GetBootMode");
                IpcResponse::success_with_data(vec![0])
            }
            // GetCurrentFocusState (9) — in focus
            9 => {
                log::info!("am:ICommonStateGetter: GetCurrentFocusState (in-focus)");
                IpcResponse::success_with_data(vec![FocusState::InFocus as u32])
            }
            // RequestToAcquireSleepLock (10)
            10 => {
                log::info!("am:ICommonStateGetter: RequestToAcquireSleepLock");
                IpcResponse::success()
            }
            // GetAcquiredSleepLockEvent (13)
            13 => {
                log::info!("am:ICommonStateGetter: GetAcquiredSleepLockEvent");
                IpcResponse::success().with_copy_handle(self.event_handle)
            }
            // GetReaderLockAccessorEx (31)
            31 => {
                log::info!("am:ICommonStateGetter: GetReaderLockAccessorEx");
                IpcResponse::success().with_move_handle(0)
            }
            // GetWriterLockAccessorEx (32)
            32 => {
                log::info!("am:ICommonStateGetter: GetWriterLockAccessorEx");
                IpcResponse::success().with_move_handle(0)
            }
            // IsVrModeEnabled (50)
            50 => {
                log::info!(
                    "am:ICommonStateGetter: IsVrModeEnabled -> {}",
                    self.vr_mode_enabled
                );
                IpcResponse::success_with_data(vec![self.vr_mode_enabled as u32])
            }
            // SetVrModeEnabled (51)
            51 => {
                let enabled = _command.raw_data.first().copied().unwrap_or(0) != 0;
                self.vr_mode_enabled = enabled;
                log::info!("am:ICommonStateGetter: SetVrModeEnabled({})", enabled);
                IpcResponse::success()
            }
            // SetLcdBacklighOffEnabled (52)
            52 => {
                log::info!("am:ICommonStateGetter: SetLcdBacklighOffEnabled");
                IpcResponse::success()
            }
            // BeginVrModeEx (53)
            53 => {
                self.vr_mode_enabled = true;
                log::info!("am:ICommonStateGetter: BeginVrModeEx");
                IpcResponse::success()
            }
            // EndVrModeEx (54)
            54 => {
                self.vr_mode_enabled = false;
                log::info!("am:ICommonStateGetter: EndVrModeEx");
                IpcResponse::success()
            }
            // IsInControllerFirmwareUpdateSection (55)
            55 => {
                log::info!("am:ICommonStateGetter: IsInControllerFirmwareUpdateSection -> false");
                IpcResponse::success_with_data(vec![0])
            }
            // GetDefaultDisplayResolution (60) — 1280x720 undocked
            60 => {
                log::info!("am:ICommonStateGetter: GetDefaultDisplayResolution (1280x720)");
                IpcResponse::success_with_data(vec![1280, 720])
            }
            // GetDefaultDisplayResolutionChangeEvent (61)
            61 => {
                log::info!("am:ICommonStateGetter: GetDefaultDisplayResolutionChangeEvent");
                IpcResponse::success().with_copy_handle(self.event_handle)
            }
            // SetCpuBoostMode (66)
            66 => {
                log::info!("am:ICommonStateGetter: SetCpuBoostMode");
                IpcResponse::success()
            }
            // GetBuiltInDisplayType (68)
            68 => {
                log::info!("am:ICommonStateGetter: GetBuiltInDisplayType -> 0");
                IpcResponse::success_with_data(vec![0])
            }
            // PerformSystemButtonPressingIfInFocus (80)
            80 => {
                log::info!("am:ICommonStateGetter: PerformSystemButtonPressingIfInFocus");
                IpcResponse::success()
            }
            // GetAppletLaunchedHistory (120)
            120 => {
                log::info!("am:ICommonStateGetter: GetAppletLaunchedHistory");
                IpcResponse::success_with_data(vec![0])
            }
            // GetOperationModeSystemInfo (200)
            200 => {
                log::info!("am:ICommonStateGetter: GetOperationModeSystemInfo -> 0");
                IpcResponse::success_with_data(vec![0])
            }
            // GetSettingsPlatformRegion (300) — Global (1)
            300 => {
                log::info!("am:ICommonStateGetter: GetSettingsPlatformRegion -> Global(1)");
                IpcResponse::success_with_data(vec![1])
            }
            _ => {
                log::warn!("am:ICommonStateGetter: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── ISelfController ──────────────────────────────────────────────────────────

pub struct SelfControllerService {
    /// Next managed display layer ID to assign.
    next_layer_id: u64,
    /// Fatal section nesting counter.
    fatal_section_count: u32,
    /// Whether exit is locked.
    exit_locked: bool,
    /// Accumulated suspended ticks (always 0 in emulation).
    suspended_ticks: u64,
    /// Idle time detection extension setting.
    idle_time_detection_extension: u32,
    /// Auto-sleep disabled flag.
    auto_sleep_disabled: bool,
    /// Screenshot permission setting.
    screenshot_permission: u32,
}

impl SelfControllerService {
    pub fn new() -> Self {
        Self {
            next_layer_id: 1,
            fatal_section_count: 0,
            exit_locked: false,
            suspended_ticks: 0,
            idle_time_detection_extension: 0,
            auto_sleep_disabled: false,
            screenshot_permission: 0,
        }
    }
}

impl Default for SelfControllerService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for SelfControllerService {
    fn service_name(&self) -> &str {
        "am:ISelfController"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("am:ISelfController: cmd_id={}", cmd_id);
        match cmd_id {
            // Exit (0)
            0 => {
                log::info!("am:ISelfController: Exit");
                IpcResponse::success()
            }
            // LockExit (1)
            1 => {
                self.exit_locked = true;
                log::info!("am:ISelfController: LockExit");
                IpcResponse::success()
            }
            // UnlockExit (2)
            2 => {
                self.exit_locked = false;
                log::info!("am:ISelfController: UnlockExit");
                IpcResponse::success()
            }
            // EnterFatalSection (3)
            3 => {
                self.fatal_section_count += 1;
                log::info!(
                    "am:ISelfController: EnterFatalSection (count={})",
                    self.fatal_section_count
                );
                IpcResponse::success()
            }
            // LeaveFatalSection (4)
            4 => {
                if self.fatal_section_count == 0 {
                    log::error!("am:ISelfController: LeaveFatalSection count imbalance!");
                    return IpcResponse::error(AM_FATAL_SECTION_COUNT_IMBALANCE);
                }
                self.fatal_section_count -= 1;
                log::info!(
                    "am:ISelfController: LeaveFatalSection (count={})",
                    self.fatal_section_count
                );
                IpcResponse::success()
            }
            // GetLibraryAppletLaunchableEvent (9)
            9 => {
                log::info!("am:ISelfController: GetLibraryAppletLaunchableEvent");
                IpcResponse::success().with_copy_handle(0)
            }
            // SetScreenShotPermission (10)
            10 => {
                let perm = _command.raw_data.first().copied().unwrap_or(0);
                self.screenshot_permission = perm;
                log::info!("am:ISelfController: SetScreenShotPermission({})", perm);
                IpcResponse::success()
            }
            // SetOperationModeChangedNotification (11)
            11 => {
                log::info!("am:ISelfController: SetOperationModeChangedNotification");
                IpcResponse::success()
            }
            // SetPerformanceModeChangedNotification (12)
            12 => {
                log::info!("am:ISelfController: SetPerformanceModeChangedNotification");
                IpcResponse::success()
            }
            // SetFocusHandlingMode (13)
            13 => {
                log::info!("am:ISelfController: SetFocusHandlingMode");
                IpcResponse::success()
            }
            // SetRestartMessageEnabled (14)
            14 => {
                log::info!("am:ISelfController: SetRestartMessageEnabled");
                IpcResponse::success()
            }
            // SetScreenShotAppletIdentityInfo (15)
            15 => {
                log::info!("am:ISelfController: SetScreenShotAppletIdentityInfo");
                IpcResponse::success()
            }
            // SetOutOfFocusSuspendingEnabled (16)
            16 => {
                log::info!("am:ISelfController: SetOutOfFocusSuspendingEnabled");
                IpcResponse::success()
            }
            // SetAlbumImageOrientation (19)
            19 => {
                log::info!("am:ISelfController: SetAlbumImageOrientation");
                IpcResponse::success()
            }
            // CreateManagedDisplayLayer (40) — returns layer_id
            40 => {
                let layer_id = self.next_layer_id;
                self.next_layer_id += 1;
                log::info!(
                    "am:ISelfController: CreateManagedDisplayLayer (layer_id={})",
                    layer_id
                );
                IpcResponse::success_with_data(vec![layer_id as u32, (layer_id >> 32) as u32])
            }
            // IsSystemBufferSharingEnabled (41)
            41 => {
                log::info!("am:ISelfController: IsSystemBufferSharingEnabled");
                IpcResponse::success()
            }
            // GetSystemSharedLayerHandle (42) — buffer_id(u64) + layer_id(u64)
            42 => {
                log::info!("am:ISelfController: GetSystemSharedLayerHandle");
                IpcResponse::success_with_data(vec![0, 0, 0, 0])
            }
            // GetSystemSharedBufferHandle (43) — buffer_id(u64)
            43 => {
                log::info!("am:ISelfController: GetSystemSharedBufferHandle");
                IpcResponse::success_with_data(vec![0, 0])
            }
            // CreateManagedDisplaySeparableLayer (44) — layer_id(u64) + recording_layer_id(u64)
            44 => {
                let layer_id = self.next_layer_id;
                self.next_layer_id += 1;
                let recording_layer_id = self.next_layer_id;
                self.next_layer_id += 1;
                log::info!(
                    "am:ISelfController: CreateManagedDisplaySeparableLayer (layer={}, recording={})",
                    layer_id, recording_layer_id
                );
                IpcResponse::success_with_data(vec![
                    layer_id as u32,
                    (layer_id >> 32) as u32,
                    recording_layer_id as u32,
                    (recording_layer_id >> 32) as u32,
                ])
            }
            // SetHandlesRequestToDisplay (50)
            50 => {
                log::info!("am:ISelfController: SetHandlesRequestToDisplay");
                IpcResponse::success()
            }
            // ApproveToDisplay (51)
            51 => {
                log::info!("am:ISelfController: ApproveToDisplay");
                IpcResponse::success()
            }
            // OverrideAutoSleepTimeAndDimmingTime (60)
            60 => {
                log::info!("am:ISelfController: OverrideAutoSleepTimeAndDimmingTime");
                IpcResponse::success()
            }
            // SetMediaPlaybackState (61)
            61 => {
                log::info!("am:ISelfController: SetMediaPlaybackState");
                IpcResponse::success()
            }
            // SetIdleTimeDetectionExtension (62)
            62 => {
                let ext = _command.raw_data.first().copied().unwrap_or(0);
                self.idle_time_detection_extension = ext;
                log::info!("am:ISelfController: SetIdleTimeDetectionExtension({})", ext);
                IpcResponse::success()
            }
            // GetIdleTimeDetectionExtension (63)
            63 => {
                log::info!(
                    "am:ISelfController: GetIdleTimeDetectionExtension -> {}",
                    self.idle_time_detection_extension
                );
                IpcResponse::success_with_data(vec![self.idle_time_detection_extension])
            }
            // ReportUserIsActive (65)
            65 => {
                log::info!("am:ISelfController: ReportUserIsActive");
                IpcResponse::success()
            }
            // SetAutoSleepDisabled (68)
            68 => {
                let disabled = _command.raw_data.first().copied().unwrap_or(0) != 0;
                self.auto_sleep_disabled = disabled;
                log::info!("am:ISelfController: SetAutoSleepDisabled({})", disabled);
                IpcResponse::success()
            }
            // IsAutoSleepDisabled (69)
            69 => {
                log::info!(
                    "am:ISelfController: IsAutoSleepDisabled -> {}",
                    self.auto_sleep_disabled
                );
                IpcResponse::success_with_data(vec![self.auto_sleep_disabled as u32])
            }
            // SetInputDetectionPolicy (72)
            72 => {
                log::info!("am:ISelfController: SetInputDetectionPolicy");
                IpcResponse::success()
            }
            // GetAccumulatedSuspendedTickValue (90) — always 0
            90 => {
                log::info!("am:ISelfController: GetAccumulatedSuspendedTickValue -> 0");
                IpcResponse::success_with_data(vec![
                    self.suspended_ticks as u32,
                    (self.suspended_ticks >> 32) as u32,
                ])
            }
            // GetAccumulatedSuspendedTickChangedEvent (91)
            91 => {
                log::info!("am:ISelfController: GetAccumulatedSuspendedTickChangedEvent");
                IpcResponse::success().with_copy_handle(0)
            }
            // SetAlbumImageTakenNotificationEnabled (100)
            100 => {
                log::info!("am:ISelfController: SetAlbumImageTakenNotificationEnabled");
                IpcResponse::success()
            }
            // SetRecordVolumeMuted (130)
            130 => {
                log::info!("am:ISelfController: SetRecordVolumeMuted");
                IpcResponse::success()
            }
            _ => {
                log::warn!("am:ISelfController: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── IWindowController ────────────────────────────────────────────────────────

pub struct WindowControllerService;

impl WindowControllerService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for WindowControllerService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for WindowControllerService {
    fn service_name(&self) -> &str {
        "am:IWindowController"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("am:IWindowController: cmd_id={}", cmd_id);
        match cmd_id {
            // GetAppletResourceUserId (1) — uid=0 (u64 as two u32s)
            1 => {
                log::info!("am:IWindowController: GetAppletResourceUserId");
                IpcResponse::success_with_data(vec![0, 0])
            }
            // GetAppletResourceUserIdOfCallerApplet (2)
            2 => {
                log::info!("am:IWindowController: GetAppletResourceUserIdOfCallerApplet");
                IpcResponse::success_with_data(vec![0, 0])
            }
            // AcquireForegroundRights (10)
            10 => {
                log::info!("am:IWindowController: AcquireForegroundRights");
                IpcResponse::success()
            }
            // ReleaseForegroundRights (11)
            11 => {
                log::info!("am:IWindowController: ReleaseForegroundRights");
                IpcResponse::success()
            }
            // RejectToChangeIntoBackground (12)
            12 => {
                log::info!("am:IWindowController: RejectToChangeIntoBackground");
                IpcResponse::success()
            }
            // SetAppletWindowVisibility (20)
            20 => {
                log::info!("am:IWindowController: SetAppletWindowVisibility");
                IpcResponse::success()
            }
            // SetAppletGpuTimeSlice (21)
            21 => {
                log::info!("am:IWindowController: SetAppletGpuTimeSlice");
                IpcResponse::success()
            }
            _ => {
                log::warn!("am:IWindowController: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── IApplicationFunctions ────────────────────────────────────────────────────

pub struct ApplicationFunctionsService {
    /// Previous program index for ExecuteProgram/GetPreviousProgramIndex.
    previous_program_index: i32,
}

impl ApplicationFunctionsService {
    pub fn new() -> Self {
        Self {
            previous_program_index: 0,
        }
    }
}

impl Default for ApplicationFunctionsService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for ApplicationFunctionsService {
    fn service_name(&self) -> &str {
        "am:IApplicationFunctions"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("am:IApplicationFunctions: cmd_id={}", cmd_id);
        match cmd_id {
            // PopLaunchParameter (1) — no launch params
            1 => {
                log::info!("am:IApplicationFunctions: PopLaunchParameter (none)");
                IpcResponse::error(AM_NO_DATA_IN_CHANNEL)
            }
            // EnsureSaveData (20) — 0 bytes needed (u64)
            20 => {
                log::info!("am:IApplicationFunctions: EnsureSaveData");
                IpcResponse::success_with_data(vec![0, 0])
            }
            // GetDesiredLanguage (21) — return "en-US" as language code u64
            21 => {
                // C++ returns language code via ns service. We return en-US directly.
                let lang_code: u64 = u64::from_le_bytes(*b"en-US\0\0\0");
                log::info!(
                    "am:IApplicationFunctions: GetDesiredLanguage -> 0x{:016X}",
                    lang_code
                );
                IpcResponse::success_with_data(vec![lang_code as u32, (lang_code >> 32) as u32])
            }
            // SetTerminateResult (22)
            22 => {
                log::info!("am:IApplicationFunctions: SetTerminateResult");
                IpcResponse::success()
            }
            // GetDisplayVersion (23) — "1.0.0" as 16-byte string
            23 => {
                log::info!("am:IApplicationFunctions: GetDisplayVersion");
                let mut version = [0u8; 16];
                let default_ver = b"1.0.0";
                version[..default_ver.len()].copy_from_slice(default_ver);
                let mut data = Vec::new();
                for chunk in version.chunks(4) {
                    let mut buf = [0u8; 4];
                    buf[..chunk.len()].copy_from_slice(chunk);
                    data.push(u32::from_le_bytes(buf));
                }
                IpcResponse::success_with_data(data)
            }
            // ExtendSaveData (25) — required_size = 0 (u64)
            25 => {
                log::info!("am:IApplicationFunctions: ExtendSaveData");
                IpcResponse::success_with_data(vec![0, 0])
            }
            // GetSaveDataSize (26) — normal=0, journal=0 (two u64s)
            26 => {
                log::info!("am:IApplicationFunctions: GetSaveDataSize");
                IpcResponse::success_with_data(vec![0, 0, 0, 0])
            }
            // CreateCacheStorage (27) — target_media=1(Nand), required_size=0
            27 => {
                log::info!("am:IApplicationFunctions: CreateCacheStorage");
                IpcResponse::success_with_data(vec![1, 0, 0])
            }
            // GetSaveDataSizeMax (28)
            28 => {
                log::info!("am:IApplicationFunctions: GetSaveDataSizeMax");
                IpcResponse::success_with_data(vec![0x0FFFFFFF, 0, 0x0FFFFFFF, 0])
            }
            // GetCacheStorageMax (29)
            29 => {
                log::info!("am:IApplicationFunctions: GetCacheStorageMax");
                IpcResponse::success_with_data(vec![0, 0, 0])
            }
            // BeginBlockingHomeButtonShortAndLongPressed (30)
            30 => {
                log::info!("am:IApplicationFunctions: BeginBlockingHomeButtonShortAndLongPressed");
                IpcResponse::success()
            }
            // EndBlockingHomeButtonShortAndLongPressed (31)
            31 => {
                log::info!("am:IApplicationFunctions: EndBlockingHomeButtonShortAndLongPressed");
                IpcResponse::success()
            }
            // BeginBlockingHomeButton (32)
            32 => {
                log::info!("am:IApplicationFunctions: BeginBlockingHomeButton");
                IpcResponse::success()
            }
            // EndBlockingHomeButton (33)
            33 => {
                log::info!("am:IApplicationFunctions: EndBlockingHomeButton");
                IpcResponse::success()
            }
            // NotifyRunning (40) — acknowledged, out_became_running = true
            40 => {
                log::info!("am:IApplicationFunctions: NotifyRunning");
                IpcResponse::success_with_data(vec![1])
            }
            // GetPseudoDeviceId (50) — zeroed 16-byte UUID (4 u32s)
            50 => {
                log::info!("am:IApplicationFunctions: GetPseudoDeviceId");
                IpcResponse::success_with_data(vec![0, 0, 0, 0])
            }
            // IsGamePlayRecordingSupported (65) — false
            65 => {
                log::info!("am:IApplicationFunctions: IsGamePlayRecordingSupported -> false");
                IpcResponse::success_with_data(vec![0])
            }
            // InitializeGamePlayRecording (66)
            66 => {
                log::info!("am:IApplicationFunctions: InitializeGamePlayRecording");
                IpcResponse::success()
            }
            // SetGamePlayRecordingState (67)
            67 => {
                log::info!("am:IApplicationFunctions: SetGamePlayRecordingState");
                IpcResponse::success()
            }
            // EnableApplicationCrashReport (90)
            90 => {
                log::info!("am:IApplicationFunctions: EnableApplicationCrashReport");
                IpcResponse::success()
            }
            // InitializeApplicationCopyrightFrameBuffer (100)
            100 => {
                log::info!("am:IApplicationFunctions: InitializeApplicationCopyrightFrameBuffer");
                IpcResponse::success()
            }
            // SetApplicationCopyrightImage (101)
            101 => {
                log::info!("am:IApplicationFunctions: SetApplicationCopyrightImage");
                IpcResponse::success()
            }
            // SetApplicationCopyrightVisibility (102)
            102 => {
                log::info!("am:IApplicationFunctions: SetApplicationCopyrightVisibility");
                IpcResponse::success()
            }
            // QueryApplicationPlayStatistics (110)
            110 => {
                log::info!("am:IApplicationFunctions: QueryApplicationPlayStatistics -> 0 entries");
                IpcResponse::success_with_data(vec![0])
            }
            // QueryApplicationPlayStatisticsByUid (111)
            111 => {
                log::info!(
                    "am:IApplicationFunctions: QueryApplicationPlayStatisticsByUid -> 0 entries"
                );
                IpcResponse::success_with_data(vec![0])
            }
            // ExecuteProgram (120)
            120 => {
                log::info!("am:IApplicationFunctions: ExecuteProgram");
                IpcResponse::success()
            }
            // ClearUserChannel (121)
            121 => {
                log::info!("am:IApplicationFunctions: ClearUserChannel");
                IpcResponse::success()
            }
            // UnpopToUserChannel (122)
            122 => {
                log::info!("am:IApplicationFunctions: UnpopToUserChannel");
                IpcResponse::success()
            }
            // GetPreviousProgramIndex (123)
            123 => {
                log::info!(
                    "am:IApplicationFunctions: GetPreviousProgramIndex -> {}",
                    self.previous_program_index
                );
                IpcResponse::success_with_data(vec![self.previous_program_index as u32])
            }
            // GetGpuErrorDetectedSystemEvent (130)
            130 => {
                log::info!("am:IApplicationFunctions: GetGpuErrorDetectedSystemEvent");
                IpcResponse::success().with_copy_handle(0)
            }
            // GetFriendInvitationStorageChannelEvent (140)
            140 => {
                log::info!("am:IApplicationFunctions: GetFriendInvitationStorageChannelEvent");
                IpcResponse::success().with_copy_handle(0)
            }
            // TryPopFromFriendInvitationStorageChannel (141)
            141 => {
                log::info!("am:IApplicationFunctions: TryPopFromFriendInvitationStorageChannel");
                IpcResponse::error(AM_NO_DATA_IN_CHANNEL)
            }
            // GetNotificationStorageChannelEvent (150)
            150 => {
                log::info!("am:IApplicationFunctions: GetNotificationStorageChannelEvent");
                IpcResponse::success().with_copy_handle(0)
            }
            // GetHealthWarningDisappearedSystemEvent (160)
            160 => {
                log::info!("am:IApplicationFunctions: GetHealthWarningDisappearedSystemEvent");
                IpcResponse::success().with_copy_handle(0)
            }
            // PrepareForJit (1001)
            1001 => {
                log::info!("am:IApplicationFunctions: PrepareForJit");
                IpcResponse::success()
            }
            _ => {
                log::warn!("am:IApplicationFunctions: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── Stub sub-services ────────────────────────────────────────────────────────

pub struct AudioControllerService;

impl AudioControllerService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for AudioControllerService {
    fn service_name(&self) -> &str {
        "am:IAudioController"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("am:IAudioController: cmd_id={}", cmd_id);
        IpcResponse::success()
    }
}

pub struct DisplayControllerService;

impl DisplayControllerService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for DisplayControllerService {
    fn service_name(&self) -> &str {
        "am:IDisplayController"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("am:IDisplayController: cmd_id={}", cmd_id);
        IpcResponse::success()
    }
}

pub struct LibraryAppletCreatorService;

impl LibraryAppletCreatorService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for LibraryAppletCreatorService {
    fn service_name(&self) -> &str {
        "am:ILibraryAppletCreator"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("am:ILibraryAppletCreator: cmd_id={}", cmd_id);
        IpcResponse::success()
    }
}

/// IProcessWindingController stub (from application_proxy cmd 10)
pub struct ProcessWindingControllerService;

impl ProcessWindingControllerService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for ProcessWindingControllerService {
    fn service_name(&self) -> &str {
        "am:IProcessWindingController"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("am:IProcessWindingController: cmd_id={}", cmd_id);
        IpcResponse::success()
    }
}

/// IDebugFunctions stub (from application_proxy cmd 1000)
pub struct DebugFunctionsService;

impl DebugFunctionsService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for DebugFunctionsService {
    fn service_name(&self) -> &str {
        "am:IDebugFunctions"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("am:IDebugFunctions: cmd_id={}", cmd_id);
        IpcResponse::success()
    }
}

/// ILockAccessor stub (from ICommonStateGetter cmd 31/32)
pub struct LockAccessorService;

impl LockAccessorService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for LockAccessorService {
    fn service_name(&self) -> &str {
        "am:ILockAccessor"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("am:ILockAccessor: cmd_id={}", cmd_id);
        match cmd_id {
            // TryLock (1) — succeeded = true, handle
            1 => {
                log::info!("am:ILockAccessor: TryLock -> true");
                IpcResponse::success_with_data(vec![1]).with_copy_handle(0)
            }
            // Unlock (2)
            2 => {
                log::info!("am:ILockAccessor: Unlock");
                IpcResponse::success()
            }
            // GetEvent (3)
            3 => {
                log::info!("am:ILockAccessor: GetEvent");
                IpcResponse::success().with_copy_handle(0)
            }
            _ => {
                log::warn!("am:ILockAccessor: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ipc::CommandType;

    fn make_command(cmd_id: u32) -> IpcCommand {
        IpcCommand {
            command_type: CommandType::Request,
            data_size: 0,
            num_x_bufs: 0,
            num_a_bufs: 0,
            num_b_bufs: 0,
            has_handle_descriptor: false,
            handles_to_copy: Vec::new(),
            handles_to_move: Vec::new(),
            send_pid: false,
            cmif_magic: 0x49434653,
            command_id: cmd_id,
            raw_data: Vec::new(),
            b_buf_addrs: Vec::new(),
            x_bufs: Vec::new(),
            a_bufs: Vec::new(),
            a_buf_data: Vec::new(),
            b_buf_sizes: Vec::new(),
        }
    }

    #[test]
    fn test_open_application_proxy() {
        let mut svc = AmService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_move, vec![0]);
    }

    #[test]
    fn test_proxy_get_sub_interfaces() {
        let mut proxy = ApplicationProxyService::new();
        for cmd_id in [0, 1, 2, 3, 4, 10, 11, 20, 1000] {
            let cmd = make_command(cmd_id);
            let resp = proxy.handle_request(cmd_id, &cmd);
            assert!(resp.result.is_success());
            assert_eq!(
                resp.handles_to_move,
                vec![0],
                "cmd_id={cmd_id} should return move_handle(0)"
            );
        }
    }

    #[test]
    fn test_common_state_getter_focus() {
        let mut svc = CommonStateGetterService::new();
        let cmd = make_command(9);
        let resp = svc.handle_request(9, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![1]); // in-focus
    }

    #[test]
    fn test_common_state_getter_receive_message() {
        let mut svc = CommonStateGetterService::new();
        let cmd = make_command(1);

        // First call returns FocusStateChanged (msg 15).
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![AppletMessage::FocusStateChanged as u32]);

        // Second call returns no messages.
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_error());
        assert_eq!(resp.result, AM_NO_MESSAGES);
    }

    #[test]
    fn test_common_state_getter_display_resolution() {
        let mut svc = CommonStateGetterService::new();
        let cmd = make_command(60);
        let resp = svc.handle_request(60, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![1280, 720]);
    }

    #[test]
    fn test_self_controller_create_managed_display_layer() {
        let mut svc = SelfControllerService::new();
        let cmd = make_command(40);
        let resp = svc.handle_request(40, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![1, 0]); // layer_id=1 as u64
    }

    #[test]
    fn test_self_controller_fatal_section() {
        let mut svc = SelfControllerService::new();
        // Enter
        let cmd = make_command(3);
        let resp = svc.handle_request(3, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(svc.fatal_section_count, 1);
        // Leave
        let cmd = make_command(4);
        let resp = svc.handle_request(4, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(svc.fatal_section_count, 0);
        // Leave again (imbalance)
        let resp = svc.handle_request(4, &cmd);
        assert!(resp.result.is_error());
    }

    #[test]
    fn test_self_controller_accumulated_suspended_ticks() {
        let mut svc = SelfControllerService::new();
        let cmd = make_command(90);
        let resp = svc.handle_request(90, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![0, 0]);
    }

    #[test]
    fn test_window_controller_applet_resource_uid() {
        let mut svc = WindowControllerService::new();
        let cmd = make_command(1);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![0, 0]); // uid=0 as u64
    }

    #[test]
    fn test_application_functions_notify_running() {
        let mut svc = ApplicationFunctionsService::new();
        let cmd = make_command(40);
        let resp = svc.handle_request(40, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![1]);
    }

    #[test]
    fn test_application_functions_pop_launch_parameter() {
        let mut svc = ApplicationFunctionsService::new();
        let cmd = make_command(1);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_error());
    }

    #[test]
    fn test_applet_ae_open_proxy() {
        let mut svc = AppletAeService::new();
        for cmd_id in [0, 100, 200, 300] {
            let cmd = make_command(cmd_id);
            let resp = svc.handle_request(cmd_id, &cmd);
            assert!(resp.result.is_success());
            assert_eq!(
                resp.handles_to_move,
                vec![0],
                "cmd_id={cmd_id} should return move_handle(0)"
            );
        }
    }

    #[test]
    fn test_application_functions_desired_language() {
        let mut svc = ApplicationFunctionsService::new();
        let cmd = make_command(21);
        let resp = svc.handle_request(21, &cmd);
        assert!(resp.result.is_success());
        // Should be "en-US\0\0\0" as u64
        let lang_code: u64 = u64::from_le_bytes(*b"en-US\0\0\0");
        assert_eq!(resp.data, vec![lang_code as u32, (lang_code >> 32) as u32]);
    }

    #[test]
    fn test_application_functions_display_version() {
        let mut svc = ApplicationFunctionsService::new();
        let cmd = make_command(23);
        let resp = svc.handle_request(23, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data.len(), 4); // 16 bytes = 4 u32s
    }

    #[test]
    fn test_application_functions_get_previous_program_index() {
        let mut svc = ApplicationFunctionsService::new();
        let cmd = make_command(123);
        let resp = svc.handle_request(123, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![0]);
    }

    #[test]
    fn test_application_functions_gpu_error_event() {
        let mut svc = ApplicationFunctionsService::new();
        let cmd = make_command(130);
        let resp = svc.handle_request(130, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_copy.len(), 1);
    }
}
