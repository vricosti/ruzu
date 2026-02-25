// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `appletOE` / `am` -- Applet Manager service.
//!
//! Service hierarchy:
//!   appletOE -> IApplicationProxy -> ICommonStateGetter
//!                                 -> ISelfController
//!                                 -> IWindowController
//!                                 -> IAudioController
//!                                 -> IDisplayController
//!                                 -> ILibraryAppletCreator
//!                                 -> IApplicationFunctions

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};
use ruzu_common::ResultCode;

/// AM error: no messages available.
const AM_NO_MESSAGES: ResultCode = ResultCode(0x680);

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
            11 => {
                log::info!("IApplicationProxy: GetLibraryAppletCreator");
                IpcResponse::success().with_move_handle(0)
            }
            20 => {
                log::info!("IApplicationProxy: GetApplicationFunctions");
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
    /// Pre-signaled at startup so games see "in focus" immediately.
    event_handle: u32,
}

impl CommonStateGetterService {
    /// Create with a dummy handle (for tests / headless mode).
    pub fn new() -> Self {
        Self::new_with_event(0)
    }

    /// Create with a real kernel event handle allocated by the caller.
    pub fn new_with_event(event_handle: u32) -> Self {
        Self { event_handle }
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
            // GetEventHandle — focus state event
            0 => {
                log::info!("am:ICommonStateGetter: GetEventHandle (handle={})", self.event_handle);
                IpcResponse::success().with_copy_handle(self.event_handle)
            }
            // ReceiveMessage — no messages available
            1 => {
                log::info!("am:ICommonStateGetter: ReceiveMessage (no messages)");
                IpcResponse::error(AM_NO_MESSAGES)
            }
            // GetOperationMode — handheld
            5 => {
                log::info!("am:ICommonStateGetter: GetOperationMode (handheld)");
                IpcResponse::success_with_data(vec![0])
            }
            // GetPerformanceMode — normal
            6 => {
                log::info!("am:ICommonStateGetter: GetPerformanceMode (normal)");
                IpcResponse::success_with_data(vec![0])
            }
            // GetBootMode — normal
            8 => {
                log::info!("am:ICommonStateGetter: GetBootMode");
                IpcResponse::success_with_data(vec![0])
            }
            // GetCurrentFocusState — in focus
            9 => {
                log::info!("am:ICommonStateGetter: GetCurrentFocusState (in-focus)");
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

pub struct SelfControllerService;

impl SelfControllerService {
    pub fn new() -> Self {
        Self
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
            1 => {
                log::info!("am:ISelfController: LockExit");
                IpcResponse::success()
            }
            10 => {
                log::info!("am:ISelfController: SetScreenShotPermission");
                IpcResponse::success()
            }
            11 => {
                log::info!("am:ISelfController: SetOperationModeChangedNotification");
                IpcResponse::success()
            }
            12 => {
                log::info!("am:ISelfController: SetPerformanceModeChangedNotification");
                IpcResponse::success()
            }
            13 => {
                log::info!("am:ISelfController: SetFocusHandlingMode");
                IpcResponse::success()
            }
            14 => {
                log::info!("am:ISelfController: SetRestartMessageEnabled");
                IpcResponse::success()
            }
            16 => {
                log::info!("am:ISelfController: SetOutOfFocusSuspendingEnabled");
                IpcResponse::success()
            }
            // CreateManagedDisplayLayer — returns layer_id=1
            40 => {
                log::info!("am:ISelfController: CreateManagedDisplayLayer (layer_id=1)");
                // layer_id is u64, return as two u32 words
                IpcResponse::success_with_data(vec![1, 0])
            }
            50 => {
                log::info!("am:ISelfController: SetHandlesRequestToDisplay");
                IpcResponse::success()
            }
            62 => {
                log::info!("am:ISelfController: SetIdleTimeDetectionExtension");
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
            // GetAppletResourceUserId — uid=0 (u64 as two u32s)
            1 => {
                log::info!("am:IWindowController: GetAppletResourceUserId");
                IpcResponse::success_with_data(vec![0, 0])
            }
            // AcquireForegroundRights
            10 => {
                log::info!("am:IWindowController: AcquireForegroundRights");
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

pub struct ApplicationFunctionsService;

impl ApplicationFunctionsService {
    pub fn new() -> Self {
        Self
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
            // PopLaunchParameter — no launch params
            1 => {
                log::info!("am:IApplicationFunctions: PopLaunchParameter (none)");
                IpcResponse::error(AM_NO_MESSAGES)
            }
            // EnsureSaveData — 0 bytes needed (u64)
            20 => {
                log::info!("am:IApplicationFunctions: EnsureSaveData");
                IpcResponse::success_with_data(vec![0, 0])
            }
            // GetDesiredLanguage — AmericanEnglish (1)
            21 => {
                log::info!("am:IApplicationFunctions: GetDesiredLanguage (en-US)");
                IpcResponse::success_with_data(vec![1])
            }
            // NotifyRunning — acknowledged
            40 => {
                log::info!("am:IApplicationFunctions: NotifyRunning");
                IpcResponse::success_with_data(vec![1])
            }
            // InitializeGamePlayRecording
            50 => {
                log::info!("am:IApplicationFunctions: InitializeGamePlayRecording");
                IpcResponse::success()
            }
            // SetGamePlayRecordingState
            51 => {
                log::info!("am:IApplicationFunctions: SetGamePlayRecordingState");
                IpcResponse::success()
            }
            // InitializeApplicationCopyrightFrameBuffer
            66 => {
                log::info!("am:IApplicationFunctions: InitializeApplicationCopyrightFrameBuffer");
                IpcResponse::success()
            }
            // GetPseudoDeviceId — zeroed 16-byte UUID (4 u32s)
            80 => {
                log::info!("am:IApplicationFunctions: GetPseudoDeviceId");
                IpcResponse::success_with_data(vec![0, 0, 0, 0])
            }
            // EnableApplicationCrashReport
            90 => {
                log::info!("am:IApplicationFunctions: EnableApplicationCrashReport");
                IpcResponse::success()
            }
            // CreateGameMovieTrimmer
            100 => {
                log::info!("am:IApplicationFunctions: CreateGameMovieTrimmer");
                IpcResponse::success()
            }
            // GetGpuErrorDetectedSystemEvent — dummy event
            110 => {
                log::info!("am:IApplicationFunctions: GetGpuErrorDetectedSystemEvent");
                IpcResponse::success().with_copy_handle(0)
            }
            _ => {
                log::warn!("am:IApplicationFunctions: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── Stub services ────────────────────────────────────────────────────────────

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
        for cmd_id in [0, 1, 2, 3, 4, 11, 20] {
            let cmd = make_command(cmd_id);
            let resp = proxy.handle_request(cmd_id, &cmd);
            assert!(resp.result.is_success());
            assert_eq!(resp.handles_to_move, vec![0], "cmd_id={cmd_id} should return move_handle(0)");
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
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_error());
        assert_eq!(resp.result, AM_NO_MESSAGES);
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
            assert_eq!(resp.handles_to_move, vec![0], "cmd_id={cmd_id} should return move_handle(0)");
        }
    }

    #[test]
    fn test_application_functions_desired_language() {
        let mut svc = ApplicationFunctionsService::new();
        let cmd = make_command(21);
        let resp = svc.handle_request(21, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![1]); // AmericanEnglish
    }
}
