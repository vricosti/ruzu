// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `appletOE` / `am` -- Applet Manager service stub.
//!
//! This is a minimal stub that returns success for all commands.
//! The applet manager exposes a hierarchy of interfaces:
//!   appletOE -> IApplicationProxy -> IApplicationFunctions, etc.
//!
//! Commands on the root interface:
//!   0 = OpenApplicationProxy

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

// ── Root interface: appletOE ─────────────────────────────────────────────────

/// HLE stub for `appletOE` (the application-side AM interface).
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

/// Dummy handle value returned when opening sub-interfaces.
const DUMMY_PROXY_HANDLE: u32 = 0xAE01;

impl ServiceHandler for AmService {
    fn service_name(&self) -> &str {
        "appletOE"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("appletOE: cmd_id={}", cmd_id);

        match cmd_id {
            // ── OpenApplicationProxy ─────────────────────────────────────
            0 => {
                log::info!("appletOE: OpenApplicationProxy");
                IpcResponse::success().with_move_handle(DUMMY_PROXY_HANDLE)
            }

            _ => {
                log::warn!("appletOE: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── Sub-interface stub: IApplicationProxy ────────────────────────────────────

/// Stub for the `IApplicationProxy` sub-interface opened via
/// `OpenApplicationProxy`.  All commands return success.
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
            // 0 = GetCommonStateGetter
            // 1 = GetSelfController
            // 2 = GetWindowController
            // 3 = GetAudioController
            // 4 = GetDisplayController
            // 10 = GetProcessWindingController
            // 11 = GetLibraryAppletCreator
            // 20 = GetApplicationFunctions
            // 1000 = GetDebugFunctions
            _ => {
                log::info!("IApplicationProxy: stub success for cmd_id={}", cmd_id);
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
        }
    }

    #[test]
    fn test_open_application_proxy() {
        let mut svc = AmService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_move.len(), 1);
        assert_eq!(resp.handles_to_move[0], DUMMY_PROXY_HANDLE);
    }

    #[test]
    fn test_application_proxy_stub() {
        let mut proxy = ApplicationProxyService::new();
        for cmd_id in [0, 1, 2, 3, 4, 10, 11, 20, 1000] {
            let cmd = make_command(cmd_id);
            let resp = proxy.handle_request(cmd_id, &cmd);
            assert!(resp.result.is_success());
        }
    }
}
