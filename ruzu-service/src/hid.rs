// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `hid` -- Human Interface Device service stub.
//!
//! Commands:
//!   0 = CreateAppletResource
//!   All others: stub success.

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

/// HLE stub for `hid`.
pub struct HidService;

impl HidService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for HidService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for HidService {
    fn service_name(&self) -> &str {
        "hid"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("hid: cmd_id={}", cmd_id);

        match cmd_id {
            // ── CreateAppletResource ─────────────────────────────────────
            0 => {
                log::info!("hid: CreateAppletResource");
                IpcResponse::success()
            }

            // ── ActivateTouchScreen (11) ─────────────────────────────────
            11 => {
                log::info!("hid: ActivateTouchScreen");
                IpcResponse::success()
            }

            // ── ActivateMouse (21) ───────────────────────────────────────
            21 => {
                log::info!("hid: ActivateMouse");
                IpcResponse::success()
            }

            // ── ActivateKeyboard (31) ────────────────────────────────────
            31 => {
                log::info!("hid: ActivateKeyboard");
                IpcResponse::success()
            }

            // ── SetSupportedNpadStyleSet (100) ───────────────────────────
            100 => {
                log::info!("hid: SetSupportedNpadStyleSet");
                IpcResponse::success()
            }

            // ── SetSupportedNpadIdType (102) ─────────────────────────────
            102 => {
                log::info!("hid: SetSupportedNpadIdType");
                IpcResponse::success()
            }

            // ── ActivateNpad (103) ───────────────────────────────────────
            103 => {
                log::info!("hid: ActivateNpad");
                IpcResponse::success()
            }

            // ── SetNpadJoyHoldType (120) ─────────────────────────────────
            120 => {
                log::info!("hid: SetNpadJoyHoldType");
                IpcResponse::success()
            }

            // ── Everything else ──────────────────────────────────────────
            _ => {
                log::warn!("hid: unhandled cmd_id={}", cmd_id);
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
    fn test_create_applet_resource() {
        let mut svc = HidService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
    }

    #[test]
    fn test_unhandled_returns_success() {
        let mut svc = HidService::new();
        let cmd = make_command(9999);
        let resp = svc.handle_request(9999, &cmd);
        assert!(resp.result.is_success());
    }
}
