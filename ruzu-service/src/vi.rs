// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `vi:m` / nvnflinger -- Visual Interface manager service stub.
//!
//! All commands return success.

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

/// HLE stub for `vi:m` (manager) and related display interfaces.
pub struct ViService;

impl ViService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ViService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for ViService {
    fn service_name(&self) -> &str {
        "vi:m"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("vi:m: cmd_id={}", cmd_id);

        match cmd_id {
            // ── GetDisplayService (2) ────────────────────────────────────
            2 => {
                log::info!("vi:m: GetDisplayService");
                IpcResponse::success()
            }

            // ── Everything else ──────────────────────────────────────────
            _ => {
                log::warn!("vi:m: unhandled cmd_id={}", cmd_id);
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
    fn test_get_display_service() {
        let mut svc = ViService::new();
        let cmd = make_command(2);
        let resp = svc.handle_request(2, &cmd);
        assert!(resp.result.is_success());
    }

    #[test]
    fn test_unhandled_returns_success() {
        let mut svc = ViService::new();
        let cmd = make_command(9999);
        let resp = svc.handle_request(9999, &cmd);
        assert!(resp.result.is_success());
    }
}
