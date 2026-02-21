// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `nvdrv` / `nvdrv:a` / `nvdrv:s` / `nvdrv:t` -- NVIDIA driver service stub.
//!
//! All commands return success.

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

/// HLE stub for the NVIDIA driver family of services.
pub struct NvdrvService;

impl NvdrvService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for NvdrvService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for NvdrvService {
    fn service_name(&self) -> &str {
        "nvdrv"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("nvdrv: cmd_id={}", cmd_id);

        match cmd_id {
            // ── Open (0) ─────────────────────────────────────────────────
            0 => {
                log::info!("nvdrv: Open");
                // Return fd = 0 and error code = 0 (success).
                IpcResponse::success_with_data(vec![0, 0])
            }

            // ── Ioctl (1) ────────────────────────────────────────────────
            1 => {
                log::info!("nvdrv: Ioctl");
                // Return error code = 0 (success).
                IpcResponse::success_with_data(vec![0])
            }

            // ── Close (2) ────────────────────────────────────────────────
            2 => {
                log::info!("nvdrv: Close");
                IpcResponse::success_with_data(vec![0])
            }

            // ── Initialize (3) ───────────────────────────────────────────
            3 => {
                log::info!("nvdrv: Initialize");
                IpcResponse::success()
            }

            // ── QueryEvent (4) ───────────────────────────────────────────
            4 => {
                log::info!("nvdrv: QueryEvent");
                IpcResponse::success()
            }

            // ── SetAruid (8) ─────────────────────────────────────────────
            8 => {
                log::info!("nvdrv: SetAruid");
                IpcResponse::success_with_data(vec![0])
            }

            // ── Everything else ──────────────────────────────────────────
            _ => {
                log::warn!("nvdrv: unhandled cmd_id={}", cmd_id);
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
    fn test_open() {
        let mut svc = NvdrvService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![0, 0]);
    }

    #[test]
    fn test_ioctl() {
        let mut svc = NvdrvService::new();
        let cmd = make_command(1);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![0]);
    }

    #[test]
    fn test_unhandled_returns_success() {
        let mut svc = NvdrvService::new();
        let cmd = make_command(9999);
        let resp = svc.handle_request(9999, &cmd);
        assert!(resp.result.is_success());
    }
}
