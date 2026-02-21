// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `fsp-srv` -- Filesystem service stub.
//!
//! Most commands return success; file-access commands that cannot be satisfied
//! return `NOT_FOUND`.

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};
use ruzu_common::error;

/// HLE stub for `fsp-srv`.
pub struct FspSrvService;

impl FspSrvService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for FspSrvService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for FspSrvService {
    fn service_name(&self) -> &str {
        "fsp-srv"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("fsp-srv: cmd_id={}", cmd_id);

        match cmd_id {
            // ── SetCurrentProcess (1) ────────────────────────────────────
            1 => {
                log::info!("fsp-srv: SetCurrentProcess");
                IpcResponse::success()
            }

            // ── OpenSdCardFileSystem (18) ────────────────────────────────
            18 => {
                log::info!("fsp-srv: OpenSdCardFileSystem (stub success)");
                IpcResponse::success()
            }

            // ── OpenSaveDataFileSystem (51) ──────────────────────────────
            51 => {
                log::info!("fsp-srv: OpenSaveDataFileSystem (stub NOT_FOUND)");
                IpcResponse::error(error::NOT_FOUND)
            }

            // ── OpenDataStorageByCurrentProcess (200) ────────────────────
            200 => {
                log::info!("fsp-srv: OpenDataStorageByCurrentProcess (stub success)");
                IpcResponse::success()
            }

            // ── OpenDataStorageByDataId (202) ────────────────────────────
            202 => {
                log::info!("fsp-srv: OpenDataStorageByDataId (stub NOT_FOUND)");
                IpcResponse::error(error::NOT_FOUND)
            }

            // ── GetGlobalAccessLogMode (1006) ────────────────────────────
            1006 => {
                log::info!("fsp-srv: GetGlobalAccessLogMode");
                // Return mode = 0 (disabled).
                IpcResponse::success_with_data(vec![0])
            }

            // ── Everything else: stub success ────────────────────────────
            _ => {
                log::warn!("fsp-srv: unhandled cmd_id={}", cmd_id);
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
    fn test_set_current_process() {
        let mut svc = FspSrvService::new();
        let cmd = make_command(1);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
    }

    #[test]
    fn test_open_save_data_returns_not_found() {
        let mut svc = FspSrvService::new();
        let cmd = make_command(51);
        let resp = svc.handle_request(51, &cmd);
        assert!(resp.result.is_error());
        assert_eq!(resp.result, error::NOT_FOUND);
    }

    #[test]
    fn test_unhandled_returns_success() {
        let mut svc = FspSrvService::new();
        let cmd = make_command(9999);
        let resp = svc.handle_request(9999, &cmd);
        assert!(resp.result.is_success());
    }
}
