// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `lm` -- Log Manager service.
//!
//! Root interface commands:
//!   0 = OpenLogger
//!
//! Logger sub-interface commands:
//!   0 = Log (read log data and print to host)

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

// ── Root interface: lm ───────────────────────────────────────────────────────

/// HLE stub for the `lm` (Log Manager) root interface.
pub struct LmService;

impl LmService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for LmService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for LmService {
    fn service_name(&self) -> &str {
        "lm"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("lm: cmd_id={}", cmd_id);

        match cmd_id {
            // ── OpenLogger ───────────────────────────────────────────────
            0 => {
                log::info!("lm: OpenLogger");
                IpcResponse::success()
            }

            _ => {
                log::warn!("lm: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── Logger sub-interface ─────────────────────────────────────────────────────

/// HLE stub for the logger object returned by `lm::OpenLogger`.
pub struct LoggerService;

impl LoggerService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for LoggerService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for LoggerService {
    fn service_name(&self) -> &str {
        "ILogger"
    }

    fn handle_request(&mut self, cmd_id: u32, command: &IpcCommand) -> IpcResponse {
        log::debug!("ILogger: cmd_id={}", cmd_id);

        match cmd_id {
            // ── Log ──────────────────────────────────────────────────────
            0 => {
                // The log payload is in the raw data words.  Convert to
                // bytes and decode as UTF-8 (lossy) for the host log.
                let bytes: Vec<u8> = command
                    .raw_data
                    .iter()
                    .flat_map(|w| w.to_le_bytes())
                    .collect();

                // Trim trailing NUL bytes.
                let end = bytes.iter().rposition(|&b| b != 0).map_or(0, |i| i + 1);
                let message = String::from_utf8_lossy(&bytes[..end]);

                log::info!("[Guest Log] {}", message);
                IpcResponse::success()
            }

            // ── SetDestination (1) ───────────────────────────────────────
            1 => {
                log::info!("ILogger: SetDestination");
                IpcResponse::success()
            }

            _ => {
                log::warn!("ILogger: unhandled cmd_id={}", cmd_id);
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

    fn make_command(cmd_id: u32, raw_data: Vec<u32>) -> IpcCommand {
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
            raw_data,
        }
    }

    #[test]
    fn test_open_logger() {
        let mut svc = LmService::new();
        let cmd = make_command(0, vec![]);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
    }

    #[test]
    fn test_logger_log() {
        let mut logger = LoggerService::new();
        // Encode "Hello" as raw data words.
        let w0 = u32::from_le_bytes([b'H', b'e', b'l', b'l']);
        let w1 = u32::from_le_bytes([b'o', 0, 0, 0]);
        let cmd = make_command(0, vec![w0, w1]);
        let resp = logger.handle_request(0, &cmd);
        assert!(resp.result.is_success());
    }

    #[test]
    fn test_logger_set_destination() {
        let mut logger = LoggerService::new();
        let cmd = make_command(1, vec![]);
        let resp = logger.handle_request(1, &cmd);
        assert!(resp.result.is_success());
    }
}
