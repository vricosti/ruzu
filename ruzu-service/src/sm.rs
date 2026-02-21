// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `sm:` -- Service Manager service.
//!
//! The Switch's service manager is the first IPC service a process connects to.
//! It provides four commands:
//!   0 = Initialize
//!   1 = GetService
//!   2 = RegisterService
//!   3 = UnregisterService

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

/// HLE implementation of `sm:`.
pub struct SmService {
    initialized: bool,
}

impl SmService {
    pub fn new() -> Self {
        Self { initialized: false }
    }
}

impl Default for SmService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for SmService {
    fn service_name(&self) -> &str {
        "sm:"
    }

    fn handle_request(&mut self, cmd_id: u32, command: &IpcCommand) -> IpcResponse {
        log::debug!("sm: cmd_id={}", cmd_id);

        match cmd_id {
            // ── Initialize ───────────────────────────────────────────────
            0 => {
                log::info!("sm: Initialize");
                self.initialized = true;
                IpcResponse::success()
            }

            // ── GetService ───────────────────────────────────────────────
            1 => {
                // The service name is encoded as 8 bytes (two u32 words) in
                // the raw data section.  Trim trailing NULs.
                let name = read_service_name(&command.raw_data);
                log::info!("sm: GetService(\"{}\")", name);

                // The actual handle creation is performed by the SVC layer.
                // We just acknowledge the request here.
                IpcResponse::success()
            }

            // ── RegisterService ──────────────────────────────────────────
            2 => {
                let name = read_service_name(&command.raw_data);
                log::info!("sm: RegisterService(\"{}\")", name);
                IpcResponse::success()
            }

            // ── UnregisterService ────────────────────────────────────────
            3 => {
                let name = read_service_name(&command.raw_data);
                log::info!("sm: UnregisterService(\"{}\")", name);
                IpcResponse::success()
            }

            _ => {
                log::warn!("sm: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

/// Read a service name (up to 8 bytes) from the first two raw data words.
fn read_service_name(raw_data: &[u32]) -> String {
    let mut bytes = [0u8; 8];

    if let Some(&w0) = raw_data.first() {
        bytes[0..4].copy_from_slice(&w0.to_le_bytes());
    }
    if let Some(&w1) = raw_data.get(1) {
        bytes[4..8].copy_from_slice(&w1.to_le_bytes());
    }

    // Trim trailing NUL bytes.
    let len = bytes.iter().position(|&b| b == 0).unwrap_or(8);
    String::from_utf8_lossy(&bytes[..len]).into_owned()
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
    fn test_initialize() {
        let mut svc = SmService::new();
        let cmd = make_command(0, vec![]);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert!(svc.initialized);
    }

    #[test]
    fn test_get_service_name_parsing() {
        // "fsp-srv\0" encoded as two little-endian u32s.
        let w0 = u32::from_le_bytes([b'f', b's', b'p', b'-']);
        let w1 = u32::from_le_bytes([b's', b'r', b'v', 0]);
        let name = read_service_name(&[w0, w1]);
        assert_eq!(name, "fsp-srv");
    }

    #[test]
    fn test_get_service() {
        let mut svc = SmService::new();
        let w0 = u32::from_le_bytes([b'h', b'i', b'd', 0]);
        let cmd = make_command(1, vec![w0, 0]);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
    }
}
