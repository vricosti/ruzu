// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `set:sys` -- System Settings service stub.
//!
//! Commands:
//!   0 = GetFirmwareVersion

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

/// HLE stub for `set:sys`.
pub struct SetSysService;

impl SetSysService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for SetSysService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for SetSysService {
    fn service_name(&self) -> &str {
        "set:sys"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("set:sys: cmd_id={}", cmd_id);

        match cmd_id {
            // ── GetFirmwareVersion ───────────────────────────────────────
            0 => {
                log::info!("set:sys: GetFirmwareVersion -> 17.0.0");

                // The firmware version struct is 0x100 bytes.  The fields
                // that games actually check are:
                //   offset 0x00: u8 major
                //   offset 0x01: u8 minor
                //   offset 0x02: u8 micro
                //   offset 0x04: u8 revision_major
                //   offset 0x08: char[0x20] platform  ("NX")
                //   offset 0x28: char[0x40] version_hash
                //   offset 0x68: char[0x18] display_version ("17.0.0")
                //   offset 0x80: char[0x80] display_title ("NintendoSDK ...")
                //
                // We return the important fields packed into u32 words.
                let mut version_bytes = [0u8; 0x100];

                // Major / minor / micro.
                version_bytes[0x00] = 17; // major
                version_bytes[0x01] = 0; // minor
                version_bytes[0x02] = 0; // micro

                // Platform string "NX".
                version_bytes[0x08] = b'N';
                version_bytes[0x09] = b'X';

                // Display version "17.0.0".
                let display = b"17.0.0";
                version_bytes[0x68..0x68 + display.len()].copy_from_slice(display);

                // Display title.
                let title = b"NintendoSDK Firmware for NX 17.0.0";
                let title_end = (0x80 + title.len()).min(0x100);
                version_bytes[0x80..title_end]
                    .copy_from_slice(&title[..title_end - 0x80]);

                // Convert to u32 words for the response data.
                let words: Vec<u32> = version_bytes
                    .chunks_exact(4)
                    .map(|c| u32::from_le_bytes([c[0], c[1], c[2], c[3]]))
                    .collect();

                IpcResponse::success_with_data(words)
            }

            // ── Everything else: stub success ────────────────────────────
            _ => {
                log::warn!("set:sys: unhandled cmd_id={}", cmd_id);
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
    fn test_get_firmware_version() {
        let mut svc = SetSysService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert!(!resp.data.is_empty());

        // Reconstruct bytes to verify major version.
        let first_word = resp.data[0];
        let bytes = first_word.to_le_bytes();
        assert_eq!(bytes[0], 17); // major
        assert_eq!(bytes[1], 0); // minor
        assert_eq!(bytes[2], 0); // micro
    }

    #[test]
    fn test_unhandled_returns_success() {
        let mut svc = SetSysService::new();
        let cmd = make_command(99);
        let resp = svc.handle_request(99, &cmd);
        assert!(resp.result.is_success());
    }
}
