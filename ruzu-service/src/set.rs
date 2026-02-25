// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `set:sys` -- System Settings service stub.
//! `set`     -- Plain Settings service stub.
//!
//! Commands (set:sys):
//!   0 = GetFirmwareVersion
//!
//! Commands (set):
//!   1 = GetAvailableLanguageCodes
//!   2 = MakeLanguageCode
//!   3 = GetAvailableLanguageCodeCount
//!   4 = GetRegionCode
//!   5 = GetAvailableLanguageCodes2
//!   6 = GetAvailableLanguageCodeCount2
//!   7 = GetLanguageCode

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
            // Per the Switch IPC ABI the caller provides a B-type output
            // buffer (0x100 bytes) and the service writes the firmware
            // version struct into it.  The inline response carries only
            // the result code.
            0 => {
                log::info!("set:sys: GetFirmwareVersion -> 17.0.0");

                // Build the 0x100-byte firmware version struct.
                //   0x00: u8  major
                //   0x01: u8  minor
                //   0x02: u8  micro
                //   0x04: u8  revision_major
                //   0x08: char[0x20] platform  ("NX")
                //   0x28: char[0x40] version_hash (left as zeros)
                //   0x68: char[0x18] display_version ("17.0.0")
                //   0x80: char[0x80] display_title
                let mut version_bytes = vec![0u8; 0x100];
                version_bytes[0x00] = 17; // major
                version_bytes[0x01] = 0;  // minor
                version_bytes[0x02] = 0;  // micro

                version_bytes[0x08] = b'N';
                version_bytes[0x09] = b'X';

                let display = b"17.0.0";
                version_bytes[0x68..0x68 + display.len()].copy_from_slice(display);

                let title = b"NintendoSDK Firmware for NX 17.0.0";
                let title_end = (0x80 + title.len()).min(0x100);
                version_bytes[0x80..title_end].copy_from_slice(&title[..title_end - 0x80]);

                // Write the struct via B-buffer; the inline payload is empty.
                IpcResponse::success().with_out_buf(version_bytes)
            }

            // ── Everything else: stub success ────────────────────────────
            _ => {
                log::warn!("set:sys: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── Plain Settings: set ──────────────────────────────────────────────────────

pub struct SetService;

impl SetService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for SetService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for SetService {
    fn service_name(&self) -> &str {
        "set"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("set: cmd_id={}", cmd_id);
        match cmd_id {
            // GetAvailableLanguageCodes / GetAvailableLanguageCodes2
            // Output: [out] LanguageCode[] via B-buffer, [out] u32 total_entries (inline)
            1 | 5 => {
                log::info!("set: GetAvailableLanguageCodes");
                // AmericanEnglish LanguageCode = "en-US\0\0\0" as little-endian u64.
                let lang_code: u64 = u64::from_le_bytes(*b"en-US\0\0\0");
                let lang_bytes = lang_code.to_le_bytes().to_vec();
                IpcResponse::success_with_data(vec![1]) // 1 language code written
                    .with_out_buf(lang_bytes)
            }
            // MakeLanguageCode
            2 => {
                log::info!("set: MakeLanguageCode");
                IpcResponse::success_with_data(vec![0])
            }
            // GetAvailableLanguageCodeCount / GetAvailableLanguageCodeCount2
            3 | 6 => {
                log::info!("set: GetAvailableLanguageCodeCount");
                IpcResponse::success_with_data(vec![1])
            }
            // GetRegionCode — USA (1)
            4 => {
                log::info!("set: GetRegionCode (USA)");
                IpcResponse::success_with_data(vec![1])
            }
            // GetLanguageCode → "en-US" as u64 LE
            7 => {
                log::info!("set: GetLanguageCode");
                let lang_code: u64 = u64::from_le_bytes(*b"en-US\0\0\0");
                IpcResponse::success_with_data(vec![
                    lang_code as u32,
                    (lang_code >> 32) as u32,
                ])
            }
            _ => {
                log::warn!("set: unhandled cmd_id={}", cmd_id);
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
        }
    }

    #[test]
    fn test_get_firmware_version() {
        let mut svc = SetSysService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        // Inline data is empty — struct goes via B-buffer.
        assert!(resp.data.is_empty());
        // out_bufs[0] is the 0x100-byte firmware version struct.
        assert_eq!(resp.out_bufs.len(), 1);
        let bytes = &resp.out_bufs[0];
        assert_eq!(bytes.len(), 0x100);
        assert_eq!(bytes[0x00], 17); // major
        assert_eq!(bytes[0x01], 0);  // minor
        assert_eq!(bytes[0x02], 0);  // micro
        assert_eq!(&bytes[0x68..0x68 + 6], b"17.0.0");
    }

    #[test]
    fn test_unhandled_returns_success() {
        let mut svc = SetSysService::new();
        let cmd = make_command(99);
        let resp = svc.handle_request(99, &cmd);
        assert!(resp.result.is_success());
    }

    #[test]
    fn test_set_get_available_language_codes() {
        let mut svc = SetService::new();
        let cmd = make_command(1);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
        // Inline data contains count=1.
        assert_eq!(resp.data, vec![1]);
        // out_bufs[0] contains "en-US\0\0\0" as bytes.
        assert_eq!(resp.out_bufs.len(), 1);
        assert_eq!(&resp.out_bufs[0], b"en-US\0\0\0");
    }

    #[test]
    fn test_set_get_language_code_count() {
        let mut svc = SetService::new();
        let cmd = make_command(3);
        let resp = svc.handle_request(3, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![1]);
    }

    #[test]
    fn test_set_get_region_code() {
        let mut svc = SetService::new();
        let cmd = make_command(4);
        let resp = svc.handle_request(4, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![1]); // USA
    }

    #[test]
    fn test_set_get_language_code() {
        let mut svc = SetService::new();
        let cmd = make_command(7);
        let resp = svc.handle_request(7, &cmd);
        assert!(resp.result.is_success());
        // Inline data: two u32 words encoding "en-US\0\0\0" as little-endian u64.
        assert_eq!(resp.data.len(), 2);
        let lo = resp.data[0] as u64;
        let hi = resp.data[1] as u64;
        let code = lo | (hi << 32);
        assert_eq!(code, u64::from_le_bytes(*b"en-US\0\0\0"));
    }
}
