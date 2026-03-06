// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/set/ (settings services)
//! Status: EN COURS
//! Derniere synchro: 2026-03-05
//!
//! Services:
//!   `set`     — ISettingsServer (language, region, keyboard layout)
//!   `set:sys` — ISystemSettingsServer (firmware version, color set, account, etc.)
//!   `set:cal` — IFactorySettingsServer (factory calibration data)
//!   `set:fd`  — IFirmwareDebugSettingsServer

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

// ── Language codes (from settings_types.h / settings_server.h) ──────────────

/// nn::settings::LanguageCode values (8-byte null-padded strings as u64 LE).
/// These match zuyu's available_language_codes array.
const AVAILABLE_LANGUAGE_CODES: &[u64] = &[
    u64::from_le_bytes(*b"ja\0\0\0\0\0\0"), // Japanese
    u64::from_le_bytes(*b"en-US\0\0\0"),    // AmericanEnglish
    u64::from_le_bytes(*b"fr\0\0\0\0\0\0"), // French
    u64::from_le_bytes(*b"de\0\0\0\0\0\0"), // German
    u64::from_le_bytes(*b"it\0\0\0\0\0\0"), // Italian
    u64::from_le_bytes(*b"es\0\0\0\0\0\0"), // Spanish
    u64::from_le_bytes(*b"zh-CN\0\0\0"),    // Chinese
    u64::from_le_bytes(*b"ko\0\0\0\0\0\0"), // Korean
    u64::from_le_bytes(*b"nl\0\0\0\0\0\0"), // Dutch
    u64::from_le_bytes(*b"pt\0\0\0\0\0\0"), // Portuguese
    u64::from_le_bytes(*b"ru\0\0\0\0\0\0"), // Russian
    u64::from_le_bytes(*b"zh-TW\0\0\0"),    // Taiwanese
    u64::from_le_bytes(*b"en-GB\0\0\0"),    // BritishEnglish
    u64::from_le_bytes(*b"fr-CA\0\0\0"),    // CanadianFrench
    u64::from_le_bytes(*b"es-419\0\0"),     // LatinAmericanSpanish
    u64::from_le_bytes(*b"zh-Hans\0"),      // SimplifiedChinese
    u64::from_le_bytes(*b"zh-Hant\0"),      // TraditionalChinese
    u64::from_le_bytes(*b"pt-BR\0\0\0"),    // BrazilianPortuguese
];

/// Pre-4.0.0 max entries for GetAvailableLanguageCodes
const PRE_4_0_0_MAX_ENTRIES: usize = 0xF;
/// Post-4.0.0 max entries for GetAvailableLanguageCodes2
const POST_4_0_0_MAX_ENTRIES: usize = 0x40;

/// Default language index (AmericanEnglish = 1)
const DEFAULT_LANGUAGE_INDEX: usize = 1;

/// nn::settings::system::SystemRegionCode
#[repr(u32)]
#[derive(Debug, Clone, Copy)]
pub enum SystemRegionCode {
    Japan = 0,
    Usa = 1,
    Europe = 2,
    Australia = 3,
    China = 4,
    Korea = 5,
    Taiwan = 6,
}

/// nn::settings::system::PlatformRegion
#[repr(u32)]
#[derive(Debug, Clone, Copy)]
pub enum PlatformRegion {
    Global = 1,
    China = 2,
}

// ── set:sys (ISystemSettingsServer) ─────────────────────────────────────────

/// HLE implementation of `set:sys`.
pub struct SetSysService {
    /// System color set (0 = BasicWhite, 1 = BasicBlack).
    color_set_id: u32,
}

impl SetSysService {
    pub fn new() -> Self {
        Self { color_set_id: 1 } // BasicBlack default
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
            // SetLanguageCode (0)
            0 => {
                log::info!("set:sys: SetLanguageCode");
                IpcResponse::success()
            }

            // GetFirmwareVersion (3)
            3 => {
                log::info!("set:sys: GetFirmwareVersion -> 17.0.0");
                IpcResponse::success().with_out_buf(build_firmware_version_struct())
            }

            // GetFirmwareVersion2 (4) — same but with revision_minor preserved
            4 => {
                log::info!("set:sys: GetFirmwareVersion2 -> 17.0.0");
                IpcResponse::success().with_out_buf(build_firmware_version_struct())
            }

            // GetColorSetId (23)
            23 => {
                log::info!("set:sys: GetColorSetId -> {}", self.color_set_id);
                IpcResponse::success_with_data(vec![self.color_set_id])
            }

            // SetColorSetId (24)
            24 => {
                let id = _command.raw_data.first().copied().unwrap_or(0);
                self.color_set_id = id;
                log::info!("set:sys: SetColorSetId({})", id);
                IpcResponse::success()
            }

            // GetSettingsItemValueSize (37) — return a default size
            37 => {
                log::info!("set:sys: GetSettingsItemValueSize");
                IpcResponse::success_with_data(vec![0, 0])
            }

            // GetSettingsItemValue (38) — return empty
            38 => {
                log::info!("set:sys: GetSettingsItemValue");
                IpcResponse::success_with_data(vec![0])
            }

            // GetAccountSettings (47) — flags = 0
            47 => {
                log::info!("set:sys: GetAccountSettings -> 0");
                IpcResponse::success_with_data(vec![0])
            }

            // GetEulaVersions (48) — 0 versions
            48 => {
                log::info!("set:sys: GetEulaVersions -> 0");
                IpcResponse::success_with_data(vec![0])
            }

            // GetDeviceNickName (77) — "ruzu" via B-buffer
            77 => {
                log::info!("set:sys: GetDeviceNickName");
                let mut name = vec![0u8; 0x80];
                let device_name = b"ruzu";
                name[..device_name.len()].copy_from_slice(device_name);
                IpcResponse::success().with_out_buf(name)
            }

            // SetDeviceNickName (78)
            78 => {
                log::info!("set:sys: SetDeviceNickName");
                IpcResponse::success()
            }

            // GetAutoUpdateEnableFlag (95)
            95 => {
                log::info!("set:sys: GetAutoUpdateEnableFlag -> true");
                IpcResponse::success_with_data(vec![1])
            }

            // GetPrimaryAlbumStorage (100) — Nand (0)
            100 => {
                log::info!("set:sys: GetPrimaryAlbumStorage -> Nand");
                IpcResponse::success_with_data(vec![0])
            }

            // GetSleepSettings (131)
            131 => {
                log::info!("set:sys: GetSleepSettings");
                // Return zeros (default sleep settings struct)
                IpcResponse::success_with_data(vec![0; 8])
            }

            // GetFieldTestingFlag (203)
            203 => {
                log::info!("set:sys: GetFieldTestingFlag -> false");
                IpcResponse::success_with_data(vec![0])
            }

            _ => {
                log::warn!("set:sys: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

/// Build the 0x100-byte FirmwareVersionFormat struct.
fn build_firmware_version_struct() -> Vec<u8> {
    let mut version_bytes = vec![0u8; 0x100];
    version_bytes[0x00] = 17; // major
    version_bytes[0x01] = 0; // minor
    version_bytes[0x02] = 0; // micro

    version_bytes[0x08] = b'N';
    version_bytes[0x09] = b'X';

    let display = b"17.0.0";
    version_bytes[0x68..0x68 + display.len()].copy_from_slice(display);

    let title = b"NintendoSDK Firmware for NX 17.0.0";
    let title_end = (0x80 + title.len()).min(0x100);
    version_bytes[0x80..title_end].copy_from_slice(&title[..title_end - 0x80]);

    version_bytes
}

// ── set (ISettingsServer) ───────────────────────────────────────────────────

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
            // GetLanguageCode (0)
            0 => {
                let lang = AVAILABLE_LANGUAGE_CODES[DEFAULT_LANGUAGE_INDEX];
                log::info!("set: GetLanguageCode -> 0x{:016X}", lang);
                IpcResponse::success_with_data(vec![lang as u32, (lang >> 32) as u32])
            }

            // GetAvailableLanguageCodes (1) — pre-4.0.0 (pointer buffer)
            1 => {
                let count = AVAILABLE_LANGUAGE_CODES.len().min(PRE_4_0_0_MAX_ENTRIES);
                log::info!("set: GetAvailableLanguageCodes -> {} codes", count);
                let mut buf = Vec::new();
                for &code in &AVAILABLE_LANGUAGE_CODES[..count] {
                    buf.extend_from_slice(&code.to_le_bytes());
                }
                IpcResponse::success_with_data(vec![count as u32]).with_out_buf(buf)
            }

            // MakeLanguageCode (2)
            2 => {
                let idx = _command.raw_data.first().copied().unwrap_or(0) as usize;
                if idx < AVAILABLE_LANGUAGE_CODES.len() {
                    let code = AVAILABLE_LANGUAGE_CODES[idx];
                    log::info!("set: MakeLanguageCode({}) -> 0x{:016X}", idx, code);
                    IpcResponse::success_with_data(vec![code as u32, (code >> 32) as u32])
                } else {
                    log::error!("set: MakeLanguageCode({}) out of range", idx);
                    IpcResponse::success_with_data(vec![0, 0])
                }
            }

            // GetAvailableLanguageCodeCount (3) — pre-4.0.0
            3 => {
                let count = AVAILABLE_LANGUAGE_CODES.len().min(PRE_4_0_0_MAX_ENTRIES);
                log::info!("set: GetAvailableLanguageCodeCount -> {}", count);
                IpcResponse::success_with_data(vec![count as u32])
            }

            // GetRegionCode (4) — USA (1)
            4 => {
                log::info!("set: GetRegionCode (USA)");
                IpcResponse::success_with_data(vec![SystemRegionCode::Usa as u32])
            }

            // GetAvailableLanguageCodes2 (5) — post-4.0.0
            5 => {
                let count = AVAILABLE_LANGUAGE_CODES.len().min(POST_4_0_0_MAX_ENTRIES);
                log::info!("set: GetAvailableLanguageCodes2 -> {} codes", count);
                let mut buf = Vec::new();
                for &code in &AVAILABLE_LANGUAGE_CODES[..count] {
                    buf.extend_from_slice(&code.to_le_bytes());
                }
                IpcResponse::success_with_data(vec![count as u32]).with_out_buf(buf)
            }

            // GetAvailableLanguageCodeCount2 (6) — post-4.0.0
            6 => {
                let count = AVAILABLE_LANGUAGE_CODES.len().min(POST_4_0_0_MAX_ENTRIES);
                log::info!("set: GetAvailableLanguageCodeCount2 -> {}", count);
                IpcResponse::success_with_data(vec![count as u32])
            }

            // GetKeyCodeMap (7) — return 0x1000-byte keyboard layout via B-buffer
            7 => {
                log::info!("set: GetKeyCodeMap");
                let buf = vec![0u8; 0x1000]; // English US International default (zeros = passthrough)
                IpcResponse::success().with_out_buf(buf)
            }

            // GetQuestFlag (8) — false
            8 => {
                log::info!("set: GetQuestFlag -> false");
                IpcResponse::success_with_data(vec![0])
            }

            // GetKeyCodeMap2 (9) — same as GetKeyCodeMap
            9 => {
                log::info!("set: GetKeyCodeMap2");
                let buf = vec![0u8; 0x1000];
                IpcResponse::success().with_out_buf(buf)
            }

            // GetDeviceNickName (11) — "ruzu" via B-buffer
            11 => {
                log::info!("set: GetDeviceNickName");
                let mut name = vec![0u8; 0x80];
                let device_name = b"ruzu";
                name[..device_name.len()].copy_from_slice(device_name);
                IpcResponse::success().with_out_buf(name)
            }

            _ => {
                log::warn!("set: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── set:cal (IFactorySettingsServer) stub ────────────────────────────────────

pub struct SetCalService;

impl SetCalService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for SetCalService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for SetCalService {
    fn service_name(&self) -> &str {
        "set:cal"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("set:cal: cmd_id={}", cmd_id);
        match cmd_id {
            // GetBdAddress (0) — return dummy MAC address
            0 => {
                log::info!("set:cal: GetBdAddress");
                // 6 bytes BD address padded to 8 (two u32 words)
                IpcResponse::success_with_data(vec![0x00112233, 0x00004455])
            }
            // GetWirelessLanMacAddress (6)
            6 => {
                log::info!("set:cal: GetWirelessLanMacAddress");
                IpcResponse::success_with_data(vec![0xAABBCCDD, 0x0000EEFF])
            }
            _ => {
                log::warn!("set:cal: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── set:fd (IFirmwareDebugSettingsServer) stub ──────────────────────────────

pub struct SetFdService;

impl SetFdService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for SetFdService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for SetFdService {
    fn service_name(&self) -> &str {
        "set:fd"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("set:fd: cmd_id={}", cmd_id);
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
            b_buf_addrs: Vec::new(),
            x_bufs: Vec::new(),
            a_bufs: Vec::new(),
            a_buf_data: Vec::new(),
            b_buf_sizes: Vec::new(),
        }
    }

    #[test]
    fn test_get_firmware_version() {
        let mut svc = SetSysService::new();
        let cmd = make_command(3);
        let resp = svc.handle_request(3, &cmd);
        assert!(resp.result.is_success());
        assert!(resp.data.is_empty());
        assert_eq!(resp.out_bufs.len(), 1);
        let bytes = &resp.out_bufs[0];
        assert_eq!(bytes.len(), 0x100);
        assert_eq!(bytes[0x00], 17); // major
        assert_eq!(bytes[0x01], 0); // minor
        assert_eq!(bytes[0x02], 0); // micro
        assert_eq!(&bytes[0x68..0x68 + 6], b"17.0.0");
    }

    #[test]
    fn test_get_color_set_id() {
        let mut svc = SetSysService::new();
        let cmd = make_command(23);
        let resp = svc.handle_request(23, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![1]); // BasicBlack
    }

    #[test]
    fn test_set_get_available_language_codes() {
        let mut svc = SetService::new();
        let cmd = make_command(1);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
        assert!(resp.data[0] > 0); // count > 0
        assert_eq!(resp.out_bufs.len(), 1);
    }

    #[test]
    fn test_set_get_language_code_count() {
        let mut svc = SetService::new();
        let cmd = make_command(3);
        let resp = svc.handle_request(3, &cmd);
        assert!(resp.result.is_success());
        assert!(resp.data[0] > 0);
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
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data.len(), 2);
        let lo = resp.data[0] as u64;
        let hi = resp.data[1] as u64;
        let code = lo | (hi << 32);
        assert_eq!(code, u64::from_le_bytes(*b"en-US\0\0\0"));
    }

    #[test]
    fn test_set_get_available_language_codes2() {
        let mut svc = SetService::new();
        let cmd = make_command(5);
        let resp = svc.handle_request(5, &cmd);
        assert!(resp.result.is_success());
        assert!(resp.data[0] > 0);
        assert_eq!(resp.out_bufs.len(), 1);
    }

    #[test]
    fn test_set_get_quest_flag() {
        let mut svc = SetService::new();
        let cmd = make_command(8);
        let resp = svc.handle_request(8, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![0]);
    }
}
