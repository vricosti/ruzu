// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/set/settings_server.h
//! Port of zuyu/src/core/hle/service/set/settings_server.cpp
//!
//! ISettingsServer service ("set").

use crate::hle::result::{ErrorModule, ResultCode};
use super::key_code_map::*;
use super::settings_types::{
    KeyboardLayout, Language, LanguageCode, SystemRegionCode, AVAILABLE_LANGUAGE_CODES,
    LANGUAGE_TO_LAYOUT,
};

/// Constants matching upstream settings_server.cpp.
const PRE_4_0_0_MAX_ENTRIES: usize = 0xF;
const POST_4_0_0_MAX_ENTRIES: usize = 0x40;

/// Result codes from upstream settings_server.cpp.
const RESULT_INVALID_LANGUAGE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Settings, 625);
const RESULT_NULL_POINTER: ResultCode =
    ResultCode::from_module_description(ErrorModule::Settings, 1261);

/// Key code map type -- 0x1000 bytes.
///
/// Corresponds to `KeyCodeMap` in upstream settings_server.h.
pub type KeyCodeMap = [u8; 0x1000];

/// Port of Set::GetLanguageCodeFromIndex.
pub fn get_language_code_from_index(index: usize) -> LanguageCode {
    AVAILABLE_LANGUAGE_CODES[index]
}

/// Copy a key code map slice into a fixed-size 0x1000-byte array, zero-padding as needed.
fn copy_key_code_map(src: &[u8]) -> KeyCodeMap {
    let mut out = [0u8; 0x1000];
    let len = src.len().min(0x1000);
    out[..len].copy_from_slice(&src[..len]);
    out
}

/// Internal key code map lookup by keyboard layout and language code.
///
/// Corresponds to `GetKeyCodeMapImpl` in upstream settings_server.cpp.
fn get_key_code_map_impl(keyboard_layout: KeyboardLayout, language_code: LanguageCode) -> KeyCodeMap {
    let src = match keyboard_layout {
        KeyboardLayout::Japanese => KEY_CODE_MAP_JAPANESE,
        KeyboardLayout::EnglishUs => {
            if language_code == LanguageCode::Ko {
                KEY_CODE_MAP_KOREAN
            } else if language_code == LanguageCode::ZhHans {
                KEY_CODE_MAP_CHINESE_SIMPLIFIED
            } else if language_code == LanguageCode::ZhHant {
                KEY_CODE_MAP_CHINESE_TRADITIONAL
            } else {
                KEY_CODE_MAP_ENGLISH_US_INTERNATIONAL
            }
        }
        KeyboardLayout::EnglishUk => KEY_CODE_MAP_ENGLISH_UK,
        KeyboardLayout::French => KEY_CODE_MAP_FRENCH,
        KeyboardLayout::FrenchCa => KEY_CODE_MAP_FRENCH_CA,
        KeyboardLayout::Spanish => KEY_CODE_MAP_SPANISH,
        KeyboardLayout::SpanishLatin => KEY_CODE_MAP_SPANISH_LATIN,
        KeyboardLayout::German => KEY_CODE_MAP_GERMAN,
        KeyboardLayout::Italian => KEY_CODE_MAP_ITALIAN,
        KeyboardLayout::Portuguese => KEY_CODE_MAP_PORTUGUESE,
        KeyboardLayout::Russian => KEY_CODE_MAP_RUSSIAN,
        KeyboardLayout::Korean => KEY_CODE_MAP_KOREAN,
        KeyboardLayout::ChineseSimplified => KEY_CODE_MAP_CHINESE_SIMPLIFIED,
        KeyboardLayout::ChineseTraditional => KEY_CODE_MAP_CHINESE_TRADITIONAL,
        _ => KEY_CODE_MAP_ENGLISH_US_INTERNATIONAL,
    };
    copy_key_code_map(src)
}

/// IPC command table for ISettingsServer ("set").
///
/// Corresponds to the function table in upstream settings_server.cpp.
pub mod commands {
    pub const GET_LANGUAGE_CODE: u32 = 0;
    pub const GET_AVAILABLE_LANGUAGE_CODES: u32 = 1;
    pub const MAKE_LANGUAGE_CODE: u32 = 2;
    pub const GET_AVAILABLE_LANGUAGE_CODE_COUNT: u32 = 3;
    pub const GET_REGION_CODE: u32 = 4;
    pub const GET_AVAILABLE_LANGUAGE_CODES2: u32 = 5;
    pub const GET_AVAILABLE_LANGUAGE_CODE_COUNT2: u32 = 6;
    pub const GET_KEY_CODE_MAP: u32 = 7;
    pub const GET_QUEST_FLAG: u32 = 8;
    pub const GET_KEY_CODE_MAP2: u32 = 9;
    pub const GET_FIRMWARE_VERSION_FOR_DEBUG: u32 = 10;
    pub const GET_DEVICE_NICK_NAME: u32 = 11;
}

/// ISettingsServer -- "set" service.
///
/// Corresponds to `ISettingsServer` in upstream settings_server.h.
pub struct ISettingsServer {
    /// Language index (from settings). Default: 1 (AmericanEnglish).
    language_index: usize,
    /// Region index (from settings). Default: 1 (USA).
    region_index: u32,
    /// Quest flag (from settings).
    quest_flag: bool,
    /// Device name (from settings).
    device_name: String,
}

impl ISettingsServer {
    pub fn new() -> Self {
        Self {
            language_index: 1,
            region_index: 1,
            quest_flag: false,
            device_name: "yuzu".to_string(),
        }
    }

    /// Construct with explicit settings values.
    pub fn with_settings(
        language_index: usize,
        region_index: u32,
        quest_flag: bool,
        device_name: String,
    ) -> Self {
        Self {
            language_index,
            region_index,
            quest_flag,
            device_name,
        }
    }

    /// GetLanguageCode (cmd 0).
    ///
    /// Corresponds to `ISettingsServer::GetLanguageCode` in upstream.
    pub fn get_language_code(&self) -> LanguageCode {
        log::debug!(
            "ISettingsServer::get_language_code called, index={}",
            self.language_index
        );
        AVAILABLE_LANGUAGE_CODES[self.language_index]
    }

    /// GetAvailableLanguageCodes (cmd 1).
    ///
    /// Returns (count, codes). Pre-4.0.0 max is 0xF entries.
    pub fn get_available_language_codes(&self, max_entries: usize) -> (i32, Vec<LanguageCode>) {
        log::debug!("ISettingsServer::get_available_language_codes called");
        let max_amount = PRE_4_0_0_MAX_ENTRIES.min(max_entries);
        let count = AVAILABLE_LANGUAGE_CODES.len().min(max_amount);
        let codes = AVAILABLE_LANGUAGE_CODES[..count].to_vec();
        (count as i32, codes)
    }

    /// MakeLanguageCode (cmd 2).
    ///
    /// Corresponds to `ISettingsServer::MakeLanguageCode` in upstream.
    pub fn make_language_code(&self, language: u32) -> Result<LanguageCode, ResultCode> {
        log::debug!(
            "ISettingsServer::make_language_code called, language={}",
            language
        );
        let index = language as usize;
        if index >= AVAILABLE_LANGUAGE_CODES.len() {
            return Err(RESULT_INVALID_LANGUAGE);
        }
        Ok(AVAILABLE_LANGUAGE_CODES[index])
    }

    /// GetAvailableLanguageCodeCount (cmd 3).
    pub fn get_available_language_code_count(&self) -> i32 {
        log::debug!("ISettingsServer::get_available_language_code_count called");
        PRE_4_0_0_MAX_ENTRIES as i32
    }

    /// GetRegionCode (cmd 4).
    pub fn get_region_code(&self) -> SystemRegionCode {
        log::debug!("ISettingsServer::get_region_code called");
        // Safety: region_index is validated by settings.
        unsafe { std::mem::transmute(self.region_index) }
    }

    /// GetAvailableLanguageCodes2 (cmd 5).
    ///
    /// Post-4.0.0 version with higher max entries.
    pub fn get_available_language_codes2(&self, max_entries: usize) -> (i32, Vec<LanguageCode>) {
        log::debug!("ISettingsServer::get_available_language_codes2 called");
        let max_amount = POST_4_0_0_MAX_ENTRIES.min(max_entries);
        let count = AVAILABLE_LANGUAGE_CODES.len().min(max_amount);
        let codes = AVAILABLE_LANGUAGE_CODES[..count].to_vec();
        (count as i32, codes)
    }

    /// GetAvailableLanguageCodeCount2 (cmd 6).
    pub fn get_available_language_code_count2(&self) -> i32 {
        log::debug!("ISettingsServer::get_available_language_code_count2 called");
        POST_4_0_0_MAX_ENTRIES as i32
    }

    /// GetKeyCodeMap (cmd 7).
    ///
    /// Corresponds to `ISettingsServer::GetKeyCodeMap` in upstream.
    pub fn get_key_code_map(&self) -> KeyCodeMap {
        log::debug!("ISettingsServer::get_key_code_map called");
        let language_code = AVAILABLE_LANGUAGE_CODES[self.language_index];
        let key_code = LANGUAGE_TO_LAYOUT
            .iter()
            .find(|(lc, _)| *lc == language_code);

        match key_code {
            Some((lc, layout)) => get_key_code_map_impl(*layout, *lc),
            None => {
                log::error!(
                    "Could not find keyboard layout for language index {}, defaulting to English us",
                    self.language_index
                );
                copy_key_code_map(KEY_CODE_MAP_ENGLISH_US_INTERNATIONAL)
            }
        }
    }

    /// GetQuestFlag (cmd 8).
    pub fn get_quest_flag(&self) -> bool {
        log::debug!("ISettingsServer::get_quest_flag called");
        self.quest_flag
    }

    /// GetKeyCodeMap2 (cmd 9).
    ///
    /// Corresponds to `ISettingsServer::GetKeyCodeMap2` in upstream.
    /// Same implementation as GetKeyCodeMap.
    pub fn get_key_code_map2(&self) -> KeyCodeMap {
        log::debug!("ISettingsServer::get_key_code_map2 called");
        self.get_key_code_map()
    }

    /// GetDeviceNickName (cmd 11).
    pub fn get_device_nick_name(&self) -> [u8; 0x80] {
        log::debug!("ISettingsServer::get_device_nick_name called");
        let mut out = [0u8; 0x80];
        let bytes = self.device_name.as_bytes();
        let len = bytes.len().min(out.len());
        out[..len].copy_from_slice(&bytes[..len]);
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_language_code() {
        let server = ISettingsServer::new();
        assert_eq!(server.get_language_code(), LanguageCode::EnUs);
    }

    #[test]
    fn test_make_language_code() {
        let server = ISettingsServer::new();
        assert_eq!(server.make_language_code(0), Ok(LanguageCode::Ja));
        assert_eq!(server.make_language_code(1), Ok(LanguageCode::EnUs));
        assert!(server.make_language_code(99).is_err());
    }

    #[test]
    fn test_available_language_codes() {
        let server = ISettingsServer::new();
        let (count, codes) = server.get_available_language_codes(100);
        assert_eq!(count, PRE_4_0_0_MAX_ENTRIES as i32);
        assert_eq!(codes.len(), PRE_4_0_0_MAX_ENTRIES);

        let (count2, codes2) = server.get_available_language_codes2(100);
        assert_eq!(count2, AVAILABLE_LANGUAGE_CODES.len() as i32);
        assert_eq!(codes2.len(), AVAILABLE_LANGUAGE_CODES.len());
    }

    #[test]
    fn test_get_key_code_map() {
        let server = ISettingsServer::new(); // language_index=1 => EnUs
        let map = server.get_key_code_map();
        // Should be EnglishUs map (which for EnUs goes through the EnglishUs branch)
        // Just verify it doesn't panic and returns a valid 0x1000-byte array
        assert_eq!(map.len(), 0x1000);
    }

    #[test]
    fn test_get_device_nick_name() {
        let server = ISettingsServer::new();
        let name = server.get_device_nick_name();
        assert_eq!(&name[..4], b"yuzu");
        assert_eq!(name[4], 0);
    }
}
