// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/set/settings_server.h
//! Port of zuyu/src/core/hle/service/set/settings_server.cpp
//!
//! ISettingsServer service ("set").

use crate::hle::result::{ErrorModule, ResultCode};
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

/// Key code map type — 0x1000 bytes.
///
/// Corresponds to `KeyCodeMap` in upstream settings_server.h.
pub type KeyCodeMap = [u8; 0x1000];

/// Port of Set::GetLanguageCodeFromIndex.
pub fn get_language_code_from_index(index: usize) -> LanguageCode {
    AVAILABLE_LANGUAGE_CODES[index]
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

/// ISettingsServer — "set" service.
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

    /// GetQuestFlag (cmd 8).
    pub fn get_quest_flag(&self) -> bool {
        log::debug!("ISettingsServer::get_quest_flag called");
        self.quest_flag
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
