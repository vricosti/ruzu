//! Port of zuyu/src/core/hle/service/set/settings_server.h and settings_server.cpp
//!
//! ISettingsServer service ("set").

use super::settings_types::LanguageCode;

/// Constants matching upstream
const PRE_4_0_0_MAX_ENTRIES: usize = 0xF;
const POST_4_0_0_MAX_ENTRIES: usize = 0x40;

/// Port of Set::GetLanguageCodeFromIndex
pub fn get_language_code_from_index(index: usize) -> LanguageCode {
    super::settings_types::AVAILABLE_LANGUAGE_CODES[index]
}

/// IPC command table for ISettingsServer ("set"):
///
/// | Cmd | Name                          |
/// |-----|-------------------------------|
/// | 0   | GetLanguageCode               |
/// | 1   | GetAvailableLanguageCodes     |
/// | 2   | MakeLanguageCode              |
/// | 3   | GetAvailableLanguageCodeCount |
/// | 4   | GetRegionCode                 |
/// | 5   | GetAvailableLanguageCodes2    |
/// | 6   | GetAvailableLanguageCodeCount2|
/// | 7   | GetKeyCodeMap                 |
/// | 8   | GetQuestFlag                  |
/// | 9   | GetKeyCodeMap2                |
/// | 10  | GetFirmwareVersionForDebug    |
/// | 11  | GetDeviceNickName             |
pub struct ISettingsServer {
    _pre_max: usize,
    _post_max: usize,
}

impl ISettingsServer {
    pub fn new() -> Self {
        Self {
            _pre_max: PRE_4_0_0_MAX_ENTRIES,
            _post_max: POST_4_0_0_MAX_ENTRIES,
        }
    }
}
