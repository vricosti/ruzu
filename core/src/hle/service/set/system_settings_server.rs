// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/set/system_settings_server.h and system_settings_server.cpp
//!
//! ISystemSettingsServer service ("set:sys").

use std::collections::BTreeMap;
use std::sync::Mutex;

use crate::hle::result::{ErrorModule, ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::settings_types::*;

/// Settings file version, matching upstream SETTINGS_VERSION.
const SETTINGS_VERSION: u32 = 4;

/// Settings magic bytes, matching upstream SETTINGS_MAGIC.
const SETTINGS_MAGIC: u64 = u64::from_le_bytes([b'y', b'u', b'z', b'u', b'_', b's', b'e', b't']);

/// Settings file header.
///
/// Corresponds to `SettingsHeader` in upstream system_settings_server.cpp.
#[repr(C)]
pub struct SettingsHeader {
    pub magic: u64,
    pub version: u32,
    pub reserved: u32,
}

/// IPC command IDs for ISystemSettingsServer ("set:sys").
/// Matches upstream system_settings_server.cpp lines 94-303.
pub mod commands {
    pub const SET_LANGUAGE_CODE: u32 = 0;
    pub const GET_FIRMWARE_VERSION: u32 = 3;
    pub const GET_FIRMWARE_VERSION2: u32 = 4;
    pub const GET_LOCK_SCREEN_FLAG: u32 = 7;
    pub const SET_LOCK_SCREEN_FLAG: u32 = 8;
    pub const GET_EXTERNAL_STEADY_CLOCK_SOURCE_ID: u32 = 13;
    pub const SET_EXTERNAL_STEADY_CLOCK_SOURCE_ID: u32 = 14;
    pub const GET_USER_SYSTEM_CLOCK_CONTEXT: u32 = 15;
    pub const SET_USER_SYSTEM_CLOCK_CONTEXT: u32 = 16;
    pub const GET_ACCOUNT_SETTINGS: u32 = 17;
    pub const SET_ACCOUNT_SETTINGS: u32 = 18;
    pub const GET_EULA_VERSIONS: u32 = 21;
    pub const SET_EULA_VERSIONS: u32 = 22;
    pub const GET_COLOR_SET_ID: u32 = 23;
    pub const SET_COLOR_SET_ID: u32 = 24;
    pub const GET_NOTIFICATION_SETTINGS: u32 = 29;
    pub const SET_NOTIFICATION_SETTINGS: u32 = 30;
    pub const GET_ACCOUNT_NOTIFICATION_SETTINGS: u32 = 31;
    pub const SET_ACCOUNT_NOTIFICATION_SETTINGS: u32 = 32;
    pub const GET_VIBRATION_MASTER_VOLUME: u32 = 35;
    pub const SET_VIBRATION_MASTER_VOLUME: u32 = 36;
    pub const GET_SETTINGS_ITEM_VALUE_SIZE: u32 = 37;
    pub const GET_SETTINGS_ITEM_VALUE: u32 = 38;
    pub const GET_TV_SETTINGS: u32 = 39;
    pub const SET_TV_SETTINGS: u32 = 40;
    pub const GET_AUDIO_OUTPUT_MODE: u32 = 43;
    pub const SET_AUDIO_OUTPUT_MODE: u32 = 44;
    pub const GET_SPEAKER_AUTO_MUTE_FLAG: u32 = 45;
    pub const SET_SPEAKER_AUTO_MUTE_FLAG: u32 = 46;
    pub const GET_QUEST_FLAG: u32 = 47;
    pub const SET_QUEST_FLAG: u32 = 48;
    pub const GET_DEVICE_TIME_ZONE_LOCATION_NAME: u32 = 53;
    pub const SET_DEVICE_TIME_ZONE_LOCATION_NAME: u32 = 54;
    pub const SET_REGION_CODE: u32 = 57;
    pub const GET_NETWORK_SYSTEM_CLOCK_CONTEXT: u32 = 58;
    pub const SET_NETWORK_SYSTEM_CLOCK_CONTEXT: u32 = 59;
    pub const IS_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_ENABLED: u32 = 60;
    pub const SET_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_ENABLED: u32 = 61;
    pub const GET_DEBUG_MODE_FLAG: u32 = 62;
    pub const GET_PRIMARY_ALBUM_STORAGE: u32 = 63;
    pub const SET_PRIMARY_ALBUM_STORAGE: u32 = 64;
    pub const GET_BATTERY_LOT: u32 = 67;
    pub const GET_SERIAL_NUMBER: u32 = 68;
    pub const GET_NFC_ENABLE_FLAG: u32 = 69;
    pub const SET_NFC_ENABLE_FLAG: u32 = 70;
    pub const GET_SLEEP_SETTINGS: u32 = 71;
    pub const SET_SLEEP_SETTINGS: u32 = 72;
    pub const GET_WIRELESS_LAN_ENABLE_FLAG: u32 = 73;
    pub const SET_WIRELESS_LAN_ENABLE_FLAG: u32 = 74;
    pub const GET_INITIAL_LAUNCH_SETTINGS: u32 = 75;
    pub const SET_INITIAL_LAUNCH_SETTINGS: u32 = 76;
    pub const GET_DEVICE_NICK_NAME: u32 = 77;
    pub const SET_DEVICE_NICK_NAME: u32 = 78;
    pub const GET_PRODUCT_MODEL: u32 = 79;
    pub const GET_BLUETOOTH_ENABLE_FLAG: u32 = 88;
    pub const SET_BLUETOOTH_ENABLE_FLAG: u32 = 89;
    pub const GET_MII_AUTHOR_ID: u32 = 90;
    pub const GET_AUTO_UPDATE_ENABLE_FLAG: u32 = 95;
    pub const SET_AUTO_UPDATE_ENABLE_FLAG: u32 = 96;
    pub const GET_BATTERY_PERCENTAGE_FLAG: u32 = 99;
    pub const SET_BATTERY_PERCENTAGE_FLAG: u32 = 100;
    pub const SET_EXTERNAL_STEADY_CLOCK_INTERNAL_OFFSET: u32 = 105;
    pub const GET_EXTERNAL_STEADY_CLOCK_INTERNAL_OFFSET: u32 = 106;
    pub const GET_PUSH_NOTIFICATION_ACTIVITY_MODE_ON_SLEEP: u32 = 120;
    pub const SET_PUSH_NOTIFICATION_ACTIVITY_MODE_ON_SLEEP: u32 = 121;
    pub const GET_ERROR_REPORT_SHARE_PERMISSION: u32 = 124;
    pub const SET_ERROR_REPORT_SHARE_PERMISSION: u32 = 125;
    pub const GET_APPLET_LAUNCH_FLAGS: u32 = 126;
    pub const SET_APPLET_LAUNCH_FLAGS: u32 = 127;
    pub const GET_KEYBOARD_LAYOUT: u32 = 136;
    pub const SET_KEYBOARD_LAYOUT: u32 = 137;
    pub const GET_DEVICE_TIME_ZONE_LOCATION_UPDATED_TIME: u32 = 150;
    pub const SET_DEVICE_TIME_ZONE_LOCATION_UPDATED_TIME: u32 = 151;
    pub const GET_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_UPDATED_TIME: u32 = 152;
    pub const SET_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_UPDATED_TIME: u32 = 153;
    pub const GET_CHINESE_TRADITIONAL_INPUT_METHOD: u32 = 170;
    pub const GET_HOME_MENU_SCHEME: u32 = 174;
    pub const GET_PLATFORM_REGION: u32 = 183;
    pub const SET_PLATFORM_REGION: u32 = 184;
    pub const GET_HOME_MENU_SCHEME_MODEL: u32 = 185;
    pub const GET_TOUCH_SCREEN_MODE: u32 = 187;
    pub const SET_TOUCH_SCREEN_MODE: u32 = 188;
    pub const GET_FIELD_TESTING_FLAG: u32 = 201;
    pub const GET_PANEL_CRC_MODE: u32 = 203;
    pub const SET_PANEL_CRC_MODE: u32 = 204;
}

/// ISystemSettingsServer — "set:sys" service.
///
/// Corresponds to `ISystemSettingsServer` in upstream system_settings_server.h.
///
/// Holds system, private, device, and application settings matching
/// upstream's m_system_settings, m_private_settings, m_device_settings, m_appln_settings.
pub struct ISystemSettingsServer {
    // System settings state - matching upstream member fields
    language_code: LanguageCode,
    lock_screen_flag: bool,
    external_steady_clock_source_id: [u8; 16], // Common::UUID
    user_system_clock_context: [u8; 0x20],     // SystemClockContext
    account_settings: AccountSettings,
    eula_versions: Vec<EulaVersion>,
    color_set_id: ColorSet,
    notification_settings: NotificationSettings,
    account_notification_settings: Vec<AccountNotificationSettings>,
    vibration_master_volume: f32,
    tv_settings: TvSettings,
    audio_output_mode_hdmi: AudioOutputMode,
    audio_output_mode_speaker: AudioOutputMode,
    audio_output_mode_headphone: AudioOutputMode,
    speaker_auto_mute_flag: bool,
    quest_flag: QuestFlag,
    device_time_zone_location_name: [u8; 0x24], // LocationName
    region_code: SystemRegionCode,
    network_system_clock_context: [u8; 0x20], // SystemClockContext
    user_system_clock_automatic_correction_enabled: bool,
    primary_album_storage: PrimaryAlbumStorage,
    battery_lot: BatteryLot,
    serial_number: SerialNumber,
    nfc_enable_flag: bool,
    sleep_settings: SleepSettings,
    wireless_lan_enable_flag: bool,
    initial_launch_settings: InitialLaunchSettings,
    device_nick_name: [u8; 0x80],
    bluetooth_enable_flag: bool,
    mii_author_id: [u8; 16], // Common::UUID
    auto_update_enable_flag: bool,
    battery_percentage_flag: bool,
    external_steady_clock_internal_offset: i64,
    push_notification_activity_mode_on_sleep: i32,
    error_report_share_permission: ErrorReportSharePermission,
    applet_launch_flags: u32,
    keyboard_layout: KeyboardLayout,
    device_time_zone_location_updated_time: [u8; 0x18], // SteadyClockTimePoint
    user_system_clock_automatic_correction_updated_time: [u8; 0x18], // SteadyClockTimePoint
    chinese_traditional_input_method: ChineseTraditionalInputMethod,
    home_menu_scheme: HomeMenuScheme,
    home_menu_scheme_model: u32,
    touch_screen_mode: TouchScreenMode,
    platform_region: PlatformRegion,
    field_testing_flag: bool,
    panel_crc_mode: i32,
    save_needed: bool,
}

impl ISystemSettingsServer {
    pub fn new() -> Self {
        let mut device_nick_name = [0u8; 0x80];
        let default_name = b"yuzu";
        device_nick_name[..default_name.len()].copy_from_slice(default_name);

        Self {
            language_code: LanguageCode::EnUs,
            lock_screen_flag: false,
            external_steady_clock_source_id: [0u8; 16],
            user_system_clock_context: [0u8; 0x20],
            account_settings: AccountSettings::default(),
            eula_versions: Vec::new(),
            color_set_id: ColorSet::BasicWhite,
            notification_settings: NotificationSettings::default(),
            account_notification_settings: Vec::new(),
            vibration_master_volume: 1.0,
            tv_settings: TvSettings::default(),
            audio_output_mode_hdmi: AudioOutputMode::Ch1,
            audio_output_mode_speaker: AudioOutputMode::Ch1,
            audio_output_mode_headphone: AudioOutputMode::Ch1,
            speaker_auto_mute_flag: true,
            quest_flag: QuestFlag::Retail,
            device_time_zone_location_name: [0u8; 0x24],
            region_code: SystemRegionCode::Usa,
            network_system_clock_context: [0u8; 0x20],
            user_system_clock_automatic_correction_enabled: true,
            primary_album_storage: PrimaryAlbumStorage::Nand,
            battery_lot: BatteryLot::default(),
            serial_number: SerialNumber::default(),
            nfc_enable_flag: true,
            sleep_settings: SleepSettings::default(),
            wireless_lan_enable_flag: true,
            initial_launch_settings: InitialLaunchSettings::default(),
            device_nick_name,
            bluetooth_enable_flag: true,
            mii_author_id: [0u8; 16],
            auto_update_enable_flag: true,
            battery_percentage_flag: true,
            external_steady_clock_internal_offset: 0,
            push_notification_activity_mode_on_sleep: 0,
            error_report_share_permission: ErrorReportSharePermission::NotConfirmed,
            applet_launch_flags: 0,
            keyboard_layout: KeyboardLayout::EnglishUsInternational,
            device_time_zone_location_updated_time: [0u8; 0x18],
            user_system_clock_automatic_correction_updated_time: [0u8; 0x18],
            chinese_traditional_input_method: ChineseTraditionalInputMethod::Unknown0,
            home_menu_scheme: HomeMenuScheme {
                main: 0xFF323232,
                back: 0xFF323232,
                sub: 0xFFFFFFFF,
                bezel: 0xFFFFFFFF,
                extra: 0xFF000000,
            },
            home_menu_scheme_model: 0,
            touch_screen_mode: TouchScreenMode::Standard,
            platform_region: PlatformRegion::Global,
            field_testing_flag: false,
            panel_crc_mode: 0,
            save_needed: false,
        }
    }

    fn set_save_needed(&mut self) {
        self.save_needed = true;
    }

    // --- Getter/Setter implementations matching upstream ---

    pub fn set_language_code(&mut self, language_code: LanguageCode) {
        log::debug!("ISystemSettingsServer::SetLanguageCode called");
        self.language_code = language_code;
        self.set_save_needed();
    }

    pub fn get_firmware_version(&self) -> FirmwareVersionFormat {
        log::debug!("ISystemSettingsServer::GetFirmwareVersion called");
        // Return a default firmware version. Full implementation would
        // read from system archives.
        let mut fw = FirmwareVersionFormat::default();
        fw.major = 18;
        fw.minor = 0;
        fw.micro = 0;
        let display = b"18.0.0";
        fw.display_version[..display.len()].copy_from_slice(display);
        let title = b"NintendoSDK Firmware for NX 18.0.0-1.0";
        fw.display_title[..title.len()].copy_from_slice(title);
        fw
    }

    pub fn get_firmware_version2(&self) -> FirmwareVersionFormat {
        log::debug!("ISystemSettingsServer::GetFirmwareVersion2 called");
        self.get_firmware_version()
    }

    pub fn get_lock_screen_flag(&self) -> bool {
        log::debug!("ISystemSettingsServer::GetLockScreenFlag called");
        self.lock_screen_flag
    }

    pub fn set_lock_screen_flag(&mut self, flag: bool) {
        log::debug!("ISystemSettingsServer::SetLockScreenFlag called");
        self.lock_screen_flag = flag;
        self.set_save_needed();
    }

    pub fn get_external_steady_clock_source_id(&self) -> [u8; 16] {
        log::debug!("ISystemSettingsServer::GetExternalSteadyClockSourceId called");
        self.external_steady_clock_source_id
    }

    pub fn set_external_steady_clock_source_id(&mut self, id: [u8; 16]) {
        log::debug!("ISystemSettingsServer::SetExternalSteadyClockSourceId called");
        self.external_steady_clock_source_id = id;
        self.set_save_needed();
    }

    pub fn get_user_system_clock_context(&self) -> [u8; 0x20] {
        log::debug!("ISystemSettingsServer::GetUserSystemClockContext called");
        self.user_system_clock_context
    }

    pub fn set_user_system_clock_context(&mut self, context: [u8; 0x20]) {
        log::debug!("ISystemSettingsServer::SetUserSystemClockContext called");
        self.user_system_clock_context = context;
        self.set_save_needed();
    }

    pub fn get_account_settings(&self) -> AccountSettings {
        log::debug!("ISystemSettingsServer::GetAccountSettings called");
        self.account_settings
    }

    pub fn set_account_settings(&mut self, settings: AccountSettings) {
        log::debug!("ISystemSettingsServer::SetAccountSettings called");
        self.account_settings = settings;
        self.set_save_needed();
    }

    pub fn get_eula_versions(&self) -> (i32, &[EulaVersion]) {
        log::debug!("ISystemSettingsServer::GetEulaVersions called");
        (self.eula_versions.len() as i32, &self.eula_versions)
    }

    pub fn set_eula_versions(&mut self, versions: Vec<EulaVersion>) {
        log::debug!("ISystemSettingsServer::SetEulaVersions called");
        self.eula_versions = versions;
        self.set_save_needed();
    }

    pub fn get_color_set_id(&self) -> ColorSet {
        log::debug!("ISystemSettingsServer::GetColorSetId called");
        self.color_set_id
    }

    pub fn set_color_set_id(&mut self, color_set: ColorSet) {
        log::debug!("ISystemSettingsServer::SetColorSetId called");
        self.color_set_id = color_set;
        self.set_save_needed();
    }

    pub fn get_notification_settings(&self) -> NotificationSettings {
        log::debug!("ISystemSettingsServer::GetNotificationSettings called");
        self.notification_settings
    }

    pub fn set_notification_settings(&mut self, settings: NotificationSettings) {
        log::debug!("ISystemSettingsServer::SetNotificationSettings called");
        self.notification_settings = settings;
        self.set_save_needed();
    }

    pub fn get_account_notification_settings(&self) -> (i32, &[AccountNotificationSettings]) {
        log::debug!("ISystemSettingsServer::GetAccountNotificationSettings called");
        (
            self.account_notification_settings.len() as i32,
            &self.account_notification_settings,
        )
    }

    pub fn set_account_notification_settings(
        &mut self,
        settings: Vec<AccountNotificationSettings>,
    ) {
        log::debug!("ISystemSettingsServer::SetAccountNotificationSettings called");
        self.account_notification_settings = settings;
        self.set_save_needed();
    }

    pub fn get_vibration_master_volume(&self) -> f32 {
        log::debug!("ISystemSettingsServer::GetVibrationMasterVolume called");
        self.vibration_master_volume
    }

    pub fn set_vibration_master_volume(&mut self, volume: f32) {
        log::debug!("ISystemSettingsServer::SetVibrationMasterVolume called");
        self.vibration_master_volume = volume;
        self.set_save_needed();
    }

    pub fn get_tv_settings(&self) -> TvSettings {
        log::debug!("ISystemSettingsServer::GetTvSettings called");
        self.tv_settings
    }

    pub fn set_tv_settings(&mut self, settings: TvSettings) {
        log::debug!("ISystemSettingsServer::SetTvSettings called");
        self.tv_settings = settings;
        self.set_save_needed();
    }

    pub fn get_audio_output_mode(&self, target: AudioOutputModeTarget) -> AudioOutputMode {
        log::debug!("ISystemSettingsServer::GetAudioOutputMode called, target={:?}", target);
        match target {
            AudioOutputModeTarget::Hdmi => self.audio_output_mode_hdmi,
            AudioOutputModeTarget::Speaker => self.audio_output_mode_speaker,
            AudioOutputModeTarget::Headphone => self.audio_output_mode_headphone,
            _ => self.audio_output_mode_hdmi,
        }
    }

    pub fn set_audio_output_mode(&mut self, target: AudioOutputModeTarget, mode: AudioOutputMode) {
        log::debug!("ISystemSettingsServer::SetAudioOutputMode called");
        match target {
            AudioOutputModeTarget::Hdmi => self.audio_output_mode_hdmi = mode,
            AudioOutputModeTarget::Speaker => self.audio_output_mode_speaker = mode,
            AudioOutputModeTarget::Headphone => self.audio_output_mode_headphone = mode,
            _ => self.audio_output_mode_hdmi = mode,
        }
        self.set_save_needed();
    }

    pub fn get_speaker_auto_mute_flag(&self) -> bool {
        log::debug!("ISystemSettingsServer::GetSpeakerAutoMuteFlag called");
        self.speaker_auto_mute_flag
    }

    pub fn set_speaker_auto_mute_flag(&mut self, flag: bool) {
        log::debug!("ISystemSettingsServer::SetSpeakerAutoMuteFlag called");
        self.speaker_auto_mute_flag = flag;
        self.set_save_needed();
    }

    pub fn get_quest_flag(&self) -> QuestFlag {
        log::debug!("ISystemSettingsServer::GetQuestFlag called");
        self.quest_flag
    }

    pub fn set_quest_flag(&mut self, flag: QuestFlag) {
        log::debug!("ISystemSettingsServer::SetQuestFlag called");
        self.quest_flag = flag;
        self.set_save_needed();
    }

    pub fn get_device_time_zone_location_name(&self) -> [u8; 0x24] {
        log::debug!("ISystemSettingsServer::GetDeviceTimeZoneLocationName called");
        self.device_time_zone_location_name
    }

    pub fn set_device_time_zone_location_name(&mut self, name: [u8; 0x24]) {
        log::debug!("ISystemSettingsServer::SetDeviceTimeZoneLocationName called");
        self.device_time_zone_location_name = name;
        self.set_save_needed();
    }

    pub fn set_region_code(&mut self, region: SystemRegionCode) {
        log::debug!("ISystemSettingsServer::SetRegionCode called");
        self.region_code = region;
        self.set_save_needed();
    }

    pub fn get_network_system_clock_context(&self) -> [u8; 0x20] {
        log::debug!("ISystemSettingsServer::GetNetworkSystemClockContext called");
        self.network_system_clock_context
    }

    pub fn set_network_system_clock_context(&mut self, context: [u8; 0x20]) {
        log::debug!("ISystemSettingsServer::SetNetworkSystemClockContext called");
        self.network_system_clock_context = context;
        self.set_save_needed();
    }

    pub fn is_user_system_clock_automatic_correction_enabled(&self) -> bool {
        log::debug!("ISystemSettingsServer::IsUserSystemClockAutomaticCorrectionEnabled called");
        self.user_system_clock_automatic_correction_enabled
    }

    pub fn set_user_system_clock_automatic_correction_enabled(&mut self, enabled: bool) {
        log::debug!("ISystemSettingsServer::SetUserSystemClockAutomaticCorrectionEnabled called");
        self.user_system_clock_automatic_correction_enabled = enabled;
        self.set_save_needed();
    }

    pub fn get_debug_mode_flag(&self) -> bool {
        log::debug!("ISystemSettingsServer::GetDebugModeFlag called");
        // Upstream reads from GetSettingsItemValueImpl("settings_debug", "is_debug_mode_enabled").
        // Default to false (non-debug mode).
        false
    }

    pub fn get_primary_album_storage(&self) -> PrimaryAlbumStorage {
        log::debug!("ISystemSettingsServer::GetPrimaryAlbumStorage called");
        self.primary_album_storage
    }

    pub fn set_primary_album_storage(&mut self, storage: PrimaryAlbumStorage) {
        log::debug!("ISystemSettingsServer::SetPrimaryAlbumStorage called");
        self.primary_album_storage = storage;
        self.set_save_needed();
    }

    pub fn get_battery_lot(&self) -> BatteryLot {
        log::debug!("ISystemSettingsServer::GetBatteryLot called");
        self.battery_lot
    }

    pub fn get_serial_number(&self) -> SerialNumber {
        log::debug!("ISystemSettingsServer::GetSerialNumber called");
        self.serial_number
    }

    pub fn get_nfc_enable_flag(&self) -> bool {
        log::debug!("ISystemSettingsServer::GetNfcEnableFlag called");
        self.nfc_enable_flag
    }

    pub fn set_nfc_enable_flag(&mut self, flag: bool) {
        log::debug!("ISystemSettingsServer::SetNfcEnableFlag called");
        self.nfc_enable_flag = flag;
        self.set_save_needed();
    }

    pub fn get_sleep_settings(&self) -> SleepSettings {
        log::debug!("ISystemSettingsServer::GetSleepSettings called");
        self.sleep_settings
    }

    pub fn set_sleep_settings(&mut self, settings: SleepSettings) {
        log::debug!("ISystemSettingsServer::SetSleepSettings called");
        self.sleep_settings = settings;
        self.set_save_needed();
    }

    pub fn get_wireless_lan_enable_flag(&self) -> bool {
        log::debug!("ISystemSettingsServer::GetWirelessLanEnableFlag called");
        self.wireless_lan_enable_flag
    }

    pub fn set_wireless_lan_enable_flag(&mut self, flag: bool) {
        log::debug!("ISystemSettingsServer::SetWirelessLanEnableFlag called");
        self.wireless_lan_enable_flag = flag;
        self.set_save_needed();
    }

    pub fn get_initial_launch_settings(&self) -> InitialLaunchSettings {
        log::debug!("ISystemSettingsServer::GetInitialLaunchSettings called");
        self.initial_launch_settings
    }

    pub fn set_initial_launch_settings(&mut self, settings: InitialLaunchSettings) {
        log::debug!("ISystemSettingsServer::SetInitialLaunchSettings called");
        self.initial_launch_settings = settings;
        self.set_save_needed();
    }

    pub fn get_device_nick_name(&self) -> [u8; 0x80] {
        log::debug!("ISystemSettingsServer::GetDeviceNickName called");
        self.device_nick_name
    }

    pub fn set_device_nick_name(&mut self, name: [u8; 0x80]) {
        log::debug!("ISystemSettingsServer::SetDeviceNickName called");
        self.device_nick_name = name;
        self.set_save_needed();
    }

    pub fn get_product_model(&self) -> u32 {
        log::debug!("ISystemSettingsServer::GetProductModel called");
        // 1 = NX (Switch), matching upstream
        1
    }

    pub fn get_bluetooth_enable_flag(&self) -> bool {
        log::debug!("ISystemSettingsServer::GetBluetoothEnableFlag called");
        self.bluetooth_enable_flag
    }

    pub fn set_bluetooth_enable_flag(&mut self, flag: bool) {
        log::debug!("ISystemSettingsServer::SetBluetoothEnableFlag called");
        self.bluetooth_enable_flag = flag;
        self.set_save_needed();
    }

    pub fn get_mii_author_id(&self) -> [u8; 16] {
        log::debug!("ISystemSettingsServer::GetMiiAuthorId called");
        self.mii_author_id
    }

    pub fn get_auto_update_enable_flag(&self) -> bool {
        log::debug!("ISystemSettingsServer::GetAutoUpdateEnableFlag called");
        self.auto_update_enable_flag
    }

    pub fn set_auto_update_enable_flag(&mut self, flag: bool) {
        log::debug!("ISystemSettingsServer::SetAutoUpdateEnableFlag called");
        self.auto_update_enable_flag = flag;
        self.set_save_needed();
    }

    pub fn get_battery_percentage_flag(&self) -> bool {
        log::debug!("ISystemSettingsServer::GetBatteryPercentageFlag called");
        self.battery_percentage_flag
    }

    pub fn set_battery_percentage_flag(&mut self, flag: bool) {
        log::debug!("ISystemSettingsServer::SetBatteryPercentageFlag called");
        self.battery_percentage_flag = flag;
        self.set_save_needed();
    }

    pub fn set_external_steady_clock_internal_offset(&mut self, offset: i64) {
        log::debug!("ISystemSettingsServer::SetExternalSteadyClockInternalOffset called");
        self.external_steady_clock_internal_offset = offset;
        self.set_save_needed();
    }

    pub fn get_external_steady_clock_internal_offset(&self) -> i64 {
        log::debug!("ISystemSettingsServer::GetExternalSteadyClockInternalOffset called");
        self.external_steady_clock_internal_offset
    }

    pub fn get_push_notification_activity_mode_on_sleep(&self) -> i32 {
        log::debug!("ISystemSettingsServer::GetPushNotificationActivityModeOnSleep called");
        self.push_notification_activity_mode_on_sleep
    }

    pub fn set_push_notification_activity_mode_on_sleep(&mut self, mode: i32) {
        log::debug!("ISystemSettingsServer::SetPushNotificationActivityModeOnSleep called");
        self.push_notification_activity_mode_on_sleep = mode;
        self.set_save_needed();
    }

    pub fn get_error_report_share_permission(&self) -> ErrorReportSharePermission {
        log::debug!("ISystemSettingsServer::GetErrorReportSharePermission called");
        self.error_report_share_permission
    }

    pub fn set_error_report_share_permission(&mut self, permission: ErrorReportSharePermission) {
        log::debug!("ISystemSettingsServer::SetErrorReportSharePermission called");
        self.error_report_share_permission = permission;
        self.set_save_needed();
    }

    pub fn get_applet_launch_flags(&self) -> u32 {
        log::debug!("ISystemSettingsServer::GetAppletLaunchFlags called");
        self.applet_launch_flags
    }

    pub fn set_applet_launch_flags(&mut self, flags: u32) {
        log::debug!("ISystemSettingsServer::SetAppletLaunchFlags called");
        self.applet_launch_flags = flags;
        self.set_save_needed();
    }

    pub fn get_keyboard_layout(&self) -> KeyboardLayout {
        log::debug!("ISystemSettingsServer::GetKeyboardLayout called");
        self.keyboard_layout
    }

    pub fn set_keyboard_layout(&mut self, layout: KeyboardLayout) {
        log::debug!("ISystemSettingsServer::SetKeyboardLayout called");
        self.keyboard_layout = layout;
        self.set_save_needed();
    }

    pub fn get_device_time_zone_location_updated_time(&self) -> [u8; 0x18] {
        log::debug!("ISystemSettingsServer::GetDeviceTimeZoneLocationUpdatedTime called");
        self.device_time_zone_location_updated_time
    }

    pub fn set_device_time_zone_location_updated_time(&mut self, time: [u8; 0x18]) {
        log::debug!("ISystemSettingsServer::SetDeviceTimeZoneLocationUpdatedTime called");
        self.device_time_zone_location_updated_time = time;
        self.set_save_needed();
    }

    pub fn get_user_system_clock_automatic_correction_updated_time(&self) -> [u8; 0x18] {
        log::debug!("ISystemSettingsServer::GetUserSystemClockAutomaticCorrectionUpdatedTime called");
        self.user_system_clock_automatic_correction_updated_time
    }

    pub fn set_user_system_clock_automatic_correction_updated_time(&mut self, time: [u8; 0x18]) {
        log::debug!("ISystemSettingsServer::SetUserSystemClockAutomaticCorrectionUpdatedTime called");
        self.user_system_clock_automatic_correction_updated_time = time;
        self.set_save_needed();
    }

    pub fn get_chinese_traditional_input_method(&self) -> ChineseTraditionalInputMethod {
        log::debug!("ISystemSettingsServer::GetChineseTraditionalInputMethod called");
        self.chinese_traditional_input_method
    }

    pub fn get_home_menu_scheme(&self) -> HomeMenuScheme {
        log::debug!("ISystemSettingsServer::GetHomeMenuScheme called");
        self.home_menu_scheme
    }

    pub fn get_home_menu_scheme_model(&self) -> u32 {
        log::debug!("ISystemSettingsServer::GetHomeMenuSchemeModel called");
        self.home_menu_scheme_model
    }

    pub fn get_touch_screen_mode(&self) -> TouchScreenMode {
        log::debug!("ISystemSettingsServer::GetTouchScreenMode called");
        self.touch_screen_mode
    }

    pub fn set_touch_screen_mode(&mut self, mode: TouchScreenMode) {
        log::debug!("ISystemSettingsServer::SetTouchScreenMode called");
        self.touch_screen_mode = mode;
        self.set_save_needed();
    }

    pub fn get_platform_region(&self) -> PlatformRegion {
        log::debug!("ISystemSettingsServer::GetPlatformRegion called");
        self.platform_region
    }

    pub fn set_platform_region(&mut self, region: PlatformRegion) {
        log::debug!("ISystemSettingsServer::SetPlatformRegion called");
        self.platform_region = region;
        self.set_save_needed();
    }

    pub fn get_field_testing_flag(&self) -> bool {
        log::debug!("ISystemSettingsServer::GetFieldTestingFlag called");
        self.field_testing_flag
    }

    pub fn get_panel_crc_mode(&self) -> i32 {
        log::debug!("ISystemSettingsServer::GetPanelCrcMode called");
        self.panel_crc_mode
    }

    pub fn set_panel_crc_mode(&mut self, mode: i32) {
        log::debug!("ISystemSettingsServer::SetPanelCrcMode called");
        self.panel_crc_mode = mode;
        self.set_save_needed();
    }
}

/// GetFirmwareVersionImpl - reads firmware version from system archives.
///
/// Corresponds to upstream `GetFirmwareVersionImpl` in system_settings_server.cpp.
/// This is a simplified version that returns default firmware info.
pub fn get_firmware_version_impl(
    fw_type: GetFirmwareVersionType,
) -> FirmwareVersionFormat {
    log::debug!("GetFirmwareVersionImpl called, type={:?}", fw_type);

    let mut out = FirmwareVersionFormat::default();
    out.major = 18;
    out.minor = 0;
    out.micro = 0;

    let display = b"18.0.0";
    out.display_version[..display.len()].copy_from_slice(display);

    let title = b"NintendoSDK Firmware for NX 18.0.0-1.0";
    out.display_title[..title.len()].copy_from_slice(title);

    out
}

// ---------------------------------------------------------------------------
// ServiceFramework wiring — makes ISystemSettingsServer a real IPC service
// ---------------------------------------------------------------------------

/// IPC-facing service wrapper for set:sys.
/// Holds ISystemSettingsServer behind a Mutex for interior mutability
/// (handlers receive &dyn ServiceFramework, not &mut self).
pub struct SystemSettingsService {
    /// Inner settings state. Public for direct service-to-service access,
    /// matching upstream where services call methods directly on handler objects
    /// via ServiceManager::GetService<T>().
    pub inner: Mutex<ISystemSettingsServer>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl SystemSettingsService {
    pub fn new() -> Self {
        Self {
            inner: Mutex::new(ISystemSettingsServer::new()),
            handlers: build_handler_map(&[
                // Commands called by MK8D during init (matching upstream IDs)
                (commands::GET_FIRMWARE_VERSION, Some(Self::get_firmware_version_handler), "GetFirmwareVersion"),
                (commands::GET_FIRMWARE_VERSION2, Some(Self::get_firmware_version2_handler), "GetFirmwareVersion2"),
                (commands::GET_EXTERNAL_STEADY_CLOCK_SOURCE_ID, Some(Self::get_external_steady_clock_source_id_handler), "GetExternalSteadyClockSourceId"),
                (commands::SET_EXTERNAL_STEADY_CLOCK_SOURCE_ID, Some(Self::set_external_steady_clock_source_id_handler), "SetExternalSteadyClockSourceId"),
                (commands::GET_USER_SYSTEM_CLOCK_CONTEXT, Some(Self::get_user_system_clock_context_handler), "GetUserSystemClockContext"),
                (commands::SET_USER_SYSTEM_CLOCK_CONTEXT, Some(Self::set_user_system_clock_context_handler), "SetUserSystemClockContext"),
                (commands::GET_VIBRATION_MASTER_VOLUME, Some(Self::get_vibration_master_volume_handler), "GetVibrationMasterVolume"),
                (commands::SET_VIBRATION_MASTER_VOLUME, Some(Self::set_vibration_master_volume_handler), "SetVibrationMasterVolume"),
                (commands::GET_DEVICE_TIME_ZONE_LOCATION_NAME, Some(Self::get_device_time_zone_location_name_handler), "GetDeviceTimeZoneLocationName"),
                (commands::SET_DEVICE_TIME_ZONE_LOCATION_NAME, Some(Self::set_device_time_zone_location_name_handler), "SetDeviceTimeZoneLocationName"),
                (commands::GET_NETWORK_SYSTEM_CLOCK_CONTEXT, Some(Self::get_network_system_clock_context_handler), "GetNetworkSystemClockContext"),
                (commands::SET_NETWORK_SYSTEM_CLOCK_CONTEXT, Some(Self::set_network_system_clock_context_handler), "SetNetworkSystemClockContext"),
                (commands::IS_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_ENABLED, Some(Self::is_user_system_clock_automatic_correction_enabled_handler), "IsUserSystemClockAutomaticCorrectionEnabled"),
                (commands::SET_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_ENABLED, Some(Self::set_user_system_clock_automatic_correction_enabled_handler), "SetUserSystemClockAutomaticCorrectionEnabled"),
                (commands::GET_DEVICE_TIME_ZONE_LOCATION_UPDATED_TIME, Some(Self::get_device_time_zone_location_updated_time_handler), "GetDeviceTimeZoneLocationUpdatedTime"),
                (commands::SET_DEVICE_TIME_ZONE_LOCATION_UPDATED_TIME, Some(Self::set_device_time_zone_location_updated_time_handler), "SetDeviceTimeZoneLocationUpdatedTime"),
                (commands::GET_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_UPDATED_TIME, Some(Self::get_user_system_clock_automatic_correction_updated_time_handler), "GetUserSystemClockAutomaticCorrectionUpdatedTime"),
                (commands::SET_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_UPDATED_TIME, Some(Self::set_user_system_clock_automatic_correction_updated_time_handler), "SetUserSystemClockAutomaticCorrectionUpdatedTime"),
                (commands::GET_COLOR_SET_ID, Some(Self::get_color_set_id_handler), "GetColorSetId"),
                (commands::SET_COLOR_SET_ID, Some(Self::set_color_set_id_handler), "SetColorSetId"),
                (commands::GET_TOUCH_SCREEN_MODE, Some(Self::get_touch_screen_mode_handler), "GetTouchScreenMode"),
                (commands::SET_TOUCH_SCREEN_MODE, Some(Self::set_touch_screen_mode_handler), "SetTouchScreenMode"),
                (commands::GET_DEBUG_MODE_FLAG, Some(Self::get_debug_mode_flag_handler), "GetDebugModeFlag"),
                (commands::GET_PRODUCT_MODEL, Some(Self::get_product_model_handler), "GetProductModel"),
                (commands::SET_EXTERNAL_STEADY_CLOCK_INTERNAL_OFFSET, Some(Self::set_external_steady_clock_internal_offset_handler), "SetExternalSteadyClockInternalOffset"),
                (commands::GET_EXTERNAL_STEADY_CLOCK_INTERNAL_OFFSET, Some(Self::get_external_steady_clock_internal_offset_handler), "GetExternalSteadyClockInternalOffset"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    // --- IPC handlers: each reads params, calls inner, writes response ---

    fn get_firmware_version_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let inner = svc.inner.lock().unwrap();
        let fw = inner.get_firmware_version();
        log::info!("ISystemSettingsServer::GetFirmwareVersion -> {}.{}.{}", fw.major, fw.minor, fw.micro);
        // FirmwareVersionFormat is 0x100 bytes, written to output buffer
        let bytes = unsafe {
            std::slice::from_raw_parts(&fw as *const FirmwareVersionFormat as *const u8, std::mem::size_of::<FirmwareVersionFormat>())
        };
        ctx.write_buffer(bytes, 0);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_firmware_version2_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::get_firmware_version_handler(this, ctx);
    }

    fn get_external_steady_clock_source_id_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let inner = svc.inner.lock().unwrap();
        let id = inner.get_external_steady_clock_source_id();
        let id_str = format!("{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
            id[0],id[1],id[2],id[3], id[4],id[5], id[6],id[7], id[8],id[9], id[10],id[11],id[12],id[13],id[14],id[15]);
        log::info!("ISystemSettingsServer::GetExternalSteadyClockSourceId -> {}", id_str);
        // UUID is 16 bytes, returned inline in response
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        // Push 16 bytes as 4 u32s
        for i in 0..4 {
            let word = u32::from_le_bytes([id[i*4], id[i*4+1], id[i*4+2], id[i*4+3]]);
            rb.push_u32(word);
        }
    }

    fn set_external_steady_clock_source_id_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let mut id = [0u8; 16];
        for i in 0..4 {
            let word = rp.pop_u32();
            id[i*4..i*4+4].copy_from_slice(&word.to_le_bytes());
        }
        let id_str = format!("{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
            id[0],id[1],id[2],id[3], id[4],id[5], id[6],id[7], id[8],id[9], id[10],id[11],id[12],id[13],id[14],id[15]);
        log::info!("ISystemSettingsServer::SetExternalSteadyClockSourceId({})", id_str);
        svc.inner.lock().unwrap().set_external_steady_clock_source_id(id);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_user_system_clock_context_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let inner = svc.inner.lock().unwrap();
        let context = inner.get_user_system_clock_context();
        log::info!("ISystemSettingsServer::GetUserSystemClockContext called");
        let mut rb = ResponseBuilder::new(ctx, 2 + 8, 0, 0); // 0x20 bytes = 8 u32s
        rb.push_result(RESULT_SUCCESS);
        for i in 0..8 {
            let word = u32::from_le_bytes([context[i*4], context[i*4+1], context[i*4+2], context[i*4+3]]);
            rb.push_u32(word);
        }
    }

    fn set_user_system_clock_context_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let mut context = [0u8; 0x20];
        for i in 0..8 {
            let word = rp.pop_u32();
            context[i*4..i*4+4].copy_from_slice(&word.to_le_bytes());
        }
        log::info!("ISystemSettingsServer::SetUserSystemClockContext called");
        svc.inner.lock().unwrap().set_user_system_clock_context(context);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_vibration_master_volume_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let vol = svc.inner.lock().unwrap().get_vibration_master_volume();
        log::info!("ISystemSettingsServer::GetVibrationMasterVolume -> {}", vol);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(vol.to_bits());
    }

    fn set_vibration_master_volume_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let vol = rp.pop_f32();
        log::info!("ISystemSettingsServer::SetVibrationMasterVolume({})", vol);
        svc.inner.lock().unwrap().set_vibration_master_volume(vol);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_device_time_zone_location_name_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let name = svc.inner.lock().unwrap().get_device_time_zone_location_name();
        let name_str = std::str::from_utf8(&name).unwrap_or("").trim_end_matches('\0');
        log::info!("ISystemSettingsServer::GetDeviceTimeZoneLocationName -> \"{}\"", name_str);
        // LocationName is 0x24 bytes = 9 u32s
        let mut rb = ResponseBuilder::new(ctx, 2 + 9, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        for i in 0..9 {
            let start = i * 4;
            let end = (start + 4).min(0x24);
            let mut word_bytes = [0u8; 4];
            let len = end - start;
            word_bytes[..len].copy_from_slice(&name[start..end]);
            rb.push_u32(u32::from_le_bytes(word_bytes));
        }
    }

    fn set_device_time_zone_location_name_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let mut name = [0u8; 0x24];
        for i in 0..9 {
            let word = rp.pop_u32();
            let start = i * 4;
            let end = (start + 4).min(0x24);
            name[start..end].copy_from_slice(&word.to_le_bytes()[..end-start]);
        }
        let name_str = std::str::from_utf8(&name).unwrap_or("").trim_end_matches('\0');
        log::info!("ISystemSettingsServer::SetDeviceTimeZoneLocationName(\"{}\")", name_str);
        svc.inner.lock().unwrap().set_device_time_zone_location_name(name);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_network_system_clock_context_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let context = svc.inner.lock().unwrap().get_network_system_clock_context();
        log::info!("ISystemSettingsServer::GetNetworkSystemClockContext called");
        let mut rb = ResponseBuilder::new(ctx, 2 + 8, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        for i in 0..8 {
            let word = u32::from_le_bytes([context[i*4], context[i*4+1], context[i*4+2], context[i*4+3]]);
            rb.push_u32(word);
        }
    }

    fn set_network_system_clock_context_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let mut context = [0u8; 0x20];
        for i in 0..8 {
            let word = rp.pop_u32();
            context[i*4..i*4+4].copy_from_slice(&word.to_le_bytes());
        }
        log::info!("ISystemSettingsServer::SetNetworkSystemClockContext called");
        svc.inner.lock().unwrap().set_network_system_clock_context(context);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn is_user_system_clock_automatic_correction_enabled_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let enabled = svc.inner.lock().unwrap().is_user_system_clock_automatic_correction_enabled();
        log::info!("ISystemSettingsServer::IsUserSystemClockAutomaticCorrectionEnabled -> {}", enabled);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(enabled as u32);
    }

    fn set_user_system_clock_automatic_correction_enabled_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let enabled = rp.pop_bool();
        log::info!("ISystemSettingsServer::SetUserSystemClockAutomaticCorrectionEnabled({})", enabled);
        svc.inner.lock().unwrap().set_user_system_clock_automatic_correction_enabled(enabled);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_device_time_zone_location_updated_time_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let time = svc.inner.lock().unwrap().get_device_time_zone_location_updated_time();
        log::info!("ISystemSettingsServer::GetDeviceTimeZoneLocationUpdatedTime called");
        // SteadyClockTimePoint is 0x18 bytes = 6 u32s
        let mut rb = ResponseBuilder::new(ctx, 2 + 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        for i in 0..6 {
            let word = u32::from_le_bytes([time[i*4], time[i*4+1], time[i*4+2], time[i*4+3]]);
            rb.push_u32(word);
        }
    }

    fn set_device_time_zone_location_updated_time_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let mut time = [0u8; 0x18];
        for i in 0..6 {
            let word = rp.pop_u32();
            time[i*4..i*4+4].copy_from_slice(&word.to_le_bytes());
        }
        log::info!("ISystemSettingsServer::SetDeviceTimeZoneLocationUpdatedTime called");
        svc.inner.lock().unwrap().set_device_time_zone_location_updated_time(time);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_user_system_clock_automatic_correction_updated_time_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let time = svc.inner.lock().unwrap().get_user_system_clock_automatic_correction_updated_time();
        log::info!("ISystemSettingsServer::GetUserSystemClockAutomaticCorrectionUpdatedTime called");
        let mut rb = ResponseBuilder::new(ctx, 2 + 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        for i in 0..6 {
            let word = u32::from_le_bytes([time[i*4], time[i*4+1], time[i*4+2], time[i*4+3]]);
            rb.push_u32(word);
        }
    }

    fn set_user_system_clock_automatic_correction_updated_time_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let mut time = [0u8; 0x18];
        for i in 0..6 {
            let word = rp.pop_u32();
            time[i*4..i*4+4].copy_from_slice(&word.to_le_bytes());
        }
        log::info!("ISystemSettingsServer::SetUserSystemClockAutomaticCorrectionUpdatedTime called");
        svc.inner.lock().unwrap().set_user_system_clock_automatic_correction_updated_time(time);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_color_set_id_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let id = svc.inner.lock().unwrap().get_color_set_id();
        log::debug!("ISystemSettingsServer::GetColorSetId -> {:?}", id);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(id as u32);
    }

    fn set_color_set_id_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let id = rp.pop_u32();
        log::debug!("ISystemSettingsServer::SetColorSetId({})", id);
        svc.inner.lock().unwrap().set_color_set_id(unsafe { std::mem::transmute(id) });
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_touch_screen_mode_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mode = svc.inner.lock().unwrap().get_touch_screen_mode();
        log::info!("ISystemSettingsServer::GetTouchScreenMode -> {:?}", mode);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(mode as u32);
    }

    fn set_touch_screen_mode_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let mode = rp.pop_u32();
        log::debug!("ISystemSettingsServer::SetTouchScreenMode({})", mode);
        svc.inner.lock().unwrap().set_touch_screen_mode(unsafe { std::mem::transmute(mode) });
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_debug_mode_flag_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("ISystemSettingsServer::GetDebugModeFlag -> false");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0); // false
    }

    fn get_product_model_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("ISystemSettingsServer::GetProductModel -> 1 (NX)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(1); // ProductModel::NX
    }

    fn set_external_steady_clock_internal_offset_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let offset = rp.pop_i64();
        log::info!("ISystemSettingsServer::SetExternalSteadyClockInternalOffset({})", offset);
        svc.inner.lock().unwrap().set_external_steady_clock_internal_offset(offset);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_external_steady_clock_internal_offset_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let offset = svc.inner.lock().unwrap().get_external_steady_clock_internal_offset();
        log::info!("ISystemSettingsServer::GetExternalSteadyClockInternalOffset -> {}", offset);
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(offset as u64);
    }
}

impl SessionRequestHandler for SystemSettingsService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        ServiceFramework::get_service_name(self)
    }
    fn as_any(&self) -> &dyn std::any::Any { self }
}

impl ServiceFramework for SystemSettingsService {
    fn get_service_name(&self) -> &str { "set:sys" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_system_settings_default() {
        let server = ISystemSettingsServer::new();
        assert_eq!(server.get_color_set_id(), ColorSet::BasicWhite);
        assert_eq!(server.get_vibration_master_volume(), 1.0);
        assert_eq!(server.get_bluetooth_enable_flag(), true);
        assert_eq!(server.get_nfc_enable_flag(), true);
        assert_eq!(server.get_wireless_lan_enable_flag(), true);
        assert_eq!(server.get_product_model(), 1);
        assert_eq!(server.get_touch_screen_mode(), TouchScreenMode::Standard);
    }

    #[test]
    fn test_system_settings_set_get_color() {
        let mut server = ISystemSettingsServer::new();
        server.set_color_set_id(ColorSet::BasicBlack);
        assert_eq!(server.get_color_set_id(), ColorSet::BasicBlack);
    }

    #[test]
    fn test_system_settings_vibration_volume() {
        let mut server = ISystemSettingsServer::new();
        server.set_vibration_master_volume(0.5);
        assert!((server.get_vibration_master_volume() - 0.5).abs() < f32::EPSILON);
    }

    #[test]
    fn test_firmware_version() {
        let fw = get_firmware_version_impl(GetFirmwareVersionType::Version1);
        assert_eq!(fw.major, 18);
        assert_eq!(fw.minor, 0);
    }
}
