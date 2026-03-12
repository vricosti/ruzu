// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/set/system_settings_server.h and system_settings_server.cpp
//!
//! ISystemSettingsServer service ("set:sys").

use crate::hle::result::{ErrorModule, ResultCode, RESULT_SUCCESS};
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

/// IPC command table for ISystemSettingsServer ("set:sys").
pub mod commands {
    pub const SET_LANGUAGE_CODE: u32 = 0;
    pub const GET_FIRMWARE_VERSION: u32 = 3;
    pub const GET_FIRMWARE_VERSION2: u32 = 4;
    pub const GET_LOCK_SCREEN_FLAG: u32 = 7;
    pub const SET_LOCK_SCREEN_FLAG: u32 = 8;
    pub const GET_EXTERNAL_STEADY_CLOCK_SOURCE_ID: u32 = 9;
    pub const SET_EXTERNAL_STEADY_CLOCK_SOURCE_ID: u32 = 10;
    pub const GET_USER_SYSTEM_CLOCK_CONTEXT: u32 = 17;
    pub const SET_USER_SYSTEM_CLOCK_CONTEXT: u32 = 18;
    pub const GET_ACCOUNT_SETTINGS: u32 = 19;
    pub const SET_ACCOUNT_SETTINGS: u32 = 20;
    pub const GET_EULA_VERSIONS: u32 = 21;
    pub const SET_EULA_VERSIONS: u32 = 22;
    pub const GET_COLOR_SET_ID: u32 = 23;
    pub const SET_COLOR_SET_ID: u32 = 24;
    pub const GET_NOTIFICATION_SETTINGS: u32 = 25;
    pub const SET_NOTIFICATION_SETTINGS: u32 = 26;
    pub const GET_ACCOUNT_NOTIFICATION_SETTINGS: u32 = 27;
    pub const SET_ACCOUNT_NOTIFICATION_SETTINGS: u32 = 28;
    pub const GET_VIBRATION_MASTER_VOLUME: u32 = 29;
    pub const SET_VIBRATION_MASTER_VOLUME: u32 = 30;
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
    pub const GET_DEVICE_TIME_ZONE_LOCATION_NAME: u32 = 49;
    pub const SET_DEVICE_TIME_ZONE_LOCATION_NAME: u32 = 50;
    pub const SET_REGION_CODE: u32 = 57;
    pub const GET_NETWORK_SYSTEM_CLOCK_CONTEXT: u32 = 58;
    pub const SET_NETWORK_SYSTEM_CLOCK_CONTEXT: u32 = 59;
    pub const IS_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_ENABLED: u32 = 60;
    pub const SET_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_ENABLED: u32 = 61;
    pub const GET_DEBUG_MODE_FLAG: u32 = 62;
    pub const GET_PRIMARY_ALBUM_STORAGE: u32 = 63;
    pub const SET_PRIMARY_ALBUM_STORAGE: u32 = 64;
    pub const GET_BATTERY_LOT: u32 = 68;
    pub const GET_SERIAL_NUMBER: u32 = 69;
    pub const GET_NFC_ENABLE_FLAG: u32 = 70;
    pub const SET_NFC_ENABLE_FLAG: u32 = 71;
    pub const GET_SLEEP_SETTINGS: u32 = 73;
    pub const SET_SLEEP_SETTINGS: u32 = 74;
    pub const GET_WIRELESS_LAN_ENABLE_FLAG: u32 = 75;
    pub const SET_WIRELESS_LAN_ENABLE_FLAG: u32 = 76;
    pub const GET_INITIAL_LAUNCH_SETTINGS: u32 = 77;
    pub const SET_INITIAL_LAUNCH_SETTINGS: u32 = 78;
    pub const GET_DEVICE_NICK_NAME: u32 = 79;
    pub const SET_DEVICE_NICK_NAME: u32 = 80;
    pub const GET_PRODUCT_MODEL: u32 = 81;
    pub const GET_BLUETOOTH_ENABLE_FLAG: u32 = 88;
    pub const SET_BLUETOOTH_ENABLE_FLAG: u32 = 89;
    pub const GET_MII_AUTHOR_ID: u32 = 90;
    pub const GET_AUTO_UPDATE_ENABLE_FLAG: u32 = 95;
    pub const SET_AUTO_UPDATE_ENABLE_FLAG: u32 = 96;
    pub const GET_BATTERY_PERCENTAGE_FLAG: u32 = 99;
    pub const SET_BATTERY_PERCENTAGE_FLAG: u32 = 100;
    pub const SET_EXTERNAL_STEADY_CLOCK_INTERNAL_OFFSET: u32 = 124;
    pub const GET_EXTERNAL_STEADY_CLOCK_INTERNAL_OFFSET: u32 = 125;
    pub const GET_PUSH_NOTIFICATION_ACTIVITY_MODE_ON_SLEEP: u32 = 126;
    pub const SET_PUSH_NOTIFICATION_ACTIVITY_MODE_ON_SLEEP: u32 = 127;
    pub const GET_ERROR_REPORT_SHARE_PERMISSION: u32 = 130;
    pub const SET_ERROR_REPORT_SHARE_PERMISSION: u32 = 131;
    pub const GET_APPLET_LAUNCH_FLAGS: u32 = 140;
    pub const SET_APPLET_LAUNCH_FLAGS: u32 = 141;
    pub const GET_KEYBOARD_LAYOUT: u32 = 152;
    pub const SET_KEYBOARD_LAYOUT: u32 = 153;
    pub const GET_DEVICE_TIME_ZONE_LOCATION_UPDATED_TIME: u32 = 162;
    pub const SET_DEVICE_TIME_ZONE_LOCATION_UPDATED_TIME: u32 = 163;
    pub const GET_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_UPDATED_TIME: u32 = 168;
    pub const SET_USER_SYSTEM_CLOCK_AUTOMATIC_CORRECTION_UPDATED_TIME: u32 = 169;
    pub const GET_CHINESE_TRADITIONAL_INPUT_METHOD: u32 = 195;
    pub const GET_HOME_MENU_SCHEME: u32 = 200;
    pub const GET_HOME_MENU_SCHEME_MODEL: u32 = 201;
    pub const GET_TOUCH_SCREEN_MODE: u32 = 203;
    pub const GET_PLATFORM_REGION: u32 = 207;
    pub const SET_PLATFORM_REGION: u32 = 208;
    pub const SET_TOUCH_SCREEN_MODE: u32 = 211;
    pub const GET_FIELD_TESTING_FLAG: u32 = 215;
    pub const GET_PANEL_CRC_MODE: u32 = 216;
    pub const SET_PANEL_CRC_MODE: u32 = 217;
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
        // Upstream reads from Settings::values.use_debug_mode
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
