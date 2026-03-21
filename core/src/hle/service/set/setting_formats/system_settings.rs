//! Port of zuyu/src/core/hle/service/set/setting_formats/system_settings.h and .cpp
//!
//! System settings format.

use crate::hle::service::psc::time::common::{
    LocationName, SteadyClockTimePoint, SystemClockContext,
};
use crate::hle::service::set::settings_types::{
    AccountNotificationSettings, AccountSettings, AudioOutputMode, ChineseTraditionalInputMethod,
    ColorSet, ErrorReportSharePermission, EulaVersion, InitialLaunchSettingsPacked,
    KeyboardLayout, LanguageCode, NotificationSettings, PrimaryAlbumStorage, QuestFlag,
    SleepSettings, SystemRegionCode, TouchScreenMode, TvSettings,
};

/// Port of Service::Set::SystemSettings
#[derive(Clone, Copy)]
#[repr(C)]
pub struct SystemSettings {
    /// Version
    pub version: u32,
    /// Flags
    pub flags: u32,
    /// Reserved
    pub _reserved_0x08: [u8; 0x8],

    // offset 0x10
    /// nn::settings::LanguageCode
    pub language_code: LanguageCode,
    /// Reserved
    pub _reserved_0x18: [u8; 0x38],

    // offset 0x50
    /// nn::settings::system::NetworkSettings count
    pub network_setting_count: u32,
    /// wireless_lan_enable_flag
    pub wireless_lan_enable_flag: bool,
    pub _padding_0x55: [u8; 0x3],
    /// Reserved
    pub _reserved_0x58: [u8; 0x8],

    // offset 0x60
    /// nn::settings::system::NetworkSettings
    pub network_settings_1b0: [[u8; 0x400]; 32],

    // offset 0x8060
    /// nn::settings::system::BluetoothDevicesSettings count
    pub bluetooth_device_settings_count: [u8; 0x4],
    /// bluetooth_enable_flag
    pub bluetooth_enable_flag: bool,
    pub _padding_0x8065: [u8; 0x3],
    /// bluetooth_afh_enable_flag
    pub bluetooth_afh_enable_flag: bool,
    pub _padding_0x8069: [u8; 0x3],
    /// bluetooth_boost_enable_flag
    pub bluetooth_boost_enable_flag: bool,
    pub _padding_0x806d: [u8; 0x3],
    /// nn::settings::system::BluetoothDevicesSettings (first 10)
    pub bluetooth_device_settings_first_10: [[u8; 0x200]; 10],

    // offset 0x9470
    /// ldn_channel
    pub ldn_channel: i32,
    /// Reserved
    pub _reserved_0x9474: [u8; 0x3C],

    // offset 0x94B0
    /// nn::util::Uuid MiiAuthorId
    pub mii_author_id: [u8; 16],

    /// Reserved
    pub _reserved_0x94c0: [u8; 0x30],

    // offset 0x94F0
    /// nn::settings::system::NxControllerSettings count
    pub nx_controller_settings_count: u32,

    /// Reserved
    pub _reserved_0x94f4: [u8; 0xC],

    // offset 0x9500
    /// nn::settings::system::NxControllerLegacySettings
    pub nx_controller_legacy_settings: [[u8; 0x40]; 10],

    /// Reserved
    pub _reserved_0x9780: [u8; 0x170],

    // offset 0x98F0
    /// external_rtc_reset_flag
    pub external_rtc_reset_flag: bool,
    pub _padding_0x98f1: [u8; 0x3],
    /// Reserved
    pub _reserved_0x98f4: [u8; 0x3C],

    // offset 0x9930
    /// push_notification_activity_mode_on_sleep
    pub push_notification_activity_mode_on_sleep: i32,
    /// Reserved
    pub _reserved_0x9934: [u8; 0x3C],

    // offset 0x9970
    /// nn::settings::system::ErrorReportSharePermission
    pub error_report_share_permission: ErrorReportSharePermission,
    /// Reserved
    pub _reserved_0x9974: [u8; 0x3C],

    // offset 0x99B0
    /// nn::settings::system::KeyboardLayout
    pub keyboard_layout: KeyboardLayout,
    /// Reserved
    pub _reserved_0x99b4: [u8; 0x3C],

    // offset 0x99F0
    /// web_inspector_flag
    pub web_inspector_flag: bool,
    pub _padding_0x99f1: [u8; 0x3],

    // offset 0x99F4
    /// nn::settings::system::AllowedSslHost count
    pub allowed_ssl_host_count: u32,

    // offset 0x99F8
    /// memory_usage_rate_flag
    pub memory_usage_rate_flag: bool,
    pub _padding_0x99f9: [u8; 0x3],
    /// Reserved
    pub _reserved_0x99fc: [u8; 0x34],

    // offset 0x9A30
    /// nn::settings::system::HostFsMountPoint
    pub host_fs_mount_point: [u8; 0x100],

    // offset 0x9B30
    /// nn::settings::system::AllowedSslHost
    pub allowed_ssl_hosts: [[u8; 0x100]; 8],

    /// Reserved
    pub _reserved_0xa330: [u8; 0x6C0],

    // offset 0xA9F0
    /// nn::settings::system::BlePairingSettings count
    pub ble_pairing_settings_count: u32,
    /// Reserved
    pub _reserved_0xa9f4: [u8; 0xC],
    // offset 0xAA00
    /// nn::settings::system::BlePairingSettings
    pub ble_pairing_settings: [[u8; 0x80]; 10],

    // offset 0xAF00
    /// nn::settings::system::AccountOnlineStorageSettings count
    pub account_online_storage_settings_count: u32,
    /// Reserved
    pub _reserved_0xaf04: [u8; 0xC],
    // offset 0xAF10
    /// nn::settings::system::AccountOnlineStorageSettings
    pub account_online_storage_settings: [[u8; 0x40]; 8],

    // offset 0xB110
    /// pctl_ready_flag
    pub pctl_ready_flag: bool,
    pub _padding_0xb111: [u8; 0x3],
    /// Reserved
    pub _reserved_0xb114: [u8; 0x3C],

    // offset 0xB150
    /// nn::settings::system::ThemeId type0
    pub theme_id_type0: [u8; 0x80],
    /// nn::settings::system::ThemeId type1
    pub theme_id_type1: [u8; 0x80],
    /// Reserved
    pub _reserved_0xb250: [u8; 0x100],

    // offset 0xB350
    /// nn::settings::system::ChineseTraditionalInputMethod
    pub chinese_traditional_input_method: ChineseTraditionalInputMethod,
    /// Reserved
    pub _reserved_0xb354: [u8; 0x3C],

    // offset 0xB390
    /// zoom_flag
    pub zoom_flag: bool,
    pub _padding_0xb391: [u8; 0x3],
    /// Reserved
    pub _reserved_0xb394: [u8; 0x3C],

    // offset 0xB3D0
    /// nn::settings::system::ButtonConfigRegisteredSettings count
    pub button_config_registered_settings_count: u32,
    /// Reserved
    pub _reserved_0xb3d4: [u8; 0xC],

    // offset 0xB3E0
    /// nn::settings::system::ButtonConfigSettings count
    pub button_config_settings_count: u32,
    /// Reserved
    pub _reserved_0xb3e4: [u8; 0x4],
    // offset 0xB3E8
    /// nn::settings::system::ButtonConfigSettings
    pub button_config_settings: [[u8; 0x5A8]; 5],
    /// Reserved
    pub _reserved_button0: [u8; 0x13B0],
    /// button_config_settings_embedded_count
    pub button_config_settings_embedded_count: u32,
    pub _reserved_emb_count: [u8; 0x4],
    /// nn::settings::system::ButtonConfigSettings embedded
    pub button_config_settings_embedded: [[u8; 0x5A8]; 5],
    /// Reserved
    pub _reserved_button1: [u8; 0x13B0],
    /// button_config_settings_left_count
    pub button_config_settings_left_count: u32,
    pub _reserved_left_count: [u8; 0x4],
    /// nn::settings::system::ButtonConfigSettings left
    pub button_config_settings_left: [[u8; 0x5A8]; 5],
    /// Reserved
    pub _reserved_button2: [u8; 0x13B0],
    /// button_config_settings_right_count
    pub button_config_settings_right_count: u32,
    pub _reserved_right_count: [u8; 0x4],
    /// nn::settings::system::ButtonConfigSettings right
    pub button_config_settings_right: [[u8; 0x5A8]; 5],
    /// Reserved
    pub _reserved_button3: [u8; 0x73B0],

    // offset 0x1D3E0
    /// nn::settings::system::ButtonConfigRegisteredSettings embedded
    pub button_config_registered_settings_embedded: [u8; 0x5C8],
    /// nn::settings::system::ButtonConfigRegisteredSettings
    pub button_config_registered_settings: [[u8; 0x5C8]; 10],
    /// Reserved
    pub _reserved_button_reg: [u8; 0x7FF8],

    // offset 0x29370
    /// nn::settings::system::ConsoleSixAxisSensorAccelerationBias
    pub console_six_axis_sensor_acceleration_bias: [f32; 3],
    /// nn::settings::system::ConsoleSixAxisSensorAngularVelocityBias
    pub console_six_axis_sensor_angular_velocity_bias: [f32; 3],
    /// nn::settings::system::ConsoleSixAxisSensorAccelerationGain
    pub console_six_axis_sensor_acceleration_gain: [u8; 0x24],
    /// nn::settings::system::ConsoleSixAxisSensorAngularVelocityGain
    pub console_six_axis_sensor_angular_velocity_gain: [u8; 0x24],
    /// nn::settings::system::ConsoleSixAxisSensorAngularVelocityTimeBias
    pub console_six_axis_sensor_angular_velocity_time_bias: [f32; 3],
    /// nn::settings::system::ConsoleSixAxisSensorAngularAcceleration
    pub console_six_axis_sensor_angular_velocity_acceleration: [u8; 0x24],
    /// Reserved
    pub _reserved_0x29400: [u8; 0x70],

    // offset 0x29470
    /// lock_screen_flag
    pub lock_screen_flag: bool,
    pub _padding_0x29471: [u8; 0x3],
    /// Reserved
    pub _reserved_0x29474: [u8; 0x4],

    // offset 0x29478
    /// nn::settings::system::ColorSet
    pub color_set_id: ColorSet,

    // offset 0x2947C
    /// nn::settings::system::QuestFlag
    pub quest_flag: QuestFlag,

    // offset 0x29480
    /// nn::settings::system::SystemRegionCode
    pub region_code: SystemRegionCode,

    // offset 0x29484
    /// InitialLaunchSettingsPacked
    pub initial_launch_settings_packed: InitialLaunchSettingsPacked,

    // offset 0x294A0
    /// battery_percentage_flag
    pub battery_percentage_flag: bool,
    pub _padding_0x294a1: [u8; 0x3],

    // offset 0x294A4
    /// BitFlagSet<32, nn::settings::system::AppletLaunchFlag>
    pub applet_launch_flag: u32,

    // offset 0x294A8
    /// nn::settings::system::ThemeSettings
    pub theme_settings: [u8; 0x8],
    /// nn::fssystem::ArchiveMacKey
    pub theme_key: [u8; 0x10],

    // offset 0x294C0
    /// field_testing_flag
    pub field_testing_flag: bool,
    pub _padding_0x294c1: [u8; 0x3],

    // offset 0x294C4
    /// panel_crc_mode
    pub panel_crc_mode: i32,
    /// Reserved
    pub _reserved_0x294c8: [u8; 0x28],

    // offset 0x294F0
    /// nn::settings::system::BacklightSettings (mixed up)
    pub backlight_settings_mixed_up: [u8; 0x2C],
    /// Reserved
    pub _reserved_0x2951c: [u8; 0x64],

    // offset 0x29580
    /// nn::time::SystemClockContext (user)
    pub user_system_clock_context: SystemClockContext,
    // offset 0x295A0
    /// nn::time::SystemClockContext (network)
    pub network_system_clock_context: SystemClockContext,
    // offset 0x295C0
    /// user_system_clock_automatic_correction_enabled
    pub user_system_clock_automatic_correction_enabled: bool,
    pub _padding_0x295c1: [u8; 0x3],
    /// Reserved
    pub _reserved_0x295c4: [u8; 0x4],
    // offset 0x295C8
    /// nn::time::SteadyClockTimePoint
    pub user_system_clock_automatic_correction_updated_time_point: SteadyClockTimePoint,
    /// Reserved
    pub _reserved_0x295e0: [u8; 0x10],

    // offset 0x295F0
    /// nn::settings::system::AccountSettings
    pub account_settings: AccountSettings,
    /// Reserved
    pub _reserved_0x295f4: [u8; 0xFC],

    // offset 0x296F0
    /// nn::settings::system::AudioVolume type0
    pub audio_volume_type0: [u8; 0x8],
    /// nn::settings::system::AudioVolume type1
    pub audio_volume_type1: [u8; 0x8],
    /// AudioOutputMode hdmi
    pub audio_output_mode_hdmi: AudioOutputMode,
    /// AudioOutputMode speaker
    pub audio_output_mode_speaker: AudioOutputMode,
    /// AudioOutputMode headphone
    pub audio_output_mode_headphone: AudioOutputMode,
    /// force_mute_on_headphone_removed
    pub force_mute_on_headphone_removed: bool,
    pub _padding_0x2970d: [u8; 0x3],
    /// headphone_volume_warning_count
    pub headphone_volume_warning_count: i32,
    /// heaphone_volume_update_flag (note: upstream typo preserved)
    pub heaphone_volume_update_flag: bool,
    pub _padding_0x29715: [u8; 0x3],
    /// nn::settings::system::AudioVolume type2
    pub audio_volume_type2: [u8; 0x8],
    /// AudioOutputMode type3
    pub audio_output_mode_type3: AudioOutputMode,
    /// AudioOutputMode type4
    pub audio_output_mode_type4: AudioOutputMode,
    /// hearing_protection_safeguard_flag
    pub hearing_protection_safeguard_flag: bool,
    pub _padding_0x29729: [u8; 0x3],
    /// Reserved
    pub _reserved_0x2972c: [u8; 0x4],
    // offset 0x29730
    /// hearing_protection_safeguard_remaining_time
    pub hearing_protection_safeguard_remaining_time: i64,
    /// Reserved
    pub _reserved_0x29738: [u8; 0x38],

    // offset 0x29770
    /// console_information_upload_flag
    pub console_information_upload_flag: bool,
    pub _padding_0x29771: [u8; 0x3],
    /// Reserved
    pub _reserved_0x29774: [u8; 0x3C],

    // offset 0x297B0
    /// automatic_application_download_flag
    pub automatic_application_download_flag: bool,
    pub _padding_0x297b1: [u8; 0x3],
    /// Reserved
    pub _reserved_0x297b4: [u8; 0x4],

    // offset 0x297B8
    /// nn::settings::system::NotificationSettings
    pub notification_settings: NotificationSettings,
    /// Reserved
    pub _reserved_0x297d0: [u8; 0x60],

    // offset 0x29830
    /// nn::settings::system::AccountNotificationSettings count
    pub account_notification_settings_count: i32,
    /// Reserved
    pub _reserved_0x29834: [u8; 0xC],
    // offset 0x29840
    /// nn::settings::system::AccountNotificationSettings
    pub account_notification_settings: [AccountNotificationSettings; 8],
    /// Reserved
    pub _reserved_0x29900: [u8; 0x140],

    // offset 0x29A40
    /// vibration_master_volume
    pub vibration_master_volume: f32,

    // offset 0x29A44
    /// usb_full_key_enable_flag
    pub usb_full_key_enable_flag: bool,
    pub _padding_0x29a45: [u8; 0x3],

    // offset 0x29A48
    /// nn::settings::system::AnalogStickUserCalibration left
    pub analog_stick_user_calibration_left: [u8; 0x10],
    /// nn::settings::system::AnalogStickUserCalibration right
    pub analog_stick_user_calibration_right: [u8; 0x10],

    // offset 0x29A68
    /// nn::settings::system::TouchScreenMode
    pub touch_screen_mode: TouchScreenMode,
    /// Reserved
    pub _reserved_0x29a6c: [u8; 0x14],

    // offset 0x29A80
    /// nn::settings::system::TvSettings
    pub tv_settings: TvSettings,

    // offset 0x29AA0
    /// nn::settings::system::Edid
    pub edid: [u8; 0x100],
    /// Reserved
    pub _reserved_0x29ba0: [u8; 0x2E0],

    // offset 0x29E80
    /// nn::settings::system::DataDeletionSettings
    pub data_deletion_settings: [u8; 0x8],
    /// Reserved
    pub _reserved_0x29e88: [u8; 0x38],

    // offset 0x29EC0
    /// nn::ncm::ProgramId initial_system_applet_program_id
    pub initial_system_applet_program_id: [u8; 0x8],
    /// nn::ncm::ProgramId overlay_disp_program_id
    pub overlay_disp_program_id: [u8; 0x8],
    /// Reserved
    pub _reserved_0x29ed0: [u8; 0x4],

    // offset 0x29ED4
    /// requires_run_repair_time_reviser
    pub requires_run_repair_time_reviser: bool,
    /// Reserved
    pub _reserved_0x29ed5: [u8; 0x6B],

    // offset 0x29F40
    /// nn::time::LocationName
    pub device_time_zone_location_name: LocationName,
    /// Reserved
    pub _reserved_0x29f64: [u8; 0x4],
    // offset 0x29F68
    /// nn::time::SteadyClockTimePoint
    pub device_time_zone_location_updated_time: SteadyClockTimePoint,

    /// Reserved
    pub _reserved_0x29f80: [u8; 0xC0],

    // offset 0x2A040
    /// nn::settings::system::PrimaryAlbumStorage
    pub primary_album_storage: PrimaryAlbumStorage,
    /// Reserved
    pub _reserved_0x2a044: [u8; 0x3C],

    // offset 0x2A080
    /// usb_30_enable_flag
    pub usb_30_enable_flag: bool,
    pub _padding_0x2a081: [u8; 0x3],
    /// usb_30_host_enable_flag
    pub usb_30_host_enable_flag: bool,
    pub _padding_0x2a085: [u8; 0x3],
    /// usb_30_device_enable_flag
    pub usb_30_device_enable_flag: bool,
    pub _padding_0x2a089: [u8; 0x3],
    /// Reserved
    pub _reserved_0x2a08c: [u8; 0x34],

    // offset 0x2A0C0
    /// nfc_enable_flag
    pub nfc_enable_flag: bool,
    pub _padding_0x2a0c1: [u8; 0x3],
    /// Reserved
    pub _reserved_0x2a0c4: [u8; 0x3C],

    // offset 0x2A100
    /// nn::settings::system::SleepSettings
    pub sleep_settings: SleepSettings,
    /// Reserved
    pub _reserved_0x2a10c: [u8; 0x34],

    // offset 0x2A140
    /// nn::settings::system::EulaVersion count
    pub eula_version_count: i32,
    /// Reserved
    pub _reserved_0x2a144: [u8; 0xC],
    // offset 0x2A150
    /// nn::settings::system::EulaVersion
    pub eula_versions: [EulaVersion; 32],
    /// Reserved
    pub _reserved_0x2a750: [u8; 0x200],

    // offset 0x2A950
    /// nn::settings::system::DeviceNickName
    pub device_nick_name: [u8; 0x80],
    /// Reserved
    pub _reserved_0x2a9d0: [u8; 0x80],

    // offset 0x2AA50
    /// auto_update_enable_flag
    pub auto_update_enable_flag: bool,
    pub _padding_0x2aa51: [u8; 0x3],
    /// Reserved
    pub _reserved_0x2aa54: [u8; 0x4C],

    // offset 0x2AAA0
    /// nn::settings::system::BluetoothDevicesSettings (last 14)
    pub bluetooth_device_settings_last_14: [[u8; 0x200]; 14],
    /// Reserved
    pub _reserved_0x2c6a0: [u8; 0x2000],

    // offset 0x2E6A0
    /// nn::settings::system::NxControllerSettings data
    pub nx_controller_settings_data_from_offset_30: [[u8; 0x800]; 10],
}

// offsetof checks (matching upstream static_asserts)
const _: () = {
    assert!(core::mem::offset_of!(SystemSettings, language_code) == 0x10);
    assert!(core::mem::offset_of!(SystemSettings, network_setting_count) == 0x50);
    assert!(core::mem::offset_of!(SystemSettings, network_settings_1b0) == 0x60);
    assert!(core::mem::offset_of!(SystemSettings, bluetooth_device_settings_count) == 0x8060);
    assert!(core::mem::offset_of!(SystemSettings, bluetooth_enable_flag) == 0x8064);
    assert!(core::mem::offset_of!(SystemSettings, bluetooth_device_settings_first_10) == 0x8070);
    assert!(core::mem::offset_of!(SystemSettings, ldn_channel) == 0x9470);
    assert!(core::mem::offset_of!(SystemSettings, mii_author_id) == 0x94B0);
    assert!(core::mem::offset_of!(SystemSettings, nx_controller_settings_count) == 0x94F0);
    assert!(core::mem::offset_of!(SystemSettings, nx_controller_legacy_settings) == 0x9500);
    assert!(core::mem::offset_of!(SystemSettings, external_rtc_reset_flag) == 0x98F0);
    assert!(core::mem::offset_of!(SystemSettings, push_notification_activity_mode_on_sleep) == 0x9930);
    assert!(core::mem::offset_of!(SystemSettings, allowed_ssl_host_count) == 0x99F4);
    assert!(core::mem::offset_of!(SystemSettings, host_fs_mount_point) == 0x9A30);
    assert!(core::mem::offset_of!(SystemSettings, allowed_ssl_hosts) == 0x9B30);
    assert!(core::mem::offset_of!(SystemSettings, ble_pairing_settings_count) == 0xA9F0);
    assert!(core::mem::offset_of!(SystemSettings, ble_pairing_settings) == 0xAA00);
    assert!(core::mem::offset_of!(SystemSettings, account_online_storage_settings_count) == 0xAF00);
    assert!(core::mem::offset_of!(SystemSettings, account_online_storage_settings) == 0xAF10);
    assert!(core::mem::offset_of!(SystemSettings, pctl_ready_flag) == 0xB110);
    assert!(core::mem::offset_of!(SystemSettings, theme_id_type0) == 0xB150);
    assert!(core::mem::offset_of!(SystemSettings, chinese_traditional_input_method) == 0xB350);
    assert!(core::mem::offset_of!(SystemSettings, button_config_registered_settings_count) == 0xB3D0);
    assert!(core::mem::offset_of!(SystemSettings, button_config_settings_count) == 0xB3E0);
    assert!(core::mem::offset_of!(SystemSettings, button_config_settings) == 0xB3E8);
    assert!(core::mem::offset_of!(SystemSettings, button_config_registered_settings_embedded) == 0x1D3E0);
    assert!(core::mem::offset_of!(SystemSettings, console_six_axis_sensor_acceleration_bias) == 0x29370);
    assert!(core::mem::offset_of!(SystemSettings, lock_screen_flag) == 0x29470);
    assert!(core::mem::offset_of!(SystemSettings, battery_percentage_flag) == 0x294A0);
    assert!(core::mem::offset_of!(SystemSettings, field_testing_flag) == 0x294C0);
    assert!(core::mem::offset_of!(SystemSettings, backlight_settings_mixed_up) == 0x294F0);
    assert!(core::mem::offset_of!(SystemSettings, user_system_clock_context) == 0x29580);
    assert!(core::mem::offset_of!(SystemSettings, network_system_clock_context) == 0x295A0);
    assert!(core::mem::offset_of!(SystemSettings, user_system_clock_automatic_correction_enabled) == 0x295C0);
    assert!(core::mem::offset_of!(SystemSettings, user_system_clock_automatic_correction_updated_time_point) == 0x295C8);
    assert!(core::mem::offset_of!(SystemSettings, account_settings) == 0x295F0);
    assert!(core::mem::offset_of!(SystemSettings, audio_volume_type0) == 0x296F0);
    assert!(core::mem::offset_of!(SystemSettings, hearing_protection_safeguard_remaining_time) == 0x29730);
    assert!(core::mem::offset_of!(SystemSettings, automatic_application_download_flag) == 0x297B0);
    assert!(core::mem::offset_of!(SystemSettings, notification_settings) == 0x297B8);
    assert!(core::mem::offset_of!(SystemSettings, account_notification_settings) == 0x29840);
    assert!(core::mem::offset_of!(SystemSettings, vibration_master_volume) == 0x29A40);
    assert!(core::mem::offset_of!(SystemSettings, analog_stick_user_calibration_left) == 0x29A48);
    assert!(core::mem::offset_of!(SystemSettings, touch_screen_mode) == 0x29A68);
    assert!(core::mem::offset_of!(SystemSettings, edid) == 0x29AA0);
    assert!(core::mem::offset_of!(SystemSettings, data_deletion_settings) == 0x29E80);
    assert!(core::mem::offset_of!(SystemSettings, requires_run_repair_time_reviser) == 0x29ED4);
    assert!(core::mem::offset_of!(SystemSettings, device_time_zone_location_name) == 0x29F40);
    assert!(core::mem::offset_of!(SystemSettings, nfc_enable_flag) == 0x2A0C0);
    assert!(core::mem::offset_of!(SystemSettings, eula_version_count) == 0x2A140);
    assert!(core::mem::offset_of!(SystemSettings, device_nick_name) == 0x2A950);
    assert!(core::mem::offset_of!(SystemSettings, bluetooth_device_settings_last_14) == 0x2AAA0);
    assert!(core::mem::offset_of!(SystemSettings, nx_controller_settings_data_from_offset_30) == 0x2E6A0);
    assert!(core::mem::size_of::<SystemSettings>() == 0x336A0);
};

impl Default for SystemSettings {
    fn default() -> Self {
        // SAFETY: All-zero is valid for this repr(C) struct of plain data types.
        unsafe { core::mem::zeroed() }
    }
}

impl core::fmt::Debug for SystemSettings {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("SystemSettings")
            .field("version", &self.version)
            .field("flags", &self.flags)
            .field("language_code", &self.language_code)
            .finish()
    }
}

impl SystemSettings {
    pub fn new() -> Self {
        Self::default()
    }
}
