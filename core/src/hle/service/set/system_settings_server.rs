//! Port of zuyu/src/core/hle/service/set/system_settings_server.h and system_settings_server.cpp
//!
//! ISystemSettingsServer service ("set:sys").

/// Settings file version, matching upstream SETTINGS_VERSION.
const _SETTINGS_VERSION: u32 = 4;

/// IPC command table for ISystemSettingsServer ("set:sys"):
///
/// | Cmd  | Name                                              |
/// |------|---------------------------------------------------|
/// | 0    | SetLanguageCode                                   |
/// | 3    | GetFirmwareVersion                                |
/// | 4    | GetFirmwareVersion2                               |
/// | 7    | GetLockScreenFlag                                 |
/// | 8    | SetLockScreenFlag                                 |
/// | 9    | GetExternalSteadyClockSourceId                    |
/// | 10   | SetExternalSteadyClockSourceId                    |
/// | 17   | GetUserSystemClockContext                         |
/// | 18   | SetUserSystemClockContext                         |
/// | 19   | GetAccountSettings                                |
/// | 20   | SetAccountSettings                                |
/// | 21   | GetEulaVersions                                   |
/// | 22   | SetEulaVersions                                   |
/// | 23   | GetColorSetId                                     |
/// | 24   | SetColorSetId                                     |
/// | 25   | GetNotificationSettings                           |
/// | 26   | SetNotificationSettings                           |
/// | 27   | GetAccountNotificationSettings                    |
/// | 28   | SetAccountNotificationSettings                    |
/// | 29   | GetVibrationMasterVolume                          |
/// | 30   | SetVibrationMasterVolume                          |
/// | 37   | GetSettingsItemValueSize                           |
/// | 38   | GetSettingsItemValue                               |
/// | 39   | GetTvSettings                                      |
/// | 40   | SetTvSettings                                      |
/// | 41   | GetEdid                                            |
/// | 42   | SetEdid                                            |
/// | 43   | GetAudioOutputMode                                 |
/// | 44   | SetAudioOutputMode                                 |
/// | 45   | GetSpeakerAutoMuteFlag                             |
/// | 46   | SetSpeakerAutoMuteFlag                             |
/// | 47   | GetQuestFlag                                       |
/// | 48   | SetQuestFlag                                       |
/// | 49   | GetDeviceTimeZoneLocationName                      |
/// | 50   | SetDeviceTimeZoneLocationName                      |
/// | 57   | SetRegionCode                                      |
/// | 58   | GetNetworkSystemClockContext                        |
/// | 59   | SetNetworkSystemClockContext                        |
/// | 60   | IsUserSystemClockAutomaticCorrectionEnabled         |
/// | 61   | SetUserSystemClockAutomaticCorrectionEnabled        |
/// | 62   | GetDebugModeFlag                                    |
/// | 63   | GetPrimaryAlbumStorage                              |
/// | 64   | SetPrimaryAlbumStorage                              |
/// | 68   | GetBatteryLot                                       |
/// | 69   | GetSerialNumber                                     |
/// | 70   | GetNfcEnableFlag                                    |
/// | 71   | SetNfcEnableFlag                                    |
/// | 73   | GetSleepSettings                                    |
/// | 74   | SetSleepSettings                                    |
/// | 75   | GetWirelessLanEnableFlag                             |
/// | 76   | SetWirelessLanEnableFlag                             |
/// | 77   | GetInitialLaunchSettings                             |
/// | 78   | SetInitialLaunchSettings                             |
/// | 79   | GetDeviceNickName                                    |
/// | 80   | SetDeviceNickName                                    |
/// | 81   | GetProductModel                                      |
/// | 88   | GetBluetoothEnableFlag                               |
/// | 89   | SetBluetoothEnableFlag                               |
/// | 90   | GetMiiAuthorId                                       |
/// | 95   | GetAutoUpdateEnableFlag                              |
/// | 96   | SetAutoUpdateEnableFlag                              |
/// | 99   | GetBatteryPercentageFlag                             |
/// | 100  | SetBatteryPercentageFlag                             |
/// | 124  | SetExternalSteadyClockInternalOffset                 |
/// | 125  | GetExternalSteadyClockInternalOffset                 |
/// | 126  | GetPushNotificationActivityModeOnSleep               |
/// | 127  | SetPushNotificationActivityModeOnSleep               |
/// | 130  | GetErrorReportSharePermission                        |
/// | 131  | SetErrorReportSharePermission                        |
/// | 140  | GetAppletLaunchFlags                                 |
/// | 141  | SetAppletLaunchFlags                                 |
/// | 152  | GetKeyboardLayout                                    |
/// | 153  | SetKeyboardLayout                                    |
/// | 162  | GetDeviceTimeZoneLocationUpdatedTime                 |
/// | 163  | SetDeviceTimeZoneLocationUpdatedTime                 |
/// | 168  | GetUserSystemClockAutomaticCorrectionUpdatedTime      |
/// | 169  | SetUserSystemClockAutomaticCorrectionUpdatedTime      |
/// | 195  | GetChineseTraditionalInputMethod                      |
/// | 200  | GetHomeMenuScheme                                      |
/// | 201  | GetHomeMenuSchemeModel                                 |
/// | 203  | GetTouchScreenMode                                     |
/// | 207  | GetPlatformRegion                                      |
/// | 208  | SetPlatformRegion                                      |
/// | 211  | SetTouchScreenMode                                     |
/// | 215  | GetFieldTestingFlag                                    |
/// | 216  | GetPanelCrcMode                                        |
/// | 217  | SetPanelCrcMode                                        |
pub struct ISystemSettingsServer {
    _save_needed: bool,
}

impl ISystemSettingsServer {
    pub fn new() -> Self {
        Self {
            _save_needed: false,
        }
    }
}
