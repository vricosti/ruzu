//! Port of zuyu/src/core/hle/service/audio/audio_controller.h and audio_controller.cpp
//!
//! IAudioController service ("audctl").

/// Port of IAudioController::ForceMutePolicy
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ForceMutePolicy {
    Disable = 0,
    SpeakerMuteOnHeadphoneUnplugged = 1,
}

/// Port of IAudioController::HeadphoneOutputLevelMode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum HeadphoneOutputLevelMode {
    Normal = 0,
    HighPower = 1,
}

/// IPC command table for IAudioController ("audctl"):
///
/// | Cmd | Handler                                  | Name                                                  |
/// |-----|------------------------------------------|-------------------------------------------------------|
/// | 0   | nullptr                                  | GetTargetVolume                                       |
/// | 1   | nullptr                                  | SetTargetVolume                                       |
/// | 2   | get_target_volume_min                    | GetTargetVolumeMin                                    |
/// | 3   | get_target_volume_max                    | GetTargetVolumeMax                                    |
/// | 4   | nullptr                                  | IsTargetMute                                          |
/// | 5   | nullptr                                  | SetTargetMute                                         |
/// | 6   | nullptr                                  | IsTargetConnected                                     |
/// | 7   | nullptr                                  | SetDefaultTarget                                      |
/// | 8   | nullptr                                  | GetDefaultTarget                                      |
/// | 9   | get_audio_output_mode                    | GetAudioOutputMode                                    |
/// | 10  | set_audio_output_mode                    | SetAudioOutputMode                                    |
/// | 11  | nullptr                                  | SetForceMutePolicy                                    |
/// | 12  | get_force_mute_policy                    | GetForceMutePolicy                                    |
/// | 13  | get_output_mode_setting                  | GetOutputModeSetting                                  |
/// | 14  | set_output_mode_setting                  | SetOutputModeSetting                                  |
/// | 15  | nullptr                                  | SetOutputTarget                                       |
/// | 16  | nullptr                                  | SetInputTargetForceEnabled                             |
/// | 17  | set_headphone_output_level_mode          | SetHeadphoneOutputLevelMode                           |
/// | 18  | get_headphone_output_level_mode          | GetHeadphoneOutputLevelMode                           |
/// | 19  | nullptr                                  | AcquireAudioVolumeUpdateEventForPlayReport             |
/// | 20  | nullptr                                  | AcquireAudioOutputDeviceUpdateEventForPlayReport       |
/// | 21  | nullptr                                  | GetAudioOutputTargetForPlayReport                      |
/// | 22  | notify_headphone_volume_warning_displayed | NotifyHeadphoneVolumeWarningDisplayedEvent             |
/// | 23  | nullptr                                  | SetSystemOutputMasterVolume                            |
/// | 24  | nullptr                                  | GetSystemOutputMasterVolume                            |
/// | 25  | nullptr                                  | GetAudioVolumeDataForPlayReport                        |
/// | 26  | nullptr                                  | UpdateHeadphoneSettings                                |
/// | 27  | nullptr                                  | SetVolumeMappingTableForDev                            |
/// | 28  | nullptr                                  | GetAudioOutputChannelCountForPlayReport                |
/// | 29  | nullptr                                  | BindAudioOutputChannelCountUpdateEventForPlayReport    |
/// | 30  | set_speaker_auto_mute_enabled            | SetSpeakerAutoMuteEnabled                             |
/// | 31  | is_speaker_auto_mute_enabled             | IsSpeakerAutoMuteEnabled                              |
/// | 32  | nullptr                                  | GetActiveOutputTarget                                 |
/// | 33  | nullptr                                  | GetTargetDeviceInfo                                   |
/// | 34  | acquire_target_notification              | AcquireTargetNotification                             |
/// | 35-42, 10000-10106, 50000 | nullptr            | (various debug/play report commands)                  |
pub struct IAudioController {
    // TODO: Fields to be wired when service framework is available.
    // notification_event: KEvent,
    // m_set_sys: Arc<ISystemSettingsServer>,
}

impl IAudioController {
    pub fn new() -> Self {
        Self {}
    }

    /// Hardcoded to 0 as of FW 8.0.0.
    pub fn get_target_volume_min(&self) -> i32 {
        0
    }

    /// Hardcoded to 15 as of FW 8.0.0.
    pub fn get_target_volume_max(&self) -> i32 {
        15
    }

    /// Removed on FW 13.2.1+. Returns ForceMutePolicy::Disable.
    pub fn get_force_mute_policy(&self) -> ForceMutePolicy {
        ForceMutePolicy::Disable
    }

    pub fn get_headphone_output_level_mode(&self) -> HeadphoneOutputLevelMode {
        HeadphoneOutputLevelMode::Normal
    }

    pub fn set_headphone_output_level_mode(&self, _mode: HeadphoneOutputLevelMode) {
        log::warn!("(STUBBED) SetHeadphoneOutputLevelMode called");
    }

    pub fn notify_headphone_volume_warning_displayed_event(&self) {
        log::warn!("(STUBBED) NotifyHeadphoneVolumeWarningDisplayedEvent called");
    }
}
