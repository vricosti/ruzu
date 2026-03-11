// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/audio_controller.h
//! Port of zuyu/src/core/hle/service/am/service/audio_controller.cpp

/// IPC command table for IAudioController:
/// - 0: SetExpectedMasterVolume
/// - 1: GetMainAppletExpectedMasterVolume
/// - 2: GetLibraryAppletExpectedMasterVolume
/// - 3: ChangeMainAppletMasterVolume
/// - 4: SetTransparentVolumeRate
pub struct IAudioController {
    main_applet_volume: f32,
    library_applet_volume: f32,
    transparent_volume_rate: f32,
    /// Volume transition fade time in nanoseconds.
    fade_time_ns: i64,
}

const MIN_ALLOWED_VOLUME: f32 = 0.0;
const MAX_ALLOWED_VOLUME: f32 = 1.0;

impl IAudioController {
    pub fn new() -> Self {
        Self {
            main_applet_volume: 0.25,
            library_applet_volume: MAX_ALLOWED_VOLUME,
            transparent_volume_rate: MIN_ALLOWED_VOLUME,
            fade_time_ns: 0,
        }
    }

    /// Port of IAudioController::SetExpectedMasterVolume
    pub fn set_expected_master_volume(
        &mut self,
        main_applet_volume: f32,
        library_applet_volume: f32,
    ) {
        log::debug!(
            "SetExpectedMasterVolume called. main={}, library={}",
            main_applet_volume,
            library_applet_volume
        );
        self.main_applet_volume =
            main_applet_volume.clamp(MIN_ALLOWED_VOLUME, MAX_ALLOWED_VOLUME);
        self.library_applet_volume =
            library_applet_volume.clamp(MIN_ALLOWED_VOLUME, MAX_ALLOWED_VOLUME);
    }

    /// Port of IAudioController::GetMainAppletExpectedMasterVolume
    pub fn get_main_applet_expected_master_volume(&self) -> f32 {
        log::debug!(
            "GetMainAppletExpectedMasterVolume called. volume={}",
            self.main_applet_volume
        );
        self.main_applet_volume
    }

    /// Port of IAudioController::GetLibraryAppletExpectedMasterVolume
    pub fn get_library_applet_expected_master_volume(&self) -> f32 {
        log::debug!(
            "GetLibraryAppletExpectedMasterVolume called. volume={}",
            self.library_applet_volume
        );
        self.library_applet_volume
    }

    /// Port of IAudioController::ChangeMainAppletMasterVolume
    pub fn change_main_applet_master_volume(&mut self, volume: f32, fade_time_ns: i64) {
        log::debug!(
            "ChangeMainAppletMasterVolume called. volume={}, fade_time_ns={}",
            volume,
            fade_time_ns
        );
        self.main_applet_volume = volume.clamp(MIN_ALLOWED_VOLUME, MAX_ALLOWED_VOLUME);
        self.fade_time_ns = fade_time_ns;
    }

    /// Port of IAudioController::SetTransparentVolumeRate
    pub fn set_transparent_volume_rate(&mut self, transparent_volume_rate: f32) {
        log::debug!(
            "SetTransparentVolumeRate called. rate={}",
            transparent_volume_rate
        );
        self.transparent_volume_rate =
            transparent_volume_rate.clamp(MIN_ALLOWED_VOLUME, MAX_ALLOWED_VOLUME);
    }
}
