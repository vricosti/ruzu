// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/olsc_service_for_application.h
//! Port of zuyu/src/core/hle/service/olsc/olsc_service_for_application.cpp
//!
//! IOlscServiceForApplication: "olsc:u" service implementation.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// IPC command table for IOlscServiceForApplication.
///
/// | Cmd  | Handler                            | Name                                         |
/// |------|------------------------------------|----------------------------------------------|
/// | 0    | Initialize                         | Initialize                                   |
/// | 10   | nullptr                            | VerifySaveDataBackupLicenseAsync              |
/// | 13   | GetSaveDataBackupSetting           | GetSaveDataBackupSetting                     |
/// | 14   | SetSaveDataBackupSettingEnabled     | SetSaveDataBackupSettingEnabled               |
/// | 15   | nullptr                            | SetCustomData                                |
/// | 16   | nullptr                            | DeleteSaveDataBackupSetting                  |
/// | 18   | nullptr                            | GetSaveDataBackupInfoCache                   |
/// | 19   | nullptr                            | UpdateSaveDataBackupInfoCacheAsync            |
/// | 22   | nullptr                            | DeleteSaveDataBackupAsync                    |
/// | 25   | nullptr                            | ListDownloadableSaveDataBackupInfoAsync       |
/// | 26   | nullptr                            | DownloadSaveDataBackupAsync                  |
/// | 27   | nullptr                            | UploadSaveDataBackupAsync                    |
/// | 9010 | nullptr                            | VerifySaveDataBackupLicenseAsyncForDebug      |
/// | 9013 | nullptr                            | GetSaveDataBackupSettingForDebug             |
/// | 9014 | nullptr                            | SetSaveDataBackupSettingEnabledForDebug       |
/// | 9015 | nullptr                            | SetCustomDataForDebug                        |
/// | 9016 | nullptr                            | DeleteSaveDataBackupSettingForDebug           |
/// | 9018 | nullptr                            | GetSaveDataBackupInfoCacheForDebug           |
/// | 9019 | nullptr                            | UpdateSaveDataBackupInfoCacheAsyncForDebug    |
/// | 9022 | nullptr                            | DeleteSaveDataBackupAsyncForDebug            |
/// | 9025 | nullptr                            | ListDownloadableSaveDataBackupInfoAsyncForDebug |
/// | 9026 | nullptr                            | DownloadSaveDataBackupAsyncForDebug          |
pub struct IOlscServiceForApplication {
    initialized: bool,
}

impl IOlscServiceForApplication {
    pub fn new() -> Self {
        Self { initialized: false }
    }

    /// Cmd 0: Initialize
    pub fn initialize(&mut self, _process_id: u64) -> ResultCode {
        log::warn!("(STUBBED) IOlscServiceForApplication::initialize called");
        self.initialized = true;
        RESULT_SUCCESS
    }

    /// Cmd 13: GetSaveDataBackupSetting
    ///
    /// Returns the save data backup setting. Upstream returns 0 (stubbed).
    pub fn get_save_data_backup_setting(&self) -> (ResultCode, u8) {
        log::warn!("(STUBBED) IOlscServiceForApplication::get_save_data_backup_setting called");
        // backup_setting is set to 0 since real value is unknown
        (RESULT_SUCCESS, 0)
    }

    /// Cmd 14: SetSaveDataBackupSettingEnabled
    pub fn set_save_data_backup_setting_enabled(
        &self,
        enabled: bool,
        account_id: u128,
    ) -> ResultCode {
        log::warn!(
            "(STUBBED) IOlscServiceForApplication::set_save_data_backup_setting_enabled called, enabled={}, account_id={:032X}",
            enabled,
            account_id
        );
        RESULT_SUCCESS
    }
}
