// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/olsc_service_for_application.h
//! Port of zuyu/src/core/hle/service/olsc/olsc_service_for_application.cpp
//!
//! IOlscServiceForApplication: "olsc:u" service implementation.

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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
    initialized: AtomicBool,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IOlscServiceForApplication {
    pub fn new() -> Self {
        Self {
            initialized: AtomicBool::new(false),
            handlers: build_handler_map(&[
                (0, Some(Self::initialize_handler), "Initialize"),
                (10, None, "VerifySaveDataBackupLicenseAsync"),
                (
                    13,
                    Some(Self::get_save_data_backup_setting_handler),
                    "GetSaveDataBackupSetting",
                ),
                (
                    14,
                    Some(Self::set_save_data_backup_setting_enabled_handler),
                    "SetSaveDataBackupSettingEnabled",
                ),
                (15, None, "SetCustomData"),
                (16, None, "DeleteSaveDataBackupSetting"),
                (18, None, "GetSaveDataBackupInfoCache"),
                (19, None, "UpdateSaveDataBackupInfoCacheAsync"),
                (22, None, "DeleteSaveDataBackupAsync"),
                (25, None, "ListDownloadableSaveDataBackupInfoAsync"),
                (26, None, "DownloadSaveDataBackupAsync"),
                (27, None, "UploadSaveDataBackupAsync"),
                (9010, None, "VerifySaveDataBackupLicenseAsyncForDebug"),
                (9013, None, "GetSaveDataBackupSettingForDebug"),
                (9014, None, "SetSaveDataBackupSettingEnabledForDebug"),
                (9015, None, "SetCustomDataForDebug"),
                (9016, None, "DeleteSaveDataBackupSettingForDebug"),
                (9018, None, "GetSaveDataBackupInfoCacheForDebug"),
                (9019, None, "UpdateSaveDataBackupInfoCacheAsyncForDebug"),
                (9022, None, "DeleteSaveDataBackupAsyncForDebug"),
                (
                    9025,
                    None,
                    "ListDownloadableSaveDataBackupInfoAsyncForDebug",
                ),
                (9026, None, "DownloadSaveDataBackupAsyncForDebug"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Cmd 0: Initialize
    pub fn initialize(&self, _process_id: u64) -> ResultCode {
        log::warn!("(STUBBED) IOlscServiceForApplication::initialize called");
        self.initialized.store(true, Ordering::Relaxed);
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

    pub fn is_initialized(&self) -> bool {
        self.initialized.load(Ordering::Relaxed)
    }

    fn initialize_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IOlscServiceForApplication) };
        let mut rp = RequestParser::new(ctx);
        let process_id = rp.pop_u64();
        let result = service.initialize(process_id);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    fn get_save_data_backup_setting_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IOlscServiceForApplication) };
        let (result, setting) = service.get_save_data_backup_setting();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(result);
        rb.push_u32(setting as u32);
    }

    fn set_save_data_backup_setting_enabled_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IOlscServiceForApplication) };
        let mut rp = RequestParser::new(ctx);
        let enabled = rp.pop_bool();
        let account_id = rp.pop_raw::<u128>();
        let result = service.set_save_data_backup_setting_enabled(enabled, account_id);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }
}

impl SessionRequestHandler for IOlscServiceForApplication {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "olsc:u"
    }
}

impl ServiceFramework for IOlscServiceForApplication {
    fn get_service_name(&self) -> &str {
        "olsc:u"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn handler_table_matches_upstream() {
        let service = IOlscServiceForApplication::new();
        assert_eq!(service.handlers().len(), 22);
        assert!(service.handlers().contains_key(&0));
        assert!(service.handlers().contains_key(&13));
        assert!(service.handlers().contains_key(&9026));
    }

    #[test]
    fn initialize_sets_state() {
        let service = IOlscServiceForApplication::new();
        assert!(!service.is_initialized());
        assert_eq!(service.initialize(0), RESULT_SUCCESS);
        assert!(service.is_initialized());
    }
}
