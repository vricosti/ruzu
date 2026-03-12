// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/daemon_controller.h
//! Port of zuyu/src/core/hle/service/olsc/daemon_controller.cpp
//!
//! IDaemonController: manages auto-transfer settings for accounts.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// IPC command table for IDaemonController.
///
/// | Cmd | Handler                                          | Name                                           |
/// |-----|--------------------------------------------------|------------------------------------------------|
/// | 0   | GetAutoTransferEnabledForAccountAndApplication   | GetAutoTransferEnabledForAccountAndApplication |
/// | 1   | nullptr                                          | SetAutoTransferEnabledForAccountAndApplication |
/// | 2   | nullptr                                          | GetGlobalUploadEnabledForAccount               |
/// | 3   | nullptr                                          | SetGlobalUploadEnabledForAccount               |
/// | 4   | nullptr                                          | TouchAccount                                   |
/// | 5   | nullptr                                          | GetGlobalDownloadEnabledForAccount             |
/// | 6   | nullptr                                          | SetGlobalDownloadEnabledForAccount             |
/// | 10  | nullptr                                          | GetForbiddenSaveDataIndication                 |
/// | 11  | nullptr                                          | GetStopperObject                               |
/// | 12  | nullptr                                          | GetState                                       |
pub struct IDaemonController;

impl IDaemonController {
    pub fn new() -> Self {
        IDaemonController
    }

    /// Cmd 0: GetAutoTransferEnabledForAccountAndApplication
    ///
    /// Returns whether auto-transfer is enabled for the given user and application.
    /// Upstream always returns false (stubbed).
    pub fn get_auto_transfer_enabled_for_account_and_application(
        &self,
        user_id: u128,
        application_id: u64,
    ) -> (ResultCode, bool) {
        log::warn!(
            "(STUBBED) IDaemonController::get_auto_transfer_enabled_for_account_and_application called, user_id={:032X}, application_id={:016X}",
            user_id,
            application_id
        );
        (RESULT_SUCCESS, false)
    }
}
