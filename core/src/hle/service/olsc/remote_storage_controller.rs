// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/remote_storage_controller.h
//! Port of zuyu/src/core/hle/service/olsc/remote_storage_controller.cpp
//!
//! IRemoteStorageController: manages remote save data storage operations.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// IPC command table for IRemoteStorageController.
///
/// | Cmd | Handler          | Name                     |
/// |-----|------------------|--------------------------|
/// | 22  | GetSecondarySave | GetSecondarySave         |
/// (All other commands are nullptr / unimplemented)
pub struct IRemoteStorageController;

impl IRemoteStorageController {
    pub fn new() -> Self {
        IRemoteStorageController
    }

    /// Cmd 22: GetSecondarySave
    ///
    /// Returns whether a secondary save exists for the given application.
    /// Upstream always returns false with zeroed output (stubbed).
    pub fn get_secondary_save(
        &self,
        application_id: u64,
    ) -> (ResultCode, bool, [u64; 3]) {
        log::error!(
            "(STUBBED) IRemoteStorageController::get_secondary_save called, application_id={:016X}",
            application_id
        );
        (RESULT_SUCCESS, false, [0u64; 3])
    }
}
