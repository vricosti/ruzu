// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/olsc_service_for_system_service.h
//! Port of zuyu/src/core/hle/service/olsc/olsc_service_for_system_service.cpp
//!
//! IOlscServiceForSystemService: "olsc:s" service implementation.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

use super::daemon_controller::IDaemonController;
use super::remote_storage_controller::IRemoteStorageController;
use super::transfer_task_list_controller::ITransferTaskListController;

/// IPC command table for IOlscServiceForSystemService.
///
/// | Cmd   | Handler                          | Name                                  |
/// |-------|----------------------------------|---------------------------------------|
/// | 0     | OpenTransferTaskListController   | OpenTransferTaskListController        |
/// | 1     | OpenRemoteStorageController      | OpenRemoteStorageController           |
/// | 2     | OpenDaemonController             | OpenDaemonController                  |
/// | 200   | GetDataTransferPolicyInfo        | GetDataTransferPolicyInfo             |
/// | 10000 | CloneService                     | CloneService                          |
pub struct IOlscServiceForSystemService;

impl IOlscServiceForSystemService {
    pub fn new() -> Self {
        IOlscServiceForSystemService
    }

    /// Cmd 0: OpenTransferTaskListController
    pub fn open_transfer_task_list_controller(&self) -> (ResultCode, ITransferTaskListController) {
        log::info!("IOlscServiceForSystemService::open_transfer_task_list_controller called");
        (RESULT_SUCCESS, ITransferTaskListController::new())
    }

    /// Cmd 1: OpenRemoteStorageController
    pub fn open_remote_storage_controller(&self) -> (ResultCode, IRemoteStorageController) {
        log::info!("IOlscServiceForSystemService::open_remote_storage_controller called");
        (RESULT_SUCCESS, IRemoteStorageController::new())
    }

    /// Cmd 2: OpenDaemonController
    pub fn open_daemon_controller(&self) -> (ResultCode, IDaemonController) {
        log::info!("IOlscServiceForSystemService::open_daemon_controller called");
        (RESULT_SUCCESS, IDaemonController::new())
    }

    /// Cmd 200: GetDataTransferPolicyInfo
    pub fn get_data_transfer_policy_info(&self, _application_id: u64) -> (ResultCode, u16) {
        log::warn!("(STUBBED) IOlscServiceForSystemService::get_data_transfer_policy_info called");
        (RESULT_SUCCESS, 0)
    }

    /// Cmd 10000: CloneService
    ///
    /// Returns a new instance of IOlscServiceForSystemService.
    /// Upstream returns shared_from_this(); we create a new instance since state is minimal.
    pub fn clone_service(&self) -> (ResultCode, IOlscServiceForSystemService) {
        log::info!("IOlscServiceForSystemService::clone_service called");
        (RESULT_SUCCESS, IOlscServiceForSystemService::new())
    }
}
