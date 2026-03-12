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
/// Corresponds to the function table in upstream olsc_service_for_system_service.cpp constructor.
pub mod commands {
    pub const OPEN_TRANSFER_TASK_LIST_CONTROLLER: u32 = 0;
    pub const OPEN_REMOTE_STORAGE_CONTROLLER: u32 = 1;
    pub const OPEN_DAEMON_CONTROLLER: u32 = 2;
    pub const UNKNOWN_10: u32 = 10;
    pub const UNKNOWN_11: u32 = 11;
    pub const UNKNOWN_12: u32 = 12;
    pub const UNKNOWN_13: u32 = 13;
    pub const LIST_LAST_TRANSFER_TASK_ERROR_INFO: u32 = 100;
    pub const GET_LAST_ERROR_INFO_COUNT: u32 = 101;
    pub const REMOVE_LAST_ERROR_INFO_OLD: u32 = 102;
    pub const GET_LAST_ERROR_INFO: u32 = 103;
    pub const GET_LAST_ERROR_EVENT_HOLDER: u32 = 104;
    pub const GET_LAST_TRANSFER_TASK_ERROR_INFO: u32 = 105;
    pub const GET_DATA_TRANSFER_POLICY_INFO: u32 = 200;
    pub const REMOVE_DATA_TRANSFER_POLICY_INFO: u32 = 201;
    pub const UPDATE_DATA_TRANSFER_POLICY_OLD: u32 = 202;
    pub const UPDATE_DATA_TRANSFER_POLICY: u32 = 203;
    pub const CLEANUP_DATA_TRANSFER_POLICY_INFO: u32 = 204;
    pub const REQUEST_DATA_TRANSFER_POLICY: u32 = 205;
    pub const GET_AUTO_TRANSFER_SERIES_INFO: u32 = 300;
    pub const UPDATE_AUTO_TRANSFER_SERIES_INFO: u32 = 301;
    pub const CLEANUP_SAVE_DATA_ARCHIVE_INFO_TYPE1: u32 = 400;
    pub const CLEANUP_TRANSFER_TASK: u32 = 900;
    pub const CLEANUP_SERIES_INFO_TYPE0: u32 = 902;
    pub const CLEANUP_SAVE_DATA_ARCHIVE_INFO_TYPE0: u32 = 903;
    pub const CLEANUP_APPLICATION_AUTO_TRANSFER_SETTING: u32 = 904;
    pub const CLEANUP_ERROR_HISTORY: u32 = 905;
    pub const SET_LAST_ERROR: u32 = 906;
    pub const ADD_SAVE_DATA_ARCHIVE_INFO_TYPE0: u32 = 907;
    pub const REMOVE_SERIES_INFO_TYPE0: u32 = 908;
    pub const GET_SERIES_INFO_TYPE0: u32 = 909;
    pub const REMOVE_LAST_ERROR_INFO: u32 = 910;
    pub const CLONE_SERVICE: u32 = 10000;
}

/// IOlscServiceForSystemService ("olsc:s").
///
/// Corresponds to `IOlscServiceForSystemService` in upstream olsc_service_for_system_service.cpp.
pub struct IOlscServiceForSystemService;

impl IOlscServiceForSystemService {
    pub fn new() -> Self {
        IOlscServiceForSystemService
    }

    /// Cmd 0: OpenTransferTaskListController
    ///
    /// Corresponds to `IOlscServiceForSystemService::OpenTransferTaskListController` in upstream.
    pub fn open_transfer_task_list_controller(&self) -> (ResultCode, ITransferTaskListController) {
        log::info!("IOlscServiceForSystemService::open_transfer_task_list_controller called");
        (RESULT_SUCCESS, ITransferTaskListController::new())
    }

    /// Cmd 1: OpenRemoteStorageController
    ///
    /// Corresponds to `IOlscServiceForSystemService::OpenRemoteStorageController` in upstream.
    pub fn open_remote_storage_controller(&self) -> (ResultCode, IRemoteStorageController) {
        log::info!("IOlscServiceForSystemService::open_remote_storage_controller called");
        (RESULT_SUCCESS, IRemoteStorageController::new())
    }

    /// Cmd 2: OpenDaemonController
    ///
    /// Corresponds to `IOlscServiceForSystemService::OpenDaemonController` in upstream.
    pub fn open_daemon_controller(&self) -> (ResultCode, IDaemonController) {
        log::info!("IOlscServiceForSystemService::open_daemon_controller called");
        (RESULT_SUCCESS, IDaemonController::new())
    }

    /// Cmd 200: GetDataTransferPolicyInfo
    ///
    /// Corresponds to `IOlscServiceForSystemService::GetDataTransferPolicyInfo` in upstream.
    pub fn get_data_transfer_policy_info(&self, _application_id: u64) -> (ResultCode, u16) {
        log::warn!("(STUBBED) IOlscServiceForSystemService::get_data_transfer_policy_info called");
        (RESULT_SUCCESS, 0)
    }

    /// Cmd 10000: CloneService
    ///
    /// Corresponds to `IOlscServiceForSystemService::CloneService` in upstream.
    /// Upstream returns shared_from_this(); we create a new instance since state is minimal.
    pub fn clone_service(&self) -> (ResultCode, IOlscServiceForSystemService) {
        log::info!("IOlscServiceForSystemService::clone_service called");
        (RESULT_SUCCESS, IOlscServiceForSystemService::new())
    }
}
