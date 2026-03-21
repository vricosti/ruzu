// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/olsc_service_for_system_service.h
//! Port of zuyu/src/core/hle/service/olsc/olsc_service_for_system_service.cpp
//!
//! IOlscServiceForSystemService: "olsc:s" service implementation.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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
    pub const CLEANUP_SERIES_INFO_TYPE1: u32 = 911;
    pub const REMOVE_SERIES_INFO_TYPE1: u32 = 912;
    pub const GET_SERIES_INFO_TYPE1: u32 = 913;
    pub const UPDATE_ISSUE_OLD: u32 = 1000;
    pub const UNKNOWN_1010: u32 = 1010;
    pub const LIST_ISSUE_INFO_OLD: u32 = 1011;
    pub const GET_ISSUE_OLD: u32 = 1012;
    pub const GET_ISSUE2_OLD: u32 = 1013;
    pub const GET_ISSUE3_OLD: u32 = 1014;
    pub const REPAIR_ISSUE_OLD: u32 = 1020;
    pub const REPAIR_ISSUE_WITH_USER_ID_OLD: u32 = 1021;
    pub const REPAIR_ISSUE2_OLD: u32 = 1022;
    pub const REPAIR_ISSUE3_OLD: u32 = 1023;
    pub const UNKNOWN_1024: u32 = 1024;
    pub const UPDATE_ISSUE: u32 = 1100;
    pub const UNKNOWN_1110: u32 = 1110;
    pub const LIST_ISSUE_INFO: u32 = 1111;
    pub const GET_ISSUE: u32 = 1112;
    pub const GET_ISSUE2: u32 = 1113;
    pub const GET_ISSUE3: u32 = 1114;
    pub const REPAIR_ISSUE: u32 = 1120;
    pub const REPAIR_ISSUE_WITH_USER_ID: u32 = 1121;
    pub const REPAIR_ISSUE2: u32 = 1122;
    pub const REPAIR_ISSUE3: u32 = 1123;
    pub const UNKNOWN_1124: u32 = 1124;
    pub const CLONE_SERVICE: u32 = 10000;
}

/// IOlscServiceForSystemService ("olsc:s").
///
/// Corresponds to `IOlscServiceForSystemService` in upstream olsc_service_for_system_service.cpp.
pub struct IOlscServiceForSystemService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    system: crate::core::SystemRef,
}

impl IOlscServiceForSystemService {
    pub fn new(system: crate::core::SystemRef) -> Self {
        let handlers = build_handler_map(&[
            (0, None, "OpenTransferTaskListController"),
            (1, None, "OpenRemoteStorageController"),
            (2, None, "OpenDaemonController"),
            (10, None, "Unknown10"),
            (11, None, "Unknown11"),
            (12, None, "Unknown12"),
            (13, None, "Unknown13"),
            (100, None, "ListLastTransferTaskErrorInfo"),
            (101, None, "GetLastErrorInfoCount"),
            (102, None, "RemoveLastErrorInfoOld"),
            (103, None, "GetLastErrorInfo"),
            (104, None, "GetLastErrorEventHolder"),
            (105, None, "GetLastTransferTaskErrorInfo"),
            (200, None, "GetDataTransferPolicyInfo"),
            (201, None, "RemoveDataTransferPolicyInfo"),
            (202, None, "UpdateDataTransferPolicyOld"),
            (203, None, "UpdateDataTransferPolicy"),
            (204, None, "CleanupDataTransferPolicyInfo"),
            (205, None, "RequestDataTransferPolicy"),
            (300, None, "GetAutoTransferSeriesInfo"),
            (301, None, "UpdateAutoTransferSeriesInfo"),
            (400, None, "CleanupSaveDataArchiveInfoType1"),
            (900, None, "CleanupTransferTask"),
            (902, None, "CleanupSeriesInfoType0"),
            (903, None, "CleanupSaveDataArchiveInfoType0"),
            (904, None, "CleanupApplicationAutoTransferSetting"),
            (905, None, "CleanupErrorHistory"),
            (906, None, "SetLastError"),
            (907, None, "AddSaveDataArchiveInfoType0"),
            (908, None, "RemoveSeriesInfoType0"),
            (909, None, "GetSeriesInfoType0"),
            (910, None, "RemoveLastErrorInfo"),
            (911, None, "CleanupSeriesInfoType1"),
            (912, None, "RemoveSeriesInfoType1"),
            (913, None, "GetSeriesInfoType1"),
            (1000, None, "UpdateIssueOld"),
            (1010, None, "Unknown1010"),
            (1011, None, "ListIssueInfoOld"),
            (1012, None, "GetIssueOld"),
            (1013, None, "GetIssue2Old"),
            (1014, None, "GetIssue3Old"),
            (1020, None, "RepairIssueOld"),
            (1021, None, "RepairIssueWithUserIdOld"),
            (1022, None, "RepairIssue2Old"),
            (1023, None, "RepairIssue3Old"),
            (1024, None, "Unknown1024"),
            (1100, None, "UpdateIssue"),
            (1110, None, "Unknown1110"),
            (1111, None, "ListIssueInfo"),
            (1112, None, "GetIssue"),
            (1113, None, "GetIssue2"),
            (1114, None, "GetIssue3"),
            (1120, None, "RepairIssue"),
            (1121, None, "RepairIssueWithUserId"),
            (1122, None, "RepairIssue2"),
            (1123, None, "RepairIssue3"),
            (1124, None, "Unknown1124"),
            (10000, None, "CloneService"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            system,
        }
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
        (RESULT_SUCCESS, IRemoteStorageController::new(self.system))
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
        (RESULT_SUCCESS, IOlscServiceForSystemService::new(self.system))
    }
}

impl SessionRequestHandler for IOlscServiceForSystemService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "olsc:s"
    }
}

impl ServiceFramework for IOlscServiceForSystemService {
    fn get_service_name(&self) -> &str {
        "olsc:s"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
