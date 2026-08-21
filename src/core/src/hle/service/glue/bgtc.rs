// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/bgtc.h
//! Port of zuyu/src/core/hle/service/glue/bgtc.cpp

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// IPC command IDs for BGTC_T
pub mod bgtc_t_commands {
    pub const OPEN_TASK_SERVICE: u32 = 100;
}

/// IPC command IDs for ITaskService
pub mod task_service_commands {
    pub const NOTIFY_TASK_STARTING: u32 = 1;
    pub const NOTIFY_TASK_FINISHED: u32 = 2;
    pub const GET_TRIGGER_EVENT: u32 = 3;
    pub const IS_IN_HALF_AWAKE: u32 = 4;
    pub const NOTIFY_CLIENT_NAME: u32 = 5;
    pub const IS_IN_FULL_AWAKE: u32 = 6;
    pub const SCHEDULE_TASK: u32 = 11;
    pub const GET_SCHEDULED_TASK_INTERVAL: u32 = 12;
    pub const UNSCHEDULE_TASK: u32 = 13;
    pub const GET_SCHEDULE_EVENT: u32 = 14;
    pub const SCHEDULE_PERIODIC_TASK: u32 = 15;
    pub const UNKNOWN_16: u32 = 16;
    pub const GET_OPERATION_MODE: u32 = 101;
    pub const WILL_DISCONNECT_NETWORK_WHEN_ENTERING_SLEEP: u32 = 102;
    pub const WILL_STAY_HALF_AWAKE_INSTEAD_SLEEP: u32 = 103;
    pub const UNKNOWN_200: u32 = 200;
}

/// IPC command IDs for BGTC_SC
pub mod bgtc_sc_commands {
    pub const GET_STATE: u32 = 1;
    pub const GET_STATE_CHANGED_EVENT: u32 = 2;
    pub const NOTIFY_ENTERING_HALF_AWAKE: u32 = 3;
    pub const NOTIFY_LEAVING_HALF_AWAKE: u32 = 4;
    pub const SET_IS_USING_SLEEP_UNSUPPORTED_DEVICES: u32 = 5;
}

/// BGTC_T service ("bgtc:t").
pub struct BgtcT;

impl BgtcT {
    pub fn new() -> Self {
        Self
    }

    /// Opens and returns an ITaskService instance.
    ///
    /// Upstream creates an ITaskService via PushIpcInterface. All ITaskService
    /// handlers are nullptr in upstream (unimplemented).
    pub fn open_task_service(&self) -> (ResultCode, ITaskService) {
        log::debug!("BGTC_T::open_task_service called");
        (RESULT_SUCCESS, ITaskService::new())
    }
}

/// ITaskService: background task service.
/// All handlers are nullptr (unimplemented) in upstream.
pub struct ITaskService;

impl ITaskService {
    pub fn new() -> Self {
        Self
    }
}

/// BGTC_SC service ("bgtc:sc").
/// All handlers are nullptr (unimplemented) in upstream.
pub struct BgtcSC;

impl BgtcSC {
    pub fn new() -> Self {
        Self
    }
}
