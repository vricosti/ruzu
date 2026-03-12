// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/transfer_task_list_controller.h
//! Port of zuyu/src/core/hle/service/olsc/transfer_task_list_controller.cpp
//!
//! ITransferTaskListController: manages transfer task lists.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

use super::native_handle_holder::INativeHandleHolder;

/// IPC command table for ITransferTaskListController.
///
/// | Cmd | Handler              | Name                     |
/// |-----|----------------------|--------------------------|
/// | 5   | GetNativeHandleHolder| GetNativeHandleHolder    |
/// | 9   | GetNativeHandleHolder| GetNativeHandleHolder2   |
/// (All other commands are nullptr / unimplemented)
pub struct ITransferTaskListController;

impl ITransferTaskListController {
    pub fn new() -> Self {
        ITransferTaskListController
    }

    /// Cmd 5 / Cmd 9: GetNativeHandleHolder
    ///
    /// Returns a new INativeHandleHolder instance.
    pub fn get_native_handle_holder(&self) -> (ResultCode, INativeHandleHolder) {
        log::warn!("(STUBBED) ITransferTaskListController::get_native_handle_holder called");
        (RESULT_SUCCESS, INativeHandleHolder::new())
    }
}
