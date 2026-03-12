// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/download_task_interface.h
//! Port of zuyu/src/core/hle/service/ns/download_task_interface.cpp
//!
//! IDownloadTaskInterface — download task management for NS.

use crate::hle::result::ResultCode;

/// IPC command table for IDownloadTaskInterface.
///
/// Corresponds to the function table in upstream download_task_interface.cpp.
pub mod commands {
    pub const CLEAR_TASK_STATUS_LIST: u32 = 701;
    pub const REQUEST_DOWNLOAD_TASK_LIST: u32 = 702;
    pub const REQUEST_ENSURE_DOWNLOAD_TASK: u32 = 703;
    pub const LIST_DOWNLOAD_TASK_STATUS: u32 = 704;
    pub const REQUEST_DOWNLOAD_TASK_LIST_DATA: u32 = 705;
    pub const TRY_COMMIT_CURRENT_APPLICATION_DOWNLOAD_TASK: u32 = 706;
    pub const ENABLE_AUTO_COMMIT: u32 = 707;
    pub const DISABLE_AUTO_COMMIT: u32 = 708;
    pub const TRIGGER_DYNAMIC_COMMIT_EVENT: u32 = 709;
}

/// IDownloadTaskInterface.
///
/// Corresponds to `IDownloadTaskInterface` in upstream.
pub struct IDownloadTaskInterface;

impl IDownloadTaskInterface {
    pub fn new() -> Self {
        Self
    }

    /// EnableAutoCommit (cmd 707).
    ///
    /// Corresponds to upstream `IDownloadTaskInterface::EnableAutoCommit`.
    pub fn enable_auto_commit(&self) -> Result<(), ResultCode> {
        log::warn!("(STUBBED) EnableAutoCommit called");
        Ok(())
    }

    /// DisableAutoCommit (cmd 708).
    ///
    /// Corresponds to upstream `IDownloadTaskInterface::DisableAutoCommit`.
    pub fn disable_auto_commit(&self) -> Result<(), ResultCode> {
        log::warn!("(STUBBED) DisableAutoCommit called");
        Ok(())
    }
}
