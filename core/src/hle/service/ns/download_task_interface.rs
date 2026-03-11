// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/download_task_interface.cpp/.h

pub const IDOWNLOAD_TASK_INTERFACE_COMMANDS: &[(u32, bool, &str)] = &[
    (701, false, "ClearTaskStatusList"),
    (702, false, "RequestDownloadTaskList"),
    (703, false, "RequestEnsureDownloadTask"),
    (704, false, "ListDownloadTaskStatus"),
    (705, false, "RequestDownloadTaskListData"),
    (706, false, "TryCommitCurrentApplicationDownloadTask"),
    (707, true, "EnableAutoCommit"),
    (708, true, "DisableAutoCommit"),
    (709, false, "TriggerDynamicCommitEvent"),
];

pub fn enable_auto_commit() {
    log::warn!("(STUBBED) IDownloadTaskInterface::EnableAutoCommit called");
}

pub fn disable_auto_commit() {
    log::warn!("(STUBBED) IDownloadTaskInterface::DisableAutoCommit called");
}
