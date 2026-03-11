// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/system_update_control.cpp/.h

pub const ISYSTEM_UPDATE_CONTROL_COMMANDS: &[(u32, &str)] = &[
    (0, "HasDownloaded"),
    (1, "RequestCheckLatestUpdate"),
    (2, "RequestDownloadLatestUpdate"),
    (3, "GetDownloadProgress"),
    (4, "ApplyDownloadedUpdate"),
    (5, "RequestPrepareCardUpdate"),
    (6, "GetPrepareCardUpdateProgress"),
    (7, "HasPreparedCardUpdate"),
    (8, "ApplyCardUpdate"),
    (9, "GetDownloadedEulaDataSize"),
    (10, "GetDownloadedEulaData"),
    (11, "SetupCardUpdate"),
    (12, "GetPreparedCardUpdateEulaDataSize"),
    (13, "GetPreparedCardUpdateEulaData"),
    (14, "SetupCardUpdateViaSystemUpdater"),
    (15, "HasReceived"),
    (16, "RequestReceiveSystemUpdate"),
    (17, "GetReceiveProgress"),
    (18, "ApplyReceivedUpdate"),
    (19, "GetReceivedEulaDataSize"),
    (20, "GetReceivedEulaData"),
    (21, "SetupToReceiveSystemUpdate"),
    (22, "RequestCheckLatestUpdateIncludesRebootlessUpdate"),
];
