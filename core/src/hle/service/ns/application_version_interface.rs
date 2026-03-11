// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/application_version_interface.cpp/.h

pub const IAPPLICATION_VERSION_INTERFACE_COMMANDS: &[(u32, &str)] = &[
    (0, "GetLaunchRequiredVersion"),
    (1, "UpgradeLaunchRequiredVersion"),
    (35, "UpdateVersionList"),
    (36, "PushLaunchVersion"),
    (37, "ListRequiredVersion"),
    (800, "RequestVersionList"),
    (801, "ListVersionList"),
    (802, "RequestVersionListData"),
    (900, "ImportAutoUpdatePolicyJsonForDebug"),
    (901, "ListDefaultAutoUpdatePolicy"),
    (902, "ListAutoUpdatePolicyForSpecificApplication"),
    (1000, "PerformAutoUpdate"),
    (1001, "ListAutoUpdateSchedule"),
];
