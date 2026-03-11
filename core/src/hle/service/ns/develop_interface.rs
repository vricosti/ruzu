// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/develop_interface.cpp/.h

pub const IDEVELOP_INTERFACE_COMMANDS: &[(u32, &str)] = &[
    (0, "LaunchProgram"),
    (1, "TerminateProcess"),
    (2, "TerminateProgram"),
    (4, "GetShellEvent"),
    (5, "GetShellEventInfo"),
    (6, "TerminateApplication"),
    (7, "PrepareLaunchProgramFromHost"),
    (8, "LaunchApplicationFromHost"),
    (9, "LaunchApplicationWithStorageIdForDevelop"),
    (10, "IsSystemMemoryResourceLimitBoosted"),
    (11, "GetRunningApplicationProcessIdForDevelop"),
    (12, "SetCurrentApplicationRightsEnvironmentCanBeActiveForDevelop"),
    (13, "CreateApplicationResourceForDevelop"),
    (14, "IsPreomiaForDevelop"),
    (15, "GetApplicationProgramIdFromHost"),
    (16, "RefreshCachedDebugValues"),
    (17, "PrepareLaunchApplicationFromHost"),
    (18, "GetLaunchEvent"),
    (19, "GetLaunchResult"),
];
