// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/factory_reset_interface.cpp/.h

pub const IFACTORY_RESET_INTERFACE_COMMANDS: &[(u32, &str)] = &[
    (100, "ResetToFactorySettings"),
    (101, "ResetToFactorySettingsWithoutUserSaveData"),
    (102, "ResetToFactorySettingsForRefurbishment"),
    (103, "ResetToFactorySettingsWithPlatformRegion"),
    (104, "ResetToFactorySettingsWithPlatformRegionAuthentication"),
    (105, "RequestResetToFactorySettingsSecurely"),
    (106, "RequestResetToFactorySettingsWithPlatformRegionAuthenticationSecurely"),
];
