// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/ecommerce_interface.cpp/.h

pub const IECOMMERCE_INTERFACE_COMMANDS: &[(u32, &str)] = &[
    (0, "RequestLinkDevice"),
    (1, "RequestCleanupAllPreInstalledApplications"),
    (2, "RequestCleanupPreInstalledApplication"),
    (3, "RequestSyncRights"),
    (4, "RequestUnlinkDevice"),
    (5, "RequestRevokeAllELicense"),
    (6, "RequestSyncRightsBasedOnAssignedELicenses"),
];
