// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/content_management_interface.cpp/.h

use super::ns_types::*;

pub const ICONTENT_MANAGEMENT_INTERFACE_COMMANDS: &[(u32, bool, &str)] = &[
    (11, true, "CalculateApplicationOccupiedSize"),
    (43, true, "CheckSdCardMountStatus"),
    (47, true, "GetTotalSpaceSize"),
    (48, true, "GetFreeSpaceSize"),
    (600, false, "CountApplicationContentMeta"),
    (601, false, "ListApplicationContentMetaStatus"),
    (605, false, "ListApplicationContentMetaStatusWithRightsCheck"),
    (607, false, "IsAnyApplicationRunning"),
];

/// Stub: CalculateApplicationOccupiedSize returns fixed sizes upstream.
pub fn calculate_application_occupied_size(
    _application_id: u64,
) -> ApplicationOccupiedSize {
    log::warn!("(STUBBED) IContentManagementInterface::CalculateApplicationOccupiedSize called");
    let stub_entity = ApplicationOccupiedSizeEntity {
        storage_id: 3, // StorageId::SdCard
        _padding: [0; 7],
        app_size: 8 * 1024 * 1024 * 1024,     // 8 GiB
        patch_size: 2 * 1024 * 1024 * 1024,    // 2 GiB
        aoc_size: 12 * 1024 * 1024,            // 12 MiB
    };
    ApplicationOccupiedSize {
        entities: [stub_entity; 4],
    }
}

/// Stub: CheckSdCardMountStatus succeeds upstream.
pub fn check_sd_card_mount_status() {
    log::warn!("(STUBBED) IContentManagementInterface::CheckSdCardMountStatus called");
}
