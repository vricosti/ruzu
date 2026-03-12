// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/content_management_interface.h
//! Port of zuyu/src/core/hle/service/ns/content_management_interface.cpp
//!
//! IContentManagementInterface — content management operations for NS.

use crate::hle::result::ResultCode;
use super::ns_types::{ApplicationOccupiedSize, ApplicationOccupiedSizeEntity};

/// IPC command table for IContentManagementInterface.
///
/// Corresponds to the function table in upstream content_management_interface.cpp.
pub mod commands {
    pub const CALCULATE_APPLICATION_OCCUPIED_SIZE: u32 = 11;
    pub const CHECK_SD_CARD_MOUNT_STATUS: u32 = 43;
    pub const GET_TOTAL_SPACE_SIZE: u32 = 47;
    pub const GET_FREE_SPACE_SIZE: u32 = 48;
    pub const COUNT_APPLICATION_CONTENT_META: u32 = 600;
    pub const LIST_APPLICATION_CONTENT_META_STATUS: u32 = 601;
    pub const LIST_APPLICATION_CONTENT_META_STATUS_WITH_RIGHTS_CHECK: u32 = 605;
    pub const IS_ANY_APPLICATION_RUNNING: u32 = 607;
}

/// IContentManagementInterface.
///
/// Corresponds to `IContentManagementInterface` in upstream.
pub struct IContentManagementInterface;

impl IContentManagementInterface {
    pub fn new() -> Self {
        Self
    }

    /// CalculateApplicationOccupiedSize (cmd 11).
    ///
    /// Corresponds to upstream `IContentManagementInterface::CalculateApplicationOccupiedSize`.
    pub fn calculate_application_occupied_size(
        &self,
        application_id: u64,
    ) -> Result<ApplicationOccupiedSize, ResultCode> {
        log::warn!(
            "(STUBBED) CalculateApplicationOccupiedSize called, application_id={:016x}",
            application_id,
        );

        let stub_entity = ApplicationOccupiedSizeEntity {
            storage_id: 3, // StorageId::SdCard
            _padding: [0; 7],
            app_size: 8 * 1024 * 1024 * 1024,  // 8 GiB
            patch_size: 2 * 1024 * 1024 * 1024, // 2 GiB
            aoc_size: 12 * 1024 * 1024,         // 12 MiB
        };

        Ok(ApplicationOccupiedSize {
            entities: [stub_entity; 4],
        })
    }

    /// CheckSdCardMountStatus (cmd 43).
    ///
    /// Corresponds to upstream `IContentManagementInterface::CheckSdCardMountStatus`.
    pub fn check_sd_card_mount_status(&self) -> Result<(), ResultCode> {
        log::warn!("(STUBBED) CheckSdCardMountStatus called");
        Ok(())
    }

    /// GetTotalSpaceSize (cmd 47).
    ///
    /// Corresponds to upstream `IContentManagementInterface::GetTotalSpaceSize`.
    pub fn get_total_space_size(&self, storage_id: u8) -> Result<i64, ResultCode> {
        log::info!("(STUBBED) GetTotalSpaceSize called, storage_id={}", storage_id);
        // TODO: system.get_file_system_controller().get_total_space_size(storage_id)
        Ok(32 * 1024 * 1024 * 1024) // 32 GiB stub
    }

    /// GetFreeSpaceSize (cmd 48).
    ///
    /// Corresponds to upstream `IContentManagementInterface::GetFreeSpaceSize`.
    pub fn get_free_space_size(&self, storage_id: u8) -> Result<i64, ResultCode> {
        log::info!("(STUBBED) GetFreeSpaceSize called, storage_id={}", storage_id);
        // TODO: system.get_file_system_controller().get_free_space_size(storage_id)
        Ok(16 * 1024 * 1024 * 1024) // 16 GiB stub
    }
}
