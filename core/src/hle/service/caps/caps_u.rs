// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_u.h
//! Port of zuyu/src/core/hle/service/caps/caps_u.cpp
//!
//! IAlbumApplicationService — "caps:u".

use crate::hle::result::ResultCode;
use super::caps_types::{
    AlbumFileDateTime, AlbumStorage, ApplicationAlbumEntry, ApplicationAlbumFileEntry,
    ContentType, ShimLibraryVersion,
};

/// IPC command table for IAlbumApplicationService.
///
/// Corresponds to the function table in upstream caps_u.cpp.
pub mod commands {
    pub const SET_SHIM_LIBRARY_VERSION: u32 = 32;
    pub const GET_ALBUM_FILE_LIST0_AAFE_ARUID_DEPRECATED: u32 = 102;
    pub const DELETE_ALBUM_FILE_BY_ARUID: u32 = 103;
    pub const GET_ALBUM_FILE_SIZE_BY_ARUID: u32 = 104;
    pub const DELETE_ALBUM_FILE_BY_ARUID_FOR_DEBUG: u32 = 105;
    pub const LOAD_ALBUM_SCREEN_SHOT_IMAGE_BY_ARUID: u32 = 110;
    pub const LOAD_ALBUM_SCREEN_SHOT_THUMBNAIL_IMAGE_BY_ARUID: u32 = 120;
    pub const PRECHECK_TO_CREATE_CONTENTS_BY_ARUID: u32 = 130;
    pub const GET_ALBUM_FILE_LIST1_AAFE_ARUID_DEPRECATED: u32 = 140;
    pub const GET_ALBUM_FILE_LIST2_AAFE_UID_ARUID_DEPRECATED: u32 = 141;
    pub const GET_ALBUM_FILE_LIST3_AAE_ARUID: u32 = 142;
    pub const GET_ALBUM_FILE_LIST4_AAE_UID_ARUID: u32 = 143;
    pub const GET_ALL_ALBUM_FILE_LIST3_AAE_ARUID: u32 = 144;
    pub const OPEN_ACCESSOR_SESSION_FOR_APPLICATION: u32 = 60002;
}

/// IAlbumApplicationService.
///
/// Corresponds to `IAlbumApplicationService` in upstream caps_u.h / caps_u.cpp.
pub struct IAlbumApplicationService {
    // TODO: AlbumManager reference
}

impl IAlbumApplicationService {
    pub fn new() -> Self {
        Self {}
    }

    /// SetShimLibraryVersion (cmd 32).
    ///
    /// Corresponds to upstream `IAlbumApplicationService::SetShimLibraryVersion`.
    pub fn set_shim_library_version(
        &self,
        library_version: u64,
        aruid: u64,
    ) -> Result<(), ResultCode> {
        log::warn!(
            "(STUBBED) SetShimLibraryVersion called. library_version={}, applet_resource_user_id={}",
            library_version,
            aruid,
        );
        Ok(())
    }

    /// GetAlbumFileList0AafeAruidDeprecated (cmd 102).
    ///
    /// Corresponds to upstream `IAlbumApplicationService::GetAlbumFileList0AafeAruidDeprecated`.
    pub fn get_album_file_list0_aafe_aruid_deprecated(
        &self,
        content_type: ContentType,
        start_posix_time: i64,
        end_posix_time: i64,
        aruid: u64,
        out_entries: &mut [ApplicationAlbumFileEntry],
    ) -> Result<u64, ResultCode> {
        log::warn!(
            "(STUBBED) GetAlbumFileList0AafeAruidDeprecated called. content_type={:?}, \
             start_posix_time={}, end_posix_time={}, applet_resource_user_id={}",
            content_type,
            start_posix_time,
            end_posix_time,
            aruid,
        );

        // TODO: manager.is_album_mounted(AlbumStorage::Sd)?;
        // TODO: manager.get_album_file_list(...)
        Ok(0)
    }

    /// GetAlbumFileList3AaeAruid (cmd 142).
    ///
    /// Corresponds to upstream `IAlbumApplicationService::GetAlbumFileList3AaeAruid`.
    pub fn get_album_file_list3_aae_aruid(
        &self,
        content_type: ContentType,
        start_date_time: AlbumFileDateTime,
        end_date_time: AlbumFileDateTime,
        aruid: u64,
        out_entries: &mut [ApplicationAlbumEntry],
    ) -> Result<u64, ResultCode> {
        log::warn!(
            "(STUBBED) GetAlbumFileList3AaeAruid called. content_type={:?}, \
             start_date={}/{}/{}, end_date={}/{}/{}, applet_resource_user_id={}",
            content_type,
            start_date_time.year,
            start_date_time.month,
            start_date_time.day,
            end_date_time.year,
            end_date_time.month,
            end_date_time.day,
            aruid,
        );

        // TODO: manager.is_album_mounted(AlbumStorage::Sd)?;
        // TODO: manager.get_album_file_list(...)
        Ok(0)
    }
}
