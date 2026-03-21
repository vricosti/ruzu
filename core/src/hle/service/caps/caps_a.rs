// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_a.h
//! Port of zuyu/src/core/hle/service/caps/caps_a.cpp
//!
//! IAlbumAccessorService — "caps:a".

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ErrorModule, ResultCode};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::caps_manager::AlbumManager;
use super::caps_result::*;
use super::caps_types::{
    AlbumEntry, AlbumFileId, AlbumStorage, LoadAlbumScreenShotImageOutput, ScreenShotDecodeOption,
};

/// IPC command table for IAlbumAccessorService.
///
/// Corresponds to the function table in upstream caps_a.cpp.
pub mod commands {
    pub const GET_ALBUM_FILE_COUNT: u32 = 0;
    pub const GET_ALBUM_FILE_LIST: u32 = 1;
    pub const LOAD_ALBUM_FILE: u32 = 2;
    pub const DELETE_ALBUM_FILE: u32 = 3;
    pub const STORAGE_COPY_ALBUM_FILE: u32 = 4;
    pub const IS_ALBUM_MOUNTED: u32 = 5;
    pub const GET_ALBUM_USAGE: u32 = 6;
    pub const GET_ALBUM_FILE_SIZE: u32 = 7;
    pub const LOAD_ALBUM_FILE_THUMBNAIL: u32 = 8;
    pub const LOAD_ALBUM_SCREEN_SHOT_IMAGE: u32 = 9;
    pub const LOAD_ALBUM_SCREEN_SHOT_THUMBNAIL_IMAGE: u32 = 10;
    pub const GET_ALBUM_ENTRY_FROM_APPLICATION_ALBUM_ENTRY: u32 = 11;
    pub const LOAD_ALBUM_SCREEN_SHOT_IMAGE_EX: u32 = 12;
    pub const LOAD_ALBUM_SCREEN_SHOT_THUMBNAIL_IMAGE_EX: u32 = 13;
    pub const LOAD_ALBUM_SCREEN_SHOT_IMAGE_EX0: u32 = 14;
    pub const GET_ALBUM_USAGE3: u32 = 15;
    pub const GET_ALBUM_MOUNT_RESULT: u32 = 16;
    pub const GET_ALBUM_USAGE16: u32 = 17;
    pub const UNKNOWN_18: u32 = 18;
    pub const UNKNOWN_19: u32 = 19;
    pub const GET_ALBUM_FILE_COUNT_EX0: u32 = 100;
    pub const GET_ALBUM_FILE_LIST_EX0: u32 = 101;
    pub const SAVE_EDITED_SCREEN_SHOT: u32 = 202;
    pub const GET_LAST_THUMBNAIL: u32 = 301;
    pub const GET_LAST_OVERLAY_MOVIE_THUMBNAIL: u32 = 302;
    pub const GET_AUTO_SAVING_STORAGE: u32 = 401;
    pub const GET_REQUIRED_STORAGE_SPACE_SIZE_TO_COPY_ALL: u32 = 501;
    pub const LOAD_ALBUM_SCREEN_SHOT_THUMBNAIL_IMAGE_EX0: u32 = 1001;
    pub const LOAD_ALBUM_SCREEN_SHOT_IMAGE_EX1: u32 = 1002;
    pub const LOAD_ALBUM_SCREEN_SHOT_THUMBNAIL_IMAGE_EX1: u32 = 1003;
    pub const FORCE_ALBUM_UNMOUNTED: u32 = 8001;
    pub const RESET_ALBUM_MOUNT_STATUS: u32 = 8002;
    pub const REFRESH_ALBUM_CACHE: u32 = 8011;
    pub const GET_ALBUM_CACHE: u32 = 8012;
    pub const GET_ALBUM_CACHE_EX: u32 = 8013;
    pub const GET_ALBUM_ENTRY_FROM_APPLICATION_ALBUM_ENTRY_ARUID: u32 = 8021;
    pub const SET_INTERNAL_ERROR_CONVERSION_ENABLED: u32 = 10011;
    pub const LOAD_MAKER_NOTE_INFO_FOR_DEBUG: u32 = 50000;
    pub const OPEN_ACCESSOR_SESSION: u32 = 60002;
}

/// IAlbumAccessorService.
///
/// Corresponds to `IAlbumAccessorService` in upstream caps_a.h / caps_a.cpp.
pub struct IAlbumAccessorService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    manager: Arc<Mutex<AlbumManager>>,
}

impl IAlbumAccessorService {
    pub fn new(album_manager: Arc<Mutex<AlbumManager>>) -> Self {
        let handlers = build_handler_map(&[
            (0, None, "GetAlbumFileCount"),
            (1, None, "GetAlbumFileList"),
            (2, None, "LoadAlbumFile"),
            (3, None, "DeleteAlbumFile"),
            (4, None, "StorageCopyAlbumFile"),
            (5, None, "IsAlbumMounted"),
            (6, None, "GetAlbumUsage"),
            (7, None, "GetAlbumFileSize"),
            (8, None, "LoadAlbumFileThumbnail"),
            (9, None, "LoadAlbumScreenShotImage"),
            (10, None, "LoadAlbumScreenShotThumbnailImage"),
            (11, None, "GetAlbumEntryFromApplicationAlbumEntry"),
            (12, None, "LoadAlbumScreenShotImageEx"),
            (13, None, "LoadAlbumScreenShotThumbnailImageEx"),
            (14, None, "LoadAlbumScreenShotImageEx0"),
            (15, None, "GetAlbumUsage3"),
            (16, None, "GetAlbumMountResult"),
            (17, None, "GetAlbumUsage16"),
            (18, None, "Unknown18"),
            (19, None, "Unknown19"),
            (100, None, "GetAlbumFileCountEx0"),
            (101, None, "GetAlbumFileListEx0"),
            (202, None, "SaveEditedScreenShot"),
            (301, None, "GetLastThumbnail"),
            (302, None, "GetLastOverlayMovieThumbnail"),
            (401, None, "GetAutoSavingStorage"),
            (501, None, "GetRequiredStorageSpaceSizeToCopyAll"),
            (1001, None, "LoadAlbumScreenShotThumbnailImageEx0"),
            (1002, None, "LoadAlbumScreenShotImageEx1"),
            (1003, None, "LoadAlbumScreenShotThumbnailImageEx1"),
            (8001, None, "ForceAlbumUnmounted"),
            (8002, None, "ResetAlbumMountStatus"),
            (8011, None, "RefreshAlbumCache"),
            (8012, None, "GetAlbumCache"),
            (8013, None, "GetAlbumCacheEx"),
            (8021, None, "GetAlbumEntryFromApplicationAlbumEntryAruid"),
            (10011, None, "SetInternalErrorConversionEnabled"),
            (50000, None, "LoadMakerNoteInfoForDebug"),
            (60002, None, "OpenAccessorSession"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            manager: album_manager,
        }
    }

    /// GetAlbumFileList (cmd 1).
    ///
    /// Corresponds to upstream `IAlbumAccessorService::GetAlbumFileList`.
    pub fn get_album_file_list(
        &self,
        storage: AlbumStorage,
        out_entries: &mut [AlbumEntry],
    ) -> Result<u64, ResultCode> {
        log::info!("GetAlbumFileList called, storage={:?}", storage);

        let manager = self.manager.lock().unwrap();
        let (result, count) = manager.get_album_file_list(out_entries, storage, 0);
        let result = self.translate_result(result);
        if result.is_success() {
            Ok(count)
        } else {
            Err(result)
        }
    }

    /// DeleteAlbumFile (cmd 3).
    ///
    /// Corresponds to upstream `IAlbumAccessorService::DeleteAlbumFile`.
    pub fn delete_album_file(&self, file_id: AlbumFileId) -> Result<(), ResultCode> {
        log::info!(
            "DeleteAlbumFile called, application_id={:#018x}, storage={:?}, type={:?}",
            file_id.application_id,
            file_id.storage,
            file_id.content_type,
        );

        let mut manager = self.manager.lock().unwrap();
        let result = manager.delete_album_file(&file_id);
        let result = self.translate_result(result);
        if result.is_success() {
            Ok(())
        } else {
            Err(result)
        }
    }

    /// IsAlbumMounted (cmd 5).
    ///
    /// Corresponds to upstream `IAlbumAccessorService::IsAlbumMounted`.
    pub fn is_album_mounted(&self, storage: AlbumStorage) -> Result<bool, ResultCode> {
        log::info!("IsAlbumMounted called, storage={:?}", storage);

        let mut manager = self.manager.lock().unwrap();
        let result = manager.is_album_mounted(storage);
        let is_mounted = result.is_success();
        let result = self.translate_result(result);
        if result.is_success() {
            Ok(is_mounted)
        } else {
            Err(result)
        }
    }

    /// Unknown18 (cmd 18).
    ///
    /// Corresponds to upstream `IAlbumAccessorService::Unknown18`.
    pub fn unknown18(&self, out_buffer: &mut [u8]) -> Result<u32, ResultCode> {
        log::warn!("(STUBBED) Unknown18 called");
        let _ = out_buffer;
        Ok(0)
    }

    /// GetAlbumFileListEx0 (cmd 101).
    ///
    /// Corresponds to upstream `IAlbumAccessorService::GetAlbumFileListEx0`.
    pub fn get_album_file_list_ex0(
        &self,
        storage: AlbumStorage,
        flags: u8,
        out_entries: &mut [AlbumEntry],
    ) -> Result<u64, ResultCode> {
        log::info!(
            "GetAlbumFileListEx0 called, storage={:?}, flags={}",
            storage,
            flags,
        );

        let manager = self.manager.lock().unwrap();
        let (result, count) = manager.get_album_file_list(out_entries, storage, flags);
        let result = self.translate_result(result);
        if result.is_success() {
            Ok(count)
        } else {
            Err(result)
        }
    }

    /// GetAutoSavingStorage (cmd 401).
    ///
    /// Corresponds to upstream `IAlbumAccessorService::GetAutoSavingStorage`.
    pub fn get_auto_saving_storage(&self) -> Result<bool, ResultCode> {
        log::warn!("(STUBBED) GetAutoSavingStorage called");

        let manager = self.manager.lock().unwrap();
        let (result, is_autosaving) = manager.get_auto_saving_storage();
        let result = self.translate_result(result);
        if result.is_success() {
            Ok(is_autosaving)
        } else {
            Err(result)
        }
    }

    /// LoadAlbumScreenShotImageEx1 (cmd 1002).
    ///
    /// Corresponds to upstream `IAlbumAccessorService::LoadAlbumScreenShotImageEx1`.
    pub fn load_album_screen_shot_image_ex1(
        &self,
        file_id: &AlbumFileId,
        decoder_options: &ScreenShotDecodeOption,
        out_image_output: &mut LoadAlbumScreenShotImageOutput,
        out_image: &mut [u8],
    ) -> Result<(), ResultCode> {
        log::info!(
            "LoadAlbumScreenShotImageEx1 called, application_id={:#018x}, storage={:?}, type={:?}, flags={:?}",
            file_id.application_id,
            file_id.storage,
            file_id.content_type,
            decoder_options.flags,
        );

        let manager = self.manager.lock().unwrap();
        let result = manager.load_album_screen_shot_image(
            out_image_output,
            out_image,
            file_id,
            decoder_options,
        );
        let result = self.translate_result(result);
        if result.is_success() {
            Ok(())
        } else {
            Err(result)
        }
    }

    /// LoadAlbumScreenShotThumbnailImageEx1 (cmd 1003).
    ///
    /// Corresponds to upstream `IAlbumAccessorService::LoadAlbumScreenShotThumbnailImageEx1`.
    pub fn load_album_screen_shot_thumbnail_image_ex1(
        &self,
        file_id: &AlbumFileId,
        decoder_options: &ScreenShotDecodeOption,
        out_image_output: &mut LoadAlbumScreenShotImageOutput,
        out_image: &mut [u8],
    ) -> Result<(), ResultCode> {
        log::info!(
            "LoadAlbumScreenShotThumbnailImageEx1 called, application_id={:#018x}, storage={:?}, type={:?}, flags={:?}",
            file_id.application_id,
            file_id.storage,
            file_id.content_type,
            decoder_options.flags,
        );

        let manager = self.manager.lock().unwrap();
        let result = manager.load_album_screen_shot_thumbnail(
            out_image_output,
            out_image,
            file_id,
            decoder_options,
        );
        let result = self.translate_result(result);
        if result.is_success() {
            Ok(())
        } else {
            Err(result)
        }
    }

    /// TranslateResult — translates internal capture result codes to public ones.
    ///
    /// Corresponds to upstream `IAlbumAccessorService::TranslateResult`.
    pub fn translate_result(&self, in_result: ResultCode) -> ResultCode {
        if in_result.is_success() {
            return in_result;
        }

        let raw = in_result.get_inner_value();
        let desc = in_result.get_description();

        if (raw & 0x3801ff) == RESULT_UNKNOWN_1024.get_inner_value() {
            if desc.wrapping_sub(0x514) < 100 {
                return RESULT_INVALID_FILE_DATA;
            }
            if desc.wrapping_sub(0x5dc) < 100 {
                return RESULT_INVALID_FILE_DATA;
            }
            if desc.wrapping_sub(0x578) < 100 {
                if in_result == RESULT_FILE_COUNT_LIMIT {
                    return RESULT_UNKNOWN_22;
                }
                return RESULT_UNKNOWN_25;
            }
            if raw < RESULT_UNKNOWN_1801.get_inner_value() {
                if in_result == RESULT_UNKNOWN_1202 {
                    return RESULT_UNKNOWN_810;
                }
                if in_result == RESULT_UNKNOWN_1203 {
                    return RESULT_UNKNOWN_810;
                }
                if in_result == RESULT_UNKNOWN_1701 {
                    return RESULT_UNKNOWN_5;
                }
            } else if raw < RESULT_UNKNOWN_1803.get_inner_value() {
                if in_result == RESULT_UNKNOWN_1801 {
                    return RESULT_UNKNOWN_5;
                }
                if in_result == RESULT_UNKNOWN_1802 {
                    return RESULT_UNKNOWN_6;
                }
            } else {
                if in_result == RESULT_UNKNOWN_1803 {
                    return RESULT_UNKNOWN_7;
                }
                if in_result == RESULT_UNKNOWN_1804 {
                    return RESULT_OUT_OF_RANGE;
                }
            }
            return RESULT_UNKNOWN_1024;
        }

        if in_result.get_module_raw() == ErrorModule::FS as u32 {
            if (desc >> 0xc < 0x7d)
                || (desc.wrapping_sub(1000) < 2000)
                || ((desc.wrapping_sub(3000) >> 3) < 0x271)
            {
                // TODO: Translate FS error
                return in_result;
            }
        }

        in_result
    }
}

impl SessionRequestHandler for IAlbumAccessorService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "caps:a"
    }
}

impl ServiceFramework for IAlbumAccessorService {
    fn get_service_name(&self) -> &str {
        "caps:a"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
