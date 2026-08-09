// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_u.h
//! Port of zuyu/src/core/hle/service/caps/caps_u.cpp
//!
//! IAlbumApplicationService — "caps:u".

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use super::caps_manager::AlbumManager;
use super::caps_types::{
    AlbumFileDateTime, AlbumStorage, ApplicationAlbumEntry, ApplicationAlbumFileEntry, ContentType,
};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    manager: Arc<Mutex<AlbumManager>>,
}

impl IAlbumApplicationService {
    pub fn new(album_manager: Arc<Mutex<AlbumManager>>) -> Self {
        let handlers = build_handler_map(&[
            (
                32,
                Some(Self::set_shim_library_version_handler),
                "SetShimLibraryVersion",
            ),
            (
                102,
                Some(Self::get_album_file_list0_aafe_aruid_deprecated_handler),
                "GetAlbumFileList0AafeAruidDeprecated",
            ),
            (103, None, "DeleteAlbumFileByAruid"),
            (104, None, "GetAlbumFileSizeByAruid"),
            (105, None, "DeleteAlbumFileByAruidForDebug"),
            (110, None, "LoadAlbumScreenShotImageByAruid"),
            (120, None, "LoadAlbumScreenShotThumbnailImageByAruid"),
            (130, None, "PrecheckToCreateContentsByAruid"),
            (140, None, "GetAlbumFileList1AafeAruidDeprecated"),
            (141, None, "GetAlbumFileList2AafeUidAruidDeprecated"),
            (
                142,
                Some(Self::get_album_file_list3_aae_aruid_handler),
                "GetAlbumFileList3AaeAruid",
            ),
            (143, None, "GetAlbumFileList4AaeUidAruid"),
            (144, None, "GetAllAlbumFileList3AaeAruid"),
            (60002, None, "OpenAccessorSessionForApplication"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            manager: album_manager,
        }
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

    fn set_shim_library_version_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut request = RequestParser::new(ctx);
        let library_version = request.pop_u64();
        let aruid = request.pop_u64();
        let result = service
            .set_shim_library_version(library_version, aruid)
            .err()
            .unwrap_or(RESULT_SUCCESS);
        let mut response = ResponseBuilder::new(ctx, 2, 0, 0);
        response.push_result(result);
    }

    fn read_raw_data_at<T: Copy + Default>(ctx: &HLERequestContext, byte_offset: usize) -> T {
        let payload_word = if ctx.is_tipc() {
            2
        } else {
            ctx.get_data_payload_offset() as usize + 2
        };
        let base = payload_word * core::mem::size_of::<u32>() + byte_offset;
        let end = base + core::mem::size_of::<T>();
        assert!(end <= core::mem::size_of_val(&ctx.cmd_buf));

        let mut value = T::default();
        unsafe {
            core::ptr::copy_nonoverlapping(
                ctx.cmd_buf.as_ptr().cast::<u8>().add(base),
                (&mut value as *mut T).cast::<u8>(),
                core::mem::size_of::<T>(),
            );
        }
        value
    }

    fn values_as_bytes<T>(values: &[T]) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(
                values.as_ptr().cast::<u8>(),
                core::mem::size_of_val(values),
            )
        }
    }

    fn get_album_file_list0_aafe_aruid_deprecated_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let content_type = ContentType(Self::read_raw_data_at::<u8>(ctx, 0));
        let start_posix_time = Self::read_raw_data_at::<i64>(ctx, 8);
        let end_posix_time = Self::read_raw_data_at::<i64>(ctx, 16);
        let aruid = Self::read_raw_data_at::<u64>(ctx, 24);
        let mut entries = vec![
            ApplicationAlbumFileEntry::default();
            ctx.get_write_buffer_size(0)
                / core::mem::size_of::<ApplicationAlbumFileEntry>()
        ];
        let (result, count) = match service.get_album_file_list0_aafe_aruid_deprecated(
            content_type,
            start_posix_time,
            end_posix_time,
            aruid,
            &mut entries,
        ) {
            Ok(count) => (RESULT_SUCCESS, count),
            Err(result) => (result, 0),
        };
        if !entries.is_empty() {
            ctx.write_buffer(Self::values_as_bytes(&entries), 0);
        }
        let mut response = ResponseBuilder::new(ctx, 4, 0, 0);
        response.push_result(result);
        response.push_u64(count);
    }

    fn get_album_file_list3_aae_aruid_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let content_type = ContentType(Self::read_raw_data_at::<u8>(ctx, 0));
        let start_date_time = Self::read_raw_data_at::<AlbumFileDateTime>(ctx, 2);
        let end_date_time = Self::read_raw_data_at::<AlbumFileDateTime>(ctx, 10);
        let aruid = Self::read_raw_data_at::<u64>(ctx, 24);
        let mut entries = vec![
            ApplicationAlbumEntry::default();
            ctx.get_write_buffer_size(0)
                / core::mem::size_of::<ApplicationAlbumEntry>()
        ];
        let (result, count) = match service.get_album_file_list3_aae_aruid(
            content_type,
            start_date_time,
            end_date_time,
            aruid,
            &mut entries,
        ) {
            Ok(count) => (RESULT_SUCCESS, count),
            Err(result) => (result, 0),
        };
        if !entries.is_empty() {
            ctx.write_buffer(Self::values_as_bytes(&entries), 0);
        }
        let mut response = ResponseBuilder::new(ctx, 4, 0, 0);
        response.push_result(result);
        response.push_u64(count);
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

        {
            let mut manager = self.manager.lock().unwrap();
            let result = manager.is_album_mounted(AlbumStorage::Sd);
            if !result.is_success() {
                return Err(result);
            }
        }
        let manager = self.manager.lock().unwrap();
        let (result, count) = manager.get_album_file_list_aafe(
            out_entries,
            content_type,
            start_posix_time,
            end_posix_time,
            aruid,
        );
        if !result.is_success() {
            return Err(result);
        }
        Ok(count)
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

        {
            let mut manager = self.manager.lock().unwrap();
            let result = manager.is_album_mounted(AlbumStorage::Sd);
            if !result.is_success() {
                return Err(result);
            }
        }
        let manager = self.manager.lock().unwrap();
        let (result, count) = manager.get_album_file_list_aae(
            out_entries,
            content_type,
            start_date_time,
            end_date_time,
            aruid,
        );
        if !result.is_success() {
            return Err(result);
        }
        Ok(count)
    }
}

impl SessionRequestHandler for IAlbumApplicationService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "caps:u"
    }
}

impl ServiceFramework for IAlbumApplicationService {
    fn get_service_name(&self) -> &str {
        "caps:u"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set_shim_library_version_is_registered() {
        let service = IAlbumApplicationService::new(Arc::new(Mutex::new(AlbumManager::new())));
        assert!(service
            .handlers
            .get(&32)
            .unwrap()
            .handler_callback
            .is_some());
        for command_id in [102, 142] {
            assert!(service
                .handlers
                .get(&command_id)
                .unwrap()
                .handler_callback
                .is_some());
        }
    }

    #[test]
    fn list3_cmif_raw_offsets_match_upstream_alignment() {
        let mut ctx = HLERequestContext::new();
        let base = 2 * core::mem::size_of::<u32>();
        let start = AlbumFileDateTime {
            year: 2024,
            month: 3,
            day: 4,
            hour: 5,
            minute: 6,
            second: 7,
            unique_id: 8,
        };
        let end = AlbumFileDateTime {
            year: 2025,
            month: 9,
            day: 10,
            hour: 11,
            minute: 12,
            second: 13,
            unique_id: 14,
        };
        let aruid = 0x1122_3344_5566_7788u64;
        let bytes = unsafe {
            core::slice::from_raw_parts_mut(
                ctx.cmd_buf.as_mut_ptr().cast::<u8>(),
                core::mem::size_of_val(&ctx.cmd_buf),
            )
        };
        bytes[base] = 3;
        unsafe {
            core::ptr::copy_nonoverlapping(
                (&start as *const AlbumFileDateTime).cast::<u8>(),
                bytes.as_mut_ptr().add(base + 2),
                core::mem::size_of::<AlbumFileDateTime>(),
            );
            core::ptr::copy_nonoverlapping(
                (&end as *const AlbumFileDateTime).cast::<u8>(),
                bytes.as_mut_ptr().add(base + 10),
                core::mem::size_of::<AlbumFileDateTime>(),
            );
            core::ptr::copy_nonoverlapping(
                (&aruid as *const u64).cast::<u8>(),
                bytes.as_mut_ptr().add(base + 24),
                core::mem::size_of::<u64>(),
            );
        }

        assert_eq!(IAlbumApplicationService::read_raw_data_at::<u8>(&ctx, 0), 3);
        assert_eq!(
            IAlbumApplicationService::read_raw_data_at::<AlbumFileDateTime>(&ctx, 2),
            start
        );
        assert_eq!(
            IAlbumApplicationService::read_raw_data_at::<AlbumFileDateTime>(&ctx, 10),
            end
        );
        assert_eq!(
            IAlbumApplicationService::read_raw_data_at::<u64>(&ctx, 24),
            aruid
        );
    }
}
