// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_ss.h
//! Port of zuyu/src/core/hle/service/caps/caps_ss.cpp
//!
//! IScreenShotService — "caps:ss".

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::caps_types::{
    AlbumFileId, AlbumReportOption, ApplicationAlbumEntry, ScreenShotAttribute,
};

/// IPC command table for IScreenShotService.
pub mod commands {
    pub const SAVE_SCREEN_SHOT: u32 = 201;
    pub const SAVE_EDITED_SCREEN_SHOT: u32 = 202;
    pub const SAVE_SCREEN_SHOT_EX0: u32 = 203;
    pub const SAVE_EDITED_SCREEN_SHOT_EX0: u32 = 204;
    pub const SAVE_EDITED_SCREEN_SHOT_EX1: u32 = 206;
    pub const SAVE_SCREEN_SHOT_OF_MOVIE_EX1: u32 = 208;
    pub const UNKNOWN_1000: u32 = 1000;
}

/// IScreenShotService.
///
/// Corresponds to `IScreenShotService` in upstream caps_ss.h / caps_ss.cpp.
pub struct IScreenShotService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    // TODO: AlbumManager reference
}

impl IScreenShotService {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (201, None, "SaveScreenShot"),
            (202, None, "SaveEditedScreenShot"),
            (203, None, "SaveScreenShotEx0"),
            (204, None, "SaveEditedScreenShotEx0"),
            (206, None, "SaveEditedScreenShotEx1"),
            (208, None, "SaveScreenShotOfMovieEx1"),
            (1000, None, "Unknown1000"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// SaveScreenShotEx0 (cmd 203).
    ///
    /// Corresponds to upstream `IScreenShotService::SaveScreenShotEx0`.
    pub fn save_screen_shot_ex0(
        &self,
        _attribute: &ScreenShotAttribute,
        report_option: AlbumReportOption,
        aruid: u64,
        image_data_buffer: &[u8],
    ) -> Result<ApplicationAlbumEntry, ResultCode> {
        log::info!(
            "SaveScreenShotEx0 called, report_option={:?}, image_data_buffer_size={}, applet_resource_user_id={}",
            report_option,
            image_data_buffer.len(),
            aruid,
        );

        // TODO: manager.flip_vertically_on_write(false);
        // TODO: manager.save_screen_shot(...)
        let entry = ApplicationAlbumEntry::default();
        Ok(entry)
    }

    /// SaveEditedScreenShotEx1 (cmd 206).
    ///
    /// Corresponds to upstream `IScreenShotService::SaveEditedScreenShotEx1`.
    pub fn save_edited_screen_shot_ex1(
        &self,
        _attribute: &ScreenShotAttribute,
        width: u64,
        height: u64,
        thumbnail_width: u64,
        thumbnail_height: u64,
        file_id: &AlbumFileId,
        _application_data_buffer: &[u8; 0x400],
        image_data_buffer: &[u8],
        thumbnail_image_data_buffer: &[u8],
    ) -> Result<ApplicationAlbumEntry, ResultCode> {
        log::info!(
            "SaveEditedScreenShotEx1 called, width={}, height={}, thumbnail_width={}, thumbnail_height={}, \
             application_id={:#018x}, storage={:?}, type={:?}, \
             image_data_buffer_size={}, thumbnail_image_buffer_size={}",
            width,
            height,
            thumbnail_width,
            thumbnail_height,
            file_id.application_id,
            file_id.storage,
            file_id.content_type,
            image_data_buffer.len(),
            thumbnail_image_data_buffer.len(),
        );

        // TODO: manager.flip_vertically_on_write(false);
        // TODO: manager.save_edited_screen_shot(...)
        let entry = ApplicationAlbumEntry::default();
        Ok(entry)
    }
}

impl SessionRequestHandler for IScreenShotService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "caps:ss"
    }
}

impl ServiceFramework for IScreenShotService {
    fn get_service_name(&self) -> &str {
        "caps:ss"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
