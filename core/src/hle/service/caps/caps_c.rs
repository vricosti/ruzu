// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_c.h
//! Port of zuyu/src/core/hle/service/caps/caps_c.cpp
//!
//! IAlbumControlService — "caps:c".

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::caps_types::ShimLibraryVersion;

/// IPC command table for IAlbumControlService.
///
/// Corresponds to the function table in upstream caps_c.cpp.
pub mod commands {
    pub const CAPTURE_RAW_IMAGE: u32 = 1;
    pub const CAPTURE_RAW_IMAGE_WITH_TIMEOUT: u32 = 2;
    pub const SET_SHIM_LIBRARY_VERSION: u32 = 33;
    pub const REQUEST_TAKING_SCREEN_SHOT: u32 = 1001;
    pub const REQUEST_TAKING_SCREEN_SHOT_WITH_TIMEOUT: u32 = 1002;
    pub const NOTIFY_TAKING_SCREEN_SHOT_REFUSED: u32 = 1011;
    pub const NOTIFY_ALBUM_STORAGE_IS_AVAILABLE: u32 = 2001;
    pub const NOTIFY_ALBUM_STORAGE_IS_UNAVAILABLE: u32 = 2002;
    pub const REGISTER_APPLET_RESOURCE_USER_ID: u32 = 2011;
    pub const UNREGISTER_APPLET_RESOURCE_USER_ID: u32 = 2012;
    pub const GET_APPLICATION_ID_FROM_ARUID: u32 = 2013;
    pub const CHECK_APPLICATION_ID_REGISTERED: u32 = 2014;
    pub const GENERATE_CURRENT_ALBUM_FILE_ID: u32 = 2101;
    pub const GENERATE_APPLICATION_ALBUM_ENTRY: u32 = 2102;
    pub const SAVE_ALBUM_SCREEN_SHOT_FILE: u32 = 2201;
    pub const SAVE_ALBUM_SCREEN_SHOT_FILE_EX: u32 = 2202;
    pub const SET_OVERLAY_SCREEN_SHOT_THUMBNAIL_DATA: u32 = 2301;
    pub const SET_OVERLAY_MOVIE_THUMBNAIL_DATA: u32 = 2302;
    pub const OPEN_CONTROL_SESSION: u32 = 60001;
}

/// IAlbumControlService.
///
/// Corresponds to `IAlbumControlService` in upstream caps_c.h / caps_c.cpp.
pub struct IAlbumControlService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    // TODO: AlbumManager reference
}

impl IAlbumControlService {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (1, None, "CaptureRawImage"),
            (2, None, "CaptureRawImageWithTimeout"),
            (33, None, "SetShimLibraryVersion"),
            (1001, None, "RequestTakingScreenShot"),
            (1002, None, "RequestTakingScreenShotWithTimeout"),
            (1011, None, "NotifyTakingScreenShotRefused"),
            (2001, None, "NotifyAlbumStorageIsAvailable"),
            (2002, None, "NotifyAlbumStorageIsUnavailable"),
            (2011, None, "RegisterAppletResourceUserId"),
            (2012, None, "UnregisterAppletResourceUserId"),
            (2013, None, "GetApplicationIdFromAruid"),
            (2014, None, "CheckApplicationIdRegistered"),
            (2101, None, "GenerateCurrentAlbumFileId"),
            (2102, None, "GenerateApplicationAlbumEntry"),
            (2201, None, "SaveAlbumScreenShotFile"),
            (2202, None, "SaveAlbumScreenShotFileEx"),
            (2301, None, "SetOverlayScreenShotThumbnailData"),
            (2302, None, "SetOverlayMovieThumbnailData"),
            (60001, None, "OpenControlSession"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// SetShimLibraryVersion (cmd 33).
    ///
    /// Corresponds to upstream `IAlbumControlService::SetShimLibraryVersion`.
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
}

impl SessionRequestHandler for IAlbumControlService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "caps:c"
    }
}

impl ServiceFramework for IAlbumControlService {
    fn get_service_name(&self) -> &str {
        "caps:c"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
