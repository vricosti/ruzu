// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_c.h
//! Port of zuyu/src/core/hle/service/caps/caps_c.cpp
//!
//! IAlbumControlService — "caps:c".

/// IPC command table for IAlbumControlService.
pub mod commands {
    pub const CAPTURE_RAW_IMAGE: u32 = 1;
    pub const CAPTURE_RAW_IMAGE_WITH_TIMEOUT: u32 = 2;
    pub const SET_SHIM_LIBRARY_VERSION: u32 = 33;
    pub const REQUEST_TAKING_SCREEN_SHOT: u32 = 1001;
    pub const REQUEST_TAKING_SCREEN_SHOT_WITH_TIMEOUT: u32 = 1002;
    pub const NOTIFY_TAKING_SCREEN_SHOT_REFUSED: u32 = 1011;
    pub const NOTIFY_ALBUM_STORAGE_IS_AVAILABLE: u32 = 2001;
    pub const NOTIFY_ALBUM_STORAGE_IS_UNAVAILABLE: u32 = 2002;
    pub const OPEN_CONTROL_SESSION: u32 = 60001;
}

/// IAlbumControlService.
///
/// Corresponds to `IAlbumControlService` in upstream caps_c.h / caps_c.cpp.
pub struct IAlbumControlService {
    // TODO: AlbumManager reference
}

impl IAlbumControlService {
    pub fn new() -> Self {
        Self {}
    }

    /// SetShimLibraryVersion (cmd 33).
    pub fn set_shim_library_version(&self, _library_version: u64, _aruid: u64) {
        log::warn!("IAlbumControlService::set_shim_library_version (STUBBED) called");
    }
}
