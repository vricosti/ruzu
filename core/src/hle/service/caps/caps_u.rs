// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_u.h
//! Port of zuyu/src/core/hle/service/caps/caps_u.cpp
//!
//! IAlbumApplicationService — "caps:u".

/// IPC command table for IAlbumApplicationService.
pub mod commands {
    pub const SET_SHIM_LIBRARY_VERSION: u32 = 32;
    pub const GET_ALBUM_FILE_LIST0_AAFE_ARUID_DEPRECATED: u32 = 102;
    pub const DELETE_ALBUM_FILE_BY_ARUID: u32 = 103;
    pub const GET_ALBUM_FILE_SIZE_BY_ARUID: u32 = 104;
    pub const GET_ALBUM_FILE_LIST3_AAE_ARUID: u32 = 142;
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
}
