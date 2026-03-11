// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_a.h
//! Port of zuyu/src/core/hle/service/caps/caps_a.cpp
//!
//! IAlbumAccessorService — "caps:a".

/// IPC command table for IAlbumAccessorService.
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
    pub const GET_ALBUM_FILE_LIST_EX0: u32 = 101;
    pub const GET_AUTO_SAVING_STORAGE: u32 = 401;
    pub const LOAD_ALBUM_SCREEN_SHOT_IMAGE_EX1: u32 = 1002;
    pub const LOAD_ALBUM_SCREEN_SHOT_THUMBNAIL_IMAGE_EX1: u32 = 1003;
    pub const UNKNOWN_18: u32 = 18;
}

/// IAlbumAccessorService.
///
/// Corresponds to `IAlbumAccessorService` in upstream caps_a.h / caps_a.cpp.
pub struct IAlbumAccessorService {
    // TODO: AlbumManager reference
}

impl IAlbumAccessorService {
    pub fn new() -> Self {
        Self {}
    }
}
