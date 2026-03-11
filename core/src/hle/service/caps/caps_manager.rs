// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_manager.h
//! Port of zuyu/src/core/hle/service/caps/caps_manager.cpp
//!
//! AlbumManager — manages screenshot album files.

use std::collections::HashMap;
use std::path::PathBuf;

use super::caps_types::*;
use crate::hle::result::ResultCode;

/// Nand album file limit.
const NAND_ALBUM_FILE_LIMIT: usize = 1000;
/// SD album file limit.
const SD_ALBUM_FILE_LIMIT: usize = 10000;

/// AlbumManager — manages screenshot album storage.
///
/// Corresponds to `AlbumManager` in upstream caps_manager.h / caps_manager.cpp.
pub struct AlbumManager {
    is_mounted: bool,
    album_files: HashMap<AlbumFileId, PathBuf>,
}

impl AlbumManager {
    pub fn new() -> Self {
        Self {
            is_mounted: false,
            album_files: HashMap::new(),
        }
    }

    /// DeleteAlbumFile.
    pub fn delete_album_file(&mut self, file_id: &AlbumFileId) -> ResultCode {
        log::debug!("AlbumManager::delete_album_file called");
        if (file_id.storage as u8) > (AlbumStorage::Sd as u8) {
            return super::caps_result::RESULT_INVALID_STORAGE;
        }
        if !self.is_mounted {
            return super::caps_result::RESULT_IS_NOT_MOUNTED;
        }
        // TODO: actual file deletion
        ResultCode::new(0)
    }

    /// IsAlbumMounted.
    pub fn is_album_mounted(&mut self, storage: AlbumStorage) -> ResultCode {
        log::debug!("AlbumManager::is_album_mounted called");
        if (storage as u8) > (AlbumStorage::Sd as u8) {
            return super::caps_result::RESULT_INVALID_STORAGE;
        }
        self.is_mounted = true;
        if storage == AlbumStorage::Sd {
            self.find_screenshots();
        }
        if self.is_mounted {
            ResultCode::new(0)
        } else {
            super::caps_result::RESULT_IS_NOT_MOUNTED
        }
    }

    /// GetAutoSavingStorage.
    pub fn get_auto_saving_storage(&self) -> (ResultCode, bool) {
        log::debug!("AlbumManager::get_auto_saving_storage called");
        (ResultCode::new(0), false)
    }

    /// FlipVerticallyOnWrite.
    pub fn flip_vertically_on_write(&self, _flip: bool) {
        // TODO: stbi_flip_vertically_on_write equivalent
    }

    fn find_screenshots(&mut self) {
        self.is_mounted = false;
        self.album_files.clear();
        // TODO: scan screenshot directory
        self.is_mounted = true;
    }
}
