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

    /// GetAlbumFileList — retrieves album entries filtered by storage and flags.
    ///
    /// Corresponds to upstream `AlbumManager::GetAlbumFileList(span<AlbumEntry>, ...)`.
    pub fn get_album_file_list(
        &self,
        out_entries: &mut [AlbumEntry],
        storage: AlbumStorage,
        _flags: u8,
    ) -> (ResultCode, u64) {
        log::debug!("AlbumManager::get_album_file_list called");
        if (storage as u8) > (AlbumStorage::Sd as u8) {
            return (super::caps_result::RESULT_INVALID_STORAGE, 0);
        }
        if !self.is_mounted {
            return (super::caps_result::RESULT_IS_NOT_MOUNTED, 0);
        }

        let mut out_entries_count: u64 = 0;
        for (file_id, path) in &self.album_files {
            if file_id.storage != storage {
                continue;
            }
            if out_entries_count as usize >= SD_ALBUM_FILE_LIMIT {
                break;
            }
            if out_entries_count as usize >= out_entries.len() {
                break;
            }
            // TODO: get actual file size via Common::FS::GetSize equivalent
            let entry_size: u64 = 0;
            out_entries[out_entries_count as usize] = AlbumEntry {
                entry_size,
                file_id: *file_id,
            };
            out_entries_count += 1;
        }

        (ResultCode::new(0), out_entries_count)
    }

    /// GetAutoSavingStorage.
    pub fn get_auto_saving_storage(&self) -> (ResultCode, bool) {
        log::debug!("AlbumManager::get_auto_saving_storage called");
        (ResultCode::new(0), false)
    }

    /// LoadAlbumScreenShotImage — loads a full-size screenshot image.
    ///
    /// Corresponds to upstream `AlbumManager::LoadAlbumScreenShotImage`.
    pub fn load_album_screen_shot_image(
        &self,
        out_image_output: &mut LoadAlbumScreenShotImageOutput,
        out_image: &mut [u8],
        file_id: &AlbumFileId,
        _decoder_options: &ScreenShotDecodeOption,
    ) -> ResultCode {
        log::debug!("AlbumManager::load_album_screen_shot_image called");
        if (file_id.storage as u8) > (AlbumStorage::Sd as u8) {
            return super::caps_result::RESULT_INVALID_STORAGE;
        }
        if !self.is_mounted {
            return super::caps_result::RESULT_IS_NOT_MOUNTED;
        }

        *out_image_output = LoadAlbumScreenShotImageOutput {
            width: 1280,
            height: 720,
            attribute: ScreenShotAttribute {
                unknown_0: 0,
                orientation: AlbumImageOrientation::None,
                unknown_1: 0,
                unknown_2: 0,
                _padding: [0u8; 0x30],
            },
            _padding: [0u8; 0x400],
        };

        // TODO: GetFile + LoadImage equivalent
        let _ = out_image;
        ResultCode::new(0)
    }

    /// LoadAlbumScreenShotThumbnail — loads a thumbnail screenshot image.
    ///
    /// Corresponds to upstream `AlbumManager::LoadAlbumScreenShotThumbnail`.
    pub fn load_album_screen_shot_thumbnail(
        &self,
        out_image_output: &mut LoadAlbumScreenShotImageOutput,
        out_image: &mut [u8],
        file_id: &AlbumFileId,
        _decoder_options: &ScreenShotDecodeOption,
    ) -> ResultCode {
        log::debug!("AlbumManager::load_album_screen_shot_thumbnail called");
        if (file_id.storage as u8) > (AlbumStorage::Sd as u8) {
            return super::caps_result::RESULT_INVALID_STORAGE;
        }
        if !self.is_mounted {
            return super::caps_result::RESULT_IS_NOT_MOUNTED;
        }

        *out_image_output = LoadAlbumScreenShotImageOutput {
            width: 320,
            height: 180,
            attribute: ScreenShotAttribute {
                unknown_0: 0,
                orientation: AlbumImageOrientation::None,
                unknown_1: 0,
                unknown_2: 0,
                _padding: [0u8; 0x30],
            },
            _padding: [0u8; 0x400],
        };

        // TODO: GetFile + LoadImage equivalent
        let _ = out_image;
        ResultCode::new(0)
    }

    /// SaveScreenShot — saves a screenshot image.
    ///
    /// Corresponds to upstream `AlbumManager::SaveScreenShot`.
    pub fn save_screen_shot(
        &mut self,
        out_entry: &mut ApplicationAlbumEntry,
        _attribute: &ScreenShotAttribute,
        _report_option: AlbumReportOption,
        _image_data: &[u8],
        _aruid: u64,
    ) -> ResultCode {
        log::debug!("AlbumManager::save_screen_shot called");
        // TODO: get title_id from system, get current time, call save_image
        *out_entry = ApplicationAlbumEntry::default();
        ResultCode::new(0)
    }

    /// SaveScreenShot (with application data) — saves a screenshot with app data.
    ///
    /// Corresponds to upstream `AlbumManager::SaveScreenShot` (overload with ApplicationData).
    pub fn save_screen_shot_with_app_data(
        &mut self,
        out_entry: &mut ApplicationAlbumEntry,
        attribute: &ScreenShotAttribute,
        report_option: AlbumReportOption,
        _app_data: &ApplicationData,
        image_data: &[u8],
        aruid: u64,
    ) -> ResultCode {
        log::debug!("AlbumManager::save_screen_shot_with_app_data called");
        // Upstream delegates to the simpler overload
        self.save_screen_shot(out_entry, attribute, report_option, image_data, aruid)
    }

    /// SaveEditedScreenShot — saves an edited screenshot image.
    ///
    /// Corresponds to upstream `AlbumManager::SaveEditedScreenShot`.
    pub fn save_edited_screen_shot(
        &mut self,
        out_entry: &mut ApplicationAlbumEntry,
        _attribute: &ScreenShotAttribute,
        _file_id: &AlbumFileId,
        _image_data: &[u8],
    ) -> ResultCode {
        log::debug!("AlbumManager::save_edited_screen_shot called");
        // TODO: get current time, call save_image
        *out_entry = ApplicationAlbumEntry::default();
        ResultCode::new(0)
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
