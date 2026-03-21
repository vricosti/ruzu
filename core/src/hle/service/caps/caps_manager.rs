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
    ///
    /// Upstream calls GetFile to resolve the path, then removes the file.
    pub fn delete_album_file(&mut self, file_id: &AlbumFileId) -> ResultCode {
        log::debug!("AlbumManager::delete_album_file called");
        if (file_id.storage as u8) > (AlbumStorage::Sd as u8) {
            return super::caps_result::RESULT_INVALID_STORAGE;
        }
        if !self.is_mounted {
            return super::caps_result::RESULT_IS_NOT_MOUNTED;
        }
        let path = match self.get_file(file_id) {
            Ok(p) => p,
            Err(rc) => return rc,
        };
        if std::fs::remove_file(&path).is_err() {
            return super::caps_result::RESULT_FILE_NOT_FOUND;
        }
        self.album_files.remove(file_id);
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
            let entry_size: u64 = std::fs::metadata(path).map(|m| m.len()).unwrap_or(0);
            out_entries[out_entries_count as usize] = AlbumEntry {
                entry_size,
                file_id: *file_id,
            };
            out_entries_count += 1;
        }

        (ResultCode::new(0), out_entries_count)
    }

    /// GetAlbumFileList — retrieves album entries filtered by content type and posix time range.
    ///
    /// Corresponds to upstream `AlbumManager::GetAlbumFileList(span<ApplicationAlbumFileEntry>, ...)`.
    /// Converts posix times to AlbumFileDateTime, delegates to the datetime-based overload,
    /// then wraps results in ApplicationAlbumFileEntry.
    pub fn get_album_file_list_aafe(
        &self,
        out_entries: &mut [ApplicationAlbumFileEntry],
        content_type: ContentType,
        _start_posix_time: i64,
        _end_posix_time: i64,
        aruid: u64,
    ) -> (ResultCode, u64) {
        log::debug!("AlbumManager::get_album_file_list_aafe called");
        if !self.is_mounted {
            return (super::caps_result::RESULT_IS_NOT_MOUNTED, 0);
        }
        // Upstream converts posix times to AlbumFileDateTime via ConvertToAlbumDateTime
        // which requires the time:u service. Use default (zero) dates as a fallback,
        // effectively not filtering by date.
        let mut album_entries = vec![ApplicationAlbumEntry::default(); out_entries.len()];
        let (result, count) = self.get_album_file_list_aae(
            &mut album_entries,
            content_type,
            AlbumFileDateTime::default(),
            AlbumFileDateTime::default(),
            aruid,
        );
        if !result.is_success() {
            return (result, 0);
        }
        for i in 0..count as usize {
            out_entries[i] = ApplicationAlbumFileEntry {
                entry: album_entries[i],
                datetime: album_entries[i].datetime,
                unknown: 0,
            };
        }
        (ResultCode::new(0), count)
    }

    /// GetAlbumFileList — retrieves ApplicationAlbumEntry filtered by content type and date range.
    ///
    /// Corresponds to upstream `AlbumManager::GetAlbumFileList(span<ApplicationAlbumEntry>, ...)`.
    pub fn get_album_file_list_aae(
        &self,
        out_entries: &mut [ApplicationAlbumEntry],
        content_type: ContentType,
        _start_date: AlbumFileDateTime,
        _end_date: AlbumFileDateTime,
        _aruid: u64,
    ) -> (ResultCode, u64) {
        log::debug!("AlbumManager::get_album_file_list_aae called");
        if !self.is_mounted {
            return (super::caps_result::RESULT_IS_NOT_MOUNTED, 0);
        }
        let mut out_entries_count: u64 = 0;
        for (file_id, path) in &self.album_files {
            if file_id.content_type != content_type {
                continue;
            }
            // Upstream filters by date range; since we pass default dates, skip filtering.
            if out_entries_count as usize >= SD_ALBUM_FILE_LIMIT {
                break;
            }
            if out_entries_count as usize >= out_entries.len() {
                break;
            }
            let entry_size: u64 = std::fs::metadata(path).map(|m| m.len()).unwrap_or(0);
            out_entries[out_entries_count as usize] = ApplicationAlbumEntry {
                size: entry_size,
                hash: 0,
                datetime: file_id.date,
                storage: file_id.storage,
                content: content_type,
                _padding: [0u8; 5],
                unknown: 1,
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

        // Upstream calls GetFile then LoadImage (stbi_load_from_memory + stbir_resize).
        // Image decoding via stb is not available in Rust; return success with the output
        // dimensions set but image buffer untouched. Games typically handle missing data
        // gracefully.
        match self.get_file(file_id) {
            Ok(_path) => {
                log::warn!(
                    "AlbumManager::load_album_screen_shot_image: image loading not yet implemented"
                );
            }
            Err(rc) => return rc,
        }
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

        // Upstream calls GetFile then LoadImage (stbi_load_from_memory + stbir_resize).
        // Image decoding via stb is not available in Rust; return success with the output
        // dimensions set but image buffer untouched.
        match self.get_file(file_id) {
            Ok(_path) => {
                log::warn!(
                    "AlbumManager::load_album_screen_shot_thumbnail: image loading not yet implemented"
                );
            }
            Err(rc) => return rc,
        }
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
        // Upstream gets title_id from system.GetApplicationProcessProgramID() and
        // obtains current time from the user system clock via time:u service.
        // Since we don't have the time service wired here, use SystemTime as a fallback.
        let now = Self::current_album_date_time();
        *out_entry = ApplicationAlbumEntry {
            size: 0,
            hash: 0,
            datetime: now,
            storage: AlbumStorage::Sd,
            content: ContentType::Screenshot,
            _padding: [0u8; 5],
            unknown: 1,
        };
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
        // Upstream obtains current time from the user system clock via time:u service.
        // Since we don't have the time service wired here, use SystemTime as a fallback.
        let now = Self::current_album_date_time();
        *out_entry = ApplicationAlbumEntry {
            size: 0,
            hash: 0,
            datetime: now,
            storage: AlbumStorage::Sd,
            content: ContentType::Screenshot,
            _padding: [0u8; 5],
            unknown: 1,
        };
        ResultCode::new(0)
    }

    /// FlipVerticallyOnWrite.
    ///
    /// Upstream calls `stbi_flip_vertically_on_write(flip)`. Since we don't use stb for
    /// image writing, this is a no-op. The flag would only matter when SaveImage encodes PNGs.
    pub fn flip_vertically_on_write(&self, flip: bool) {
        log::debug!(
            "AlbumManager::flip_vertically_on_write called, flip={}",
            flip
        );
    }

    /// GetFile — resolves an AlbumFileId to its filesystem path.
    ///
    /// Corresponds to upstream `AlbumManager::GetFile`.
    fn get_file(&self, file_id: &AlbumFileId) -> Result<PathBuf, ResultCode> {
        match self.album_files.get(file_id) {
            Some(path) => Ok(path.clone()),
            None => Err(super::caps_result::RESULT_FILE_NOT_FOUND),
        }
    }

    /// FindScreenshots — scans the screenshots directory and populates album_files.
    ///
    /// Upstream iterates the screenshots directory, parses filenames to extract
    /// AlbumEntry metadata, and populates the album_files map.
    fn find_screenshots(&mut self) {
        self.is_mounted = false;
        self.album_files.clear();

        let screenshots_dir =
            common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::ScreenshotsDir);
        if let Ok(entries) = std::fs::read_dir(&screenshots_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if !path.is_file() {
                    continue;
                }
                if let Some(album_entry) = Self::get_album_entry_from_path(&path) {
                    let mut file_id = album_entry.file_id;
                    // Deduplicate: increment unique_id if this file_id already exists
                    while self.album_files.contains_key(&file_id) {
                        file_id.date.unique_id = file_id.date.unique_id.wrapping_add(1);
                        if file_id.date.unique_id == 0 {
                            break;
                        }
                    }
                    self.album_files.insert(file_id, path);
                }
            }
        }
        self.is_mounted = true;
    }

    /// GetAlbumEntry — parses a screenshot filename to extract album metadata.
    ///
    /// Upstream filename format: `{application_id:016x}_{YYYY-MM-DD}_{HH-MM-SS}_{...}.png`
    fn get_album_entry_from_path(path: &std::path::Path) -> Option<AlbumEntry> {
        let filename = path.file_name()?.to_str()?;
        let mut parts = filename.splitn(4, '_');
        let application = parts.next()?;
        let date = parts.next()?;
        let time = parts.next()?;

        let mut date_parts = date.splitn(3, '-');
        let year: i16 = date_parts.next()?.parse().ok()?;
        let month: i8 = date_parts.next()?.parse().ok()?;
        let day: i8 = date_parts.next()?.parse().ok()?;

        let mut time_parts = time.splitn(3, '-');
        let hour: i8 = time_parts.next()?.parse().ok()?;
        let minute: i8 = time_parts.next()?.parse().ok()?;
        let second: i8 = time_parts.next()?.parse().ok()?;

        let application_id = u64::from_str_radix(application, 16).ok()?;

        Some(AlbumEntry {
            entry_size: 1,
            file_id: AlbumFileId {
                application_id,
                date: AlbumFileDateTime {
                    year,
                    month,
                    day,
                    hour,
                    minute,
                    second,
                    unique_id: 0,
                },
                storage: AlbumStorage::Sd,
                content_type: ContentType::Screenshot,
                _padding: [0u8; 5],
                unknown: 1,
            },
        })
    }

    /// Returns the current time as an AlbumFileDateTime.
    ///
    /// Upstream uses the time:u service's user system clock + timezone conversion.
    /// As a fallback, we use `std::time::SystemTime` with UTC.
    fn current_album_date_time() -> AlbumFileDateTime {
        use std::time::SystemTime;
        let secs = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs() as i64)
            .unwrap_or(0);
        // Simple UTC decomposition (no timezone service available)
        let days = secs / 86400;
        let time_of_day = secs % 86400;
        let hour = (time_of_day / 3600) as i8;
        let minute = ((time_of_day % 3600) / 60) as i8;
        let second = (time_of_day % 60) as i8;
        // Civil date from days since epoch (algorithm from Howard Hinnant)
        let z = days + 719468;
        let era = (if z >= 0 { z } else { z - 146096 }) / 146097;
        let doe = (z - era * 146097) as u32;
        let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
        let y = yoe as i64 + era * 400;
        let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
        let mp = (5 * doy + 2) / 153;
        let day = (doy - (153 * mp + 2) / 5 + 1) as i8;
        let month = if mp < 10 { mp + 3 } else { mp - 9 } as i8;
        let year = (if month <= 2 { y + 1 } else { y }) as i16;
        AlbumFileDateTime {
            year,
            month,
            day,
            hour,
            minute,
            second,
            unique_id: 0,
        }
    }
}
