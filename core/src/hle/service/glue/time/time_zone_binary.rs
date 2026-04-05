// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/time_zone_binary.h
//! Port of zuyu/src/core/hle/service/glue/time/time_zone_binary.cpp
//!
//! TimeZoneBinary: mount and read operations for timezone data from the system archive.

use crate::file_sys::romfs;
use crate::file_sys::nca_metadata::ContentRecordType;
use crate::file_sys::system_archive::system_archive;
use crate::file_sys::registered_cache::ContentProvider;
use crate::file_sys::vfs::vfs_types::VirtualDir;
use crate::hle::result::{ResultCode, RESULT_SUCCESS, RESULT_UNKNOWN};
use crate::hle::service::psc::time::common::{LocationName, RuleVersion};
use crate::hle::service::psc::time::errors::{RESULT_FAILED, RESULT_TIME_ZONE_NOT_FOUND};

/// Title ID for the timezone binary system archive.
///
/// Corresponds to `TimeZoneBinaryId` in upstream time_zone_binary.cpp.
pub const TIME_ZONE_BINARY_ID: u64 = 0x10000000000080E;

/// TimeZoneBinary manages mounting and reading timezone data from the system archive.
///
/// Corresponds to `TimeZoneBinary` in upstream time_zone_binary.h.
pub struct TimeZoneBinary {
    /// Scratch space for reading timezone data.
    ///
    /// Corresponds to `time_zone_scratch_space` in upstream (0x2800 bytes).
    time_zone_scratch_space: Vec<u8>,

    /// Result of the mount operation.
    ///
    /// Corresponds to `time_zone_binary_mount_result` in upstream.
    mount_result: ResultCode,

    /// Whether the binary has been mounted successfully.
    mounted: bool,
    /// The mounted RomFS directory tree.
    /// Corresponds to `time_zone_binary_romfs` in upstream.
    time_zone_binary_romfs: Option<VirtualDir>,
    system: crate::core::SystemRef,
}

impl TimeZoneBinary {
    pub fn new(system: crate::core::SystemRef) -> Self {
        Self {
            time_zone_scratch_space: vec![0u8; 0x2800],
            mount_result: RESULT_UNKNOWN,
            mounted: false,
            time_zone_binary_romfs: None,
            system,
        }
    }

    /// Reset the timezone binary state.
    ///
    /// Corresponds to `TimeZoneBinary::Reset` in upstream.
    fn reset(&mut self) {
        self.mounted = false;
        self.mount_result = RESULT_UNKNOWN;
        self.time_zone_binary_romfs = None;
        self.time_zone_scratch_space.clear();
        self.time_zone_scratch_space.resize(0x2800, 0);
    }

    /// Mount the timezone binary from NAND or synthesize it.
    ///
    /// Corresponds to `TimeZoneBinary::Mount` in upstream.
    /// Upstream flow:
    /// 1. Try to get NCA from NAND via filesystem controller
    /// 2. Extract RomFS from NCA
    /// 3. Fall back to SynthesizeSystemArchive(TimeZoneBinaryId)
    /// 4. Extract the synthesized RomFS
    pub fn mount(&mut self) -> ResultCode {
        self.reset();

        if self.system.is_null() {
            return RESULT_UNKNOWN;
        }

        let fsc = self.system.get().get_filesystem_controller();

        if let Some(nca) = fsc
            .lock()
            .unwrap()
            .get_system_nand_contents()
            .and_then(|cache| cache.get_entry(TIME_ZONE_BINARY_ID, ContentRecordType::Data))
        {
            self.time_zone_binary_romfs = romfs::extract_romfs(nca.get_romfs());
        }

        if self.time_zone_binary_romfs.is_some() {
            self.mount_result = RESULT_SUCCESS;
            let mut etc_gmt = [0u8; 0x24];
            let bytes = b"Etc/GMT";
            etc_gmt[..bytes.len()].copy_from_slice(bytes);
            if !self.is_valid(&etc_gmt) {
                self.reset();
            }
        }

        if self.time_zone_binary_romfs.is_none() {
            if let Some(romfs_file) = system_archive::synthesize_system_archive(TIME_ZONE_BINARY_ID) {
                self.time_zone_binary_romfs = romfs::extract_romfs(Some(romfs_file));
            }
        }

        if self.time_zone_binary_romfs.is_none() {
            return RESULT_UNKNOWN;
        }

        self.mount_result = RESULT_SUCCESS;
        self.mounted = true;
        RESULT_SUCCESS
    }

    fn read(&mut self, out_buffer_size: usize, path: &str) -> Result<&[u8], ResultCode> {
        if self.mount_result != RESULT_SUCCESS {
            return Err(self.mount_result);
        }

        let romfs = self.time_zone_binary_romfs.as_ref().ok_or(RESULT_UNKNOWN)?;
        let file = romfs.get_file_relative(path).ok_or(RESULT_UNKNOWN)?;
        let file_size = file.get_size();
        if file_size == 0 {
            return Err(RESULT_UNKNOWN);
        }
        if file_size > out_buffer_size {
            return Err(RESULT_FAILED);
        }

        let bytes = file.read_all_bytes();
        if bytes.is_empty() {
            return Err(RESULT_UNKNOWN);
        }

        let read_size = bytes.len().min(self.time_zone_scratch_space.len());
        self.time_zone_scratch_space[..read_size].copy_from_slice(&bytes[..read_size]);
        Ok(&self.time_zone_scratch_space[..read_size])
    }

    /// Check if a timezone location name is valid.
    ///
    /// Corresponds to `TimeZoneBinary::IsValid` in upstream.
    /// Upstream checks whether the zoneinfo file for the given name exists
    /// in the mounted romfs.
    pub fn is_valid(&self, name: &LocationName) -> bool {
        if self.mount_result.is_error() {
            return false;
        }
        let Some(romfs) = self.time_zone_binary_romfs.as_ref() else {
            return false;
        };
        let Some(path) = self.get_time_zone_path(name) else {
            return false;
        };
        romfs.get_file_relative(&path).is_some_and(|file| file.get_size() != 0)
    }

    /// Get the total number of timezone locations.
    ///
    /// Corresponds to `TimeZoneBinary::GetTimeZoneCount` in upstream.
    /// Upstream reads binaryList.txt and counts newlines.
    pub fn get_time_zone_count(&mut self) -> u32 {
        let Some(path) = self.get_list_path() else {
            return 0;
        };
        let Ok(bytes) = self.read(0x2800, &path) else {
            return 0;
        };

        bytes.iter().filter(|&&b| b == b'\n').count() as u32
    }

    /// Get the timezone rule version.
    ///
    /// Corresponds to `TimeZoneBinary::GetTimeZoneVersion` in upstream.
    /// Upstream reads version.txt from the mounted romfs.
    pub fn get_time_zone_version(&mut self) -> Result<RuleVersion, ResultCode> {
        let path = self.get_version_path().ok_or(self.mount_result)?;
        let bytes = self.read(core::mem::size_of::<RuleVersion>(), &path)?;
        let mut version = [0u8; 0x10];
        let copy_len = bytes.len().min(version.len().saturating_sub(1));
        version[..copy_len].copy_from_slice(&bytes[..copy_len]);
        Ok(version)
    }

    /// Get the timezone rule data for a location.
    ///
    /// Corresponds to `TimeZoneBinary::GetTimeZoneRule` in upstream.
    /// Upstream reads `/zoneinfo/{name}` from the mounted romfs.
    pub fn get_time_zone_rule(&mut self, name: &LocationName) -> Result<Vec<u8>, ResultCode> {
        let path = self
            .get_time_zone_path(name)
            .ok_or(RESULT_TIME_ZONE_NOT_FOUND)?;
        let bytes = self
            .read(self.time_zone_scratch_space.len(), &path)
            .map_err(|_| RESULT_TIME_ZONE_NOT_FOUND)?;
        Ok(bytes.to_vec())
    }

    /// Get a list of timezone location names.
    ///
    /// Corresponds to `TimeZoneBinary::GetTimeZoneLocationList` in upstream.
    /// Upstream reads binaryList.txt from romfs and parses location names.
    pub fn get_time_zone_location_list(
        &mut self,
        max_names: usize,
        index: u32,
    ) -> Result<Vec<LocationName>, ResultCode> {
        let path = self.get_list_path().ok_or(self.mount_result)?;
        let data = self.read(self.time_zone_scratch_space.len(), &path)?.to_vec();
        let text = std::str::from_utf8(&data).map_err(|_| RESULT_FAILED)?;
        let mut names = Vec::new();
        for line in text.lines() {
            let trimmed = line.trim_end_matches('\r');
            if trimmed.is_empty() {
                continue;
            }
            let mut name: LocationName = [0u8; 0x24];
            let bytes = trimmed.as_bytes();
            if bytes.len() > name.len().saturating_sub(1) {
                return Err(RESULT_FAILED);
            }
            let copy_len = bytes.len();
            name[..copy_len].copy_from_slice(&bytes[..copy_len]);
            names.push(name);
            if names.len() >= index as usize + max_names {
                break;
            }
        }
        let start = (index as usize).min(names.len());
        let end = (start + max_names).min(names.len());
        Ok(names[start..end].to_vec())
    }

    /// Get the path for the binary list file.
    ///
    /// Corresponds to `TimeZoneBinary::GetListPath` in upstream.
    fn get_list_path(&self) -> Option<String> {
        if self.mount_result.is_error() {
            return None;
        }
        Some("/binaryList.txt".to_string())
    }

    /// Get the path for the version file.
    ///
    /// Corresponds to `TimeZoneBinary::GetVersionPath` in upstream.
    fn get_version_path(&self) -> Option<String> {
        if self.mount_result.is_error() {
            return None;
        }
        Some("/version.txt".to_string())
    }

    /// Get the path for a specific timezone rule.
    ///
    /// Corresponds to `TimeZoneBinary::GetTimeZonePath` in upstream.
    fn get_time_zone_path(&self, name: &LocationName) -> Option<String> {
        if self.mount_result.is_error() {
            return None;
        }
        let name_str = std::str::from_utf8(name)
            .unwrap_or("")
            .trim_end_matches('\0');
        Some(format!("/zoneinfo/{}", name_str))
    }
}
