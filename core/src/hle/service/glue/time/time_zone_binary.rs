// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/time_zone_binary.h
//! Port of zuyu/src/core/hle/service/glue/time/time_zone_binary.cpp
//!
//! TimeZoneBinary: mount and read operations for timezone data from the system archive.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::psc::time::common::{LocationName, RuleVersion};
use crate::hle::service::psc::time::errors::RESULT_FAILED;

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
    // TODO: time_zone_binary_romfs (FileSys::VirtualDir)
    // Requires filesystem integration
}

impl TimeZoneBinary {
    pub fn new() -> Self {
        Self {
            time_zone_scratch_space: vec![0u8; 0x2800],
            mount_result: ResultCode(1), // ResultUnknown
            mounted: false,
        }
    }

    /// Reset the timezone binary state.
    ///
    /// Corresponds to `TimeZoneBinary::Reset` in upstream.
    fn reset(&mut self) {
        self.mounted = false;
        self.mount_result = ResultCode(1);
        self.time_zone_scratch_space.clear();
        self.time_zone_scratch_space.resize(0x2800, 0);
    }

    /// Mount the timezone binary from NAND or synthesize it.
    ///
    /// Corresponds to `TimeZoneBinary::Mount` in upstream.
    pub fn mount(&mut self) -> ResultCode {
        self.reset();

        // TODO: Full implementation requires:
        // 1. Get NAND contents via filesystem controller
        // 2. Look up NCA by TimeZoneBinaryId
        // 3. Extract RomFS
        // 4. Fall back to synthesized system archive if needed
        //
        // For now, mark as mounted with a stub implementation
        log::warn!("TimeZoneBinary::Mount (STUBBED) - no real filesystem access");
        self.mount_result = RESULT_SUCCESS;
        self.mounted = true;
        RESULT_SUCCESS
    }

    /// Check if a timezone location name is valid.
    ///
    /// Corresponds to `TimeZoneBinary::IsValid` in upstream.
    pub fn is_valid(&self, _name: &LocationName) -> bool {
        if self.mount_result.is_error() {
            return false;
        }
        // TODO: Check if the timezone file exists in romfs
        // For now, accept all names when mounted
        self.mounted
    }

    /// Get the total number of timezone locations.
    ///
    /// Corresponds to `TimeZoneBinary::GetTimeZoneCount` in upstream.
    pub fn get_time_zone_count(&mut self) -> u32 {
        if self.mount_result.is_error() {
            return 0;
        }
        // TODO: Read binaryList.txt and count newlines
        // For now, return a minimal count
        1
    }

    /// Get the timezone rule version.
    ///
    /// Corresponds to `TimeZoneBinary::GetTimeZoneVersion` in upstream.
    pub fn get_time_zone_version(&mut self) -> Result<RuleVersion, ResultCode> {
        if self.mount_result.is_error() {
            return Err(self.mount_result);
        }
        // TODO: Read version.txt from romfs
        Ok([0u8; 0x10])
    }

    /// Get the timezone rule data for a location.
    ///
    /// Corresponds to `TimeZoneBinary::GetTimeZoneRule` in upstream.
    pub fn get_time_zone_rule(&mut self, _name: &LocationName) -> Result<Vec<u8>, ResultCode> {
        if self.mount_result.is_error() {
            return Err(self.mount_result);
        }
        // TODO: Read /zoneinfo/{name} from romfs
        log::warn!("TimeZoneBinary::GetTimeZoneRule (STUBBED)");
        Ok(Vec::new())
    }

    /// Get a list of timezone location names.
    ///
    /// Corresponds to `TimeZoneBinary::GetTimeZoneLocationList` in upstream.
    pub fn get_time_zone_location_list(
        &mut self,
        max_names: usize,
        index: u32,
    ) -> Result<Vec<LocationName>, ResultCode> {
        if self.mount_result.is_error() {
            return Err(self.mount_result);
        }

        // TODO: Read binaryList.txt from romfs, parse location names
        // For now, return a minimal list
        if index > 0 {
            return Ok(Vec::new());
        }

        let mut utc: LocationName = [0u8; 0x24];
        utc[0] = b'U';
        utc[1] = b'T';
        utc[2] = b'C';

        let mut result = Vec::new();
        if max_names > 0 {
            result.push(utc);
        }
        Ok(result)
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
