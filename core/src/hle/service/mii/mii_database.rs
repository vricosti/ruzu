// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/mii_database.h
//! Port of zuyu/src/core/hle/service/mii/mii_database.cpp
//!
//! NintendoFigurineDatabase: the on-disk Mii database format.

use super::mii_util;

/// Maximum number of Mii entries in the database.
pub const MAX_MII_COUNT: usize = 100;

/// Magic number for the database file.
pub const DATABASE_MAGIC: u32 = 0x4644464E; // "NFDB" in LE

/// Database version.
pub const DATABASE_VERSION: u8 = 1;

/// NintendoFigurineDatabase represents the on-disk Mii database.
///
/// Upstream struct layout (0x1A98 bytes):
/// - magic: u32 (offset 0x00)
/// - version: u8 (offset 0x04)
/// - padding: [u8; 3]
/// - entries: [StoreData; 100] (offset 0x08)
/// - crc: u16 (offset 0x1A96)
#[derive(Clone)]
pub struct NintendoFigurineDatabase {
    pub magic: u32,
    pub version: u8,
    // entries would be [StoreData; MAX_MII_COUNT] once StoreData is ported
    pub entry_count: usize,
}

impl NintendoFigurineDatabase {
    pub fn new() -> Self {
        Self {
            magic: DATABASE_MAGIC,
            version: DATABASE_VERSION,
            entry_count: 0,
        }
    }

    pub fn is_valid(&self) -> bool {
        self.magic == DATABASE_MAGIC && self.version == DATABASE_VERSION
    }

    /// Calculate the CRC-16 for the database.
    pub fn calculate_crc(&self, data: &[u8]) -> u16 {
        if data.len() < 2 {
            return 0;
        }
        mii_util::calculate_crc16(&data[..data.len() - 2])
    }
}

impl Default for NintendoFigurineDatabase {
    fn default() -> Self {
        Self::new()
    }
}
