// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/mii_database_manager.h
//! Port of zuyu/src/core/hle/service/mii/mii_database_manager.cpp
//!
//! DatabaseManager: manages the Mii database file lifecycle.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::mii_database::{NintendoFigurineDatabase, MAX_MII_COUNT};

/// DatabaseManager handles loading, saving, and querying the Mii database.
pub struct DatabaseManager {
    database: NintendoFigurineDatabase,
    is_dirty: bool,
    is_broken: bool,
}

impl DatabaseManager {
    pub fn new() -> Self {
        Self {
            database: NintendoFigurineDatabase::new(),
            is_dirty: false,
            is_broken: false,
        }
    }

    /// Initialize the database manager.
    /// Loads the database from the save data filesystem.
    ///
    /// Upstream mounts save data at NAND/system/save/8000000000000030, reads
    /// MiiDatabase.dat into the NintendoFigurineDatabase struct, and validates
    /// the CRC. Filesystem save data mounting (Common::FS / VfsRealDirectory)
    /// is not yet wired for Mii save data. For now, start with an empty database.
    /// When the filesystem layer supports Mii save data paths, load the database
    /// file here.
    pub fn initialize(&mut self) -> ResultCode {
        log::debug!(
            "DatabaseManager::initialize: filesystem load not yet wired, starting with empty database"
        );
        self.database = NintendoFigurineDatabase::new();
        RESULT_SUCCESS
    }

    /// Check if the database has been updated since last query.
    pub fn is_updated(&self) -> bool {
        self.is_dirty
    }

    /// Check if the database is full.
    pub fn is_full_database(&self) -> bool {
        self.database.entry_count >= MAX_MII_COUNT
    }

    /// Get the number of Mii entries.
    pub fn get_count(&self) -> u32 {
        self.database.entry_count as u32
    }

    /// Check if the database file was broken and cleared.
    pub fn is_broken_with_clear_flag(&self) -> bool {
        self.is_broken
    }

    /// Mark the database as needing save.
    pub fn mark_dirty(&mut self) {
        self.is_dirty = true;
    }

    /// Save the database to the filesystem.
    ///
    /// Upstream writes the NintendoFigurineDatabase struct to MiiDatabase.dat
    /// in the mounted save data directory. Filesystem save data writing is not
    /// yet wired for Mii save data. When available, serialize self.database
    /// to the save file here.
    pub fn save_database(&mut self) -> ResultCode {
        log::debug!(
            "DatabaseManager::save_database: filesystem save not yet wired, marking clean"
        );
        self.is_dirty = false;
        RESULT_SUCCESS
    }

    /// Destroy (format) the database file.
    pub fn destroy_file(&mut self) -> ResultCode {
        self.database = NintendoFigurineDatabase::new();
        self.is_dirty = true;
        RESULT_SUCCESS
    }

    /// Delete the database file.
    pub fn delete_file(&mut self) -> ResultCode {
        self.database = NintendoFigurineDatabase::new();
        self.is_dirty = false;
        RESULT_SUCCESS
    }

    /// Format (reset) the database.
    pub fn format(&mut self) -> ResultCode {
        self.database = NintendoFigurineDatabase::new();
        self.is_dirty = true;
        self.save_database()
    }
}

impl Default for DatabaseManager {
    fn default() -> Self {
        Self::new()
    }
}
