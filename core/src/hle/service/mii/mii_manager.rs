// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/mii_manager.h
//! Port of zuyu/src/core/hle/service/mii/mii_manager.cpp
//!
//! MiiManager: high-level Mii management, building random/default Miis,
//! and coordinating between the database and the Mii types.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::mii_database_manager::DatabaseManager;
use super::mii_types::{Age, Gender, Race};
use super::types::store_data::StoreData;

/// MiiManager coordinates Mii operations.
pub struct MiiManager {
    database_manager: DatabaseManager,
}

impl MiiManager {
    pub fn new() -> Self {
        Self {
            database_manager: DatabaseManager::new(),
        }
    }

    /// Initialize the Mii manager and its database.
    pub fn initialize(&mut self) -> ResultCode {
        self.database_manager.initialize()
    }

    /// Check if the database has been updated.
    pub fn is_updated(&self) -> bool {
        self.database_manager.is_updated()
    }

    /// Check if the database is full.
    pub fn is_full_database(&self) -> bool {
        self.database_manager.is_full_database()
    }

    /// Get the count of Mii entries.
    pub fn get_count(&self) -> u32 {
        self.database_manager.get_count()
    }

    /// Build a random Mii based on age, gender, and race parameters.
    ///
    /// Upstream creates a StoreData, calls store_data.BuildRandom(age, gender, race),
    /// then sets out_char_info from the store data via CharInfo::SetFromStoreData.
    pub fn build_random(&self, age: Age, gender: Gender, race: Race) -> StoreData {
        let mut store_data = StoreData::new();
        store_data.build_random(age, gender, race);
        store_data
    }

    /// Build a default Mii with index.
    ///
    /// Upstream creates a StoreData, calls store_data.BuildDefault(index).
    pub fn build_default(&self, index: u32) -> StoreData {
        let mut store_data = StoreData::new();
        store_data.build_default(index);
        store_data
    }

    /// Check if the database was broken and cleared.
    pub fn is_broken_database_with_clear_flag(&self) -> bool {
        self.database_manager.is_broken_with_clear_flag()
    }

    /// Destroy the database file.
    pub fn destroy_file(&mut self) -> ResultCode {
        self.database_manager.destroy_file()
    }

    /// Delete the database file.
    pub fn delete_file(&mut self) -> ResultCode {
        self.database_manager.delete_file()
    }

    /// Format (reset) the database.
    pub fn format(&mut self) -> ResultCode {
        self.database_manager.format()
    }
}

impl Default for MiiManager {
    fn default() -> Self {
        Self::new()
    }
}
