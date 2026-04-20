// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/mii_database_manager.h
//! Port of zuyu/src/core/hle/service/mii/mii_database_manager.cpp
//!
//! DatabaseManager: manages the Mii database file lifecycle.

use std::path::PathBuf;

use common::fs::file::IOFile;
use common::fs::fs::{create_dirs, remove_file};
use common::fs::fs_types::{FileAccessMode, FileShareFlag, FileType};
use common::fs::path_util::{get_ruzu_path, RuzuPath};

use super::mii_database::{NintendoFigurineDatabase, MAX_MII_COUNT, MII_MAGIC};
use super::mii_result::{
    RESULT_DATABASE_FULL, RESULT_INVALID_ARGUMENT, RESULT_INVALID_CHAR_INFO2,
    RESULT_INVALID_CHAR_INFO_TYPE, RESULT_INVALID_OPERATION, RESULT_INVALID_STORE_DATA,
    RESULT_NOT_FOUND, RESULT_NOT_UPDATED,
};
use super::mii_types::{DatabaseSessionMetadata, ValidationResult};
use super::types::char_info::CharInfo;
use super::types::store_data::StoreData;
use crate::hle::result::{ResultCode, RESULT_SUCCESS, RESULT_UNKNOWN};

const DB_FILE_NAME: &str = "MiiDatabase.dat";

pub struct DatabaseManager {
    database: NintendoFigurineDatabase,
    is_test_db: bool,
    is_moddified: bool,
    is_save_data_mounted: bool,
    update_counter: u64,
    system_save_dir: PathBuf,
}

impl DatabaseManager {
    pub fn new() -> Self {
        Self {
            database: NintendoFigurineDatabase::new(),
            is_test_db: false,
            is_moddified: false,
            is_save_data_mounted: false,
            update_counter: 0,
            system_save_dir: PathBuf::new(),
        }
    }

    pub fn mount_save_data(&mut self) -> ResultCode {
        if !self.is_save_data_mounted {
            self.system_save_dir = get_ruzu_path(RuzuPath::NANDDir).join(if self.is_test_db {
                "system/save/8000000000000031"
            } else {
                "system/save/8000000000000030"
            });
            if !create_dirs(&self.system_save_dir) {
                return RESULT_UNKNOWN;
            }
        }

        self.is_save_data_mounted = true;
        RESULT_SUCCESS
    }

    pub fn set_test_db(&mut self, is_test_db: bool) {
        if self.is_save_data_mounted && self.is_test_db != is_test_db {
            self.is_save_data_mounted = false;
            self.system_save_dir.clear();
        }
        self.is_test_db = is_test_db;
    }

    pub fn initialize(
        &mut self,
        metadata: &mut DatabaseSessionMetadata,
        is_database_broken: &mut bool,
    ) -> ResultCode {
        *is_database_broken = false;
        if !self.is_save_data_mounted {
            return RESULT_INVALID_ARGUMENT;
        }

        self.database.clean_database();
        self.update_counter = self.update_counter.saturating_add(1);
        metadata.update_counter = self.update_counter;
        self.is_moddified = false;

        let db_path = self.system_save_dir.join(DB_FILE_NAME);
        let mut db_file = IOFile::new(
            &db_path,
            FileAccessMode::Read,
            FileType::BinaryFile,
            FileShareFlag::ShareReadOnly,
        );

        if !db_file.is_open() {
            return self.save_database();
        }

        let file_size = db_file.get_size();
        if file_size as usize != core::mem::size_of::<NintendoFigurineDatabase>() {
            log::error!("Mii database has invalid size: {}", file_size);
            *is_database_broken = true;
        }

        let mut loaded = NintendoFigurineDatabase::default();
        if unsafe { !db_file.read_object(&mut loaded) } {
            *is_database_broken = true;
        }

        if *is_database_broken {
            log::error!("Mii database is corrupted");
            self.database.clean_database();
            return RESULT_UNKNOWN;
        }

        self.database = loaded;
        let result = self.database.check_integrity();
        if result.is_error() {
            log::error!(
                "Mii database is corrupted: 0x{:08x}",
                result.get_inner_value()
            );
            self.database.clean_database();
            return RESULT_SUCCESS;
        }

        log::info!(
            "Successfully loaded mii database. size={}",
            self.database.get_database_length()
        );
        RESULT_SUCCESS
    }

    pub fn is_updated(&self) -> bool {
        self.is_moddified
    }

    pub fn get_update_counter(&self) -> u64 {
        self.update_counter
    }

    pub fn is_full_database(&self) -> bool {
        self.database.get_database_length() as usize >= MAX_MII_COUNT
    }

    pub fn get_count(&self, metadata: &DatabaseSessionMetadata) -> u32 {
        self.database.get_count(metadata)
    }

    pub fn get(&self, metadata: &DatabaseSessionMetadata, index: usize) -> Option<StoreData> {
        if metadata.magic == MII_MAGIC {
            if index >= self.database.get_database_length() as usize {
                return None;
            }
            return Some(self.database.get(index));
        }

        let mut virtual_index = 0usize;
        for i in 0..self.database.get_database_length() as usize {
            let store_data = self.database.get(i);
            if store_data.is_special() {
                continue;
            }
            if virtual_index == index {
                return Some(store_data);
            }
            virtual_index += 1;
        }

        if self.database.get_database_length() == 0 {
            None
        } else {
            Some(self.database.get(0))
        }
    }

    pub fn find_index(
        &self,
        metadata: &DatabaseSessionMetadata,
        create_id: [u8; 16],
    ) -> Result<u32, ResultCode> {
        let Some(index) = self.database.get_index_by_creator_id(create_id) else {
            return Err(RESULT_NOT_FOUND);
        };

        if metadata.magic == MII_MAGIC {
            return Ok(index);
        }

        if self.database.get(index as usize).is_special() {
            return Err(RESULT_NOT_FOUND);
        }

        let mut out_index = 0u32;
        if index < 1 {
            return Ok(out_index);
        }

        for i in 0..=index as usize {
            let store_data = self.database.get(i);
            if store_data.is_special() {
                continue;
            }
            out_index += 1;
        }
        Ok(out_index.saturating_sub(1))
    }

    pub fn find_index_signed(
        &self,
        out_index: &mut i32,
        create_id: [u8; 16],
        is_special: bool,
    ) -> ResultCode {
        let Some(index) = self.database.get_index_by_creator_id(create_id) else {
            return RESULT_NOT_FOUND;
        };

        if is_special {
            *out_index = index as i32;
            return RESULT_SUCCESS;
        }

        if self.database.get(index as usize).is_special() {
            return RESULT_NOT_FOUND;
        }

        *out_index = 0;
        if index < 1 {
            return RESULT_SUCCESS;
        }

        for i in 0..index as usize {
            if self.database.get(i).is_special() {
                continue;
            }
            *out_index += 1;
        }
        RESULT_SUCCESS
    }

    pub fn find_index_raw(&self, create_id: [u8; 16], is_special: bool) -> Result<i32, ResultCode> {
        let mut out_index = 0;
        let result = self.find_index_signed(&mut out_index, create_id, is_special);
        if result.is_error() {
            return Err(result);
        }
        Ok(out_index)
    }

    pub fn find_move_index(&self, new_index: u32, create_id: [u8; 16]) -> Result<u32, ResultCode> {
        let database_size = self.database.get_database_length() as usize;

        if database_size >= 1 {
            let mut virtual_index = 0u32;
            for i in 0..database_size {
                let store_data = self.database.get(i);
                if store_data.is_special() {
                    continue;
                }
                if virtual_index == new_index {
                    let Some(index) = self.database.get_index_by_creator_id(create_id) else {
                        return Err(RESULT_NOT_FOUND);
                    };
                    if store_data.is_special() {
                        return Err(RESULT_INVALID_OPERATION);
                    }
                    return Ok(index);
                }
                virtual_index += 1;
            }
        }

        let Some(index) = self.database.get_index_by_creator_id(create_id) else {
            return Err(RESULT_NOT_FOUND);
        };
        if self.database.get(index as usize).is_special() {
            return Err(RESULT_INVALID_OPERATION);
        }
        Ok(index)
    }

    pub fn r#move(
        &mut self,
        metadata: &mut DatabaseSessionMetadata,
        new_index: u32,
        create_id: [u8; 16],
    ) -> ResultCode {
        let current_index = if metadata.magic == MII_MAGIC {
            let Some(index) = self.database.get_index_by_creator_id(create_id) else {
                return RESULT_NOT_FOUND;
            };
            index
        } else {
            let index = match self.find_move_index(new_index, create_id) {
                Ok(index) => index,
                Err(result) => return result,
            };
            index
        };

        let result = self.database.move_entry(current_index, new_index);
        if result.is_failure() {
            return result;
        }

        self.is_moddified = true;
        self.update_counter = self.update_counter.saturating_add(1);
        metadata.update_counter = self.update_counter;
        RESULT_SUCCESS
    }

    pub fn add_or_replace(
        &mut self,
        metadata: &mut DatabaseSessionMetadata,
        store_data: StoreData,
    ) -> ResultCode {
        if store_data.is_valid() != ValidationResult::NoErrors {
            return RESULT_INVALID_STORE_DATA;
        }
        if metadata.magic != MII_MAGIC && store_data.is_special() {
            return RESULT_INVALID_OPERATION;
        }

        if let Some(index) = self
            .database
            .get_index_by_creator_id(store_data.get_create_id())
        {
            let old_store_data = self.database.get(index as usize);
            if store_data.is_special() != old_store_data.is_special() {
                return RESULT_INVALID_OPERATION;
            }
            self.database.replace(index, store_data);
        } else {
            if self.database.is_full() {
                return RESULT_DATABASE_FULL;
            }
            self.database.add(store_data);
        }

        self.is_moddified = true;
        self.update_counter = self.update_counter.saturating_add(1);
        metadata.update_counter = self.update_counter;
        RESULT_SUCCESS
    }

    pub fn delete(
        &mut self,
        metadata: &mut DatabaseSessionMetadata,
        create_id: [u8; 16],
    ) -> ResultCode {
        let Some(index) = self.database.get_index_by_creator_id(create_id) else {
            return RESULT_NOT_FOUND;
        };

        if metadata.magic != MII_MAGIC {
            let store_data = self.database.get(index as usize);
            if store_data.is_special() {
                return RESULT_INVALID_OPERATION;
            }
        }

        self.database.delete(index);
        self.is_moddified = true;
        self.update_counter = self.update_counter.saturating_add(1);
        metadata.update_counter = self.update_counter;
        RESULT_SUCCESS
    }

    pub fn append(
        &mut self,
        metadata: &mut DatabaseSessionMetadata,
        char_info: &CharInfo,
    ) -> ResultCode {
        if char_info.verify() != ValidationResult::NoErrors {
            return RESULT_INVALID_CHAR_INFO2;
        }
        if char_info.type_val == 1 {
            return RESULT_INVALID_CHAR_INFO_TYPE;
        }

        let mut store_data = StoreData::new();
        loop {
            store_data.build_with_char_info(char_info);
            if self
                .database
                .get_index_by_creator_id(store_data.get_create_id())
                .is_none()
            {
                break;
            }
        }

        let result = store_data.restore();
        if result.is_success() || result == RESULT_NOT_UPDATED {
            return self.add_or_replace(metadata, store_data);
        }

        result
    }

    pub fn is_broken_with_clear_flag(&self) -> bool {
        false
    }

    pub fn mark_dirty(&mut self) {
        self.is_moddified = true;
        self.update_counter = self.update_counter.saturating_add(1);
    }

    pub fn save_database(&mut self) -> ResultCode {
        if !self.is_save_data_mounted {
            let result = self.mount_save_data();
            if result.is_error() {
                return result;
            }
        }

        let db_path = self.system_save_dir.join(DB_FILE_NAME);
        let mut db_file = IOFile::new(
            &db_path,
            FileAccessMode::Write,
            FileType::BinaryFile,
            FileShareFlag::ShareReadOnly,
        );
        if !db_file.is_open() {
            return RESULT_UNKNOWN;
        }

        if unsafe { !db_file.write_object(&self.database) } || !db_file.flush() {
            return RESULT_UNKNOWN;
        }

        self.is_moddified = false;
        RESULT_SUCCESS
    }

    pub fn destroy_file(&mut self, metadata: &mut DatabaseSessionMetadata) -> ResultCode {
        self.database.corrupt_crc();
        self.is_moddified = true;
        self.update_counter = self.update_counter.saturating_add(1);
        metadata.update_counter = self.update_counter;

        let result = self.save_database();
        self.database.clean_database();
        result
    }

    pub fn delete_file(&mut self) -> ResultCode {
        let db_path = self.system_save_dir.join(DB_FILE_NAME);
        if remove_file(&db_path) {
            RESULT_SUCCESS
        } else {
            RESULT_UNKNOWN
        }
    }

    pub fn format(&mut self, metadata: &mut DatabaseSessionMetadata) -> ResultCode {
        self.database.clean_database();
        self.is_moddified = true;
        self.update_counter = self.update_counter.saturating_add(1);
        metadata.update_counter = self.update_counter;
        RESULT_SUCCESS
    }
}

impl Default for DatabaseManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::fs::path_util::set_ruzu_path;
    use std::fs;

    #[test]
    fn mount_save_data_uses_test_db_path_when_enabled() {
        let base =
            std::env::temp_dir().join(format!("ruzu_mii_db_test_mode_{}", std::process::id()));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let mut manager = DatabaseManager::new();
        manager.set_test_db(true);
        assert_eq!(manager.mount_save_data(), RESULT_SUCCESS);
        assert_eq!(
            manager.system_save_dir,
            base.join("system/save/8000000000000031")
        );
        assert!(manager.system_save_dir.exists());

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn save_and_initialize_round_trip_database_file() {
        let base = std::env::temp_dir().join(format!("ruzu_mii_db_test_{}", std::process::id()));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let mut manager = DatabaseManager::new();
        assert_eq!(manager.mount_save_data(), RESULT_SUCCESS);
        assert_eq!(manager.save_database(), RESULT_SUCCESS);

        let db_path = base.join("system/save/8000000000000030").join(DB_FILE_NAME);
        assert!(db_path.exists());

        let mut reloaded = DatabaseManager::new();
        let mut metadata = DatabaseSessionMetadata::default();
        let mut is_database_broken = false;
        assert_eq!(
            reloaded.initialize(&mut metadata, &mut is_database_broken),
            RESULT_SUCCESS
        );
        assert!(!is_database_broken);
        assert_eq!(reloaded.get_count(&metadata), 0);

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn initialize_returns_unknown_and_sets_broken_on_invalid_file_size() {
        let base =
            std::env::temp_dir().join(format!("ruzu_mii_db_broken_size_{}", std::process::id()));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let save_dir = base.join("system/save/8000000000000030");
        fs::create_dir_all(&save_dir).unwrap();
        fs::write(save_dir.join(DB_FILE_NAME), [0u8; 16]).unwrap();

        let mut manager = DatabaseManager::new();
        assert_eq!(manager.mount_save_data(), RESULT_SUCCESS);

        let mut metadata = DatabaseSessionMetadata::default();
        let mut is_database_broken = false;
        assert_eq!(
            manager.initialize(&mut metadata, &mut is_database_broken),
            RESULT_UNKNOWN
        );
        assert!(is_database_broken);
        assert_eq!(manager.get_count(&metadata), 0);

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn move_skips_special_entries_for_non_mii_magic_metadata() {
        let mut manager = DatabaseManager::new();
        let mut special = StoreData::new();
        special.build_default(0);
        special.core_data.set_type(1);
        special.set_checksum();
        let special_id = special.get_create_id();

        let mut normal_a = StoreData::new();
        normal_a.build_default(1);
        let normal_a_id = normal_a.get_create_id();

        let mut normal_b = StoreData::new();
        normal_b.build_default(2);

        manager.database.add(special);
        manager.database.add(normal_a);
        manager.database.add(normal_b);

        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(
            manager.r#move(&mut metadata, 0, normal_a_id),
            RESULT_SUCCESS
        );
        assert_eq!(manager.database.get(0).get_create_id(), special_id);
        assert_eq!(manager.database.get(1).get_create_id(), normal_a_id);
        assert_eq!(metadata.update_counter, 1);
    }

    #[test]
    fn find_index_signed_skips_special_entries_for_non_special_lookup() {
        let mut manager = DatabaseManager::new();

        let mut special = StoreData::new();
        special.build_default(0);
        special.core_data.set_type(1);
        special.set_checksum();

        let mut normal = StoreData::new();
        normal.build_default(1);
        let normal_id = normal.get_create_id();

        manager.database.add(special);
        manager.database.add(normal);

        let mut out_index = -1;
        assert_eq!(
            manager.find_index_signed(&mut out_index, normal_id, false),
            RESULT_SUCCESS
        );
        assert_eq!(out_index, 0);
    }

    #[test]
    fn add_or_replace_rejects_special_store_data_without_mii_magic() {
        let mut manager = DatabaseManager::new();
        let mut special = StoreData::new();
        special.build_default(0);
        special.core_data.set_type(1);
        special.set_checksum();

        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(
            manager.add_or_replace(&mut metadata, special),
            RESULT_INVALID_OPERATION
        );
    }

    #[test]
    fn delete_rejects_special_store_data_without_mii_magic() {
        let mut manager = DatabaseManager::new();
        let mut special = StoreData::new();
        special.build_default(0);
        special.core_data.set_type(1);
        special.set_checksum();
        let create_id = special.get_create_id();
        manager.database.add(special);

        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(
            manager.delete(&mut metadata, create_id),
            RESULT_INVALID_OPERATION
        );
    }

    #[test]
    fn destroy_file_updates_metadata_saves_corrupt_blob_and_cleans_database() {
        let base =
            std::env::temp_dir().join(format!("ruzu_mii_db_destroy_file_{}", std::process::id()));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let mut manager = DatabaseManager::new();
        assert_eq!(manager.mount_save_data(), RESULT_SUCCESS);

        let mut store_data = StoreData::new();
        store_data.build_default(0);
        manager.database.add(store_data);

        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(manager.destroy_file(&mut metadata), RESULT_SUCCESS);
        assert_eq!(metadata.update_counter, 1);
        assert_eq!(manager.database.get_database_length(), 0);
        assert!(manager.is_updated());

        let db_path = base.join("system/save/8000000000000030").join(DB_FILE_NAME);
        assert!(db_path.exists());

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn format_updates_metadata_without_writing_file() {
        let mut manager = DatabaseManager::new();
        let mut store_data = StoreData::new();
        store_data.build_default(0);
        manager.database.add(store_data);

        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(manager.format(&mut metadata), RESULT_SUCCESS);
        assert_eq!(metadata.update_counter, 1);
        assert_eq!(manager.database.get_database_length(), 0);
        assert!(manager.is_updated());
    }

    #[test]
    fn delete_file_returns_unknown_and_keeps_memory_state_on_fs_failure() {
        let mut manager = DatabaseManager::new();
        let mut store_data = StoreData::new();
        store_data.build_default(0);
        manager.database.add(store_data);
        manager.is_moddified = true;

        assert_eq!(manager.delete_file(), RESULT_UNKNOWN);
        assert_eq!(manager.database.get_database_length(), 1);
        assert!(manager.is_updated());
    }

    #[test]
    fn delete_file_removes_backing_file_without_cleaning_memory_state() {
        let base =
            std::env::temp_dir().join(format!("ruzu_mii_db_delete_file_{}", std::process::id()));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let mut manager = DatabaseManager::new();
        assert_eq!(manager.mount_save_data(), RESULT_SUCCESS);

        let mut store_data = StoreData::new();
        store_data.build_default(0);
        manager.database.add(store_data);

        assert_eq!(manager.save_database(), RESULT_SUCCESS);

        let db_path = base.join("system/save/8000000000000030").join(DB_FILE_NAME);
        assert!(db_path.exists());

        assert_eq!(manager.delete_file(), RESULT_SUCCESS);
        assert!(!db_path.exists());
        assert_eq!(manager.database.get_database_length(), 1);
        assert!(!manager.is_updated());

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn append_rejects_invalid_char_info() {
        let mut manager = DatabaseManager::new();
        assert_eq!(manager.mount_save_data(), RESULT_SUCCESS);

        let mut metadata = DatabaseSessionMetadata::default();
        let mut is_database_broken = false;
        assert_eq!(
            manager.initialize(&mut metadata, &mut is_database_broken),
            RESULT_SUCCESS
        );

        let char_info = CharInfo::default();
        assert_eq!(
            manager.append(&mut metadata, &char_info),
            RESULT_INVALID_CHAR_INFO2
        );
    }

    #[test]
    fn append_builds_unique_store_data_and_updates_metadata() {
        let mut manager = DatabaseManager::new();
        assert_eq!(manager.mount_save_data(), RESULT_SUCCESS);

        let mut metadata = DatabaseSessionMetadata::default();
        let mut is_database_broken = false;
        assert_eq!(
            manager.initialize(&mut metadata, &mut is_database_broken),
            RESULT_SUCCESS
        );

        let mut source = StoreData::new();
        source.build_default(0);
        let mut char_info = CharInfo::default();
        char_info.set_from_store_data(&source);
        char_info.type_val = 0;

        let before = metadata.update_counter;
        assert_eq!(manager.append(&mut metadata, &char_info), RESULT_SUCCESS);
        assert_eq!(manager.get_count(&metadata), 1);
        assert!(metadata.update_counter > before);
    }
}
