// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/mii_manager.h
//! Port of zuyu/src/core/hle/service/mii/mii_manager.cpp
//!
//! MiiManager: high-level Mii management, building random/default Miis,
//! and coordinating between the database and the Mii types.

use super::mii_database::MII_MAGIC;
use super::mii_database_manager::DatabaseManager;
use super::mii_result::{
    RESULT_INVALID_ARGUMENT_SIZE, RESULT_INVALID_CHAR_INFO, RESULT_NOT_FOUND, RESULT_NOT_UPDATED,
};
use super::mii_types::{
    Age, DatabaseSessionMetadata, Gender, Race, Source, SourceFlag, DEFAULT_MII_COUNT,
};
use super::types::char_info::CharInfo;
use super::types::core_data::CoreData;
use super::types::store_data::{StoreData, StoreDataElement};
use super::types::ver3_store_data::Ver3StoreData;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::mii::mii_util;

/// MiiManager coordinates Mii operations.
pub struct MiiManager {
    database_manager: DatabaseManager,
    is_broken_with_clear_flag: bool,
}

impl MiiManager {
    pub fn new() -> Self {
        Self {
            database_manager: DatabaseManager::new(),
            is_broken_with_clear_flag: false,
        }
    }

    /// Initialize the Mii manager and its database.
    pub fn initialize(&mut self, metadata: &mut DatabaseSessionMetadata) -> ResultCode {
        let _ = self.database_manager.mount_save_data();
        let _ = self
            .database_manager
            .initialize(metadata, &mut self.is_broken_with_clear_flag);
        RESULT_SUCCESS
    }

    pub fn set_test_db(&mut self, is_test_db: bool) {
        self.database_manager.set_test_db(is_test_db);
    }

    pub fn set_interface_version(&self, metadata: &mut DatabaseSessionMetadata, version: u32) {
        metadata.interface_version = version;
    }

    /// Check if the database has been updated.
    pub fn is_updated(&self, metadata: &mut DatabaseSessionMetadata, source_flag: u32) -> bool {
        if (source_flag & SourceFlag::Database as u32) == 0 {
            return false;
        }

        let metadata_update_counter = metadata.update_counter;
        let database_update_counter = self.database_manager.get_update_counter();
        metadata.update_counter = database_update_counter;
        metadata_update_counter != database_update_counter
    }

    /// Check if the database is full.
    pub fn is_full_database(&self) -> bool {
        self.database_manager.is_full_database()
    }

    /// Get the count of Mii entries.
    pub fn get_count(&self, metadata: &DatabaseSessionMetadata, source_flag: u32) -> u32 {
        let mut mii_count = 0u32;
        if (source_flag & SourceFlag::Default as u32) != 0 {
            mii_count += DEFAULT_MII_COUNT as u32;
        }
        if (source_flag & SourceFlag::Database as u32) != 0 {
            mii_count += self.database_manager.get_count(metadata);
        }
        mii_count
    }

    pub fn get_char_info_elements(
        &self,
        metadata: &DatabaseSessionMetadata,
        source_flag: u32,
    ) -> Vec<(CharInfo, Source)> {
        let mut out = Vec::new();

        if (source_flag & SourceFlag::Database as u32) != 0 {
            let mii_count = self.database_manager.get_count(metadata) as usize;
            for index in 0..mii_count {
                if let Some(store_data) = self.database_manager.get(metadata, index) {
                    let mut char_info = CharInfo::default();
                    char_info.set_from_store_data(&store_data);
                    out.push((char_info, Source::Database));
                }
            }
        }

        if (source_flag & SourceFlag::Default as u32) != 0 {
            for index in 0..DEFAULT_MII_COUNT {
                out.push((self.build_default(index as u32), Source::Default));
            }
        }

        out
    }

    pub fn get_char_info_elements_limited(
        &self,
        metadata: &DatabaseSessionMetadata,
        max_count: usize,
        source_flag: u32,
    ) -> Result<Vec<(CharInfo, Source)>, ResultCode> {
        let mut out = Vec::new();
        let result = self.fill_char_info_elements(metadata, &mut out, max_count, source_flag);
        if result.is_error() {
            return Err(result);
        }
        Ok(out)
    }

    pub fn get_char_infos(
        &self,
        metadata: &DatabaseSessionMetadata,
        source_flag: u32,
    ) -> Vec<CharInfo> {
        self.get_char_info_elements(metadata, source_flag)
            .into_iter()
            .map(|(char_info, _)| char_info)
            .collect()
    }

    pub fn get_char_infos_limited(
        &self,
        metadata: &DatabaseSessionMetadata,
        max_count: usize,
        source_flag: u32,
    ) -> Result<Vec<CharInfo>, ResultCode> {
        let mut out = Vec::new();
        let result = self.fill_char_infos(metadata, &mut out, max_count, source_flag);
        if result.is_error() {
            return Err(result);
        }
        Ok(out)
    }

    pub fn get_store_data_elements(
        &self,
        metadata: &DatabaseSessionMetadata,
        source_flag: u32,
    ) -> Vec<StoreDataElement> {
        let mut out = Vec::new();

        if (source_flag & SourceFlag::Database as u32) != 0 {
            let mii_count = self.database_manager.get_count(metadata) as usize;
            for index in 0..mii_count {
                if let Some(store_data) = self.database_manager.get(metadata, index) {
                    out.push(StoreDataElement {
                        store_data,
                        source: Source::Database,
                    });
                }
            }
        }

        self.build_default_store_data_elements(&mut out, source_flag);

        out
    }

    pub fn get_store_data_elements_limited(
        &self,
        metadata: &DatabaseSessionMetadata,
        max_count: usize,
        source_flag: u32,
    ) -> Result<Vec<StoreDataElement>, ResultCode> {
        let mut out = Vec::new();
        let result = self.fill_store_data_elements(metadata, &mut out, max_count, source_flag);
        if result.is_error() {
            return Err(result);
        }
        Ok(out)
    }

    pub fn get_store_datas(
        &self,
        metadata: &DatabaseSessionMetadata,
        source_flag: u32,
    ) -> Vec<StoreData> {
        let mut out = Vec::new();

        if (source_flag & SourceFlag::Database as u32) != 0 {
            let mii_count = self.database_manager.get_count(metadata) as usize;
            for index in 0..mii_count {
                if let Some(store_data) = self.database_manager.get(metadata, index) {
                    out.push(store_data);
                }
            }
        }

        self.build_default_store_datas(&mut out, source_flag);

        out
    }

    pub fn get_store_datas_limited(
        &self,
        metadata: &DatabaseSessionMetadata,
        max_count: usize,
        source_flag: u32,
    ) -> Result<Vec<StoreData>, ResultCode> {
        let mut out = Vec::new();
        let result = self.fill_store_datas(metadata, &mut out, max_count, source_flag);
        if result.is_error() {
            return Err(result);
        }
        Ok(out)
    }

    pub fn update_latest(
        &self,
        metadata: &DatabaseSessionMetadata,
        char_info: &CharInfo,
        source_flag: u32,
    ) -> Result<CharInfo, ResultCode> {
        if (source_flag & SourceFlag::Database as u32) == 0 {
            return Err(RESULT_NOT_FOUND);
        }

        if metadata.is_interface_version_supported(1) {
            if char_info.verify() != super::mii_types::ValidationResult::NoErrors {
                return Err(RESULT_INVALID_CHAR_INFO);
            }
        }

        let index = self
            .database_manager
            .find_index(metadata, char_info.create_id)?;
        let Some(store_data) = self.database_manager.get(metadata, index as usize) else {
            return Err(RESULT_NOT_FOUND);
        };

        if store_data.get_type() != char_info.type_val {
            return Err(RESULT_NOT_FOUND);
        }

        let mut out_char_info = CharInfo::default();
        out_char_info.set_from_store_data(&store_data);
        if out_char_info == *char_info {
            return Err(RESULT_NOT_UPDATED);
        }
        Ok(out_char_info)
    }

    pub fn update_latest_store_data(
        &self,
        metadata: &DatabaseSessionMetadata,
        store_data: &StoreData,
        source_flag: u32,
    ) -> Result<StoreData, ResultCode> {
        if (source_flag & SourceFlag::Database as u32) == 0 {
            return Err(RESULT_NOT_FOUND);
        }

        if metadata.is_interface_version_supported(1) {
            if store_data.is_valid() != super::mii_types::ValidationResult::NoErrors {
                return Err(RESULT_INVALID_CHAR_INFO);
            }
        }

        let index = self
            .database_manager
            .find_index(metadata, store_data.get_create_id())?;
        let Some(out_store_data) = self.database_manager.get(metadata, index as usize) else {
            return Err(RESULT_NOT_FOUND);
        };

        if out_store_data.get_type() != store_data.get_type() {
            return Err(RESULT_NOT_FOUND);
        }

        if out_store_data == *store_data {
            return Err(RESULT_NOT_UPDATED);
        }

        Ok(out_store_data)
    }

    /// Build a random Mii based on age, gender, and race parameters.
    ///
    /// Upstream creates a StoreData, calls store_data.BuildRandom(age, gender, race),
    /// then sets out_char_info from the store data via CharInfo::SetFromStoreData.
    pub fn build_random(&self, age: Age, gender: Gender, race: Race) -> CharInfo {
        let mut store_data = StoreData::new();
        store_data.build_random(age, gender, race);
        let mut char_info = CharInfo::default();
        char_info.set_from_store_data(&store_data);
        char_info
    }

    /// Build a default Mii with index.
    ///
    /// Upstream creates a StoreData, calls store_data.BuildDefault(index).
    pub fn build_default(&self, index: u32) -> CharInfo {
        let mut store_data = StoreData::new();
        store_data.build_default(index);
        let mut char_info = CharInfo::default();
        char_info.set_from_store_data(&store_data);
        char_info
    }

    pub fn build_base(&self, gender: Gender) -> CharInfo {
        let mut store_data = StoreData::new();
        store_data.build_base(gender);
        let mut char_info = CharInfo::default();
        char_info.set_from_store_data(&store_data);
        char_info
    }

    pub fn convert_core_data_to_char_info(
        &self,
        core_data: &CoreData,
    ) -> Result<CharInfo, ResultCode> {
        if core_data.is_valid() != super::mii_types::ValidationResult::NoErrors {
            return Err(RESULT_INVALID_CHAR_INFO);
        }

        let mut store_data = StoreData::new();
        store_data.build_with_core_data(*core_data);
        let name = store_data.get_nickname();
        if !mii_util::is_font_region_valid(store_data.get_font_region(), &name.data) {
            store_data.set_invalid_name();
        }

        let mut out_char_info = CharInfo::default();
        out_char_info.set_from_store_data(&store_data);
        Ok(out_char_info)
    }

    pub fn convert_v3_to_char_info(&self, mii_v3: &Ver3StoreData) -> Result<CharInfo, ResultCode> {
        if !mii_v3.is_valid() {
            return Err(RESULT_INVALID_CHAR_INFO);
        }

        let mut store_data = StoreData::new();
        mii_v3.build_to_store_data(&mut store_data);
        let name = store_data.get_nickname();
        if !mii_util::is_font_region_valid(store_data.get_font_region(), &name.data) {
            store_data.set_invalid_name();
        }

        let mut out_char_info = CharInfo::default();
        out_char_info.set_from_store_data(&store_data);
        Ok(out_char_info)
    }

    pub fn convert_char_info_to_core_data(
        &self,
        char_info: &CharInfo,
    ) -> Result<CoreData, ResultCode> {
        if char_info.verify() != super::mii_types::ValidationResult::NoErrors {
            return Err(RESULT_INVALID_CHAR_INFO);
        }

        let mut out_core_data = CoreData::default();
        out_core_data.build_from_char_info(char_info);
        let name = out_core_data.get_nickname();
        if !mii_util::is_font_region_valid(out_core_data.get_font_region(), &name.data) {
            out_core_data.set_nickname(out_core_data.get_invalid_nickname());
        }

        Ok(out_core_data)
    }

    fn build_default_store_data_elements(&self, out: &mut Vec<StoreDataElement>, source_flag: u32) {
        if (source_flag & SourceFlag::Default as u32) == 0 {
            return;
        }

        for index in 0..DEFAULT_MII_COUNT {
            let mut store_data = StoreData::new();
            store_data.build_default(index as u32);
            out.push(StoreDataElement {
                store_data,
                source: Source::Default,
            });
        }
    }

    fn build_default_store_datas(&self, out: &mut Vec<StoreData>, source_flag: u32) {
        if (source_flag & SourceFlag::Default as u32) == 0 {
            return;
        }

        for index in 0..DEFAULT_MII_COUNT {
            let mut store_data = StoreData::new();
            store_data.build_default(index as u32);
            out.push(store_data);
        }
    }

    fn fill_char_info_elements(
        &self,
        metadata: &DatabaseSessionMetadata,
        out: &mut Vec<(CharInfo, Source)>,
        max_count: usize,
        source_flag: u32,
    ) -> ResultCode {
        if (source_flag & SourceFlag::Database as u32) == 0 {
            return self.build_default_char_info_elements(out, max_count, source_flag);
        }

        let mii_count = self.database_manager.get_count(metadata) as usize;
        for index in 0..mii_count {
            if out.len() >= max_count {
                return RESULT_INVALID_ARGUMENT_SIZE;
            }
            if let Some(store_data) = self.database_manager.get(metadata, index) {
                let mut char_info = CharInfo::default();
                char_info.set_from_store_data(&store_data);
                out.push((char_info, Source::Database));
            }
        }

        self.build_default_char_info_elements(out, max_count, source_flag)
    }

    fn fill_char_infos(
        &self,
        metadata: &DatabaseSessionMetadata,
        out: &mut Vec<CharInfo>,
        max_count: usize,
        source_flag: u32,
    ) -> ResultCode {
        if (source_flag & SourceFlag::Database as u32) == 0 {
            return self.build_default_char_infos(out, max_count, source_flag);
        }

        let mii_count = self.database_manager.get_count(metadata) as usize;
        for index in 0..mii_count {
            if out.len() >= max_count {
                return RESULT_INVALID_ARGUMENT_SIZE;
            }
            if let Some(store_data) = self.database_manager.get(metadata, index) {
                let mut char_info = CharInfo::default();
                char_info.set_from_store_data(&store_data);
                out.push(char_info);
            }
        }

        self.build_default_char_infos(out, max_count, source_flag)
    }

    fn fill_store_data_elements(
        &self,
        metadata: &DatabaseSessionMetadata,
        out: &mut Vec<StoreDataElement>,
        max_count: usize,
        source_flag: u32,
    ) -> ResultCode {
        if (source_flag & SourceFlag::Database as u32) == 0 {
            return self.build_default_store_data_elements_limited(out, max_count, source_flag);
        }

        let mii_count = self.database_manager.get_count(metadata) as usize;
        for index in 0..mii_count {
            if out.len() >= max_count {
                return RESULT_INVALID_ARGUMENT_SIZE;
            }
            if let Some(store_data) = self.database_manager.get(metadata, index) {
                out.push(StoreDataElement {
                    store_data,
                    source: Source::Database,
                });
            }
        }

        self.build_default_store_data_elements_limited(out, max_count, source_flag)
    }

    fn fill_store_datas(
        &self,
        metadata: &DatabaseSessionMetadata,
        out: &mut Vec<StoreData>,
        max_count: usize,
        source_flag: u32,
    ) -> ResultCode {
        if (source_flag & SourceFlag::Database as u32) == 0 {
            return self.build_default_store_datas_limited(out, max_count, source_flag);
        }

        let mii_count = self.database_manager.get_count(metadata) as usize;
        for index in 0..mii_count {
            if out.len() >= max_count {
                return RESULT_INVALID_ARGUMENT_SIZE;
            }
            if let Some(store_data) = self.database_manager.get(metadata, index) {
                out.push(store_data);
            }
        }

        self.build_default_store_datas_limited(out, max_count, source_flag)
    }

    fn build_default_char_info_elements(
        &self,
        out: &mut Vec<(CharInfo, Source)>,
        max_count: usize,
        source_flag: u32,
    ) -> ResultCode {
        if (source_flag & SourceFlag::Default as u32) == 0 {
            return RESULT_SUCCESS;
        }

        for index in 0..DEFAULT_MII_COUNT {
            if out.len() >= max_count {
                return RESULT_INVALID_ARGUMENT_SIZE;
            }
            out.push((self.build_default(index as u32), Source::Default));
        }

        RESULT_SUCCESS
    }

    fn build_default_char_infos(
        &self,
        out: &mut Vec<CharInfo>,
        max_count: usize,
        source_flag: u32,
    ) -> ResultCode {
        if (source_flag & SourceFlag::Default as u32) == 0 {
            return RESULT_SUCCESS;
        }

        for index in 0..DEFAULT_MII_COUNT {
            if out.len() >= max_count {
                return RESULT_INVALID_ARGUMENT_SIZE;
            }
            out.push(self.build_default(index as u32));
        }

        RESULT_SUCCESS
    }

    fn build_default_store_data_elements_limited(
        &self,
        out: &mut Vec<StoreDataElement>,
        max_count: usize,
        source_flag: u32,
    ) -> ResultCode {
        if (source_flag & SourceFlag::Default as u32) == 0 {
            return RESULT_SUCCESS;
        }

        for index in 0..DEFAULT_MII_COUNT {
            if out.len() >= max_count {
                return RESULT_INVALID_ARGUMENT_SIZE;
            }
            let mut store_data = StoreData::new();
            store_data.build_default(index as u32);
            out.push(StoreDataElement {
                store_data,
                source: Source::Default,
            });
        }

        RESULT_SUCCESS
    }

    fn build_default_store_datas_limited(
        &self,
        out: &mut Vec<StoreData>,
        max_count: usize,
        source_flag: u32,
    ) -> ResultCode {
        if (source_flag & SourceFlag::Default as u32) == 0 {
            return RESULT_SUCCESS;
        }

        for index in 0..DEFAULT_MII_COUNT {
            if out.len() >= max_count {
                return RESULT_INVALID_ARGUMENT_SIZE;
            }
            let mut store_data = StoreData::new();
            store_data.build_default(index as u32);
            out.push(store_data);
        }

        RESULT_SUCCESS
    }

    /// Check if the database was broken and clear the flag like upstream.
    pub fn is_broken_with_clear_flag(&mut self, metadata: &mut DatabaseSessionMetadata) -> bool {
        let is_broken = self.is_broken_with_clear_flag;
        if self.is_broken_with_clear_flag {
            self.is_broken_with_clear_flag = false;
            self.database_manager.format(metadata);
            self.database_manager.save_database();
        }
        is_broken
    }

    /// Destroy the database file.
    pub fn destroy_file(&mut self, metadata: &mut DatabaseSessionMetadata) -> ResultCode {
        self.is_broken_with_clear_flag = true;
        self.database_manager.destroy_file(metadata)
    }

    /// Delete the database file.
    pub fn delete_file(&mut self) -> ResultCode {
        self.database_manager.delete_file()
    }

    /// Format (reset) the database.
    pub fn format(&mut self, metadata: &mut DatabaseSessionMetadata) -> ResultCode {
        self.database_manager.format(metadata);

        if !self.database_manager.is_updated() {
            return RESULT_NOT_UPDATED;
        }

        self.database_manager.save_database()
    }

    pub fn append(
        &mut self,
        metadata: &mut DatabaseSessionMetadata,
        char_info: &CharInfo,
    ) -> ResultCode {
        let result = self.database_manager.append(metadata, char_info);
        if result.is_error() {
            return RESULT_NOT_FOUND;
        }

        if !self.database_manager.is_updated() {
            return RESULT_NOT_UPDATED;
        }

        self.database_manager.save_database()
    }

    pub fn r#move(
        &mut self,
        metadata: &mut DatabaseSessionMetadata,
        index: u32,
        create_id: [u8; 16],
    ) -> ResultCode {
        let result = self.database_manager.r#move(metadata, index, create_id);

        if result.is_error() {
            return result;
        }

        if !self.database_manager.is_updated() {
            return RESULT_NOT_UPDATED;
        }

        self.database_manager.save_database()
    }

    pub fn add_or_replace(
        &mut self,
        metadata: &mut DatabaseSessionMetadata,
        store_data: StoreData,
    ) -> ResultCode {
        let result = self.database_manager.add_or_replace(metadata, store_data);

        if result.is_error() {
            return result;
        }

        if !self.database_manager.is_updated() {
            return RESULT_NOT_UPDATED;
        }

        self.database_manager.save_database()
    }

    pub fn delete(
        &mut self,
        metadata: &mut DatabaseSessionMetadata,
        create_id: [u8; 16],
    ) -> ResultCode {
        let result = self.database_manager.delete(metadata, create_id);

        if result.is_error() {
            return result;
        }

        if !self.database_manager.is_updated() {
            return RESULT_NOT_UPDATED;
        }

        self.database_manager.save_database()
    }

    pub fn find_index(&self, create_id: [u8; 16], is_special: bool) -> i32 {
        let mut index = 0;
        let result = self
            .database_manager
            .find_index_signed(&mut index, create_id, is_special);
        if result.is_error() {
            -1
        } else {
            index
        }
    }

    pub fn get_index(
        &self,
        metadata: &DatabaseSessionMetadata,
        char_info: &CharInfo,
    ) -> Result<i32, ResultCode> {
        if char_info.verify() != super::mii_types::ValidationResult::NoErrors {
            return Err(RESULT_INVALID_CHAR_INFO);
        }

        let is_special = metadata.magic == MII_MAGIC;
        let mut index = 0;
        let result =
            self.database_manager
                .find_index_signed(&mut index, char_info.create_id, is_special);
        if result.is_error() {
            index = -1;
        }

        if index == -1 {
            return Err(RESULT_NOT_FOUND);
        }

        Ok(index)
    }
}

impl Default for MiiManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::fs::path_util::{set_ruzu_path, RuzuPath};
    use std::fs;

    #[test]
    fn find_index_returns_minus_one_when_missing() {
        let manager = MiiManager::new();
        assert_eq!(manager.find_index([0; 16], false), -1);
    }

    #[test]
    fn get_index_rejects_invalid_char_info() {
        let manager = MiiManager::new();
        let metadata = DatabaseSessionMetadata::default();
        let char_info = CharInfo::default();

        assert_eq!(
            manager.get_index(&metadata, &char_info),
            Err(RESULT_INVALID_CHAR_INFO)
        );
    }

    #[test]
    fn initialize_returns_success_even_when_mount_fails() {
        let base = std::env::temp_dir().join(format!(
            "ruzu_mii_manager_mount_fail_{}",
            std::process::id()
        ));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        let nand_file = base.join("nand_file");
        fs::write(&nand_file, b"not a directory").unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &nand_file);

        let mut manager = MiiManager::new();
        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(manager.initialize(&mut metadata), RESULT_SUCCESS);
        assert_eq!(manager.get_count(&metadata, SourceFlag::Database as u32), 0);

        let _ = fs::remove_file(&nand_file);
        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn append_then_get_index_round_trips_database_index() {
        let base =
            std::env::temp_dir().join(format!("ruzu_mii_manager_get_index_{}", std::process::id()));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let mut manager = MiiManager::new();
        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(manager.initialize(&mut metadata), RESULT_SUCCESS);

        let mut store_data = StoreData::new();
        store_data.build_default(0);
        let mut char_info = CharInfo::default();
        char_info.set_from_store_data(&store_data);
        char_info.type_val = 0;

        assert_eq!(manager.append(&mut metadata, &char_info), RESULT_SUCCESS);
        assert_eq!(manager.find_index(char_info.create_id, false), 0);
        assert_eq!(manager.get_index(&metadata, &char_info), Ok(0));

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn move_updates_index_and_saves_database() {
        let base =
            std::env::temp_dir().join(format!("ruzu_mii_manager_move_{}", std::process::id()));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let mut manager = MiiManager::new();
        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(manager.initialize(&mut metadata), RESULT_SUCCESS);

        let mut a_store = StoreData::new();
        a_store.build_default(0);
        let mut a_info = CharInfo::default();
        a_info.set_from_store_data(&a_store);
        a_info.type_val = 0;

        let mut b_store = StoreData::new();
        b_store.build_default(1);
        let mut b_info = CharInfo::default();
        b_info.set_from_store_data(&b_store);
        b_info.type_val = 0;

        assert_eq!(manager.append(&mut metadata, &a_info), RESULT_SUCCESS);
        assert_eq!(manager.append(&mut metadata, &b_info), RESULT_SUCCESS);
        assert_eq!(manager.find_index(a_info.create_id, false), 0);
        assert_eq!(manager.find_index(b_info.create_id, false), 1);

        assert_eq!(
            manager.r#move(&mut metadata, 0, b_info.create_id),
            RESULT_SUCCESS
        );
        assert_eq!(manager.find_index(b_info.create_id, false), 0);
        assert_eq!(manager.find_index(a_info.create_id, false), 1);

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn add_or_replace_round_trips_store_data() {
        let base = std::env::temp_dir().join(format!(
            "ruzu_mii_manager_add_or_replace_{}",
            std::process::id()
        ));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let mut manager = MiiManager::new();
        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(manager.initialize(&mut metadata), RESULT_SUCCESS);

        let mut store_data = StoreData::new();
        store_data.build_default(0);
        store_data.core_data.set_type(0);
        assert_eq!(
            manager.add_or_replace(&mut metadata, store_data),
            RESULT_SUCCESS
        );
        assert_eq!(manager.find_index(store_data.get_create_id(), false), 0);

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn delete_removes_existing_database_entry() {
        let base =
            std::env::temp_dir().join(format!("ruzu_mii_manager_delete_{}", std::process::id()));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let mut manager = MiiManager::new();
        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(manager.initialize(&mut metadata), RESULT_SUCCESS);

        let mut store_data = StoreData::new();
        store_data.build_default(0);
        let mut char_info = CharInfo::default();
        char_info.set_from_store_data(&store_data);
        char_info.type_val = 0;

        assert_eq!(manager.append(&mut metadata, &char_info), RESULT_SUCCESS);
        assert_eq!(manager.find_index(char_info.create_id, false), 0);
        assert_eq!(
            manager.delete(&mut metadata, char_info.create_id),
            RESULT_SUCCESS
        );
        assert_eq!(manager.find_index(char_info.create_id, false), -1);

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn get_store_data_elements_appends_default_entries_after_database_entries() {
        let base = std::env::temp_dir().join(format!(
            "ruzu_mii_manager_store_elements_{}",
            std::process::id()
        ));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let mut manager = MiiManager::new();
        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(manager.initialize(&mut metadata), RESULT_SUCCESS);

        let mut store_data = StoreData::new();
        store_data.build_default(0);
        let mut char_info = CharInfo::default();
        char_info.set_from_store_data(&store_data);
        char_info.type_val = 0;
        assert_eq!(manager.append(&mut metadata, &char_info), RESULT_SUCCESS);

        let elements = manager.get_store_data_elements(
            &metadata,
            (SourceFlag::Database as u32) | (SourceFlag::Default as u32),
        );
        assert_eq!(elements.len(), 1 + DEFAULT_MII_COUNT);
        assert_eq!(elements[0].source, Source::Database);
        assert_eq!(elements[1].source, Source::Default);

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn update_latest_store_data_returns_newer_database_entry() {
        let base = std::env::temp_dir().join(format!(
            "ruzu_mii_manager_update_latest_store_{}",
            std::process::id()
        ));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let mut manager = MiiManager::new();
        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(manager.initialize(&mut metadata), RESULT_SUCCESS);
        manager.set_interface_version(&mut metadata, 1);

        let mut old_store = StoreData::new();
        old_store.build_default(0);
        old_store.core_data.set_type(0);
        assert_eq!(
            manager.add_or_replace(&mut metadata, old_store),
            RESULT_SUCCESS
        );

        let mut new_store = old_store;
        new_store
            .core_data
            .set_height(new_store.core_data.get_height().saturating_add(1));
        new_store.set_checksum();
        assert_eq!(
            manager.add_or_replace(&mut metadata, new_store),
            RESULT_SUCCESS
        );

        let latest = manager
            .update_latest_store_data(&metadata, &old_store, SourceFlag::Database as u32)
            .unwrap();
        assert_eq!(latest.get_create_id(), old_store.get_create_id());
        assert_ne!(latest.get_height(), old_store.get_height());

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn get_store_datas_appends_default_entries_after_database_entries() {
        let base = std::env::temp_dir().join(format!(
            "ruzu_mii_manager_store_datas_{}",
            std::process::id()
        ));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let mut manager = MiiManager::new();
        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(manager.initialize(&mut metadata), RESULT_SUCCESS);

        let mut store_data = StoreData::new();
        store_data.build_default(0);
        store_data.core_data.set_type(0);
        assert_eq!(
            manager.add_or_replace(&mut metadata, store_data),
            RESULT_SUCCESS
        );

        let out = manager.get_store_datas(
            &metadata,
            (SourceFlag::Database as u32) | (SourceFlag::Default as u32),
        );
        assert_eq!(out.len(), 1 + DEFAULT_MII_COUNT);
        assert_eq!(out[0].get_create_id(), store_data.get_create_id());
        assert_ne!(out[1].get_create_id(), store_data.get_create_id());

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn get_store_datas_limited_returns_invalid_argument_size_when_buffer_is_too_small() {
        let base = std::env::temp_dir().join(format!(
            "ruzu_mii_manager_store_datas_limited_{}",
            std::process::id()
        ));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let mut manager = MiiManager::new();
        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(manager.initialize(&mut metadata), RESULT_SUCCESS);

        let mut store_data = StoreData::new();
        store_data.build_default(0);
        store_data.core_data.set_type(0);
        assert_eq!(
            manager.add_or_replace(&mut metadata, store_data),
            RESULT_SUCCESS
        );

        let result = manager.get_store_datas_limited(
            &metadata,
            1,
            (SourceFlag::Database as u32) | (SourceFlag::Default as u32),
        );
        assert_eq!(result, Err(RESULT_INVALID_ARGUMENT_SIZE));

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn is_broken_with_clear_flag_formats_and_clears_flag() {
        let base = std::env::temp_dir().join(format!(
            "ruzu_mii_manager_broken_clear_flag_{}",
            std::process::id()
        ));
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        set_ruzu_path(RuzuPath::NANDDir, &base);

        let mut manager = MiiManager::new();
        let mut metadata = DatabaseSessionMetadata::default();
        assert_eq!(manager.initialize(&mut metadata), RESULT_SUCCESS);

        let mut store_data = StoreData::new();
        store_data.build_default(0);
        let mut char_info = CharInfo::default();
        char_info.set_from_store_data(&store_data);
        char_info.type_val = 0;
        assert_eq!(manager.append(&mut metadata, &char_info), RESULT_SUCCESS);
        assert_eq!(manager.get_count(&metadata, SourceFlag::Database as u32), 1);

        assert_eq!(manager.destroy_file(&mut metadata), RESULT_SUCCESS);
        assert!(manager.is_broken_with_clear_flag(&mut metadata));
        assert_eq!(manager.get_count(&metadata, SourceFlag::Database as u32), 0);
        assert!(!manager.is_broken_with_clear_flag(&mut metadata));

        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn convert_core_data_to_char_info_rejects_invalid_core_data() {
        let manager = MiiManager::new();
        let core_data = CoreData::default();

        assert_eq!(
            manager.convert_core_data_to_char_info(&core_data),
            Err(RESULT_INVALID_CHAR_INFO)
        );
    }

    #[test]
    fn convert_char_info_to_core_data_rejects_invalid_char_info() {
        let manager = MiiManager::new();
        let char_info = CharInfo::default();

        assert_eq!(
            manager
                .convert_char_info_to_core_data(&char_info)
                .unwrap_err(),
            RESULT_INVALID_CHAR_INFO
        );
    }

    #[test]
    fn convert_core_data_to_char_info_round_trips_valid_default_data() {
        let manager = MiiManager::new();
        let mut store_data = StoreData::new();
        store_data.build_default(0);
        let core_data = store_data.core_data;

        let char_info = manager
            .convert_core_data_to_char_info(&core_data)
            .expect("default core data should convert");

        assert_eq!(
            char_info.verify(),
            super::super::mii_types::ValidationResult::NoErrors
        );
        assert_eq!(char_info.name.data, core_data.get_nickname().data);
    }

    #[test]
    fn convert_char_info_to_core_data_round_trips_valid_default_char_info() {
        let manager = MiiManager::new();
        let char_info = manager.build_default(0);

        let core_data = manager
            .convert_char_info_to_core_data(&char_info)
            .expect("default char info should convert");

        assert_eq!(
            core_data.is_valid(),
            super::super::mii_types::ValidationResult::NoErrors
        );
        assert_eq!(core_data.get_nickname().data, char_info.name.data);
    }

    #[test]
    fn convert_v3_to_char_info_rejects_invalid_ver3_data() {
        let manager = MiiManager::new();
        let ver3 = Ver3StoreData::default();

        assert_eq!(
            manager.convert_v3_to_char_info(&ver3),
            Err(RESULT_INVALID_CHAR_INFO)
        );
    }
}
