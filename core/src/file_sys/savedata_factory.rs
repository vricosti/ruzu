// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/savedata_factory.h / .cpp

use super::fs_save_data_types::{SaveDataAttribute, SaveDataSize, SaveDataSpaceId, SaveDataType};
use super::vfs::vfs_types::VirtualDir;

/// The hidden file name used to persist save data size.
/// Corresponds to upstream `GetSaveDataSizeFileName`.
pub const SAVE_DATA_SIZE_FILE_NAME: &str = ".yuzu_save_size";

pub type ProgramId = u64;

/// File system interface to the SaveData archive.
/// Corresponds to upstream `SaveDataFactory`.
pub struct SaveDataFactory {
    program_id: ProgramId,
    dir: VirtualDir,
    auto_create: bool,
}

impl SaveDataFactory {
    pub fn new(program_id: ProgramId, save_directory: VirtualDir) -> Self {
        Self {
            program_id,
            dir: save_directory,
            auto_create: true,
        }
    }

    /// Create a save data directory. Stub: returns None.
    /// TODO: implement directory creation.
    pub fn create(
        &self,
        _space: SaveDataSpaceId,
        _meta: &SaveDataAttribute,
    ) -> Option<VirtualDir> {
        None
    }

    /// Open an existing save data directory. Stub: returns None.
    /// TODO: implement directory lookup.
    pub fn open(
        &self,
        _space: SaveDataSpaceId,
        _meta: &SaveDataAttribute,
    ) -> Option<VirtualDir> {
        None
    }

    pub fn get_save_data_space_directory(&self, _space: SaveDataSpaceId) -> Option<VirtualDir> {
        None
    }

    /// Get the path for a save data space.
    /// Corresponds to upstream `SaveDataFactory::GetSaveDataSpaceIdPath`.
    pub fn get_save_data_space_id_path(space: SaveDataSpaceId) -> &'static str {
        match space {
            SaveDataSpaceId::System => "/save/system",
            SaveDataSpaceId::User | SaveDataSpaceId::SdUser => "/save/user",
            SaveDataSpaceId::SdSystem => "/save/sd_system",
            SaveDataSpaceId::Temporary => "/save/temp",
            SaveDataSpaceId::ProperSystem => "/save/proper_system",
            SaveDataSpaceId::SafeMode => "/save/safe_mode",
        }
    }

    /// Get the full path for a save data.
    /// Corresponds to upstream `SaveDataFactory::GetFullPath`.
    pub fn get_full_path(
        _program_id: ProgramId,
        _space: SaveDataSpaceId,
        _save_type: SaveDataType,
        _title_id: u64,
        _user_id: u128,
        _save_id: u64,
    ) -> String {
        // TODO: implement path building matching upstream logic.
        String::new()
    }

    /// Get the root path for user game save data.
    /// Corresponds to upstream `SaveDataFactory::GetUserGameSaveDataRoot`.
    pub fn get_user_game_save_data_root(_user_id: u128, _future: bool) -> String {
        // TODO: implement.
        String::new()
    }

    pub fn read_save_data_size(
        &self,
        _save_type: SaveDataType,
        _title_id: u64,
        _user_id: u128,
    ) -> SaveDataSize {
        SaveDataSize::default()
    }

    pub fn write_save_data_size(
        &self,
        _save_type: SaveDataType,
        _title_id: u64,
        _user_id: u128,
        _new_value: SaveDataSize,
    ) {
        // TODO: implement.
    }

    pub fn set_auto_create(&mut self, state: bool) {
        self.auto_create = state;
    }
}
