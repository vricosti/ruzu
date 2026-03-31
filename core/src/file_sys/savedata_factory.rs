// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/savedata_factory.h / .cpp
// Status: COMPLETE (structural parity; path building matches upstream)
//
// File system interface to the SaveData archive. Provides create, open, and
// size persistence for save data directories organized by space/type/user/title.

use super::fs_save_data_types::{
    SaveDataAttribute, SaveDataSize, SaveDataSpaceId, SaveDataType, UserId,
};
use super::vfs::vfs::{get_or_create_directory_relative, VfsDirectory, VfsFile};
use super::vfs::vfs_types::VirtualDir;

/// The hidden file name used to persist save data size.
/// Corresponds to upstream `GetSaveDataSizeFileName`.
pub const SAVE_DATA_SIZE_FILE_NAME: &str = ".yuzu_save_size";

pub type ProgramId = u64;

/// Check if save data should be automatically created.
///
/// Corresponds to upstream anonymous `ShouldSaveDataBeAutomaticallyCreated`.
fn should_save_data_be_automatically_created(
    space: SaveDataSpaceId,
    attr: &SaveDataAttribute,
) -> bool {
    attr.save_type == SaveDataType::Cache
        || attr.save_type == SaveDataType::Temporary
        || (space == SaveDataSpaceId::User
            && (attr.save_type == SaveDataType::Account || attr.save_type == SaveDataType::Device)
            && attr.program_id == 0
            && attr.system_save_data_id == 0)
}

/// File system interface to the SaveData archive.
/// Corresponds to upstream `SaveDataFactory`.
pub struct SaveDataFactory {
    program_id: ProgramId,
    dir: VirtualDir,
    auto_create: bool,
}

impl SaveDataFactory {
    /// Construct a new SaveDataFactory.
    ///
    /// Corresponds to upstream `SaveDataFactory::SaveDataFactory`.
    /// Deletes all temporary storages on construction, matching hardware behavior.
    pub fn new(program_id: ProgramId, save_directory: VirtualDir) -> Self {
        // Delete all temporary storages.
        // On hardware, it is expected that temporary storage be empty at first use.
        save_directory.delete_subdirectory_recursive("temp");

        Self {
            program_id,
            dir: save_directory,
            auto_create: true,
        }
    }

    /// Create a save data directory.
    ///
    /// Corresponds to upstream `SaveDataFactory::Create`.
    pub fn create(&self, space: SaveDataSpaceId, meta: &SaveDataAttribute) -> Option<VirtualDir> {
        let save_directory = Self::get_full_path(
            self.program_id,
            &self.dir,
            space,
            meta.save_type,
            meta.program_id,
            meta.user_id,
            meta.system_save_data_id,
        );
        self.dir.create_directory_relative(&save_directory)
    }

    /// Open an existing save data directory.
    ///
    /// Corresponds to upstream `SaveDataFactory::Open`.
    pub fn open(&self, space: SaveDataSpaceId, meta: &SaveDataAttribute) -> Option<VirtualDir> {
        let save_directory = Self::get_full_path(
            self.program_id,
            &self.dir,
            space,
            meta.save_type,
            meta.program_id,
            meta.user_id,
            meta.system_save_data_id,
        );

        let out = self.dir.get_directory_relative(&save_directory);

        if out.is_none()
            && should_save_data_be_automatically_created(space, meta)
            && self.auto_create
        {
            return self.create(space, meta);
        }

        out
    }

    /// Get the directory for a given save data space.
    ///
    /// Corresponds to upstream `SaveDataFactory::GetSaveDataSpaceDirectory`.
    pub fn get_save_data_space_directory(&self, space: SaveDataSpaceId) -> Option<VirtualDir> {
        self.dir
            .get_directory_relative(Self::get_save_data_space_id_path(space))
    }

    /// Get the path prefix for a save data space.
    ///
    /// Corresponds to upstream `SaveDataFactory::GetSaveDataSpaceIdPath`.
    pub fn get_save_data_space_id_path(space: SaveDataSpaceId) -> &'static str {
        match space {
            SaveDataSpaceId::System => "/system/",
            SaveDataSpaceId::User => "/user/",
            SaveDataSpaceId::Temporary => "/temp/",
            _ => {
                log::error!("Unrecognized SaveDataSpaceId: {:?}", space);
                "/unrecognized/"
            }
        }
    }

    /// Get the full path for a save data entry.
    ///
    /// Corresponds to upstream `SaveDataFactory::GetFullPath`.
    /// According to switchbrew, if a save is of type SaveData and the title id
    /// field is 0, it should be interpreted as the title id of the current process.
    pub fn get_full_path(
        program_id: ProgramId,
        dir: &VirtualDir,
        space: SaveDataSpaceId,
        save_type: SaveDataType,
        mut title_id: u64,
        user_id: UserId,
        save_id: u64,
    ) -> String {
        // If title_id is 0 for Account or Device saves, use the program_id.
        if (save_type == SaveDataType::Account || save_type == SaveDataType::Device)
            && title_id == 0
        {
            title_id = program_id;
        }

        // For compat with a future impl: check if the future path location exists.
        let future_path = get_future_save_data_path(space, save_type, title_id & !0xFF, user_id);
        if !future_path.is_empty() {
            if let Some(_future_dir) = dir.get_directory_relative(&future_path) {
                log::info!("Using save at new location: {}", future_path);
                return future_path;
            }
        }

        let out = Self::get_save_data_space_id_path(space);

        match save_type {
            SaveDataType::System => {
                format!(
                    "{}save/{:016X}/{:016X}{:016X}",
                    out, save_id, user_id[1], user_id[0]
                )
            }
            SaveDataType::Account | SaveDataType::Device => {
                format!(
                    "{}save/{:016X}/{:016X}{:016X}/{:016X}",
                    out, 0u64, user_id[1], user_id[0], title_id
                )
            }
            SaveDataType::Temporary => {
                format!(
                    "{}{:016X}/{:016X}{:016X}/{:016X}",
                    out, 0u64, user_id[1], user_id[0], title_id
                )
            }
            SaveDataType::Cache => {
                format!("{}save/cache/{:016X}", out, title_id)
            }
            _ => {
                log::error!("Unrecognized SaveDataType: {:?}", save_type);
                format!(
                    "{}save/unknown_{:X}/{:016X}",
                    out, save_type as u8, title_id
                )
            }
        }
    }

    /// Get the root path for user game save data.
    ///
    /// Corresponds to upstream `SaveDataFactory::GetUserGameSaveDataRoot`.
    pub fn get_user_game_save_data_root(user_id: UserId, future: bool) -> String {
        if future {
            // In upstream, this uses Common::UUID::RawString() which produces a
            // hex representation. We format the user_id bytes as hex.
            let uuid_bytes: [u8; 16] = {
                let mut buf = [0u8; 16];
                buf[..8].copy_from_slice(&user_id[0].to_le_bytes());
                buf[8..].copy_from_slice(&user_id[1].to_le_bytes());
                buf
            };
            let uuid_hex: String = uuid_bytes.iter().map(|b| format!("{:02x}", b)).collect();
            format!("/user/save/account/{}", uuid_hex)
        } else {
            format!(
                "/user/save/{:016X}/{:016X}{:016X}",
                0u64, user_id[1], user_id[0]
            )
        }
    }

    /// Read the persisted save data size.
    ///
    /// Corresponds to upstream `SaveDataFactory::ReadSaveDataSize`.
    pub fn read_save_data_size(
        &self,
        save_type: SaveDataType,
        title_id: u64,
        user_id: UserId,
    ) -> SaveDataSize {
        let path = Self::get_full_path(
            self.program_id,
            &self.dir,
            SaveDataSpaceId::User,
            save_type,
            title_id,
            user_id,
            0,
        );
        let relative_dir = match get_or_create_directory_relative(self.dir.as_ref(), &path) {
            Some(d) => d,
            None => return SaveDataSize::default(),
        };

        let size_file = match relative_dir.get_file(SAVE_DATA_SIZE_FILE_NAME) {
            Some(f) => f,
            None => return SaveDataSize::default(),
        };

        if size_file.get_size() < std::mem::size_of::<SaveDataSize>() {
            return SaveDataSize::default();
        }

        let mut buf = [0u8; std::mem::size_of::<SaveDataSize>()];
        let len = buf.len();
        let read = size_file.read(&mut buf, len, 0);
        if read != buf.len() {
            return SaveDataSize::default();
        }

        // Safety: SaveDataSize is repr(C) with only u64 fields.
        unsafe { std::ptr::read_unaligned(buf.as_ptr() as *const SaveDataSize) }
    }

    /// Write the persisted save data size.
    ///
    /// Corresponds to upstream `SaveDataFactory::WriteSaveDataSize`.
    pub fn write_save_data_size(
        &self,
        save_type: SaveDataType,
        title_id: u64,
        user_id: UserId,
        new_value: SaveDataSize,
    ) {
        let path = Self::get_full_path(
            self.program_id,
            &self.dir,
            SaveDataSpaceId::User,
            save_type,
            title_id,
            user_id,
            0,
        );
        let relative_dir = match get_or_create_directory_relative(self.dir.as_ref(), &path) {
            Some(d) => d,
            None => return,
        };

        let size_file = match relative_dir.create_file(SAVE_DATA_SIZE_FILE_NAME) {
            Some(f) => f,
            None => return,
        };

        size_file.resize(std::mem::size_of::<SaveDataSize>());

        // Safety: SaveDataSize is repr(C) with only u64 fields.
        let bytes = unsafe {
            std::slice::from_raw_parts(
                &new_value as *const SaveDataSize as *const u8,
                std::mem::size_of::<SaveDataSize>(),
            )
        };
        size_file.write(bytes, bytes.len(), 0);
    }

    /// Set the auto-create flag.
    ///
    /// Corresponds to upstream `SaveDataFactory::SetAutoCreate`.
    pub fn set_auto_create(&mut self, state: bool) {
        self.auto_create = state;
    }
}

/// Get a future-format save data path (for forward compatibility).
///
/// Corresponds to upstream anonymous `GetFutureSaveDataPath`.
fn get_future_save_data_path(
    space_id: SaveDataSpaceId,
    save_type: SaveDataType,
    title_id: u64,
    user_id: UserId,
) -> String {
    // Only detect nand user saves.
    let space_id_path = match space_id {
        SaveDataSpaceId::User => "/user/save",
        _ => return String::new(),
    };

    // Format UUID as hex string.
    let uuid_bytes: [u8; 16] = {
        let mut buf = [0u8; 16];
        buf[..8].copy_from_slice(&user_id[0].to_le_bytes());
        buf[8..].copy_from_slice(&user_id[1].to_le_bytes());
        buf
    };
    let uuid_hex: String = uuid_bytes.iter().map(|b| format!("{:02x}", b)).collect();

    // Only detect account/device saves from the future location.
    match save_type {
        SaveDataType::Account => {
            format!("{}/account/{}/{:016X}/0", space_id_path, uuid_hex, title_id)
        }
        SaveDataType::Device => {
            format!("{}/device/{:016X}/0", space_id_path, title_id)
        }
        _ => String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_save_data_space_id_path() {
        assert_eq!(
            SaveDataFactory::get_save_data_space_id_path(SaveDataSpaceId::System),
            "/system/"
        );
        assert_eq!(
            SaveDataFactory::get_save_data_space_id_path(SaveDataSpaceId::User),
            "/user/"
        );
        assert_eq!(
            SaveDataFactory::get_save_data_space_id_path(SaveDataSpaceId::Temporary),
            "/temp/"
        );
    }

    #[test]
    fn test_get_user_game_save_data_root_legacy() {
        let user_id: UserId = [0x0123456789ABCDEF, 0xFEDCBA9876543210];
        let result = SaveDataFactory::get_user_game_save_data_root(user_id, false);
        assert_eq!(
            result,
            "/user/save/0000000000000000/FEDCBA98765432100123456789ABCDEF"
        );
    }

    #[test]
    fn test_get_full_path_system() {
        use crate::file_sys::vfs::vfs_vector::VectorVfsDirectory;
        use std::sync::Arc;

        let dir: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "save".to_string(),
            None,
        ));

        let user_id: UserId = [0, 0];
        let result = SaveDataFactory::get_full_path(
            0x0100000000001000,
            &dir,
            SaveDataSpaceId::System,
            SaveDataType::System,
            0,
            user_id,
            0x8000000000000001,
        );
        assert!(result.starts_with("/system/save/"));
        assert!(result.contains("8000000000000001"));
    }

    #[test]
    fn test_get_full_path_cache() {
        use crate::file_sys::vfs::vfs_vector::VectorVfsDirectory;
        use std::sync::Arc;

        let dir: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "save".to_string(),
            None,
        ));

        let user_id: UserId = [0, 0];
        let result = SaveDataFactory::get_full_path(
            0x0100000000001000,
            &dir,
            SaveDataSpaceId::User,
            SaveDataType::Cache,
            0x0100000000001000,
            user_id,
            0,
        );
        assert_eq!(result, "/user/save/cache/0100000000001000");
    }

    #[test]
    fn test_get_full_path_account_uses_program_id_when_title_is_zero() {
        use crate::file_sys::vfs::vfs_vector::VectorVfsDirectory;
        use std::sync::Arc;

        let dir: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "save".to_string(),
            None,
        ));

        let user_id: UserId = [1, 0];
        let result = SaveDataFactory::get_full_path(
            0xABCD,
            &dir,
            SaveDataSpaceId::User,
            SaveDataType::Account,
            0, // title_id = 0, should use program_id
            user_id,
            0,
        );
        // Should contain the program_id 0xABCD instead of 0.
        assert!(result.contains("000000000000ABCD"));
    }
}
