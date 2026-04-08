// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fsa/fs_i_filesystem.h
// Status: COMPLETE (structural parity; Do* methods delegate to backend service wrapper)
//
// Filesystem abstraction for filesystem access. The upstream implementation
// wraps a VfsDirectoryServiceWrapper backend and delegates all Do* methods
// to it. Public methods include upstream validation logic (size checks,
// mode checks, null checks).

use crate::file_sys::errors;
use crate::file_sys::fs_filesystem::{
    CreateOption, DirectoryEntryType, OpenDirectoryMode, OpenMode,
};
use crate::file_sys::vfs::vfs_types::FileTimeStampRaw;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use crate::hle::service::filesystem::filesystem::VfsDirectoryServiceWrapper;
use common::ResultCode;

/// Query ID for filesystem operations.
/// Corresponds to upstream `FileSys::Fsa::QueryId`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum QueryId {
    SetConcatenationFileAttribute = 0,
    UpdateMac = 1,
    IsSignedSystemPartitionOnSdCardValid = 2,
    QueryUnpreparedFileInformation = 3,
}

/// Filesystem abstraction for filesystem access.
/// Corresponds to upstream `FileSys::Fsa::IFileSystem`.
///
/// The upstream implementation wraps a `VfsDirectoryServiceWrapper` backend.
/// Full delegation requires the service wrapper port; for now, Do* methods
/// that require it return RESULT_NOT_IMPLEMENTED.
/// Filesystem abstraction for filesystem access.
/// Corresponds to upstream `FileSys::Fsa::IFileSystem`.
///
/// The upstream implementation wraps a `VfsDirectoryServiceWrapper` backend
/// and delegates all Do* methods to it.
pub struct IFileSystem {
    backend: VfsDirectoryServiceWrapper,
}

impl IFileSystem {
    pub fn new(backing: VirtualDir) -> Self {
        Self {
            backend: VfsDirectoryServiceWrapper::new(backing),
        }
    }

    /// Create a file at the given path.
    ///
    /// Corresponds to upstream `IFileSystem::CreateFile`.
    pub fn create_file(
        &self,
        path: &str,
        size: i64,
        option: CreateOption,
    ) -> Result<(), ResultCode> {
        if size < 0 {
            return Err(errors::RESULT_OUT_OF_RANGE);
        }
        self.do_create_file(path, size, option as i32)
    }

    /// Delete a file at the given path.
    ///
    /// Corresponds to upstream `IFileSystem::DeleteFile`.
    pub fn delete_file(&self, path: &str) -> Result<(), ResultCode> {
        self.do_delete_file(path)
    }

    /// Create a directory at the given path.
    ///
    /// Corresponds to upstream `IFileSystem::CreateDirectory`.
    pub fn create_directory(&self, path: &str) -> Result<(), ResultCode> {
        self.do_create_directory(path)
    }

    /// Delete a directory at the given path.
    ///
    /// Corresponds to upstream `IFileSystem::DeleteDirectory`.
    pub fn delete_directory(&self, path: &str) -> Result<(), ResultCode> {
        self.do_delete_directory(path)
    }

    /// Delete a directory and all its contents recursively.
    ///
    /// Corresponds to upstream `IFileSystem::DeleteDirectoryRecursively`.
    pub fn delete_directory_recursively(&self, path: &str) -> Result<(), ResultCode> {
        self.do_delete_directory_recursively(path)
    }

    /// Rename a file.
    ///
    /// Corresponds to upstream `IFileSystem::RenameFile`.
    pub fn rename_file(&self, old_path: &str, new_path: &str) -> Result<(), ResultCode> {
        self.do_rename_file(old_path, new_path)
    }

    /// Rename a directory.
    ///
    /// Corresponds to upstream `IFileSystem::RenameDirectory`.
    pub fn rename_directory(&self, old_path: &str, new_path: &str) -> Result<(), ResultCode> {
        self.do_rename_directory(old_path, new_path)
    }

    /// Get the entry type at the given path.
    ///
    /// Corresponds to upstream `IFileSystem::GetEntryType`.
    pub fn get_entry_type(&self, path: &str) -> Result<DirectoryEntryType, ResultCode> {
        self.do_get_entry_type(path)
    }

    /// Open a file at the given path with the specified mode.
    ///
    /// Corresponds to upstream `IFileSystem::OpenFile`.
    pub fn open_file(&self, path: &str, mode: OpenMode) -> Result<VirtualFile, ResultCode> {
        // Validate mode: must have at least read or write.
        if !mode.intersects(OpenMode::READ_WRITE) {
            return Err(errors::RESULT_INVALID_OPEN_MODE);
        }
        // Must not have bits outside All.
        if mode.bits() & !OpenMode::ALL.bits() != 0 {
            return Err(errors::RESULT_INVALID_OPEN_MODE);
        }
        self.do_open_file(path, mode)
    }

    /// Open a directory at the given path with the specified mode.
    ///
    /// Corresponds to upstream `IFileSystem::OpenDirectory`.
    pub fn open_directory(
        &self,
        path: &str,
        mode: OpenDirectoryMode,
    ) -> Result<VirtualDir, ResultCode> {
        // Must have at least one of the All bits.
        if !mode.intersects(OpenDirectoryMode::ALL) {
            return Err(errors::RESULT_INVALID_OPEN_MODE);
        }
        // Must not have bits outside All | NotRequireFileSize.
        let valid_bits =
            OpenDirectoryMode::ALL.bits() | OpenDirectoryMode::NOT_REQUIRE_FILE_SIZE.bits();
        if mode.bits() & !valid_bits != 0 {
            return Err(errors::RESULT_INVALID_OPEN_MODE);
        }
        self.do_open_directory(path, mode)
    }

    /// Commit filesystem changes.
    ///
    /// Corresponds to upstream `IFileSystem::Commit`.
    pub fn commit(&self) -> Result<(), ResultCode> {
        self.do_commit()
    }

    /// Get the free space size at the given path.
    ///
    /// Corresponds to upstream `IFileSystem::GetFreeSpaceSize`.
    pub fn get_free_space_size(&self, path: &str) -> Result<i64, ResultCode> {
        self.do_get_free_space_size(path)
    }

    /// Get the total space size at the given path.
    ///
    /// Corresponds to upstream `IFileSystem::GetTotalSpaceSize`.
    pub fn get_total_space_size(&self, path: &str) -> Result<i64, ResultCode> {
        self.do_get_total_space_size(path)
    }

    /// Clean a directory recursively (remove contents but keep the directory).
    ///
    /// Corresponds to upstream `IFileSystem::CleanDirectoryRecursively`.
    pub fn clean_directory_recursively(&self, path: &str) -> Result<(), ResultCode> {
        self.do_clean_directory_recursively(path)
    }

    /// Query entry metadata.
    ///
    /// Corresponds to upstream `IFileSystem::QueryEntry`.
    pub fn query_entry(&self, query: QueryId, path: &str) -> Result<Vec<u8>, ResultCode> {
        self.do_query_entry(query, path)
    }

    /// Get a raw file timestamp.
    ///
    /// Corresponds to upstream `IFileSystem::GetFileTimeStampRaw`.
    pub fn get_file_time_stamp_raw(&self, path: &str) -> Result<FileTimeStampRaw, ResultCode> {
        self.do_get_file_time_stamp_raw(path)
    }

    /// Commit provisionally (not accessible as a command).
    ///
    /// Corresponds to upstream `IFileSystem::CommitProvisionally`.
    pub fn commit_provisionally(&self, _counter: i64) -> Result<(), ResultCode> {
        Err(errors::RESULT_NOT_IMPLEMENTED)
    }

    /// Rollback (not accessible as a command).
    ///
    /// Corresponds to upstream `IFileSystem::Rollback`.
    pub fn rollback(&self) -> Result<(), ResultCode> {
        Err(errors::RESULT_NOT_IMPLEMENTED)
    }

    /// Flush (not accessible as a command).
    ///
    /// Corresponds to upstream `IFileSystem::Flush`.
    pub fn flush(&self) -> Result<(), ResultCode> {
        Err(errors::RESULT_NOT_IMPLEMENTED)
    }

    // -- Private Do* methods --
    // These correspond to the upstream private virtual Do* methods.
    // They delegate to the backend service wrapper when available.

    fn do_create_file(&self, path: &str, size: i64, _flags: i32) -> Result<(), ResultCode> {
        self.backend.create_file(path, size as u64)
    }

    fn do_delete_file(&self, path: &str) -> Result<(), ResultCode> {
        self.backend.delete_file(path)
    }

    fn do_create_directory(&self, path: &str) -> Result<(), ResultCode> {
        self.backend.create_directory(path)
    }

    fn do_delete_directory(&self, path: &str) -> Result<(), ResultCode> {
        self.backend.delete_directory(path)
    }

    fn do_delete_directory_recursively(&self, path: &str) -> Result<(), ResultCode> {
        self.backend.delete_directory_recursively(path)
    }

    fn do_rename_file(&self, old_path: &str, new_path: &str) -> Result<(), ResultCode> {
        self.backend.rename_file(old_path, new_path)
    }

    fn do_rename_directory(&self, old_path: &str, new_path: &str) -> Result<(), ResultCode> {
        self.backend.rename_directory(old_path, new_path)
    }

    fn do_get_entry_type(&self, path: &str) -> Result<DirectoryEntryType, ResultCode> {
        self.backend.get_entry_type(path)
    }

    fn do_open_file(&self, path: &str, mode: OpenMode) -> Result<VirtualFile, ResultCode> {
        self.backend.open_file(path, mode)
    }

    fn do_open_directory(
        &self,
        path: &str,
        _mode: OpenDirectoryMode,
    ) -> Result<VirtualDir, ResultCode> {
        self.backend.open_directory(path)
    }

    fn do_commit(&self) -> Result<(), ResultCode> {
        Err(errors::RESULT_NOT_IMPLEMENTED)
    }

    fn do_get_free_space_size(&self, _path: &str) -> Result<i64, ResultCode> {
        Err(errors::RESULT_NOT_IMPLEMENTED)
    }

    fn do_get_total_space_size(&self, _path: &str) -> Result<i64, ResultCode> {
        Err(errors::RESULT_NOT_IMPLEMENTED)
    }

    fn do_clean_directory_recursively(&self, path: &str) -> Result<(), ResultCode> {
        self.backend.clean_directory_recursively(path)
    }

    fn do_get_file_time_stamp_raw(&self, path: &str) -> Result<FileTimeStampRaw, ResultCode> {
        self.backend.get_file_time_stamp_raw(path)
    }

    fn do_query_entry(&self, _query: QueryId, _path: &str) -> Result<Vec<u8>, ResultCode> {
        Err(errors::RESULT_NOT_IMPLEMENTED)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs_vector::VectorVfsDirectory;
    use std::sync::Arc;

    fn make_empty_fs() -> IFileSystem {
        let dir: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "root".to_string(),
            None,
        ));
        IFileSystem::new(dir)
    }

    #[test]
    fn test_create_file_negative_size() {
        let fs = make_empty_fs();
        let result = fs.create_file("/test.txt", -1, CreateOption::None);
        assert!(result.is_err());
    }

    #[test]
    fn test_open_file_invalid_mode() {
        let fs = make_empty_fs();
        // Mode with no read or write bits.
        let result = fs.open_file("/test.txt", OpenMode::ALLOW_APPEND);
        assert!(result.is_err());
    }

    #[test]
    fn test_open_directory_invalid_mode() {
        let fs = make_empty_fs();
        // Mode with no directory or file bits.
        let result = fs.open_directory("/test", OpenDirectoryMode::NOT_REQUIRE_FILE_SIZE);
        assert!(result.is_err());
    }
}
