// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fsa/fs_i_filesystem.h

use crate::file_sys::fs_filesystem::{
    CreateOption, DirectoryEntryType, OpenDirectoryMode, OpenMode,
};
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
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
/// This is a stub. The upstream implementation wraps a
/// `VfsDirectoryServiceWrapper`. Full implementation requires
/// the service wrapper port.
pub struct IFileSystem {
    // TODO: wrap VfsDirectoryServiceWrapper once service/filesystem is ported.
    _backend: Option<VirtualDir>,
}

impl IFileSystem {
    pub fn new(_backend: VirtualDir) -> Self {
        Self {
            _backend: Some(_backend),
        }
    }

    pub fn create_file(
        &self,
        _path: &str,
        _size: i64,
        _option: CreateOption,
    ) -> Result<(), ResultCode> {
        Err(crate::file_sys::errors::RESULT_NOT_IMPLEMENTED)
    }

    pub fn delete_file(&self, _path: &str) -> Result<(), ResultCode> {
        Err(crate::file_sys::errors::RESULT_NOT_IMPLEMENTED)
    }

    pub fn create_directory(&self, _path: &str) -> Result<(), ResultCode> {
        Err(crate::file_sys::errors::RESULT_NOT_IMPLEMENTED)
    }

    pub fn delete_directory(&self, _path: &str) -> Result<(), ResultCode> {
        Err(crate::file_sys::errors::RESULT_NOT_IMPLEMENTED)
    }

    pub fn get_entry_type(&self, _path: &str) -> Result<DirectoryEntryType, ResultCode> {
        Err(crate::file_sys::errors::RESULT_NOT_IMPLEMENTED)
    }

    pub fn open_file(
        &self,
        _path: &str,
        _mode: OpenMode,
    ) -> Result<VirtualFile, ResultCode> {
        Err(crate::file_sys::errors::RESULT_NOT_IMPLEMENTED)
    }

    pub fn open_directory(
        &self,
        _path: &str,
        _mode: OpenDirectoryMode,
    ) -> Result<VirtualDir, ResultCode> {
        Err(crate::file_sys::errors::RESULT_NOT_IMPLEMENTED)
    }

    pub fn commit(&self) -> Result<(), ResultCode> {
        Err(crate::file_sys::errors::RESULT_NOT_IMPLEMENTED)
    }

    pub fn get_free_space_size(&self, _path: &str) -> Result<i64, ResultCode> {
        Err(crate::file_sys::errors::RESULT_NOT_IMPLEMENTED)
    }

    pub fn get_total_space_size(&self, _path: &str) -> Result<i64, ResultCode> {
        Err(crate::file_sys::errors::RESULT_NOT_IMPLEMENTED)
    }
}
