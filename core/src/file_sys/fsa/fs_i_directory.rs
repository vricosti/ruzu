// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fsa/fs_i_directory.h

use crate::file_sys::fs_directory::DirectoryEntry;
use crate::file_sys::fs_filesystem::{DirectoryEntryType, OpenDirectoryMode};
use crate::file_sys::savedata_factory;
use crate::file_sys::vfs::vfs_types::VirtualDir;
use common::ResultCode;

/// Directory abstraction for filesystem access.
/// Corresponds to upstream `FileSys::Fsa::IDirectory`.
pub struct IDirectory {
    backend: VirtualDir,
    entries: Vec<DirectoryEntry>,
    next_entry_index: usize,
}

impl IDirectory {
    pub fn new(backend: VirtualDir, mode: OpenDirectoryMode) -> Self {
        let mut dir = Self {
            backend,
            entries: Vec::new(),
            next_entry_index: 0,
        };

        if mode.contains(OpenDirectoryMode::DIRECTORY) {
            dir.build_entry_index_dirs();
        }
        if mode.contains(OpenDirectoryMode::FILE) {
            dir.build_entry_index_files();
        }

        dir
    }

    pub fn read(&mut self, max_entries: i64) -> Result<Vec<DirectoryEntry>, ResultCode> {
        if max_entries == 0 {
            return Ok(Vec::new());
        }
        if max_entries < 0 {
            return Err(crate::file_sys::errors::RESULT_OUT_OF_RANGE);
        }

        let actual = std::cmp::min(
            max_entries as usize,
            self.entries.len() - self.next_entry_index,
        );
        let result: Vec<DirectoryEntry> =
            self.entries[self.next_entry_index..self.next_entry_index + actual].to_vec();
        self.next_entry_index += actual;
        Ok(result)
    }

    pub fn get_entry_count(&self) -> i64 {
        (self.entries.len() - self.next_entry_index) as i64
    }

    fn build_entry_index_dirs(&mut self) {
        let subdirs = self.backend.get_subdirectories();
        for subdir in &subdirs {
            let name = subdir.get_name();
            self.entries.push(DirectoryEntry::new(
                &name,
                DirectoryEntryType::Directory as i8,
                0,
            ));
        }
    }

    fn build_entry_index_files(&mut self) {
        let files = self.backend.get_files();
        for file in &files {
            let name = file.get_name();
            if name == savedata_factory::SAVE_DATA_SIZE_FILE_NAME {
                continue;
            }
            let size = file.get_size() as u64;
            self.entries.push(DirectoryEntry::new(
                &name,
                DirectoryEntryType::File as i8,
                size,
            ));
        }
    }
}
