// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/file_sys/vfs/vfs_cached.h and vfs_cached.cpp
//! CachedVfsDirectory: caches subdirectories and files from a source directory into BTreeMaps
//! for fast lookup by name.

use std::collections::BTreeMap;
use std::sync::Arc;

use super::vfs::VfsDirectory;
use super::vfs_types::{VirtualDir, VirtualFile};

/// A read-only directory that caches subdirectories and files from a source directory
/// into maps for efficient name-based lookup. Subdirectories are recursively cached.
///
/// Maps to upstream `CachedVfsDirectory`.
pub struct CachedVfsDirectory {
    name: String,
    parent: Option<VirtualDir>,
    dirs: BTreeMap<String, VirtualDir>,
    files: BTreeMap<String, VirtualFile>,
}

impl CachedVfsDirectory {
    /// Constructs a CachedVfsDirectory by reading the source directory's contents.
    /// Subdirectories are recursively wrapped in CachedVfsDirectory.
    ///
    /// Maps to upstream `CachedVfsDirectory::CachedVfsDirectory(VirtualDir&&)`.
    pub fn new(source_dir: VirtualDir) -> Self {
        let name = source_dir.get_name();
        let parent = source_dir.get_parent_directory();

        let mut dirs = BTreeMap::new();
        for dir in source_dir.get_subdirectories() {
            let dir_name = dir.get_name();
            let cached: VirtualDir = Arc::new(CachedVfsDirectory::new(dir));
            dirs.insert(dir_name, cached);
        }

        let mut files = BTreeMap::new();
        for file in source_dir.get_files() {
            let file_name = file.get_name();
            files.insert(file_name, file);
        }

        Self {
            name,
            parent,
            dirs,
            files,
        }
    }
}

impl VfsDirectory for CachedVfsDirectory {
    fn get_file(&self, name: &str) -> Option<VirtualFile> {
        self.files.get(name).cloned()
    }

    fn get_subdirectory(&self, name: &str) -> Option<VirtualDir> {
        self.dirs.get(name).cloned()
    }

    fn get_files(&self) -> Vec<VirtualFile> {
        self.files.values().cloned().collect()
    }

    fn get_subdirectories(&self) -> Vec<VirtualDir> {
        self.dirs.values().cloned().collect()
    }

    fn get_name(&self) -> String {
        self.name.clone()
    }

    fn get_parent_directory(&self) -> Option<VirtualDir> {
        self.parent.clone()
    }

    // Read-only: all write operations return false/None
    fn is_writable(&self) -> bool {
        false
    }

    fn is_readable(&self) -> bool {
        true
    }

    fn create_subdirectory(&self, _name: &str) -> Option<VirtualDir> {
        None
    }

    fn create_file(&self, _name: &str) -> Option<VirtualFile> {
        None
    }

    fn create_file_relative(&self, _path: &str) -> Option<VirtualFile> {
        None
    }

    fn create_file_absolute(&self, _path: &str) -> Option<VirtualFile> {
        None
    }

    fn create_directory_relative(&self, _path: &str) -> Option<VirtualDir> {
        None
    }

    fn create_directory_absolute(&self, _path: &str) -> Option<VirtualDir> {
        None
    }

    fn delete_subdirectory(&self, _name: &str) -> bool {
        false
    }

    fn delete_subdirectory_recursive(&self, _name: &str) -> bool {
        false
    }

    fn clean_subdirectory_recursive(&self, _name: &str) -> bool {
        false
    }

    fn delete_file(&self, _name: &str) -> bool {
        false
    }

    fn rename(&self, _name: &str) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs_vector::{VectorVfsDirectory, VectorVfsFile};

    #[test]
    fn test_cached_vfs_directory() {
        let f1: VirtualFile =
            Arc::new(VectorVfsFile::new(vec![1, 2, 3], "a.bin".to_string(), None));
        let f2: VirtualFile =
            Arc::new(VectorVfsFile::new(vec![4, 5], "b.bin".to_string(), None));

        let inner_dir: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "sub".to_string(),
            None,
        ));
        let source: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![f1, f2],
            vec![inner_dir],
            "root".to_string(),
            None,
        ));

        let cached = CachedVfsDirectory::new(source);

        assert_eq!(cached.get_name(), "root");
        assert!(cached.is_readable());
        assert!(!cached.is_writable());

        assert!(cached.get_file("a.bin").is_some());
        assert!(cached.get_file("b.bin").is_some());
        assert!(cached.get_file("c.bin").is_none());

        assert!(cached.get_subdirectory("sub").is_some());
        assert!(cached.get_subdirectory("missing").is_none());

        assert_eq!(cached.get_files().len(), 2);
        assert_eq!(cached.get_subdirectories().len(), 1);
    }
}
